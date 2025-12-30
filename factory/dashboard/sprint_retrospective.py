"""
Sprint Retrospective - Integrated Retrospective Tool
Issue #240: Implement Sprint Retrospective with templates and action items

Provides real-time retrospective board with templates, voting,
anonymous mode, and action items tracking.
"""
from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime, date
import logging
import uuid

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/retrospectives", tags=["Retrospective"])


# ============== Templates ==============

RETRO_TEMPLATES = {
    "start_stop_continue": {
        "name": "Start, Stop, Continue",
        "columns": [
            {"id": "start", "title": "Start", "emoji": "play_arrow", "description": "O que devemos comecar a fazer?"},
            {"id": "stop", "title": "Stop", "emoji": "stop", "description": "O que devemos parar de fazer?"},
            {"id": "continue", "title": "Continue", "emoji": "fast_forward", "description": "O que devemos continuar?"}
        ]
    },
    "mad_sad_glad": {
        "name": "Mad, Sad, Glad",
        "columns": [
            {"id": "mad", "title": "Mad", "emoji": "angry", "description": "O que te frustrou?"},
            {"id": "sad", "title": "Sad", "emoji": "sad", "description": "O que te decepcionou?"},
            {"id": "glad", "title": "Glad", "emoji": "happy", "description": "O que te deixou feliz?"}
        ]
    },
    "4ls": {
        "name": "4Ls",
        "columns": [
            {"id": "liked", "title": "Liked", "emoji": "heart", "description": "O que voce gostou?"},
            {"id": "learned", "title": "Learned", "emoji": "book", "description": "O que voce aprendeu?"},
            {"id": "lacked", "title": "Lacked", "emoji": "close", "description": "O que faltou?"},
            {"id": "longed", "title": "Longed For", "emoji": "star", "description": "O que voce desejou?"}
        ]
    },
    "sailboat": {
        "name": "Sailboat",
        "columns": [
            {"id": "wind", "title": "Vento", "emoji": "air", "description": "O que nos impulsionou?"},
            {"id": "anchor", "title": "Ancora", "emoji": "anchor", "description": "O que nos segurou?"},
            {"id": "rocks", "title": "Rochas", "emoji": "warning", "description": "Riscos a frente?"},
            {"id": "island", "title": "Ilha", "emoji": "flag", "description": "Nosso objetivo?"}
        ]
    },
    "good_bad_ideas": {
        "name": "O que foi bem / Melhorar / Ideias",
        "columns": [
            {"id": "good", "title": "O que foi bem", "emoji": "thumb_up", "description": "O que funcionou bem?"},
            {"id": "improve", "title": "O que melhorar", "emoji": "construction", "description": "O que pode ser melhorado?"},
            {"id": "ideas", "title": "Ideias/Acoes", "emoji": "lightbulb", "description": "Ideias e sugestoes?"}
        ]
    }
}

RETRO_PHASES = [
    {"name": "collecting", "title": "Coleta", "duration": 600, "description": "Todos adicionam items"},
    {"name": "grouping", "title": "Agrupamento", "duration": 300, "description": "Agrupar items similares"},
    {"name": "voting", "title": "Votacao", "duration": 300, "description": "Votar nos mais importantes"},
    {"name": "discussing", "title": "Discussao", "duration": 1200, "description": "Discutir top items"},
    {"name": "action_items", "title": "Acoes", "duration": 300, "description": "Definir action items"}
]


# ============== Pydantic Schemas ==============

class CreateRetroRequest(BaseModel):
    name: str = Field(..., min_length=1, max_length=200)
    sprint_id: Optional[str] = None
    template: str = Field(default="start_stop_continue")
    facilitator_id: str
    votes_per_person: int = Field(default=3, ge=1, le=10)
    anonymous_mode: bool = Field(default=True)
    tenant_id: Optional[str] = None


class AddItemRequest(BaseModel):
    column_id: str
    content: str = Field(..., min_length=1)
    author_id: str
    author_name: Optional[str] = None
    anonymous: bool = Field(default=False)


class VoteRequest(BaseModel):
    user_id: str


class AddActionRequest(BaseModel):
    content: str = Field(..., min_length=1)
    assignee_id: Optional[str] = None
    assignee_name: Optional[str] = None
    due_date: Optional[str] = None
    source_item_id: Optional[str] = None


class UpdateActionRequest(BaseModel):
    content: Optional[str] = None
    assignee_id: Optional[str] = None
    assignee_name: Optional[str] = None
    due_date: Optional[str] = None
    status: Optional[str] = None


# ============== Helper Functions ==============

def generate_retro_id() -> str:
    return f"RETRO-{uuid.uuid4().hex[:8].upper()}"


def generate_item_id() -> str:
    return f"ITEM-{uuid.uuid4().hex[:8].upper()}"


def generate_vote_id() -> str:
    return f"VOTE-{uuid.uuid4().hex[:8].upper()}"


def generate_action_id() -> str:
    return f"ACTION-{uuid.uuid4().hex[:8].upper()}"


# ============== API Endpoints ==============

@router.get("/templates")
async def get_templates():
    """Get available retrospective templates"""
    return {
        "templates": [
            {"id": k, "name": v["name"], "columns": v["columns"]}
            for k, v in RETRO_TEMPLATES.items()
        ],
        "phases": RETRO_PHASES
    }


@router.post("")
async def create_retrospective(request: CreateRetroRequest):
    """Create a new retrospective session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetrospectiveStatus

        retro_id = generate_retro_id()
        template = RETRO_TEMPLATES.get(request.template, RETRO_TEMPLATES["start_stop_continue"])

        with get_session() as db:
            retro = Retrospective(
                retro_id=retro_id,
                tenant_id=request.tenant_id,
                sprint_id=request.sprint_id,
                template=request.template,
                name=request.name,
                status=RetrospectiveStatus.DRAFT.value,
                facilitator_id=request.facilitator_id,
                columns=template["columns"],
                votes_per_person=request.votes_per_person,
                anonymous_mode=request.anonymous_mode,
                participants=[{
                    "user_id": request.facilitator_id,
                    "user_name": "Facilitador",
                    "joined_at": datetime.utcnow().isoformat(),
                    "is_facilitator": True
                }]
            )
            db.add(retro)
            db.commit()

            logger.info(f"[Retro] Created: {retro_id}")

            return {
                "success": True,
                "retro_id": retro_id,
                "name": request.name,
                "template": request.template,
                "columns": template["columns"]
            }

    except Exception as e:
        logger.error(f"[Retro] Error creating: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{retro_id}")
async def get_retrospective(retro_id: str):
    """Get retrospective details with all items"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetroItem, RetroActionItem

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            items = db.query(RetroItem).filter(
                RetroItem.retro_id == retro_id
            ).order_by(RetroItem.vote_count.desc()).all()

            actions = db.query(RetroActionItem).filter(
                RetroActionItem.retro_id == retro_id
            ).order_by(RetroActionItem.created_at.desc()).all()

            # Group items by column
            items_by_column = {}
            for item in items:
                if item.column_id not in items_by_column:
                    items_by_column[item.column_id] = []
                show_author = not retro.anonymous_mode or retro.status == "completed"
                items_by_column[item.column_id].append(item.to_dict(show_author))

            return {
                **retro.to_dict(),
                "items": items_by_column,
                "action_items": [a.to_dict() for a in actions],
                "total_items": len(items),
                "phases": RETRO_PHASES
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error getting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("")
async def list_retrospectives(
    tenant_id: Optional[str] = None,
    sprint_id: Optional[str] = None,
    status: Optional[str] = None,
    limit: int = Query(default=20, le=100)
):
    """List retrospectives"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective

        with get_session() as db:
            query = db.query(Retrospective)

            if tenant_id:
                query = query.filter(Retrospective.tenant_id == tenant_id)
            if sprint_id:
                query = query.filter(Retrospective.sprint_id == sprint_id)
            if status:
                query = query.filter(Retrospective.status == status)

            retros = query.order_by(Retrospective.created_at.desc()).limit(limit).all()

            return {
                "retrospectives": [r.to_dict() for r in retros],
                "total": len(retros)
            }

    except Exception as e:
        logger.error(f"[Retro] Error listing: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/join")
async def join_retrospective(retro_id: str, user_id: str, user_name: str):
    """Join a retrospective session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            participants = retro.participants or []
            existing = next((p for p in participants if p.get("user_id") == user_id), None)

            if not existing:
                participants.append({
                    "user_id": user_id,
                    "user_name": user_name,
                    "joined_at": datetime.utcnow().isoformat(),
                    "is_facilitator": False
                })
                retro.participants = participants
                db.commit()

            return {"success": True, "participants": participants}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error joining: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/start")
async def start_retrospective(retro_id: str):
    """Start the retrospective (move to collecting phase)"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetrospectiveStatus

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            retro.status = RetrospectiveStatus.COLLECTING.value
            retro.current_phase = "collecting"
            db.commit()

            return {"success": True, "status": retro.status, "phase": retro.current_phase}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error starting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/phase/{phase}")
async def change_phase(retro_id: str, phase: str):
    """Change the retrospective phase"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetrospectiveStatus

        valid_phases = ["collecting", "grouping", "voting", "discussing", "action_items"]
        if phase not in valid_phases:
            raise HTTPException(status_code=400, detail=f"Invalid phase: {phase}")

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            retro.current_phase = phase
            retro.status = getattr(RetrospectiveStatus, phase.upper(), RetrospectiveStatus.COLLECTING).value
            db.commit()

            return {"success": True, "phase": phase, "status": retro.status}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error changing phase: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/complete")
async def complete_retrospective(retro_id: str):
    """Complete the retrospective"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetrospectiveStatus

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            retro.status = RetrospectiveStatus.COMPLETED.value
            retro.completed_at = datetime.utcnow()
            db.commit()

            return {"success": True, "status": retro.status}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error completing: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/items")
async def add_item(retro_id: str, request: AddItemRequest):
    """Add an item to the retrospective board"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetroItem

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            item_id = generate_item_id()
            item = RetroItem(
                item_id=item_id,
                retro_id=retro_id,
                column_id=request.column_id,
                content=request.content,
                author_id=request.author_id,
                author_name=request.author_name,
                anonymous=request.anonymous or retro.anonymous_mode
            )
            db.add(item)
            db.commit()

            logger.info(f"[Retro] Item added: {item_id}")

            show_author = not retro.anonymous_mode
            return {"success": True, "item": item.to_dict(show_author)}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error adding item: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/{retro_id}/items/{item_id}")
async def delete_item(retro_id: str, item_id: str):
    """Delete an item from the retrospective"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import RetroItem

        with get_session() as db:
            item = db.query(RetroItem).filter(
                RetroItem.item_id == item_id,
                RetroItem.retro_id == retro_id
            ).first()

            if not item:
                raise HTTPException(status_code=404, detail="Item not found")

            db.delete(item)
            db.commit()

            return {"success": True}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error deleting item: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/items/{item_id}/vote")
async def vote_item(retro_id: str, item_id: str, request: VoteRequest):
    """Vote for an item"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective, RetroItem, RetroVote

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            # Check vote limit
            user_votes = db.query(RetroVote).join(RetroItem).filter(
                RetroItem.retro_id == retro_id,
                RetroVote.user_id == request.user_id
            ).count()

            if user_votes >= retro.votes_per_person:
                raise HTTPException(status_code=400, detail=f"Vote limit reached ({retro.votes_per_person})")

            # Check if already voted
            existing = db.query(RetroVote).filter(
                RetroVote.item_id == item_id,
                RetroVote.user_id == request.user_id
            ).first()

            if existing:
                raise HTTPException(status_code=400, detail="Already voted for this item")

            # Add vote
            vote = RetroVote(
                vote_id=generate_vote_id(),
                item_id=item_id,
                user_id=request.user_id
            )
            db.add(vote)

            # Update item vote count
            item = db.query(RetroItem).filter(RetroItem.item_id == item_id).first()
            if item:
                item.vote_count = (item.vote_count or 0) + 1

            db.commit()

            return {
                "success": True,
                "vote_count": item.vote_count if item else 0,
                "user_votes_remaining": retro.votes_per_person - user_votes - 1
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error voting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/{retro_id}/items/{item_id}/vote")
async def unvote_item(retro_id: str, item_id: str, user_id: str):
    """Remove vote from an item"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import RetroItem, RetroVote

        with get_session() as db:
            vote = db.query(RetroVote).filter(
                RetroVote.item_id == item_id,
                RetroVote.user_id == user_id
            ).first()

            if not vote:
                raise HTTPException(status_code=404, detail="Vote not found")

            db.delete(vote)

            # Update item vote count
            item = db.query(RetroItem).filter(RetroItem.item_id == item_id).first()
            if item and item.vote_count > 0:
                item.vote_count -= 1

            db.commit()

            return {"success": True, "vote_count": item.vote_count if item else 0}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error unvoting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/actions")
async def add_action_item(retro_id: str, request: AddActionRequest):
    """Add an action item"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import RetroActionItem

        action_id = generate_action_id()

        due = None
        if request.due_date:
            try:
                due = datetime.strptime(request.due_date, "%Y-%m-%d").date()
            except ValueError:
                pass

        with get_session() as db:
            action = RetroActionItem(
                action_id=action_id,
                retro_id=retro_id,
                content=request.content,
                assignee_id=request.assignee_id,
                assignee_name=request.assignee_name,
                due_date=due,
                source_item_id=request.source_item_id
            )
            db.add(action)
            db.commit()

            logger.info(f"[Retro] Action added: {action_id}")

            return {"success": True, "action": action.to_dict()}

    except Exception as e:
        logger.error(f"[Retro] Error adding action: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.put("/{retro_id}/actions/{action_id}")
async def update_action_item(retro_id: str, action_id: str, request: UpdateActionRequest):
    """Update an action item"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import RetroActionItem

        with get_session() as db:
            action = db.query(RetroActionItem).filter(
                RetroActionItem.action_id == action_id,
                RetroActionItem.retro_id == retro_id
            ).first()

            if not action:
                raise HTTPException(status_code=404, detail="Action item not found")

            if request.content is not None:
                action.content = request.content
            if request.assignee_id is not None:
                action.assignee_id = request.assignee_id
            if request.assignee_name is not None:
                action.assignee_name = request.assignee_name
            if request.due_date is not None:
                try:
                    action.due_date = datetime.strptime(request.due_date, "%Y-%m-%d").date()
                except ValueError:
                    pass
            if request.status is not None:
                action.status = request.status
                if request.status == "done":
                    action.completed_at = datetime.utcnow()

            db.commit()

            return {"success": True, "action": action.to_dict()}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error updating action: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{retro_id}/actions/{action_id}/convert-to-task")
async def convert_action_to_task(retro_id: str, action_id: str, project_id: str):
    """Convert an action item to a project task"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import RetroActionItem, Task, TaskStatus
        import uuid

        with get_session() as db:
            action = db.query(RetroActionItem).filter(
                RetroActionItem.action_id == action_id,
                RetroActionItem.retro_id == retro_id
            ).first()

            if not action:
                raise HTTPException(status_code=404, detail="Action item not found")

            if action.task_id:
                return {"success": False, "message": "Already converted to task", "task_id": action.task_id}

            # Create task
            task_id = f"TSK-{uuid.uuid4().hex[:8].upper()}"
            task = Task(
                task_id=task_id,
                project_id=project_id,
                title=action.content,
                description=f"Action item from retrospective: {retro_id}",
                status=TaskStatus.TODO.value,
                priority="medium",
                assigned_to=action.assignee_id
            )
            db.add(task)

            # Link action to task
            action.task_id = task_id

            db.commit()

            logger.info(f"[Retro] Action {action_id} converted to task {task_id}")

            return {"success": True, "task_id": task_id}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error converting to task: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/{retro_id}")
async def delete_retrospective(retro_id: str):
    """Delete a retrospective"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import Retrospective

        with get_session() as db:
            retro = db.query(Retrospective).filter(
                Retrospective.retro_id == retro_id
            ).first()

            if not retro:
                raise HTTPException(status_code=404, detail="Retrospective not found")

            db.delete(retro)
            db.commit()

            return {"success": True}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Retro] Error deleting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


def register_retrospective_routes(app):
    """Register retrospective routes with the main app"""
    app.include_router(router)
    logger.info("[Retro] Routes registered")
