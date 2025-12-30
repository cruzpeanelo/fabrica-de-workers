# -*- coding: utf-8 -*-
"""
Bulk Actions Module (Issue #278)
================================
Sistema avancado de operacoes em lote para o Kanban.

Funcionalidades:
- Selecao multipla de stories
- Mover em lote para qualquer coluna
- Alterar prioridade em lote
- Atribuir responsavel em lote
- Desfazer ultima acao
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import List, Optional
from factory.database.connection import SessionLocal
from factory.database.models import Story

router = APIRouter(prefix="/api/bulk", tags=["Bulk Actions"])


class BulkMoveRequest(BaseModel):
    story_ids: List[str]
    status: str


class BulkPriorityRequest(BaseModel):
    story_ids: List[str]
    priority: str


class BulkAssignRequest(BaseModel):
    story_ids: List[str]
    assignee: Optional[str] = None


class BulkDeleteRequest(BaseModel):
    story_ids: List[str]


@router.post("/move")
async def bulk_move_stories(request: BulkMoveRequest):
    """Move multiplas stories para um novo status."""
    db = SessionLocal()
    try:
        updated = 0
        previous_states = []

        for story_id in request.story_ids:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if story:
                previous_states.append({
                    "id": story_id,
                    "status": story.status.value if hasattr(story.status, 'value') else story.status
                })
                story.status = request.status
                updated += 1

        db.commit()

        return {
            "success": True,
            "updated": updated,
            "previous_states": previous_states,
            "message": f"{updated} stories movidas para {request.status}"
        }
    except Exception as e:
        db.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


@router.post("/priority")
async def bulk_set_priority(request: BulkPriorityRequest):
    """Altera a prioridade de multiplas stories."""
    db = SessionLocal()
    try:
        updated = 0
        previous_states = []

        for story_id in request.story_ids:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if story:
                previous_states.append({
                    "id": story_id,
                    "priority": story.priority.value if hasattr(story.priority, 'value') else story.priority
                })
                story.priority = request.priority
                updated += 1

        db.commit()

        return {
            "success": True,
            "updated": updated,
            "previous_states": previous_states,
            "message": f"{updated} stories com prioridade {request.priority}"
        }
    except Exception as e:
        db.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


@router.post("/assign")
async def bulk_assign_stories(request: BulkAssignRequest):
    """Atribui multiplas stories para um responsavel."""
    db = SessionLocal()
    try:
        updated = 0
        previous_states = []

        for story_id in request.story_ids:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if story:
                previous_states.append({
                    "id": story_id,
                    "assignee": story.assignee
                })
                story.assignee = request.assignee
                updated += 1

        db.commit()

        assignee_name = request.assignee or "ninguem"
        return {
            "success": True,
            "updated": updated,
            "previous_states": previous_states,
            "message": f"{updated} stories atribuidas para {assignee_name}"
        }
    except Exception as e:
        db.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


@router.post("/delete")
async def bulk_delete_stories(request: BulkDeleteRequest):
    """Exclui multiplas stories."""
    db = SessionLocal()
    try:
        deleted = 0

        for story_id in request.story_ids:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if story:
                db.delete(story)
                deleted += 1

        db.commit()

        return {
            "success": True,
            "deleted": deleted,
            "message": f"{deleted} stories excluidas"
        }
    except Exception as e:
        db.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


@router.post("/undo")
async def undo_bulk_action(previous_states: List[dict], action_type: str):
    """Desfaz uma acao em lote."""
    db = SessionLocal()
    try:
        restored = 0

        for state in previous_states:
            story = db.query(Story).filter(Story.story_id == state["id"]).first()
            if story:
                if action_type == "move" and "status" in state:
                    story.status = state["status"]
                elif action_type == "priority" and "priority" in state:
                    story.priority = state["priority"]
                elif action_type == "assign" and "assignee" in state:
                    story.assignee = state["assignee"]
                restored += 1

        db.commit()

        return {
            "success": True,
            "restored": restored,
            "message": f"{restored} stories restauradas"
        }
    except Exception as e:
        db.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


def register_bulk_actions(app):
    """Registra os endpoints de acoes em lote no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Bulk Actions endpoints loaded: /api/bulk/*")
