"""
Planning Poker - Real-time Estimation
Issue #244: Implement Planning Poker for story estimation

Provides real-time collaborative estimation sessions where team members
can vote on story points using Fibonacci or custom scales.
"""
from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime
import logging

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/planning-poker", tags=["Planning Poker"])


# ============== Pydantic Schemas ==============

class CreateSessionRequest(BaseModel):
    name: str = Field(..., min_length=1, max_length=200)
    facilitator_id: str = Field(..., min_length=1)
    scale_type: str = Field(default="fibonacci")
    custom_scale: Optional[List[str]] = None
    story_ids: List[str] = Field(default_factory=list)
    tenant_id: Optional[str] = None


class JoinSessionRequest(BaseModel):
    user_id: str
    user_name: str


class SubmitVoteRequest(BaseModel):
    user_id: str
    story_id: str
    vote_value: str


class SaveEstimateRequest(BaseModel):
    story_id: str
    final_estimate: int


class SessionResponse(BaseModel):
    session_id: str
    name: str
    status: str
    facilitator_id: str
    scale_type: str
    scale_values: List[str]
    story_ids: List[str]
    current_story_index: int
    participants: List[Dict[str, Any]]
    created_at: datetime


class VoteResponse(BaseModel):
    vote_id: str
    session_id: str
    story_id: str
    user_id: str
    vote_value: Optional[str]
    is_revealed: bool


# ============== Helper Functions ==============

def get_scale_values(scale_type: str, custom_scale: Optional[List[str]] = None) -> List[str]:
    """Get voting scale values based on type"""
    scales = {
        "fibonacci": ["1", "2", "3", "5", "8", "13", "21", "?"],
        "modified_fibonacci": ["0", "0.5", "1", "2", "3", "5", "8", "13", "20", "40", "100", "?"],
        "tshirt": ["XS", "S", "M", "L", "XL", "XXL", "?"],
        "powers_of_2": ["1", "2", "4", "8", "16", "32", "?"],
    }
    if scale_type == "custom" and custom_scale:
        return custom_scale
    return scales.get(scale_type, scales["fibonacci"])


def calculate_vote_statistics(votes: List[Dict]) -> Dict[str, Any]:
    """Calculate statistics from votes"""
    numeric_votes = []
    vote_distribution = {}

    for vote in votes:
        value = vote.get("vote_value")
        if value and value != "?":
            vote_distribution[value] = vote_distribution.get(value, 0) + 1
            try:
                numeric_votes.append(float(value))
            except ValueError:
                pass

    stats = {
        "total_votes": len(votes),
        "distribution": vote_distribution,
        "consensus": len(vote_distribution) == 1 and len(votes) > 1,
    }

    if numeric_votes:
        stats["average"] = round(sum(numeric_votes) / len(numeric_votes), 1)
        stats["min"] = min(numeric_votes)
        stats["max"] = max(numeric_votes)
        stats["spread"] = stats["max"] - stats["min"]

    return stats


def generate_session_id() -> str:
    """Generate unique session ID"""
    import uuid
    return f"PP-{uuid.uuid4().hex[:8].upper()}"


def generate_vote_id() -> str:
    """Generate unique vote ID"""
    import uuid
    return f"VOTE-{uuid.uuid4().hex[:8].upper()}"


# ============== API Endpoints ==============

@router.post("/sessions", response_model=Dict[str, Any])
async def create_session(request: CreateSessionRequest):
    """Create a new Planning Poker session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningSessionStatus

        session_id = generate_session_id()
        scale_values = get_scale_values(request.scale_type, request.custom_scale)

        with get_session() as db:
            session = PlanningSession(
                session_id=session_id,
                tenant_id=request.tenant_id,
                name=request.name,
                status=PlanningSessionStatus.CREATED.value,
                facilitator_id=request.facilitator_id,
                scale_type=request.scale_type,
                scale_values=scale_values,
                story_ids=request.story_ids,
                current_story_index=0,
                participants=[{
                    "user_id": request.facilitator_id,
                    "user_name": "Facilitator",
                    "joined_at": datetime.utcnow().isoformat(),
                    "is_facilitator": True
                }]
            )
            db.add(session)
            db.commit()
            db.refresh(session)

            logger.info(f"[Planning Poker] Session created: {session_id}")

            return {
                "success": True,
                "session_id": session_id,
                "name": session.name,
                "status": session.status,
                "scale_values": scale_values,
                "story_count": len(request.story_ids)
            }

    except Exception as e:
        logger.error(f"[Planning Poker] Error creating session: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/sessions/{session_id}")
async def get_session(session_id: str):
    """Get Planning Poker session details"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            return {
                "session_id": session.session_id,
                "name": session.name,
                "status": session.status,
                "facilitator_id": session.facilitator_id,
                "scale_type": session.scale_type,
                "scale_values": session.scale_values or [],
                "story_ids": session.story_ids or [],
                "current_story_index": session.current_story_index,
                "participants": session.participants or [],
                "created_at": session.created_at.isoformat() if session.created_at else None,
                "current_story_id": (session.story_ids or [])[session.current_story_index]
                    if session.story_ids and session.current_story_index < len(session.story_ids)
                    else None
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error getting session: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/sessions")
async def list_sessions(
    tenant_id: Optional[str] = None,
    status: Optional[str] = None,
    limit: int = Query(default=20, le=100)
):
    """List Planning Poker sessions"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession

        with get_session() as db:
            query = db.query(PlanningSession)

            if tenant_id:
                query = query.filter(PlanningSession.tenant_id == tenant_id)
            if status:
                query = query.filter(PlanningSession.status == status)

            sessions = query.order_by(PlanningSession.created_at.desc()).limit(limit).all()

            return {
                "sessions": [{
                    "session_id": s.session_id,
                    "name": s.name,
                    "status": s.status,
                    "facilitator_id": s.facilitator_id,
                    "participant_count": len(s.participants or []),
                    "story_count": len(s.story_ids or []),
                    "created_at": s.created_at.isoformat() if s.created_at else None
                } for s in sessions],
                "total": len(sessions)
            }

    except Exception as e:
        logger.error(f"[Planning Poker] Error listing sessions: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/join")
async def join_session(session_id: str, request: JoinSessionRequest):
    """Join a Planning Poker session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            participants = session.participants or []

            # Check if already joined
            existing = next((p for p in participants if p.get("user_id") == request.user_id), None)
            if existing:
                return {"success": True, "message": "Already joined", "participants": participants}

            # Add new participant
            participants.append({
                "user_id": request.user_id,
                "user_name": request.user_name,
                "joined_at": datetime.utcnow().isoformat(),
                "is_facilitator": False
            })

            session.participants = participants
            db.commit()

            logger.info(f"[Planning Poker] User {request.user_id} joined session {session_id}")

            return {
                "success": True,
                "message": "Joined session",
                "participants": participants
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error joining session: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/start")
async def start_voting(session_id: str):
    """Start voting for current story"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningSessionStatus

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            if not session.story_ids:
                raise HTTPException(status_code=400, detail="No stories in session")

            session.status = PlanningSessionStatus.VOTING.value
            db.commit()

            current_story_id = session.story_ids[session.current_story_index]

            logger.info(f"[Planning Poker] Voting started for story {current_story_id}")

            return {
                "success": True,
                "status": session.status,
                "current_story_id": current_story_id,
                "current_story_index": session.current_story_index
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error starting voting: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/next")
async def next_story(session_id: str):
    """Move to next story in session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningSessionStatus

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            if session.current_story_index >= len(session.story_ids) - 1:
                # Last story - complete session
                session.status = PlanningSessionStatus.COMPLETED.value
                db.commit()
                return {
                    "success": True,
                    "completed": True,
                    "message": "All stories estimated"
                }

            # Move to next story
            session.current_story_index += 1
            session.status = PlanningSessionStatus.VOTING.value
            db.commit()

            current_story_id = session.story_ids[session.current_story_index]

            return {
                "success": True,
                "completed": False,
                "current_story_id": current_story_id,
                "current_story_index": session.current_story_index,
                "remaining": len(session.story_ids) - session.current_story_index - 1
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error moving to next story: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/complete")
async def complete_session(session_id: str):
    """Complete the Planning Poker session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningSessionStatus

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            session.status = PlanningSessionStatus.COMPLETED.value
            db.commit()

            logger.info(f"[Planning Poker] Session {session_id} completed")

            return {"success": True, "status": session.status}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error completing session: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/votes")
async def submit_vote(session_id: str, request: SubmitVoteRequest):
    """Submit a vote for a story"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningVote

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            if session.status != "voting":
                raise HTTPException(status_code=400, detail="Session not in voting state")

            # Check if user already voted
            existing_vote = db.query(PlanningVote).filter(
                PlanningVote.session_id == session_id,
                PlanningVote.story_id == request.story_id,
                PlanningVote.user_id == request.user_id
            ).first()

            if existing_vote:
                # Update existing vote
                existing_vote.vote_value = request.vote_value
                existing_vote.updated_at = datetime.utcnow()
                db.commit()

                return {
                    "success": True,
                    "vote_id": existing_vote.vote_id,
                    "message": "Vote updated"
                }

            # Create new vote
            vote = PlanningVote(
                vote_id=generate_vote_id(),
                session_id=session_id,
                story_id=request.story_id,
                user_id=request.user_id,
                vote_value=request.vote_value,
                is_revealed=False
            )
            db.add(vote)
            db.commit()

            logger.info(f"[Planning Poker] Vote submitted: {request.user_id} -> {request.vote_value}")

            return {
                "success": True,
                "vote_id": vote.vote_id,
                "message": "Vote submitted"
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error submitting vote: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/reveal")
async def reveal_votes(session_id: str, story_id: str):
    """Reveal all votes for a story"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningVote, PlanningSessionStatus

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            # Get all votes for this story
            votes = db.query(PlanningVote).filter(
                PlanningVote.session_id == session_id,
                PlanningVote.story_id == story_id
            ).all()

            # Mark all as revealed
            for vote in votes:
                vote.is_revealed = True

            session.status = PlanningSessionStatus.REVEALED.value
            db.commit()

            # Calculate statistics
            vote_data = [{"user_id": v.user_id, "vote_value": v.vote_value} for v in votes]
            stats = calculate_vote_statistics(vote_data)

            logger.info(f"[Planning Poker] Votes revealed for story {story_id}")

            return {
                "success": True,
                "votes": [{
                    "user_id": v.user_id,
                    "vote_value": v.vote_value
                } for v in votes],
                "statistics": stats
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error revealing votes: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/sessions/{session_id}/votes")
async def get_votes(session_id: str, story_id: Optional[str] = None):
    """Get votes for a session (or specific story)"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningVote

        with get_session() as db:
            query = db.query(PlanningVote).filter(PlanningVote.session_id == session_id)

            if story_id:
                query = query.filter(PlanningVote.story_id == story_id)

            votes = query.all()

            return {
                "votes": [{
                    "vote_id": v.vote_id,
                    "story_id": v.story_id,
                    "user_id": v.user_id,
                    "vote_value": v.vote_value if v.is_revealed else None,
                    "has_voted": v.vote_value is not None,
                    "is_revealed": v.is_revealed
                } for v in votes],
                "total": len(votes)
            }

    except Exception as e:
        logger.error(f"[Planning Poker] Error getting votes: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sessions/{session_id}/estimate")
async def save_estimate(session_id: str, request: SaveEstimateRequest):
    """Save final estimate for a story"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, Story

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            # Update story points
            story = db.query(Story).filter(Story.story_id == request.story_id).first()

            if story:
                story.story_points = request.final_estimate
                story.updated_at = datetime.utcnow()
                db.commit()

                logger.info(f"[Planning Poker] Story {request.story_id} estimated at {request.final_estimate} points")

                return {
                    "success": True,
                    "story_id": request.story_id,
                    "story_points": request.final_estimate
                }
            else:
                return {
                    "success": False,
                    "message": "Story not found"
                }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error saving estimate: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/sessions/{session_id}")
async def delete_session(session_id: str):
    """Delete a Planning Poker session"""
    try:
        from factory.database.connection import get_session
        from factory.database.models import PlanningSession, PlanningVote

        with get_session() as db:
            session = db.query(PlanningSession).filter(
                PlanningSession.session_id == session_id
            ).first()

            if not session:
                raise HTTPException(status_code=404, detail="Session not found")

            # Delete votes first
            db.query(PlanningVote).filter(PlanningVote.session_id == session_id).delete()

            # Delete session
            db.delete(session)
            db.commit()

            logger.info(f"[Planning Poker] Session {session_id} deleted")

            return {"success": True, "message": "Session deleted"}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"[Planning Poker] Error deleting session: {e}")
        raise HTTPException(status_code=500, detail=str(e))


def register_planning_poker_routes(app):
    """Register Planning Poker routes with the main app"""
    app.include_router(router)
    logger.info("[Planning Poker] Routes registered")
