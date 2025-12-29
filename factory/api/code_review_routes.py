# -*- coding: utf-8 -*-
"""
Code Review Routes - Issue #52
==============================
API endpoints para code review assistido por IA.
"""
from datetime import datetime
from typing import Optional
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

# Database imports
try:
    from factory.database.connection import SessionLocal
    from factory.database.repositories import StoryRepository, StoryTaskRepository
except ImportError:
    SessionLocal = None

# Code Review function
try:
    from factory.api.code_review import perform_code_review
except ImportError:
    perform_code_review = None

# Create router
router = APIRouter(prefix="/api", tags=["code-review"])


@router.post("/stories/{story_id}/review-code")
def review_story_code(story_id: str):
    """
    Revisa todo o codigo de uma story usando IA

    Analisa:
    - Bugs potenciais
    - Vulnerabilidades de seguranca
    - Boas praticas
    - Performance
    - Qualidade do codigo

    Returns:
        {
            "success": bool,
            "score": int (0-100),
            "summary": str,
            "reviews": [...],
            "issues": [...],
            "suggestions": [...],
            "positives": [...]
        }
    """
    if not SessionLocal:
        raise HTTPException(500, "Database not configured")

    if not perform_code_review:
        raise HTTPException(500, "Code review module not available")

    db = SessionLocal()
    try:
        story_repo = StoryRepository(db)
        story_data = story_repo.get_with_tasks(story_id)

        if not story_data:
            raise HTTPException(404, "Story not found")

        story = story_data.get("story", story_data)
        tasks = story_data.get("tasks", [])

        # Filter tasks with code
        tasks_with_code = [t for t in tasks if t.get("code_output")]

        if not tasks_with_code:
            raise HTTPException(400, "Story has no code to review")

        # Review each task
        all_reviews = []
        total_score = 0

        for task in tasks_with_code:
            code = task.get("code_output", "")
            task_title = task.get("title", "Unknown")
            task_type = task.get("task_type", "development")

            # Perform review
            review = perform_code_review(code, task_title, task_type)
            review["task_id"] = task.get("task_id")
            review["task_title"] = task_title
            all_reviews.append(review)
            total_score += review.get("score", 0)

        # Calculate average score
        avg_score = total_score // len(all_reviews) if all_reviews else 0

        # Build result
        result = {
            "success": True,
            "story_id": story_id,
            "score": avg_score,
            "summary": f"Review de {len(all_reviews)} task(s) - Score: {avg_score}/100",
            "reviews": all_reviews,
            "issues": [],
            "suggestions": [],
            "positives": [],
            "metrics": {
                "tasks_reviewed": len(all_reviews),
                "avg_score": avg_score
            },
            "reviewed_at": datetime.utcnow().isoformat()
        }

        # Aggregate issues and suggestions from all reviews
        for review in all_reviews:
            # Quality issues
            if review.get("quality", {}).get("issues"):
                for issue in review["quality"]["issues"]:
                    result["issues"].append({
                        "task_id": review.get("task_id"),
                        "issue_type": "quality",
                        "severity": "warning",
                        "description": issue
                    })

            # Security vulnerabilities
            if review.get("security", {}).get("vulnerabilities"):
                for vuln in review["security"]["vulnerabilities"]:
                    result["issues"].append({
                        "task_id": review.get("task_id"),
                        "issue_type": "security",
                        "severity": "critical",
                        "description": vuln
                    })

            # Performance issues
            if review.get("performance", {}).get("issues"):
                for perf in review["performance"]["issues"]:
                    result["issues"].append({
                        "task_id": review.get("task_id"),
                        "issue_type": "performance",
                        "severity": "warning",
                        "description": perf
                    })

            # Suggestions
            if review.get("suggestions"):
                result["suggestions"].extend(review["suggestions"])

            # Positives
            if review.get("quality", {}).get("positives"):
                result["positives"].extend(review["quality"]["positives"])

        return result

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error reviewing code: {str(e)}")
    finally:
        db.close()


@router.post("/story-tasks/{task_id}/review-code")
def review_task_code(task_id: str):
    """
    Revisa o codigo de uma task especifica usando IA
    """
    if not SessionLocal:
        raise HTTPException(500, "Database not configured")

    if not perform_code_review:
        raise HTTPException(500, "Code review module not available")

    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.get_by_id(task_id)

        if not task:
            raise HTTPException(404, "Task not found")

        if not task.code_output:
            raise HTTPException(400, "Task has no code to review")

        # Perform review
        review = perform_code_review(
            task.code_output,
            task.title,
            task.task_type or "development"
        )

        review["task_id"] = task_id
        review["task_title"] = task.title
        review["reviewed_at"] = datetime.utcnow().isoformat()

        # Save review result to task
        task.review_result = review
        task.updated_at = datetime.utcnow()
        db.commit()

        return review

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error reviewing code: {str(e)}")
    finally:
        db.close()


# Issue #52 - Code Review endpoint (alias)
@router.post("/story-tasks/{task_id}/code-review")
def code_review_task(task_id: str, code: Optional[str] = None):
    """
    Code Review assistido por IA para uma task (Issue #52).

    Analisa codigo para:
    - Bugs potenciais
    - Vulnerabilidades de seguranca
    - Problemas de performance
    - Boas praticas

    Retorna analise completa com secoes:
    - Qualidade
    - Seguranca
    - Performance
    - Sugestoes

    Args:
        task_id: ID da task
        code: Codigo a analisar (opcional, usa code_output da task se nao fornecido)
    """
    if not SessionLocal:
        raise HTTPException(500, "Database not configured")

    if not perform_code_review:
        raise HTTPException(500, "Code review module not available")

    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.get_by_id(task_id)

        if not task:
            raise HTTPException(404, "Task not found")

        # Get code to review
        code_to_review = code if code else task.code_output

        if not code_to_review:
            raise HTTPException(400, "No code available for review. Task has no code_output.")

        # Perform review
        review = perform_code_review(
            code_to_review,
            task.title,
            task.task_type or "development"
        )

        review["task_id"] = task_id
        review["task_title"] = task.title

        # Save review result to task
        task.review_result = review
        task.updated_at = datetime.utcnow()
        db.commit()

        return review

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error performing code review: {str(e)}")
    finally:
        db.close()
