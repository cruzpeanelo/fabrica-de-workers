# -*- coding: utf-8 -*-
"""
Sandbox API Routes - Issue #380
===============================
REST endpoints for code execution in sandbox.

Provides secure code execution with:
- Language support (Python, JavaScript, Bash)
- Timeout control
- Output capture
- Error handling
"""

import time
import uuid
import subprocess
import tempfile
import os
from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel, Field
import logging

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/sandbox", tags=["Sandbox"])


# =============================================================================
# CONFIGURATION
# =============================================================================

MAX_EXECUTION_TIME = 30  # seconds
MAX_OUTPUT_SIZE = 100000  # characters
SUPPORTED_LANGUAGES = ["python", "javascript", "bash"]


# =============================================================================
# MODELS
# =============================================================================

class ExecuteRequest(BaseModel):
    """Request to execute code."""
    code: str = Field(..., min_length=1, max_length=50000)
    language: str = Field(..., description="python, javascript, bash")
    timeout: int = Field(default=30, ge=1, le=MAX_EXECUTION_TIME)


class ExecuteResponse(BaseModel):
    """Execution result."""
    success: bool
    output: str
    error: Optional[str]
    execution_time: float
    language: str


class CodeTemplate(BaseModel):
    """Code template."""
    id: str
    name: str
    language: str
    description: str
    code: str
    category: str


class SaveSnippetRequest(BaseModel):
    """Request to save a code snippet."""
    name: str = Field(..., min_length=1, max_length=100)
    code: str = Field(..., min_length=1)
    language: str
    description: Optional[str] = None


class SnippetResponse(BaseModel):
    """Saved snippet response."""
    id: str
    name: str
    language: str
    description: Optional[str]
    created_at: str


class ExecutionHistoryItem(BaseModel):
    """Execution history item."""
    id: str
    language: str
    success: bool
    execution_time: float
    timestamp: str
    output_preview: str


# =============================================================================
# STORAGE
# =============================================================================

_snippets: Dict[str, Dict] = {}
_execution_history: List[Dict] = []


# =============================================================================
# DEFAULT TEMPLATES
# =============================================================================

DEFAULT_TEMPLATES = [
    CodeTemplate(
        id="tpl_hello_python",
        name="Hello World",
        language="python",
        description="Simple hello world in Python",
        code='print("Hello, World!")',
        category="basics"
    ),
    CodeTemplate(
        id="tpl_hello_js",
        name="Hello World",
        language="javascript",
        description="Simple hello world in JavaScript",
        code='console.log("Hello, World!");',
        category="basics"
    ),
    CodeTemplate(
        id="tpl_fibonacci",
        name="Fibonacci",
        language="python",
        description="Calculate Fibonacci sequence",
        code='''def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(10):
    print(f"F({i}) = {fibonacci(i)}")
''',
        category="algorithms"
    ),
    CodeTemplate(
        id="tpl_api_request",
        name="API Request",
        language="python",
        description="Make HTTP request to API",
        code='''import json
# Simulated API response
data = {"status": "ok", "items": [1, 2, 3]}
print(json.dumps(data, indent=2))
''',
        category="api"
    ),
    CodeTemplate(
        id="tpl_list_files",
        name="List Files",
        language="bash",
        description="List files in current directory",
        code="ls -la",
        category="system"
    ),
]


# =============================================================================
# EXECUTOR
# =============================================================================

class SandboxExecutor:
    """
    Safe code executor.

    Uses subprocess with timeout and output limits.
    """

    @classmethod
    def execute(
        cls,
        code: str,
        language: str,
        timeout: int = 30
    ) -> Dict[str, Any]:
        """
        Execute code in sandbox.

        Returns dict with success, output, error, execution_time.
        """
        if language not in SUPPORTED_LANGUAGES:
            return {
                "success": False,
                "output": "",
                "error": f"Unsupported language: {language}",
                "execution_time": 0
            }

        start_time = time.time()

        try:
            if language == "python":
                result = cls._execute_python(code, timeout)
            elif language == "javascript":
                result = cls._execute_javascript(code, timeout)
            elif language == "bash":
                result = cls._execute_bash(code, timeout)
            else:
                result = {"success": False, "output": "", "error": "Unknown language"}

        except subprocess.TimeoutExpired:
            result = {
                "success": False,
                "output": "",
                "error": f"Execution timed out after {timeout} seconds"
            }
        except Exception as e:
            result = {
                "success": False,
                "output": "",
                "error": str(e)
            }

        execution_time = time.time() - start_time
        result["execution_time"] = round(execution_time, 3)

        # Truncate output if too long
        if len(result.get("output", "")) > MAX_OUTPUT_SIZE:
            result["output"] = result["output"][:MAX_OUTPUT_SIZE] + "\n... (output truncated)"

        return result

    @classmethod
    def _execute_python(cls, code: str, timeout: int) -> Dict:
        """Execute Python code."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["python", temp_file],
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=tempfile.gettempdir()
            )

            if result.returncode == 0:
                return {
                    "success": True,
                    "output": result.stdout,
                    "error": None
                }
            else:
                return {
                    "success": False,
                    "output": result.stdout,
                    "error": result.stderr
                }

        finally:
            try:
                os.unlink(temp_file)
            except:
                pass

    @classmethod
    def _execute_javascript(cls, code: str, timeout: int) -> Dict:
        """Execute JavaScript code using Node.js."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.js', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["node", temp_file],
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=tempfile.gettempdir()
            )

            if result.returncode == 0:
                return {
                    "success": True,
                    "output": result.stdout,
                    "error": None
                }
            else:
                return {
                    "success": False,
                    "output": result.stdout,
                    "error": result.stderr
                }

        finally:
            try:
                os.unlink(temp_file)
            except:
                pass

    @classmethod
    def _execute_bash(cls, code: str, timeout: int) -> Dict:
        """Execute Bash commands."""
        # Security: Only allow safe commands
        forbidden = ["rm -rf", "sudo", "chmod", "chown", "mkfs", "> /dev"]
        for cmd in forbidden:
            if cmd in code.lower():
                return {
                    "success": False,
                    "output": "",
                    "error": f"Command not allowed: {cmd}"
                }

        try:
            result = subprocess.run(
                ["bash", "-c", code],
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=tempfile.gettempdir()
            )

            if result.returncode == 0:
                return {
                    "success": True,
                    "output": result.stdout,
                    "error": None
                }
            else:
                return {
                    "success": False,
                    "output": result.stdout,
                    "error": result.stderr
                }

        except FileNotFoundError:
            return {
                "success": False,
                "output": "",
                "error": "Bash not available on this system"
            }


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/execute", response_model=ExecuteResponse)
async def execute_code(data: ExecuteRequest, request: Request):
    """
    Execute code in sandbox.

    Supports: python, javascript, bash
    Max timeout: 30 seconds
    """
    # Validate language
    if data.language.lower() not in SUPPORTED_LANGUAGES:
        raise HTTPException(
            400,
            f"Unsupported language: {data.language}. Supported: {SUPPORTED_LANGUAGES}"
        )

    # Execute
    result = SandboxExecutor.execute(
        code=data.code,
        language=data.language.lower(),
        timeout=data.timeout
    )

    # Log execution
    user_id = request.headers.get("X-User-Id", "anonymous")
    _log_execution(
        user_id=user_id,
        language=data.language,
        success=result["success"],
        execution_time=result["execution_time"],
        output_preview=result["output"][:100] if result["output"] else ""
    )

    return ExecuteResponse(
        success=result["success"],
        output=result["output"],
        error=result.get("error"),
        execution_time=result["execution_time"],
        language=data.language.lower()
    )


@router.get("/templates")
async def get_templates(
    language: Optional[str] = None,
    category: Optional[str] = None
):
    """Get available code templates."""
    templates = DEFAULT_TEMPLATES

    if language:
        templates = [t for t in templates if t.language == language.lower()]

    if category:
        templates = [t for t in templates if t.category == category.lower()]

    return {
        "templates": [t.dict() for t in templates],
        "count": len(templates)
    }


@router.post("/save", response_model=SnippetResponse)
async def save_snippet(data: SaveSnippetRequest, request: Request):
    """Save a code snippet for later use."""
    user_id = request.headers.get("X-User-Id", "anonymous")

    snippet_id = f"snp_{uuid.uuid4().hex[:12]}"

    snippet = {
        "id": snippet_id,
        "user_id": user_id,
        "name": data.name,
        "code": data.code,
        "language": data.language.lower(),
        "description": data.description,
        "created_at": datetime.utcnow().isoformat()
    }

    _snippets[snippet_id] = snippet

    return SnippetResponse(
        id=snippet_id,
        name=data.name,
        language=data.language,
        description=data.description,
        created_at=snippet["created_at"]
    )


@router.get("/snippets")
async def list_snippets(request: Request):
    """List user's saved snippets."""
    user_id = request.headers.get("X-User-Id", "anonymous")

    user_snippets = [
        s for s in _snippets.values()
        if s["user_id"] == user_id
    ]

    return {
        "snippets": user_snippets,
        "count": len(user_snippets)
    }


@router.get("/snippets/{snippet_id}")
async def get_snippet(snippet_id: str, request: Request):
    """Get a specific snippet."""
    snippet = _snippets.get(snippet_id)

    if not snippet:
        raise HTTPException(404, "Snippet not found")

    return snippet


@router.delete("/snippets/{snippet_id}")
async def delete_snippet(snippet_id: str, request: Request):
    """Delete a snippet."""
    user_id = request.headers.get("X-User-Id", "anonymous")

    snippet = _snippets.get(snippet_id)
    if not snippet:
        raise HTTPException(404, "Snippet not found")

    if snippet["user_id"] != user_id:
        raise HTTPException(403, "Not authorized to delete this snippet")

    del _snippets[snippet_id]
    return {"message": "Snippet deleted", "id": snippet_id}


@router.get("/history")
async def get_execution_history(
    request: Request,
    limit: int = 50
):
    """Get execution history for current user."""
    user_id = request.headers.get("X-User-Id", "anonymous")

    user_history = [
        h for h in _execution_history
        if h["user_id"] == user_id
    ][-limit:]

    return {
        "history": [
            ExecutionHistoryItem(
                id=h["id"],
                language=h["language"],
                success=h["success"],
                execution_time=h["execution_time"],
                timestamp=h["timestamp"],
                output_preview=h["output_preview"]
            ).dict()
            for h in reversed(user_history)
        ],
        "count": len(user_history)
    }


@router.get("/languages")
async def get_supported_languages():
    """Get list of supported languages."""
    return {
        "languages": [
            {"id": "python", "name": "Python", "extension": ".py"},
            {"id": "javascript", "name": "JavaScript", "extension": ".js"},
            {"id": "bash", "name": "Bash", "extension": ".sh"}
        ]
    }


# =============================================================================
# HELPERS
# =============================================================================

def _log_execution(
    user_id: str,
    language: str,
    success: bool,
    execution_time: float,
    output_preview: str
):
    """Log an execution to history."""
    entry = {
        "id": f"exec_{uuid.uuid4().hex[:8]}",
        "user_id": user_id,
        "language": language,
        "success": success,
        "execution_time": execution_time,
        "timestamp": datetime.utcnow().isoformat(),
        "output_preview": output_preview
    }

    _execution_history.append(entry)

    # Keep last 1000 entries per user (rough limit)
    if len(_execution_history) > 10000:
        _execution_history.pop(0)
