# -*- coding: utf-8 -*-
"""
Security API Routes - Analise de Seguranca (Issue #57)
======================================================
Fabrica de Agentes

Endpoints para analise de seguranca SAST/DAST.
"""
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional
from datetime import datetime

from factory.core.security_analyzer import (
    SecurityAnalyzer, SecurityScanConfig, ScanType,
    scan_project, get_security_badge
)

router = APIRouter(prefix="/api", tags=["security"])


class SecurityScanRequest(BaseModel):
    use_bandit: Optional[bool] = True
    include_info: Optional[bool] = False
    scan_dependencies: Optional[bool] = False


@router.post("/projects/{project_id}/security-scan")
async def run_security_scan(project_id: str, request: Optional[SecurityScanRequest] = None):
    """
    Executa analise de seguranca SAST no projeto.

    Verifica:
    - SQL Injection, XSS, Command Injection
    - Hardcoded secrets (passwords, API keys, tokens)
    - Unsafe deserialization
    - Path traversal
    - OWASP Top 10 vulnerabilities

    Returns:
        SecurityReport com score, vulnerabilidades e recomendacoes
    """
    try:
        # Configurar scan
        scan_types = [ScanType.SAST]
        if request and request.scan_dependencies:
            scan_types.append(ScanType.SCA)

        config = SecurityScanConfig(
            scan_types=scan_types,
            use_bandit=request.use_bandit if request else True,
            include_info=request.include_info if request else False,
            verbose=False
        )

        # Executar scan
        analyzer = SecurityAnalyzer(config)
        report = await analyzer.analyze(project_id)

        return report.to_dict()

    except Exception as e:
        raise HTTPException(500, f"Erro ao executar scan de seguranca: {str(e)}")


@router.get("/projects/{project_id}/security-status")
def get_security_status(project_id: str):
    """
    Retorna status rapido de seguranca do projeto (ultimo scan ou estimativa).
    """
    return {
        "project_id": project_id,
        "has_scan": False,
        "message": "Nenhum scan de seguranca realizado ainda. Execute POST /api/projects/{id}/security-scan"
    }


@router.get("/security/badge/{project_id}")
def get_project_security_badge(project_id: str, score: int = 100):
    """
    Retorna dados do badge de seguranca para exibicao no dashboard.
    """
    badge = get_security_badge(score)
    return {
        "project_id": project_id,
        "badge": badge,
        "svg": f'''<svg xmlns="http://www.w3.org/2000/svg" width="90" height="20">
            <rect width="90" height="20" fill="{badge['color']}" rx="3"/>
            <text x="45" y="14" fill="{badge['text_color']}" font-size="11" text-anchor="middle" font-family="Arial">
                {badge['label']} {score}%
            </text>
        </svg>'''
    }


# Para uso como modulo standalone
if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI

    app = FastAPI(title="Security API")
    app.include_router(router)

    uvicorn.run(app, host="0.0.0.0", port=8001)
