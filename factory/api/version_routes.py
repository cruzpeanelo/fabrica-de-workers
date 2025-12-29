# -*- coding: utf-8 -*-
"""
Version Control API Routes - Issue #58
======================================
Endpoints para versionamento e historico de codigo:
- Listar versoes de uma story
- Criar snapshots
- Diff entre versoes
- Rollback para versoes anteriores
- Branches e merge
"""
from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel
from typing import Optional, Dict, List

from factory.database.connection import SessionLocal
from factory.core.version_control import VersionControl, create_auto_snapshot


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/stories", tags=["Version Control"])


# =============================================================================
# SCHEMAS
# =============================================================================

class SnapshotCreate(BaseModel):
    """Schema para criar snapshot"""
    files: Dict[str, str]
    message: Optional[str] = None
    task_id: Optional[str] = None
    author: Optional[str] = "user"
    branch: Optional[str] = "main"


class RollbackRequest(BaseModel):
    """Schema para rollback"""
    target_hash: str
    create_backup: Optional[bool] = True


class BranchCreate(BaseModel):
    """Schema para criar branch"""
    branch_name: str
    from_hash: Optional[str] = None


class MergeRequest(BaseModel):
    """Schema para merge de branches"""
    source_branch: str
    target_branch: Optional[str] = "main"


# =============================================================================
# VERSION ENDPOINTS
# =============================================================================

@router.get("/{story_id}/versions")
def list_story_versions(
    story_id: str,
    branch: Optional[str] = None,
    limit: int = Query(50, le=100)
):
    """
    Lista versoes de codigo de uma story

    Retorna historico de snapshots com metadados (sem conteudo dos arquivos)
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        versions = vc.get_versions(story_id, branch=branch, limit=limit)
        return {
            "story_id": story_id,
            "branch": branch or "all",
            "versions": versions,
            "total": len(versions)
        }
    finally:
        db.close()


@router.get("/{story_id}/versions/{version_hash}")
def get_version_details(story_id: str, version_hash: str):
    """
    Retorna detalhes de uma versao especifica

    Inclui lista de arquivos e metadados
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        snapshot = vc.get_snapshot(version_hash)

        if not snapshot:
            raise HTTPException(404, "Versao nao encontrada")

        if snapshot.story_id != story_id:
            raise HTTPException(400, "Versao pertence a outra story")

        return {
            "version": snapshot.to_dict(),
            "files": list(snapshot.files.keys()),
            "files_count": len(snapshot.files)
        }
    finally:
        db.close()


@router.get("/{story_id}/versions/{version_hash}/file")
def get_version_file(
    story_id: str,
    version_hash: str,
    file_path: str = Query(..., description="Caminho do arquivo")
):
    """
    Retorna conteudo de um arquivo em uma versao especifica
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        snapshot = vc.get_snapshot(version_hash)

        if not snapshot:
            raise HTTPException(404, "Versao nao encontrada")

        if snapshot.story_id != story_id:
            raise HTTPException(400, "Versao pertence a outra story")

        if file_path not in snapshot.files:
            raise HTTPException(404, f"Arquivo nao encontrado: {file_path}")

        return {
            "version_hash": version_hash,
            "file_path": file_path,
            "content": snapshot.files[file_path],
            "hash": snapshot.file_hashes.get(file_path, "")
        }
    finally:
        db.close()


@router.post("/{story_id}/snapshot")
def create_story_snapshot(story_id: str, data: SnapshotCreate):
    """
    Cria um novo snapshot do codigo

    Automaticamente chamado durante geracao de codigo,
    mas tambem pode ser criado manualmente
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        snapshot = vc.create_snapshot(
            story_id=story_id,
            files=data.files,
            message=data.message,
            task_id=data.task_id,
            author=data.author,
            branch=data.branch
        )

        return snapshot.to_dict()
    finally:
        db.close()


# =============================================================================
# DIFF ENDPOINTS
# =============================================================================

@router.get("/{story_id}/diff")
def get_versions_diff(
    story_id: str,
    from_hash: str = Query(..., description="Hash da versao anterior"),
    to_hash: str = Query(..., description="Hash da versao atual"),
    context_lines: int = Query(3, le=10)
):
    """
    Calcula diff entre duas versoes

    Retorna diff unificado de todos os arquivos alterados
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        diff = vc.get_diff(from_hash, to_hash, context_lines=context_lines)

        if "error" in diff:
            raise HTTPException(404, diff["error"])

        return diff
    finally:
        db.close()


@router.get("/{story_id}/diff/file")
def get_file_diff(
    story_id: str,
    from_hash: str = Query(..., description="Hash da versao anterior"),
    to_hash: str = Query(..., description="Hash da versao atual"),
    file_path: str = Query(..., description="Caminho do arquivo"),
    context_lines: int = Query(5, le=20)
):
    """
    Calcula diff de um arquivo especifico entre versoes

    Retorna diff unificado e side-by-side
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        diff = vc.get_file_diff(from_hash, to_hash, file_path, context_lines=context_lines)

        if "error" in diff:
            raise HTTPException(404, diff["error"])

        return diff
    finally:
        db.close()


# =============================================================================
# ROLLBACK ENDPOINTS
# =============================================================================

@router.post("/{story_id}/rollback")
def rollback_story_version(story_id: str, data: RollbackRequest):
    """
    Reverte o codigo para uma versao anterior

    Cria backup automatico antes do rollback (pode ser desabilitado)
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        result = vc.rollback(
            story_id=story_id,
            target_hash=data.target_hash,
            create_backup=data.create_backup
        )

        if not result.get("success"):
            raise HTTPException(400, result.get("error", "Erro ao fazer rollback"))

        return result
    finally:
        db.close()


@router.get("/{story_id}/rollback/preview")
def preview_rollback(
    story_id: str,
    target_hash: str = Query(..., description="Hash da versao alvo")
):
    """
    Pre-visualiza as mudancas de um rollback sem aplica-lo

    Use para ver o que sera alterado antes de confirmar
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        preview = vc.preview_rollback(story_id, target_hash)

        if "error" in preview:
            raise HTTPException(404, preview["error"])

        return preview
    finally:
        db.close()


# =============================================================================
# BRANCH ENDPOINTS
# =============================================================================

@router.get("/{story_id}/branches")
def list_story_branches(story_id: str):
    """
    Lista todas as branches de uma story
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        branches = vc.list_branches(story_id)
        return {
            "story_id": story_id,
            "branches": branches,
            "total": len(branches)
        }
    finally:
        db.close()


@router.post("/{story_id}/branch")
def create_story_branch(story_id: str, data: BranchCreate):
    """
    Cria uma nova branch para experimentos

    A branch inicia com o codigo da versao especificada (ou ultima versao da main)
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        result = vc.create_branch(
            story_id=story_id,
            branch_name=data.branch_name,
            from_hash=data.from_hash
        )

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.post("/{story_id}/merge")
def merge_story_branches(story_id: str, data: MergeRequest):
    """
    Faz merge de uma branch na branch de destino

    Por padrao, faz merge na main. Conflitos sao resolvidos
    usando a versao da source branch.
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        result = vc.merge_branch(
            story_id=story_id,
            source_branch=data.source_branch,
            target_branch=data.target_branch
        )

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.get("/{story_id}/branches/compare")
def compare_story_branches(
    story_id: str,
    branch_a: str = Query(..., description="Primeira branch"),
    branch_b: str = Query(..., description="Segunda branch")
):
    """
    Compara duas branches lado a lado

    Util para revisar mudancas antes de fazer merge
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        comparison = vc.compare_branches(story_id, branch_a, branch_b)

        if "error" in comparison:
            raise HTTPException(404, comparison["error"])

        return comparison
    finally:
        db.close()


# =============================================================================
# TIMELINE ENDPOINT
# =============================================================================

@router.get("/{story_id}/timeline")
def get_version_timeline(
    story_id: str,
    include_branches: bool = Query(True, description="Incluir todas as branches")
):
    """
    Retorna timeline visual das versoes

    Formato adequado para renderizacao de grafico de historico
    """
    db = SessionLocal()
    try:
        vc = VersionControl(db_session=db)
        timeline = vc.get_timeline(story_id, include_branches=include_branches)
        return timeline
    finally:
        db.close()
