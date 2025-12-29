# -*- coding: utf-8 -*-
"""
A/B Testing API Routes - Issue #71
===================================

API endpoints para gerenciamento de testes A/B de codigo gerado.
"""

from fastapi import APIRouter, HTTPException, Depends
from pydantic import BaseModel
from typing import List, Optional

from factory.database.connection import SessionLocal
from factory.database.models import Story, ABTest


router = APIRouter(prefix="/api", tags=["A/B Testing"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class ABTestCreate(BaseModel):
    """Request para criar teste A/B"""
    title: str
    description: Optional[str] = None
    num_variants: int = 3
    approaches: Optional[List[str]] = None
    requirements: Optional[str] = None
    test_code: Optional[str] = None


class ABTestSelectWinner(BaseModel):
    """Request para selecionar vencedor"""
    variant_id: str
    reason: Optional[str] = None


class GenerateVariantsRequest(BaseModel):
    """Request para gerar variantes"""
    requirements: str
    tech_stack: str = "python"


class RunTestsRequest(BaseModel):
    """Request para executar testes"""
    test_code: str


# =============================================================================
# API ENDPOINTS
# =============================================================================

@router.post("/stories/{story_id}/ab-test")
def create_ab_test(story_id: str, request: ABTestCreate):
    """
    Cria um novo teste A/B para uma story.

    Gera multiplas variantes de implementacao com abordagens diferentes:
    - simple: Codigo direto e facil de entender
    - optimized: Performance otimizada
    - robust: Tratamento de erros robusto
    - modular: Alta modularidade
    - functional: Estilo funcional
    - oop: Orientado a objetos

    Args:
        story_id: ID da story
        request: Dados do teste A/B

    Returns:
        Dados do teste A/B criado com variantes
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        # Verificar se story existe
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            raise HTTPException(404, "Story not found")

        # Criar teste A/B
        manager = ABTestManager(db)
        ab_test = manager.create_ab_test(
            story_id=story_id,
            title=request.title or f"A/B Test - {story.title}",
            description=request.description or story.description,
            num_variants=request.num_variants,
            approaches=request.approaches
        )

        # Se requirements fornecido, gerar variantes
        if request.requirements:
            ab_test = manager.generate_all_variants(
                ab_test["test_id"],
                request.requirements
            )

            # Se test_code fornecido, executar testes
            if request.test_code:
                ab_test = manager.run_tests_on_all_variants(
                    ab_test["test_id"],
                    request.test_code
                )

        return ab_test

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error creating A/B test: {str(e)}")
    finally:
        db.close()


@router.get("/stories/{story_id}/ab-tests")
def list_story_ab_tests(story_id: str):
    """
    Lista todos os testes A/B de uma story.

    Args:
        story_id: ID da story

    Returns:
        Lista de testes A/B
    """
    db = SessionLocal()
    try:
        tests = db.query(ABTest).filter(ABTest.story_id == story_id).all()
        return [t.to_dict() for t in tests]
    finally:
        db.close()


@router.get("/ab-tests/{test_id}")
def get_ab_test(test_id: str):
    """
    Retorna detalhes de um teste A/B.

    Args:
        test_id: ID do teste A/B

    Returns:
        Dados completos do teste A/B com variantes
    """
    db = SessionLocal()
    try:
        test = db.query(ABTest).filter(ABTest.test_id == test_id).first()
        if not test:
            raise HTTPException(404, "AB Test not found")
        return test.to_dict()
    finally:
        db.close()


@router.get("/ab-tests/{test_id}/compare")
def compare_ab_test_variants(test_id: str):
    """
    Compara todas as variantes de um teste A/B.

    Retorna metricas comparativas:
    - Testes passando
    - Taxa de sucesso
    - Tempo de execucao
    - Linhas de codigo
    - Complexidade ciclomatica
    - Score de legibilidade
    - Score total

    Args:
        test_id: ID do teste A/B

    Returns:
        Comparacao detalhada das variantes
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        comparison = manager.compare_variants(test_id)

        if "error" in comparison:
            raise HTTPException(404, comparison["error"])

        return comparison
    finally:
        db.close()


@router.post("/ab-tests/{test_id}/generate-variants")
def generate_ab_test_variants(test_id: str, request: GenerateVariantsRequest):
    """
    Gera codigo para todas as variantes de um teste A/B.

    Args:
        test_id: ID do teste A/B
        request: Requisitos e stack tecnologica

    Returns:
        Teste A/B atualizado com codigo gerado
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        result = manager.generate_all_variants(test_id, request.requirements, request.tech_stack)

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.post("/ab-tests/{test_id}/run-tests")
def run_ab_test_tests(test_id: str, request: RunTestsRequest):
    """
    Executa testes em todas as variantes.

    Args:
        test_id: ID do teste A/B
        request: Codigo de testes a executar

    Returns:
        Teste A/B atualizado com resultados dos testes e recomendacao
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        result = manager.run_tests_on_all_variants(test_id, request.test_code)

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.post("/ab-tests/{test_id}/select-winner")
def select_ab_test_winner(test_id: str, request: ABTestSelectWinner):
    """
    Seleciona manualmente o vencedor de um teste A/B.

    Args:
        test_id: ID do teste A/B
        request: ID da variante vencedora e justificativa

    Returns:
        Teste A/B atualizado com vencedor selecionado
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        result = manager.select_winner(test_id, request.variant_id, request.reason)

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.post("/ab-tests/{test_id}/auto-select")
def auto_select_ab_test_winner(test_id: str):
    """
    Seleciona automaticamente o vencedor baseado no score.

    A IA analisa as metricas e recomenda a melhor variante:
    - 40% peso: Testes passando
    - 25% peso: Legibilidade
    - 20% peso: Complexidade baixa
    - 15% peso: Codigo conciso

    Args:
        test_id: ID do teste A/B

    Returns:
        Teste A/B atualizado com vencedor auto-selecionado
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        result = manager.auto_select_winner(test_id)

        if "error" in result:
            raise HTTPException(400, result["error"])

        return result
    finally:
        db.close()


@router.get("/ab-tests/{test_id}/winner-code")
def get_ab_test_winner_code(test_id: str):
    """
    Retorna o codigo da variante vencedora.

    Args:
        test_id: ID do teste A/B

    Returns:
        Codigo da variante vencedora
    """
    db = SessionLocal()
    try:
        from factory.core.ab_testing import ABTestManager

        manager = ABTestManager(db)
        code = manager.get_winner_code(test_id)

        if code is None:
            raise HTTPException(404, "No winner selected or code not found")

        return {"code": code}
    finally:
        db.close()


@router.delete("/ab-tests/{test_id}")
def delete_ab_test(test_id: str):
    """
    Remove um teste A/B e todas as suas variantes.

    Args:
        test_id: ID do teste A/B

    Returns:
        Mensagem de confirmacao
    """
    db = SessionLocal()
    try:
        test = db.query(ABTest).filter(ABTest.test_id == test_id).first()
        if not test:
            raise HTTPException(404, "AB Test not found")

        db.delete(test)
        db.commit()
        return {"message": "AB Test deleted", "test_id": test_id}
    finally:
        db.close()


# =============================================================================
# FUNCAO PARA REGISTRAR ROUTER NO APP PRINCIPAL
# =============================================================================

def register_routes(app):
    """
    Registra as rotas de A/B testing no app FastAPI.

    Uso:
        from factory.api.ab_testing_routes import register_routes
        register_routes(app)
    """
    app.include_router(router)
