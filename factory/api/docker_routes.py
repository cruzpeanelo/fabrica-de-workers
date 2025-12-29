# -*- coding: utf-8 -*-
"""
Docker Routes - Endpoints de API para deploy Docker
====================================================

Este modulo define os endpoints REST para deploy Docker das aplicacoes
geradas pela Fabrica de Agentes.

Issue #77: App Generator - Deploy com Docker

Endpoints:
- GET  /api/projects/{id}/docker-status    - Status do Docker
- POST /api/projects/{id}/docker-generate  - Gerar arquivos Docker
- POST /api/projects/{id}/docker-build     - Build da imagem
- POST /api/projects/{id}/docker-run       - Iniciar containers
- POST /api/projects/{id}/docker-stop      - Parar containers
- GET  /api/projects/{id}/docker-logs      - Logs dos containers
- POST /api/projects/{id}/docker-deploy    - Deploy completo
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional

from factory.core.docker_generator import (
    DockerGenerator,
    generate_docker,
    build_docker,
    run_docker,
    stop_docker,
    docker_logs,
    docker_status,
    deploy_docker
)

# Criar router
router = APIRouter(prefix="/api/projects", tags=["docker"])


class DockerDeployRequest(BaseModel):
    """Request para deploy Docker."""
    database: str = "postgresql"
    include_nginx: bool = False


@router.get("/{project_id}/docker-status")
def get_docker_status(project_id: str):
    """
    Verifica status Docker do projeto.

    Retorna informacoes sobre:
    - Se os arquivos Docker existem
    - Se os containers estao rodando
    - URL da aplicacao (se rodando)
    """
    try:
        result = docker_status(project_id)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao verificar status Docker: {str(e)}")


@router.post("/{project_id}/docker-generate")
def generate_docker_files(project_id: str, request: DockerDeployRequest):
    """
    Gera arquivos Docker para o projeto.

    Arquivos gerados:
    - Dockerfile (otimizado por tipo de projeto)
    - docker-compose.yml (com banco de dados)
    - .dockerignore
    - .env.docker
    - Scripts de conveniencia (docker-build.bat, docker-run.bat, etc)
    """
    try:
        generator = DockerGenerator(project_id)
        result = generator.generate_docker_files(
            database=request.database,
            include_nginx=request.include_nginx
        )
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao gerar arquivos Docker: {str(e)}")


@router.post("/{project_id}/docker-build")
def build_docker_image(project_id: str):
    """
    Executa docker build para o projeto.

    Cria uma imagem Docker otimizada com multi-stage build.
    """
    try:
        result = build_docker(project_id)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro no docker build: {str(e)}")


@router.post("/{project_id}/docker-run")
def run_docker_containers(project_id: str):
    """
    Inicia containers Docker do projeto.

    Executa docker-compose up -d para iniciar todos os servicos
    (app, banco de dados, nginx se configurado).
    """
    try:
        result = run_docker(project_id)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao iniciar containers: {str(e)}")


@router.post("/{project_id}/docker-stop")
def stop_docker_containers(project_id: str):
    """
    Para containers Docker do projeto.

    Executa docker-compose down para parar e remover containers.
    """
    try:
        result = stop_docker(project_id)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao parar containers: {str(e)}")


@router.get("/{project_id}/docker-logs")
def get_docker_logs_endpoint(project_id: str, lines: int = 100):
    """
    Obtem logs Docker do projeto.

    Args:
        project_id: ID do projeto
        lines: Numero de linhas a retornar (default: 100)
    """
    try:
        result = docker_logs(project_id, lines=lines)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao obter logs: {str(e)}")


@router.post("/{project_id}/docker-deploy")
def deploy_docker_project(project_id: str, request: DockerDeployRequest):
    """
    Deploy completo com Docker.

    Executa em sequencia:
    1. Gerar arquivos Docker
    2. Build da imagem
    3. Iniciar containers

    Retorna URL da aplicacao se bem sucedido.
    """
    try:
        result = deploy_docker(project_id, database=request.database)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro no deploy Docker: {str(e)}")
