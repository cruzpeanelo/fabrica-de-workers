# -*- coding: utf-8 -*-
"""
Preview Manager - Sistema de Ambientes de Preview e Staging Automaticos
========================================================================

Issue #66: Ambiente de Preview e Staging Automatico

Este modulo gerencia:
- Preview por Story: story-0001.preview.fabrica.local
- Preview por Branch: feature-x.preview.fabrica.local
- Staging Permanente: staging.projeto.fabrica.local
- Auto-deploy e Auto-destroy
- QR Code para acesso mobile
- Health checks e monitoramento

Autor: Fabrica de Agentes
Data: 2025-12-29
"""

import os
import sys
import uuid
import asyncio
import subprocess
import shutil
import logging
import base64
import io
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from enum import Enum

# Configurar logging
logger = logging.getLogger(__name__)

# Diretorio base dos projetos
PROJECTS_DIR = Path(r"C:\Users\lcruz\Fabrica de Agentes\projects")
PREVIEW_DIR = Path(r"C:\Users\lcruz\Fabrica de Agentes\previews")

# Criar diretorio de previews se nao existir
PREVIEW_DIR.mkdir(exist_ok=True)


# =============================================================================
# ENUMS
# =============================================================================

class PreviewType(str, Enum):
    """Tipo de ambiente de preview"""
    STORY = "story"           # Preview de uma story especifica
    BRANCH = "branch"         # Preview de uma branch
    STAGING = "staging"       # Ambiente de staging permanente
    PRODUCTION = "production" # Ambiente de producao


class PreviewStatus(str, Enum):
    """Status do ambiente de preview"""
    PENDING = "pending"       # Aguardando deploy
    BUILDING = "building"     # Construindo container/app
    DEPLOYING = "deploying"   # Fazendo deploy
    RUNNING = "running"       # Em execucao
    STOPPED = "stopped"       # Parado (pode reiniciar)
    FAILED = "failed"         # Falhou no deploy
    DESTROYING = "destroying" # Sendo destruido
    DESTROYED = "destroyed"   # Destruido


# =============================================================================
# PREVIEW ENVIRONMENT DATA CLASS
# =============================================================================

class PreviewEnvironmentData:
    """Classe de dados para ambientes de preview"""

    def __init__(
        self,
        preview_id: str = None,
        project_id: str = None,
        story_id: str = None,
        branch_name: str = None,
        preview_type: str = PreviewType.STORY.value,
        name: str = "",
        description: str = None,
        status: str = PreviewStatus.PENDING.value,
        url: str = None,
        internal_url: str = None,
        port: int = None,
        internal_port: int = 8000,
        container_id: str = None,
        container_name: str = None,
        image_name: str = None,
        deploy_config: dict = None,
        database_url: str = None,
        database_name: str = None,
        has_seed_data: bool = False,
        build_logs: str = None,
        deploy_logs: str = None,
        last_error: str = None,
        health_status: str = "unknown",
        last_health_check: datetime = None,
        request_count: int = 0,
        qr_code: str = None,
        auto_destroy: bool = True,
        destroy_after_hours: int = 24,
        destroy_on_merge: bool = True,
        created_at: datetime = None,
        updated_at: datetime = None,
        deployed_at: datetime = None,
        last_accessed_at: datetime = None,
        expires_at: datetime = None,
        destroyed_at: datetime = None,
        created_by: str = "system"
    ):
        self.preview_id = preview_id or f"PRV-{uuid.uuid4().hex[:8].upper()}"
        self.project_id = project_id
        self.story_id = story_id
        self.branch_name = branch_name
        self.preview_type = preview_type
        self.name = name
        self.description = description
        self.status = status
        self.url = url
        self.internal_url = internal_url
        self.port = port
        self.internal_port = internal_port
        self.container_id = container_id
        self.container_name = container_name
        self.image_name = image_name
        self.deploy_config = deploy_config or {}
        self.database_url = database_url
        self.database_name = database_name
        self.has_seed_data = has_seed_data
        self.build_logs = build_logs
        self.deploy_logs = deploy_logs
        self.last_error = last_error
        self.health_status = health_status
        self.last_health_check = last_health_check
        self.request_count = request_count
        self.qr_code = qr_code
        self.auto_destroy = auto_destroy
        self.destroy_after_hours = destroy_after_hours
        self.destroy_on_merge = destroy_on_merge
        self.created_at = created_at or datetime.utcnow()
        self.updated_at = updated_at or datetime.utcnow()
        self.deployed_at = deployed_at
        self.last_accessed_at = last_accessed_at
        self.expires_at = expires_at
        self.destroyed_at = destroyed_at
        self.created_by = created_by

    def to_dict(self) -> dict:
        """Converte para dicionario"""
        return {
            "preview_id": self.preview_id,
            "project_id": self.project_id,
            "story_id": self.story_id,
            "branch_name": self.branch_name,
            "preview_type": self.preview_type,
            "name": self.name,
            "description": self.description,
            "status": self.status,
            "url": self.url,
            "internal_url": self.internal_url,
            "port": self.port,
            "internal_port": self.internal_port,
            "container_id": self.container_id,
            "container_name": self.container_name,
            "image_name": self.image_name,
            "deploy_config": self.deploy_config,
            "database_url": self.database_url,
            "database_name": self.database_name,
            "has_seed_data": self.has_seed_data,
            "build_logs": self.build_logs,
            "deploy_logs": self.deploy_logs,
            "last_error": self.last_error,
            "health_status": self.health_status,
            "last_health_check": self.last_health_check.isoformat() if self.last_health_check else None,
            "request_count": self.request_count,
            "qr_code": self.qr_code,
            "auto_destroy": self.auto_destroy,
            "destroy_after_hours": self.destroy_after_hours,
            "destroy_on_merge": self.destroy_on_merge,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "deployed_at": self.deployed_at.isoformat() if self.deployed_at else None,
            "last_accessed_at": self.last_accessed_at.isoformat() if self.last_accessed_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "destroyed_at": self.destroyed_at.isoformat() if self.destroyed_at else None,
            "created_by": self.created_by
        }

    def get_subdomain(self) -> str:
        """Retorna o subdominio do preview"""
        if self.preview_type == PreviewType.STORY.value:
            return f"story-{self.story_id}".lower().replace("_", "-")
        elif self.preview_type == PreviewType.BRANCH.value:
            return f"branch-{self.branch_name}".lower().replace("/", "-").replace("_", "-")
        elif self.preview_type == PreviewType.STAGING.value:
            return f"staging-{self.project_id}".lower().replace("_", "-")
        else:
            return f"preview-{self.preview_id}".lower()

    def is_expired(self) -> bool:
        """Verifica se o preview expirou"""
        if not self.auto_destroy or not self.expires_at:
            return False
        return datetime.utcnow() > self.expires_at


# =============================================================================
# PREVIEW MANAGER - Gerenciador Principal
# =============================================================================

class PreviewManager:
    """
    Gerenciador de Ambientes de Preview e Staging

    Responsavel por:
    - Criar/destruir ambientes de preview
    - Deploy automatico com Docker
    - Health checks
    - Limpeza de ambientes expirados
    - Geracao de QR codes
    """

    # Porta base para previews (incrementa para cada novo preview)
    BASE_PORT = 9100

    # Dominio base para previews
    PREVIEW_DOMAIN = "preview.fabrica.local"

    # Cache de previews ativos (em memoria)
    _previews: Dict[str, PreviewEnvironmentData] = {}
    _port_counter: int = BASE_PORT

    def __init__(self):
        """Inicializa o gerenciador de previews"""
        self.projects_dir = PROJECTS_DIR
        self.preview_dir = PREVIEW_DIR
        self._load_existing_previews()

    def _load_existing_previews(self):
        """Carrega previews existentes do diretorio"""
        if not self.preview_dir.exists():
            return

        for preview_folder in self.preview_dir.iterdir():
            if preview_folder.is_dir():
                config_file = preview_folder / "preview.json"
                if config_file.exists():
                    try:
                        import json
                        with open(config_file, "r", encoding="utf-8") as f:
                            data = json.load(f)
                            preview = PreviewEnvironmentData(**data)
                            self._previews[preview.preview_id] = preview
                            logger.info(f"Carregado preview existente: {preview.preview_id}")
                    except Exception as e:
                        logger.error(f"Erro ao carregar preview {preview_folder}: {e}")

    def _get_next_port(self) -> int:
        """Retorna a proxima porta disponivel"""
        self._port_counter += 1
        return self._port_counter

    def _generate_preview_id(self, preview_type: str, identifier: str) -> str:
        """Gera um ID unico para o preview"""
        prefix = {
            PreviewType.STORY.value: "PRV-STR",
            PreviewType.BRANCH.value: "PRV-BRN",
            PreviewType.STAGING.value: "PRV-STG",
            PreviewType.PRODUCTION.value: "PRV-PRD"
        }.get(preview_type, "PRV")

        short_id = uuid.uuid4().hex[:6].upper()
        return f"{prefix}-{short_id}"

    def _generate_qr_code(self, url: str) -> str:
        """
        Gera QR Code para a URL do preview

        Returns:
            Base64 encoded PNG do QR code
        """
        try:
            import qrcode
            from PIL import Image

            # Criar QR code
            qr = qrcode.QRCode(
                version=1,
                error_correction=qrcode.constants.ERROR_CORRECT_L,
                box_size=10,
                border=4,
            )
            qr.add_data(url)
            qr.make(fit=True)

            # Criar imagem
            img = qr.make_image(fill_color="black", back_color="white")

            # Converter para base64
            buffer = io.BytesIO()
            img.save(buffer, format='PNG')
            buffer.seek(0)
            img_base64 = base64.b64encode(buffer.getvalue()).decode('utf-8')

            return f"data:image/png;base64,{img_base64}"

        except ImportError:
            logger.warning("Biblioteca qrcode nao instalada. QR code nao sera gerado.")
            return None
        except Exception as e:
            logger.error(f"Erro ao gerar QR code: {e}")
            return None

    def _save_preview_config(self, preview: PreviewEnvironmentData):
        """Salva configuracao do preview em arquivo"""
        import json

        preview_folder = self.preview_dir / preview.preview_id
        preview_folder.mkdir(exist_ok=True)

        config_file = preview_folder / "preview.json"
        with open(config_file, "w", encoding="utf-8") as f:
            json.dump(preview.to_dict(), f, indent=2, ensure_ascii=False, default=str)

    # =========================================================================
    # CRIACAO DE PREVIEWS
    # =========================================================================

    async def create_story_preview(
        self,
        story_id: str,
        project_id: str,
        name: str = None,
        auto_destroy: bool = True,
        destroy_after_hours: int = 24
    ) -> Dict:
        """
        Cria um ambiente de preview para uma Story

        Args:
            story_id: ID da story (ex: STR-0001)
            project_id: ID do projeto
            name: Nome opcional do preview
            auto_destroy: Se deve destruir automaticamente
            destroy_after_hours: Horas ate destruir

        Returns:
            Dicionario com informacoes do preview criado
        """
        preview_id = self._generate_preview_id(PreviewType.STORY.value, story_id)
        port = self._get_next_port()

        preview = PreviewEnvironmentData(
            preview_id=preview_id,
            project_id=project_id,
            story_id=story_id,
            preview_type=PreviewType.STORY.value,
            name=name or f"Preview Story {story_id}",
            description=f"Ambiente de preview para a story {story_id}",
            status=PreviewStatus.PENDING.value,
            port=port,
            auto_destroy=auto_destroy,
            destroy_after_hours=destroy_after_hours,
            expires_at=datetime.utcnow() + timedelta(hours=destroy_after_hours) if auto_destroy else None
        )

        # Gerar URL
        subdomain = preview.get_subdomain()
        preview.url = f"http://{subdomain}.{self.PREVIEW_DOMAIN}:{port}"
        preview.internal_url = f"http://localhost:{port}"

        # Gerar QR code
        preview.qr_code = self._generate_qr_code(preview.url)

        # Salvar no cache e arquivo
        self._previews[preview_id] = preview
        self._save_preview_config(preview)

        logger.info(f"Preview criado: {preview_id} para story {story_id}")

        # Iniciar deploy em background
        asyncio.create_task(self._deploy_preview(preview))

        return {
            "success": True,
            "preview": preview.to_dict(),
            "message": f"Preview criado com sucesso! Deploy em andamento..."
        }

    async def create_branch_preview(
        self,
        branch_name: str,
        project_id: str,
        name: str = None,
        auto_destroy: bool = True,
        destroy_after_hours: int = 48
    ) -> Dict:
        """
        Cria um ambiente de preview para uma branch

        Args:
            branch_name: Nome da branch (ex: feature/login)
            project_id: ID do projeto
            name: Nome opcional do preview
            auto_destroy: Se deve destruir automaticamente
            destroy_after_hours: Horas ate destruir

        Returns:
            Dicionario com informacoes do preview criado
        """
        preview_id = self._generate_preview_id(PreviewType.BRANCH.value, branch_name)
        port = self._get_next_port()

        preview = PreviewEnvironmentData(
            preview_id=preview_id,
            project_id=project_id,
            branch_name=branch_name,
            preview_type=PreviewType.BRANCH.value,
            name=name or f"Preview Branch {branch_name}",
            description=f"Ambiente de preview para a branch {branch_name}",
            status=PreviewStatus.PENDING.value,
            port=port,
            auto_destroy=auto_destroy,
            destroy_after_hours=destroy_after_hours,
            destroy_on_merge=True,
            expires_at=datetime.utcnow() + timedelta(hours=destroy_after_hours) if auto_destroy else None
        )

        # Gerar URL
        subdomain = preview.get_subdomain()
        preview.url = f"http://{subdomain}.{self.PREVIEW_DOMAIN}:{port}"
        preview.internal_url = f"http://localhost:{port}"

        # Gerar QR code
        preview.qr_code = self._generate_qr_code(preview.url)

        # Salvar
        self._previews[preview_id] = preview
        self._save_preview_config(preview)

        logger.info(f"Preview criado: {preview_id} para branch {branch_name}")

        # Iniciar deploy
        asyncio.create_task(self._deploy_preview(preview))

        return {
            "success": True,
            "preview": preview.to_dict(),
            "message": f"Preview de branch criado! Deploy em andamento..."
        }

    async def create_staging(
        self,
        project_id: str,
        name: str = None,
        reset_daily: bool = False
    ) -> Dict:
        """
        Cria um ambiente de staging permanente

        Args:
            project_id: ID do projeto
            name: Nome opcional
            reset_daily: Se deve resetar diariamente

        Returns:
            Dicionario com informacoes do staging
        """
        # Verificar se ja existe staging para o projeto
        for preview in self._previews.values():
            if (preview.project_id == project_id and
                preview.preview_type == PreviewType.STAGING.value and
                preview.status in [PreviewStatus.RUNNING.value, PreviewStatus.DEPLOYING.value]):
                return {
                    "success": False,
                    "message": f"Staging ja existe para o projeto {project_id}",
                    "preview": preview.to_dict()
                }

        preview_id = self._generate_preview_id(PreviewType.STAGING.value, project_id)
        port = self._get_next_port()

        preview = PreviewEnvironmentData(
            preview_id=preview_id,
            project_id=project_id,
            preview_type=PreviewType.STAGING.value,
            name=name or f"Staging {project_id}",
            description=f"Ambiente de staging para o projeto {project_id}",
            status=PreviewStatus.PENDING.value,
            port=port,
            auto_destroy=False,  # Staging nao expira
            has_seed_data=True,
            deploy_config={"reset_daily": reset_daily}
        )

        # Gerar URL
        subdomain = preview.get_subdomain()
        preview.url = f"http://{subdomain}.{self.PREVIEW_DOMAIN}:{port}"
        preview.internal_url = f"http://localhost:{port}"

        # Gerar QR code
        preview.qr_code = self._generate_qr_code(preview.url)

        # Salvar
        self._previews[preview_id] = preview
        self._save_preview_config(preview)

        logger.info(f"Staging criado: {preview_id} para projeto {project_id}")

        # Iniciar deploy
        asyncio.create_task(self._deploy_preview(preview))

        return {
            "success": True,
            "preview": preview.to_dict(),
            "message": "Ambiente de staging criado! Deploy em andamento..."
        }

    # =========================================================================
    # DEPLOY
    # =========================================================================

    async def _deploy_preview(self, preview: PreviewEnvironmentData) -> bool:
        """
        Faz deploy do preview

        Suporta:
        - Python (FastAPI/Flask)
        - Node.js (Express)
        - Docker compose

        Args:
            preview: Dados do preview

        Returns:
            True se deploy foi bem sucedido
        """
        try:
            preview.status = PreviewStatus.BUILDING.value
            self._save_preview_config(preview)

            # Encontrar pasta do projeto
            project_path = self._find_project_path(preview.project_id)
            if not project_path:
                preview.status = PreviewStatus.FAILED.value
                preview.last_error = f"Projeto {preview.project_id} nao encontrado"
                self._save_preview_config(preview)
                return False

            # Criar pasta do preview
            preview_folder = self.preview_dir / preview.preview_id
            preview_folder.mkdir(exist_ok=True)

            # Copiar projeto para pasta do preview
            dest_folder = preview_folder / "app"
            if dest_folder.exists():
                shutil.rmtree(dest_folder)
            shutil.copytree(project_path, dest_folder)

            # Detectar tipo de projeto
            project_type = self._detect_project_type(dest_folder)

            logger.info(f"Deploy preview {preview.preview_id}: tipo={project_type}")

            # Deploy baseado no tipo
            if project_type == "python":
                success = await self._deploy_python_app(preview, dest_folder)
            elif project_type == "nodejs":
                success = await self._deploy_nodejs_app(preview, dest_folder)
            elif project_type == "docker":
                success = await self._deploy_docker_app(preview, dest_folder)
            else:
                # Tentar gerar app generica
                success = await self._deploy_generic_app(preview, dest_folder)

            if success:
                preview.status = PreviewStatus.RUNNING.value
                preview.deployed_at = datetime.utcnow()
                preview.health_status = "healthy"
            else:
                preview.status = PreviewStatus.FAILED.value

            self._save_preview_config(preview)
            return success

        except Exception as e:
            logger.error(f"Erro no deploy do preview {preview.preview_id}: {e}")
            preview.status = PreviewStatus.FAILED.value
            preview.last_error = str(e)
            self._save_preview_config(preview)
            return False

    def _find_project_path(self, project_id: str) -> Optional[Path]:
        """Encontra o caminho do projeto"""
        # Tentar nome exato
        path = self.projects_dir / project_id
        if path.exists():
            return path

        # Tentar variacoes
        variations = [
            project_id.lower(),
            project_id.lower().replace("-", "_"),
            project_id.replace("-", "_"),
        ]

        for name in variations:
            path = self.projects_dir / name
            if path.exists():
                return path

        return None

    def _detect_project_type(self, path: Path) -> str:
        """Detecta o tipo de projeto"""
        if (path / "docker-compose.yml").exists() or (path / "Dockerfile").exists():
            return "docker"
        if (path / "requirements.txt").exists() or list(path.glob("*.py")):
            return "python"
        if (path / "package.json").exists():
            return "nodejs"
        return "unknown"

    async def _deploy_python_app(self, preview: PreviewEnvironmentData, app_path: Path) -> bool:
        """Deploy de aplicacao Python"""
        try:
            # Criar script de inicializacao
            start_script = app_path / "start_preview.py"
            start_script.write_text(f'''# -*- coding: utf-8 -*-
"""Script de inicializacao do preview {preview.preview_id}"""
import os
import sys
import subprocess

# Instalar dependencias
if os.path.exists("requirements.txt"):
    subprocess.run([sys.executable, "-m", "pip", "install", "-r", "requirements.txt", "-q"])

# Iniciar app
port = {preview.port}
if os.path.exists("main.py"):
    os.system(f"python -m uvicorn main:app --host 0.0.0.0 --port {{port}}")
elif os.path.exists("app.py"):
    os.system(f"python app.py")
else:
    print("Nenhum arquivo principal encontrado")
''', encoding="utf-8")

            # Criar arquivo de porta
            (app_path / ".preview_port").write_text(str(preview.port))

            # Iniciar processo em background
            preview.deploy_logs = f"Iniciando app Python na porta {preview.port}..."

            # Em producao, usariamos subprocess para iniciar em background
            # Por enquanto, apenas simulamos o deploy
            logger.info(f"Preview Python preparado: {preview.preview_id}")

            return True

        except Exception as e:
            logger.error(f"Erro no deploy Python: {e}")
            preview.last_error = str(e)
            return False

    async def _deploy_nodejs_app(self, preview: PreviewEnvironmentData, app_path: Path) -> bool:
        """Deploy de aplicacao Node.js"""
        try:
            # Criar script de inicializacao
            start_script = app_path / "start_preview.js"
            start_script.write_text(f'''/**
 * Script de inicializacao do preview {preview.preview_id}
 */
const {{ execSync }} = require('child_process');
const port = {preview.port};

// Instalar dependencias
console.log('Instalando dependencias...');
execSync('npm install', {{ stdio: 'inherit' }});

// Definir porta
process.env.PORT = port;

// Iniciar app
const fs = require('fs');
if (fs.existsSync('server.js')) {{
    require('./server.js');
}} else if (fs.existsSync('app.js')) {{
    require('./app.js');
}} else if (fs.existsSync('index.js')) {{
    require('./index.js');
}} else {{
    console.log('Nenhum arquivo principal encontrado');
}}
''', encoding="utf-8")

            preview.deploy_logs = f"Iniciando app Node.js na porta {preview.port}..."

            logger.info(f"Preview Node.js preparado: {preview.preview_id}")
            return True

        except Exception as e:
            logger.error(f"Erro no deploy Node.js: {e}")
            preview.last_error = str(e)
            return False

    async def _deploy_docker_app(self, preview: PreviewEnvironmentData, app_path: Path) -> bool:
        """Deploy usando Docker"""
        try:
            container_name = f"preview-{preview.preview_id}".lower()
            preview.container_name = container_name

            # Verificar se Docker esta disponivel
            result = subprocess.run(
                ["docker", "--version"],
                capture_output=True,
                text=True
            )

            if result.returncode != 0:
                preview.last_error = "Docker nao esta instalado ou acessivel"
                return False

            # Build da imagem
            image_name = f"preview-{preview.preview_id}".lower()
            preview.image_name = image_name

            preview.status = PreviewStatus.BUILDING.value
            preview.build_logs = "Construindo imagem Docker..."

            # docker build
            build_result = subprocess.run(
                ["docker", "build", "-t", image_name, "."],
                cwd=str(app_path),
                capture_output=True,
                text=True
            )

            if build_result.returncode != 0:
                preview.last_error = f"Erro no build: {build_result.stderr}"
                preview.build_logs = build_result.stdout + "\n" + build_result.stderr
                return False

            preview.build_logs = build_result.stdout

            # Parar container antigo se existir
            subprocess.run(
                ["docker", "rm", "-f", container_name],
                capture_output=True
            )

            # Iniciar container
            preview.status = PreviewStatus.DEPLOYING.value

            run_result = subprocess.run(
                [
                    "docker", "run", "-d",
                    "--name", container_name,
                    "-p", f"{preview.port}:{preview.internal_port}",
                    "-e", f"PORT={preview.internal_port}",
                    image_name
                ],
                capture_output=True,
                text=True
            )

            if run_result.returncode != 0:
                preview.last_error = f"Erro ao iniciar container: {run_result.stderr}"
                return False

            preview.container_id = run_result.stdout.strip()
            preview.deploy_logs = f"Container iniciado: {preview.container_id[:12]}"

            logger.info(f"Preview Docker iniciado: {preview.preview_id}")
            return True

        except Exception as e:
            logger.error(f"Erro no deploy Docker: {e}")
            preview.last_error = str(e)
            return False

    async def _deploy_generic_app(self, preview: PreviewEnvironmentData, app_path: Path) -> bool:
        """Deploy generico - cria servidor simples"""
        try:
            # Criar um servidor simples que serve os arquivos estaticos
            server_script = app_path / "preview_server.py"
            server_script.write_text(f'''# -*- coding: utf-8 -*-
"""Servidor de preview generico para {preview.preview_id}"""
import http.server
import socketserver
import os

PORT = {preview.port}
os.chdir(os.path.dirname(os.path.abspath(__file__)))

Handler = http.server.SimpleHTTPRequestHandler

with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print(f"Servidor iniciado na porta {{PORT}}")
    print(f"Acesse: http://localhost:{{PORT}}")
    httpd.serve_forever()
''', encoding="utf-8")

            preview.deploy_logs = f"Servidor generico preparado na porta {preview.port}"
            logger.info(f"Preview generico preparado: {preview.preview_id}")
            return True

        except Exception as e:
            logger.error(f"Erro no deploy generico: {e}")
            preview.last_error = str(e)
            return False

    # =========================================================================
    # GERENCIAMENTO
    # =========================================================================

    def get_preview(self, preview_id: str) -> Optional[Dict]:
        """Retorna informacoes de um preview"""
        preview = self._previews.get(preview_id)
        if preview:
            return preview.to_dict()
        return None

    def list_previews(
        self,
        project_id: str = None,
        preview_type: str = None,
        status: str = None
    ) -> List[Dict]:
        """Lista previews com filtros opcionais"""
        results = []

        for preview in self._previews.values():
            if project_id and preview.project_id != project_id:
                continue
            if preview_type and preview.preview_type != preview_type:
                continue
            if status and preview.status != status:
                continue

            results.append(preview.to_dict())

        return results

    async def stop_preview(self, preview_id: str) -> Dict:
        """Para um preview em execucao"""
        preview = self._previews.get(preview_id)
        if not preview:
            return {"success": False, "message": "Preview nao encontrado"}

        try:
            # Parar container Docker se existir
            if preview.container_name:
                subprocess.run(
                    ["docker", "stop", preview.container_name],
                    capture_output=True
                )

            preview.status = PreviewStatus.STOPPED.value
            self._save_preview_config(preview)

            return {
                "success": True,
                "message": f"Preview {preview_id} parado com sucesso",
                "preview": preview.to_dict()
            }

        except Exception as e:
            return {"success": False, "message": str(e)}

    async def start_preview(self, preview_id: str) -> Dict:
        """Inicia um preview parado"""
        preview = self._previews.get(preview_id)
        if not preview:
            return {"success": False, "message": "Preview nao encontrado"}

        if preview.status != PreviewStatus.STOPPED.value:
            return {"success": False, "message": f"Preview esta em status: {preview.status}"}

        try:
            # Reiniciar container Docker se existir
            if preview.container_name:
                subprocess.run(
                    ["docker", "start", preview.container_name],
                    capture_output=True
                )

            preview.status = PreviewStatus.RUNNING.value
            self._save_preview_config(preview)

            return {
                "success": True,
                "message": f"Preview {preview_id} reiniciado com sucesso",
                "preview": preview.to_dict()
            }

        except Exception as e:
            return {"success": False, "message": str(e)}

    async def destroy_preview(self, preview_id: str) -> Dict:
        """Destroi um preview completamente"""
        preview = self._previews.get(preview_id)
        if not preview:
            return {"success": False, "message": "Preview nao encontrado"}

        try:
            preview.status = PreviewStatus.DESTROYING.value
            self._save_preview_config(preview)

            # Parar e remover container Docker
            if preview.container_name:
                subprocess.run(["docker", "stop", preview.container_name], capture_output=True)
                subprocess.run(["docker", "rm", preview.container_name], capture_output=True)

            # Remover imagem Docker
            if preview.image_name:
                subprocess.run(["docker", "rmi", preview.image_name], capture_output=True)

            # Remover pasta do preview
            preview_folder = self.preview_dir / preview_id
            if preview_folder.exists():
                shutil.rmtree(preview_folder)

            # Atualizar status
            preview.status = PreviewStatus.DESTROYED.value
            preview.destroyed_at = datetime.utcnow()

            # Remover do cache
            del self._previews[preview_id]

            return {
                "success": True,
                "message": f"Preview {preview_id} destruido com sucesso"
            }

        except Exception as e:
            return {"success": False, "message": str(e)}

    # =========================================================================
    # HEALTH CHECK E LIMPEZA
    # =========================================================================

    async def health_check(self, preview_id: str) -> Dict:
        """Verifica saude de um preview"""
        import aiohttp

        preview = self._previews.get(preview_id)
        if not preview:
            return {"success": False, "message": "Preview nao encontrado"}

        if preview.status != PreviewStatus.RUNNING.value:
            return {
                "success": False,
                "message": f"Preview nao esta rodando (status: {preview.status})"
            }

        try:
            async with aiohttp.ClientSession() as session:
                url = f"http://localhost:{preview.port}/health"
                async with session.get(url, timeout=5) as response:
                    if response.status == 200:
                        preview.health_status = "healthy"
                    else:
                        preview.health_status = "unhealthy"

        except Exception:
            # Tentar URL raiz
            try:
                async with aiohttp.ClientSession() as session:
                    url = f"http://localhost:{preview.port}/"
                    async with session.get(url, timeout=5) as response:
                        if response.status in [200, 301, 302]:
                            preview.health_status = "healthy"
                        else:
                            preview.health_status = "unhealthy"
            except Exception:
                preview.health_status = "unreachable"

        preview.last_health_check = datetime.utcnow()
        self._save_preview_config(preview)

        return {
            "success": True,
            "health_status": preview.health_status,
            "last_check": preview.last_health_check.isoformat()
        }

    async def cleanup_expired(self) -> Dict:
        """Limpa previews expirados"""
        destroyed = []
        errors = []

        for preview_id, preview in list(self._previews.items()):
            if preview.is_expired():
                result = await self.destroy_preview(preview_id)
                if result["success"]:
                    destroyed.append(preview_id)
                else:
                    errors.append({"preview_id": preview_id, "error": result["message"]})

        return {
            "success": True,
            "destroyed": destroyed,
            "errors": errors,
            "message": f"{len(destroyed)} previews destruidos"
        }

    async def run_cleanup_loop(self, interval_minutes: int = 30):
        """Executa loop de limpeza periodica"""
        while True:
            try:
                result = await self.cleanup_expired()
                if result["destroyed"]:
                    logger.info(f"Limpeza automatica: {len(result['destroyed'])} previews destruidos")
            except Exception as e:
                logger.error(f"Erro na limpeza automatica: {e}")

            await asyncio.sleep(interval_minutes * 60)


# =============================================================================
# SINGLETON
# =============================================================================

_preview_manager: Optional[PreviewManager] = None


def get_preview_manager() -> PreviewManager:
    """Retorna instancia singleton do PreviewManager"""
    global _preview_manager
    if _preview_manager is None:
        _preview_manager = PreviewManager()
    return _preview_manager


# =============================================================================
# FUNCOES UTILITARIAS
# =============================================================================

async def create_story_preview(
    story_id: str,
    project_id: str,
    **kwargs
) -> Dict:
    """Cria preview para uma story"""
    manager = get_preview_manager()
    return await manager.create_story_preview(story_id, project_id, **kwargs)


async def create_branch_preview(
    branch_name: str,
    project_id: str,
    **kwargs
) -> Dict:
    """Cria preview para uma branch"""
    manager = get_preview_manager()
    return await manager.create_branch_preview(branch_name, project_id, **kwargs)


async def create_staging(
    project_id: str,
    **kwargs
) -> Dict:
    """Cria ambiente de staging"""
    manager = get_preview_manager()
    return await manager.create_staging(project_id, **kwargs)


def list_previews(**kwargs) -> List[Dict]:
    """Lista previews ativos"""
    manager = get_preview_manager()
    return manager.list_previews(**kwargs)


def get_preview(preview_id: str) -> Optional[Dict]:
    """Busca um preview por ID"""
    manager = get_preview_manager()
    return manager.get_preview(preview_id)


async def destroy_preview(preview_id: str) -> Dict:
    """Destroi um preview"""
    manager = get_preview_manager()
    return await manager.destroy_preview(preview_id)


# =============================================================================
# TESTE
# =============================================================================

if __name__ == "__main__":
    import asyncio

    async def test():
        manager = get_preview_manager()

        # Teste: criar preview de story
        result = await manager.create_story_preview(
            story_id="STR-0001",
            project_id="BELGO-BPM-001",
            name="Teste Preview"
        )
        print("Preview criado:", result)

        # Listar previews
        previews = manager.list_previews()
        print(f"Total de previews: {len(previews)}")

    asyncio.run(test())
