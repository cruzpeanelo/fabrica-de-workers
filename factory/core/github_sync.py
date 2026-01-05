# -*- coding: utf-8 -*-
"""
GitHub Code Sync - Sincronizacao de Codigo
==========================================

Modulo para sincronizacao bidirecional de codigo com GitHub.
Complementa a integracao existente de Issues (factory/integrations/github.py).

Funcionalidades:
- Push automatico de codigo gerado
- Pull de changes externos
- Branch por story/feature
- PR automatico
- Historico de versoes
- Rollback

Inspirado no Base44.app - GitHub Sync
"""

import asyncio
import json
import logging
import os
import shutil
import subprocess
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional
import uuid

logger = logging.getLogger(__name__)


class SyncStatus(str, Enum):
    """Status da sincronizacao"""
    IDLE = "idle"
    SYNCING = "syncing"
    PUSHING = "pushing"
    PULLING = "pulling"
    CONFLICT = "conflict"
    ERROR = "error"
    SUCCESS = "success"


class BranchStrategy(str, Enum):
    """Estrategia de branches"""
    SINGLE = "single"           # Tudo em main/master
    FEATURE = "feature"         # Branch por story (feature/STR-001)
    GITFLOW = "gitflow"         # develop, feature/*, release/*, hotfix/*


@dataclass
class GitConfig:
    """Configuracao do Git"""
    remote_url: str = ""
    branch: str = "main"
    branch_strategy: BranchStrategy = BranchStrategy.FEATURE
    auto_push: bool = True
    auto_pull: bool = True
    commit_author: str = "Plataforma E Bot"
    commit_email: str = "bot@plataformae.app"


@dataclass
class CommitInfo:
    """Informacoes de um commit"""
    sha: str
    message: str
    author: str
    date: datetime
    files_changed: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "sha": self.sha,
            "message": self.message,
            "author": self.author,
            "date": self.date.isoformat(),
            "files_changed": self.files_changed
        }


@dataclass
class SyncResult:
    """Resultado de uma operacao de sync"""
    success: bool
    status: SyncStatus
    message: str = ""
    commits_pushed: int = 0
    commits_pulled: int = 0
    files_changed: List[str] = field(default_factory=list)
    conflicts: List[str] = field(default_factory=list)
    pr_url: Optional[str] = None
    error: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "status": self.status.value,
            "message": self.message,
            "commits_pushed": self.commits_pushed,
            "commits_pulled": self.commits_pulled,
            "files_changed": self.files_changed,
            "conflicts": self.conflicts,
            "pr_url": self.pr_url,
            "error": self.error
        }


@dataclass
class VersionSnapshot:
    """Snapshot de uma versao do projeto"""
    version_id: str
    project_id: str
    commit_sha: str
    branch: str
    message: str
    created_at: datetime
    created_by: str
    files_count: int
    can_rollback: bool = True

    def to_dict(self) -> Dict[str, Any]:
        return {
            "version_id": self.version_id,
            "project_id": self.project_id,
            "commit_sha": self.commit_sha,
            "branch": self.branch,
            "message": self.message,
            "created_at": self.created_at.isoformat(),
            "created_by": self.created_by,
            "files_count": self.files_count,
            "can_rollback": self.can_rollback
        }


class GitHubSync:
    """
    Gerenciador de sincronizacao de codigo com GitHub.

    Responsavel por:
    - Inicializar repositorios Git em projetos
    - Push/Pull de codigo
    - Criacao de branches por feature
    - Criacao automatica de PRs
    - Historico de versoes
    - Rollback para versoes anteriores
    """

    def __init__(self, projects_dir: str = "projects", data_dir: str = "factory/state/github"):
        """
        Inicializa o sync manager.

        Args:
            projects_dir: Diretorio base dos projetos
            data_dir: Diretorio para dados de sync
        """
        self.projects_dir = Path(projects_dir)
        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(parents=True, exist_ok=True)

        # Configuracoes por projeto
        self.configs: Dict[str, GitConfig] = {}
        self.versions: Dict[str, List[VersionSnapshot]] = {}

        # GitHub token
        self.github_token = os.getenv("GITHUB_TOKEN", "")
        self.github_owner = os.getenv("GITHUB_OWNER", "")

        self._load_configs()

    def _load_configs(self):
        """Carrega configuracoes salvas"""
        config_file = self.data_dir / "sync_configs.json"
        if config_file.exists():
            try:
                with open(config_file, "r") as f:
                    data = json.load(f)
                    for project_id, cfg in data.get("configs", {}).items():
                        self.configs[project_id] = GitConfig(
                            remote_url=cfg.get("remote_url", ""),
                            branch=cfg.get("branch", "main"),
                            branch_strategy=BranchStrategy(cfg.get("branch_strategy", "feature")),
                            auto_push=cfg.get("auto_push", True),
                            auto_pull=cfg.get("auto_pull", True)
                        )
            except Exception as e:
                logger.error(f"Error loading sync configs: {e}")

    def _save_configs(self):
        """Salva configuracoes"""
        config_file = self.data_dir / "sync_configs.json"
        try:
            data = {
                "configs": {
                    pid: {
                        "remote_url": cfg.remote_url,
                        "branch": cfg.branch,
                        "branch_strategy": cfg.branch_strategy.value,
                        "auto_push": cfg.auto_push,
                        "auto_pull": cfg.auto_pull
                    }
                    for pid, cfg in self.configs.items()
                },
                "updated_at": datetime.now().isoformat()
            }
            with open(config_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.error(f"Error saving sync configs: {e}")

    def _run_git(self, project_path: Path, *args, check: bool = True) -> subprocess.CompletedProcess:
        """Executa comando git"""
        cmd = ["git"] + list(args)
        result = subprocess.run(
            cmd,
            cwd=project_path,
            capture_output=True,
            text=True,
            check=False
        )
        if check and result.returncode != 0:
            raise subprocess.CalledProcessError(result.returncode, cmd, result.stdout, result.stderr)
        return result

    def init_repo(self, project_id: str, remote_url: Optional[str] = None) -> SyncResult:
        """
        Inicializa repositorio Git em um projeto.

        Args:
            project_id: ID do projeto
            remote_url: URL do repositorio remoto (opcional)

        Returns:
            SyncResult com status da operacao
        """
        project_path = self.projects_dir / project_id

        if not project_path.exists():
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Projeto {project_id} nao encontrado"
            )

        try:
            git_dir = project_path / ".git"

            if not git_dir.exists():
                # Inicializar novo repo
                self._run_git(project_path, "init")
                self._run_git(project_path, "config", "user.name", "Plataforma E Bot")
                self._run_git(project_path, "config", "user.email", "bot@plataformae.app")

                # Criar .gitignore basico
                gitignore_path = project_path / ".gitignore"
                if not gitignore_path.exists():
                    gitignore_content = """# Python
__pycache__/
*.py[cod]
*$py.class
.Python
venv/
.env

# Node
node_modules/
npm-debug.log

# IDE
.idea/
.vscode/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db

# Build
dist/
build/
*.egg-info/
"""
                    with open(gitignore_path, "w") as f:
                        f.write(gitignore_content)

                # Commit inicial
                self._run_git(project_path, "add", "-A")
                self._run_git(project_path, "commit", "-m", "Initial commit - Plataforma E")

            # Configurar remote se fornecido
            if remote_url:
                # Verificar se remote existe
                result = self._run_git(project_path, "remote", "get-url", "origin", check=False)
                if result.returncode == 0:
                    # Atualizar remote existente
                    self._run_git(project_path, "remote", "set-url", "origin", remote_url)
                else:
                    # Adicionar novo remote
                    self._run_git(project_path, "remote", "add", "origin", remote_url)

                # Salvar configuracao
                self.configs[project_id] = GitConfig(remote_url=remote_url)
                self._save_configs()

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message="Repositorio inicializado com sucesso"
            )

        except subprocess.CalledProcessError as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro Git: {e.stderr}"
            )
        except Exception as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=str(e)
            )

    def commit_changes(
        self,
        project_id: str,
        message: str,
        files: Optional[List[str]] = None
    ) -> SyncResult:
        """
        Cria commit com as mudancas.

        Args:
            project_id: ID do projeto
            message: Mensagem do commit
            files: Arquivos especificos (None = todos)

        Returns:
            SyncResult com status
        """
        project_path = self.projects_dir / project_id

        if not (project_path / ".git").exists():
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="Repositorio nao inicializado"
            )

        try:
            # Adicionar arquivos
            if files:
                for f in files:
                    self._run_git(project_path, "add", f)
            else:
                self._run_git(project_path, "add", "-A")

            # Verificar se ha mudancas
            status_result = self._run_git(project_path, "status", "--porcelain")
            if not status_result.stdout.strip():
                return SyncResult(
                    success=True,
                    status=SyncStatus.SUCCESS,
                    message="Nenhuma mudanca para commitar"
                )

            # Criar commit
            commit_message = f"{message}\n\n🤖 Generated by Plataforma E"
            self._run_git(project_path, "commit", "-m", commit_message)

            # Obter SHA do commit
            sha_result = self._run_git(project_path, "rev-parse", "HEAD")
            commit_sha = sha_result.stdout.strip()

            # Salvar versao
            self._save_version(project_id, commit_sha, message)

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message=f"Commit criado: {commit_sha[:7]}",
                commits_pushed=0,
                files_changed=status_result.stdout.strip().split('\n')
            )

        except subprocess.CalledProcessError as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro Git: {e.stderr}"
            )

    def push(self, project_id: str, branch: Optional[str] = None, force: bool = False) -> SyncResult:
        """
        Push para o repositorio remoto.

        Args:
            project_id: ID do projeto
            branch: Branch para push (None = branch atual)
            force: Force push

        Returns:
            SyncResult com status
        """
        project_path = self.projects_dir / project_id
        config = self.configs.get(project_id, GitConfig())

        if not config.remote_url:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="Repositorio remoto nao configurado"
            )

        try:
            target_branch = branch or config.branch

            # Configurar credenciais se tiver token
            if self.github_token and "github.com" in config.remote_url:
                # Usar token no URL
                auth_url = config.remote_url.replace(
                    "https://github.com",
                    f"https://{self.github_token}@github.com"
                )
                self._run_git(project_path, "remote", "set-url", "origin", auth_url)

            # Push
            push_args = ["push", "-u", "origin", target_branch]
            if force:
                push_args.insert(1, "--force")

            self._run_git(project_path, *push_args)

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message=f"Push realizado para {target_branch}",
                commits_pushed=1
            )

        except subprocess.CalledProcessError as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro no push: {e.stderr}"
            )

    def pull(self, project_id: str, branch: Optional[str] = None) -> SyncResult:
        """
        Pull do repositorio remoto.

        Args:
            project_id: ID do projeto
            branch: Branch para pull

        Returns:
            SyncResult com status
        """
        project_path = self.projects_dir / project_id
        config = self.configs.get(project_id, GitConfig())

        if not config.remote_url:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="Repositorio remoto nao configurado"
            )

        try:
            target_branch = branch or config.branch

            # Fetch primeiro
            self._run_git(project_path, "fetch", "origin")

            # Pull com rebase
            result = self._run_git(
                project_path,
                "pull", "--rebase", "origin", target_branch,
                check=False
            )

            if result.returncode != 0:
                if "conflict" in result.stderr.lower():
                    return SyncResult(
                        success=False,
                        status=SyncStatus.CONFLICT,
                        message="Conflitos detectados",
                        conflicts=self._get_conflicts(project_path)
                    )
                else:
                    return SyncResult(
                        success=False,
                        status=SyncStatus.ERROR,
                        error=result.stderr
                    )

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message="Pull realizado com sucesso"
            )

        except subprocess.CalledProcessError as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro no pull: {e.stderr}"
            )

    def _get_conflicts(self, project_path: Path) -> List[str]:
        """Retorna lista de arquivos em conflito"""
        try:
            result = self._run_git(project_path, "diff", "--name-only", "--diff-filter=U", check=False)
            return result.stdout.strip().split('\n') if result.stdout.strip() else []
        except:
            return []

    def create_feature_branch(self, project_id: str, story_id: str, story_title: str) -> SyncResult:
        """
        Cria branch para uma feature/story.

        Args:
            project_id: ID do projeto
            story_id: ID da story (ex: STR-001)
            story_title: Titulo da story

        Returns:
            SyncResult com nome da branch
        """
        project_path = self.projects_dir / project_id

        try:
            # Normalizar nome da branch
            branch_name = self._normalize_branch_name(story_id, story_title)

            # Criar e checkout
            self._run_git(project_path, "checkout", "-b", branch_name)

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message=f"Branch criada: {branch_name}"
            )

        except subprocess.CalledProcessError as e:
            if "already exists" in e.stderr:
                # Branch ja existe, fazer checkout
                self._run_git(project_path, "checkout", branch_name)
                return SyncResult(
                    success=True,
                    status=SyncStatus.SUCCESS,
                    message=f"Checkout para branch existente: {branch_name}"
                )
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro ao criar branch: {e.stderr}"
            )

    def _normalize_branch_name(self, story_id: str, title: str) -> str:
        """Normaliza nome para branch"""
        import re
        import unicodedata

        # Remover acentos
        title = unicodedata.normalize('NFKD', title)
        title = title.encode('ASCII', 'ignore').decode('ASCII')

        # Lowercase e substituir espacos
        title = title.lower().strip()
        title = re.sub(r'[^a-z0-9]+', '-', title)
        title = re.sub(r'-+', '-', title)
        title = title.strip('-')

        # Limitar tamanho
        if len(title) > 30:
            title = title[:30].rstrip('-')

        return f"feature/{story_id.lower()}-{title}"

    async def create_pull_request(
        self,
        project_id: str,
        title: str,
        body: str,
        head_branch: str,
        base_branch: str = "main"
    ) -> SyncResult:
        """
        Cria Pull Request no GitHub.

        Args:
            project_id: ID do projeto
            title: Titulo do PR
            body: Descricao do PR
            head_branch: Branch de origem
            base_branch: Branch de destino

        Returns:
            SyncResult com URL do PR
        """
        config = self.configs.get(project_id)

        if not config or not config.remote_url:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="Repositorio remoto nao configurado"
            )

        if not self.github_token:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="GitHub token nao configurado"
            )

        try:
            import httpx

            # Extrair owner/repo do URL
            parts = config.remote_url.rstrip('/').rstrip('.git').split('/')
            owner = parts[-2]
            repo = parts[-1]

            async with httpx.AsyncClient() as client:
                response = await client.post(
                    f"https://api.github.com/repos/{owner}/{repo}/pulls",
                    headers={
                        "Authorization": f"Bearer {self.github_token}",
                        "Accept": "application/vnd.github+json"
                    },
                    json={
                        "title": title,
                        "body": body + "\n\n🤖 Generated by Plataforma E",
                        "head": head_branch,
                        "base": base_branch
                    }
                )

                if response.status_code == 201:
                    data = response.json()
                    return SyncResult(
                        success=True,
                        status=SyncStatus.SUCCESS,
                        message="Pull Request criado!",
                        pr_url=data.get("html_url")
                    )
                else:
                    error_data = response.json()
                    return SyncResult(
                        success=False,
                        status=SyncStatus.ERROR,
                        error=error_data.get("message", "Erro desconhecido")
                    )

        except Exception as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=str(e)
            )

    def _save_version(self, project_id: str, commit_sha: str, message: str):
        """Salva snapshot de versao"""
        if project_id not in self.versions:
            self.versions[project_id] = []

        project_path = self.projects_dir / project_id

        # Contar arquivos
        files_count = sum(1 for _ in project_path.rglob("*") if _.is_file() and ".git" not in str(_))

        # Obter branch atual
        try:
            result = self._run_git(project_path, "rev-parse", "--abbrev-ref", "HEAD")
            branch = result.stdout.strip()
        except:
            branch = "main"

        version = VersionSnapshot(
            version_id=f"v-{uuid.uuid4().hex[:8]}",
            project_id=project_id,
            commit_sha=commit_sha,
            branch=branch,
            message=message,
            created_at=datetime.now(),
            created_by="Plataforma E",
            files_count=files_count
        )

        self.versions[project_id].insert(0, version)

        # Manter apenas ultimas 50 versoes
        self.versions[project_id] = self.versions[project_id][:50]

        # Salvar em disco
        self._save_versions()

    def _save_versions(self):
        """Salva historico de versoes"""
        versions_file = self.data_dir / "versions.json"
        try:
            data = {
                pid: [v.to_dict() for v in versions]
                for pid, versions in self.versions.items()
            }
            with open(versions_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.error(f"Error saving versions: {e}")

    def get_versions(self, project_id: str, limit: int = 20) -> List[VersionSnapshot]:
        """Retorna historico de versoes de um projeto"""
        return self.versions.get(project_id, [])[:limit]

    def rollback(self, project_id: str, version_id: str) -> SyncResult:
        """
        Rollback para uma versao anterior.

        Args:
            project_id: ID do projeto
            version_id: ID da versao

        Returns:
            SyncResult com status
        """
        project_path = self.projects_dir / project_id

        # Encontrar versao
        versions = self.versions.get(project_id, [])
        version = next((v for v in versions if v.version_id == version_id), None)

        if not version:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error="Versao nao encontrada"
            )

        try:
            # Criar backup antes do rollback
            backup_result = self._run_git(project_path, "rev-parse", "HEAD")
            backup_sha = backup_result.stdout.strip()

            # Criar branch de backup
            backup_branch = f"backup-{datetime.now().strftime('%Y%m%d-%H%M%S')}"
            self._run_git(project_path, "branch", backup_branch)

            # Reset para a versao desejada
            self._run_git(project_path, "reset", "--hard", version.commit_sha)

            return SyncResult(
                success=True,
                status=SyncStatus.SUCCESS,
                message=f"Rollback realizado para {version.commit_sha[:7]}. Backup em branch: {backup_branch}"
            )

        except subprocess.CalledProcessError as e:
            return SyncResult(
                success=False,
                status=SyncStatus.ERROR,
                error=f"Erro no rollback: {e.stderr}"
            )

    def get_sync_status(self, project_id: str) -> Dict[str, Any]:
        """Retorna status de sync de um projeto"""
        project_path = self.projects_dir / project_id
        config = self.configs.get(project_id, GitConfig())

        status = {
            "project_id": project_id,
            "git_initialized": (project_path / ".git").exists(),
            "remote_configured": bool(config.remote_url),
            "remote_url": config.remote_url,
            "branch_strategy": config.branch_strategy.value,
            "auto_push": config.auto_push,
            "auto_pull": config.auto_pull,
            "current_branch": None,
            "ahead_by": 0,
            "behind_by": 0,
            "uncommitted_changes": False,
            "last_commit": None,
            "versions_count": len(self.versions.get(project_id, []))
        }

        if status["git_initialized"]:
            try:
                # Branch atual
                result = self._run_git(project_path, "rev-parse", "--abbrev-ref", "HEAD", check=False)
                status["current_branch"] = result.stdout.strip() if result.returncode == 0 else None

                # Mudancas nao commitadas
                result = self._run_git(project_path, "status", "--porcelain", check=False)
                status["uncommitted_changes"] = bool(result.stdout.strip())

                # Ultimo commit
                result = self._run_git(project_path, "log", "-1", "--format=%H|%s|%an|%ci", check=False)
                if result.returncode == 0 and result.stdout.strip():
                    parts = result.stdout.strip().split("|")
                    if len(parts) >= 4:
                        status["last_commit"] = {
                            "sha": parts[0][:7],
                            "message": parts[1],
                            "author": parts[2],
                            "date": parts[3]
                        }

            except Exception as e:
                logger.error(f"Error getting sync status: {e}")

        return status

    def configure_sync(
        self,
        project_id: str,
        remote_url: str,
        branch: str = "main",
        branch_strategy: str = "feature",
        auto_push: bool = True,
        auto_pull: bool = True
    ) -> bool:
        """Configura sincronizacao para um projeto"""
        self.configs[project_id] = GitConfig(
            remote_url=remote_url,
            branch=branch,
            branch_strategy=BranchStrategy(branch_strategy),
            auto_push=auto_push,
            auto_pull=auto_pull
        )
        self._save_configs()
        return True


# Instancia global
github_sync = GitHubSync()
