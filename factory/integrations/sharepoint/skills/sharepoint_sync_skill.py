# -*- coding: utf-8 -*-
"""
SharePoint Sync Skill
=====================
Skill para sincronizacao bidirecional com SharePoint.

Terminal 5 - Issue #298
"""

import asyncio
import logging
import os
import hashlib
from datetime import datetime
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, field
from pathlib import Path

from ..config import SharePointConfig, SharePointSyncConfig
from ..document_client import DocumentClient

logger = logging.getLogger(__name__)


@dataclass
class SyncResult:
    """Resultado de uma operacao de sincronizacao"""
    success: bool
    uploaded: List[str] = field(default_factory=list)
    downloaded: List[str] = field(default_factory=list)
    deleted: List[str] = field(default_factory=list)
    conflicts: List[Dict[str, Any]] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    delta_token: Optional[str] = None

    @property
    def total_changes(self) -> int:
        return len(self.uploaded) + len(self.downloaded) + len(self.deleted)


@dataclass
class FileState:
    """Estado de um arquivo para rastreamento de sync"""
    path: str
    local_hash: Optional[str] = None
    remote_hash: Optional[str] = None
    local_modified: Optional[datetime] = None
    remote_modified: Optional[datetime] = None
    synced_at: Optional[datetime] = None


class SharePointSyncSkill:
    """
    Skill para sincronizacao bidirecional com SharePoint.

    Permite:
    - Sync de pasta local com pasta SharePoint
    - Deteccao de conflitos
    - Resolucao automatica ou manual de conflitos
    - Delta sync (apenas mudancas)

    Isolamento por tenant garantido atraves do config.integration_tenant_id.

    Exemplo:
        config = SharePointConfig(...)
        sync_config = SharePointSyncConfig(
            local_path="/projetos/cliente",
            remote_folder="/Documentos/Projeto"
        )

        skill = SharePointSyncSkill(config, sync_config)

        # Sync completo
        result = await skill.sync()

        # Apenas download
        result = await skill.download_changes()

        # Apenas upload
        result = await skill.upload_changes()
    """

    def __init__(
        self,
        config: SharePointConfig,
        sync_config: SharePointSyncConfig
    ):
        """
        Inicializa o skill de sincronizacao.

        Args:
            config: Configuracao do SharePoint
            sync_config: Configuracao de sincronizacao
        """
        self.config = config
        self.sync_config = sync_config
        self._client: Optional[DocumentClient] = None
        self._authenticated = False
        self._file_states: Dict[str, FileState] = {}
        self._delta_token: Optional[str] = None

    async def _ensure_client(self) -> bool:
        """Inicializa e autentica o cliente se necessario"""
        if self._authenticated:
            return True

        try:
            self._client = DocumentClient(self.config)
            if await self._client.authenticate():
                self._authenticated = True
                return True
            return False
        except Exception as e:
            logger.error(f"Erro ao inicializar cliente: {e}")
            return False

    async def close(self) -> None:
        """Fecha conexoes"""
        if self._client:
            await self._client.close()

    def _calculate_hash(self, content: bytes) -> str:
        """Calcula hash MD5 de um conteudo"""
        return hashlib.md5(content).hexdigest()

    def _get_local_files(self) -> Dict[str, Dict[str, Any]]:
        """Lista arquivos locais com metadados"""
        local_files = {}
        local_path = Path(self.sync_config.local_path)

        if not local_path.exists():
            return local_files

        for file_path in local_path.rglob("*"):
            if file_path.is_file():
                # Verifica padroes de exclusao
                relative_path = str(file_path.relative_to(local_path))
                should_exclude = False

                for pattern in self.sync_config.exclude_patterns:
                    if pattern in relative_path:
                        should_exclude = True
                        break

                if not should_exclude:
                    stat = file_path.stat()
                    local_files[relative_path] = {
                        "path": str(file_path),
                        "relative_path": relative_path,
                        "size": stat.st_size,
                        "modified": datetime.fromtimestamp(stat.st_mtime)
                    }

        return local_files

    # =========================================================================
    # Sincronizacao
    # =========================================================================

    async def sync(
        self,
        site_id: str,
        drive_id: str
    ) -> SyncResult:
        """
        Executa sincronizacao bidirecional.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca

        Returns:
            Resultado da sincronizacao
        """
        if not await self._ensure_client():
            return SyncResult(success=False, errors=["Falha na autenticacao"])

        result = SyncResult(success=True)

        try:
            # 1. Obtem estado remoto
            remote_files = await self._get_remote_files(site_id, drive_id)

            # 2. Obtem estado local
            local_files = self._get_local_files()

            # 3. Identifica mudancas
            to_upload = []
            to_download = []
            conflicts = []

            all_paths = set(local_files.keys()) | set(remote_files.keys())

            for path in all_paths:
                local = local_files.get(path)
                remote = remote_files.get(path)

                if local and not remote:
                    # Arquivo existe apenas localmente - upload
                    to_upload.append(path)
                elif remote and not local:
                    # Arquivo existe apenas remotamente - download
                    to_download.append(path)
                elif local and remote:
                    # Arquivo existe em ambos - verificar conflito
                    local_modified = local["modified"]
                    remote_modified = remote.get("lastModifiedDateTime")

                    if remote_modified:
                        remote_dt = datetime.fromisoformat(
                            remote_modified.replace("Z", "+00:00")
                        ).replace(tzinfo=None)

                        if abs((local_modified - remote_dt).total_seconds()) > 60:
                            # Potencial conflito
                            conflicts.append({
                                "path": path,
                                "local_modified": local_modified,
                                "remote_modified": remote_dt
                            })

            # 4. Resolve conflitos
            for conflict in conflicts:
                resolution = self._resolve_conflict(conflict)
                if resolution == "upload":
                    to_upload.append(conflict["path"])
                elif resolution == "download":
                    to_download.append(conflict["path"])
                result.conflicts.append(conflict)

            # 5. Executa upload
            for path in to_upload[:self.sync_config.max_files_per_sync]:
                try:
                    await self._upload_file(site_id, drive_id, path)
                    result.uploaded.append(path)
                except Exception as e:
                    result.errors.append(f"Erro ao fazer upload de {path}: {e}")

            # 6. Executa download
            for path in to_download[:self.sync_config.max_files_per_sync]:
                try:
                    await self._download_file(site_id, drive_id, path)
                    result.downloaded.append(path)
                except Exception as e:
                    result.errors.append(f"Erro ao fazer download de {path}: {e}")

            result.success = len(result.errors) == 0

        except Exception as e:
            logger.error(f"Erro na sincronizacao: {e}")
            result.success = False
            result.errors.append(str(e))

        return result

    async def _get_remote_files(
        self,
        site_id: str,
        drive_id: str
    ) -> Dict[str, Dict[str, Any]]:
        """Obtem arquivos remotos"""
        remote_files = {}
        folder_path = self.sync_config.remote_folder

        contents = await self._client.list_folder_contents(
            site_id, drive_id, folder_path
        )

        for item in contents:
            if "file" in item:
                path = item["name"]
                remote_files[path] = item

        return remote_files

    def _resolve_conflict(self, conflict: Dict[str, Any]) -> str:
        """
        Resolve conflito baseado na estrategia configurada.

        Returns:
            "upload", "download" ou "skip"
        """
        strategy = self.sync_config.conflict_resolution

        if strategy == "remote_wins":
            return "download"
        elif strategy == "local_wins":
            return "upload"
        elif strategy == "newest_wins":
            if conflict["local_modified"] > conflict["remote_modified"]:
                return "upload"
            else:
                return "download"
        else:
            return "skip"

    async def _upload_file(
        self,
        site_id: str,
        drive_id: str,
        relative_path: str
    ) -> None:
        """Faz upload de um arquivo"""
        local_path = Path(self.sync_config.local_path) / relative_path
        remote_path = f"{self.sync_config.remote_folder}/{relative_path}"

        with open(local_path, "rb") as f:
            content = f.read()

        await self._client.upload_file(
            site_id, drive_id, remote_path, content
        )

    async def _download_file(
        self,
        site_id: str,
        drive_id: str,
        relative_path: str
    ) -> None:
        """Faz download de um arquivo"""
        local_path = Path(self.sync_config.local_path) / relative_path
        remote_path = f"{self.sync_config.remote_folder}/{relative_path}"

        content = await self._client.download_file_by_path(
            site_id, drive_id, remote_path
        )

        if content:
            # Cria diretorios se necessario
            local_path.parent.mkdir(parents=True, exist_ok=True)

            with open(local_path, "wb") as f:
                f.write(content)

    # =========================================================================
    # Delta Sync
    # =========================================================================

    async def delta_sync(
        self,
        drive_id: str
    ) -> SyncResult:
        """
        Sincronizacao incremental usando delta API.

        Args:
            drive_id: ID da biblioteca

        Returns:
            Resultado da sincronizacao
        """
        if not await self._ensure_client():
            return SyncResult(success=False, errors=["Falha na autenticacao"])

        result = SyncResult(success=True)

        try:
            delta = await self._client.get_delta(drive_id, self._delta_token)
            changes = delta.get("value", [])
            result.delta_token = delta.get("@odata.deltaLink") or delta.get("deltaLink")
            self._delta_token = result.delta_token

            for change in changes:
                item_name = change.get("name", "")

                if change.get("deleted"):
                    # Item deletado
                    local_path = Path(self.sync_config.local_path) / item_name
                    if local_path.exists():
                        try:
                            local_path.unlink()
                            result.deleted.append(item_name)
                        except Exception as e:
                            result.errors.append(f"Erro ao deletar {item_name}: {e}")

                elif "file" in change:
                    # Arquivo novo ou modificado
                    try:
                        item_id = change.get("id")
                        content = await self._client.download_file(
                            change.get("parentReference", {}).get("driveId", ""),
                            item_id
                        )

                        if content:
                            local_path = Path(self.sync_config.local_path) / item_name
                            local_path.parent.mkdir(parents=True, exist_ok=True)

                            with open(local_path, "wb") as f:
                                f.write(content)

                            result.downloaded.append(item_name)
                    except Exception as e:
                        result.errors.append(f"Erro ao baixar {item_name}: {e}")

            result.success = len(result.errors) == 0

        except Exception as e:
            logger.error(f"Erro no delta sync: {e}")
            result.success = False
            result.errors.append(str(e))

        return result

    # =========================================================================
    # Operacoes Unidirecionais
    # =========================================================================

    async def upload_all(
        self,
        site_id: str,
        drive_id: str
    ) -> SyncResult:
        """
        Faz upload de todos os arquivos locais.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca

        Returns:
            Resultado do upload
        """
        if not await self._ensure_client():
            return SyncResult(success=False, errors=["Falha na autenticacao"])

        result = SyncResult(success=True)
        local_files = self._get_local_files()

        for relative_path in local_files:
            try:
                await self._upload_file(site_id, drive_id, relative_path)
                result.uploaded.append(relative_path)
            except Exception as e:
                result.errors.append(f"Erro ao fazer upload de {relative_path}: {e}")

        result.success = len(result.errors) == 0
        return result

    async def download_all(
        self,
        site_id: str,
        drive_id: str
    ) -> SyncResult:
        """
        Faz download de todos os arquivos remotos.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca

        Returns:
            Resultado do download
        """
        if not await self._ensure_client():
            return SyncResult(success=False, errors=["Falha na autenticacao"])

        result = SyncResult(success=True)
        remote_files = await self._get_remote_files(site_id, drive_id)

        for relative_path in remote_files:
            try:
                await self._download_file(site_id, drive_id, relative_path)
                result.downloaded.append(relative_path)
            except Exception as e:
                result.errors.append(f"Erro ao fazer download de {relative_path}: {e}")

        result.success = len(result.errors) == 0
        return result

    # =========================================================================
    # Estado e Monitoramento
    # =========================================================================

    def get_sync_status(self) -> Dict[str, Any]:
        """
        Retorna status atual da sincronizacao.

        Returns:
            Status com informacoes de arquivos rastreados
        """
        local_files = self._get_local_files()

        return {
            "local_path": self.sync_config.local_path,
            "remote_folder": self.sync_config.remote_folder,
            "local_file_count": len(local_files),
            "tracked_files": len(self._file_states),
            "has_delta_token": self._delta_token is not None,
            "conflict_resolution": self.sync_config.conflict_resolution,
            "tenant_id": self.config.integration_tenant_id
        }

    async def check_for_changes(
        self,
        site_id: str,
        drive_id: str
    ) -> Dict[str, int]:
        """
        Verifica se ha mudancas pendentes.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca

        Returns:
            Contagem de mudancas por tipo
        """
        if not await self._ensure_client():
            return {"error": "Falha na autenticacao"}

        remote_files = await self._get_remote_files(site_id, drive_id)
        local_files = self._get_local_files()

        local_only = len(set(local_files.keys()) - set(remote_files.keys()))
        remote_only = len(set(remote_files.keys()) - set(local_files.keys()))
        both = len(set(local_files.keys()) & set(remote_files.keys()))

        return {
            "to_upload": local_only,
            "to_download": remote_only,
            "synced": both,
            "total_local": len(local_files),
            "total_remote": len(remote_files)
        }
