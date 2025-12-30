# -*- coding: utf-8 -*-
"""
SharePoint Configuration
========================
Configuracoes para integracao com Microsoft SharePoint.

Terminal 5 - Issue #298
"""

from dataclasses import dataclass, field
from typing import List, Optional
from enum import Enum


class SharePointScope(str, Enum):
    """Escopos de permissao do SharePoint via Graph API"""
    SITES_READ_ALL = "Sites.Read.All"
    SITES_READ_WRITE_ALL = "Sites.ReadWrite.All"
    FILES_READ = "Files.Read"
    FILES_READ_ALL = "Files.Read.All"
    FILES_READ_WRITE = "Files.ReadWrite"
    FILES_READ_WRITE_ALL = "Files.ReadWrite.All"
    SITES_MANAGE_ALL = "Sites.Manage.All"
    SITES_FULL_CONTROL_ALL = "Sites.FullControl.All"
    DEFAULT = "https://graph.microsoft.com/.default"


@dataclass
class SharePointConfig:
    """
    Configuracao do cliente SharePoint.

    Attributes:
        tenant_id: ID do tenant Azure AD (obrigatorio para multi-tenant)
        client_id: ID do aplicativo registrado no Azure AD
        client_secret: Secret do aplicativo
        site_url: URL do site SharePoint (opcional)
        scopes: Lista de escopos OAuth necessarios
        base_url: URL base da Graph API
        timeout_seconds: Timeout das requisicoes
        max_retries: Numero maximo de tentativas
        retry_delay_seconds: Delay entre tentativas
    """
    # Credenciais Azure AD
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""

    # Configuracao do site
    site_url: Optional[str] = None
    site_id: Optional[str] = None

    # Escopos OAuth
    scopes: List[str] = field(default_factory=lambda: [SharePointScope.DEFAULT.value])

    # Configuracao de API
    base_url: str = "https://graph.microsoft.com/v1.0"
    token_url_template: str = "https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/token"

    # Configuracao de requisicoes
    timeout_seconds: int = 60  # SharePoint pode ser lento em uploads grandes
    max_retries: int = 3
    retry_delay_seconds: float = 2.0

    # Configuracao de upload
    chunk_size: int = 10 * 1024 * 1024  # 10MB por chunk
    max_file_size: int = 250 * 1024 * 1024  # 250MB maximo

    # Isolamento de tenant
    integration_tenant_id: Optional[str] = None  # Tenant da Fabrica de Agentes

    def get_site_identifier(self) -> Optional[str]:
        """
        Retorna o identificador do site para uso na API.

        Returns:
            Site ID ou None se nao configurado
        """
        if self.site_id:
            return self.site_id
        if self.site_url:
            # Extrai hostname e path do site
            from urllib.parse import urlparse
            parsed = urlparse(self.site_url)
            hostname = parsed.netloc
            path = parsed.path.rstrip('/')
            return f"{hostname}:{path}"
        return None

    def validate(self) -> List[str]:
        """
        Valida a configuracao.

        Returns:
            Lista de erros de validacao
        """
        errors = []

        if not self.tenant_id:
            errors.append("tenant_id e obrigatorio")
        if not self.client_id:
            errors.append("client_id e obrigatorio")
        if not self.client_secret:
            errors.append("client_secret e obrigatorio")

        return errors


@dataclass
class SharePointSyncConfig:
    """
    Configuracao de sincronizacao bidirecional.

    Attributes:
        local_path: Caminho local para sincronizacao
        remote_folder: Pasta remota no SharePoint
        sync_interval_seconds: Intervalo entre sincronizacoes
        conflict_resolution: Estrategia de resolucao de conflitos
        include_patterns: Padroes de arquivos para incluir
        exclude_patterns: Padroes de arquivos para excluir
    """
    local_path: str = ""
    remote_folder: str = "/"
    sync_interval_seconds: int = 300  # 5 minutos
    conflict_resolution: str = "remote_wins"  # remote_wins, local_wins, newest_wins
    include_patterns: List[str] = field(default_factory=lambda: ["*"])
    exclude_patterns: List[str] = field(default_factory=lambda: [".git", "__pycache__", "*.pyc"])

    # Limites
    max_files_per_sync: int = 1000
    max_size_per_sync: int = 1024 * 1024 * 1024  # 1GB
