# -*- coding: utf-8 -*-
"""
SAP ECC Integration
===================
Modulo principal de integracao com SAP ECC.

Este modulo fornece a classe principal de integracao que:
- Gerencia conexoes RFC e OData
- Fornece acesso unificado a analisadores e geradores
- Implementa interface padrao de integracao
- Suporta multiplos modulos SAP

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc import SAPECCIntegration, SAPECCConfig

# Configuracao
config = SAPECCConfig(
    rfc_host="sap-server.company.com",
    rfc_sysnr="00",
    rfc_client="100",
    rfc_user="RFC_USER",
    rfc_password="xxx"
)

# Conectar
sap = SAPECCIntegration(config)
await sap.connect()

# Usar skills
result = await sap.read_skill.get_material("MATERIAL001")

# Desconectar
await sap.disconnect()
```
"""

import asyncio
import logging
import os
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

from ..base import IntegrationBase, IntegrationConfig, IntegrationStatus, SyncResult
from .rfc_client import SAPRFCClient, SAPRFCConfig
from .odata_client import SAPODataClient, SAPODataConfig
from .analyzers import ABAPAnalyzer, TableAnalyzer, BADIAnalyzer, ConfigAnalyzer
from .generators import ABAPGenerator, FunctionGenerator, ClassGenerator, IDocGenerator
from .skills import SAPReadSkill, SAPABAPSkill, SAPConfigSkill

logger = logging.getLogger(__name__)


class SAPModule(str, Enum):
    """Modulos SAP suportados"""
    FI = "FI"  # Financeiro
    CO = "CO"  # Controladoria
    MM = "MM"  # Materiais
    SD = "SD"  # Vendas
    PP = "PP"  # Producao
    WM = "WM"  # Warehouse
    QM = "QM"  # Qualidade
    PM = "PM"  # Manutencao
    HR = "HR"  # RH
    ABAP = "ABAP"  # Desenvolvimento


class SAPConnectionStatus(str, Enum):
    """Status de conexao SAP"""
    DISCONNECTED = "disconnected"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    ERROR = "error"


@dataclass
class SAPECCConfig(IntegrationConfig):
    """
    Configuracao de integracao SAP ECC.

    Atributos:
        rfc_host: Hostname do servidor SAP
        rfc_sysnr: Numero do sistema
        rfc_client: Mandante
        rfc_user: Usuario RFC
        rfc_password: Senha
        rfc_lang: Idioma
        odata_enabled: Habilitar OData
        odata_url: URL base do SAP Gateway
        odata_username: Usuario OData
        odata_password: Senha OData
        enabled_modules: Modulos habilitados
        pool_size: Tamanho do pool de conexoes
    """
    # RFC
    rfc_host: str = ""
    rfc_sysnr: str = "00"
    rfc_client: str = "100"
    rfc_user: str = ""
    rfc_password: str = ""
    rfc_lang: str = "PT"

    # Load Balancing (opcional)
    rfc_mshost: Optional[str] = None
    rfc_group: Optional[str] = None

    # OData
    odata_enabled: bool = False
    odata_url: str = ""
    odata_username: str = ""
    odata_password: str = ""

    # Modulos e pool
    enabled_modules: List[SAPModule] = field(default_factory=lambda: list(SAPModule))
    pool_size: int = 5

    @classmethod
    def from_env(cls) -> 'SAPECCConfig':
        """Carrega configuracao de variaveis de ambiente"""
        return cls(
            rfc_host=os.getenv("SAP_RFC_ASHOST", ""),
            rfc_sysnr=os.getenv("SAP_RFC_SYSNR", "00"),
            rfc_client=os.getenv("SAP_RFC_CLIENT", "100"),
            rfc_user=os.getenv("SAP_RFC_USER", ""),
            rfc_password=os.getenv("SAP_RFC_PASSWD", ""),
            rfc_lang=os.getenv("SAP_RFC_LANG", "PT"),
            rfc_mshost=os.getenv("SAP_RFC_MSHOST"),
            rfc_group=os.getenv("SAP_RFC_GROUP"),
            odata_enabled=os.getenv("SAP_ODATA_ENABLED", "false").lower() == "true",
            odata_url=os.getenv("SAP_ODATA_URL", ""),
            odata_username=os.getenv("SAP_ODATA_USER", ""),
            odata_password=os.getenv("SAP_ODATA_PASSWD", ""),
            enabled=True
        )

    def validate(self) -> tuple[bool, List[str]]:
        """Valida a configuracao"""
        errors = []

        if not self.rfc_host and not self.rfc_mshost:
            errors.append("Servidor SAP nao configurado (rfc_host ou rfc_mshost)")
        if not self.rfc_user:
            errors.append("Usuario RFC nao configurado")
        if not self.rfc_password:
            errors.append("Senha RFC nao configurada")

        if self.odata_enabled:
            if not self.odata_url:
                errors.append("URL OData nao configurada")
            if not self.odata_username:
                errors.append("Usuario OData nao configurado")

        return len(errors) == 0, errors


class SAPECCIntegration(IntegrationBase):
    """
    Integracao principal com SAP ECC.

    Fornece acesso unificado a:
    - Conexao RFC e OData
    - Analisadores (ABAP, Tabelas, BADIs, Config)
    - Geradores (ABAP, FM, Classes, IDocs)
    - Skills (Leitura, ABAP, Config)
    """

    def __init__(self, config: SAPECCConfig):
        """
        Inicializa a integracao.

        Args:
            config: Configuracao SAP ECC
        """
        super().__init__(config)
        self.sap_config = config
        self._rfc_client: Optional[SAPRFCClient] = None
        self._odata_client: Optional[SAPODataClient] = None
        self._connection_status = SAPConnectionStatus.DISCONNECTED
        self._connected_at: Optional[datetime] = None

        # Analisadores (inicializados apos conexao)
        self._abap_analyzer: Optional[ABAPAnalyzer] = None
        self._table_analyzer: Optional[TableAnalyzer] = None
        self._badi_analyzer: Optional[BADIAnalyzer] = None
        self._config_analyzer: Optional[ConfigAnalyzer] = None

        # Geradores (nao precisam de conexao)
        self._abap_generator = ABAPGenerator()
        self._function_generator = FunctionGenerator()
        self._class_generator = ClassGenerator()
        self._idoc_generator = IDocGenerator()

        # Skills (inicializadas apos conexao)
        self._read_skill: Optional[SAPReadSkill] = None
        self._abap_skill: Optional[SAPABAPSkill] = None
        self._config_skill: Optional[SAPConfigSkill] = None

    # =========================================================================
    # Propriedades
    # =========================================================================

    @property
    def connection_status(self) -> SAPConnectionStatus:
        """Status da conexao"""
        return self._connection_status

    @property
    def rfc_client(self) -> Optional[SAPRFCClient]:
        """Cliente RFC"""
        return self._rfc_client

    @property
    def odata_client(self) -> Optional[SAPODataClient]:
        """Cliente OData"""
        return self._odata_client

    # Analisadores
    @property
    def abap_analyzer(self) -> Optional[ABAPAnalyzer]:
        return self._abap_analyzer

    @property
    def table_analyzer(self) -> Optional[TableAnalyzer]:
        return self._table_analyzer

    @property
    def badi_analyzer(self) -> Optional[BADIAnalyzer]:
        return self._badi_analyzer

    @property
    def config_analyzer(self) -> Optional[ConfigAnalyzer]:
        return self._config_analyzer

    # Geradores
    @property
    def abap_generator(self) -> ABAPGenerator:
        return self._abap_generator

    @property
    def function_generator(self) -> FunctionGenerator:
        return self._function_generator

    @property
    def class_generator(self) -> ClassGenerator:
        return self._class_generator

    @property
    def idoc_generator(self) -> IDocGenerator:
        return self._idoc_generator

    # Skills
    @property
    def read_skill(self) -> Optional[SAPReadSkill]:
        return self._read_skill

    @property
    def abap_skill(self) -> Optional[SAPABAPSkill]:
        return self._abap_skill

    @property
    def config_skill(self) -> Optional[SAPConfigSkill]:
        return self._config_skill

    # =========================================================================
    # Conexao
    # =========================================================================

    async def connect(self) -> bool:
        """
        Conecta ao SAP.

        Returns:
            True se conectou com sucesso
        """
        if self.is_connected:
            return True

        # Validar configuracao
        is_valid, errors = self.sap_config.validate()
        if not is_valid:
            self._last_error = f"Configuracao invalida: {', '.join(errors)}"
            self._connection_status = SAPConnectionStatus.ERROR
            return False

        self._connection_status = SAPConnectionStatus.CONNECTING

        try:
            # Conectar RFC
            rfc_config = SAPRFCConfig(
                ashost=self.sap_config.rfc_host,
                sysnr=self.sap_config.rfc_sysnr,
                client=self.sap_config.rfc_client,
                user=self.sap_config.rfc_user,
                passwd=self.sap_config.rfc_password,
                lang=self.sap_config.rfc_lang,
                mshost=self.sap_config.rfc_mshost,
                group=self.sap_config.rfc_group,
                pool_size=self.sap_config.pool_size
            )

            self._rfc_client = SAPRFCClient(rfc_config)
            await self._rfc_client.connect()

            # Conectar OData se habilitado
            if self.sap_config.odata_enabled:
                odata_config = SAPODataConfig(
                    base_url=self.sap_config.odata_url,
                    username=self.sap_config.odata_username,
                    password=self.sap_config.odata_password,
                    client=self.sap_config.rfc_client
                )

                self._odata_client = SAPODataClient(odata_config)
                await self._odata_client.connect()

            # Inicializar analisadores
            self._abap_analyzer = ABAPAnalyzer(self._rfc_client)
            self._table_analyzer = TableAnalyzer(self._rfc_client)
            self._badi_analyzer = BADIAnalyzer(self._rfc_client)
            self._config_analyzer = ConfigAnalyzer(self._rfc_client)

            # Inicializar skills
            self._read_skill = SAPReadSkill(self._rfc_client)
            self._abap_skill = SAPABAPSkill(self._rfc_client)
            self._config_skill = SAPConfigSkill(self._rfc_client)

            self._connection_status = SAPConnectionStatus.CONNECTED
            self._connected_at = datetime.now()
            self.status = IntegrationStatus.CONNECTED

            logger.info(
                f"Conectado ao SAP: {self.sap_config.rfc_host or self.sap_config.rfc_mshost} "
                f"mandante {self.sap_config.rfc_client}"
            )

            return True

        except Exception as e:
            self._connection_status = SAPConnectionStatus.ERROR
            self._last_error = str(e)
            self.status = IntegrationStatus.ERROR
            logger.error(f"Erro ao conectar ao SAP: {e}")
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do SAP.

        Returns:
            True se desconectou com sucesso
        """
        try:
            if self._rfc_client:
                await self._rfc_client.disconnect()
                self._rfc_client = None

            if self._odata_client:
                await self._odata_client.disconnect()
                self._odata_client = None

            # Limpar analisadores e skills
            self._abap_analyzer = None
            self._table_analyzer = None
            self._badi_analyzer = None
            self._config_analyzer = None
            self._read_skill = None
            self._abap_skill = None
            self._config_skill = None

            self._connection_status = SAPConnectionStatus.DISCONNECTED
            self.status = IntegrationStatus.DISCONNECTED

            logger.info("Desconectado do SAP")
            return True

        except Exception as e:
            logger.error(f"Erro ao desconectar do SAP: {e}")
            return False

    async def test_connection(self) -> bool:
        """
        Testa a conexao SAP.

        Returns:
            True se a conexao esta funcionando
        """
        if not self._rfc_client:
            return False

        return await self._rfc_client.test_connection()

    # =========================================================================
    # Interface IntegrationBase (nao aplicavel para SAP)
    # =========================================================================

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Nao aplicavel - SAP nao e sistema de gestao de stories"""
        return SyncResult(
            success=False,
            errors=["Sincronizacao de stories nao suportada para SAP ECC"]
        )

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Nao aplicavel - SAP nao e sistema de gestao de stories"""
        return SyncResult(
            success=False,
            errors=["Sincronizacao de stories nao suportada para SAP ECC"]
        )

    async def handle_webhook(self, payload: Dict) -> bool:
        """Nao aplicavel - SAP usa IDocs para integracao"""
        return False

    # =========================================================================
    # Metodos de Conveniencia
    # =========================================================================

    async def get_system_info(self) -> Dict[str, Any]:
        """
        Obtem informacoes do sistema SAP.

        Returns:
            Dicionario com informacoes
        """
        if not self._rfc_client or not self._rfc_client.is_connected:
            return {"error": "Nao conectado"}

        return await self._rfc_client.get_system_info()

    async def read_table(
        self,
        table_name: str,
        fields: Optional[List[str]] = None,
        where: Optional[str] = None,
        max_rows: int = 100
    ) -> List[Dict]:
        """
        Le dados de uma tabela SAP.

        Args:
            table_name: Nome da tabela
            fields: Campos a retornar
            where: Clausula WHERE
            max_rows: Maximo de linhas

        Returns:
            Lista de registros
        """
        if not self._rfc_client:
            return []

        options = None
        if where:
            options = [{"TEXT": where}]

        return await self._rfc_client.read_table(
            table_name,
            fields=fields,
            options=options,
            max_rows=max_rows
        )

    async def call_bapi(
        self,
        bapi_name: str,
        parameters: Dict[str, Any],
        auto_commit: bool = True
    ):
        """
        Executa uma BAPI.

        Args:
            bapi_name: Nome da BAPI
            parameters: Parametros
            auto_commit: Commit automatico

        Returns:
            Resultado da BAPI
        """
        if not self._rfc_client:
            return None

        return await self._rfc_client.call_bapi(
            bapi_name,
            parameters,
            auto_commit=auto_commit
        )

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        base_status = super().get_status()

        base_status.update({
            "sap_status": self._connection_status.value,
            "server": self.sap_config.rfc_host or self.sap_config.rfc_mshost,
            "client": self.sap_config.rfc_client,
            "connected_at": self._connected_at.isoformat() if self._connected_at else None,
            "odata_enabled": self.sap_config.odata_enabled,
            "enabled_modules": [m.value for m in self.sap_config.enabled_modules]
        })

        if self._rfc_client:
            base_status["rfc_stats"] = self._rfc_client.get_statistics()

        return base_status

    # =========================================================================
    # Context Manager
    # =========================================================================

    async def __aenter__(self):
        """Context manager - entrada"""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Context manager - saida"""
        await self.disconnect()
        return False
