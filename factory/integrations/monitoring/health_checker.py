# -*- coding: utf-8 -*-
"""
Health Checker
==============
Sistema de health check para integracoes.

Issue #333 - Terminal A

Verifica o status de cada integracao:
- Jira
- Azure DevOps
- Salesforce
- SAP S/4 HANA
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


class HealthStatus(str, Enum):
    """Status de saude da integracao"""
    HEALTHY = "healthy"           # Funcionando normalmente
    DEGRADED = "degraded"         # Funcionando com problemas
    UNHEALTHY = "unhealthy"       # Nao funcionando
    UNKNOWN = "unknown"           # Status desconhecido
    CHECKING = "checking"         # Verificacao em andamento


@dataclass
class IntegrationHealth:
    """Informacoes de saude de uma integracao"""
    name: str
    status: HealthStatus = HealthStatus.UNKNOWN
    last_check: Optional[datetime] = None
    last_success: Optional[datetime] = None
    response_time_ms: Optional[float] = None
    error_message: Optional[str] = None
    consecutive_failures: int = 0
    details: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "status": self.status.value,
            "last_check": self.last_check.isoformat() if self.last_check else None,
            "last_success": self.last_success.isoformat() if self.last_success else None,
            "response_time_ms": self.response_time_ms,
            "error_message": self.error_message,
            "consecutive_failures": self.consecutive_failures,
            "details": self.details
        }


@dataclass
class HealthCheckResult:
    """Resultado de um health check"""
    overall_status: HealthStatus
    integrations: Dict[str, IntegrationHealth]
    checked_at: datetime = field(default_factory=datetime.utcnow)
    total_healthy: int = 0
    total_unhealthy: int = 0
    total_degraded: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "overall_status": self.overall_status.value,
            "checked_at": self.checked_at.isoformat(),
            "summary": {
                "healthy": self.total_healthy,
                "degraded": self.total_degraded,
                "unhealthy": self.total_unhealthy
            },
            "integrations": {
                name: health.to_dict()
                for name, health in self.integrations.items()
            }
        }


class HealthChecker:
    """
    Gerenciador de health checks para integracoes.

    Exemplo:
        checker = HealthChecker()

        # Verificar todas as integracoes
        result = await checker.check_all()

        # Verificar integracao especifica
        health = await checker.check_integration("jira")

        # Obter ultimo status
        status = checker.get_status("salesforce")
    """

    # Integracoes suportadas
    INTEGRATIONS = [
        "jira",
        "azure_devops",
        "salesforce",
        "sap_s4"
    ]

    def __init__(
        self,
        check_interval_seconds: int = 60,
        timeout_seconds: int = 10,
        failure_threshold: int = 3
    ):
        """
        Inicializa o health checker.

        Args:
            check_interval_seconds: Intervalo entre verificacoes
            timeout_seconds: Timeout para cada verificacao
            failure_threshold: Falhas consecutivas para marcar como unhealthy
        """
        self.check_interval = check_interval_seconds
        self.timeout = timeout_seconds
        self.failure_threshold = failure_threshold

        self._health_status: Dict[str, IntegrationHealth] = {}
        self._check_handlers: Dict[str, Callable] = {}
        self._background_task: Optional[asyncio.Task] = None

        # Inicializa status para todas as integracoes
        for name in self.INTEGRATIONS:
            self._health_status[name] = IntegrationHealth(name=name)

        # Registra handlers padrao
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Registra handlers de verificacao padrao"""
        self._check_handlers["jira"] = self._check_jira
        self._check_handlers["azure_devops"] = self._check_azure_devops
        self._check_handlers["salesforce"] = self._check_salesforce
        self._check_handlers["sap_s4"] = self._check_sap_s4

    def register_handler(
        self,
        integration_name: str,
        handler: Callable
    ):
        """
        Registra handler customizado para verificacao.

        Args:
            integration_name: Nome da integracao
            handler: Funcao async que retorna (success, response_time, details)
        """
        self._check_handlers[integration_name] = handler

    async def check_all(self) -> HealthCheckResult:
        """
        Verifica todas as integracoes.

        Returns:
            Resultado agregado de todas as verificacoes
        """
        tasks = [
            self.check_integration(name)
            for name in self.INTEGRATIONS
        ]

        await asyncio.gather(*tasks, return_exceptions=True)

        # Calcula status geral
        healthy = sum(
            1 for h in self._health_status.values()
            if h.status == HealthStatus.HEALTHY
        )
        degraded = sum(
            1 for h in self._health_status.values()
            if h.status == HealthStatus.DEGRADED
        )
        unhealthy = sum(
            1 for h in self._health_status.values()
            if h.status == HealthStatus.UNHEALTHY
        )

        if unhealthy > 0:
            overall = HealthStatus.UNHEALTHY
        elif degraded > 0:
            overall = HealthStatus.DEGRADED
        elif healthy == len(self.INTEGRATIONS):
            overall = HealthStatus.HEALTHY
        else:
            overall = HealthStatus.UNKNOWN

        return HealthCheckResult(
            overall_status=overall,
            integrations=self._health_status.copy(),
            total_healthy=healthy,
            total_degraded=degraded,
            total_unhealthy=unhealthy
        )

    async def check_integration(self, name: str) -> IntegrationHealth:
        """
        Verifica uma integracao especifica.

        Args:
            name: Nome da integracao

        Returns:
            Status de saude da integracao
        """
        if name not in self._health_status:
            self._health_status[name] = IntegrationHealth(name=name)

        health = self._health_status[name]
        health.status = HealthStatus.CHECKING
        health.last_check = datetime.utcnow()

        handler = self._check_handlers.get(name)
        if not handler:
            health.status = HealthStatus.UNKNOWN
            health.error_message = "No health check handler registered"
            return health

        try:
            start_time = datetime.utcnow()
            success, details = await asyncio.wait_for(
                handler(),
                timeout=self.timeout
            )
            end_time = datetime.utcnow()

            response_time = (end_time - start_time).total_seconds() * 1000
            health.response_time_ms = response_time
            health.details = details or {}

            if success:
                health.status = HealthStatus.HEALTHY
                health.last_success = datetime.utcnow()
                health.consecutive_failures = 0
                health.error_message = None
            else:
                health.consecutive_failures += 1
                if health.consecutive_failures >= self.failure_threshold:
                    health.status = HealthStatus.UNHEALTHY
                else:
                    health.status = HealthStatus.DEGRADED
                health.error_message = details.get("error", "Health check failed")

        except asyncio.TimeoutError:
            health.status = HealthStatus.UNHEALTHY
            health.error_message = f"Health check timeout ({self.timeout}s)"
            health.consecutive_failures += 1

        except Exception as e:
            health.status = HealthStatus.UNHEALTHY
            health.error_message = str(e)
            health.consecutive_failures += 1
            logger.error(f"Health check error for {name}: {e}")

        return health

    def get_status(self, name: str) -> Optional[IntegrationHealth]:
        """
        Obtem ultimo status de uma integracao.

        Args:
            name: Nome da integracao

        Returns:
            Status de saude ou None se nao encontrado
        """
        return self._health_status.get(name)

    def get_all_status(self) -> Dict[str, IntegrationHealth]:
        """Obtem status de todas as integracoes"""
        return self._health_status.copy()

    async def start_background_checks(self):
        """Inicia verificacoes periodicas em background"""
        if self._background_task:
            return

        async def check_loop():
            while True:
                try:
                    await self.check_all()
                    await asyncio.sleep(self.check_interval)
                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Background health check error: {e}")
                    await asyncio.sleep(self.check_interval)

        self._background_task = asyncio.create_task(check_loop())
        logger.info(f"Started background health checks (interval: {self.check_interval}s)")

    async def stop_background_checks(self):
        """Para verificacoes periodicas"""
        if self._background_task:
            self._background_task.cancel()
            try:
                await self._background_task
            except asyncio.CancelledError:
                pass
            self._background_task = None
            logger.info("Stopped background health checks")

    # =========================================================================
    # Handlers de verificacao por integracao
    # =========================================================================

    async def _check_jira(self) -> tuple:
        """Verifica conexao com Jira"""
        try:
            from ..jira import JiraIntegration, JiraConfig
            from ..jira.config import JiraConfig

            config = JiraConfig.from_env() if hasattr(JiraConfig, 'from_env') else None
            if not config:
                return False, {"error": "Jira not configured"}

            integration = JiraIntegration(config)
            connected = await integration.connect()

            if connected:
                return True, {"connected": True, "server": config.base_url}
            else:
                return False, {"error": "Failed to connect"}

        except ImportError:
            return False, {"error": "Jira module not available"}
        except Exception as e:
            return False, {"error": str(e)}

    async def _check_azure_devops(self) -> tuple:
        """Verifica conexao com Azure DevOps"""
        try:
            from ..azure_devops import AzureDevOpsIntegration, AzureDevOpsConfig

            config = AzureDevOpsConfig.from_env()
            if not config.is_valid():
                return False, {"error": "Azure DevOps not configured"}

            integration = AzureDevOpsIntegration(config)
            connected = await integration.connect()

            if connected:
                return True, {
                    "connected": True,
                    "organization": config.organization
                }
            else:
                return False, {"error": "Failed to connect"}

        except ImportError:
            return False, {"error": "Azure DevOps module not available"}
        except Exception as e:
            return False, {"error": str(e)}

    async def _check_salesforce(self) -> tuple:
        """Verifica conexao com Salesforce"""
        try:
            from ..salesforce_connector import SalesforceConnector
            from ..salesforce import SalesforceConfig

            config = SalesforceConfig.from_env()
            if not config.username:
                return False, {"error": "Salesforce not configured"}

            connector = SalesforceConnector(config)
            connected = await connector.connect()

            if connected:
                return True, {
                    "connected": True,
                    "instance_url": config.instance_url
                }
            else:
                return False, {"error": "Failed to connect"}

        except ImportError:
            return False, {"error": "Salesforce module not available"}
        except Exception as e:
            return False, {"error": str(e)}

    async def _check_sap_s4(self) -> tuple:
        """Verifica conexao com SAP S/4 HANA"""
        try:
            from ..sap_s4hana import SAPS4HanaIntegration

            integration = SAPS4HanaIntegration.from_environment()
            if not integration:
                return False, {"error": "SAP S/4 not configured"}

            connected = await integration.connect()

            if connected:
                return True, {
                    "connected": True,
                    "system_url": integration.config.system_url if integration.config else None
                }
            else:
                return False, {"error": "Failed to connect"}

        except ImportError:
            return False, {"error": "SAP S/4 module not available"}
        except Exception as e:
            return False, {"error": str(e)}
