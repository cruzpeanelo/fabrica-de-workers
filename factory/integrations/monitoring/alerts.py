# -*- coding: utf-8 -*-
"""
Alert System
============
Sistema de alertas para integracoes.

Issue #333 - Terminal A

Tipos de alertas:
- Integration Down: Integracao nao responde
- High Error Rate: Taxa de erro acima do limite
- High Latency: Latencia acima do limite
- Recovery: Integracao recuperada
"""

import asyncio
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


class AlertLevel(str, Enum):
    """Nivel de alerta"""
    INFO = "info"           # Informativo
    WARNING = "warning"     # Aviso
    ERROR = "error"         # Erro
    CRITICAL = "critical"   # Critico


class AlertType(str, Enum):
    """Tipo de alerta"""
    INTEGRATION_DOWN = "integration_down"
    INTEGRATION_UP = "integration_up"
    HIGH_ERROR_RATE = "high_error_rate"
    HIGH_LATENCY = "high_latency"
    CONSECUTIVE_FAILURES = "consecutive_failures"
    RECOVERY = "recovery"
    CUSTOM = "custom"


@dataclass
class Alert:
    """Representacao de um alerta"""
    alert_id: str
    alert_type: AlertType
    level: AlertLevel
    integration_name: str
    message: str
    created_at: datetime = field(default_factory=datetime.utcnow)
    resolved_at: Optional[datetime] = None
    acknowledged: bool = False
    acknowledged_by: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    @property
    def is_resolved(self) -> bool:
        return self.resolved_at is not None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "alert_id": self.alert_id,
            "type": self.alert_type.value,
            "level": self.level.value,
            "integration": self.integration_name,
            "message": self.message,
            "created_at": self.created_at.isoformat(),
            "resolved_at": self.resolved_at.isoformat() if self.resolved_at else None,
            "acknowledged": self.acknowledged,
            "acknowledged_by": self.acknowledged_by,
            "metadata": self.metadata
        }


class AlertHandler(ABC):
    """Interface para handlers de alerta"""

    @abstractmethod
    async def send(self, alert: Alert):
        """Envia o alerta"""
        pass


class LogAlertHandler(AlertHandler):
    """Handler que loga alertas"""

    async def send(self, alert: Alert):
        level_map = {
            AlertLevel.INFO: logging.INFO,
            AlertLevel.WARNING: logging.WARNING,
            AlertLevel.ERROR: logging.ERROR,
            AlertLevel.CRITICAL: logging.CRITICAL
        }
        log_level = level_map.get(alert.level, logging.INFO)
        logger.log(
            log_level,
            f"[{alert.alert_type.value}] {alert.integration_name}: {alert.message}"
        )


class WebhookAlertHandler(AlertHandler):
    """Handler que envia alertas via webhook"""

    def __init__(self, webhook_url: str):
        self.webhook_url = webhook_url

    async def send(self, alert: Alert):
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    self.webhook_url,
                    json=alert.to_dict()
                ) as response:
                    if response.status >= 400:
                        logger.warning(
                            f"Webhook alert failed: {response.status}"
                        )
        except Exception as e:
            logger.error(f"Failed to send webhook alert: {e}")


class EmailAlertHandler(AlertHandler):
    """Handler que envia alertas por email"""

    def __init__(self, recipients: List[str], smtp_config: Dict):
        self.recipients = recipients
        self.smtp_config = smtp_config

    async def send(self, alert: Alert):
        # Implementacao de envio de email
        logger.info(
            f"Email alert would be sent to {self.recipients}: "
            f"{alert.message}"
        )


class AlertManager:
    """
    Gerenciador de alertas.

    Exemplo:
        manager = AlertManager()

        # Adicionar handler
        manager.add_handler(LogAlertHandler())
        manager.add_handler(WebhookAlertHandler("https://..."))

        # Disparar alerta
        await manager.on_integration_failure("jira", "Connection timeout")

        # Obter alertas ativos
        active = manager.get_active_alerts()
    """

    # Limiares padrao
    DEFAULT_ERROR_RATE_THRESHOLD = 0.1    # 10%
    DEFAULT_LATENCY_THRESHOLD_MS = 5000   # 5 segundos
    DEFAULT_FAILURE_THRESHOLD = 3          # 3 falhas consecutivas

    def __init__(
        self,
        error_rate_threshold: float = DEFAULT_ERROR_RATE_THRESHOLD,
        latency_threshold_ms: float = DEFAULT_LATENCY_THRESHOLD_MS,
        failure_threshold: int = DEFAULT_FAILURE_THRESHOLD
    ):
        """
        Inicializa o gerenciador.

        Args:
            error_rate_threshold: Limite de taxa de erro
            latency_threshold_ms: Limite de latencia em ms
            failure_threshold: Limite de falhas consecutivas
        """
        self.error_rate_threshold = error_rate_threshold
        self.latency_threshold_ms = latency_threshold_ms
        self.failure_threshold = failure_threshold

        self._handlers: List[AlertHandler] = [LogAlertHandler()]
        self._alerts: Dict[str, Alert] = {}
        self._active_alerts: Dict[str, str] = {}  # integration -> alert_id
        self._alert_counter = 0
        self._cooldown: Dict[str, datetime] = {}  # Evita spam de alertas

    def add_handler(self, handler: AlertHandler):
        """Adiciona handler de alerta"""
        self._handlers.append(handler)

    def remove_handler(self, handler: AlertHandler):
        """Remove handler de alerta"""
        if handler in self._handlers:
            self._handlers.remove(handler)

    def _generate_alert_id(self) -> str:
        """Gera ID unico para alerta"""
        self._alert_counter += 1
        return f"ALERT-{datetime.utcnow().strftime('%Y%m%d%H%M%S')}-{self._alert_counter:04d}"

    def _is_in_cooldown(self, integration: str, alert_type: AlertType) -> bool:
        """Verifica se alerta esta em cooldown"""
        key = f"{integration}:{alert_type.value}"
        if key in self._cooldown:
            cooldown_end = self._cooldown[key]
            if datetime.utcnow() < cooldown_end:
                return True
        return False

    def _set_cooldown(self, integration: str, alert_type: AlertType, seconds: int = 300):
        """Define cooldown para evitar spam"""
        key = f"{integration}:{alert_type.value}"
        self._cooldown[key] = datetime.utcnow() + timedelta(seconds=seconds)

    async def _send_alert(self, alert: Alert):
        """Envia alerta para todos os handlers"""
        for handler in self._handlers:
            try:
                await handler.send(alert)
            except Exception as e:
                logger.error(f"Alert handler error: {e}")

    async def on_integration_failure(
        self,
        integration_name: str,
        error: str,
        consecutive_failures: int = 1
    ):
        """
        Dispara alerta de falha de integracao.

        Args:
            integration_name: Nome da integracao
            error: Mensagem de erro
            consecutive_failures: Numero de falhas consecutivas
        """
        if consecutive_failures >= self.failure_threshold:
            await self._create_alert(
                integration_name,
                AlertType.INTEGRATION_DOWN,
                AlertLevel.CRITICAL,
                f"Integration {integration_name} is down: {error}",
                {"error": error, "consecutive_failures": consecutive_failures}
            )
        elif consecutive_failures >= 1:
            await self._create_alert(
                integration_name,
                AlertType.CONSECUTIVE_FAILURES,
                AlertLevel.WARNING,
                f"Integration {integration_name} failed ({consecutive_failures}x): {error}",
                {"error": error, "consecutive_failures": consecutive_failures}
            )

    async def on_integration_recovery(self, integration_name: str):
        """
        Dispara alerta de recuperacao de integracao.

        Args:
            integration_name: Nome da integracao
        """
        # Resolve alerta ativo
        if integration_name in self._active_alerts:
            alert_id = self._active_alerts[integration_name]
            if alert_id in self._alerts:
                self._alerts[alert_id].resolved_at = datetime.utcnow()
            del self._active_alerts[integration_name]

        await self._create_alert(
            integration_name,
            AlertType.RECOVERY,
            AlertLevel.INFO,
            f"Integration {integration_name} has recovered"
        )

    async def on_high_error_rate(
        self,
        integration_name: str,
        error_rate: float
    ):
        """
        Dispara alerta de alta taxa de erro.

        Args:
            integration_name: Nome da integracao
            error_rate: Taxa de erro (0-1)
        """
        if error_rate >= self.error_rate_threshold:
            await self._create_alert(
                integration_name,
                AlertType.HIGH_ERROR_RATE,
                AlertLevel.ERROR,
                f"High error rate for {integration_name}: {error_rate:.1%}",
                {"error_rate": error_rate, "threshold": self.error_rate_threshold}
            )

    async def on_high_latency(
        self,
        integration_name: str,
        latency_ms: float
    ):
        """
        Dispara alerta de alta latencia.

        Args:
            integration_name: Nome da integracao
            latency_ms: Latencia em milissegundos
        """
        if latency_ms >= self.latency_threshold_ms:
            await self._create_alert(
                integration_name,
                AlertType.HIGH_LATENCY,
                AlertLevel.WARNING,
                f"High latency for {integration_name}: {latency_ms:.0f}ms",
                {"latency_ms": latency_ms, "threshold": self.latency_threshold_ms}
            )

    async def _create_alert(
        self,
        integration_name: str,
        alert_type: AlertType,
        level: AlertLevel,
        message: str,
        metadata: Optional[Dict] = None
    ):
        """Cria e envia alerta"""
        # Verifica cooldown
        if self._is_in_cooldown(integration_name, alert_type):
            return

        alert = Alert(
            alert_id=self._generate_alert_id(),
            alert_type=alert_type,
            level=level,
            integration_name=integration_name,
            message=message,
            metadata=metadata or {}
        )

        self._alerts[alert.alert_id] = alert

        # Marca como alerta ativo
        if alert_type in (AlertType.INTEGRATION_DOWN, AlertType.HIGH_ERROR_RATE):
            self._active_alerts[integration_name] = alert.alert_id

        # Envia alerta
        await self._send_alert(alert)

        # Define cooldown
        self._set_cooldown(integration_name, alert_type)

        return alert

    def acknowledge_alert(self, alert_id: str, user: str):
        """Reconhece um alerta"""
        if alert_id in self._alerts:
            self._alerts[alert_id].acknowledged = True
            self._alerts[alert_id].acknowledged_by = user
            return True
        return False

    def resolve_alert(self, alert_id: str):
        """Resolve um alerta"""
        if alert_id in self._alerts:
            self._alerts[alert_id].resolved_at = datetime.utcnow()
            # Remove dos ativos
            for integration, active_id in list(self._active_alerts.items()):
                if active_id == alert_id:
                    del self._active_alerts[integration]
            return True
        return False

    def get_active_alerts(self) -> List[Alert]:
        """Obtem alertas ativos (nao resolvidos)"""
        return [
            alert for alert in self._alerts.values()
            if not alert.is_resolved
        ]

    def get_alerts_by_integration(self, integration_name: str) -> List[Alert]:
        """Obtem alertas de uma integracao"""
        return [
            alert for alert in self._alerts.values()
            if alert.integration_name == integration_name
        ]

    def get_alert(self, alert_id: str) -> Optional[Alert]:
        """Obtem alerta por ID"""
        return self._alerts.get(alert_id)

    def clear_old_alerts(self, days: int = 7):
        """Remove alertas antigos resolvidos"""
        cutoff = datetime.utcnow() - timedelta(days=days)
        to_remove = [
            alert_id for alert_id, alert in self._alerts.items()
            if alert.is_resolved and alert.resolved_at < cutoff
        ]
        for alert_id in to_remove:
            del self._alerts[alert_id]


# Instancia global
alert_manager = AlertManager()
