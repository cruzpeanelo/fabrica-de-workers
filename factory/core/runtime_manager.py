# -*- coding: utf-8 -*-
"""
RuntimeManager - Gerenciador de Tempo de Execucao Autonoma.

Controla a duracao da execucao autonoma do orquestrador:
- Duracoes pre-definidas (1h, 2h, 4h, 8h, 10h, 12h, 24h)
- Shutdown gracioso ao atingir o tempo
- Avisos antes do encerramento
- Extensao de tempo em execucao
- Persistencia de estado

Reutilizavel na plataforma Plataforma E.
"""

import os
import sys
import json
import time
import logging
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, Callable, List
from dataclasses import dataclass, field
from enum import Enum

# Configurar logging
logger = logging.getLogger(__name__)


class RuntimeStatus(str, Enum):
    """Status do runtime."""
    IDLE = "idle"
    RUNNING = "running"
    WARNING = "warning"
    STOPPING = "stopping"
    STOPPED = "stopped"
    PAUSED = "paused"


# Duracoes pre-definidas
DURATION_PRESETS = {
    "1h": 3600,
    "2h": 7200,
    "4h": 14400,
    "8h": 28800,
    "10h": 36000,
    "12h": 43200,
    "24h": 86400,
    "30m": 1800,
    "unlimited": None,
    "ilimitado": None,
}


def parse_duration(duration_str: str) -> Optional[int]:
    """
    Converte string de duracao para segundos.

    Args:
        duration_str: String como "1h", "2h", "30m", "3600", ou "unlimited"

    Returns:
        Segundos ou None para ilimitado
    """
    if not duration_str:
        return None

    duration_str = duration_str.lower().strip()

    # Verificar presets
    if duration_str in DURATION_PRESETS:
        return DURATION_PRESETS[duration_str]

    # Tentar parse como numero (segundos)
    try:
        return int(duration_str)
    except ValueError:
        pass

    # Tentar parse como formato "Xh" ou "Xm"
    if duration_str.endswith("h"):
        try:
            hours = int(duration_str[:-1])
            return hours * 3600
        except ValueError:
            pass
    elif duration_str.endswith("m"):
        try:
            minutes = int(duration_str[:-1])
            return minutes * 60
        except ValueError:
            pass
    elif duration_str.endswith("s"):
        try:
            return int(duration_str[:-1])
        except ValueError:
            pass

    logger.warning(f"Duracao invalida: {duration_str}, usando ilimitado")
    return None


def format_duration(seconds: Optional[int]) -> str:
    """
    Formata segundos para string legivel.

    Args:
        seconds: Segundos ou None

    Returns:
        String formatada como "2h 30m 15s"
    """
    if seconds is None:
        return "ilimitado"

    if seconds <= 0:
        return "0s"

    hours = seconds // 3600
    minutes = (seconds % 3600) // 60
    secs = seconds % 60

    parts = []
    if hours > 0:
        parts.append(f"{hours}h")
    if minutes > 0:
        parts.append(f"{minutes}m")
    if secs > 0 or not parts:
        parts.append(f"{secs}s")

    return " ".join(parts)


def format_time_remaining(seconds: Optional[int]) -> str:
    """
    Formata tempo restante para display.

    Args:
        seconds: Segundos restantes

    Returns:
        String formatada como "02:30:15"
    """
    if seconds is None:
        return "--:--:--"

    if seconds <= 0:
        return "00:00:00"

    hours = seconds // 3600
    minutes = (seconds % 3600) // 60
    secs = seconds % 60

    return f"{hours:02d}:{minutes:02d}:{secs:02d}"


@dataclass
class RuntimeConfig:
    """Configuracao do runtime."""
    duration_seconds: Optional[int] = None  # None = ilimitado
    graceful_shutdown_delay: int = 60       # segundos para cleanup
    warn_before_shutdown: int = 300         # aviso 5 min antes
    warn_intervals: List[int] = field(default_factory=lambda: [300, 60, 30, 10])  # avisos em segundos
    auto_save_state: bool = True            # salvar estado ao parar
    state_file: Optional[str] = None        # arquivo para salvar estado

    def to_dict(self) -> Dict[str, Any]:
        return {
            "duration_seconds": self.duration_seconds,
            "graceful_shutdown_delay": self.graceful_shutdown_delay,
            "warn_before_shutdown": self.warn_before_shutdown,
            "warn_intervals": self.warn_intervals,
            "auto_save_state": self.auto_save_state,
            "state_file": self.state_file
        }


@dataclass
class RuntimeStats:
    """Estatisticas do runtime."""
    cycles_completed: int = 0
    tasks_processed: int = 0
    issues_handled: int = 0
    agents_spawned: int = 0
    errors_count: int = 0
    warnings_shown: int = 0
    extensions_made: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "cycles_completed": self.cycles_completed,
            "tasks_processed": self.tasks_processed,
            "issues_handled": self.issues_handled,
            "agents_spawned": self.agents_spawned,
            "errors_count": self.errors_count,
            "warnings_shown": self.warnings_shown,
            "extensions_made": self.extensions_made
        }


class RuntimeManager:
    """
    Gerenciador de tempo de execucao autonoma.

    Controla quanto tempo o orquestrador deve executar,
    gerencia shutdown gracioso e fornece metricas de progresso.

    Exemplo de uso:
        config = RuntimeConfig(duration_seconds=7200)  # 2 horas
        runtime = RuntimeManager(config)
        runtime.start()

        while runtime.is_running():
            if runtime.should_shutdown():
                break
            if runtime.is_in_warning_period():
                print(f"Aviso: {runtime.check_time_remaining()}s restantes")
            # ... fazer trabalho ...

        runtime.stop()
    """

    def __init__(self, config: RuntimeConfig = None):
        """
        Inicializa o RuntimeManager.

        Args:
            config: Configuracao do runtime
        """
        self.config = config or RuntimeConfig()
        self.start_time: Optional[datetime] = None
        self.end_time: Optional[datetime] = None
        self.pause_time: Optional[datetime] = None
        self.total_paused_seconds: int = 0
        self._status = RuntimeStatus.IDLE
        self._warned_intervals: set = set()
        self._on_warning_callbacks: List[Callable[[int], None]] = []
        self._on_shutdown_callbacks: List[Callable[[], None]] = []
        self.stats = RuntimeStats()

        # Estado salvo
        self._state_path = Path(self.config.state_file) if self.config.state_file else None

    @property
    def status(self) -> RuntimeStatus:
        """Retorna status atual."""
        return self._status

    @status.setter
    def status(self, value: RuntimeStatus):
        """Define status e loga mudanca."""
        if self._status != value:
            logger.info(f"Runtime status: {self._status.value} -> {value.value}")
            self._status = value

    def start(self) -> None:
        """
        Inicia contagem de tempo.

        Raises:
            RuntimeError: Se ja estiver rodando
        """
        if self._status == RuntimeStatus.RUNNING:
            raise RuntimeError("Runtime ja esta em execucao")

        self.start_time = datetime.now()
        self._warned_intervals.clear()
        self.stats = RuntimeStats()

        if self.config.duration_seconds:
            self.end_time = self.start_time + timedelta(seconds=self.config.duration_seconds)
            logger.info(
                f"Runtime iniciado: duracao={format_duration(self.config.duration_seconds)}, "
                f"termino={self.end_time.strftime('%Y-%m-%d %H:%M:%S')}"
            )
        else:
            self.end_time = None
            logger.info("Runtime iniciado: duracao=ilimitada")

        self.status = RuntimeStatus.RUNNING

    def stop(self) -> Dict[str, Any]:
        """
        Para o runtime e retorna resumo.

        Returns:
            Dict com resumo da sessao
        """
        if self._status == RuntimeStatus.STOPPED:
            return self.get_summary()

        self.status = RuntimeStatus.STOPPING

        # Executar callbacks de shutdown
        for callback in self._on_shutdown_callbacks:
            try:
                callback()
            except Exception as e:
                logger.error(f"Erro em callback de shutdown: {e}")

        # Salvar estado se configurado
        if self.config.auto_save_state:
            self._save_state()

        self.status = RuntimeStatus.STOPPED

        summary = self.get_summary()
        logger.info(f"Runtime encerrado: {summary}")

        return summary

    def pause(self) -> None:
        """Pausa o runtime (nao conta tempo)."""
        if self._status == RuntimeStatus.RUNNING:
            self.pause_time = datetime.now()
            self.status = RuntimeStatus.PAUSED
            logger.info("Runtime pausado")

    def resume(self) -> None:
        """Retoma o runtime apos pausa."""
        if self._status == RuntimeStatus.PAUSED and self.pause_time:
            paused_duration = (datetime.now() - self.pause_time).total_seconds()
            self.total_paused_seconds += int(paused_duration)

            # Ajustar end_time se tiver duracao definida
            if self.end_time:
                self.end_time += timedelta(seconds=paused_duration)

            self.pause_time = None
            self.status = RuntimeStatus.RUNNING
            logger.info(f"Runtime retomado (pausado por {int(paused_duration)}s)")

    def is_running(self) -> bool:
        """Verifica se esta em execucao."""
        return self._status in (RuntimeStatus.RUNNING, RuntimeStatus.WARNING)

    def check_time_remaining(self) -> Optional[int]:
        """
        Retorna segundos restantes.

        Returns:
            Segundos restantes ou None se ilimitado
        """
        if self.config.duration_seconds is None:
            return None

        if self.end_time is None:
            return None

        if self._status == RuntimeStatus.PAUSED:
            return int((self.end_time - self.pause_time).total_seconds())

        remaining = (self.end_time - datetime.now()).total_seconds()
        return max(0, int(remaining))

    def should_shutdown(self) -> bool:
        """
        Verifica se deve encerrar.

        Returns:
            True se tempo excedido ou status de parada
        """
        if self._status in (RuntimeStatus.STOPPING, RuntimeStatus.STOPPED):
            return True

        if self.config.duration_seconds is None:
            return False

        remaining = self.check_time_remaining()
        if remaining is not None and remaining <= 0:
            return True

        return False

    def is_in_warning_period(self) -> bool:
        """
        Verifica se esta no periodo de aviso.

        Returns:
            True se proximo do encerramento
        """
        if self.config.duration_seconds is None:
            return False

        remaining = self.check_time_remaining()
        if remaining is None:
            return False

        return remaining <= self.config.warn_before_shutdown

    def check_warnings(self) -> Optional[int]:
        """
        Verifica e dispara avisos de tempo.

        Returns:
            Segundos restantes se deve avisar, None caso contrario
        """
        if self.config.duration_seconds is None:
            return None

        remaining = self.check_time_remaining()
        if remaining is None:
            return None

        # Verificar cada intervalo de aviso
        for interval in self.config.warn_intervals:
            if remaining <= interval and interval not in self._warned_intervals:
                self._warned_intervals.add(interval)
                self.stats.warnings_shown += 1

                if remaining <= self.config.warn_before_shutdown:
                    self.status = RuntimeStatus.WARNING

                # Executar callbacks
                for callback in self._on_warning_callbacks:
                    try:
                        callback(remaining)
                    except Exception as e:
                        logger.error(f"Erro em callback de aviso: {e}")

                return remaining

        return None

    def get_elapsed(self) -> int:
        """
        Retorna segundos decorridos.

        Returns:
            Segundos desde o inicio (descontando pausas)
        """
        if self.start_time is None:
            return 0

        if self._status == RuntimeStatus.PAUSED and self.pause_time:
            elapsed = (self.pause_time - self.start_time).total_seconds()
        else:
            elapsed = (datetime.now() - self.start_time).total_seconds()

        return max(0, int(elapsed - self.total_paused_seconds))

    def get_progress(self) -> float:
        """
        Retorna progresso de 0.0 a 1.0.

        Returns:
            Fracao do tempo decorrido
        """
        if self.config.duration_seconds is None:
            return 0.0

        if self.config.duration_seconds == 0:
            return 1.0

        elapsed = self.get_elapsed()
        progress = elapsed / self.config.duration_seconds

        return min(1.0, max(0.0, progress))

    def extend_duration(self, seconds: int) -> None:
        """
        Estende tempo de execucao.

        Args:
            seconds: Segundos adicionais
        """
        if seconds <= 0:
            return

        if self.config.duration_seconds is None:
            # Converter de ilimitado para limitado
            self.config.duration_seconds = self.get_elapsed() + seconds
            self.end_time = datetime.now() + timedelta(seconds=seconds)
        else:
            self.config.duration_seconds += seconds
            if self.end_time:
                self.end_time += timedelta(seconds=seconds)

        # Limpar avisos ja mostrados para que possam ser mostrados novamente
        self._warned_intervals.clear()
        self.stats.extensions_made += 1

        if self._status == RuntimeStatus.WARNING:
            self.status = RuntimeStatus.RUNNING

        logger.info(
            f"Runtime estendido em {format_duration(seconds)}, "
            f"novo termino: {self.end_time.strftime('%Y-%m-%d %H:%M:%S') if self.end_time else 'ilimitado'}"
        )

    def set_duration(self, seconds: Optional[int]) -> None:
        """
        Define nova duracao total.

        Args:
            seconds: Nova duracao em segundos ou None para ilimitado
        """
        self.config.duration_seconds = seconds

        if seconds is None:
            self.end_time = None
        elif self.start_time:
            self.end_time = self.start_time + timedelta(seconds=seconds)

        self._warned_intervals.clear()

        logger.info(f"Runtime duracao definida: {format_duration(seconds)}")

    def on_warning(self, callback: Callable[[int], None]) -> None:
        """
        Registra callback para avisos de tempo.

        Args:
            callback: Funcao que recebe segundos restantes
        """
        self._on_warning_callbacks.append(callback)

    def on_shutdown(self, callback: Callable[[], None]) -> None:
        """
        Registra callback para shutdown.

        Args:
            callback: Funcao chamada ao encerrar
        """
        self._on_shutdown_callbacks.append(callback)

    def increment_stat(self, stat_name: str, value: int = 1) -> None:
        """
        Incrementa estatistica.

        Args:
            stat_name: Nome da estatistica
            value: Valor a incrementar
        """
        if hasattr(self.stats, stat_name):
            current = getattr(self.stats, stat_name)
            setattr(self.stats, stat_name, current + value)

    def get_status_dict(self) -> Dict[str, Any]:
        """
        Retorna status como dicionario.

        Returns:
            Dict com todos os dados de status
        """
        remaining = self.check_time_remaining()

        return {
            "status": self._status.value,
            "started_at": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "duration_configured": self.config.duration_seconds,
            "duration_formatted": format_duration(self.config.duration_seconds),
            "elapsed_seconds": self.get_elapsed(),
            "elapsed_formatted": format_duration(self.get_elapsed()),
            "remaining_seconds": remaining,
            "remaining_formatted": format_time_remaining(remaining),
            "progress": self.get_progress(),
            "progress_percent": f"{self.get_progress() * 100:.1f}%",
            "is_unlimited": self.config.duration_seconds is None,
            "is_warning": self._status == RuntimeStatus.WARNING,
            "total_paused_seconds": self.total_paused_seconds,
            "stats": self.stats.to_dict()
        }

    def get_summary(self) -> Dict[str, Any]:
        """
        Retorna resumo da sessao.

        Returns:
            Dict com resumo completo
        """
        return {
            "duration_configured": format_duration(self.config.duration_seconds),
            "duration_actual": format_duration(self.get_elapsed()),
            "started_at": self.start_time.isoformat() if self.start_time else None,
            "ended_at": datetime.now().isoformat(),
            "status": self._status.value,
            "stats": self.stats.to_dict(),
            "total_paused_seconds": self.total_paused_seconds
        }

    def _save_state(self) -> None:
        """Salva estado para arquivo."""
        if not self._state_path:
            return

        try:
            state = {
                "timestamp": datetime.now().isoformat(),
                "status": self.get_status_dict(),
                "summary": self.get_summary(),
                "config": self.config.to_dict()
            }

            self._state_path.parent.mkdir(parents=True, exist_ok=True)

            with open(self._state_path, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=2, ensure_ascii=False)

            logger.info(f"Estado salvo em: {self._state_path}")

        except Exception as e:
            logger.error(f"Erro ao salvar estado: {e}")

    def print_status_line(self) -> str:
        """
        Retorna linha de status formatada para terminal.

        Returns:
            String com status formatado
        """
        elapsed = format_duration(self.get_elapsed())
        remaining = self.check_time_remaining()

        if self.config.duration_seconds is None:
            return f"[{self._status.value.upper()}] Tempo: {elapsed} (ilimitado)"

        progress_pct = self.get_progress() * 100
        remaining_str = format_time_remaining(remaining)
        total_str = format_duration(self.config.duration_seconds)

        # Barra de progresso ASCII
        bar_width = 20
        filled = int(bar_width * self.get_progress())
        bar = "=" * filled + "-" * (bar_width - filled)

        status_icon = "!" if self._status == RuntimeStatus.WARNING else "*"

        return (
            f"[{status_icon}] [{bar}] {progress_pct:.1f}% | "
            f"{elapsed} / {total_str} | Restante: {remaining_str}"
        )


# Instancia global
_runtime_manager: Optional[RuntimeManager] = None


def get_runtime_manager(config: RuntimeConfig = None) -> RuntimeManager:
    """
    Retorna instancia global do RuntimeManager.

    Args:
        config: Configuracao (apenas na primeira chamada)

    Returns:
        RuntimeManager singleton
    """
    global _runtime_manager
    if _runtime_manager is None:
        _runtime_manager = RuntimeManager(config)
    return _runtime_manager


def reset_runtime_manager() -> None:
    """Reseta instancia global."""
    global _runtime_manager
    if _runtime_manager:
        if _runtime_manager.is_running():
            _runtime_manager.stop()
    _runtime_manager = None


if __name__ == "__main__":
    # Demo: Testar RuntimeManager
    import time as time_module

    print("\n=== DEMO: RuntimeManager ===\n")

    # Configurar para 10 segundos com avisos em 5s e 3s
    config = RuntimeConfig(
        duration_seconds=10,
        warn_intervals=[5, 3, 1],
        warn_before_shutdown=5
    )

    runtime = RuntimeManager(config)

    # Registrar callback de aviso
    def on_warning(remaining: int):
        print(f"\n[AVISO] {remaining} segundos restantes!\n")

    runtime.on_warning(on_warning)

    # Iniciar
    runtime.start()
    print(f"Iniciado: {runtime.start_time}")
    print(f"Termino previsto: {runtime.end_time}")
    print()

    # Loop de demo
    while runtime.is_running():
        if runtime.should_shutdown():
            print("\n[!] Tempo esgotado!")
            break

        # Verificar avisos
        runtime.check_warnings()

        # Mostrar status
        print(f"\r{runtime.print_status_line()}", end="", flush=True)

        # Simular trabalho
        runtime.increment_stat("cycles_completed")
        time_module.sleep(1)

    # Encerrar
    summary = runtime.stop()

    print("\n\n=== Resumo da Sessao ===")
    print(f"Duracao configurada: {summary['duration_configured']}")
    print(f"Duracao real: {summary['duration_actual']}")
    print(f"Ciclos: {summary['stats']['cycles_completed']}")
    print(f"Avisos: {summary['stats']['warnings_shown']}")
