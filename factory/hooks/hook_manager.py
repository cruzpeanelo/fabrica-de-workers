"""
Hook Manager - Gerenciador de hooks da Fabrica de Agentes.

Permite registrar callbacks para eventos do sistema.
"""

import os
import json
import importlib
from pathlib import Path
from datetime import datetime
from typing import Optional, Dict, List, Callable, Any
from dataclasses import dataclass, field, asdict
from enum import Enum


class HookEvent(Enum):
    """Eventos disponiveis para hooks."""
    # Git events
    PRE_COMMIT = "pre_commit"
    POST_COMMIT = "post_commit"

    # Task events
    PRE_TASK = "pre_task"
    POST_TASK = "post_task"

    # Handoff events
    PRE_HANDOFF = "pre_handoff"
    POST_HANDOFF = "post_handoff"

    # Agent events
    ON_SPAWN = "on_spawn"
    ON_TERMINATE = "on_terminate"

    # System events
    ON_ERROR = "on_error"
    ON_CYCLE = "on_cycle"
    ON_INTERRUPT = "on_interrupt"

    # Custom
    CUSTOM = "custom"


@dataclass
class Hook:
    """Definicao de um hook."""
    hook_id: str
    event: HookEvent
    name: str
    description: str = ""
    module: str = ""  # Modulo Python a executar
    function: str = "execute"  # Funcao a chamar
    command: str = ""  # Comando shell a executar (alternativa ao module)
    enabled: bool = True
    priority: int = 100  # Menor = executa primeiro
    created_at: str = field(default_factory=lambda: datetime.now().isoformat())

    def to_dict(self) -> Dict:
        result = asdict(self)
        result["event"] = self.event.value
        return result

    @classmethod
    def from_dict(cls, data: Dict) -> "Hook":
        data = data.copy()
        if isinstance(data.get("event"), str):
            data["event"] = HookEvent(data["event"])
        return cls(**data)


class HookManager:
    """Gerenciador de hooks."""

    def __init__(self, base_path: Optional[str] = None):
        """
        Inicializa o gerenciador de hooks.

        Args:
            base_path: Caminho base do projeto
        """
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.hooks_path = self.base_path / "factory" / "hooks"
        self.custom_path = self.hooks_path / "custom"
        self.config_file = self.hooks_path / "hooks_config.json"

        # Hooks registrados
        self.hooks: Dict[str, Hook] = {}

        # Callbacks em memoria (para hooks programaticos)
        self.callbacks: Dict[HookEvent, List[Callable]] = {
            event: [] for event in HookEvent
        }

        # Carregar hooks
        self._load_hooks()

    def _load_hooks(self):
        """Carrega hooks do arquivo de configuracao."""
        if not self.config_file.exists():
            self._create_default_config()
            return

        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                data = json.load(f)

            for hook_data in data.get("hooks", []):
                hook = Hook.from_dict(hook_data)
                if hook.enabled:
                    self.hooks[hook.hook_id] = hook

        except Exception as e:
            print(f"[HookManager] Erro ao carregar hooks: {e}")

    def _create_default_config(self):
        """Cria configuracao padrao."""
        default_hooks = [
            Hook(
                hook_id="log_commits",
                event=HookEvent.POST_COMMIT,
                name="Log Commits",
                description="Loga commits realizados",
                module="factory.hooks.builtin.log_hook",
                enabled=True,
                priority=100
            ),
            Hook(
                hook_id="notify_errors",
                event=HookEvent.ON_ERROR,
                name="Notify Errors",
                description="Notifica sobre erros",
                module="factory.hooks.builtin.notify_hook",
                enabled=True,
                priority=50
            ),
        ]

        config = {
            "hooks": [h.to_dict() for h in default_hooks],
            "updated_at": datetime.now().isoformat()
        }

        self.config_file.parent.mkdir(parents=True, exist_ok=True)
        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(config, f, indent=2, ensure_ascii=False)

        for hook in default_hooks:
            self.hooks[hook.hook_id] = hook

    def _save_hooks(self):
        """Salva hooks no arquivo de configuracao."""
        config = {
            "hooks": [h.to_dict() for h in self.hooks.values()],
            "updated_at": datetime.now().isoformat()
        }

        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(config, f, indent=2, ensure_ascii=False)

    def register(self, hook: Hook) -> bool:
        """
        Registra um novo hook.

        Args:
            hook: Hook a registrar

        Returns:
            True se registrou com sucesso
        """
        if hook.hook_id in self.hooks:
            print(f"[HookManager] Hook ja existe: {hook.hook_id}")
            return False

        self.hooks[hook.hook_id] = hook
        self._save_hooks()
        print(f"[HookManager] Hook registrado: {hook.hook_id}")
        return True

    def unregister(self, hook_id: str) -> bool:
        """Remove um hook."""
        if hook_id not in self.hooks:
            return False

        del self.hooks[hook_id]
        self._save_hooks()
        return True

    def register_callback(self, event: HookEvent, callback: Callable):
        """
        Registra um callback em memoria.

        Args:
            event: Evento para o callback
            callback: Funcao a chamar (recebe context como argumento)
        """
        self.callbacks[event].append(callback)

    def unregister_callback(self, event: HookEvent, callback: Callable):
        """Remove um callback."""
        if callback in self.callbacks[event]:
            self.callbacks[event].remove(callback)

    def trigger(self, event: str | HookEvent, context: Dict[str, Any] = None):
        """
        Dispara um evento, executando todos os hooks registrados.

        Args:
            event: Nome do evento ou HookEvent
            context: Contexto passado para os hooks
        """
        if isinstance(event, str):
            try:
                event = HookEvent(event)
            except ValueError:
                event = HookEvent.CUSTOM

        context = context or {}
        context["event"] = event.value
        context["timestamp"] = datetime.now().isoformat()

        # Ordenar hooks por prioridade
        hooks_to_run = [
            h for h in self.hooks.values()
            if h.event == event and h.enabled
        ]
        hooks_to_run.sort(key=lambda h: h.priority)

        # Executar hooks registrados
        for hook in hooks_to_run:
            self._execute_hook(hook, context)

        # Executar callbacks em memoria
        for callback in self.callbacks[event]:
            try:
                callback(context)
            except Exception as e:
                print(f"[HookManager] Erro em callback: {e}")

    def _execute_hook(self, hook: Hook, context: Dict[str, Any]):
        """Executa um hook."""
        try:
            if hook.module:
                # Executar modulo Python
                module = importlib.import_module(hook.module)
                if hasattr(module, hook.function):
                    func = getattr(module, hook.function)
                    func(context)
                else:
                    print(f"[HookManager] Funcao {hook.function} nao encontrada em {hook.module}")

            elif hook.command:
                # Executar comando shell
                import subprocess
                result = subprocess.run(
                    hook.command,
                    shell=True,
                    capture_output=True,
                    text=True,
                    cwd=str(self.base_path),
                    env={**os.environ, "HOOK_CONTEXT": json.dumps(context)}
                )
                if result.returncode != 0:
                    print(f"[HookManager] Comando falhou: {result.stderr}")

        except Exception as e:
            print(f"[HookManager] Erro ao executar hook {hook.hook_id}: {e}")

    def list_hooks(self) -> List[str]:
        """Retorna lista de IDs de hooks."""
        return list(self.hooks.keys())

    def get_hook(self, hook_id: str) -> Optional[Hook]:
        """Retorna um hook pelo ID."""
        return self.hooks.get(hook_id)

    def get_hooks_by_event(self, event: HookEvent) -> List[Hook]:
        """Retorna hooks de um evento."""
        return [h for h in self.hooks.values() if h.event == event]

    def enable_hook(self, hook_id: str) -> bool:
        """Habilita um hook."""
        if hook_id not in self.hooks:
            return False
        self.hooks[hook_id].enabled = True
        self._save_hooks()
        return True

    def disable_hook(self, hook_id: str) -> bool:
        """Desabilita um hook."""
        if hook_id not in self.hooks:
            return False
        self.hooks[hook_id].enabled = False
        self._save_hooks()
        return True


# Instancia global
_hook_manager: Optional[HookManager] = None


def get_hook_manager(base_path: Optional[str] = None) -> HookManager:
    """Retorna instancia global do HookManager."""
    global _hook_manager
    if _hook_manager is None:
        _hook_manager = HookManager(base_path)
    return _hook_manager
