"""
Factory Hooks - Sistema de Hooks para a Fabrica de Agentes.

Hooks permitem executar codigo antes/depois de eventos importantes como:
- pre_commit / post_commit
- pre_task / post_task
- pre_handoff / post_handoff
- on_error, on_spawn, on_terminate
"""

from .hook_manager import HookManager, Hook, HookEvent

__all__ = ["HookManager", "Hook", "HookEvent"]
