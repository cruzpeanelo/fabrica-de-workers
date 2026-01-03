"""
MCP Manager - Gerenciador de servidores MCP.

Gerencia o ciclo de vida dos servidores MCP:
- Iniciar/parar servidores
- Monitorar status
- Configurar conexoes
"""

import os
import json
import subprocess
import time
from pathlib import Path
from typing import Optional, Dict, List, Any
from dataclasses import dataclass, field, asdict
from datetime import datetime


@dataclass
class MCPServerConfig:
    """Configuracao de um servidor MCP."""
    name: str
    command: str
    args: List[str] = field(default_factory=list)
    env: Dict[str, str] = field(default_factory=dict)
    cwd: Optional[str] = None
    auto_start: bool = False
    enabled: bool = True
    description: str = ""


@dataclass
class MCPServerStatus:
    """Status de um servidor MCP."""
    name: str
    running: bool
    pid: Optional[int] = None
    started_at: Optional[str] = None
    error: Optional[str] = None


class MCPManager:
    """Gerenciador de servidores MCP."""

    def __init__(self, base_path: Optional[str] = None):
        """
        Inicializa o gerenciador.

        Args:
            base_path: Caminho base do projeto
        """
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.config_path = self.base_path / "factory" / "mcp" / "config"
        self.config_file = self.config_path / "mcp_servers.json"

        # Servidores configurados
        self.servers: Dict[str, MCPServerConfig] = {}

        # Processos ativos
        self.processes: Dict[str, subprocess.Popen] = {}

        # Carregar configuracao
        self._load_config()

        # Auto-start servers
        self._auto_start()

    def _load_config(self):
        """Carrega configuracao dos servidores."""
        if not self.config_file.exists():
            self._create_default_config()
            return

        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                data = json.load(f)

            for server_data in data.get("servers", []):
                config = MCPServerConfig(**server_data)
                self.servers[config.name] = config

        except Exception as e:
            print(f"[MCPManager] Erro ao carregar config: {e}")

    def _create_default_config(self):
        """Cria configuracao padrao com Playwright."""
        default_servers = [
            MCPServerConfig(
                name="playwright",
                command="python",
                args=[str(self.base_path / "factory" / "mcp" / "playwright" / "server.py")],
                env={"HEADLESS": "true"},
                auto_start=False,
                enabled=True,
                description="Automacao de browser com Playwright"
            ),
        ]

        config = {
            "servers": [asdict(s) for s in default_servers],
            "updated_at": datetime.now().isoformat()
        }

        self.config_path.mkdir(parents=True, exist_ok=True)
        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(config, f, indent=2, ensure_ascii=False)

        for server in default_servers:
            self.servers[server.name] = server

    def _save_config(self):
        """Salva configuracao."""
        config = {
            "servers": [asdict(s) for s in self.servers.values()],
            "updated_at": datetime.now().isoformat()
        }

        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(config, f, indent=2, ensure_ascii=False)

    def _auto_start(self):
        """Inicia servidores com auto_start=True."""
        for name, config in self.servers.items():
            if config.auto_start and config.enabled:
                self.start_server(name)

    def start_server(self, name: str) -> bool:
        """
        Inicia um servidor MCP.

        Args:
            name: Nome do servidor

        Returns:
            True se iniciou com sucesso
        """
        if name not in self.servers:
            print(f"[MCPManager] Servidor nao encontrado: {name}")
            return False

        if name in self.processes:
            if self.processes[name].poll() is None:
                print(f"[MCPManager] Servidor ja esta rodando: {name}")
                return True

        config = self.servers[name]
        if not config.enabled:
            print(f"[MCPManager] Servidor desabilitado: {name}")
            return False

        try:
            # Preparar ambiente
            env = os.environ.copy()
            env.update(config.env)

            # Preparar comando
            cmd = [config.command] + config.args

            # Iniciar processo
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd=config.cwd or str(self.base_path),
                env=env
            )

            self.processes[name] = process
            print(f"[MCPManager] Servidor iniciado: {name} (PID: {process.pid})")
            return True

        except Exception as e:
            print(f"[MCPManager] Erro ao iniciar {name}: {e}")
            return False

    def stop_server(self, name: str) -> bool:
        """
        Para um servidor MCP.

        Args:
            name: Nome do servidor

        Returns:
            True se parou com sucesso
        """
        if name not in self.processes:
            return True

        try:
            process = self.processes[name]
            process.terminate()

            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()

            del self.processes[name]
            print(f"[MCPManager] Servidor parado: {name}")
            return True

        except Exception as e:
            print(f"[MCPManager] Erro ao parar {name}: {e}")
            return False

    def stop_all(self):
        """Para todos os servidores."""
        for name in list(self.processes.keys()):
            self.stop_server(name)

    def restart_server(self, name: str) -> bool:
        """Reinicia um servidor."""
        self.stop_server(name)
        time.sleep(1)
        return self.start_server(name)

    def get_status(self, name: str) -> MCPServerStatus:
        """
        Retorna status de um servidor.

        Args:
            name: Nome do servidor

        Returns:
            Status do servidor
        """
        if name not in self.servers:
            return MCPServerStatus(name=name, running=False, error="Servidor nao encontrado")

        if name not in self.processes:
            return MCPServerStatus(name=name, running=False)

        process = self.processes[name]
        running = process.poll() is None

        return MCPServerStatus(
            name=name,
            running=running,
            pid=process.pid if running else None
        )

    def list_servers(self) -> Dict[str, str]:
        """
        Lista todos os servidores e seus status.

        Returns:
            Dict com nome -> status
        """
        result = {}
        for name in self.servers:
            status = self.get_status(name)
            result[name] = "running" if status.running else "stopped"
        return result

    def register_server(self, config: MCPServerConfig) -> bool:
        """
        Registra um novo servidor.

        Args:
            config: Configuracao do servidor

        Returns:
            True se registrou com sucesso
        """
        if config.name in self.servers:
            print(f"[MCPManager] Servidor ja existe: {config.name}")
            return False

        self.servers[config.name] = config
        self._save_config()
        print(f"[MCPManager] Servidor registrado: {config.name}")
        return True

    def unregister_server(self, name: str) -> bool:
        """Remove um servidor."""
        if name not in self.servers:
            return False

        self.stop_server(name)
        del self.servers[name]
        self._save_config()
        return True


# Instancia global
_mcp_manager: Optional[MCPManager] = None


def get_mcp_manager(base_path: Optional[str] = None) -> MCPManager:
    """Retorna instancia global do MCPManager."""
    global _mcp_manager
    if _mcp_manager is None:
        _mcp_manager = MCPManager(base_path)
    return _mcp_manager
