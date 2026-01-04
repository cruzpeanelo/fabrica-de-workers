"""
Skill Manager - Plataforma E
Gerencia habilidades disponiveis para os agentes
"""
from typing import Dict, List, Optional, Any
from pathlib import Path
import subprocess
import json

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import SessionLocal
from factory.database.models import Skill, SkillType


class SkillManager:
    """Gerenciador de Skills da Fabrica"""

    def __init__(self):
        self.active_mcp_servers: Dict[str, subprocess.Popen] = {}

    def get_all_skills(self, enabled_only: bool = True) -> List[Dict]:
        """Lista todas as skills disponiveis"""
        db = SessionLocal()
        try:
            query = db.query(Skill)
            if enabled_only:
                query = query.filter(Skill.enabled == True)
            skills = query.all()
            return [s.to_dict() for s in skills]
        finally:
            db.close()

    def get_skill(self, skill_id: str) -> Optional[Dict]:
        """Busca uma skill especifica"""
        db = SessionLocal()
        try:
            skill = db.query(Skill).filter(Skill.skill_id == skill_id).first()
            return skill.to_dict() if skill else None
        finally:
            db.close()

    def get_skills_by_type(self, skill_type: str) -> List[Dict]:
        """Lista skills por tipo (core, mcp, vessel, custom)"""
        db = SessionLocal()
        try:
            skills = db.query(Skill).filter(Skill.skill_type == skill_type).all()
            return [s.to_dict() for s in skills]
        finally:
            db.close()

    def get_skills_by_category(self, category: str) -> List[Dict]:
        """Lista skills por categoria (file, web, data, etc)"""
        db = SessionLocal()
        try:
            skills = db.query(Skill).filter(Skill.category == category).all()
            return [s.to_dict() for s in skills]
        finally:
            db.close()

    def get_agent_skills(self, agent_id: str) -> List[Dict]:
        """Lista skills disponiveis para um agente"""
        from factory.database.models import Agent

        db = SessionLocal()
        try:
            agent = db.query(Agent).filter(Agent.agent_id == agent_id).first()
            if not agent or not agent.skills:
                return self.get_all_skills()  # Retorna todas se nao especificado

            # Busca skills especificas do agente
            skills = []
            for skill_ref in agent.skills:
                skill = db.query(Skill).filter(Skill.skill_id == skill_ref).first()
                if skill and skill.enabled:
                    skills.append(skill.to_dict())
            return skills
        finally:
            db.close()

    # =========================================================================
    # MCP SERVER MANAGEMENT
    # =========================================================================

    def start_mcp_server(self, skill_id: str) -> bool:
        """Inicia um servidor MCP"""
        skill = self.get_skill(skill_id)
        if not skill or skill["skill_type"] != "mcp":
            return False

        if skill_id in self.active_mcp_servers:
            return True  # Ja esta rodando

        try:
            command = skill.get("server_command")
            args = skill.get("server_args", [])

            if not command:
                return False

            process = subprocess.Popen(
                [command] + args,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                shell=True
            )
            self.active_mcp_servers[skill_id] = process
            return True
        except Exception as e:
            print(f"[SkillManager] Erro ao iniciar MCP {skill_id}: {e}")
            return False

    def stop_mcp_server(self, skill_id: str) -> bool:
        """Para um servidor MCP"""
        if skill_id not in self.active_mcp_servers:
            return True

        try:
            process = self.active_mcp_servers[skill_id]
            process.terminate()
            process.wait(timeout=5)
            del self.active_mcp_servers[skill_id]
            return True
        except Exception as e:
            print(f"[SkillManager] Erro ao parar MCP {skill_id}: {e}")
            return False

    def stop_all_mcp_servers(self):
        """Para todos os servidores MCP"""
        for skill_id in list(self.active_mcp_servers.keys()):
            self.stop_mcp_server(skill_id)

    def get_mcp_status(self) -> Dict[str, bool]:
        """Retorna status dos servidores MCP"""
        status = {}
        for skill_id, process in self.active_mcp_servers.items():
            status[skill_id] = process.poll() is None  # True se ainda rodando
        return status

    # =========================================================================
    # SKILL EXECUTION
    # =========================================================================

    def execute_skill(self, skill_id: str, params: Dict[str, Any] = None) -> Dict:
        """Executa uma skill (para skills customizadas)"""
        skill = self.get_skill(skill_id)
        if not skill:
            return {"success": False, "error": f"Skill {skill_id} nao encontrada"}

        if not skill.get("enabled", True):
            return {"success": False, "error": f"Skill {skill_id} esta desabilitada"}

        # Core skills sao executadas diretamente pelo Claude Code
        if skill["skill_type"] == "core":
            return {"success": True, "message": "Core skill - executar via Claude Code"}

        # MCP skills precisam do servidor rodando
        if skill["skill_type"] == "mcp":
            if skill_id not in self.active_mcp_servers:
                self.start_mcp_server(skill_id)
            return {"success": True, "message": f"MCP server {skill_id} ativo"}

        # Vessel skills (futuro)
        if skill["skill_type"] == "vessel":
            return {"success": False, "error": "Vessel skills ainda nao implementadas"}

        return {"success": False, "error": "Tipo de skill desconhecido"}

    # =========================================================================
    # SKILL REGISTRATION
    # =========================================================================

    def register_skill(self, skill_data: Dict) -> Optional[Dict]:
        """Registra uma nova skill"""
        db = SessionLocal()
        try:
            # Verifica se ja existe
            existing = db.query(Skill).filter(
                Skill.skill_id == skill_data.get("skill_id")
            ).first()
            if existing:
                return None

            skill = Skill(**skill_data)
            db.add(skill)
            db.commit()
            db.refresh(skill)
            return skill.to_dict()
        finally:
            db.close()

    def update_skill(self, skill_id: str, data: Dict) -> Optional[Dict]:
        """Atualiza uma skill existente"""
        db = SessionLocal()
        try:
            skill = db.query(Skill).filter(Skill.skill_id == skill_id).first()
            if not skill:
                return None

            for key, value in data.items():
                if hasattr(skill, key):
                    setattr(skill, key, value)

            db.commit()
            db.refresh(skill)
            return skill.to_dict()
        finally:
            db.close()

    def enable_skill(self, skill_id: str) -> bool:
        """Habilita uma skill"""
        return self.update_skill(skill_id, {"enabled": True}) is not None

    def disable_skill(self, skill_id: str) -> bool:
        """Desabilita uma skill"""
        return self.update_skill(skill_id, {"enabled": False}) is not None


# Instancia global
skill_manager = SkillManager()


def get_skill_manager() -> SkillManager:
    """Retorna instancia do gerenciador de skills"""
    return skill_manager
