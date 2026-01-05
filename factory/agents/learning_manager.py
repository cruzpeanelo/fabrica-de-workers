"""
Learning Manager - Sistema de Auto-Aprendizado dos Agentes

Este modulo gerencia o aprendizado continuo dos agentes.
Apos cada tarefa completada, extrai conhecimento e atualiza
as instrucoes do agente e de agentes relacionados.

Fluxo:
1. Agente completa tarefa
2. LearningManager.learn_from_task() e chamado
3. Extrai conhecimento (arquivos, endpoints, modelos, testes)
4. Atualiza agent_knowledge.py
5. Propaga para agentes relacionados
6. Salva historico de aprendizado

Versao: 1.0
"""

import os
import re
import json
import subprocess
from datetime import datetime
from typing import Dict, List, Any, Optional, Set
from dataclasses import dataclass, field
from enum import Enum
import difflib

from factory.agents.agent_knowledge import (
    PLATFORM_KNOWLEDGE,
    AGENT_KNOWLEDGE,
    LEARNING_HISTORY,
    add_learning,
    save_knowledge_to_file,
    AgentType
)


# ==============================================================================
# CONFIGURACOES
# ==============================================================================

REPO_PATH = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# ==============================================================================
# MAPEAMENTO DE PROPAGACAO ENTRE AGENTES
# ==============================================================================

PROPAGATION_RULES: Dict[str, Dict[str, List[str]]] = {
    "BACK": {
        "endpoint_created": ["FRONT", "QA", "SEC"],
        "model_created": ["FRONT", "QA", "ARCH"],
        "service_created": ["FRONT", "QA"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "FRONT": {
        "component_created": ["QA", "BACK"],
        "page_created": ["QA", "PROD"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "SEC": {
        "auth_updated": ["BACK", "FRONT", "DEVOPS"],
        "permission_added": ["BACK", "FRONT", "ORCH"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "DEVOPS": {
        "infra_updated": ["BACK", "FRONT", "SEC"],
        "pipeline_updated": ["QA", "ORCH"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "QA": {
        "test_added": ["BACK", "FRONT", "ORCH"],
        "coverage_updated": ["ORCH", "PROD"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "ARCH": {
        "pattern_defined": ["BACK", "FRONT", "DEVOPS"],
        "structure_changed": ["BACK", "FRONT", "DEVOPS", "ORCH"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "PROD": {
        "story_created": ["ORCH", "BACK", "FRONT"],
        "backlog_updated": ["ORCH"],
        "issue_resolved": ["ORCH"]
    },
    "INOV": {
        "poc_completed": ["ARCH", "BACK", "PROD"],
        "research_done": ["PROD", "ORCH"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "FIN": {
        "pricing_updated": ["PROD", "GROWTH"],
        "billing_changed": ["BACK", "FRONT"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "GROWTH": {
        "campaign_launched": ["PROD", "FRONT"],
        "analytics_updated": ["FIN", "PROD"],
        "issue_resolved": ["PROD", "ORCH"]
    },
    "ORCH": {
        "task_distributed": ["BACK", "FRONT", "DEVOPS", "SEC", "QA"],
        "review_completed": ["BACK", "FRONT"],
        "issue_resolved": ["PROD"]
    }
}


# ==============================================================================
# CLASSES DE DADOS
# ==============================================================================

@dataclass
class LearnedKnowledge:
    """Representa conhecimento aprendido de uma tarefa."""
    agent_id: str
    task_id: str
    task_title: str
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    # Arquivos
    files_created: List[str] = field(default_factory=list)
    files_modified: List[str] = field(default_factory=list)
    files_deleted: List[str] = field(default_factory=list)

    # Codigo
    endpoints_added: List[str] = field(default_factory=list)
    models_added: List[str] = field(default_factory=list)
    services_added: List[str] = field(default_factory=list)
    tests_added: List[str] = field(default_factory=list)
    components_added: List[str] = field(default_factory=list)

    # Issues
    issue_resolved: Optional[int] = None
    issue_created: Optional[int] = None

    # Contexto
    context: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario."""
        return {
            "agent_id": self.agent_id,
            "task_id": self.task_id,
            "task_title": self.task_title,
            "timestamp": self.timestamp,
            "files_created": self.files_created,
            "files_modified": self.files_modified,
            "files_deleted": self.files_deleted,
            "endpoints_added": self.endpoints_added,
            "models_added": self.models_added,
            "services_added": self.services_added,
            "tests_added": self.tests_added,
            "components_added": self.components_added,
            "issue_resolved": self.issue_resolved,
            "issue_created": self.issue_created,
            "context": self.context
        }

    def has_content(self) -> bool:
        """Verifica se ha conteudo aprendido."""
        return bool(
            self.files_created or self.files_modified or
            self.endpoints_added or self.models_added or
            self.services_added or self.tests_added or
            self.components_added or self.issue_resolved
        )


# ==============================================================================
# LEARNING MANAGER
# ==============================================================================

class LearningManager:
    """Gerenciador de aprendizado dos agentes."""

    def __init__(self):
        self.repo_path = REPO_PATH

    async def learn_from_task(
        self,
        agent_id: str,
        task: Dict[str, Any],
        result: Dict[str, Any]
    ) -> LearnedKnowledge:
        """
        Extrai e armazena conhecimento de uma tarefa completada.

        Args:
            agent_id: ID do agente que completou a tarefa
            task: Dados da tarefa
            result: Resultado da execucao

        Returns:
            LearnedKnowledge com o conhecimento extraido
        """
        learned = LearnedKnowledge(
            agent_id=agent_id,
            task_id=task.get("id", "unknown"),
            task_title=task.get("title", task.get("description", "Untitled"))
        )

        # Extrai conhecimento do resultado
        await self._extract_file_changes(learned, result)
        await self._extract_code_artifacts(learned, result)
        await self._extract_issue_info(learned, task, result)

        # So continua se houver algo aprendido
        if not learned.has_content():
            return learned

        # Atualiza conhecimento do agente
        self._update_agent_knowledge(agent_id, learned)

        # Propaga para agentes relacionados
        self._propagate_to_related_agents(agent_id, learned)

        # Registra no historico
        self._commit_learning(agent_id, learned)

        # Salva em arquivo
        save_knowledge_to_file()

        return learned

    async def _extract_file_changes(
        self,
        learned: LearnedKnowledge,
        result: Dict[str, Any]
    ) -> None:
        """Extrai mudancas de arquivos do resultado."""
        # Arquivos criados
        if "files_created" in result:
            learned.files_created = result["files_created"]
        elif "created_files" in result:
            learned.files_created = result["created_files"]

        # Arquivos modificados
        if "files_modified" in result:
            learned.files_modified = result["files_modified"]
        elif "modified_files" in result:
            learned.files_modified = result["modified_files"]

        # Tenta extrair do output se nao especificado
        output = result.get("output", "") or result.get("message", "")
        if isinstance(output, str):
            # Procura padroes de arquivos criados/modificados
            created_pattern = r"(?:Created|Wrote|Generated)\s+['\"]?([^\s'\"]+\.py)['\"]?"
            modified_pattern = r"(?:Modified|Updated|Edited)\s+['\"]?([^\s'\"]+\.py)['\"]?"

            created = re.findall(created_pattern, output, re.IGNORECASE)
            modified = re.findall(modified_pattern, output, re.IGNORECASE)

            learned.files_created.extend(created)
            learned.files_modified.extend(modified)

    async def _extract_code_artifacts(
        self,
        learned: LearnedKnowledge,
        result: Dict[str, Any]
    ) -> None:
        """Extrai artefatos de codigo (endpoints, modelos, etc)."""
        # Analisa arquivos criados/modificados
        all_files = learned.files_created + learned.files_modified

        for filepath in all_files:
            full_path = os.path.join(self.repo_path, filepath)
            if not os.path.exists(full_path):
                continue

            try:
                with open(full_path, 'r', encoding='utf-8') as f:
                    content = f.read()
            except Exception:
                continue

            # Extrai endpoints
            if 'api' in filepath.lower() or 'route' in filepath.lower():
                endpoints = self._extract_endpoints(content)
                learned.endpoints_added.extend(endpoints)

            # Extrai modelos
            if 'model' in filepath.lower() or 'database' in filepath.lower():
                models = self._extract_models(content)
                learned.models_added.extend(models)

            # Extrai servicos
            if 'service' in filepath.lower() or 'core' in filepath.lower():
                services = self._extract_services(content)
                learned.services_added.extend(services)

            # Extrai testes
            if 'test' in filepath.lower():
                tests = self._extract_tests(content)
                learned.tests_added.extend(tests)

            # Extrai componentes
            if 'component' in filepath.lower() or 'dashboard' in filepath.lower():
                components = self._extract_components(content)
                learned.components_added.extend(components)

        # Remove duplicatas
        learned.endpoints_added = list(set(learned.endpoints_added))
        learned.models_added = list(set(learned.models_added))
        learned.services_added = list(set(learned.services_added))
        learned.tests_added = list(set(learned.tests_added))
        learned.components_added = list(set(learned.components_added))

    def _extract_endpoints(self, content: str) -> List[str]:
        """Extrai endpoints de codigo Python."""
        endpoints = []
        patterns = [
            r'@\w+\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']',
            r'@router\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']',
            r'@app\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']'
        ]

        for pattern in patterns:
            matches = re.findall(pattern, content)
            endpoints.extend(matches)

        return endpoints

    def _extract_models(self, content: str) -> List[str]:
        """Extrai modelos SQLAlchemy de codigo Python."""
        pattern = r'class\s+(\w+)\s*\([^)]*(?:Base|Model)[^)]*\)'
        return re.findall(pattern, content)

    def _extract_services(self, content: str) -> List[str]:
        """Extrai servicos de codigo Python."""
        pattern = r'class\s+(\w+(?:Service|Manager|Handler|Processor))\s*[:\(]'
        return re.findall(pattern, content)

    def _extract_tests(self, content: str) -> List[str]:
        """Extrai funcoes de teste de codigo Python."""
        pattern = r'(?:def|async def)\s+(test_\w+)\s*\('
        return re.findall(pattern, content)

    def _extract_components(self, content: str) -> List[str]:
        """Extrai componentes de codigo."""
        patterns = [
            r'class\s+(\w+Component)\s*[:\(]',
            r'class\s+(\w+Page)\s*[:\(]',
            r'class\s+(\w+View)\s*[:\(]'
        ]
        components = []
        for pattern in patterns:
            components.extend(re.findall(pattern, content))
        return components

    async def _extract_issue_info(
        self,
        learned: LearnedKnowledge,
        task: Dict[str, Any],
        result: Dict[str, Any]
    ) -> None:
        """Extrai informacoes de issue relacionado."""
        # Procura numero de issue no titulo ou descricao
        text = f"{task.get('title', '')} {task.get('description', '')} {result.get('output', '')}"

        # Padrao: #123 ou Issue #123 ou issue-123
        patterns = [
            r'#(\d+)',
            r'[Ii]ssue[:\s]*#?(\d+)',
            r'issue-(\d+)'
        ]

        for pattern in patterns:
            match = re.search(pattern, text)
            if match:
                issue_num = int(match.group(1))
                # Se a tarefa foi concluida com sucesso, e um issue resolvido
                if result.get("status") == "success":
                    learned.issue_resolved = issue_num
                break

    def _update_agent_knowledge(
        self,
        agent_id: str,
        learned: LearnedKnowledge
    ) -> None:
        """Atualiza conhecimento do agente com novo aprendizado."""
        if agent_id not in AGENT_KNOWLEDGE:
            return

        agent = AGENT_KNOWLEDGE[agent_id]

        # Adiciona arquivos gerenciados
        if learned.files_created:
            if "managed_files" not in agent:
                agent["managed_files"] = []
            for f in learned.files_created:
                if f not in agent["managed_files"]:
                    agent["managed_files"].append(f)

        # Adiciona endpoints conhecidos
        if learned.endpoints_added:
            if "known_endpoints" not in agent:
                agent["known_endpoints"] = []
            agent["known_endpoints"].extend(learned.endpoints_added)
            agent["known_endpoints"] = list(set(agent["known_endpoints"]))

        # Adiciona modelos conhecidos
        if learned.models_added:
            if "known_models" not in agent:
                agent["known_models"] = []
            agent["known_models"].extend(learned.models_added)
            agent["known_models"] = list(set(agent["known_models"]))

        # Atualiza issues resolvidos
        if learned.issue_resolved:
            if "implemented_features" not in agent:
                agent["implemented_features"] = []
            feature = f"#{learned.issue_resolved} {learned.task_title}"
            if feature not in agent["implemented_features"]:
                agent["implemented_features"].append(feature)

        # Atualiza timestamp
        agent["last_learning"] = learned.timestamp

    def _propagate_to_related_agents(
        self,
        agent_id: str,
        learned: LearnedKnowledge
    ) -> None:
        """Propaga conhecimento para agentes relacionados."""
        if agent_id not in PROPAGATION_RULES:
            return

        rules = PROPAGATION_RULES[agent_id]

        # Determina tipo de evento
        events = []
        if learned.endpoints_added:
            events.append("endpoint_created")
        if learned.models_added:
            events.append("model_created")
        if learned.services_added:
            events.append("service_created")
        if learned.tests_added:
            events.append("test_added")
        if learned.components_added:
            events.append("component_created")
        if learned.issue_resolved:
            events.append("issue_resolved")

        # Propaga para cada evento
        notified_agents: Set[str] = set()
        for event in events:
            if event in rules:
                for target_agent in rules[event]:
                    if target_agent not in notified_agents:
                        self._notify_agent(target_agent, agent_id, learned, event)
                        notified_agents.add(target_agent)

    def _notify_agent(
        self,
        target_agent: str,
        source_agent: str,
        learned: LearnedKnowledge,
        event_type: str
    ) -> None:
        """Notifica um agente sobre novo conhecimento."""
        if target_agent not in AGENT_KNOWLEDGE:
            return

        agent = AGENT_KNOWLEDGE[target_agent]

        # Inicializa lista de notificacoes se nao existir
        if "received_updates" not in agent:
            agent["received_updates"] = []

        # Adiciona notificacao
        notification = {
            "from": source_agent,
            "event": event_type,
            "timestamp": learned.timestamp,
            "summary": self._create_summary(learned, event_type)
        }
        agent["received_updates"].append(notification)

        # Limita a 50 notificacoes mais recentes
        agent["received_updates"] = agent["received_updates"][-50:]

        # Atualiza conhecimento especifico baseado no evento
        if event_type == "endpoint_created" and learned.endpoints_added:
            if "available_endpoints" not in agent:
                agent["available_endpoints"] = []
            agent["available_endpoints"].extend(learned.endpoints_added)
            agent["available_endpoints"] = list(set(agent["available_endpoints"]))

        if event_type == "model_created" and learned.models_added:
            if "available_models" not in agent:
                agent["available_models"] = []
            agent["available_models"].extend(learned.models_added)
            agent["available_models"] = list(set(agent["available_models"]))

        if event_type == "test_added" and learned.tests_added:
            if target_agent == "QA":
                if "tests_to_maintain" not in agent:
                    agent["tests_to_maintain"] = []
                agent["tests_to_maintain"].extend(learned.tests_added)

    def _create_summary(self, learned: LearnedKnowledge, event_type: str) -> str:
        """Cria resumo do aprendizado para notificacao."""
        parts = []

        if learned.endpoints_added:
            parts.append(f"{len(learned.endpoints_added)} endpoints")
        if learned.models_added:
            parts.append(f"{len(learned.models_added)} modelos")
        if learned.services_added:
            parts.append(f"{len(learned.services_added)} servicos")
        if learned.tests_added:
            parts.append(f"{len(learned.tests_added)} testes")
        if learned.issue_resolved:
            parts.append(f"issue #{learned.issue_resolved}")

        if parts:
            return f"Adicionados: {', '.join(parts)}"
        return learned.task_title

    def _commit_learning(
        self,
        agent_id: str,
        learned: LearnedKnowledge
    ) -> None:
        """Registra aprendizado no historico."""
        add_learning(agent_id, learned.to_dict())

    def get_learning_history(
        self,
        agent_id: str,
        limit: int = 10
    ) -> List[Dict[str, Any]]:
        """Retorna historico de aprendizado de um agente."""
        if agent_id not in LEARNING_HISTORY:
            return []
        return LEARNING_HISTORY[agent_id][-limit:]

    def get_all_learning_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas de aprendizado de todos os agentes."""
        stats = {}
        for agent_id, history in LEARNING_HISTORY.items():
            stats[agent_id] = {
                "total_learnings": len(history),
                "endpoints_learned": sum(
                    len(l.get("endpoints_added", [])) for l in history
                ),
                "models_learned": sum(
                    len(l.get("models_added", [])) for l in history
                ),
                "issues_resolved": sum(
                    1 for l in history if l.get("issue_resolved")
                ),
                "last_learning": history[-1].get("timestamp") if history else None
            }
        return stats


# ==============================================================================
# FUNCOES DE CONVENIENCIA
# ==============================================================================

_learning_manager: Optional[LearningManager] = None


def get_learning_manager() -> LearningManager:
    """Retorna instancia singleton do LearningManager."""
    global _learning_manager
    if _learning_manager is None:
        _learning_manager = LearningManager()
    return _learning_manager


async def learn_from_task(
    agent_id: str,
    task: Dict[str, Any],
    result: Dict[str, Any]
) -> LearnedKnowledge:
    """Wrapper para aprendizado facil."""
    manager = get_learning_manager()
    return await manager.learn_from_task(agent_id, task, result)


def get_agent_updates(agent_id: str, limit: int = 5) -> List[Dict[str, Any]]:
    """Retorna atualizacoes recebidas por um agente."""
    if agent_id not in AGENT_KNOWLEDGE:
        return []
    return AGENT_KNOWLEDGE[agent_id].get("received_updates", [])[-limit:]


# ==============================================================================
# CLI
# ==============================================================================

def main():
    """CLI para testar o LearningManager."""
    import argparse

    parser = argparse.ArgumentParser(description="Learning Manager CLI")
    parser.add_argument("--stats", action="store_true", help="Mostra estatisticas")
    parser.add_argument("--history", type=str, help="Mostra historico do agente")

    args = parser.parse_args()

    manager = LearningManager()

    if args.stats:
        stats = manager.get_all_learning_stats()
        print("\n=== ESTATISTICAS DE APRENDIZADO ===\n")
        for agent_id, data in stats.items():
            print(f"{agent_id}:")
            print(f"  Total de aprendizados: {data['total_learnings']}")
            print(f"  Endpoints aprendidos: {data['endpoints_learned']}")
            print(f"  Modelos aprendidos: {data['models_learned']}")
            print(f"  Issues resolvidos: {data['issues_resolved']}")
            print(f"  Ultimo aprendizado: {data['last_learning']}")
            print()

    if args.history:
        history = manager.get_learning_history(args.history)
        print(f"\n=== HISTORICO DE {args.history} ===\n")
        for item in history:
            print(f"- {item.get('timestamp', 'N/A')}: {item.get('task_title', 'N/A')}")
            if item.get("endpoints_added"):
                print(f"  Endpoints: {item['endpoints_added']}")
            if item.get("models_added"):
                print(f"  Modelos: {item['models_added']}")
            if item.get("issue_resolved"):
                print(f"  Issue: #{item['issue_resolved']}")
            print()


if __name__ == "__main__":
    main()
