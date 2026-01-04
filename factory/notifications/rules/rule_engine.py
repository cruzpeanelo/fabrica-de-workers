# -*- coding: utf-8 -*-
"""
Motor de Regras para Notificacoes
Plataforma E v6.0

Avalia condicoes e determina quando/como enviar notificacoes.
Suporta:
- Condicoes compostas (AND, OR, NOT)
- Operadores de comparacao
- Horarios de nao perturbe
- Agrupamento de notificacoes
"""

import logging
from typing import Dict, List, Any, Optional, Callable
from datetime import datetime, time
from dataclasses import dataclass

logger = logging.getLogger(__name__)


@dataclass
class RuleMatch:
    """Resultado da avaliacao de uma regra"""
    matched: bool
    rule_id: str
    rule_name: str
    channels: List[str]
    recipients: List[str]
    template: Optional[str]
    priority: str
    config: Dict[str, Any]


class RuleEngine:
    """
    Motor de Regras para Notificacoes.

    Avalia eventos contra regras definidas e retorna
    os canais, destinatarios e templates a usar.
    """

    # Operadores suportados
    OPERATORS = {
        "eq": lambda a, b: a == b,
        "ne": lambda a, b: a != b,
        "gt": lambda a, b: a > b,
        "gte": lambda a, b: a >= b,
        "lt": lambda a, b: a < b,
        "lte": lambda a, b: a <= b,
        "in": lambda a, b: a in b,
        "not_in": lambda a, b: a not in b,
        "contains": lambda a, b: b in str(a),
        "not_contains": lambda a, b: b not in str(a),
        "starts_with": lambda a, b: str(a).startswith(b),
        "ends_with": lambda a, b: str(a).endswith(b),
        "regex": lambda a, b: bool(__import__("re").match(b, str(a))),
        "exists": lambda a, b: a is not None,
        "not_exists": lambda a, b: a is None,
        "is_empty": lambda a, b: not a,
        "is_not_empty": lambda a, b: bool(a),
    }

    def __init__(self, rules: List[Dict[str, Any]] = None):
        """
        Inicializa o motor com regras.

        Args:
            rules: Lista de regras no formato do modelo NotificationRule
        """
        self.rules = rules or []
        self.quiet_hours_config = None
        self.custom_operators: Dict[str, Callable] = {}

    def set_rules(self, rules: List[Dict[str, Any]]) -> None:
        """Define as regras a usar"""
        self.rules = rules

    def add_rule(self, rule: Dict[str, Any]) -> None:
        """Adiciona uma regra"""
        self.rules.append(rule)

    def remove_rule(self, rule_id: str) -> bool:
        """Remove uma regra por ID"""
        initial_count = len(self.rules)
        self.rules = [r for r in self.rules if r.get("rule_id") != rule_id]
        return len(self.rules) < initial_count

    def set_quiet_hours(
        self,
        start: str,
        end: str,
        timezone: str = "America/Sao_Paulo",
        exceptions: List[str] = None
    ) -> None:
        """
        Define horario de nao perturbe.

        Args:
            start: Hora de inicio (HH:MM)
            end: Hora de fim (HH:MM)
            timezone: Fuso horario
            exceptions: Lista de event_types que ignoram quiet hours
        """
        self.quiet_hours_config = {
            "start": start,
            "end": end,
            "timezone": timezone,
            "exceptions": exceptions or ["error", "urgent"]
        }

    def register_operator(self, name: str, func: Callable[[Any, Any], bool]) -> None:
        """
        Registra um operador customizado.

        Args:
            name: Nome do operador
            func: Funcao que recebe (valor_campo, valor_condicao) -> bool
        """
        self.custom_operators[name] = func

    def evaluate(
        self,
        event_type: str,
        event_data: Dict[str, Any],
        context: Dict[str, Any] = None
    ) -> List[RuleMatch]:
        """
        Avalia todas as regras para um evento.

        Args:
            event_type: Tipo do evento
            event_data: Dados do evento
            context: Contexto adicional (user_id, project_id, etc)

        Returns:
            Lista de RuleMatch para regras que combinaram
        """
        matches = []
        context = context or {}

        # Verificar quiet hours
        if self._is_quiet_hours(event_type):
            logger.debug(f"Evento {event_type} bloqueado por quiet hours")
            return matches

        # Combinar event_data com context para avaliacao
        full_data = {**event_data, **context, "event_type": event_type}

        for rule in self.rules:
            # Verificar se regra esta ativa
            if not rule.get("is_active", True):
                continue

            # Verificar se o event_type combina
            if rule.get("event_type") != event_type and rule.get("event_type") != "*":
                continue

            # Verificar escopo de projeto
            rule_project = rule.get("project_id")
            if rule_project and rule_project != context.get("project_id"):
                continue

            # Avaliar condicoes
            conditions = rule.get("conditions", {})
            if self._evaluate_conditions(conditions, full_data):
                matches.append(RuleMatch(
                    matched=True,
                    rule_id=rule.get("rule_id", "unknown"),
                    rule_name=rule.get("name", "Regra sem nome"),
                    channels=rule.get("channels", []),
                    recipients=rule.get("recipients", []),
                    template=rule.get("template"),
                    priority=rule.get("priority", "normal"),
                    config=rule.get("config", {})
                ))

        return matches

    def _evaluate_conditions(
        self,
        conditions: Dict[str, Any],
        data: Dict[str, Any]
    ) -> bool:
        """
        Avalia um bloco de condicoes.

        Formato suportado:
        {
            "operator": "AND",  # AND, OR, NOT
            "conditions": [
                {"field": "priority", "op": "eq", "value": "high"},
                {"field": "project_id", "op": "in", "value": ["PRJ-001"]}
            ]
        }

        Ou formato simples (sem aninhamento):
        {"priority": "high", "status": "completed"}
        """
        # Se nao tem condicoes, sempre combina
        if not conditions:
            return True

        # Formato composto (com operator)
        if "operator" in conditions:
            operator = conditions.get("operator", "AND").upper()
            sub_conditions = conditions.get("conditions", [])

            if operator == "AND":
                return all(
                    self._evaluate_single_condition(cond, data)
                    for cond in sub_conditions
                )
            elif operator == "OR":
                return any(
                    self._evaluate_single_condition(cond, data)
                    for cond in sub_conditions
                )
            elif operator == "NOT":
                if sub_conditions:
                    return not self._evaluate_single_condition(sub_conditions[0], data)
                return True

        # Formato simples (dict de campo: valor)
        # Assume igualdade para todos os campos
        for field, expected_value in conditions.items():
            if field in ["operator", "conditions"]:
                continue

            actual_value = self._get_nested_value(data, field)
            if actual_value != expected_value:
                return False

        return True

    def _evaluate_single_condition(
        self,
        condition: Dict[str, Any],
        data: Dict[str, Any]
    ) -> bool:
        """
        Avalia uma unica condicao.

        Args:
            condition: {"field": str, "op": str, "value": Any}
            data: Dados do evento

        Returns:
            True se a condicao e satisfeita
        """
        # Suporte para condicoes aninhadas
        if "operator" in condition:
            return self._evaluate_conditions(condition, data)

        field = condition.get("field")
        op = condition.get("op", "eq")
        expected = condition.get("value")

        # Obter valor do campo (suporta notacao de ponto: "project.name")
        actual = self._get_nested_value(data, field)

        # Buscar operador
        operator_func = self.custom_operators.get(op) or self.OPERATORS.get(op)

        if not operator_func:
            logger.warning(f"Operador desconhecido: {op}")
            return False

        try:
            return operator_func(actual, expected)
        except Exception as e:
            logger.warning(f"Erro ao avaliar condicao {field} {op} {expected}: {e}")
            return False

    def _get_nested_value(self, data: Dict[str, Any], field: str) -> Any:
        """
        Obtem valor de campo aninhado (ex: "project.name").

        Args:
            data: Dicionario de dados
            field: Nome do campo (pode conter ".")

        Returns:
            Valor do campo ou None
        """
        if not field:
            return None

        parts = field.split(".")
        value = data

        for part in parts:
            if isinstance(value, dict):
                value = value.get(part)
            elif hasattr(value, part):
                value = getattr(value, part)
            else:
                return None

            if value is None:
                return None

        return value

    def _is_quiet_hours(self, event_type: str) -> bool:
        """
        Verifica se estamos em horario de nao perturbe.

        Args:
            event_type: Tipo do evento

        Returns:
            True se devemos bloquear notificacoes
        """
        if not self.quiet_hours_config:
            return False

        # Verificar excecoes
        exceptions = self.quiet_hours_config.get("exceptions", [])
        if event_type in exceptions:
            return False

        # Verificar se prioridade "urgent" esta nas excecoes
        # (isso seria checado com o event_data, mas aqui simplificamos)

        try:
            now = datetime.now()
            start_str = self.quiet_hours_config["start"]
            end_str = self.quiet_hours_config["end"]

            start_time = time(
                int(start_str.split(":")[0]),
                int(start_str.split(":")[1])
            )
            end_time = time(
                int(end_str.split(":")[0]),
                int(end_str.split(":")[1])
            )

            current_time = now.time()

            # Quiet hours que cruzam meia-noite (ex: 22:00 - 07:00)
            if start_time > end_time:
                return current_time >= start_time or current_time <= end_time
            else:
                return start_time <= current_time <= end_time

        except Exception as e:
            logger.warning(f"Erro ao verificar quiet hours: {e}")
            return False

    def validate_rule(self, rule: Dict[str, Any]) -> Dict[str, Any]:
        """
        Valida uma regra.

        Args:
            rule: Regra a validar

        Returns:
            {"valid": bool, "errors": [], "warnings": []}
        """
        result = {
            "valid": True,
            "errors": [],
            "warnings": []
        }

        # Campos obrigatorios
        required = ["rule_id", "name", "event_type"]
        for field in required:
            if not rule.get(field):
                result["valid"] = False
                result["errors"].append(f"Campo obrigatorio ausente: {field}")

        # Verificar canais
        if not rule.get("channels"):
            result["warnings"].append("Nenhum canal definido na regra")

        # Verificar destinatarios
        if not rule.get("recipients"):
            result["warnings"].append("Nenhum destinatario definido na regra")

        # Validar condicoes
        conditions = rule.get("conditions", {})
        self._validate_conditions(conditions, result)

        return result

    def _validate_conditions(
        self,
        conditions: Dict[str, Any],
        result: Dict[str, Any]
    ) -> None:
        """Valida recursivamente as condicoes"""
        if not conditions:
            return

        if "operator" in conditions:
            operator = conditions.get("operator", "").upper()
            if operator not in ["AND", "OR", "NOT"]:
                result["warnings"].append(f"Operador desconhecido: {operator}")

            for cond in conditions.get("conditions", []):
                if isinstance(cond, dict):
                    if "operator" in cond:
                        self._validate_conditions(cond, result)
                    else:
                        op = cond.get("op", "eq")
                        if op not in self.OPERATORS and op not in self.custom_operators:
                            result["warnings"].append(
                                f"Operador de comparacao desconhecido: {op}"
                            )

    def get_effective_rules(
        self,
        event_type: str,
        project_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Retorna regras que se aplicam a um evento/projeto.

        Args:
            event_type: Tipo do evento
            project_id: ID do projeto (opcional)

        Returns:
            Lista de regras aplicaveis (ordenadas por prioridade)
        """
        applicable = []

        for rule in self.rules:
            if not rule.get("is_active", True):
                continue

            rule_event = rule.get("event_type")
            if rule_event != event_type and rule_event != "*":
                continue

            rule_project = rule.get("project_id")
            if rule_project and rule_project != project_id:
                continue

            applicable.append(rule)

        # Ordenar por prioridade (urgent > high > normal > low)
        priority_order = {"urgent": 0, "high": 1, "normal": 2, "low": 3}
        applicable.sort(
            key=lambda r: priority_order.get(r.get("priority", "normal"), 2)
        )

        return applicable

    def __repr__(self) -> str:
        return f"<RuleEngine rules={len(self.rules)}>"
