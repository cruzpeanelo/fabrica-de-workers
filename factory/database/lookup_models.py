# -*- coding: utf-8 -*-
"""
Modelos de Lookup para a Plataforma E
======================================

Tabelas de referencia para eliminar dados hardcoded no codigo.
Todos os valores de status, prioridade, complexidade, etc.
devem vir destas tabelas em vez de estar hardcoded.

Multi-Tenancy:
- Valores globais tem tenant_id = NULL
- Valores customizados por tenant tem tenant_id preenchido

Author: Plataforma E
"""

from sqlalchemy import (
    Column, Integer, String, Text, DateTime, Boolean,
    Index, UniqueConstraint, CheckConstraint
)
from sqlalchemy.orm import relationship
from datetime import datetime

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# STATUS LOOKUP - Status de Story, Task, Project, Epic, Sprint
# =============================================================================

class StatusLookup(Base):
    """
    Tabela de lookup para status de diferentes entidades.

    Exemplos:
    - entity_type='story', status_code='backlog', label='Backlog'
    - entity_type='task', status_code='pending', label='Pendente'
    - entity_type='project', status_code='planning', label='Planejamento'
    """
    __tablename__ = "status_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    entity_type = Column(String(50), nullable=False, index=True)  # story, task, project, epic, sprint
    status_code = Column(String(50), nullable=False)  # backlog, ready, in_progress, done

    # Exibicao
    label = Column(String(100), nullable=False)  # Nome para exibicao
    label_en = Column(String(100), nullable=True)  # Label em ingles (opcional)
    color = Column(String(20), nullable=True)  # Cor hex (#10B981)
    icon = Column(String(50), nullable=True)  # Icone (opcional)
    description = Column(Text, nullable=True)  # Descricao

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)  # Ordem de exibicao
    is_initial = Column(Boolean, default=False)  # Status inicial padrao
    is_final = Column(Boolean, default=False)  # Status final (concluido)
    is_active = Column(Boolean, default=True)  # Se esta ativo para uso

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)  # NULL = global

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('entity_type', 'status_code', 'tenant_id', name='uq_status_entity_code_tenant'),
        Index('ix_status_entity_tenant', 'entity_type', 'tenant_id'),
    )

    def __repr__(self):
        return f"<StatusLookup({self.entity_type}.{self.status_code}={self.label})>"


# =============================================================================
# PRIORITY LOOKUP - Prioridades
# =============================================================================

class PriorityLookup(Base):
    """
    Tabela de lookup para prioridades.

    Exemplos:
    - priority_code='low', label='Baixa', numeric_value=3
    - priority_code='urgent', label='Urgente', numeric_value=9
    """
    __tablename__ = "priority_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    priority_code = Column(String(20), nullable=False)  # low, medium, high, urgent

    # Exibicao
    label = Column(String(50), nullable=False)  # Baixa, Media, Alta, Urgente
    label_en = Column(String(50), nullable=True)  # Low, Medium, High, Urgent
    color = Column(String(20), nullable=True)  # Cor hex
    icon = Column(String(50), nullable=True)  # Icone

    # Valor numerico para ordenacao e calculos
    numeric_value = Column(Integer, nullable=False)  # 1-9 scale

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)
    is_default = Column(Boolean, default=False)  # Prioridade padrao
    is_active = Column(Boolean, default=True)

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)  # NULL = global

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('priority_code', 'tenant_id', name='uq_priority_code_tenant'),
        CheckConstraint('numeric_value >= 1 AND numeric_value <= 9', name='ck_priority_value_range'),
    )

    def __repr__(self):
        return f"<PriorityLookup({self.priority_code}={self.label}, value={self.numeric_value})>"


# =============================================================================
# COMPLEXITY LOOKUP - Complexidade
# =============================================================================

class ComplexityLookup(Base):
    """
    Tabela de lookup para complexidade.

    Mapeia story points para niveis de complexidade.

    Exemplos:
    - complexity_code='low', min_points=1, max_points=3
    - complexity_code='very_high', min_points=13, max_points=21
    """
    __tablename__ = "complexity_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    complexity_code = Column(String(20), nullable=False)  # low, medium, high, very_high

    # Exibicao
    label = Column(String(50), nullable=False)  # Baixa, Media, Alta, Muito Alta
    label_en = Column(String(50), nullable=True)  # Low, Medium, High, Very High
    color = Column(String(20), nullable=True)  # Cor hex
    icon = Column(String(50), nullable=True)  # Icone

    # Regras de mapeamento de story points
    min_points = Column(Integer, nullable=True)  # Minimo de pontos para esta complexidade
    max_points = Column(Integer, nullable=True)  # Maximo de pontos para esta complexidade

    # Estimativa de esforco
    estimated_hours_min = Column(Integer, nullable=True)  # Horas minimas estimadas
    estimated_hours_max = Column(Integer, nullable=True)  # Horas maximas estimadas

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)
    is_default = Column(Boolean, default=False)
    is_active = Column(Boolean, default=True)

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)  # NULL = global

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('complexity_code', 'tenant_id', name='uq_complexity_code_tenant'),
    )

    def __repr__(self):
        return f"<ComplexityLookup({self.complexity_code}={self.label}, points={self.min_points}-{self.max_points})>"


# =============================================================================
# STORY POINTS LOOKUP - Valores validos de Story Points (Fibonacci)
# =============================================================================

class StoryPointsLookup(Base):
    """
    Tabela de lookup para valores validos de story points.

    Por padrao usa sequencia Fibonacci: 0, 1, 2, 3, 5, 8, 13, 21
    Mas pode ser customizado por tenant.
    """
    __tablename__ = "story_points_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Valor
    points_value = Column(Integer, nullable=False)  # 0, 1, 2, 3, 5, 8, 13, 21

    # Exibicao
    label = Column(String(20), nullable=True)  # Label opcional (ex: "XS", "S", "M", "L", "XL")
    description = Column(Text, nullable=True)  # Descricao do que significa este valor

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)
    is_active = Column(Boolean, default=True)

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)  # NULL = global

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('points_value', 'tenant_id', name='uq_points_value_tenant'),
    )

    def __repr__(self):
        return f"<StoryPointsLookup(points={self.points_value})>"


# =============================================================================
# TASK TYPE LOOKUP - Tipos de Task
# =============================================================================

class TaskTypeLookup(Base):
    """
    Tabela de lookup para tipos de task.

    Exemplos:
    - type_code='development', label='Desenvolvimento'
    - type_code='review', label='Revisao de Codigo'
    - type_code='test', label='Testes'
    """
    __tablename__ = "task_type_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    type_code = Column(String(50), nullable=False)  # development, review, test, documentation, design

    # Exibicao
    label = Column(String(100), nullable=False)
    label_en = Column(String(100), nullable=True)
    color = Column(String(20), nullable=True)
    icon = Column(String(50), nullable=True)

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)
    is_default = Column(Boolean, default=False)
    is_active = Column(Boolean, default=True)

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('type_code', 'tenant_id', name='uq_task_type_code_tenant'),
    )

    def __repr__(self):
        return f"<TaskTypeLookup({self.type_code}={self.label})>"


# =============================================================================
# ROLE LOOKUP - Papeis de Usuario
# =============================================================================

class RoleLookup(Base):
    """
    Tabela de lookup para papeis de usuario.

    Exemplos:
    - role_code='admin', label='Administrador'
    - role_code='developer', label='Desenvolvedor'
    """
    __tablename__ = "role_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    role_code = Column(String(50), nullable=False)  # admin, developer, viewer, manager

    # Exibicao
    label = Column(String(100), nullable=False)
    label_en = Column(String(100), nullable=True)
    description = Column(Text, nullable=True)

    # Permissoes (JSON array de permission codes)
    permissions = Column(Text, nullable=True)  # JSON: ["read", "write", "admin"]

    # Ordenacao e comportamento
    sort_order = Column(Integer, default=0)
    is_system = Column(Boolean, default=False)  # Papel do sistema (nao pode ser deletado)
    is_active = Column(Boolean, default=True)

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('role_code', 'tenant_id', name='uq_role_code_tenant'),
    )

    def __repr__(self):
        return f"<RoleLookup({self.role_code}={self.label})>"


# =============================================================================
# SYSTEM CONFIG - Configuracoes do Sistema
# =============================================================================

class SystemConfig(Base):
    """
    Tabela de configuracoes do sistema.

    Armazena configuracoes globais e por tenant.

    Exemplos:
    - config_key='wip_limit_in_progress', config_value='3'
    - config_key='default_story_points', config_value='5'
    """
    __tablename__ = "system_config"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao
    config_key = Column(String(100), nullable=False)  # Nome da configuracao

    # Valor
    config_value = Column(Text, nullable=True)  # Valor como string
    data_type = Column(String(20), default='string')  # string, number, boolean, json

    # Descricao
    description = Column(Text, nullable=True)  # O que esta configuracao faz
    default_value = Column(Text, nullable=True)  # Valor padrao

    # Validacao (opcional)
    min_value = Column(String(50), nullable=True)  # Valor minimo (para numeros)
    max_value = Column(String(50), nullable=True)  # Valor maximo (para numeros)
    allowed_values = Column(Text, nullable=True)  # JSON array de valores permitidos

    # Categoria para organizacao no UI
    category = Column(String(50), nullable=True)  # general, kanban, story, notification

    # Comportamento
    is_public = Column(Boolean, default=False)  # Se pode ser lido sem autenticacao
    is_readonly = Column(Boolean, default=False)  # Se nao pode ser alterado

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)  # NULL = global

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    updated_by = Column(String(100), nullable=True)  # Quem alterou por ultimo

    __table_args__ = (
        UniqueConstraint('config_key', 'tenant_id', name='uq_config_key_tenant'),
        Index('ix_config_category_tenant', 'category', 'tenant_id'),
    )

    def __repr__(self):
        return f"<SystemConfig({self.config_key}={self.config_value})>"

    def get_typed_value(self):
        """Retorna o valor convertido para o tipo correto."""
        if self.config_value is None:
            return None

        if self.data_type == 'number':
            try:
                return int(self.config_value)
            except ValueError:
                return float(self.config_value)
        elif self.data_type == 'boolean':
            return self.config_value.lower() in ('true', '1', 'yes', 'on')
        elif self.data_type == 'json':
            import json
            return json.loads(self.config_value)
        else:
            return self.config_value


# =============================================================================
# AGENT SKILL LOOKUP - Habilidades dos Agentes
# =============================================================================

class AgentSkillLookup(Base):
    """
    Tabela de lookup para habilidades dos agentes.

    Mapeia keywords para agentes responsaveis.

    Exemplos:
    - agent_code='SEC', keyword='security'
    - agent_code='BACK', keyword='api'
    """
    __tablename__ = "agent_skill_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Agente
    agent_code = Column(String(20), nullable=False, index=True)  # SEC, BACK, FRONT, QA

    # Keyword que dispara este agente
    keyword = Column(String(100), nullable=False)  # security, api, frontend

    # Prioridade (maior = mais importante)
    priority = Column(Integer, default=0)  # Se multiplas keywords match, usa maior prioridade

    # Categoria da skill
    category = Column(String(50), nullable=True)  # security, infrastructure, ui, testing

    # Comportamento
    is_active = Column(Boolean, default=True)

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('agent_code', 'keyword', name='uq_agent_keyword'),
        Index('ix_agent_skill_keyword', 'keyword'),
    )

    def __repr__(self):
        return f"<AgentSkillLookup({self.agent_code}: {self.keyword})>"


# =============================================================================
# WIP LIMIT LOOKUP - Limites WIP por Coluna do Kanban
# =============================================================================

class WipLimitLookup(Base):
    """
    Tabela de lookup para limites WIP por coluna do Kanban.

    Exemplos:
    - column_code='in_progress', limit_value=3
    - column_code='review', limit_value=2
    """
    __tablename__ = "wip_limit_lookup"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificacao da coluna
    entity_type = Column(String(50), nullable=False)  # story, task
    column_code = Column(String(50), nullable=False)  # in_progress, review, testing

    # Limite
    limit_value = Column(Integer, nullable=False)  # Maximo de items na coluna
    warn_at = Column(Integer, nullable=True)  # Avisar quando atingir este valor

    # Multi-tenancy
    tenant_id = Column(String(50), nullable=True, index=True)

    # Auditoria
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    __table_args__ = (
        UniqueConstraint('entity_type', 'column_code', 'tenant_id', name='uq_wip_entity_column_tenant'),
    )

    def __repr__(self):
        return f"<WipLimitLookup({self.entity_type}.{self.column_code}={self.limit_value})>"
