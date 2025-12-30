# -*- coding: utf-8 -*-
"""
OKR Manager - Issue #281
========================

Manager para operacoes CRUD de OKRs (Objectives and Key Results).

Funcionalidades:
- CRUD de Objectives
- CRUD de Key Results
- Calculo de progresso
- Vinculacao com Stories e Projects
- Hierarquia de objetivos
"""

import uuid
from datetime import datetime
from typing import Dict, List, Optional, Any
from sqlalchemy.orm import Session

from factory.database.models import (
    Objective, KeyResult, OKRStoryLink, OKRProjectLink,
    Story, Project, OKRStatus, OKRPeriod
)


class OKRManager:
    """
    Gerenciador de OKRs (Objectives and Key Results).

    Fornece operacoes CRUD completas para objetivos e key results,
    alem de funcionalidades de vinculacao com stories e projetos.
    """

    def __init__(self, db: Session, tenant_id: str = None):
        """
        Inicializa o OKR Manager.

        Args:
            db: Sessao SQLAlchemy
            tenant_id: ID do tenant para isolamento multi-tenant
        """
        self.db = db
        self.tenant_id = tenant_id

    # =========================================================================
    # OBJECTIVES - CRUD
    # =========================================================================

    def create_objective(
        self,
        title: str,
        description: str = None,
        period: str = OKRPeriod.QUARTERLY.value,
        start_date: datetime = None,
        end_date: datetime = None,
        owner_id: str = None,
        owner_name: str = None,
        parent_objective_id: str = None,
        category: str = None,
        tags: List[str] = None,
        created_by: str = "system"
    ) -> Dict[str, Any]:
        """
        Cria um novo Objective.

        Args:
            title: Titulo do objetivo
            description: Descricao detalhada
            period: Periodo (quarterly, yearly, monthly, custom)
            start_date: Data de inicio
            end_date: Data de fim
            owner_id: ID do responsavel
            owner_name: Nome do responsavel
            parent_objective_id: ID do objetivo pai (para hierarquia)
            category: Categoria do objetivo
            tags: Tags para organizacao
            created_by: Usuario que criou

        Returns:
            Dicionario com dados do objetivo criado
        """
        objective_id = f"OBJ-{uuid.uuid4().hex[:8].upper()}"

        objective = Objective(
            objective_id=objective_id,
            tenant_id=self.tenant_id,
            title=title,
            description=description,
            period=period,
            start_date=start_date,
            end_date=end_date,
            owner_id=owner_id,
            owner_name=owner_name,
            parent_objective_id=parent_objective_id,
            category=category,
            tags=tags or [],
            status=OKRStatus.DRAFT.value,
            progress=0.0,
            created_by=created_by
        )

        self.db.add(objective)
        self.db.commit()
        self.db.refresh(objective)

        return objective.to_dict()

    def get_objective(self, objective_id: str) -> Optional[Dict[str, Any]]:
        """
        Busca um objetivo pelo ID.

        Args:
            objective_id: ID do objetivo

        Returns:
            Dicionario com dados do objetivo ou None
        """
        query = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        )

        if self.tenant_id:
            query = query.filter(Objective.tenant_id == self.tenant_id)

        objective = query.first()

        if not objective:
            return None

        result = objective.to_dict()
        result["key_results"] = [kr.to_dict() for kr in objective.key_results if not kr.is_deleted]
        result["linked_stories"] = [link.to_dict() for link in objective.linked_stories]
        result["linked_projects"] = [link.to_dict() for link in objective.linked_projects]

        return result

    def list_objectives(
        self,
        status: str = None,
        period: str = None,
        owner_id: str = None,
        category: str = None,
        parent_objective_id: str = None,
        include_key_results: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Lista objetivos com filtros.

        Args:
            status: Filtrar por status
            period: Filtrar por periodo
            owner_id: Filtrar por owner
            category: Filtrar por categoria
            parent_objective_id: Filtrar por objetivo pai
            include_key_results: Incluir key results no resultado

        Returns:
            Lista de objetivos
        """
        query = self.db.query(Objective).filter(Objective.is_deleted == False)

        if self.tenant_id:
            query = query.filter(Objective.tenant_id == self.tenant_id)

        if status:
            query = query.filter(Objective.status == status)

        if period:
            query = query.filter(Objective.period == period)

        if owner_id:
            query = query.filter(Objective.owner_id == owner_id)

        if category:
            query = query.filter(Objective.category == category)

        if parent_objective_id:
            query = query.filter(Objective.parent_objective_id == parent_objective_id)

        objectives = query.order_by(Objective.created_at.desc()).all()

        results = []
        for obj in objectives:
            data = obj.to_dict()
            if include_key_results:
                data["key_results"] = [kr.to_dict() for kr in obj.key_results if not kr.is_deleted]
            results.append(data)

        return results

    def update_objective(
        self,
        objective_id: str,
        **updates
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza um objetivo.

        Args:
            objective_id: ID do objetivo
            **updates: Campos a atualizar

        Returns:
            Dicionario com dados atualizados ou None
        """
        query = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        )

        if self.tenant_id:
            query = query.filter(Objective.tenant_id == self.tenant_id)

        objective = query.first()

        if not objective:
            return None

        allowed_fields = [
            "title", "description", "period", "start_date", "end_date",
            "owner_id", "owner_name", "status", "parent_objective_id",
            "category", "tags"
        ]

        for field, value in updates.items():
            if field in allowed_fields and value is not None:
                setattr(objective, field, value)

        objective.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(objective)

        return objective.to_dict()

    def delete_objective(self, objective_id: str, deleted_by: str = "system") -> bool:
        """
        Deleta um objetivo (soft delete).

        Args:
            objective_id: ID do objetivo
            deleted_by: Usuario que deletou

        Returns:
            True se deletado, False se nao encontrado
        """
        query = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        )

        if self.tenant_id:
            query = query.filter(Objective.tenant_id == self.tenant_id)

        objective = query.first()

        if not objective:
            return False

        objective.soft_delete(deleted_by)
        self.db.commit()

        return True

    def activate_objective(self, objective_id: str) -> Optional[Dict[str, Any]]:
        """
        Ativa um objetivo (muda status de draft para active).

        Args:
            objective_id: ID do objetivo

        Returns:
            Objetivo atualizado ou None
        """
        return self.update_objective(objective_id, status=OKRStatus.ACTIVE.value)

    def complete_objective(self, objective_id: str) -> Optional[Dict[str, Any]]:
        """
        Marca um objetivo como completado.

        Args:
            objective_id: ID do objetivo

        Returns:
            Objetivo atualizado ou None
        """
        return self.update_objective(objective_id, status=OKRStatus.COMPLETED.value)

    # =========================================================================
    # KEY RESULTS - CRUD
    # =========================================================================

    def create_key_result(
        self,
        objective_id: str,
        title: str,
        target: float,
        description: str = None,
        initial: float = 0.0,
        unit: str = "units",
        metric_type: str = "increase",
        weight: float = 1.0,
        owner_id: str = None,
        owner_name: str = None,
        created_by: str = "system"
    ) -> Optional[Dict[str, Any]]:
        """
        Cria um novo Key Result.

        Args:
            objective_id: ID do objetivo pai
            title: Titulo do key result
            target: Meta a ser atingida
            description: Descricao
            initial: Valor inicial (baseline)
            unit: Unidade de medida (%, units, $, etc)
            metric_type: Tipo (increase, decrease, maintain)
            weight: Peso para calculo de progresso
            owner_id: ID do responsavel
            owner_name: Nome do responsavel
            created_by: Usuario que criou

        Returns:
            Dicionario com dados do key result criado ou None
        """
        # Verificar se objetivo existe
        objective = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        ).first()

        if not objective:
            return None

        key_result_id = f"KR-{uuid.uuid4().hex[:8].upper()}"

        key_result = KeyResult(
            key_result_id=key_result_id,
            objective_id=objective_id,
            title=title,
            description=description,
            target=target,
            current=initial,
            initial=initial,
            unit=unit,
            metric_type=metric_type,
            weight=weight,
            progress=0.0,
            status=OKRStatus.ACTIVE.value,
            owner_id=owner_id,
            owner_name=owner_name,
            value_history=[],
            created_by=created_by
        )

        self.db.add(key_result)
        self.db.commit()
        self.db.refresh(key_result)

        # Atualizar progresso do objetivo
        self._update_objective_progress(objective_id)

        return key_result.to_dict()

    def get_key_result(self, key_result_id: str) -> Optional[Dict[str, Any]]:
        """
        Busca um key result pelo ID.

        Args:
            key_result_id: ID do key result

        Returns:
            Dicionario com dados ou None
        """
        key_result = self.db.query(KeyResult).filter(
            KeyResult.key_result_id == key_result_id,
            KeyResult.is_deleted == False
        ).first()

        if not key_result:
            return None

        return key_result.to_dict()

    def update_key_result(
        self,
        key_result_id: str,
        **updates
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza um key result.

        Args:
            key_result_id: ID do key result
            **updates: Campos a atualizar

        Returns:
            Dicionario com dados atualizados ou None
        """
        key_result = self.db.query(KeyResult).filter(
            KeyResult.key_result_id == key_result_id,
            KeyResult.is_deleted == False
        ).first()

        if not key_result:
            return None

        allowed_fields = [
            "title", "description", "target", "initial", "unit",
            "metric_type", "weight", "status", "owner_id", "owner_name"
        ]

        for field, value in updates.items():
            if field in allowed_fields and value is not None:
                setattr(key_result, field, value)

        key_result.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(key_result)

        # Atualizar progresso do objetivo
        self._update_objective_progress(key_result.objective_id)

        return key_result.to_dict()

    def update_key_result_value(
        self,
        key_result_id: str,
        new_value: float,
        note: str = None
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza o valor atual de um key result.

        Args:
            key_result_id: ID do key result
            new_value: Novo valor atual
            note: Nota sobre a atualizacao

        Returns:
            Dicionario com dados atualizados ou None
        """
        key_result = self.db.query(KeyResult).filter(
            KeyResult.key_result_id == key_result_id,
            KeyResult.is_deleted == False
        ).first()

        if not key_result:
            return None

        key_result.update_value(new_value, note)
        self.db.commit()
        self.db.refresh(key_result)

        # Atualizar progresso do objetivo
        self._update_objective_progress(key_result.objective_id)

        return key_result.to_dict()

    def delete_key_result(self, key_result_id: str, deleted_by: str = "system") -> bool:
        """
        Deleta um key result (soft delete).

        Args:
            key_result_id: ID do key result
            deleted_by: Usuario que deletou

        Returns:
            True se deletado, False se nao encontrado
        """
        key_result = self.db.query(KeyResult).filter(
            KeyResult.key_result_id == key_result_id,
            KeyResult.is_deleted == False
        ).first()

        if not key_result:
            return False

        objective_id = key_result.objective_id
        key_result.soft_delete(deleted_by)
        self.db.commit()

        # Atualizar progresso do objetivo
        self._update_objective_progress(objective_id)

        return True

    # =========================================================================
    # PROGRESS CALCULATION
    # =========================================================================

    def _update_objective_progress(self, objective_id: str):
        """
        Atualiza o progresso de um objetivo baseado nos key results.

        Args:
            objective_id: ID do objetivo
        """
        objective = self.db.query(Objective).filter(
            Objective.objective_id == objective_id
        ).first()

        if not objective:
            return

        objective.progress = objective.calculate_progress()
        self.db.commit()

    def get_okr_progress(self, objective_id: str) -> Optional[Dict[str, Any]]:
        """
        Retorna progresso detalhado de um OKR.

        Args:
            objective_id: ID do objetivo

        Returns:
            Dicionario com progresso detalhado
        """
        objective = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        ).first()

        if not objective:
            return None

        key_results = [kr for kr in objective.key_results if not kr.is_deleted]

        return {
            "objective_id": objective.objective_id,
            "title": objective.title,
            "status": objective.status,
            "overall_progress": objective.progress,
            "period": objective.period,
            "start_date": objective.start_date.isoformat() if objective.start_date else None,
            "end_date": objective.end_date.isoformat() if objective.end_date else None,
            "key_results": [
                {
                    "key_result_id": kr.key_result_id,
                    "title": kr.title,
                    "current": kr.current,
                    "target": kr.target,
                    "unit": kr.unit,
                    "progress": kr.progress,
                    "metric_type": kr.metric_type
                }
                for kr in key_results
            ],
            "total_key_results": len(key_results),
            "completed_key_results": sum(1 for kr in key_results if kr.progress >= 100)
        }

    # =========================================================================
    # STORY LINKING
    # =========================================================================

    def link_story(
        self,
        objective_id: str,
        story_id: str,
        contribution: float = 1.0,
        notes: str = None,
        created_by: str = "system"
    ) -> Optional[Dict[str, Any]]:
        """
        Vincula uma story a um objetivo.

        Args:
            objective_id: ID do objetivo
            story_id: ID da story
            contribution: Peso da contribuicao (0-1)
            notes: Notas sobre a vinculacao
            created_by: Usuario que criou

        Returns:
            Dicionario com dados do link ou None
        """
        # Verificar se objetivo existe
        objective = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        ).first()

        if not objective:
            return None

        # Verificar se story existe
        story = self.db.query(Story).filter(
            Story.story_id == story_id,
            Story.is_deleted == False
        ).first()

        if not story:
            return None

        # Verificar se link ja existe
        existing_link = self.db.query(OKRStoryLink).filter(
            OKRStoryLink.objective_id == objective_id,
            OKRStoryLink.story_id == story_id
        ).first()

        if existing_link:
            # Atualizar link existente
            existing_link.contribution = contribution
            existing_link.notes = notes
            self.db.commit()
            return existing_link.to_dict()

        # Criar novo link
        link = OKRStoryLink(
            objective_id=objective_id,
            story_id=story_id,
            contribution=contribution,
            notes=notes,
            created_by=created_by
        )

        self.db.add(link)
        self.db.commit()
        self.db.refresh(link)

        return link.to_dict()

    def unlink_story(self, objective_id: str, story_id: str) -> bool:
        """
        Remove vinculo entre story e objetivo.

        Args:
            objective_id: ID do objetivo
            story_id: ID da story

        Returns:
            True se removido, False se nao encontrado
        """
        link = self.db.query(OKRStoryLink).filter(
            OKRStoryLink.objective_id == objective_id,
            OKRStoryLink.story_id == story_id
        ).first()

        if not link:
            return False

        self.db.delete(link)
        self.db.commit()

        return True

    def get_stories_for_objective(self, objective_id: str) -> List[Dict[str, Any]]:
        """
        Retorna stories vinculadas a um objetivo.

        Args:
            objective_id: ID do objetivo

        Returns:
            Lista de stories com dados de vinculacao
        """
        links = self.db.query(OKRStoryLink).filter(
            OKRStoryLink.objective_id == objective_id
        ).all()

        results = []
        for link in links:
            story_data = link.story.to_dict() if link.story else {}
            story_data["contribution"] = link.contribution
            story_data["link_notes"] = link.notes
            results.append(story_data)

        return results

    # =========================================================================
    # PROJECT LINKING
    # =========================================================================

    def link_project(
        self,
        objective_id: str,
        project_id: str,
        contribution: float = 1.0,
        notes: str = None,
        created_by: str = "system"
    ) -> Optional[Dict[str, Any]]:
        """
        Vincula um projeto a um objetivo.

        Args:
            objective_id: ID do objetivo
            project_id: ID do projeto
            contribution: Peso da contribuicao (0-1)
            notes: Notas sobre a vinculacao
            created_by: Usuario que criou

        Returns:
            Dicionario com dados do link ou None
        """
        # Verificar se objetivo existe
        objective = self.db.query(Objective).filter(
            Objective.objective_id == objective_id,
            Objective.is_deleted == False
        ).first()

        if not objective:
            return None

        # Verificar se projeto existe
        project = self.db.query(Project).filter(
            Project.project_id == project_id,
            Project.is_deleted == False
        ).first()

        if not project:
            return None

        # Verificar se link ja existe
        existing_link = self.db.query(OKRProjectLink).filter(
            OKRProjectLink.objective_id == objective_id,
            OKRProjectLink.project_id == project_id
        ).first()

        if existing_link:
            # Atualizar link existente
            existing_link.contribution = contribution
            existing_link.notes = notes
            self.db.commit()
            return existing_link.to_dict()

        # Criar novo link
        link = OKRProjectLink(
            objective_id=objective_id,
            project_id=project_id,
            contribution=contribution,
            notes=notes,
            created_by=created_by
        )

        self.db.add(link)
        self.db.commit()
        self.db.refresh(link)

        return link.to_dict()

    def unlink_project(self, objective_id: str, project_id: str) -> bool:
        """
        Remove vinculo entre projeto e objetivo.

        Args:
            objective_id: ID do objetivo
            project_id: ID do projeto

        Returns:
            True se removido, False se nao encontrado
        """
        link = self.db.query(OKRProjectLink).filter(
            OKRProjectLink.objective_id == objective_id,
            OKRProjectLink.project_id == project_id
        ).first()

        if not link:
            return False

        self.db.delete(link)
        self.db.commit()

        return True

    def get_projects_for_objective(self, objective_id: str) -> List[Dict[str, Any]]:
        """
        Retorna projetos vinculados a um objetivo.

        Args:
            objective_id: ID do objetivo

        Returns:
            Lista de projetos com dados de vinculacao
        """
        links = self.db.query(OKRProjectLink).filter(
            OKRProjectLink.objective_id == objective_id
        ).all()

        results = []
        for link in links:
            project_data = link.project.to_dict() if link.project else {}
            project_data["contribution"] = link.contribution
            project_data["link_notes"] = link.notes
            results.append(project_data)

        return results

    # =========================================================================
    # HIERARCHY
    # =========================================================================

    def get_objective_children(self, objective_id: str) -> List[Dict[str, Any]]:
        """
        Retorna objetivos filhos de um objetivo.

        Args:
            objective_id: ID do objetivo pai

        Returns:
            Lista de objetivos filhos
        """
        return self.list_objectives(parent_objective_id=objective_id, include_key_results=True)

    def get_objective_hierarchy(self, objective_id: str) -> Dict[str, Any]:
        """
        Retorna hierarquia completa de um objetivo.

        Args:
            objective_id: ID do objetivo

        Returns:
            Objetivo com filhos aninhados
        """
        objective = self.get_objective(objective_id)

        if not objective:
            return None

        children = self.get_objective_children(objective_id)

        for child in children:
            child["children"] = self.get_objective_children(child["objective_id"])

        objective["children"] = children

        return objective

    # =========================================================================
    # ANALYTICS
    # =========================================================================

    def get_okr_summary(self) -> Dict[str, Any]:
        """
        Retorna resumo geral dos OKRs.

        Returns:
            Dicionario com metricas gerais
        """
        query = self.db.query(Objective).filter(Objective.is_deleted == False)

        if self.tenant_id:
            query = query.filter(Objective.tenant_id == self.tenant_id)

        objectives = query.all()

        total = len(objectives)
        by_status = {}
        by_period = {}
        total_progress = 0

        for obj in objectives:
            by_status[obj.status] = by_status.get(obj.status, 0) + 1
            by_period[obj.period] = by_period.get(obj.period, 0) + 1
            total_progress += obj.progress

        avg_progress = total_progress / total if total > 0 else 0

        return {
            "total_objectives": total,
            "average_progress": round(avg_progress, 2),
            "by_status": by_status,
            "by_period": by_period,
            "on_track": sum(1 for obj in objectives if obj.progress >= 70),
            "at_risk": sum(1 for obj in objectives if 30 <= obj.progress < 70),
            "behind": sum(1 for obj in objectives if obj.progress < 30)
        }
