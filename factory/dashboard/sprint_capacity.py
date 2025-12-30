# -*- coding: utf-8 -*-
"""
Sprint Capacity Planning Module (Issue #279)
=============================================
Planejamento de capacidade do time para sprints.

Funcionalidades:
- Definir capacidade por membro do time
- Considerar ferias, feriados, reunioes
- Calcular disponibilidade do sprint
- Alertar sobre over-commitment

Endpoints:
- GET /api/capacity/team - Capacidade do time
- POST /api/capacity/member - Define capacidade de membro
- GET /api/capacity/sprint/{id} - Capacidade do sprint
- GET /api/capacity/availability - Disponibilidade
- POST /api/capacity/absence - Registra ausencia
- GET /api/capacity/forecast - Previsao de entrega
"""

from fastapi import FastAPI, HTTPException, Query
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime, date, timedelta
from enum import Enum
import uuid


# =============================================================================
# ENUMS
# =============================================================================

class AbsenceType(str, Enum):
    """Tipos de ausencia"""
    VACATION = "vacation"  # Ferias
    HOLIDAY = "holiday"  # Feriado
    SICK_LEAVE = "sick_leave"  # Licenca medica
    MEETING = "meeting"  # Reunioes
    TRAINING = "training"  # Treinamento
    OTHER = "other"  # Outros


class SprintHealthStatus(str, Enum):
    """Status de saude do sprint"""
    HEALTHY = "healthy"  # Verde - capacidade ok
    WARNING = "warning"  # Amarelo - proximo do limite
    OVERCOMMITTED = "overcommitted"  # Vermelho - acima da capacidade
    UNDERCOMMITTED = "undercommitted"  # Azul - capacidade sobrando


# =============================================================================
# MODELS (Pydantic)
# =============================================================================

class TeamMemberBase(BaseModel):
    """Base model para membro do time"""
    name: str = Field(..., min_length=1, max_length=100)
    email: str = Field(..., min_length=5, max_length=200)
    role: str = Field("developer", max_length=50)
    hours_per_day: float = Field(8.0, ge=0, le=24)
    velocity_factor: float = Field(1.0, ge=0.1, le=2.0)  # Fator de produtividade


class TeamMemberCreate(TeamMemberBase):
    """Schema para criar membro do time"""
    pass


class TeamMember(TeamMemberBase):
    """Schema completo do membro do time"""
    member_id: str
    project_id: str
    created_at: datetime
    is_active: bool = True


class AbsenceBase(BaseModel):
    """Base model para ausencia"""
    member_id: str
    absence_type: AbsenceType
    start_date: date
    end_date: date
    hours_per_day: float = Field(8.0, ge=0, le=24)  # Horas de ausencia por dia
    reason: Optional[str] = None


class AbsenceCreate(AbsenceBase):
    """Schema para criar ausencia"""
    pass


class Absence(AbsenceBase):
    """Schema completo da ausencia"""
    absence_id: str
    created_at: datetime


class CapacityBase(BaseModel):
    """Base model para capacidade"""
    member_id: str
    sprint_id: str
    available_hours: float
    allocated_hours: float = 0.0
    notes: Optional[str] = None


class CapacityCreate(CapacityBase):
    """Schema para definir capacidade"""
    pass


class Capacity(CapacityBase):
    """Schema completo da capacidade"""
    capacity_id: str
    remaining_hours: float
    utilization_percent: float
    created_at: datetime


class SprintCapacity(BaseModel):
    """Capacidade total do sprint"""
    sprint_id: str
    sprint_name: str
    start_date: date
    end_date: date
    working_days: int
    total_team_hours: float
    allocated_hours: float
    remaining_hours: float
    utilization_percent: float
    health_status: SprintHealthStatus
    members_capacity: List[Dict[str, Any]]
    velocity_forecast: float  # Story points estimados


class AvailabilityResponse(BaseModel):
    """Resposta de disponibilidade"""
    member_id: str
    member_name: str
    sprint_id: str
    total_hours: float
    absence_hours: float
    available_hours: float
    allocated_hours: float
    remaining_hours: float
    availability_percent: float


class ForecastResponse(BaseModel):
    """Resposta de previsao de entrega"""
    sprint_id: str
    total_capacity_hours: float
    committed_story_points: int
    average_velocity: float
    forecast_story_points: float
    confidence_level: str  # low, medium, high
    risk_factors: List[str]
    recommendations: List[str]


# =============================================================================
# IN-MEMORY STORAGE (para demonstracao)
# =============================================================================

# Armazenamento em memoria
team_members_db: Dict[str, Dict[str, Any]] = {}
absences_db: Dict[str, Dict[str, Any]] = {}
capacities_db: Dict[str, Dict[str, Any]] = {}

# Dados de exemplo
SAMPLE_MEMBERS = [
    {"member_id": "MBR-001", "name": "Joao Silva", "email": "joao@example.com", "role": "developer", "hours_per_day": 8.0, "velocity_factor": 1.0},
    {"member_id": "MBR-002", "name": "Maria Santos", "email": "maria@example.com", "role": "developer", "hours_per_day": 8.0, "velocity_factor": 1.2},
    {"member_id": "MBR-003", "name": "Pedro Costa", "email": "pedro@example.com", "role": "tech_lead", "hours_per_day": 6.0, "velocity_factor": 0.8},
    {"member_id": "MBR-004", "name": "Ana Oliveira", "email": "ana@example.com", "role": "developer", "hours_per_day": 8.0, "velocity_factor": 1.1},
    {"member_id": "MBR-005", "name": "Carlos Lima", "email": "carlos@example.com", "role": "qa", "hours_per_day": 8.0, "velocity_factor": 0.9},
]

# Feriados nacionais 2025 (Brasil)
HOLIDAYS_2025 = [
    date(2025, 1, 1),   # Ano Novo
    date(2025, 3, 3),   # Carnaval
    date(2025, 3, 4),   # Carnaval
    date(2025, 4, 18),  # Sexta-feira Santa
    date(2025, 4, 21),  # Tiradentes
    date(2025, 5, 1),   # Dia do Trabalho
    date(2025, 6, 19),  # Corpus Christi
    date(2025, 9, 7),   # Independencia
    date(2025, 10, 12), # Nossa Senhora
    date(2025, 11, 2),  # Finados
    date(2025, 11, 15), # Proclamacao Republica
    date(2025, 12, 25), # Natal
]


def init_sample_data():
    """Inicializa dados de exemplo"""
    for member in SAMPLE_MEMBERS:
        team_members_db[member["member_id"]] = {
            **member,
            "project_id": "default",
            "created_at": datetime.now(),
            "is_active": True
        }


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_working_days(start_date: date, end_date: date, holidays: List[date] = None) -> int:
    """Calcula dias uteis entre duas datas"""
    if holidays is None:
        holidays = HOLIDAYS_2025

    working_days = 0
    current = start_date
    while current <= end_date:
        # Verifica se nao e fim de semana e nao e feriado
        if current.weekday() < 5 and current not in holidays:
            working_days += 1
        current += timedelta(days=1)
    return working_days


def calculate_absence_hours(member_id: str, start_date: date, end_date: date) -> float:
    """Calcula horas de ausencia de um membro em um periodo"""
    total_hours = 0.0
    for absence in absences_db.values():
        if absence["member_id"] != member_id:
            continue

        # Verifica intersecao de datas
        abs_start = absence["start_date"]
        abs_end = absence["end_date"]

        # Calcula intersecao
        overlap_start = max(start_date, abs_start)
        overlap_end = min(end_date, abs_end)

        if overlap_start <= overlap_end:
            overlap_days = get_working_days(overlap_start, overlap_end)
            total_hours += overlap_days * absence["hours_per_day"]

    return total_hours


def calculate_member_availability(member_id: str, start_date: date, end_date: date) -> Dict[str, Any]:
    """Calcula disponibilidade de um membro para um periodo"""
    member = team_members_db.get(member_id)
    if not member:
        return None

    working_days = get_working_days(start_date, end_date)
    total_hours = working_days * member["hours_per_day"]
    absence_hours = calculate_absence_hours(member_id, start_date, end_date)
    available_hours = max(0, total_hours - absence_hours)

    # Aplica fator de velocidade
    effective_hours = available_hours * member["velocity_factor"]

    # Busca horas alocadas
    allocated_hours = 0.0
    for cap in capacities_db.values():
        if cap["member_id"] == member_id:
            allocated_hours += cap.get("allocated_hours", 0)

    remaining_hours = max(0, effective_hours - allocated_hours)

    return {
        "member_id": member_id,
        "member_name": member["name"],
        "role": member["role"],
        "working_days": working_days,
        "total_hours": total_hours,
        "absence_hours": absence_hours,
        "effective_hours": effective_hours,
        "allocated_hours": allocated_hours,
        "remaining_hours": remaining_hours,
        "availability_percent": round((remaining_hours / effective_hours * 100) if effective_hours > 0 else 0, 1)
    }


def get_sprint_health(utilization_percent: float) -> SprintHealthStatus:
    """Determina status de saude baseado na utilizacao"""
    if utilization_percent > 100:
        return SprintHealthStatus.OVERCOMMITTED
    elif utilization_percent > 85:
        return SprintHealthStatus.WARNING
    elif utilization_percent < 50:
        return SprintHealthStatus.UNDERCOMMITTED
    else:
        return SprintHealthStatus.HEALTHY


# =============================================================================
# REGISTER FUNCTION
# =============================================================================

def register_sprint_capacity(app: FastAPI):
    """Registra endpoints de capacidade do sprint"""

    # Inicializa dados de exemplo
    init_sample_data()

    # -------------------------------------------------------------------------
    # GET /api/capacity/team - Capacidade do time
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/team", tags=["Sprint Capacity"])
    async def get_team_capacity(
        project_id: str = Query("default", description="ID do projeto"),
        sprint_id: Optional[str] = Query(None, description="ID do sprint para filtrar")
    ):
        """
        Retorna capacidade do time completo.

        Returns:
            Lista de membros com suas capacidades
        """
        members = []
        for member_id, member in team_members_db.items():
            if member.get("project_id") != project_id and project_id != "default":
                continue
            if not member.get("is_active", True):
                continue

            # Calcula disponibilidade para proximo sprint (2 semanas)
            today = date.today()
            sprint_start = today
            sprint_end = today + timedelta(days=14)

            availability = calculate_member_availability(member_id, sprint_start, sprint_end)

            members.append({
                **member,
                "availability": availability
            })

        # Calcula totais
        total_hours = sum(m["availability"]["effective_hours"] for m in members)
        allocated_hours = sum(m["availability"]["allocated_hours"] for m in members)

        return {
            "project_id": project_id,
            "team_size": len(members),
            "total_capacity_hours": total_hours,
            "allocated_hours": allocated_hours,
            "remaining_hours": total_hours - allocated_hours,
            "utilization_percent": round((allocated_hours / total_hours * 100) if total_hours > 0 else 0, 1),
            "members": members
        }

    # -------------------------------------------------------------------------
    # POST /api/capacity/member - Define capacidade de membro
    # -------------------------------------------------------------------------
    @app.post("/api/capacity/member", tags=["Sprint Capacity"])
    async def create_team_member(member: TeamMemberCreate):
        """
        Cria ou atualiza capacidade de um membro do time.

        Args:
            member: Dados do membro

        Returns:
            Membro criado/atualizado
        """
        member_id = f"MBR-{str(uuid.uuid4())[:8].upper()}"

        member_data = {
            "member_id": member_id,
            **member.dict(),
            "project_id": "default",
            "created_at": datetime.now(),
            "is_active": True
        }

        team_members_db[member_id] = member_data

        return {
            "success": True,
            "message": "Membro adicionado ao time",
            "member": member_data
        }

    # -------------------------------------------------------------------------
    # PUT /api/capacity/member/{member_id} - Atualiza membro
    # -------------------------------------------------------------------------
    @app.put("/api/capacity/member/{member_id}", tags=["Sprint Capacity"])
    async def update_team_member(member_id: str, member: TeamMemberCreate):
        """
        Atualiza dados de um membro do time.
        """
        if member_id not in team_members_db:
            raise HTTPException(status_code=404, detail="Membro nao encontrado")

        existing = team_members_db[member_id]
        team_members_db[member_id] = {
            **existing,
            **member.dict(),
            "updated_at": datetime.now()
        }

        return {
            "success": True,
            "message": "Membro atualizado",
            "member": team_members_db[member_id]
        }

    # -------------------------------------------------------------------------
    # DELETE /api/capacity/member/{member_id} - Remove membro
    # -------------------------------------------------------------------------
    @app.delete("/api/capacity/member/{member_id}", tags=["Sprint Capacity"])
    async def delete_team_member(member_id: str):
        """
        Remove um membro do time (soft delete).
        """
        if member_id not in team_members_db:
            raise HTTPException(status_code=404, detail="Membro nao encontrado")

        team_members_db[member_id]["is_active"] = False
        team_members_db[member_id]["deleted_at"] = datetime.now()

        return {
            "success": True,
            "message": "Membro removido do time"
        }

    # -------------------------------------------------------------------------
    # GET /api/capacity/sprint/{sprint_id} - Capacidade do sprint
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/sprint/{sprint_id}", tags=["Sprint Capacity"])
    async def get_sprint_capacity(sprint_id: str):
        """
        Retorna capacidade detalhada de um sprint especifico.

        Args:
            sprint_id: ID do sprint

        Returns:
            Capacidade do sprint com membros
        """
        # Busca dados do sprint (mock para demonstracao)
        today = date.today()
        sprint_data = {
            "sprint_id": sprint_id,
            "sprint_name": f"Sprint {sprint_id}",
            "start_date": today,
            "end_date": today + timedelta(days=14),
            "goal": "Entregar funcionalidades core"
        }

        # Calcula capacidade de cada membro
        members_capacity = []
        total_hours = 0.0
        allocated_hours = 0.0

        for member_id, member in team_members_db.items():
            if not member.get("is_active", True):
                continue

            availability = calculate_member_availability(
                member_id,
                sprint_data["start_date"],
                sprint_data["end_date"]
            )

            members_capacity.append({
                "member_id": member_id,
                "name": member["name"],
                "role": member["role"],
                **availability
            })

            total_hours += availability["effective_hours"]
            allocated_hours += availability["allocated_hours"]

        working_days = get_working_days(sprint_data["start_date"], sprint_data["end_date"])
        remaining_hours = max(0, total_hours - allocated_hours)
        utilization = (allocated_hours / total_hours * 100) if total_hours > 0 else 0

        # Estima velocity baseado em historico (mock: 1 story point = 4 horas)
        velocity_forecast = total_hours / 4

        return SprintCapacity(
            sprint_id=sprint_id,
            sprint_name=sprint_data["sprint_name"],
            start_date=sprint_data["start_date"],
            end_date=sprint_data["end_date"],
            working_days=working_days,
            total_team_hours=total_hours,
            allocated_hours=allocated_hours,
            remaining_hours=remaining_hours,
            utilization_percent=round(utilization, 1),
            health_status=get_sprint_health(utilization),
            members_capacity=members_capacity,
            velocity_forecast=round(velocity_forecast, 1)
        )

    # -------------------------------------------------------------------------
    # GET /api/capacity/availability - Disponibilidade
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/availability", tags=["Sprint Capacity"])
    async def get_availability(
        member_id: Optional[str] = Query(None, description="ID do membro (opcional)"),
        start_date: Optional[str] = Query(None, description="Data inicio (YYYY-MM-DD)"),
        end_date: Optional[str] = Query(None, description="Data fim (YYYY-MM-DD)")
    ):
        """
        Retorna disponibilidade do time ou membro especifico.

        Returns:
            Lista de disponibilidades
        """
        # Parse datas
        today = date.today()
        if start_date:
            start = date.fromisoformat(start_date)
        else:
            start = today

        if end_date:
            end = date.fromisoformat(end_date)
        else:
            end = today + timedelta(days=14)

        availabilities = []

        members_to_check = []
        if member_id:
            if member_id in team_members_db:
                members_to_check = [member_id]
        else:
            members_to_check = [m for m in team_members_db.keys()]

        for mid in members_to_check:
            member = team_members_db.get(mid)
            if not member or not member.get("is_active", True):
                continue

            availability = calculate_member_availability(mid, start, end)
            if availability:
                availabilities.append(availability)

        # Calcula totais
        total_effective = sum(a["effective_hours"] for a in availabilities)
        total_remaining = sum(a["remaining_hours"] for a in availabilities)

        return {
            "period": {
                "start_date": start.isoformat(),
                "end_date": end.isoformat(),
                "working_days": get_working_days(start, end)
            },
            "summary": {
                "team_size": len(availabilities),
                "total_effective_hours": total_effective,
                "total_remaining_hours": total_remaining,
                "average_availability_percent": round(
                    sum(a["availability_percent"] for a in availabilities) / len(availabilities)
                    if availabilities else 0, 1
                )
            },
            "members": availabilities
        }

    # -------------------------------------------------------------------------
    # POST /api/capacity/absence - Registra ausencia
    # -------------------------------------------------------------------------
    @app.post("/api/capacity/absence", tags=["Sprint Capacity"])
    async def create_absence(absence: AbsenceCreate):
        """
        Registra uma ausencia (ferias, feriado, reuniao, etc).

        Args:
            absence: Dados da ausencia

        Returns:
            Ausencia registrada
        """
        if absence.member_id not in team_members_db:
            raise HTTPException(status_code=404, detail="Membro nao encontrado")

        if absence.start_date > absence.end_date:
            raise HTTPException(status_code=400, detail="Data inicio deve ser anterior a data fim")

        absence_id = f"ABS-{str(uuid.uuid4())[:8].upper()}"

        absence_data = {
            "absence_id": absence_id,
            "member_id": absence.member_id,
            "absence_type": absence.absence_type.value,
            "start_date": absence.start_date,
            "end_date": absence.end_date,
            "hours_per_day": absence.hours_per_day,
            "reason": absence.reason,
            "created_at": datetime.now()
        }

        absences_db[absence_id] = absence_data

        # Calcula impacto
        working_days = get_working_days(absence.start_date, absence.end_date)
        total_hours = working_days * absence.hours_per_day

        member = team_members_db[absence.member_id]

        return {
            "success": True,
            "message": "Ausencia registrada",
            "absence": {
                **absence_data,
                "start_date": absence_data["start_date"].isoformat(),
                "end_date": absence_data["end_date"].isoformat()
            },
            "impact": {
                "member_name": member["name"],
                "working_days_affected": working_days,
                "total_hours_lost": total_hours
            }
        }

    # -------------------------------------------------------------------------
    # GET /api/capacity/absences - Lista ausencias
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/absences", tags=["Sprint Capacity"])
    async def list_absences(
        member_id: Optional[str] = Query(None),
        absence_type: Optional[AbsenceType] = Query(None),
        start_date: Optional[str] = Query(None),
        end_date: Optional[str] = Query(None)
    ):
        """
        Lista ausencias registradas.
        """
        results = []

        for absence in absences_db.values():
            # Filtros
            if member_id and absence["member_id"] != member_id:
                continue
            if absence_type and absence["absence_type"] != absence_type.value:
                continue

            member = team_members_db.get(absence["member_id"], {})

            results.append({
                **absence,
                "start_date": absence["start_date"].isoformat(),
                "end_date": absence["end_date"].isoformat(),
                "member_name": member.get("name", "Desconhecido")
            })

        return {
            "count": len(results),
            "absences": sorted(results, key=lambda x: x["start_date"], reverse=True)
        }

    # -------------------------------------------------------------------------
    # DELETE /api/capacity/absence/{absence_id} - Remove ausencia
    # -------------------------------------------------------------------------
    @app.delete("/api/capacity/absence/{absence_id}", tags=["Sprint Capacity"])
    async def delete_absence(absence_id: str):
        """
        Remove uma ausencia registrada.
        """
        if absence_id not in absences_db:
            raise HTTPException(status_code=404, detail="Ausencia nao encontrada")

        del absences_db[absence_id]

        return {
            "success": True,
            "message": "Ausencia removida"
        }

    # -------------------------------------------------------------------------
    # GET /api/capacity/forecast - Previsao de entrega
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/forecast", tags=["Sprint Capacity"])
    async def get_delivery_forecast(
        sprint_id: str = Query(..., description="ID do sprint"),
        committed_points: int = Query(0, ge=0, description="Story points comprometidos")
    ):
        """
        Retorna previsao de entrega baseada em capacidade e velocidade.

        Args:
            sprint_id: ID do sprint
            committed_points: Story points ja comprometidos

        Returns:
            Previsao com recomendacoes
        """
        # Calcula capacidade do sprint
        today = date.today()
        sprint_start = today
        sprint_end = today + timedelta(days=14)

        total_hours = 0.0
        members_count = 0

        for member_id, member in team_members_db.items():
            if not member.get("is_active", True):
                continue

            availability = calculate_member_availability(member_id, sprint_start, sprint_end)
            total_hours += availability["effective_hours"]
            members_count += 1

        # Calcula velocidade media (mock: 1 SP = 4 horas em media)
        hours_per_point = 4.0
        average_velocity = total_hours / hours_per_point if total_hours > 0 else 0

        # Fatores de risco
        risk_factors = []
        recommendations = []

        # Analisa riscos
        utilization = (committed_points * hours_per_point / total_hours * 100) if total_hours > 0 else 0

        if utilization > 100:
            risk_factors.append("Over-commitment: Time comprometeu mais do que a capacidade")
            recommendations.append(f"Remover {int(committed_points - average_velocity)} story points do sprint")
        elif utilization > 85:
            risk_factors.append("Alta utilizacao: Pouca margem para imprevistos")
            recommendations.append("Considere buffer de 15-20% para imprevistos")
        elif utilization < 50:
            risk_factors.append("Baixa utilizacao: Capacidade subutilizada")
            recommendations.append(f"Time pode assumir mais {int(average_velocity - committed_points)} story points")

        # Verifica ausencias
        absences_count = len([a for a in absences_db.values()
                            if a["start_date"] <= sprint_end and a["end_date"] >= sprint_start])
        if absences_count > 0:
            risk_factors.append(f"{absences_count} ausencias planejadas afetam este sprint")
            recommendations.append("Revisar alocacao considerando ausencias")

        # Determina nivel de confianca
        if len(risk_factors) == 0:
            confidence = "high"
        elif len(risk_factors) <= 2:
            confidence = "medium"
        else:
            confidence = "low"

        # Se nao houver riscos, adiciona recomendacao positiva
        if not recommendations:
            recommendations.append("Planejamento adequado - mantenha o ritmo")

        return ForecastResponse(
            sprint_id=sprint_id,
            total_capacity_hours=total_hours,
            committed_story_points=committed_points,
            average_velocity=round(average_velocity, 1),
            forecast_story_points=round(average_velocity * 0.85, 1),  # 85% de confianca
            confidence_level=confidence,
            risk_factors=risk_factors,
            recommendations=recommendations
        )

    # -------------------------------------------------------------------------
    # GET /api/capacity/holidays - Lista feriados
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/holidays", tags=["Sprint Capacity"])
    async def list_holidays(year: int = Query(2025, ge=2020, le=2030)):
        """
        Lista feriados do ano.
        """
        holidays = []
        for h in HOLIDAYS_2025:
            if h.year == year:
                holidays.append({
                    "date": h.isoformat(),
                    "weekday": h.strftime("%A"),
                    "name": "Feriado Nacional"
                })

        return {
            "year": year,
            "count": len(holidays),
            "holidays": holidays
        }

    # -------------------------------------------------------------------------
    # GET /api/capacity/health - Saude do sprint
    # -------------------------------------------------------------------------
    @app.get("/api/capacity/health", tags=["Sprint Capacity"])
    async def get_sprint_health_status(
        sprint_id: str = Query(..., description="ID do sprint"),
        committed_points: int = Query(0, ge=0)
    ):
        """
        Retorna indicadores de saude do sprint.
        """
        # Calcula capacidade
        today = date.today()
        sprint_start = today
        sprint_end = today + timedelta(days=14)

        total_hours = 0.0
        allocated_hours = 0.0
        members_at_risk = []

        for member_id, member in team_members_db.items():
            if not member.get("is_active", True):
                continue

            availability = calculate_member_availability(member_id, sprint_start, sprint_end)
            total_hours += availability["effective_hours"]
            allocated_hours += availability["allocated_hours"]

            # Verifica se membro esta sobrecarregado
            if availability["availability_percent"] < 20:
                members_at_risk.append({
                    "member_id": member_id,
                    "name": member["name"],
                    "availability_percent": availability["availability_percent"]
                })

        utilization = (allocated_hours / total_hours * 100) if total_hours > 0 else 0
        health = get_sprint_health(utilization)

        # Indicadores
        indicators = {
            "capacity_utilization": {
                "value": round(utilization, 1),
                "status": health.value,
                "threshold": {"warning": 85, "critical": 100}
            },
            "team_availability": {
                "value": round(100 - (len(members_at_risk) / len(team_members_db) * 100), 1),
                "status": "healthy" if len(members_at_risk) == 0 else "warning",
                "members_at_risk": members_at_risk
            },
            "velocity_vs_commitment": {
                "planned": committed_points,
                "capacity": round(total_hours / 4, 1),
                "status": "healthy" if committed_points <= total_hours / 4 else "overcommitted"
            }
        }

        return {
            "sprint_id": sprint_id,
            "overall_health": health.value,
            "indicators": indicators,
            "alerts": [
                {
                    "type": "warning" if health == SprintHealthStatus.WARNING else "error",
                    "message": f"Utilizacao em {round(utilization, 1)}%"
                }
            ] if health in [SprintHealthStatus.WARNING, SprintHealthStatus.OVERCOMMITTED] else []
        }

    # -------------------------------------------------------------------------
    # GET /capacity - Pagina HTML de capacidade
    # -------------------------------------------------------------------------
    @app.get("/capacity", response_class="HTMLResponse", tags=["Sprint Capacity"])
    async def capacity_page():
        """
        Pagina de visualizacao de capacidade do time.
        """
        return get_capacity_html()

    print("[Dashboard] Sprint Capacity endpoints loaded")


# =============================================================================
# VUE.JS COMPONENT HTML
# =============================================================================

def get_capacity_html() -> str:
    """Retorna HTML com componente Vue.js para visualizacao de capacidade"""
    return '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Capacidade do Sprint - Fabrica de Agentes</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
            --success-green: #10B981;
            --warning-yellow: #F59E0B;
            --danger-red: #EF4444;
            --info-blue: #3B82F6;
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background-color: #F3F4F6;
            color: #1F2937;
        }

        .header {
            background: linear-gradient(135deg, var(--belgo-blue) 0%, #005566 100%);
            color: white;
            padding: 1.5rem 2rem;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        .header h1 {
            font-size: 1.5rem;
            font-weight: 600;
        }

        .header p {
            opacity: 0.8;
            font-size: 0.9rem;
            margin-top: 0.25rem;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 1.5rem;
        }

        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 1.5rem;
            margin-bottom: 1.5rem;
        }

        .card {
            background: white;
            border-radius: 12px;
            padding: 1.5rem;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .card-title {
            font-size: 0.875rem;
            color: #6B7280;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 0.75rem;
        }

        .card-value {
            font-size: 2rem;
            font-weight: 700;
            color: var(--belgo-blue);
        }

        .card-subtitle {
            font-size: 0.875rem;
            color: #9CA3AF;
            margin-top: 0.25rem;
        }

        .health-badge {
            display: inline-flex;
            align-items: center;
            padding: 0.5rem 1rem;
            border-radius: 9999px;
            font-weight: 600;
            font-size: 0.875rem;
        }

        .health-healthy { background: #D1FAE5; color: #065F46; }
        .health-warning { background: #FEF3C7; color: #92400E; }
        .health-overcommitted { background: #FEE2E2; color: #991B1B; }
        .health-undercommitted { background: #DBEAFE; color: #1E40AF; }

        .progress-bar {
            width: 100%;
            height: 12px;
            background: #E5E7EB;
            border-radius: 6px;
            overflow: hidden;
            margin-top: 1rem;
        }

        .progress-fill {
            height: 100%;
            border-radius: 6px;
            transition: width 0.3s ease;
        }

        .progress-healthy { background: var(--success-green); }
        .progress-warning { background: var(--warning-yellow); }
        .progress-overcommitted { background: var(--danger-red); }

        .team-table {
            width: 100%;
            border-collapse: collapse;
        }

        .team-table th,
        .team-table td {
            padding: 1rem;
            text-align: left;
            border-bottom: 1px solid #E5E7EB;
        }

        .team-table th {
            background: #F9FAFB;
            font-weight: 600;
            color: #374151;
            font-size: 0.875rem;
        }

        .team-table tr:hover {
            background: #F9FAFB;
        }

        .avatar {
            width: 40px;
            height: 40px;
            border-radius: 50%;
            background: var(--belgo-blue);
            color: white;
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: 600;
        }

        .role-badge {
            display: inline-block;
            padding: 0.25rem 0.75rem;
            border-radius: 9999px;
            font-size: 0.75rem;
            font-weight: 500;
        }

        .role-developer { background: #DBEAFE; color: #1E40AF; }
        .role-tech_lead { background: #FEF3C7; color: #92400E; }
        .role-qa { background: #D1FAE5; color: #065F46; }

        .btn {
            padding: 0.75rem 1.5rem;
            border: none;
            border-radius: 8px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.2s;
        }

        .btn-primary {
            background: var(--belgo-orange);
            color: white;
        }

        .btn-primary:hover {
            background: #E55A00;
        }

        .btn-secondary {
            background: #E5E7EB;
            color: #374151;
        }

        .chart-container {
            position: relative;
            height: 300px;
        }

        .alert {
            padding: 1rem;
            border-radius: 8px;
            margin-bottom: 1rem;
            display: flex;
            align-items: center;
            gap: 0.75rem;
        }

        .alert-warning {
            background: #FEF3C7;
            border: 1px solid #F59E0B;
            color: #92400E;
        }

        .alert-error {
            background: #FEE2E2;
            border: 1px solid #EF4444;
            color: #991B1B;
        }

        .modal {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 1000;
        }

        .modal-content {
            background: white;
            border-radius: 12px;
            padding: 2rem;
            max-width: 500px;
            width: 90%;
        }

        .form-group {
            margin-bottom: 1rem;
        }

        .form-group label {
            display: block;
            margin-bottom: 0.5rem;
            font-weight: 500;
        }

        .form-group input,
        .form-group select {
            width: 100%;
            padding: 0.75rem;
            border: 1px solid #D1D5DB;
            border-radius: 8px;
            font-size: 1rem;
        }

        .nav-link {
            color: white;
            text-decoration: none;
            opacity: 0.8;
            transition: opacity 0.2s;
        }

        .nav-link:hover {
            opacity: 1;
        }
    </style>
</head>
<body>
    <div id="app">
        <header class="header">
            <div style="display: flex; justify-content: space-between; align-items: center;">
                <div>
                    <h1>Planejamento de Capacidade</h1>
                    <p>Sprint Capacity Planning - Issue #279</p>
                </div>
                <a href="/" class="nav-link">Voltar ao Dashboard</a>
            </div>
        </header>

        <div class="container">
            <!-- Alertas -->
            <div v-if="alerts.length > 0">
                <div v-for="alert in alerts" :key="alert.message"
                     :class="['alert', 'alert-' + alert.type]">
                    <span v-if="alert.type === 'warning'">&#9888;</span>
                    <span v-else>&#10060;</span>
                    {{ alert.message }}
                </div>
            </div>

            <!-- Cards de Resumo -->
            <div class="grid">
                <div class="card">
                    <div class="card-title">Tamanho do Time</div>
                    <div class="card-value">{{ teamData.team_size || 0 }}</div>
                    <div class="card-subtitle">membros ativos</div>
                </div>

                <div class="card">
                    <div class="card-title">Capacidade Total</div>
                    <div class="card-value">{{ teamData.total_capacity_hours?.toFixed(0) || 0 }}h</div>
                    <div class="card-subtitle">horas disponiveis</div>
                </div>

                <div class="card">
                    <div class="card-title">Horas Alocadas</div>
                    <div class="card-value">{{ teamData.allocated_hours?.toFixed(0) || 0 }}h</div>
                    <div class="card-subtitle">{{ teamData.utilization_percent?.toFixed(1) || 0 }}% utilizacao</div>
                    <div class="progress-bar">
                        <div class="progress-fill"
                             :class="getProgressClass(teamData.utilization_percent)"
                             :style="{ width: Math.min(teamData.utilization_percent || 0, 100) + '%' }">
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">Saude do Sprint</div>
                    <span :class="['health-badge', 'health-' + healthStatus]">
                        {{ getHealthLabel(healthStatus) }}
                    </span>
                    <div class="card-subtitle" style="margin-top: 0.75rem;">
                        Previsao: {{ forecast.forecast_story_points || 0 }} story points
                    </div>
                </div>
            </div>

            <!-- Grafico de Capacidade -->
            <div class="card" style="margin-bottom: 1.5rem;">
                <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;">
                    <h2 style="font-size: 1.25rem; font-weight: 600;">Capacidade por Membro</h2>
                    <button class="btn btn-primary" @click="showAddMemberModal = true">
                        + Adicionar Membro
                    </button>
                </div>
                <div class="chart-container">
                    <canvas id="capacityChart"></canvas>
                </div>
            </div>

            <!-- Tabela do Time -->
            <div class="card">
                <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;">
                    <h2 style="font-size: 1.25rem; font-weight: 600;">Membros do Time</h2>
                    <button class="btn btn-secondary" @click="showAbsenceModal = true">
                        + Registrar Ausencia
                    </button>
                </div>

                <table class="team-table">
                    <thead>
                        <tr>
                            <th>Membro</th>
                            <th>Funcao</th>
                            <th>Horas/Dia</th>
                            <th>Disponivel</th>
                            <th>Alocado</th>
                            <th>Disponibilidade</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr v-for="member in teamData.members" :key="member.member_id">
                            <td>
                                <div style="display: flex; align-items: center; gap: 0.75rem;">
                                    <div class="avatar">{{ getInitials(member.name) }}</div>
                                    <div>
                                        <div style="font-weight: 500;">{{ member.name }}</div>
                                        <div style="font-size: 0.875rem; color: #6B7280;">{{ member.email }}</div>
                                    </div>
                                </div>
                            </td>
                            <td>
                                <span :class="['role-badge', 'role-' + member.role]">
                                    {{ getRoleLabel(member.role) }}
                                </span>
                            </td>
                            <td>{{ member.hours_per_day }}h</td>
                            <td>{{ member.availability?.effective_hours?.toFixed(1) || 0 }}h</td>
                            <td>{{ member.availability?.allocated_hours?.toFixed(1) || 0 }}h</td>
                            <td>
                                <div style="display: flex; align-items: center; gap: 0.5rem;">
                                    <div class="progress-bar" style="width: 100px; height: 8px;">
                                        <div class="progress-fill"
                                             :class="getProgressClass(100 - (member.availability?.availability_percent || 0))"
                                             :style="{ width: (100 - (member.availability?.availability_percent || 0)) + '%' }">
                                        </div>
                                    </div>
                                    <span style="font-size: 0.875rem; color: #6B7280;">
                                        {{ member.availability?.availability_percent?.toFixed(0) || 0 }}%
                                    </span>
                                </div>
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>

            <!-- Modal Adicionar Membro -->
            <div class="modal" v-if="showAddMemberModal" @click.self="showAddMemberModal = false">
                <div class="modal-content">
                    <h3 style="margin-bottom: 1.5rem; font-size: 1.25rem;">Adicionar Membro ao Time</h3>
                    <form @submit.prevent="addMember">
                        <div class="form-group">
                            <label>Nome</label>
                            <input type="text" v-model="newMember.name" required>
                        </div>
                        <div class="form-group">
                            <label>Email</label>
                            <input type="email" v-model="newMember.email" required>
                        </div>
                        <div class="form-group">
                            <label>Funcao</label>
                            <select v-model="newMember.role">
                                <option value="developer">Desenvolvedor</option>
                                <option value="tech_lead">Tech Lead</option>
                                <option value="qa">QA</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Horas por Dia</label>
                            <input type="number" v-model="newMember.hours_per_day" min="1" max="12" step="0.5">
                        </div>
                        <div style="display: flex; gap: 1rem; justify-content: flex-end;">
                            <button type="button" class="btn btn-secondary" @click="showAddMemberModal = false">
                                Cancelar
                            </button>
                            <button type="submit" class="btn btn-primary">Adicionar</button>
                        </div>
                    </form>
                </div>
            </div>

            <!-- Modal Registrar Ausencia -->
            <div class="modal" v-if="showAbsenceModal" @click.self="showAbsenceModal = false">
                <div class="modal-content">
                    <h3 style="margin-bottom: 1.5rem; font-size: 1.25rem;">Registrar Ausencia</h3>
                    <form @submit.prevent="addAbsence">
                        <div class="form-group">
                            <label>Membro</label>
                            <select v-model="newAbsence.member_id" required>
                                <option v-for="member in teamData.members" :key="member.member_id" :value="member.member_id">
                                    {{ member.name }}
                                </option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Tipo</label>
                            <select v-model="newAbsence.absence_type">
                                <option value="vacation">Ferias</option>
                                <option value="holiday">Feriado</option>
                                <option value="sick_leave">Licenca Medica</option>
                                <option value="meeting">Reuniao</option>
                                <option value="training">Treinamento</option>
                                <option value="other">Outro</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Data Inicio</label>
                            <input type="date" v-model="newAbsence.start_date" required>
                        </div>
                        <div class="form-group">
                            <label>Data Fim</label>
                            <input type="date" v-model="newAbsence.end_date" required>
                        </div>
                        <div class="form-group">
                            <label>Horas/Dia Ausente</label>
                            <input type="number" v-model="newAbsence.hours_per_day" min="1" max="8" step="0.5">
                        </div>
                        <div class="form-group">
                            <label>Motivo (opcional)</label>
                            <input type="text" v-model="newAbsence.reason">
                        </div>
                        <div style="display: flex; gap: 1rem; justify-content: flex-end;">
                            <button type="button" class="btn btn-secondary" @click="showAbsenceModal = false">
                                Cancelar
                            </button>
                            <button type="submit" class="btn btn-primary">Registrar</button>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    </div>

    <script>
        const { createApp } = Vue;

        createApp({
            data() {
                return {
                    teamData: {},
                    forecast: {},
                    healthStatus: 'healthy',
                    alerts: [],
                    showAddMemberModal: false,
                    showAbsenceModal: false,
                    capacityChart: null,
                    newMember: {
                        name: '',
                        email: '',
                        role: 'developer',
                        hours_per_day: 8,
                        velocity_factor: 1.0
                    },
                    newAbsence: {
                        member_id: '',
                        absence_type: 'vacation',
                        start_date: '',
                        end_date: '',
                        hours_per_day: 8,
                        reason: ''
                    }
                };
            },

            mounted() {
                this.loadData();
            },

            methods: {
                async loadData() {
                    try {
                        // Carrega dados do time
                        const teamRes = await fetch('/api/capacity/team');
                        this.teamData = await teamRes.json();

                        // Carrega previsao
                        const forecastRes = await fetch('/api/capacity/forecast?sprint_id=current&committed_points=30');
                        this.forecast = await forecastRes.json();

                        // Carrega saude
                        const healthRes = await fetch('/api/capacity/health?sprint_id=current&committed_points=30');
                        const healthData = await healthRes.json();
                        this.healthStatus = healthData.overall_health;
                        this.alerts = healthData.alerts || [];

                        // Atualiza grafico
                        this.updateChart();
                    } catch (error) {
                        console.error('Erro ao carregar dados:', error);
                    }
                },

                updateChart() {
                    const ctx = document.getElementById('capacityChart');
                    if (!ctx) return;

                    if (this.capacityChart) {
                        this.capacityChart.destroy();
                    }

                    const members = this.teamData.members || [];

                    this.capacityChart = new Chart(ctx, {
                        type: 'bar',
                        data: {
                            labels: members.map(m => m.name),
                            datasets: [
                                {
                                    label: 'Horas Disponiveis',
                                    data: members.map(m => m.availability?.effective_hours || 0),
                                    backgroundColor: '#10B981'
                                },
                                {
                                    label: 'Horas Alocadas',
                                    data: members.map(m => m.availability?.allocated_hours || 0),
                                    backgroundColor: '#3B82F6'
                                }
                            ]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: {
                                legend: {
                                    position: 'top'
                                }
                            },
                            scales: {
                                y: {
                                    beginAtZero: true,
                                    title: {
                                        display: true,
                                        text: 'Horas'
                                    }
                                }
                            }
                        }
                    });
                },

                async addMember() {
                    try {
                        const res = await fetch('/api/capacity/member', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(this.newMember)
                        });

                        if (res.ok) {
                            this.showAddMemberModal = false;
                            this.newMember = { name: '', email: '', role: 'developer', hours_per_day: 8, velocity_factor: 1.0 };
                            this.loadData();
                        }
                    } catch (error) {
                        console.error('Erro ao adicionar membro:', error);
                    }
                },

                async addAbsence() {
                    try {
                        const res = await fetch('/api/capacity/absence', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(this.newAbsence)
                        });

                        if (res.ok) {
                            this.showAbsenceModal = false;
                            this.newAbsence = { member_id: '', absence_type: 'vacation', start_date: '', end_date: '', hours_per_day: 8, reason: '' };
                            this.loadData();
                        }
                    } catch (error) {
                        console.error('Erro ao registrar ausencia:', error);
                    }
                },

                getInitials(name) {
                    return name.split(' ').map(n => n[0]).join('').substring(0, 2).toUpperCase();
                },

                getRoleLabel(role) {
                    const labels = {
                        developer: 'Desenvolvedor',
                        tech_lead: 'Tech Lead',
                        qa: 'QA'
                    };
                    return labels[role] || role;
                },

                getHealthLabel(status) {
                    const labels = {
                        healthy: 'Saudavel',
                        warning: 'Atencao',
                        overcommitted: 'Sobrecarga',
                        undercommitted: 'Subaproveitado'
                    };
                    return labels[status] || status;
                },

                getProgressClass(percent) {
                    if (percent > 100) return 'progress-overcommitted';
                    if (percent > 85) return 'progress-warning';
                    return 'progress-healthy';
                }
            }
        }).mount('#app');
    </script>
</body>
</html>
'''
