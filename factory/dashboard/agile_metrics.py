# -*- coding: utf-8 -*-
"""
Agile Metrics Dashboard Module (Issue #258)
===========================================
Dashboard de metricas Agile com visualizacoes Chart.js.

Funcionalidades:
- Velocity chart por sprint
- Burndown/Burnup charts
- Cycle time distribution
- Lead time metrics
- Throughput analysis
- Story points distribution
- Team performance
- Sprint reports
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from collections import defaultdict

router = APIRouter(prefix="/api/metrics", tags=["Agile Metrics"])


@router.get("/velocity")
async def get_velocity_metrics(
    project_id: str = Query(...),
    sprints: int = Query(6, ge=1, le=20)
):
    """Retorna metricas de velocity por sprint usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story, Sprint

    db = SessionLocal()
    try:
        # Issue #405: Get real sprints from database
        sprint_records = db.query(Sprint).filter(
            Sprint.project_id == project_id
        ).order_by(Sprint.created_at.desc()).limit(sprints).all()

        # Reverse to show oldest first
        sprint_records = list(reversed(sprint_records))

        velocity_data = []
        for sprint in sprint_records:
            # Get stories for this sprint
            sprint_stories = db.query(Story).filter(
                Story.project_id == project_id,
                Story.sprint_id == sprint.sprint_id
            ).all()

            # Calculate planned (all stories in sprint) and completed (done stories)
            planned = sum(s.story_points or 0 for s in sprint_stories)
            completed = sum(s.story_points or 0 for s in sprint_stories if s.status == 'done')

            velocity_data.append({
                "sprint": sprint.name,
                "sprint_id": sprint.sprint_id,
                "planned": planned,
                "completed": completed,
                "stories_count": len(sprint_stories),
                "stories_done": len([s for s in sprint_stories if s.status == 'done']),
                "commitment_rate": round(completed / planned * 100, 1) if planned > 0 else 0
            })

        # If no sprints found, get velocity from stories without sprint (grouped by month)
        if not velocity_data:
            stories = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == 'done'
            ).order_by(Story.updated_at.desc()).limit(50).all()

            # Group by month
            monthly_data = defaultdict(lambda: {"planned": 0, "completed": 0})
            for story in stories:
                if story.updated_at:
                    month_key = story.updated_at.strftime("%Y-%m")
                    monthly_data[month_key]["completed"] += story.story_points or 0
                    monthly_data[month_key]["planned"] += story.story_points or 0

            for month, data in sorted(monthly_data.items())[-sprints:]:
                velocity_data.append({
                    "sprint": month,
                    "planned": data["planned"],
                    "completed": data["completed"],
                    "commitment_rate": 100.0  # All shown are completed
                })

        # Calculate averages
        avg_planned = sum(d["planned"] for d in velocity_data) / len(velocity_data) if velocity_data else 0
        avg_completed = sum(d["completed"] for d in velocity_data) / len(velocity_data) if velocity_data else 0

        # Determine trend
        trend = "stable"
        if len(velocity_data) >= 2:
            if velocity_data[-1]["completed"] > velocity_data[0]["completed"]:
                trend = "up"
            elif velocity_data[-1]["completed"] < velocity_data[0]["completed"]:
                trend = "down"

        return {
            "velocity_data": velocity_data,
            "average_velocity": round(avg_completed, 1),
            "average_planned": round(avg_planned, 1),
            "commitment_rate": round(avg_completed / avg_planned * 100, 1) if avg_planned > 0 else 0,
            "trend": trend,
            "sprints_found": len(sprint_records)
        }
    finally:
        db.close()


@router.get("/burndown")
async def get_burndown_chart(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna dados do burndown chart usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Sprint, Story

    db = SessionLocal()
    try:
        today = datetime.now()

        # Issue #405: Get real sprint data
        sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()

        if sprint and sprint.start_date and sprint.end_date:
            sprint_start = sprint.start_date if isinstance(sprint.start_date, datetime) else datetime.combine(sprint.start_date, datetime.min.time())
            sprint_end = sprint.end_date if isinstance(sprint.end_date, datetime) else datetime.combine(sprint.end_date, datetime.min.time())
        else:
            # Default to 2-week sprint ending in 4 days
            sprint_start = today - timedelta(days=10)
            sprint_end = today + timedelta(days=4)

        total_days = (sprint_end - sprint_start).days
        if total_days <= 0:
            total_days = 14

        # Get stories for this sprint
        sprint_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.sprint_id == sprint_id
        ).all()

        total_points = sum(s.story_points or 0 for s in sprint_stories)

        # Get completed stories with their completion dates
        done_stories = [s for s in sprint_stories if s.status == 'done']

        burndown_data = []
        for i in range(total_days + 1):
            date = sprint_start + timedelta(days=i)
            ideal = total_points * (1 - i / total_days) if total_days > 0 else 0

            if date <= today:
                # Calculate actual remaining points based on stories completed by this date
                completed_by_date = sum(
                    s.story_points or 0 for s in done_stories
                    if s.updated_at and s.updated_at.date() <= date.date()
                )
                actual = total_points - completed_by_date
            else:
                actual = None

            burndown_data.append({
                "date": date.strftime("%Y-%m-%d"),
                "ideal": round(ideal, 1),
                "actual": round(actual, 1) if actual is not None else None
            })

        # Calculate remaining points
        remaining = burndown_data[-1]["actual"] if burndown_data and burndown_data[-1]["actual"] is not None else None
        if remaining is None and len(burndown_data) > 1:
            for d in reversed(burndown_data):
                if d["actual"] is not None:
                    remaining = d["actual"]
                    break

        # Check if on track (actual <= ideal for today)
        on_track = True
        for d in burndown_data:
            if d["actual"] is not None and d["ideal"] is not None:
                if d["actual"] > d["ideal"] * 1.1:  # 10% tolerance
                    on_track = False

        return {
            "sprint_id": sprint_id,
            "sprint_name": sprint.name if sprint else "Sprint",
            "total_points": total_points,
            "completed_points": sum(s.story_points or 0 for s in done_stories),
            "remaining_points": remaining or 0,
            "days_remaining": max(0, (sprint_end - today).days),
            "burndown_data": burndown_data,
            "on_track": on_track,
            "stories_count": len(sprint_stories),
            "stories_done": len(done_stories)
        }
    finally:
        db.close()


@router.get("/burnup")
async def get_burnup_chart(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna dados do burnup chart usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Sprint, Story

    db = SessionLocal()
    try:
        today = datetime.now()

        # Issue #405: Get real sprint data
        sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()

        if sprint and sprint.start_date and sprint.end_date:
            sprint_start = sprint.start_date if isinstance(sprint.start_date, datetime) else datetime.combine(sprint.start_date, datetime.min.time())
            sprint_end = sprint.end_date if isinstance(sprint.end_date, datetime) else datetime.combine(sprint.end_date, datetime.min.time())
        else:
            sprint_start = today - timedelta(days=10)
            sprint_end = today + timedelta(days=4)

        total_days = (sprint_end - sprint_start).days
        if total_days <= 0:
            total_days = 14

        # Get all stories for this sprint
        sprint_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.sprint_id == sprint_id
        ).all()

        # Track scope changes - stories added after sprint start
        scope_changes = 0
        initial_scope = 0
        for story in sprint_stories:
            if story.created_at and story.created_at > sprint_start:
                scope_changes += 1
            else:
                initial_scope += story.story_points or 0

        current_scope = sum(s.story_points or 0 for s in sprint_stories)
        done_stories = [s for s in sprint_stories if s.status == 'done']

        burnup_data = []
        for i in range(total_days + 1):
            date = sprint_start + timedelta(days=i)

            # Calculate scope at this date (stories added by this date)
            scope_at_date = sum(
                s.story_points or 0 for s in sprint_stories
                if not s.created_at or s.created_at.date() <= date.date()
            )

            if date <= today:
                # Calculate completed points by this date
                completed = sum(
                    s.story_points or 0 for s in done_stories
                    if s.updated_at and s.updated_at.date() <= date.date()
                )
            else:
                completed = None

            burnup_data.append({
                "date": date.strftime("%Y-%m-%d"),
                "scope": scope_at_date,
                "completed": completed
            })

        return {
            "sprint_id": sprint_id,
            "sprint_name": sprint.name if sprint else "Sprint",
            "initial_scope": initial_scope,
            "current_scope": current_scope,
            "scope_changes": scope_changes,
            "completed_points": sum(s.story_points or 0 for s in done_stories),
            "burnup_data": burnup_data
        }
    finally:
        db.close()


@router.get("/cycle-time")
async def get_cycle_time_metrics(
    project_id: str = Query(...),
    days: int = Query(30, ge=7, le=90)
):
    """Retorna metricas de cycle time."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        cutoff = datetime.now() - timedelta(days=days)
        stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == "done",
            Story.updated_at >= cutoff
        ).all()

        cycle_times = []
        for story in stories:
            if story.created_at and story.updated_at:
                ct = (story.updated_at - story.created_at).days
                cycle_times.append({
                    "story_id": story.story_id,
                    "title": story.title,
                    "cycle_time": ct,
                    "story_points": story.story_points or 0
                })

        # Calculate distribution
        if cycle_times:
            avg_ct = sum(c["cycle_time"] for c in cycle_times) / len(cycle_times)
            sorted_ct = sorted(c["cycle_time"] for c in cycle_times)
            median_ct = sorted_ct[len(sorted_ct) // 2]
            min_ct = sorted_ct[0]
            max_ct = sorted_ct[-1]

            # Distribution buckets
            distribution = defaultdict(int)
            for ct in sorted_ct:
                if ct <= 1:
                    bucket = "1 day"
                elif ct <= 3:
                    bucket = "2-3 days"
                elif ct <= 5:
                    bucket = "4-5 days"
                elif ct <= 7:
                    bucket = "6-7 days"
                else:
                    bucket = "8+ days"
                distribution[bucket] += 1
        else:
            avg_ct = median_ct = min_ct = max_ct = 0
            distribution = {}

        return {
            "period_days": days,
            "total_stories": len(cycle_times),
            "average_cycle_time": round(avg_ct, 1),
            "median_cycle_time": median_ct,
            "min_cycle_time": min_ct,
            "max_cycle_time": max_ct,
            "distribution": dict(distribution),
            "stories": cycle_times[:20]  # Top 20
        }
    finally:
        db.close()


@router.get("/lead-time")
async def get_lead_time_metrics(
    project_id: str = Query(...),
    days: int = Query(30)
):
    """Retorna metricas de lead time (backlog to done) usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        cutoff = datetime.now() - timedelta(days=days)

        # Issue #405: Get real stories that were completed
        stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == "done",
            Story.updated_at >= cutoff
        ).all()

        # Calculate lead times
        lead_times = []
        by_priority = defaultdict(list)
        by_points = defaultdict(list)

        for story in stories:
            if story.created_at and story.updated_at:
                lt = (story.updated_at - story.created_at).days
                lead_times.append(lt)

                # Group by priority
                priority = story.priority or "medium"
                by_priority[priority].append(lt)

                # Group by story points
                points = story.story_points or 0
                if points <= 2:
                    by_points["1-2"].append(lt)
                elif points <= 5:
                    by_points["3-5"].append(lt)
                elif points <= 13:
                    by_points["8-13"].append(lt)
                else:
                    by_points["21+"].append(lt)

        # Calculate averages
        avg_lt = sum(lead_times) / len(lead_times) if lead_times else 0
        sorted_lt = sorted(lead_times) if lead_times else [0]
        median_lt = sorted_lt[len(sorted_lt) // 2] if sorted_lt else 0

        # Average by priority
        priority_avg = {}
        for priority, times in by_priority.items():
            priority_avg[priority] = round(sum(times) / len(times), 1) if times else 0

        # Average by story points
        points_avg = {}
        for points_range, times in by_points.items():
            points_avg[points_range] = round(sum(times) / len(times), 1) if times else 0

        return {
            "period_days": days,
            "total_stories": len(stories),
            "average_lead_time": round(avg_lt, 1),
            "median_lead_time": median_lt,
            "min_lead_time": min(lead_times) if lead_times else 0,
            "max_lead_time": max(lead_times) if lead_times else 0,
            "by_priority": priority_avg,
            "by_story_points": points_avg
        }
    finally:
        db.close()


@router.get("/throughput")
async def get_throughput_metrics(
    project_id: str = Query(...),
    weeks: int = Query(8, ge=4, le=26)
):
    """Retorna metricas de throughput (stories por semana)."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        throughput_data = []
        today = datetime.now()

        for i in range(weeks):
            week_start = today - timedelta(weeks=weeks - i)
            week_end = week_start + timedelta(days=7)

            count = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == "done",
                Story.updated_at >= week_start,
                Story.updated_at < week_end
            ).count()

            throughput_data.append({
                "week": f"Week {i + 1}",
                "start_date": week_start.strftime("%Y-%m-%d"),
                "stories_completed": count
            })

        avg_throughput = sum(d["stories_completed"] for d in throughput_data) / len(throughput_data) if throughput_data else 0

        return {
            "throughput_data": throughput_data,
            "average_throughput": round(avg_throughput, 1),
            "total_completed": sum(d["stories_completed"] for d in throughput_data)
        }
    finally:
        db.close()


@router.get("/story-points-distribution")
async def get_story_points_distribution(project_id: str = Query(...)):
    """Retorna distribuicao de story points."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        stories = db.query(Story).filter(Story.project_id == project_id).all()

        distribution = defaultdict(int)
        for story in stories:
            points = story.story_points or 0
            distribution[str(points)] += 1

        # Sort by point value
        sorted_dist = dict(sorted(distribution.items(), key=lambda x: int(x[0]) if x[0].isdigit() else 0))

        return {
            "distribution": sorted_dist,
            "total_stories": len(stories),
            "total_points": sum(s.story_points or 0 for s in stories)
        }
    finally:
        db.close()


@router.get("/team-performance")
async def get_team_performance(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """Retorna metricas de performance do time."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        query = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == "done"
        )

        stories = query.all()

        # Group by assignee
        by_assignee = defaultdict(lambda: {"stories": 0, "points": 0})
        for story in stories:
            assignee = story.assignee or "Unassigned"
            by_assignee[assignee]["stories"] += 1
            by_assignee[assignee]["points"] += story.story_points or 0

        team_data = [
            {
                "name": name,
                "stories_completed": data["stories"],
                "points_completed": data["points"]
            }
            for name, data in by_assignee.items()
        ]

        # Sort by points
        team_data.sort(key=lambda x: x["points_completed"], reverse=True)

        return {
            "team_data": team_data,
            "total_team_members": len(team_data),
            "total_stories": len(stories),
            "total_points": sum(s.story_points or 0 for s in stories)
        }
    finally:
        db.close()


@router.get("/cumulative-flow")
async def get_cumulative_flow(
    project_id: str = Query(...),
    days: int = Query(30)
):
    """Retorna dados do Cumulative Flow Diagram (CFD) usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
        today = datetime.now()

        # Issue #405: Get all stories for the project
        all_stories = db.query(Story).filter(
            Story.project_id == project_id
        ).all()

        cfd_data = []
        for i in range(days):
            date = today - timedelta(days=days - 1 - i)
            date_end = date.replace(hour=23, minute=59, second=59)

            # Count stories by status at this point in time
            # Stories that existed by this date (created_at <= date)
            stories_at_date = [
                s for s in all_stories
                if s.created_at and s.created_at <= date_end
            ]

            # For cumulative flow, we approximate based on:
            # - Stories created before date count towards their current status
            # - Done stories that were updated after this date aren't counted as done yet
            status_counts = defaultdict(int)
            for story in stories_at_date:
                if story.status == 'done' and story.updated_at and story.updated_at > date_end:
                    # Story wasn't done yet at this date - put in previous state
                    status_counts["in_progress"] += 1
                else:
                    status_counts[story.status or "backlog"] += 1

            data_point = {
                "date": date.strftime("%Y-%m-%d"),
            }
            for status in statuses:
                data_point[status] = status_counts.get(status, 0)

            cfd_data.append(data_point)

        # Calculate current totals
        current_totals = defaultdict(int)
        for story in all_stories:
            current_totals[story.status or "backlog"] += 1

        return {
            "cfd_data": cfd_data,
            "statuses": statuses,
            "current_totals": dict(current_totals),
            "total_stories": len(all_stories)
        }
    finally:
        db.close()


@router.get("/sprint-report")
async def get_sprint_report(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna relatorio completo do sprint usando dados reais."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Sprint, Story

    db = SessionLocal()
    try:
        # Issue #405: Get real sprint data
        sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()

        # Get stories for this sprint
        sprint_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.sprint_id == sprint_id
        ).all()

        # Calculate summary
        planned_points = sum(s.story_points or 0 for s in sprint_stories)
        done_stories = [s for s in sprint_stories if s.status == 'done']
        completed_points = sum(s.story_points or 0 for s in done_stories)

        # Count stories added after sprint start
        stories_added = 0
        if sprint and sprint.start_date:
            sprint_start = sprint.start_date if isinstance(sprint.start_date, datetime) else datetime.combine(sprint.start_date, datetime.min.time())
            stories_added = sum(1 for s in sprint_stories if s.created_at and s.created_at > sprint_start)

        # Get velocity, burndown, and cycle time using the other endpoints
        velocity = await get_velocity_metrics(project_id, sprints=1)
        burndown = await get_burndown_chart(sprint_id, project_id)
        cycle_time = await get_cycle_time_metrics(project_id, days=14)

        # Calculate commitment rate
        commitment_rate = round(completed_points / planned_points * 100, 1) if planned_points > 0 else 0

        # Sprint dates
        start_date = None
        end_date = None
        if sprint:
            if sprint.start_date:
                start_date = sprint.start_date.strftime("%Y-%m-%d") if hasattr(sprint.start_date, 'strftime') else str(sprint.start_date)
            if sprint.end_date:
                end_date = sprint.end_date.strftime("%Y-%m-%d") if hasattr(sprint.end_date, 'strftime') else str(sprint.end_date)

        return {
            "sprint_id": sprint_id,
            "sprint_name": sprint.name if sprint else "Sprint",
            "goal": sprint.goal if sprint else None,
            "dates": {
                "start": start_date,
                "end": end_date
            },
            "summary": {
                "planned_points": planned_points,
                "completed_points": completed_points,
                "remaining_points": planned_points - completed_points,
                "commitment_rate": commitment_rate,
                "stories_total": len(sprint_stories),
                "stories_completed": len(done_stories),
                "stories_in_progress": len([s for s in sprint_stories if s.status == 'in_progress']),
                "stories_added": stories_added
            },
            "velocity": velocity,
            "burndown": burndown,
            "cycle_time": cycle_time,
            "stories": [
                {
                    "story_id": s.story_id,
                    "title": s.title,
                    "status": s.status,
                    "story_points": s.story_points,
                    "assignee": s.assignee
                }
                for s in sprint_stories
            ]
        }
    finally:
        db.close()


def get_metrics_dashboard_html():
    """Retorna o HTML do dashboard de metricas."""
    return '''
    <!-- Agile Metrics Dashboard (Issue #258) -->
    <div v-if="currentTab === 'metrics'" class="metrics-dashboard p-6">
        <!-- Header -->
        <div class="flex items-center justify-between mb-6">
            <div>
                <h2 class="text-2xl font-bold">Metricas Agile</h2>
                <p class="text-gray-500">Analise de performance e tendencias</p>
            </div>
            <div class="flex gap-2">
                <select v-model="metricsSprintFilter" @change="loadMetrics"
                        class="px-3 py-2 border border-gray-300 rounded-lg">
                    <option value="">Todos Sprints</option>
                    <option v-for="s in sprints" :key="s.id" :value="s.id">{{ s.name }}</option>
                </select>
                <button @click="exportMetrics"
                        class="px-4 py-2 bg-gray-100 hover:bg-gray-200 rounded-lg">
                    Exportar
                </button>
            </div>
        </div>

        <!-- KPI Cards -->
        <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-blue-600">{{ metricsData.velocity?.average_velocity || 0 }}</div>
                <div class="text-sm text-gray-500">Velocity Medio</div>
                <div class="text-xs mt-1" :class="metricsData.velocity?.trend === 'up' ? 'text-green-600' : 'text-gray-400'">
                    {{ metricsData.velocity?.trend === 'up' ? '↑ Crescendo' : '→ Estavel' }}
                </div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-green-600">{{ metricsData.velocity?.commitment_rate || 0 }}%</div>
                <div class="text-sm text-gray-500">Taxa de Entrega</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-purple-600">{{ metricsData.cycleTime?.average_cycle_time || 0 }}d</div>
                <div class="text-sm text-gray-500">Cycle Time Medio</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-orange-600">{{ metricsData.throughput?.average_throughput || 0 }}</div>
                <div class="text-sm text-gray-500">Throughput/Semana</div>
            </div>
        </div>

        <!-- Charts Row 1 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
            <!-- Velocity Chart -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Velocity por Sprint</h3>
                <canvas ref="velocityChart" height="200"></canvas>
            </div>

            <!-- Burndown Chart -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Burndown Chart</h3>
                <canvas ref="burndownChart" height="200"></canvas>
            </div>
        </div>

        <!-- Charts Row 2 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
            <!-- Cycle Time Distribution -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Distribuicao de Cycle Time</h3>
                <canvas ref="cycleTimeChart" height="200"></canvas>
            </div>

            <!-- Throughput Trend -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Throughput Semanal</h3>
                <canvas ref="throughputChart" height="200"></canvas>
            </div>
        </div>

        <!-- Charts Row 3 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <!-- Story Points Distribution -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Distribuicao de Story Points</h3>
                <canvas ref="pointsChart" height="200"></canvas>
            </div>

            <!-- Team Performance -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Performance do Time</h3>
                <div class="space-y-3">
                    <div v-for="member in metricsData.teamPerformance?.team_data?.slice(0, 5)" :key="member.name"
                         class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <div class="w-8 h-8 bg-blue-100 rounded-full flex items-center justify-center text-blue-600 font-semibold text-sm">
                                {{ member.name.charAt(0) }}
                            </div>
                            <span>{{ member.name }}</span>
                        </div>
                        <div class="flex gap-4 text-sm">
                            <span class="text-gray-500">{{ member.stories_completed }} stories</span>
                            <span class="font-semibold">{{ member.points_completed }} pts</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
    '''


def get_metrics_js():
    """Retorna o JavaScript para o dashboard de metricas."""
    return '''
    // Metrics State
    metricsData: {
        velocity: null,
        burndown: null,
        cycleTime: null,
        throughput: null,
        pointsDistribution: null,
        teamPerformance: null
    },
    metricsSprintFilter: '',
    metricsCharts: {},

    // Metrics Methods
    async loadMetrics() {
        const projectId = this.currentProject;
        const sprintId = this.metricsSprintFilter || 'current';

        try {
            const [velocity, burndown, cycleTime, throughput, points, team] = await Promise.all([
                fetch(`/api/metrics/velocity?project_id=${projectId}&sprints=6`).then(r => r.json()),
                fetch(`/api/metrics/burndown?project_id=${projectId}&sprint_id=${sprintId}`).then(r => r.json()),
                fetch(`/api/metrics/cycle-time?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/throughput?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/story-points-distribution?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/team-performance?project_id=${projectId}`).then(r => r.json())
            ]);

            this.metricsData = { velocity, burndown, cycleTime, throughput, pointsDistribution: points, teamPerformance: team };

            this.$nextTick(() => this.renderMetricsCharts());
        } catch (e) {
            console.error('Error loading metrics:', e);
        }
    },

    renderMetricsCharts() {
        // Velocity Chart
        this.renderChart('velocityChart', {
            type: 'bar',
            data: {
                labels: this.metricsData.velocity?.velocity_data?.map(d => d.sprint) || [],
                datasets: [
                    {
                        label: 'Planejado',
                        data: this.metricsData.velocity?.velocity_data?.map(d => d.planned) || [],
                        backgroundColor: 'rgba(59, 130, 246, 0.3)',
                        borderColor: 'rgb(59, 130, 246)',
                        borderWidth: 1
                    },
                    {
                        label: 'Entregue',
                        data: this.metricsData.velocity?.velocity_data?.map(d => d.completed) || [],
                        backgroundColor: 'rgba(16, 185, 129, 0.7)',
                        borderColor: 'rgb(16, 185, 129)',
                        borderWidth: 1
                    }
                ]
            },
            options: { responsive: true, plugins: { legend: { position: 'bottom' } } }
        });

        // Burndown Chart
        const burndownData = this.metricsData.burndown?.burndown_data || [];
        this.renderChart('burndownChart', {
            type: 'line',
            data: {
                labels: burndownData.map(d => d.date),
                datasets: [
                    {
                        label: 'Ideal',
                        data: burndownData.map(d => d.ideal),
                        borderColor: 'rgb(156, 163, 175)',
                        borderDash: [5, 5],
                        fill: false
                    },
                    {
                        label: 'Atual',
                        data: burndownData.map(d => d.actual),
                        borderColor: 'rgb(59, 130, 246)',
                        backgroundColor: 'rgba(59, 130, 246, 0.1)',
                        fill: true
                    }
                ]
            },
            options: { responsive: true, plugins: { legend: { position: 'bottom' } } }
        });

        // Cycle Time Chart
        const cycleDistribution = this.metricsData.cycleTime?.distribution || {};
        this.renderChart('cycleTimeChart', {
            type: 'doughnut',
            data: {
                labels: Object.keys(cycleDistribution),
                datasets: [{
                    data: Object.values(cycleDistribution),
                    backgroundColor: ['#10B981', '#3B82F6', '#F59E0B', '#EF4444', '#6B7280']
                }]
            },
            options: { responsive: true, plugins: { legend: { position: 'right' } } }
        });

        // Throughput Chart
        const throughputData = this.metricsData.throughput?.throughput_data || [];
        this.renderChart('throughputChart', {
            type: 'line',
            data: {
                labels: throughputData.map(d => d.week),
                datasets: [{
                    label: 'Stories Completas',
                    data: throughputData.map(d => d.stories_completed),
                    borderColor: 'rgb(139, 92, 246)',
                    backgroundColor: 'rgba(139, 92, 246, 0.1)',
                    fill: true,
                    tension: 0.4
                }]
            },
            options: { responsive: true }
        });

        // Points Distribution Chart
        const pointsDist = this.metricsData.pointsDistribution?.distribution || {};
        this.renderChart('pointsChart', {
            type: 'bar',
            data: {
                labels: Object.keys(pointsDist),
                datasets: [{
                    label: 'Stories',
                    data: Object.values(pointsDist),
                    backgroundColor: 'rgba(245, 158, 11, 0.7)'
                }]
            },
            options: { responsive: true, plugins: { legend: { display: false } } }
        });
    },

    renderChart(refName, config) {
        const canvas = this.$refs[refName];
        if (!canvas) return;

        // Destroy existing chart
        if (this.metricsCharts[refName]) {
            this.metricsCharts[refName].destroy();
        }

        // Create new chart
        this.metricsCharts[refName] = new Chart(canvas.getContext('2d'), config);
    },

    async exportMetrics() {
        const projectId = this.currentProject;
        const response = await fetch(`/api/metrics/sprint-report?project_id=${projectId}&sprint_id=current`);
        const report = await response.json();

        // Download as JSON
        const blob = new Blob([JSON.stringify(report, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `metrics_report_${new Date().toISOString().split('T')[0]}.json`;
        a.click();
    }
    '''


def register_agile_metrics(app):
    """Registra os endpoints de metricas no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Agile Metrics endpoints loaded: /api/metrics/*")
