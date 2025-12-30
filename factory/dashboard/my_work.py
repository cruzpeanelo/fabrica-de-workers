# -*- coding: utf-8 -*-
"""
My Work Dashboard (Issue #272)
==============================
Dashboard pessoal que consolida tudo relacionado ao usuario.

Funcionalidades:
- Lista de tasks atribuidas
- Calendario pessoal de deadlines
- Notificacoes pendentes
- Atividade recente
- Metricas pessoais
- Quick actions
"""

from fastapi import APIRouter, Query
from fastapi.responses import HTMLResponse
from typing import Optional
from datetime import datetime, timedelta
from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask, ActivityLog

router = APIRouter(prefix="/api/my-work", tags=["My Work"])


@router.get("/summary")
async def get_my_work_summary(user: str = Query("current_user")):
    """Retorna resumo do trabalho do usuario."""
    db = SessionLocal()
    try:
        today = datetime.now().date()
        week_start = today - timedelta(days=today.weekday())

        # Stories assigned to user
        assigned_stories = db.query(Story).filter(
            Story.assignee == user,
            Story.status.notin_(['done'])
        ).all()

        # Tasks assigned to user
        assigned_tasks = db.query(StoryTask).filter(
            StoryTask.assignee == user,
            StoryTask.status.notin_(['completed'])
        ).all()

        # Due this week
        due_this_week = [s for s in assigned_stories
                        if s.due_date and week_start <= s.due_date.date() <= today + timedelta(days=7)]

        # Overdue
        overdue = [s for s in assigned_stories
                   if s.due_date and s.due_date.date() < today]

        # Completed this week
        completed_stories = db.query(Story).filter(
            Story.assignee == user,
            Story.status == 'done',
            Story.updated_at >= datetime.combine(week_start, datetime.min.time())
        ).count()

        completed_tasks = db.query(StoryTask).filter(
            StoryTask.assignee == user,
            StoryTask.status == 'completed',
            StoryTask.updated_at >= datetime.combine(week_start, datetime.min.time())
        ).count()

        return {
            "assigned_stories": len(assigned_stories),
            "assigned_tasks": len(assigned_tasks),
            "due_this_week": len(due_this_week),
            "overdue": len(overdue),
            "completed_stories_this_week": completed_stories,
            "completed_tasks_this_week": completed_tasks,
            "stories": [
                {
                    "id": s.story_id,
                    "title": s.title,
                    "status": s.status.value if hasattr(s.status, 'value') else s.status,
                    "priority": s.priority.value if hasattr(s.priority, 'value') else s.priority,
                    "due_date": s.due_date.isoformat() if s.due_date else None,
                    "progress": getattr(s, 'progress', 0)
                }
                for s in assigned_stories[:10]
            ],
            "tasks": [
                {
                    "id": t.task_id,
                    "title": t.title,
                    "story_id": t.story_id,
                    "status": t.status.value if hasattr(t.status, 'value') else t.status,
                    "progress": t.progress
                }
                for t in assigned_tasks[:10]
            ]
        }

    finally:
        db.close()


@router.get("/deadlines")
async def get_my_deadlines(user: str = Query("current_user"), days: int = Query(14)):
    """Retorna deadlines proximas do usuario."""
    db = SessionLocal()
    try:
        end_date = datetime.now() + timedelta(days=days)

        stories = db.query(Story).filter(
            Story.assignee == user,
            Story.due_date <= end_date,
            Story.status.notin_(['done'])
        ).order_by(Story.due_date).all()

        return {
            "deadlines": [
                {
                    "id": s.story_id,
                    "title": s.title,
                    "due_date": s.due_date.isoformat() if s.due_date else None,
                    "days_remaining": (s.due_date.date() - datetime.now().date()).days if s.due_date else None,
                    "priority": s.priority.value if hasattr(s.priority, 'value') else s.priority
                }
                for s in stories
            ]
        }

    finally:
        db.close()


@router.get("/activity")
async def get_my_activity(user: str = Query("current_user"), limit: int = Query(20)):
    """Retorna atividade recente do usuario."""
    db = SessionLocal()
    try:
        activities = db.query(ActivityLog).filter(
            ActivityLog.user == user
        ).order_by(ActivityLog.created_at.desc()).limit(limit).all()

        return {
            "activities": [
                {
                    "id": a.id,
                    "action": a.action,
                    "entity_type": a.entity_type,
                    "entity_id": a.entity_id,
                    "timestamp": a.created_at.isoformat() if a.created_at else None,
                    "details": a.details
                }
                for a in activities
            ]
        }

    except Exception:
        return {"activities": []}
    finally:
        db.close()


@router.get("/metrics")
async def get_my_metrics(user: str = Query("current_user")):
    """Retorna metricas pessoais do usuario."""
    db = SessionLocal()
    try:
        # This week
        today = datetime.now().date()
        week_start = today - timedelta(days=today.weekday())
        last_week_start = week_start - timedelta(days=7)

        # Stories completed this week
        stories_this_week = db.query(Story).filter(
            Story.assignee == user,
            Story.status == 'done',
            Story.updated_at >= datetime.combine(week_start, datetime.min.time())
        ).count()

        # Stories completed last week
        stories_last_week = db.query(Story).filter(
            Story.assignee == user,
            Story.status == 'done',
            Story.updated_at >= datetime.combine(last_week_start, datetime.min.time()),
            Story.updated_at < datetime.combine(week_start, datetime.min.time())
        ).count()

        # Calculate velocity (story points)
        velocity_this_week = db.query(Story).filter(
            Story.assignee == user,
            Story.status == 'done',
            Story.updated_at >= datetime.combine(week_start, datetime.min.time())
        ).all()
        velocity = sum(s.story_points or 0 for s in velocity_this_week)

        return {
            "stories_this_week": stories_this_week,
            "stories_last_week": stories_last_week,
            "velocity": velocity,
            "trend": "up" if stories_this_week > stories_last_week else "down" if stories_this_week < stories_last_week else "stable"
        }

    finally:
        db.close()


def get_my_work_html():
    """Retorna o HTML do dashboard My Work."""
    return '''
    <!-- My Work Dashboard (Issue #272) -->
    <div v-if="currentTab === 'my-work'" class="my-work-dashboard p-6">
        <!-- Header -->
        <div class="flex items-center justify-between mb-6">
            <div>
                <h2 class="text-2xl font-bold text-gray-800">Meu Trabalho</h2>
                <p class="text-gray-500">Suas tarefas e progresso</p>
            </div>
            <div class="flex items-center gap-2">
                <span class="text-sm text-gray-500">{{ formatDate(new Date()) }}</span>
            </div>
        </div>

        <!-- Stats Cards -->
        <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-blue-600">{{ myWork.assigned_stories }}</div>
                <div class="text-sm text-gray-500">Stories atribuidas</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-green-600">{{ myWork.assigned_tasks }}</div>
                <div class="text-sm text-gray-500">Tasks pendentes</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-orange-600">{{ myWork.due_this_week }}</div>
                <div class="text-sm text-gray-500">Vence esta semana</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div :class="['text-3xl font-bold', myWork.overdue > 0 ? 'text-red-600' : 'text-gray-400']">
                    {{ myWork.overdue }}
                </div>
                <div class="text-sm text-gray-500">Atrasadas</div>
            </div>
        </div>

        <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
            <!-- Stories List -->
            <div class="lg:col-span-2">
                <div class="bg-white rounded-xl shadow">
                    <div class="px-4 py-3 border-b border-gray-200">
                        <h3 class="font-semibold">Minhas Stories</h3>
                    </div>
                    <div class="divide-y divide-gray-100">
                        <div v-for="story in myWork.stories" :key="story.id"
                             @click="openStoryById(story.id)"
                             class="px-4 py-3 hover:bg-gray-50 cursor-pointer flex items-center justify-between">
                            <div class="flex-1 min-w-0">
                                <div class="flex items-center gap-2">
                                    <span class="text-xs font-mono text-gray-400">{{ story.id }}</span>
                                    <span :class="'w-2 h-2 rounded-full ' + getPriorityColor(story.priority)"></span>
                                </div>
                                <div class="font-medium text-gray-800 truncate">{{ story.title }}</div>
                            </div>
                            <div class="flex items-center gap-3 ml-4">
                                <div class="w-16">
                                    <div class="h-1.5 bg-gray-200 rounded-full">
                                        <div class="h-full bg-green-500 rounded-full" :style="{width: story.progress + '%'}"></div>
                                    </div>
                                </div>
                                <span :class="'px-2 py-0.5 rounded text-xs ' + getStatusClass(story.status)">
                                    {{ story.status }}
                                </span>
                            </div>
                        </div>
                        <div v-if="myWork.stories?.length === 0" class="px-4 py-8 text-center text-gray-400">
                            Nenhuma story atribuida
                        </div>
                    </div>
                </div>

                <!-- Tasks List -->
                <div class="bg-white rounded-xl shadow mt-6">
                    <div class="px-4 py-3 border-b border-gray-200">
                        <h3 class="font-semibold">Minhas Tasks</h3>
                    </div>
                    <div class="divide-y divide-gray-100">
                        <div v-for="task in myWork.tasks" :key="task.id"
                             class="px-4 py-3 hover:bg-gray-50 flex items-center justify-between">
                            <div class="flex-1 min-w-0">
                                <div class="text-xs text-gray-400">{{ task.story_id }}</div>
                                <div class="font-medium text-gray-800 truncate">{{ task.title }}</div>
                            </div>
                            <div class="flex items-center gap-3 ml-4">
                                <span class="text-sm text-gray-500">{{ task.progress }}%</span>
                                <span :class="'px-2 py-0.5 rounded text-xs ' + getTaskStatusClass(task.status)">
                                    {{ task.status }}
                                </span>
                            </div>
                        </div>
                        <div v-if="myWork.tasks?.length === 0" class="px-4 py-8 text-center text-gray-400">
                            Nenhuma task pendente
                        </div>
                    </div>
                </div>
            </div>

            <!-- Sidebar -->
            <div class="space-y-6">
                <!-- Deadlines -->
                <div class="bg-white rounded-xl shadow">
                    <div class="px-4 py-3 border-b border-gray-200">
                        <h3 class="font-semibold">Proximos Prazos</h3>
                    </div>
                    <div class="p-4 space-y-3">
                        <div v-for="deadline in myDeadlines" :key="deadline.id"
                             class="flex items-center justify-between">
                            <div class="truncate flex-1">
                                <div class="font-medium text-sm">{{ deadline.title }}</div>
                            </div>
                            <div :class="['text-sm font-medium',
                                          deadline.days_remaining < 0 ? 'text-red-600' :
                                          deadline.days_remaining <= 2 ? 'text-orange-600' : 'text-gray-500']">
                                {{ formatDeadline(deadline.days_remaining) }}
                            </div>
                        </div>
                        <div v-if="myDeadlines?.length === 0" class="text-center text-gray-400 text-sm py-4">
                            Sem prazos proximos
                        </div>
                    </div>
                </div>

                <!-- This Week Progress -->
                <div class="bg-white rounded-xl shadow">
                    <div class="px-4 py-3 border-b border-gray-200">
                        <h3 class="font-semibold">Esta Semana</h3>
                    </div>
                    <div class="p-4">
                        <div class="flex items-center justify-between mb-4">
                            <span class="text-sm text-gray-500">Stories completadas</span>
                            <span class="text-lg font-bold text-green-600">{{ myWork.completed_stories_this_week }}</span>
                        </div>
                        <div class="flex items-center justify-between mb-4">
                            <span class="text-sm text-gray-500">Tasks completadas</span>
                            <span class="text-lg font-bold text-green-600">{{ myWork.completed_tasks_this_week }}</span>
                        </div>
                        <div class="flex items-center justify-between">
                            <span class="text-sm text-gray-500">Velocity</span>
                            <span class="text-lg font-bold text-blue-600">{{ myMetrics.velocity }} pts</span>
                        </div>
                    </div>
                </div>

                <!-- Quick Actions -->
                <div class="bg-white rounded-xl shadow">
                    <div class="px-4 py-3 border-b border-gray-200">
                        <h3 class="font-semibold">Acoes Rapidas</h3>
                    </div>
                    <div class="p-4 space-y-2">
                        <button @click="showNewStoryModal = true"
                                class="w-full text-left px-3 py-2 rounded-lg hover:bg-gray-100 flex items-center gap-2 text-sm">
                            <svg class="w-4 h-4 text-blue-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/>
                            </svg>
                            Nova Story
                        </button>
                        <button @click="showGamificationPanel = true"
                                class="w-full text-left px-3 py-2 rounded-lg hover:bg-gray-100 flex items-center gap-2 text-sm">
                            <span class="text-base">🏆</span>
                            Ver Conquistas
                        </button>
                        <button @click="showGlobalSearch = true"
                                class="w-full text-left px-3 py-2 rounded-lg hover:bg-gray-100 flex items-center gap-2 text-sm">
                            <svg class="w-4 h-4 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                            </svg>
                            Buscar (Ctrl+K)
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>
    '''


def get_my_work_js():
    """Retorna o codigo JavaScript para My Work."""
    return '''
    // My Work State (Issue #272)
    const myWork = ref({
        assigned_stories: 0,
        assigned_tasks: 0,
        due_this_week: 0,
        overdue: 0,
        completed_stories_this_week: 0,
        completed_tasks_this_week: 0,
        stories: [],
        tasks: []
    });
    const myDeadlines = ref([]);
    const myMetrics = ref({ velocity: 0, trend: 'stable' });

    const loadMyWork = async () => {
        try {
            const [summaryRes, deadlinesRes, metricsRes] = await Promise.all([
                fetch('/api/my-work/summary'),
                fetch('/api/my-work/deadlines'),
                fetch('/api/my-work/metrics')
            ]);

            if (summaryRes.ok) myWork.value = await summaryRes.json();
            if (deadlinesRes.ok) myDeadlines.value = (await deadlinesRes.json()).deadlines;
            if (metricsRes.ok) myMetrics.value = await metricsRes.json();
        } catch (e) {
            console.error('Failed to load my work:', e);
        }
    };

    const formatDeadline = (days) => {
        if (days < 0) return Math.abs(days) + 'd atrasado';
        if (days === 0) return 'Hoje';
        if (days === 1) return 'Amanha';
        return 'Em ' + days + ' dias';
    };

    const getPriorityColor = (priority) => {
        const colors = {
            'urgent': 'bg-red-500',
            'high': 'bg-orange-500',
            'medium': 'bg-yellow-500',
            'low': 'bg-green-500'
        };
        return colors[priority] || 'bg-gray-400';
    };

    const getTaskStatusClass = (status) => {
        const classes = {
            'pending': 'bg-gray-100 text-gray-700',
            'in_progress': 'bg-blue-100 text-blue-700',
            'completed': 'bg-green-100 text-green-700',
            'blocked': 'bg-red-100 text-red-700'
        };
        return classes[status] || 'bg-gray-100 text-gray-700';
    };

    // Load my work when switching to tab
    watch(currentTab, (tab) => {
        if (tab === 'my-work') loadMyWork();
    });
    '''


def register_my_work(app):
    """Registra os endpoints de My Work no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] My Work endpoints loaded: /api/my-work/*")
