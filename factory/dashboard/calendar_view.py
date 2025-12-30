# -*- coding: utf-8 -*-
"""
Calendar View Module (Issue #267)
=================================
Visualizacao alternativa do Kanban em formato de calendario.

Funcionalidades:
- Visualizacao mensal e semanal
- Stories posicionadas por deadline
- Drag-drop para alterar datas
- Cores por status/prioridade
- Filtros por assignee/epic
"""

from fastapi import APIRouter, Query
from fastapi.responses import HTMLResponse
from typing import Optional, List
from datetime import datetime, timedelta
from factory.database.connection import SessionLocal
from factory.database.models import Story, Sprint

router = APIRouter(prefix="/api/calendar", tags=["Calendar View"])


@router.get("/stories")
async def get_calendar_stories(
    start_date: Optional[str] = Query(None, description="Data inicial (YYYY-MM-DD). Default: inicio do mes atual"),
    end_date: Optional[str] = Query(None, description="Data final (YYYY-MM-DD). Default: fim do mes atual"),
    project_id: Optional[str] = Query(None),
    assignee: Optional[str] = Query(None),
    epic_id: Optional[str] = Query(None)
):
    """
    Retorna stories para exibicao no calendario.
    Se start_date/end_date nao forem informados, usa o mes atual.
    """
    db = SessionLocal()
    try:
        # Default to current month if dates not provided
        today = datetime.now()
        if not start_date:
            start = today.replace(day=1, hour=0, minute=0, second=0, microsecond=0)
        else:
            start = datetime.strptime(start_date, "%Y-%m-%d")

        if not end_date:
            # Last day of current month
            if today.month == 12:
                end = today.replace(year=today.year + 1, month=1, day=1) - timedelta(days=1)
            else:
                end = today.replace(month=today.month + 1, day=1) - timedelta(days=1)
            end = end.replace(hour=23, minute=59, second=59)
        else:
            end = datetime.strptime(end_date, "%Y-%m-%d")

        query = db.query(Story)

        if project_id:
            query = query.filter(Story.project_id == project_id)
        if assignee:
            query = query.filter(Story.assignee == assignee)
        if epic_id:
            query = query.filter(Story.epic_id == epic_id)

        # Filter by due_date or created_at range
        query = query.filter(
            ((Story.due_date >= start) & (Story.due_date <= end)) |
            ((Story.created_at >= start) & (Story.created_at <= end))
        )

        stories = query.all()

        calendar_events = []
        for story in stories:
            event_date = story.due_date if story.due_date else story.created_at

            calendar_events.append({
                "id": story.story_id,
                "title": story.title,
                "date": event_date.strftime("%Y-%m-%d") if event_date else None,
                "status": story.status.value if hasattr(story.status, 'value') else story.status,
                "priority": story.priority.value if hasattr(story.priority, 'value') else story.priority,
                "story_points": story.story_points,
                "assignee": story.assignee,
                "epic_id": story.epic_id,
                "progress": getattr(story, 'progress', 0)
            })

        return {
            "events": calendar_events,
            "total": len(calendar_events)
        }

    finally:
        db.close()


@router.get("/sprints")
async def get_calendar_sprints(project_id: Optional[str] = Query(None)):
    """
    Retorna sprints para exibicao no calendario.
    """
    db = SessionLocal()
    try:
        query = db.query(Sprint)
        if project_id:
            query = query.filter(Sprint.project_id == project_id)

        sprints = query.all()

        sprint_events = []
        for sprint in sprints:
            if sprint.start_date and sprint.end_date:
                sprint_events.append({
                    "id": sprint.sprint_id,
                    "name": sprint.name,
                    "start_date": sprint.start_date.strftime("%Y-%m-%d"),
                    "end_date": sprint.end_date.strftime("%Y-%m-%d"),
                    "goal": sprint.goal,
                    "type": "sprint"
                })

        return {"sprints": sprint_events}

    finally:
        db.close()


@router.patch("/story/{story_id}/date")
async def update_story_date(story_id: str, new_date: str):
    """
    Atualiza a data de uma story (drag-drop no calendario).
    """
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            return {"success": False, "error": "Story nao encontrada"}

        story.due_date = datetime.strptime(new_date, "%Y-%m-%d")
        db.commit()

        return {
            "success": True,
            "story_id": story_id,
            "new_date": new_date
        }

    except Exception as e:
        db.rollback()
        return {"success": False, "error": str(e)}
    finally:
        db.close()


def get_calendar_view_html():
    """Retorna o HTML do componente de calendario."""
    return '''
    <!-- Calendar View Component (Issue #267) -->
    <div v-if="viewMode === 'calendar'" class="calendar-container p-4">
        <!-- Calendar Header -->
        <div class="flex items-center justify-between mb-4">
            <div class="flex items-center gap-2">
                <button @click="calendarPrev" class="p-2 hover:bg-gray-100 rounded">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7"/>
                    </svg>
                </button>
                <h3 class="text-lg font-semibold">{{ calendarTitle }}</h3>
                <button @click="calendarNext" class="p-2 hover:bg-gray-100 rounded">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                    </svg>
                </button>
            </div>

            <div class="flex items-center gap-2">
                <button @click="calendarGoToday" class="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 rounded">
                    Hoje
                </button>
                <div class="flex items-center bg-gray-100 rounded">
                    <button @click="calendarViewType = 'month'"
                            :class="['px-3 py-1 text-sm rounded', calendarViewType === 'month' ? 'bg-blue-500 text-white' : '']">
                        Mes
                    </button>
                    <button @click="calendarViewType = 'week'"
                            :class="['px-3 py-1 text-sm rounded', calendarViewType === 'week' ? 'bg-blue-500 text-white' : '']">
                        Semana
                    </button>
                </div>
            </div>
        </div>

        <!-- Month View -->
        <div v-if="calendarViewType === 'month'" class="calendar-month">
            <!-- Weekday Headers -->
            <div class="grid grid-cols-7 gap-px bg-gray-200 rounded-t-lg overflow-hidden">
                <div v-for="day in ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab']" :key="day"
                     class="bg-gray-100 text-center py-2 text-sm font-medium text-gray-600">
                    {{ day }}
                </div>
            </div>

            <!-- Calendar Days -->
            <div class="grid grid-cols-7 gap-px bg-gray-200">
                <div v-for="(day, idx) in calendarDays" :key="idx"
                     :class="['bg-white min-h-[100px] p-1',
                              day.isToday ? 'ring-2 ring-blue-500 ring-inset' : '',
                              day.isCurrentMonth ? '' : 'bg-gray-50']"
                     @drop="onCalendarDrop($event, day.date)"
                     @dragover.prevent>

                    <!-- Day Number -->
                    <div class="text-right mb-1">
                        <span :class="['inline-block w-6 h-6 text-center text-sm rounded-full',
                                       day.isToday ? 'bg-blue-500 text-white' : 'text-gray-700']">
                            {{ day.dayNumber }}
                        </span>
                    </div>

                    <!-- Events for this day -->
                    <div class="space-y-1 overflow-y-auto max-h-20">
                        <div v-for="event in getEventsForDate(day.date)" :key="event.id"
                             @click="openStoryById(event.id)"
                             draggable="true"
                             @dragstart="onCalendarDragStart($event, event)"
                             :class="['text-xs px-1 py-0.5 rounded truncate cursor-pointer',
                                      getCalendarEventClass(event)]">
                            {{ event.title }}
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Week View -->
        <div v-if="calendarViewType === 'week'" class="calendar-week">
            <div class="grid grid-cols-7 gap-2">
                <div v-for="day in calendarWeekDays" :key="day.date"
                     :class="['bg-white rounded-lg shadow p-3 min-h-[400px]',
                              day.isToday ? 'ring-2 ring-blue-500' : '']"
                     @drop="onCalendarDrop($event, day.date)"
                     @dragover.prevent>

                    <!-- Day Header -->
                    <div class="text-center mb-3 pb-2 border-b">
                        <div class="text-xs text-gray-500 uppercase">{{ day.dayName }}</div>
                        <div :class="['text-lg font-semibold',
                                      day.isToday ? 'text-blue-500' : 'text-gray-700']">
                            {{ day.dayNumber }}
                        </div>
                    </div>

                    <!-- Events -->
                    <div class="space-y-2">
                        <div v-for="event in getEventsForDate(day.date)" :key="event.id"
                             @click="openStoryById(event.id)"
                             draggable="true"
                             @dragstart="onCalendarDragStart($event, event)"
                             :class="['p-2 rounded cursor-pointer',
                                      getCalendarEventClass(event)]">
                            <div class="text-xs font-mono text-gray-500 mb-1">{{ event.id }}</div>
                            <div class="text-sm font-medium">{{ event.title }}</div>
                            <div class="flex items-center justify-between mt-1 text-xs">
                                <span>{{ event.story_points }} pts</span>
                                <span v-if="event.assignee" class="flex items-center gap-1">
                                    <span class="w-4 h-4 bg-gray-300 rounded-full flex items-center justify-center text-[10px]">
                                        {{ event.assignee.charAt(0).toUpperCase() }}
                                    </span>
                                </span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Sprint Markers -->
        <div v-if="calendarSprints.length > 0" class="mt-4">
            <h4 class="text-sm font-medium text-gray-600 mb-2">Sprints</h4>
            <div class="flex flex-wrap gap-2">
                <div v-for="sprint in calendarSprints" :key="sprint.id"
                     class="px-3 py-1 bg-purple-100 text-purple-700 rounded-full text-sm">
                    {{ sprint.name }}: {{ sprint.start_date }} - {{ sprint.end_date }}
                </div>
            </div>
        </div>
    </div>
    '''


def get_calendar_view_js():
    """Retorna o codigo JavaScript para a visualizacao de calendario."""
    return '''
    // Calendar View State (Issue #267)
    const calendarViewType = ref('month');
    const calendarCurrentDate = ref(new Date());
    const calendarEvents = ref([]);
    const calendarSprints = ref([]);
    const calendarDraggedEvent = ref(null);

    const calendarTitle = computed(() => {
        const date = calendarCurrentDate.value;
        const months = ['Janeiro', 'Fevereiro', 'Marco', 'Abril', 'Maio', 'Junho',
                        'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro'];
        return months[date.getMonth()] + ' ' + date.getFullYear();
    });

    const calendarDays = computed(() => {
        const date = calendarCurrentDate.value;
        const year = date.getFullYear();
        const month = date.getMonth();

        const firstDay = new Date(year, month, 1);
        const lastDay = new Date(year, month + 1, 0);
        const today = new Date();

        const days = [];

        // Add days from previous month
        const startPadding = firstDay.getDay();
        for (let i = startPadding - 1; i >= 0; i--) {
            const d = new Date(year, month, -i);
            days.push({
                date: d.toISOString().split('T')[0],
                dayNumber: d.getDate(),
                isCurrentMonth: false,
                isToday: false
            });
        }

        // Add days of current month
        for (let d = 1; d <= lastDay.getDate(); d++) {
            const dateObj = new Date(year, month, d);
            const dateStr = dateObj.toISOString().split('T')[0];
            days.push({
                date: dateStr,
                dayNumber: d,
                isCurrentMonth: true,
                isToday: dateStr === today.toISOString().split('T')[0]
            });
        }

        // Add days from next month
        const endPadding = 42 - days.length;
        for (let i = 1; i <= endPadding; i++) {
            const d = new Date(year, month + 1, i);
            days.push({
                date: d.toISOString().split('T')[0],
                dayNumber: i,
                isCurrentMonth: false,
                isToday: false
            });
        }

        return days;
    });

    const calendarWeekDays = computed(() => {
        const date = calendarCurrentDate.value;
        const today = new Date();
        const dayOfWeek = date.getDay();
        const startOfWeek = new Date(date);
        startOfWeek.setDate(date.getDate() - dayOfWeek);

        const dayNames = ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab'];
        const days = [];

        for (let i = 0; i < 7; i++) {
            const d = new Date(startOfWeek);
            d.setDate(startOfWeek.getDate() + i);
            const dateStr = d.toISOString().split('T')[0];
            days.push({
                date: dateStr,
                dayNumber: d.getDate(),
                dayName: dayNames[i],
                isToday: dateStr === today.toISOString().split('T')[0]
            });
        }

        return days;
    });

    const calendarPrev = () => {
        const date = calendarCurrentDate.value;
        if (calendarViewType.value === 'month') {
            calendarCurrentDate.value = new Date(date.getFullYear(), date.getMonth() - 1, 1);
        } else {
            calendarCurrentDate.value = new Date(date.getTime() - 7 * 24 * 60 * 60 * 1000);
        }
        loadCalendarEvents();
    };

    const calendarNext = () => {
        const date = calendarCurrentDate.value;
        if (calendarViewType.value === 'month') {
            calendarCurrentDate.value = new Date(date.getFullYear(), date.getMonth() + 1, 1);
        } else {
            calendarCurrentDate.value = new Date(date.getTime() + 7 * 24 * 60 * 60 * 1000);
        }
        loadCalendarEvents();
    };

    const calendarGoToday = () => {
        calendarCurrentDate.value = new Date();
        loadCalendarEvents();
    };

    const loadCalendarEvents = async () => {
        const date = calendarCurrentDate.value;
        const start = new Date(date.getFullYear(), date.getMonth(), 1);
        const end = new Date(date.getFullYear(), date.getMonth() + 2, 0);

        try {
            const params = new URLSearchParams({
                start_date: start.toISOString().split('T')[0],
                end_date: end.toISOString().split('T')[0]
            });

            const response = await fetch('/api/calendar/stories?' + params);
            if (response.ok) {
                const data = await response.json();
                calendarEvents.value = data.events;
            }

            // Load sprints
            const sprintResponse = await fetch('/api/calendar/sprints');
            if (sprintResponse.ok) {
                const sprintData = await sprintResponse.json();
                calendarSprints.value = sprintData.sprints;
            }
        } catch (e) {
            console.error('Failed to load calendar events:', e);
        }
    };

    const getEventsForDate = (dateStr) => {
        return calendarEvents.value.filter(e => e.date === dateStr);
    };

    const getCalendarEventClass = (event) => {
        const statusClasses = {
            'backlog': 'bg-gray-100 text-gray-700',
            'ready': 'bg-blue-100 text-blue-700',
            'in_progress': 'bg-yellow-100 text-yellow-700',
            'review': 'bg-purple-100 text-purple-700',
            'testing': 'bg-orange-100 text-orange-700',
            'done': 'bg-green-100 text-green-700'
        };
        return statusClasses[event.status] || 'bg-gray-100 text-gray-700';
    };

    const onCalendarDragStart = (e, event) => {
        calendarDraggedEvent.value = event;
        e.dataTransfer.effectAllowed = 'move';
    };

    const onCalendarDrop = async (e, dateStr) => {
        e.preventDefault();
        if (!calendarDraggedEvent.value) return;

        try {
            const response = await fetch('/api/calendar/story/' + calendarDraggedEvent.value.id + '/date', {
                method: 'PATCH',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ new_date: dateStr })
            });

            if (response.ok) {
                addToast('success', 'Data alterada', 'Story movida para ' + dateStr);
                loadCalendarEvents();
            }
        } catch (e) {
            addToast('error', 'Erro', 'Falha ao mover story');
        }

        calendarDraggedEvent.value = null;
    };

    const openStoryById = (storyId) => {
        const story = stories.value.find(s => s.story_id === storyId);
        if (story) openStoryDetail(story);
    };

    // Load events when switching to calendar view
    watch(viewMode, (newMode) => {
        if (newMode === 'calendar') {
            loadCalendarEvents();
        }
    });
    '''


def register_calendar_view(app):
    """Registra os endpoints de calendario no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Calendar View endpoints loaded: /api/calendar/*")
