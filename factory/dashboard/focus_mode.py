# -*- coding: utf-8 -*-
"""
Focus Mode with Pomodoro Timer (Issue #265)
==========================================
Sistema de modo foco que esconde distracoes e inclui timer Pomodoro.

Funcionalidades:
- Modo foco que esconde sidebar e notificacoes
- Timer Pomodoro integrado (25/5 minutos)
- Estatisticas de tempo focado
- Sons de ambiente opcionais
- Historico de sessoes de foco
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional, List
from datetime import datetime, timedelta
from factory.database.connection import SessionLocal

router = APIRouter(prefix="/api/focus", tags=["Focus Mode"])


class FocusSession(BaseModel):
    task_id: Optional[str] = None
    story_id: Optional[str] = None
    duration_minutes: int = 25
    completed: bool = False
    started_at: Optional[str] = None
    ended_at: Optional[str] = None


# In-memory storage for focus sessions (in production, use database)
focus_sessions = []


@router.post("/start")
async def start_focus_session(session: FocusSession):
    """Inicia uma nova sessao de foco."""
    session_data = {
        "id": len(focus_sessions) + 1,
        "task_id": session.task_id,
        "story_id": session.story_id,
        "duration_minutes": session.duration_minutes,
        "started_at": datetime.now().isoformat(),
        "ended_at": None,
        "completed": False
    }
    focus_sessions.append(session_data)

    return {
        "success": True,
        "session": session_data
    }


@router.post("/complete/{session_id}")
async def complete_focus_session(session_id: int):
    """Marca uma sessao de foco como completa."""
    for session in focus_sessions:
        if session["id"] == session_id:
            session["completed"] = True
            session["ended_at"] = datetime.now().isoformat()
            return {"success": True, "session": session}

    raise HTTPException(status_code=404, detail="Sessao nao encontrada")


@router.get("/stats")
async def get_focus_stats():
    """Retorna estatisticas de foco do usuario."""
    today = datetime.now().date()
    week_start = today - timedelta(days=today.weekday())

    today_sessions = [s for s in focus_sessions if s["completed"] and
                      s["ended_at"] and datetime.fromisoformat(s["ended_at"]).date() == today]

    week_sessions = [s for s in focus_sessions if s["completed"] and
                     s["ended_at"] and datetime.fromisoformat(s["ended_at"]).date() >= week_start]

    return {
        "today": {
            "sessions": len(today_sessions),
            "total_minutes": sum(s["duration_minutes"] for s in today_sessions)
        },
        "week": {
            "sessions": len(week_sessions),
            "total_minutes": sum(s["duration_minutes"] for s in week_sessions)
        },
        "all_time": {
            "sessions": len([s for s in focus_sessions if s["completed"]]),
            "total_minutes": sum(s["duration_minutes"] for s in focus_sessions if s["completed"])
        }
    }


@router.get("/history")
async def get_focus_history(limit: int = 20):
    """Retorna historico de sessoes de foco."""
    completed = [s for s in focus_sessions if s["completed"]]
    return completed[-limit:]


def get_focus_mode_html():
    """Retorna o HTML do componente de modo foco."""
    return '''
    <!-- Focus Mode Overlay (Issue #265) -->
    <div v-if="focusMode.active"
         class="fixed inset-0 z-50 bg-gray-900 flex flex-col"
         :class="{ 'focus-dark': focusMode.theme === 'dark' }">

        <!-- Focus Header -->
        <div class="flex items-center justify-between px-6 py-4 border-b border-gray-700">
            <div class="flex items-center gap-4">
                <button @click="exitFocusMode"
                        class="text-gray-400 hover:text-white transition">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
                <span class="text-white font-medium">Modo Foco</span>
            </div>

            <!-- Timer Display -->
            <div class="flex items-center gap-4">
                <div class="text-4xl font-mono text-white tabular-nums">
                    {{ formatFocusTime(focusMode.timeRemaining) }}
                </div>

                <!-- Timer Controls -->
                <div class="flex items-center gap-2">
                    <button @click="toggleFocusTimer"
                            class="w-10 h-10 rounded-full bg-white/10 hover:bg-white/20 flex items-center justify-center text-white transition">
                        <svg v-if="!focusMode.running" class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M8 5v14l11-7z"/>
                        </svg>
                        <svg v-else class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M6 19h4V5H6v14zm8-14v14h4V5h-4z"/>
                        </svg>
                    </button>
                    <button @click="resetFocusTimer"
                            class="w-10 h-10 rounded-full bg-white/10 hover:bg-white/20 flex items-center justify-center text-white transition">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
                        </svg>
                    </button>
                </div>
            </div>

            <!-- Settings -->
            <div class="flex items-center gap-3">
                <!-- Duration Presets -->
                <div class="flex items-center gap-1 bg-white/10 rounded-lg p-1">
                    <button v-for="mins in [15, 25, 45, 60]" :key="mins"
                            @click="setFocusDuration(mins)"
                            :class="['px-3 py-1 rounded text-sm transition',
                                     focusMode.duration === mins ? 'bg-white text-gray-900' : 'text-gray-400 hover:text-white']">
                        {{ mins }}m
                    </button>
                </div>

                <!-- Sound Toggle -->
                <button @click="focusMode.soundEnabled = !focusMode.soundEnabled"
                        :class="['w-10 h-10 rounded-full flex items-center justify-center transition',
                                 focusMode.soundEnabled ? 'bg-green-500/20 text-green-400' : 'bg-white/10 text-gray-400']">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path v-if="focusMode.soundEnabled" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                              d="M15.536 8.464a5 5 0 010 7.072m2.828-9.9a9 9 0 010 12.728M5.586 15H4a1 1 0 01-1-1v-4a1 1 0 011-1h1.586l4.707-4.707C10.923 3.663 12 4.109 12 5v14c0 .891-1.077 1.337-1.707.707L5.586 15z"/>
                        <path v-else stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                              d="M5.586 15H4a1 1 0 01-1-1v-4a1 1 0 011-1h1.586l4.707-4.707C10.923 3.663 12 4.109 12 5v14c0 .891-1.077 1.337-1.707.707L5.586 15z M17 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2"/>
                    </svg>
                </button>
            </div>
        </div>

        <!-- Focus Content Area -->
        <div class="flex-1 flex items-center justify-center p-8">
            <!-- Current Task/Story -->
            <div v-if="focusMode.currentTask" class="max-w-2xl w-full">
                <div class="bg-white/5 rounded-2xl p-8 backdrop-blur">
                    <div class="text-gray-400 text-sm mb-2">{{ focusMode.currentTask.story_id }}</div>
                    <h2 class="text-2xl font-semibold text-white mb-4">{{ focusMode.currentTask.title }}</h2>
                    <p class="text-gray-300 leading-relaxed">{{ focusMode.currentTask.description }}</p>

                    <!-- Progress -->
                    <div class="mt-6">
                        <div class="flex justify-between text-sm text-gray-400 mb-2">
                            <span>Progresso</span>
                            <span>{{ focusMode.currentTask.progress }}%</span>
                        </div>
                        <div class="h-2 bg-white/10 rounded-full overflow-hidden">
                            <div class="h-full bg-green-500 rounded-full transition-all"
                                 :style="{width: focusMode.currentTask.progress + '%'}"></div>
                        </div>
                    </div>
                </div>

                <!-- Mark Complete Button -->
                <div class="mt-6 flex justify-center">
                    <button @click="markFocusTaskComplete"
                            class="px-6 py-3 bg-green-500 hover:bg-green-600 text-white rounded-lg font-medium transition flex items-center gap-2">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                        </svg>
                        Marcar como Concluido
                    </button>
                </div>
            </div>

            <!-- No Task Selected -->
            <div v-else class="text-center">
                <div class="text-6xl mb-4">🎯</div>
                <h2 class="text-2xl font-semibold text-white mb-2">Modo Foco Ativo</h2>
                <p class="text-gray-400">Concentre-se na sua tarefa atual</p>
            </div>
        </div>

        <!-- Focus Stats -->
        <div class="border-t border-gray-700 px-6 py-3">
            <div class="flex items-center justify-center gap-8 text-sm">
                <div class="text-center">
                    <div class="text-gray-400">Sessoes Hoje</div>
                    <div class="text-white font-semibold">{{ focusMode.stats.todaySessions }}</div>
                </div>
                <div class="text-center">
                    <div class="text-gray-400">Tempo Focado Hoje</div>
                    <div class="text-white font-semibold">{{ focusMode.stats.todayMinutes }} min</div>
                </div>
                <div class="text-center">
                    <div class="text-gray-400">Sessoes na Semana</div>
                    <div class="text-white font-semibold">{{ focusMode.stats.weekSessions }}</div>
                </div>
            </div>
        </div>
    </div>

    <!-- Focus Mode Button (FAB) -->
    <button v-if="selectedStoryForDetail && !focusMode.active"
            @click="enterFocusMode(selectedStoryForDetail)"
            class="fixed bottom-24 right-6 w-12 h-12 bg-purple-600 hover:bg-purple-700 text-white rounded-full shadow-lg flex items-center justify-center transition z-40"
            title="Modo Foco">
        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"/>
        </svg>
    </button>
    '''


def get_focus_mode_js():
    """Retorna o codigo JavaScript para o modo foco."""
    return '''
    // Focus Mode State (Issue #265)
    const focusMode = ref({
        active: false,
        running: false,
        duration: 25,
        timeRemaining: 25 * 60,
        currentTask: null,
        soundEnabled: true,
        theme: 'dark',
        stats: {
            todaySessions: 0,
            todayMinutes: 0,
            weekSessions: 0
        }
    });

    let focusInterval = null;

    const enterFocusMode = (story) => {
        focusMode.value.active = true;
        focusMode.value.currentTask = {
            story_id: story.story_id,
            title: story.title,
            description: story.description,
            progress: story.progress || 0
        };
        focusMode.value.timeRemaining = focusMode.value.duration * 60;
        loadFocusStats();

        // Start session on backend
        fetch('/api/focus/start', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                story_id: story.story_id,
                duration_minutes: focusMode.value.duration
            })
        });
    };

    const exitFocusMode = () => {
        focusMode.value.active = false;
        focusMode.value.running = false;
        if (focusInterval) {
            clearInterval(focusInterval);
            focusInterval = null;
        }
    };

    const toggleFocusTimer = () => {
        if (focusMode.value.running) {
            // Pause
            focusMode.value.running = false;
            if (focusInterval) {
                clearInterval(focusInterval);
                focusInterval = null;
            }
        } else {
            // Start
            focusMode.value.running = true;
            focusInterval = setInterval(() => {
                if (focusMode.value.timeRemaining > 0) {
                    focusMode.value.timeRemaining--;
                } else {
                    // Timer complete
                    focusMode.value.running = false;
                    clearInterval(focusInterval);
                    focusInterval = null;
                    onFocusComplete();
                }
            }, 1000);
        }
    };

    const resetFocusTimer = () => {
        focusMode.value.running = false;
        focusMode.value.timeRemaining = focusMode.value.duration * 60;
        if (focusInterval) {
            clearInterval(focusInterval);
            focusInterval = null;
        }
    };

    const setFocusDuration = (minutes) => {
        focusMode.value.duration = minutes;
        focusMode.value.timeRemaining = minutes * 60;
    };

    const formatFocusTime = (seconds) => {
        const mins = Math.floor(seconds / 60);
        const secs = seconds % 60;
        return String(mins).padStart(2, '0') + ':' + String(secs).padStart(2, '0');
    };

    const onFocusComplete = () => {
        // Play sound
        if (focusMode.value.soundEnabled) {
            const audio = new Audio('data:audio/wav;base64,UklGRl9vT19XQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQ==');
            audio.play().catch(() => {});
        }

        // Show notification
        if (Notification.permission === 'granted') {
            new Notification('Pomodoro Completo!', {
                body: 'Parabens! Hora de uma pausa.',
                icon: '/static/icons/icon-192.png'
            });
        }

        // Update stats
        focusMode.value.stats.todaySessions++;
        focusMode.value.stats.todayMinutes += focusMode.value.duration;

        addToast('success', 'Pomodoro Completo!', 'Hora de uma pausa de 5 minutos');
    };

    const loadFocusStats = async () => {
        try {
            const response = await fetch('/api/focus/stats');
            if (response.ok) {
                const data = await response.json();
                focusMode.value.stats = {
                    todaySessions: data.today.sessions,
                    todayMinutes: data.today.total_minutes,
                    weekSessions: data.week.sessions
                };
            }
        } catch (e) {
            console.error('Failed to load focus stats:', e);
        }
    };

    const markFocusTaskComplete = () => {
        if (focusMode.value.currentTask) {
            // Update task progress
            addToast('success', 'Tarefa Concluida', 'Otimo trabalho!');
            exitFocusMode();
        }
    };
    '''


def get_focus_mode_css():
    """Retorna o CSS para o modo foco."""
    return '''
    /* Focus Mode Styles (Issue #265) */
    .focus-dark {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
    }

    .focus-mode-active {
        overflow: hidden;
    }

    /* Focus timer animation */
    @keyframes pulse-focus {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
    }

    .focus-timer-running {
        animation: pulse-focus 2s ease-in-out infinite;
    }
    '''


def register_focus_mode(app):
    """Registra os endpoints de modo foco no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Focus Mode endpoints loaded: /api/focus/*")
