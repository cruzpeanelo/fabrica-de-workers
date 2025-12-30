# -*- coding: utf-8 -*-
"""
Gamification Module (Issue #266)
================================
Sistema de gamificacao com pontos, badges e leaderboard.

Funcionalidades:
- Pontos por acoes (criar story, completar task)
- Badges por conquistas
- Leaderboard por periodo
- Niveis de experiencia
- Celebracoes visuais
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, List, Dict
from datetime import datetime, timedelta
from factory.database.connection import SessionLocal

router = APIRouter(prefix="/api/gamification", tags=["Gamification"])

# Points configuration
POINTS_CONFIG = {
    "create_story": 10,
    "complete_task": 20,
    "complete_story": 50,
    "review_code": 15,
    "help_teammate": 25,
    "fix_bug": 30,
    "write_docs": 10,
    "daily_standup": 5,
    "weekly_goal": 100
}

# Badge definitions
BADGES = [
    {"id": "first_story", "name": "Primeira Story", "description": "Criou sua primeira story", "icon": "📝", "requirement": 1},
    {"id": "ten_stories", "name": "Contador de Historias", "description": "Criou 10 stories", "icon": "📚", "requirement": 10},
    {"id": "hundred_tasks", "name": "Maquina de Tarefas", "description": "Completou 100 tasks", "icon": "⚡", "requirement": 100},
    {"id": "streak_7", "name": "Semana Perfeita", "description": "7 dias consecutivos de atividade", "icon": "🔥", "requirement": 7},
    {"id": "streak_30", "name": "Mestre da Consistencia", "description": "30 dias consecutivos", "icon": "💪", "requirement": 30},
    {"id": "reviewer", "name": "Revisor Expert", "description": "Fez 20 code reviews", "icon": "🔍", "requirement": 20},
    {"id": "helper", "name": "Colaborador", "description": "Ajudou 10 colegas", "icon": "🤝", "requirement": 10},
    {"id": "bug_hunter", "name": "Cacador de Bugs", "description": "Corrigiu 25 bugs", "icon": "🐛", "requirement": 25},
    {"id": "documenter", "name": "Documentador", "description": "Escreveu 15 documentacoes", "icon": "📖", "requirement": 15},
    {"id": "speed_demon", "name": "Velocista", "description": "Completou 5 tasks em 1 dia", "icon": "🏃", "requirement": 5},
]

# Level thresholds
LEVELS = [
    {"level": 1, "name": "Iniciante", "min_points": 0, "icon": "🌱"},
    {"level": 2, "name": "Aprendiz", "min_points": 100, "icon": "🌿"},
    {"level": 3, "name": "Desenvolvedor", "min_points": 300, "icon": "🌳"},
    {"level": 4, "name": "Senior", "min_points": 700, "icon": "🌲"},
    {"level": 5, "name": "Expert", "min_points": 1500, "icon": "🏔️"},
    {"level": 6, "name": "Mestre", "min_points": 3000, "icon": "⭐"},
    {"level": 7, "name": "Lenda", "min_points": 6000, "icon": "🏆"},
]

# In-memory storage (in production, use database)
user_points: Dict[str, int] = {}
user_badges: Dict[str, List[str]] = {}
user_activities: Dict[str, List[dict]] = {}
user_streaks: Dict[str, int] = {}


class PointsEvent(BaseModel):
    user: str
    action: str
    metadata: Optional[dict] = None


@router.get("/points")
async def get_points(user: str = Query(None)):
    """Retorna pontos do usuario ou ranking geral."""
    if user:
        total = user_points.get(user, 0)
        level_info = get_level(total)
        return {
            "user": user,
            "points": total,
            "level": level_info
        }
    else:
        # Return all users sorted by points
        ranking = sorted(user_points.items(), key=lambda x: x[1], reverse=True)
        return {
            "ranking": [{"user": u, "points": p, "level": get_level(p)} for u, p in ranking[:20]]
        }


@router.post("/points")
async def award_points(event: PointsEvent):
    """Concede pontos ao usuario por uma acao."""
    points = POINTS_CONFIG.get(event.action, 5)

    if event.user not in user_points:
        user_points[event.user] = 0
        user_badges[event.user] = []
        user_activities[event.user] = []

    user_points[event.user] += points

    # Log activity
    activity = {
        "action": event.action,
        "points": points,
        "timestamp": datetime.now().isoformat(),
        "metadata": event.metadata
    }
    user_activities[event.user].append(activity)

    # Check for new badges
    new_badges = check_badges(event.user)

    # Get current level
    level_info = get_level(user_points[event.user])

    return {
        "success": True,
        "points_awarded": points,
        "total_points": user_points[event.user],
        "new_badges": new_badges,
        "level": level_info
    }


@router.get("/profile/{user}")
async def get_user_profile(user: str):
    """Retorna o perfil de gamificacao do usuario."""
    total_points = user_points.get(user, 0)
    badges = user_badges.get(user, [])
    streak = user_streaks.get(user, 0)
    level_info = get_level(total_points)

    # Calculate points to next level
    next_level = None
    points_to_next = 0
    for lvl in LEVELS:
        if lvl["min_points"] > total_points:
            next_level = lvl
            points_to_next = lvl["min_points"] - total_points
            break

    # Get recent activities
    activities = user_activities.get(user, [])[-10:]

    return {
        "user": user,
        "total_points": total_points,
        "level": level_info,
        "next_level": next_level,
        "points_to_next_level": points_to_next,
        "badges": [b for b in BADGES if b["id"] in badges],
        "streak_days": streak,
        "recent_activities": activities
    }


@router.get("/leaderboard")
async def get_leaderboard(
    period: str = Query("week", description="all, week, month"),
    limit: int = Query(10, ge=1, le=50)
):
    """Retorna o leaderboard de pontos."""
    sorted_users = sorted(user_points.items(), key=lambda x: x[1], reverse=True)[:limit]

    leaderboard = []
    for rank, (user, points) in enumerate(sorted_users, 1):
        level_info = get_level(points)
        leaderboard.append({
            "rank": rank,
            "user": user,
            "points": points,
            "level": level_info,
            "badges_count": len(user_badges.get(user, []))
        })

    return {"leaderboard": leaderboard, "period": period}


@router.get("/badges")
async def get_all_badges():
    """Retorna todos os badges disponiveis."""
    return {"badges": BADGES}


@router.get("/levels")
async def get_all_levels():
    """Retorna todos os niveis disponiveis."""
    return {"levels": LEVELS}


def get_level(points: int) -> dict:
    """Determina o nivel baseado nos pontos."""
    current_level = LEVELS[0]
    for level in LEVELS:
        if points >= level["min_points"]:
            current_level = level
        else:
            break
    return current_level


def check_badges(user: str) -> List[dict]:
    """Verifica e concede novos badges ao usuario."""
    new_badges = []
    activities = user_activities.get(user, [])

    # Count activities by type
    story_count = len([a for a in activities if a["action"] == "create_story"])
    task_count = len([a for a in activities if a["action"] == "complete_task"])
    review_count = len([a for a in activities if a["action"] == "review_code"])
    help_count = len([a for a in activities if a["action"] == "help_teammate"])
    bug_count = len([a for a in activities if a["action"] == "fix_bug"])
    docs_count = len([a for a in activities if a["action"] == "write_docs"])

    badge_checks = [
        ("first_story", story_count >= 1),
        ("ten_stories", story_count >= 10),
        ("hundred_tasks", task_count >= 100),
        ("reviewer", review_count >= 20),
        ("helper", help_count >= 10),
        ("bug_hunter", bug_count >= 25),
        ("documenter", docs_count >= 15),
    ]

    for badge_id, condition in badge_checks:
        if condition and badge_id not in user_badges.get(user, []):
            if user not in user_badges:
                user_badges[user] = []
            user_badges[user].append(badge_id)
            badge_info = next((b for b in BADGES if b["id"] == badge_id), None)
            if badge_info:
                new_badges.append(badge_info)

    return new_badges


def get_gamification_html():
    """Retorna o HTML do componente de gamificacao."""
    return '''
    <!-- Gamification Panel (Issue #266) -->
    <div v-if="showGamificationPanel"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showGamificationPanel = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-2xl mx-4 overflow-hidden">
            <!-- Header -->
            <div class="bg-gradient-to-r from-purple-600 to-indigo-600 px-6 py-4 text-white">
                <div class="flex items-center justify-between">
                    <h2 class="text-xl font-bold">Suas Conquistas</h2>
                    <button @click="showGamificationPanel = false" class="text-white/70 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
            </div>

            <!-- Content -->
            <div class="p-6">
                <!-- Level & Points -->
                <div class="flex items-center justify-between mb-6">
                    <div class="flex items-center gap-4">
                        <div class="text-4xl">{{ gamification.level?.icon || '🌱' }}</div>
                        <div>
                            <div class="text-sm text-gray-500">Nivel {{ gamification.level?.level || 1 }}</div>
                            <div class="font-bold text-lg">{{ gamification.level?.name || 'Iniciante' }}</div>
                        </div>
                    </div>
                    <div class="text-right">
                        <div class="text-3xl font-bold text-purple-600">{{ gamification.total_points }}</div>
                        <div class="text-sm text-gray-500">pontos totais</div>
                    </div>
                </div>

                <!-- Progress to next level -->
                <div class="mb-6">
                    <div class="flex justify-between text-sm text-gray-500 mb-1">
                        <span>Proximo nivel: {{ gamification.next_level?.name || 'Max' }}</span>
                        <span>{{ gamification.points_to_next_level }} pts restantes</span>
                    </div>
                    <div class="h-3 bg-gray-200 rounded-full overflow-hidden">
                        <div class="h-full bg-gradient-to-r from-purple-500 to-indigo-500 rounded-full transition-all"
                             :style="{width: levelProgress + '%'}"></div>
                    </div>
                </div>

                <!-- Streak -->
                <div class="flex items-center gap-3 p-4 bg-orange-50 rounded-xl mb-6">
                    <div class="text-3xl">🔥</div>
                    <div>
                        <div class="font-bold text-orange-600">{{ gamification.streak_days }} dias</div>
                        <div class="text-sm text-gray-600">de atividade consecutiva</div>
                    </div>
                </div>

                <!-- Badges -->
                <div class="mb-6">
                    <h3 class="font-semibold mb-3">Badges Conquistados</h3>
                    <div class="flex flex-wrap gap-3">
                        <div v-for="badge in gamification.badges" :key="badge.id"
                             class="flex items-center gap-2 px-3 py-2 bg-gray-100 rounded-lg"
                             :title="badge.description">
                            <span class="text-2xl">{{ badge.icon }}</span>
                            <span class="text-sm font-medium">{{ badge.name }}</span>
                        </div>
                        <div v-if="gamification.badges?.length === 0"
                             class="text-gray-400 text-sm">
                            Nenhum badge ainda. Continue trabalhando!
                        </div>
                    </div>
                </div>

                <!-- Recent Activity -->
                <div>
                    <h3 class="font-semibold mb-3">Atividade Recente</h3>
                    <div class="space-y-2 max-h-40 overflow-y-auto">
                        <div v-for="activity in gamification.recent_activities" :key="activity.timestamp"
                             class="flex items-center justify-between text-sm p-2 hover:bg-gray-50 rounded">
                            <span>{{ formatActivityAction(activity.action) }}</span>
                            <span class="text-green-600 font-medium">+{{ activity.points }} pts</span>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Leaderboard Link -->
            <div class="px-6 py-3 bg-gray-50 border-t text-center">
                <button @click="showLeaderboard = true; showGamificationPanel = false"
                        class="text-purple-600 hover:text-purple-700 font-medium text-sm">
                    Ver Leaderboard Completo →
                </button>
            </div>
        </div>
    </div>

    <!-- Leaderboard Modal -->
    <div v-if="showLeaderboard"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showLeaderboard = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-lg mx-4 overflow-hidden">
            <div class="bg-gradient-to-r from-yellow-500 to-orange-500 px-6 py-4 text-white flex items-center justify-between">
                <h2 class="text-xl font-bold flex items-center gap-2">
                    <span>🏆</span> Leaderboard
                </h2>
                <button @click="showLeaderboard = false" class="text-white/70 hover:text-white">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <div class="p-4">
                <div class="space-y-2">
                    <div v-for="entry in leaderboard" :key="entry.user"
                         :class="['flex items-center gap-3 p-3 rounded-lg',
                                  entry.rank === 1 ? 'bg-yellow-50' :
                                  entry.rank === 2 ? 'bg-gray-100' :
                                  entry.rank === 3 ? 'bg-orange-50' : 'hover:bg-gray-50']">
                        <div :class="['w-8 h-8 rounded-full flex items-center justify-center font-bold text-sm',
                                      entry.rank === 1 ? 'bg-yellow-400 text-white' :
                                      entry.rank === 2 ? 'bg-gray-400 text-white' :
                                      entry.rank === 3 ? 'bg-orange-400 text-white' : 'bg-gray-200']">
                            {{ entry.rank }}
                        </div>
                        <div class="flex-1">
                            <div class="font-medium">{{ entry.user }}</div>
                            <div class="text-xs text-gray-500">{{ entry.level.name }} • {{ entry.badges_count }} badges</div>
                        </div>
                        <div class="text-right">
                            <div class="font-bold text-purple-600">{{ entry.points }}</div>
                            <div class="text-xs text-gray-500">pontos</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <!-- XP Gain Animation -->
    <div v-if="xpGainAnimation.show"
         class="fixed top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 z-50 pointer-events-none">
        <div class="text-4xl font-bold text-green-500 animate-bounce-up">
            +{{ xpGainAnimation.points }} XP
        </div>
    </div>
    '''


def get_gamification_js():
    """Retorna o codigo JavaScript para gamificacao."""
    return '''
    // Gamification State (Issue #266)
    const showGamificationPanel = ref(false);
    const showLeaderboard = ref(false);
    const gamification = ref({
        total_points: 0,
        level: null,
        next_level: null,
        points_to_next_level: 0,
        badges: [],
        streak_days: 0,
        recent_activities: []
    });
    const leaderboard = ref([]);
    const xpGainAnimation = ref({ show: false, points: 0 });

    const loadGamificationProfile = async () => {
        try {
            const response = await fetch('/api/gamification/profile/current_user');
            if (response.ok) {
                gamification.value = await response.json();
            }
        } catch (e) {
            console.error('Failed to load gamification profile:', e);
        }
    };

    const loadLeaderboard = async () => {
        try {
            const response = await fetch('/api/gamification/leaderboard');
            if (response.ok) {
                const data = await response.json();
                leaderboard.value = data.leaderboard;
            }
        } catch (e) {
            console.error('Failed to load leaderboard:', e);
        }
    };

    const awardPoints = async (action, metadata = null) => {
        try {
            const response = await fetch('/api/gamification/points', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ user: 'current_user', action, metadata })
            });

            if (response.ok) {
                const data = await response.json();

                // Show XP animation
                xpGainAnimation.value = { show: true, points: data.points_awarded };
                setTimeout(() => { xpGainAnimation.value.show = false; }, 1500);

                // Show badge notification
                if (data.new_badges && data.new_badges.length > 0) {
                    for (const badge of data.new_badges) {
                        addToast('success', 'Novo Badge!', badge.icon + ' ' + badge.name);
                    }
                }

                // Refresh profile
                await loadGamificationProfile();
            }
        } catch (e) {
            console.error('Failed to award points:', e);
        }
    };

    const levelProgress = computed(() => {
        if (!gamification.value.level || !gamification.value.next_level) return 100;
        const current = gamification.value.total_points - gamification.value.level.min_points;
        const total = gamification.value.next_level.min_points - gamification.value.level.min_points;
        return Math.min(100, Math.round((current / total) * 100));
    });

    const formatActivityAction = (action) => {
        const actionNames = {
            'create_story': 'Criou uma story',
            'complete_task': 'Completou uma task',
            'complete_story': 'Completou uma story',
            'review_code': 'Fez code review',
            'help_teammate': 'Ajudou um colega',
            'fix_bug': 'Corrigiu um bug',
            'write_docs': 'Escreveu documentacao',
            'daily_standup': 'Participou do standup'
        };
        return actionNames[action] || action;
    };

    // Load on mount
    onMounted(() => {
        loadGamificationProfile();
        loadLeaderboard();
    });
    '''


def register_gamification(app):
    """Registra os endpoints de gamificacao no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Gamification endpoints loaded: /api/gamification/*")
