# -*- coding: utf-8 -*-
"""
Comments and Threads Module (Issue #225)
========================================
Sistema de comentarios com threads para Stories e Tasks.

Funcionalidades:
- Comentarios em Stories e Tasks
- Respostas em threads (nested comments)
- Edicao e exclusao de comentarios proprios
- @mentions com autocomplete
- Suporte a Markdown
- Reacoes/emoji em comentarios
- Contagem de comentarios nos cards
"""

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import HTMLResponse
from pydantic import BaseModel, Field
from typing import Optional, List, Dict
from datetime import datetime
from uuid import uuid4
import re

router = APIRouter(prefix="/api", tags=["Comments"])

# =============================================================================
# MODELS
# =============================================================================

class CommentCreate(BaseModel):
    """Modelo para criar um comentario."""
    entity_type: str = Field(..., description="story ou task")
    entity_id: str = Field(..., description="ID da story ou task")
    parent_id: Optional[str] = Field(None, description="ID do comentario pai (para replies)")
    author_id: str = Field(..., description="ID do autor")
    author_name: str = Field(..., description="Nome do autor")
    content: str = Field(..., description="Conteudo do comentario (markdown)")


class CommentUpdate(BaseModel):
    """Modelo para atualizar um comentario."""
    content: str = Field(..., description="Novo conteudo do comentario")


class ReactionCreate(BaseModel):
    """Modelo para adicionar reacao."""
    emoji: str = Field(..., description="Emoji da reacao")
    user_id: str = Field(..., description="ID do usuario")


class Comment(BaseModel):
    """Modelo completo de um comentario."""
    id: str
    entity_type: str  # "story" ou "task"
    entity_id: str
    parent_id: Optional[str] = None
    author_id: str
    author_name: str
    content: str
    reactions: Dict[str, List[str]] = {}  # emoji -> user_ids
    created_at: datetime
    updated_at: Optional[datetime] = None
    is_edited: bool = False
    replies: List['Comment'] = []
    mentions: List[str] = []  # usernames mentioned


# =============================================================================
# STORAGE (In-memory, replace with database in production)
# =============================================================================

# All comments indexed by ID
comments_db: Dict[str, dict] = {}

# Index by entity (story_id or task_id) -> list of comment_ids
entity_comments: Dict[str, List[str]] = {}

# Users for mention autocomplete
users_db: Dict[str, dict] = {
    "user1": {"id": "user1", "name": "Joao Silva", "avatar": "https://ui-avatars.com/api/?name=Joao+Silva&background=003B4A&color=fff"},
    "user2": {"id": "user2", "name": "Maria Santos", "avatar": "https://ui-avatars.com/api/?name=Maria+Santos&background=FF6C00&color=fff"},
    "user3": {"id": "user3", "name": "Carlos Oliveira", "avatar": "https://ui-avatars.com/api/?name=Carlos+Oliveira&background=10B981&color=fff"},
    "admin": {"id": "admin", "name": "Admin User", "avatar": "https://ui-avatars.com/api/?name=Admin+User&background=003B4A&color=fff"},
}

# Available reaction emojis
AVAILABLE_REACTIONS = ["thumbsup", "thumbsdown", "heart", "tada", "confused"]
REACTION_DISPLAY = {
    "thumbsup": {"emoji": "\ud83d\udc4d", "label": "Like"},
    "thumbsdown": {"emoji": "\ud83d\udc4e", "label": "Dislike"},
    "heart": {"emoji": "\u2764\ufe0f", "label": "Love"},
    "tada": {"emoji": "\ud83c\udf89", "label": "Celebrate"},
    "confused": {"emoji": "\ud83d\ude15", "label": "Confused"},
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def extract_mentions(content: str) -> List[str]:
    """Extrai @mentions do conteudo."""
    mentions = re.findall(r'@(\w+)', content)
    return list(set(mentions))


def generate_comment_id() -> str:
    """Gera um ID unico para o comentario."""
    return f"CMT-{uuid4().hex[:8].upper()}"


def build_comment_tree(comment_ids: List[str]) -> List[dict]:
    """Constroi a arvore de comentarios com respostas aninhadas."""
    # Get all top-level comments (no parent)
    top_level = []
    children_map: Dict[str, List[dict]] = {}

    for cid in comment_ids:
        comment = comments_db.get(cid)
        if not comment:
            continue

        if comment.get("parent_id"):
            parent_id = comment["parent_id"]
            if parent_id not in children_map:
                children_map[parent_id] = []
            children_map[parent_id].append(comment)
        else:
            top_level.append(comment)

    # Sort by created_at
    top_level.sort(key=lambda x: x["created_at"])

    # Recursively add replies
    def add_replies(comment):
        comment_id = comment["id"]
        replies = children_map.get(comment_id, [])
        replies.sort(key=lambda x: x["created_at"])
        comment["replies"] = [add_replies(r) for r in replies]
        return comment

    return [add_replies(c) for c in top_level]


def get_comment_count(entity_type: str, entity_id: str) -> int:
    """Retorna a contagem de comentarios para uma entidade."""
    key = f"{entity_type}:{entity_id}"
    comment_ids = entity_comments.get(key, [])
    return len(comment_ids)


# =============================================================================
# API ENDPOINTS - COMMENTS
# =============================================================================

@router.get("/stories/{story_id}/comments")
async def list_story_comments(story_id: str):
    """Lista todos os comentarios de uma story com threads."""
    key = f"story:{story_id}"
    comment_ids = entity_comments.get(key, [])
    comments_tree = build_comment_tree(comment_ids)

    return {
        "entity_type": "story",
        "entity_id": story_id,
        "total_count": len(comment_ids),
        "comments": comments_tree
    }


@router.get("/tasks/{task_id}/comments")
async def list_task_comments(task_id: str):
    """Lista todos os comentarios de uma task com threads."""
    key = f"task:{task_id}"
    comment_ids = entity_comments.get(key, [])
    comments_tree = build_comment_tree(comment_ids)

    return {
        "entity_type": "task",
        "entity_id": task_id,
        "total_count": len(comment_ids),
        "comments": comments_tree
    }


@router.post("/comments")
async def create_comment(comment: CommentCreate):
    """Cria um novo comentario (ou reply se parent_id fornecido)."""
    # Validate entity_type
    if comment.entity_type not in ["story", "task"]:
        raise HTTPException(status_code=400, detail="entity_type deve ser 'story' ou 'task'")

    # Validate parent exists if provided
    if comment.parent_id and comment.parent_id not in comments_db:
        raise HTTPException(status_code=404, detail="Comentario pai nao encontrado")

    # Extract mentions from content
    mentions = extract_mentions(comment.content)

    # Create comment
    comment_id = generate_comment_id()
    now = datetime.utcnow()

    new_comment = {
        "id": comment_id,
        "entity_type": comment.entity_type,
        "entity_id": comment.entity_id,
        "parent_id": comment.parent_id,
        "author_id": comment.author_id,
        "author_name": comment.author_name,
        "content": comment.content,
        "reactions": {},
        "created_at": now.isoformat(),
        "updated_at": None,
        "is_edited": False,
        "replies": [],
        "mentions": mentions
    }

    # Store comment
    comments_db[comment_id] = new_comment

    # Index by entity
    key = f"{comment.entity_type}:{comment.entity_id}"
    if key not in entity_comments:
        entity_comments[key] = []
    entity_comments[key].append(comment_id)

    return {
        "success": True,
        "comment": new_comment,
        "mentions_notified": mentions
    }


@router.put("/comments/{comment_id}")
async def update_comment(comment_id: str, update: CommentUpdate):
    """Atualiza um comentario existente."""
    if comment_id not in comments_db:
        raise HTTPException(status_code=404, detail="Comentario nao encontrado")

    comment = comments_db[comment_id]

    # Extract new mentions
    mentions = extract_mentions(update.content)

    # Update comment
    comment["content"] = update.content
    comment["updated_at"] = datetime.utcnow().isoformat()
    comment["is_edited"] = True
    comment["mentions"] = mentions

    return {
        "success": True,
        "comment": comment
    }


@router.delete("/comments/{comment_id}")
async def delete_comment(comment_id: str):
    """Deleta um comentario e suas respostas."""
    if comment_id not in comments_db:
        raise HTTPException(status_code=404, detail="Comentario nao encontrado")

    comment = comments_db[comment_id]
    entity_key = f"{comment['entity_type']}:{comment['entity_id']}"

    # Find all replies recursively
    def find_all_children(parent_id: str) -> List[str]:
        children = [cid for cid, c in comments_db.items() if c.get("parent_id") == parent_id]
        all_descendants = list(children)
        for child_id in children:
            all_descendants.extend(find_all_children(child_id))
        return all_descendants

    # Get all comments to delete
    to_delete = [comment_id] + find_all_children(comment_id)

    # Remove from database and index
    for cid in to_delete:
        if cid in comments_db:
            del comments_db[cid]
        if entity_key in entity_comments and cid in entity_comments[entity_key]:
            entity_comments[entity_key].remove(cid)

    return {
        "success": True,
        "deleted_count": len(to_delete)
    }


# =============================================================================
# API ENDPOINTS - REACTIONS
# =============================================================================

@router.post("/comments/{comment_id}/reactions")
async def add_reaction(comment_id: str, reaction: ReactionCreate):
    """Adiciona uma reacao a um comentario."""
    if comment_id not in comments_db:
        raise HTTPException(status_code=404, detail="Comentario nao encontrado")

    if reaction.emoji not in AVAILABLE_REACTIONS:
        raise HTTPException(status_code=400, detail=f"Emoji invalido. Use: {AVAILABLE_REACTIONS}")

    comment = comments_db[comment_id]

    # Initialize reactions dict if needed
    if "reactions" not in comment:
        comment["reactions"] = {}

    # Add user to reaction
    if reaction.emoji not in comment["reactions"]:
        comment["reactions"][reaction.emoji] = []

    if reaction.user_id not in comment["reactions"][reaction.emoji]:
        comment["reactions"][reaction.emoji].append(reaction.user_id)

    return {
        "success": True,
        "reactions": comment["reactions"]
    }


@router.delete("/comments/{comment_id}/reactions/{emoji}")
async def remove_reaction(comment_id: str, emoji: str, user_id: str = Query(...)):
    """Remove uma reacao de um comentario."""
    if comment_id not in comments_db:
        raise HTTPException(status_code=404, detail="Comentario nao encontrado")

    comment = comments_db[comment_id]

    if emoji in comment.get("reactions", {}) and user_id in comment["reactions"][emoji]:
        comment["reactions"][emoji].remove(user_id)
        # Remove emoji key if no users left
        if not comment["reactions"][emoji]:
            del comment["reactions"][emoji]

    return {
        "success": True,
        "reactions": comment.get("reactions", {})
    }


# =============================================================================
# API ENDPOINTS - MENTIONS
# =============================================================================

@router.get("/users/mentions")
async def search_users_for_mention(q: str = Query("", min_length=0)):
    """Busca usuarios para @mention autocomplete."""
    query = q.lower()
    results = []

    for user_id, user in users_db.items():
        if query in user["name"].lower() or query in user_id.lower():
            results.append({
                "id": user_id,
                "name": user["name"],
                "avatar": user.get("avatar", ""),
                "mention": f"@{user_id}"
            })

    return {
        "users": results[:10]  # Limit to 10 results
    }


@router.get("/comments/{entity_type}/{entity_id}/count")
async def get_comments_count(entity_type: str, entity_id: str):
    """Retorna a contagem de comentarios para uma entidade."""
    count = get_comment_count(entity_type, entity_id)
    return {
        "entity_type": entity_type,
        "entity_id": entity_id,
        "count": count
    }


# =============================================================================
# API ENDPOINTS - AVAILABLE REACTIONS
# =============================================================================

@router.get("/comments/reactions/available")
async def get_available_reactions():
    """Retorna as reacoes disponiveis."""
    return {
        "reactions": REACTION_DISPLAY
    }


# =============================================================================
# HTML COMPONENT (Vue.js 3)
# =============================================================================

def get_comments_html():
    """Retorna o HTML do componente Vue.js para comentarios."""
    return '''
    <!-- Comments Panel (Issue #225) -->
    <div v-if="showCommentsPanel"
         class="fixed inset-0 bg-black/50 z-50 flex items-end md:items-center justify-center"
         @click.self="showCommentsPanel = false">
        <div class="bg-white rounded-t-2xl md:rounded-2xl shadow-2xl w-full md:max-w-2xl md:mx-4 max-h-[90vh] md:max-h-[80vh] overflow-hidden flex flex-col">
            <!-- Header -->
            <div class="bg-gradient-to-r from-[#003B4A] to-[#005f73] px-6 py-4 flex items-center justify-between">
                <div class="flex items-center gap-3">
                    <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"/>
                    </svg>
                    <div>
                        <h2 class="text-white text-lg font-bold">Comentarios</h2>
                        <p class="text-white/70 text-sm">{{ commentsEntity.type === 'story' ? 'Story' : 'Task' }}: {{ commentsEntity.title || commentsEntity.id }}</p>
                    </div>
                </div>
                <button @click="showCommentsPanel = false" class="text-white/80 hover:text-white transition">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <!-- Comments List -->
            <div class="flex-1 overflow-y-auto p-4 space-y-4" id="comments-list">
                <!-- Loading State -->
                <div v-if="commentsLoading" class="flex justify-center py-8">
                    <div class="animate-spin w-8 h-8 border-4 border-[#003B4A] border-t-transparent rounded-full"></div>
                </div>

                <!-- Empty State -->
                <div v-else-if="!comments.length" class="text-center py-12">
                    <svg class="w-16 h-16 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"/>
                    </svg>
                    <p class="text-gray-500">Nenhum comentario ainda</p>
                    <p class="text-gray-400 text-sm mt-1">Seja o primeiro a comentar!</p>
                </div>

                <!-- Comments Tree -->
                <template v-else v-for="comment in comments" :key="comment.id">
                    <div class="comment-item">
                        <!-- Main Comment -->
                        <div class="flex gap-3">
                            <img :src="'https://ui-avatars.com/api/?name=' + encodeURIComponent(comment.author_name) + '&background=003B4A&color=fff'"
                                 class="w-10 h-10 rounded-full flex-shrink-0" :alt="comment.author_name">
                            <div class="flex-1 min-w-0">
                                <div class="bg-gray-100 rounded-2xl rounded-tl-sm px-4 py-3">
                                    <div class="flex items-center gap-2 mb-1">
                                        <span class="font-semibold text-[#003B4A]">{{ comment.author_name }}</span>
                                        <span class="text-xs text-gray-400">{{ formatCommentDate(comment.created_at) }}</span>
                                        <span v-if="comment.is_edited" class="text-xs text-gray-400">(editado)</span>
                                    </div>
                                    <div class="text-gray-700 prose prose-sm max-w-none" v-html="renderMarkdown(comment.content)"></div>
                                </div>

                                <!-- Actions -->
                                <div class="flex items-center gap-4 mt-2 ml-2">
                                    <!-- Reactions -->
                                    <div class="flex items-center gap-1">
                                        <button v-for="(reaction, emoji) in availableReactions" :key="emoji"
                                                @click="toggleReaction(comment.id, emoji)"
                                                :class="[
                                                    'px-2 py-1 rounded-full text-sm transition-all',
                                                    hasUserReacted(comment, emoji)
                                                        ? 'bg-[#003B4A] text-white'
                                                        : 'hover:bg-gray-200 text-gray-600'
                                                ]">
                                            {{ reaction.emoji }}
                                            <span v-if="getReactionCount(comment, emoji)" class="ml-1">{{ getReactionCount(comment, emoji) }}</span>
                                        </button>
                                    </div>

                                    <!-- Reply Button -->
                                    <button @click="startReply(comment)" class="text-sm text-gray-500 hover:text-[#003B4A] transition">
                                        Responder
                                    </button>

                                    <!-- Edit/Delete (own comments) -->
                                    <template v-if="comment.author_id === currentUserId">
                                        <button @click="startEditComment(comment)" class="text-sm text-gray-500 hover:text-[#003B4A] transition">
                                            Editar
                                        </button>
                                        <button @click="deleteComment(comment.id)" class="text-sm text-gray-500 hover:text-red-500 transition">
                                            Excluir
                                        </button>
                                    </template>
                                </div>
                            </div>
                        </div>

                        <!-- Replies (nested) -->
                        <div v-if="comment.replies && comment.replies.length" class="ml-12 mt-3 space-y-3 border-l-2 border-gray-200 pl-4">
                            <div v-for="reply in comment.replies" :key="reply.id" class="flex gap-3">
                                <img :src="'https://ui-avatars.com/api/?name=' + encodeURIComponent(reply.author_name) + '&background=FF6C00&color=fff'"
                                     class="w-8 h-8 rounded-full flex-shrink-0" :alt="reply.author_name">
                                <div class="flex-1 min-w-0">
                                    <div class="bg-gray-50 rounded-2xl rounded-tl-sm px-3 py-2">
                                        <div class="flex items-center gap-2 mb-1">
                                            <span class="font-semibold text-sm text-[#003B4A]">{{ reply.author_name }}</span>
                                            <span class="text-xs text-gray-400">{{ formatCommentDate(reply.created_at) }}</span>
                                            <span v-if="reply.is_edited" class="text-xs text-gray-400">(editado)</span>
                                        </div>
                                        <div class="text-sm text-gray-700 prose prose-sm max-w-none" v-html="renderMarkdown(reply.content)"></div>
                                    </div>

                                    <!-- Reply Actions -->
                                    <div class="flex items-center gap-3 mt-1 ml-2">
                                        <div class="flex items-center gap-1">
                                            <button v-for="(reaction, emoji) in availableReactions" :key="emoji"
                                                    @click="toggleReaction(reply.id, emoji)"
                                                    :class="[
                                                        'px-1.5 py-0.5 rounded-full text-xs transition-all',
                                                        hasUserReacted(reply, emoji)
                                                            ? 'bg-[#003B4A] text-white'
                                                            : 'hover:bg-gray-200 text-gray-500'
                                                    ]">
                                                {{ reaction.emoji }}
                                                <span v-if="getReactionCount(reply, emoji)" class="ml-0.5">{{ getReactionCount(reply, emoji) }}</span>
                                            </button>
                                        </div>
                                        <template v-if="reply.author_id === currentUserId">
                                            <button @click="startEditComment(reply)" class="text-xs text-gray-500 hover:text-[#003B4A] transition">
                                                Editar
                                            </button>
                                            <button @click="deleteComment(reply.id)" class="text-xs text-gray-500 hover:text-red-500 transition">
                                                Excluir
                                            </button>
                                        </template>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </template>
            </div>

            <!-- Reply Banner -->
            <div v-if="replyingTo" class="px-4 py-2 bg-blue-50 border-t flex items-center justify-between">
                <span class="text-sm text-blue-700">
                    Respondendo a <strong>{{ replyingTo.author_name }}</strong>
                </span>
                <button @click="cancelReply" class="text-blue-500 hover:text-blue-700">
                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <!-- Edit Banner -->
            <div v-if="editingComment" class="px-4 py-2 bg-amber-50 border-t flex items-center justify-between">
                <span class="text-sm text-amber-700">
                    Editando comentario
                </span>
                <button @click="cancelEdit" class="text-amber-500 hover:text-amber-700">
                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <!-- Comment Input -->
            <div class="border-t p-4">
                <!-- Mention Autocomplete -->
                <div v-if="showMentionSuggestions" class="mb-2 bg-white border rounded-lg shadow-lg max-h-40 overflow-y-auto">
                    <button v-for="user in mentionSuggestions" :key="user.id"
                            @click="selectMention(user)"
                            class="w-full px-4 py-2 flex items-center gap-3 hover:bg-gray-100 transition text-left">
                        <img :src="user.avatar" class="w-8 h-8 rounded-full" :alt="user.name">
                        <div>
                            <div class="font-medium text-[#003B4A]">{{ user.name }}</div>
                            <div class="text-xs text-gray-500">{{ user.mention }}</div>
                        </div>
                    </button>
                </div>

                <!-- Markdown Preview Toggle -->
                <div class="flex items-center gap-2 mb-2">
                    <button @click="showPreview = false"
                            :class="['px-3 py-1 rounded text-sm transition', !showPreview ? 'bg-[#003B4A] text-white' : 'text-gray-600 hover:bg-gray-100']">
                        Escrever
                    </button>
                    <button @click="showPreview = true"
                            :class="['px-3 py-1 rounded text-sm transition', showPreview ? 'bg-[#003B4A] text-white' : 'text-gray-600 hover:bg-gray-100']">
                        Preview
                    </button>
                    <span class="text-xs text-gray-400 ml-auto">Markdown suportado</span>
                </div>

                <!-- Input or Preview -->
                <div v-if="!showPreview" class="relative">
                    <textarea v-model="newCommentContent"
                              @input="handleCommentInput"
                              @keydown.enter.ctrl="submitComment"
                              :placeholder="replyingTo ? 'Escreva sua resposta...' : 'Escreva um comentario... (@ para mencionar)'"
                              class="w-full border rounded-xl px-4 py-3 pr-12 resize-none focus:ring-2 focus:ring-[#003B4A] focus:border-transparent min-h-[80px]"
                              rows="3"></textarea>
                    <button @click="submitComment"
                            :disabled="!newCommentContent.trim()"
                            class="absolute right-3 bottom-3 bg-[#FF6C00] text-white p-2 rounded-full hover:bg-[#e55f00] transition disabled:opacity-50 disabled:cursor-not-allowed">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"/>
                        </svg>
                    </button>
                </div>
                <div v-else class="border rounded-xl px-4 py-3 min-h-[80px] bg-gray-50 prose prose-sm max-w-none"
                     v-html="renderMarkdown(newCommentContent || '*Nada para visualizar*')"></div>
            </div>
        </div>
    </div>
    '''


def get_comments_js():
    """Retorna o JavaScript/Vue.js para o sistema de comentarios."""
    return '''
    // Comments System Data (Issue #225)
    showCommentsPanel: false,
    commentsEntity: { type: '', id: '', title: '' },
    comments: [],
    commentsLoading: false,
    newCommentContent: '',
    replyingTo: null,
    editingComment: null,
    showPreview: false,
    showMentionSuggestions: false,
    mentionSuggestions: [],
    mentionSearchQuery: '',
    currentUserId: 'admin', // Set from auth in production
    availableReactions: {
        thumbsup: { emoji: '\\ud83d\\udc4d', label: 'Like' },
        thumbsdown: { emoji: '\\ud83d\\udc4e', label: 'Dislike' },
        heart: { emoji: '\\u2764\\ufe0f', label: 'Love' },
        tada: { emoji: '\\ud83c\\udf89', label: 'Celebrate' },
        confused: { emoji: '\\ud83d\\ude15', label: 'Confused' }
    },
    '''


def get_comments_methods():
    """Retorna os metodos Vue.js para o sistema de comentarios."""
    return '''
    // Comments Methods (Issue #225)
    async openComments(entityType, entityId, entityTitle) {
        this.commentsEntity = { type: entityType, id: entityId, title: entityTitle };
        this.showCommentsPanel = true;
        this.commentsLoading = true;
        this.comments = [];
        this.newCommentContent = '';
        this.replyingTo = null;
        this.editingComment = null;

        try {
            const response = await fetch(`/api/${entityType}s/${entityId}/comments`);
            const data = await response.json();
            this.comments = data.comments || [];
        } catch (error) {
            console.error('Error loading comments:', error);
            this.showNotification('Erro ao carregar comentarios', 'error');
        } finally {
            this.commentsLoading = false;
        }
    },

    async submitComment() {
        if (!this.newCommentContent.trim()) return;

        const payload = {
            entity_type: this.commentsEntity.type,
            entity_id: this.commentsEntity.id,
            parent_id: this.replyingTo ? this.replyingTo.id : null,
            author_id: this.currentUserId,
            author_name: 'Admin User', // Get from auth in production
            content: this.newCommentContent.trim()
        };

        try {
            if (this.editingComment) {
                // Update existing comment
                const response = await fetch(`/api/comments/${this.editingComment.id}`, {
                    method: 'PUT',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ content: this.newCommentContent.trim() })
                });
                if (response.ok) {
                    this.showNotification('Comentario atualizado!', 'success');
                }
            } else {
                // Create new comment
                const response = await fetch('/api/comments', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(payload)
                });
                if (response.ok) {
                    this.showNotification(this.replyingTo ? 'Resposta enviada!' : 'Comentario adicionado!', 'success');
                }
            }

            // Reload comments
            await this.openComments(this.commentsEntity.type, this.commentsEntity.id, this.commentsEntity.title);
            this.newCommentContent = '';
            this.replyingTo = null;
            this.editingComment = null;

        } catch (error) {
            console.error('Error submitting comment:', error);
            this.showNotification('Erro ao enviar comentario', 'error');
        }
    },

    async deleteComment(commentId) {
        if (!confirm('Tem certeza que deseja excluir este comentario?')) return;

        try {
            const response = await fetch(`/api/comments/${commentId}`, { method: 'DELETE' });
            if (response.ok) {
                this.showNotification('Comentario excluido!', 'success');
                await this.openComments(this.commentsEntity.type, this.commentsEntity.id, this.commentsEntity.title);
            }
        } catch (error) {
            console.error('Error deleting comment:', error);
            this.showNotification('Erro ao excluir comentario', 'error');
        }
    },

    async toggleReaction(commentId, emoji) {
        const comment = this.findComment(commentId);
        if (!comment) return;

        const hasReacted = this.hasUserReacted(comment, emoji);

        try {
            if (hasReacted) {
                await fetch(`/api/comments/${commentId}/reactions/${emoji}?user_id=${this.currentUserId}`, { method: 'DELETE' });
            } else {
                await fetch(`/api/comments/${commentId}/reactions`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ emoji, user_id: this.currentUserId })
                });
            }

            // Reload comments
            await this.openComments(this.commentsEntity.type, this.commentsEntity.id, this.commentsEntity.title);
        } catch (error) {
            console.error('Error toggling reaction:', error);
        }
    },

    findComment(commentId) {
        // Search in top-level and replies
        for (const comment of this.comments) {
            if (comment.id === commentId) return comment;
            if (comment.replies) {
                for (const reply of comment.replies) {
                    if (reply.id === commentId) return reply;
                }
            }
        }
        return null;
    },

    hasUserReacted(comment, emoji) {
        return comment.reactions &&
               comment.reactions[emoji] &&
               comment.reactions[emoji].includes(this.currentUserId);
    },

    getReactionCount(comment, emoji) {
        return (comment.reactions && comment.reactions[emoji]) ? comment.reactions[emoji].length : 0;
    },

    startReply(comment) {
        this.replyingTo = comment;
        this.editingComment = null;
        this.newCommentContent = '';
        this.$nextTick(() => {
            document.querySelector('.comments textarea')?.focus();
        });
    },

    cancelReply() {
        this.replyingTo = null;
        this.newCommentContent = '';
    },

    startEditComment(comment) {
        this.editingComment = comment;
        this.replyingTo = null;
        this.newCommentContent = comment.content;
    },

    cancelEdit() {
        this.editingComment = null;
        this.newCommentContent = '';
    },

    async handleCommentInput(event) {
        const text = event.target.value;
        const cursorPos = event.target.selectionStart;

        // Check for @ mention
        const textBeforeCursor = text.substring(0, cursorPos);
        const mentionMatch = textBeforeCursor.match(/@(\\w*)$/);

        if (mentionMatch) {
            this.mentionSearchQuery = mentionMatch[1];
            await this.searchMentions(mentionMatch[1]);
            this.showMentionSuggestions = this.mentionSuggestions.length > 0;
        } else {
            this.showMentionSuggestions = false;
        }
    },

    async searchMentions(query) {
        try {
            const response = await fetch(`/api/users/mentions?q=${encodeURIComponent(query)}`);
            const data = await response.json();
            this.mentionSuggestions = data.users || [];
        } catch (error) {
            console.error('Error searching mentions:', error);
        }
    },

    selectMention(user) {
        // Replace the @query with @username
        const regex = new RegExp(`@${this.mentionSearchQuery}$`);
        this.newCommentContent = this.newCommentContent.replace(regex, `@${user.id} `);
        this.showMentionSuggestions = false;
    },

    formatCommentDate(dateStr) {
        if (!dateStr) return '';
        const date = new Date(dateStr);
        const now = new Date();
        const diffMs = now - date;
        const diffMins = Math.floor(diffMs / 60000);
        const diffHours = Math.floor(diffMs / 3600000);
        const diffDays = Math.floor(diffMs / 86400000);

        if (diffMins < 1) return 'agora';
        if (diffMins < 60) return `${diffMins}m`;
        if (diffHours < 24) return `${diffHours}h`;
        if (diffDays < 7) return `${diffDays}d`;
        return date.toLocaleDateString('pt-BR');
    },

    renderMarkdown(text) {
        if (!text) return '';
        // Simple markdown rendering
        let html = text
            .replace(/\\*\\*(.+?)\\*\\*/g, '<strong>$1</strong>')
            .replace(/\\*(.+?)\\*/g, '<em>$1</em>')
            .replace(/`(.+?)`/g, '<code class="bg-gray-200 px-1 rounded">$1</code>')
            .replace(/@(\\w+)/g, '<span class="text-[#003B4A] font-semibold">@$1</span>')
            .replace(/\\n/g, '<br>');
        return html;
    },

    async getCommentCount(entityType, entityId) {
        try {
            const response = await fetch(`/api/comments/${entityType}/${entityId}/count`);
            const data = await response.json();
            return data.count || 0;
        } catch (error) {
            return 0;
        }
    },
    '''


def get_comments_card_badge():
    """Retorna o HTML do badge de contagem de comentarios para os cards."""
    return '''
    <!-- Comment Count Badge (Issue #225) -->
    <button @click.stop="openComments('story', story.story_id, story.title)"
            class="flex items-center gap-1 text-gray-500 hover:text-[#003B4A] transition"
            title="Ver comentarios">
        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"/>
        </svg>
        <span class="text-xs">{{ story.comment_count || 0 }}</span>
    </button>
    '''


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_comments(app):
    """Registra os endpoints de comentarios no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Comments endpoints loaded: /api/*/comments, /api/comments/*, /api/users/mentions")


# =============================================================================
# STANDALONE HTML PAGE FOR TESTING
# =============================================================================

@router.get("/comments-test", response_class=HTMLResponse)
async def comments_test_page():
    """Pagina de teste para o sistema de comentarios."""
    return f'''
    <!DOCTYPE html>
    <html lang="pt-BR">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Comments Test - Fabrica de Agentes</title>
        <script src="https://cdn.tailwindcss.com"></script>
        <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    </head>
    <body class="bg-gray-100 min-h-screen p-8">
        <div id="app">
            <div class="max-w-4xl mx-auto">
                <h1 class="text-3xl font-bold text-[#003B4A] mb-6">Comments System Test (Issue #225)</h1>

                <!-- Test Cards -->
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4 mb-8">
                    <div class="bg-white rounded-xl p-4 shadow-lg">
                        <h3 class="font-bold text-lg mb-2">Story: STR-0001</h3>
                        <p class="text-gray-600 mb-4">Test story para comentarios</p>
                        <button @click="openComments('story', 'STR-0001', 'Test Story')"
                                class="bg-[#003B4A] text-white px-4 py-2 rounded-lg hover:bg-[#002832] transition">
                            Ver Comentarios
                        </button>
                    </div>
                    <div class="bg-white rounded-xl p-4 shadow-lg">
                        <h3 class="font-bold text-lg mb-2">Task: TSK-0001</h3>
                        <p class="text-gray-600 mb-4">Test task para comentarios</p>
                        <button @click="openComments('task', 'TSK-0001', 'Test Task')"
                                class="bg-[#FF6C00] text-white px-4 py-2 rounded-lg hover:bg-[#e55f00] transition">
                            Ver Comentarios
                        </button>
                    </div>
                </div>

                <!-- Notification -->
                <div v-if="notification.show"
                     :class="['fixed top-4 right-4 px-6 py-3 rounded-lg shadow-lg transition-all z-50',
                              notification.type === 'success' ? 'bg-green-500 text-white' : 'bg-red-500 text-white']">
                    {{{{ notification.message }}}}
                </div>

                {get_comments_html()}
            </div>
        </div>

        <script>
        const {{ createApp }} = Vue;

        createApp({{
            data() {{
                return {{
                    {get_comments_js()}
                    notification: {{ show: false, message: '', type: 'success' }}
                }};
            }},
            methods: {{
                {get_comments_methods()}

                showNotification(message, type = 'success') {{
                    this.notification = {{ show: true, message, type }};
                    setTimeout(() => {{ this.notification.show = false; }}, 3000);
                }}
            }}
        }}).mount('#app');
        </script>
    </body>
    </html>
    '''
