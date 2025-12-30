# -*- coding: utf-8 -*-
"""
Lazy Loading & Virtualization Module (Issue #269)
=================================================
Performance optimization with lazy loading and list virtualization.

Funcionalidades:
- Paginacao infinita com scroll
- Virtualizacao de listas longas
- Lazy loading de imagens e componentes
- Skeleton loaders
- Debounce de scroll/input
- Caching inteligente
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime

router = APIRouter(prefix="/api/perf", tags=["Performance"])


class PaginatedResponse(BaseModel):
    items: List[dict]
    total: int
    page: int
    page_size: int
    has_more: bool
    cursor: Optional[str] = None


@router.get("/stories/paginated")
async def get_stories_paginated(
    project_id: Optional[str] = Query(None),
    cursor: Optional[str] = Query(None),
    page_size: int = Query(20, ge=1, le=100),
    status: Optional[str] = Query(None),
    sort_by: str = Query("created_at"),
    sort_order: str = Query("desc")
):
    """Retorna stories com paginacao por cursor para scroll infinito."""
    # This would connect to the actual database
    # For now, return mock paginated response
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        query = db.query(Story)

        if project_id:
            query = query.filter(Story.project_id == project_id)

        if status:
            query = query.filter(Story.status == status)

        # Apply cursor-based pagination
        if cursor:
            query = query.filter(Story.story_id > cursor)

        # Order
        order_col = getattr(Story, sort_by, Story.created_at)
        if sort_order == "desc":
            query = query.order_by(order_col.desc())
        else:
            query = query.order_by(order_col.asc())

        # Get total count (cached ideally)
        total = query.count()

        # Get page of items
        items = query.limit(page_size + 1).all()

        has_more = len(items) > page_size
        items = items[:page_size]

        next_cursor = items[-1].story_id if items and has_more else None

        return {
            "items": [story_to_dict(s) for s in items],
            "total": total,
            "page_size": page_size,
            "has_more": has_more,
            "cursor": next_cursor
        }
    except Exception as e:
        return {
            "items": [],
            "total": 0,
            "page_size": page_size,
            "has_more": False,
            "cursor": None,
            "error": str(e)
        }
    finally:
        db.close()


@router.get("/tasks/paginated")
async def get_tasks_paginated(
    story_id: Optional[str] = Query(None),
    cursor: Optional[str] = Query(None),
    page_size: int = Query(30, ge=1, le=100),
    status: Optional[str] = Query(None)
):
    """Retorna tasks com paginacao por cursor."""
    from factory.database.connection import SessionLocal
    from factory.database.models import StoryTask

    db = SessionLocal()
    try:
        query = db.query(StoryTask)

        if story_id:
            query = query.filter(StoryTask.story_id == story_id)

        if status:
            query = query.filter(StoryTask.status == status)

        if cursor:
            query = query.filter(StoryTask.task_id > cursor)

        query = query.order_by(StoryTask.created_at.desc())

        total = query.count()
        items = query.limit(page_size + 1).all()

        has_more = len(items) > page_size
        items = items[:page_size]

        next_cursor = items[-1].task_id if items and has_more else None

        return {
            "items": [task_to_dict(t) for t in items],
            "total": total,
            "page_size": page_size,
            "has_more": has_more,
            "cursor": next_cursor
        }
    except Exception as e:
        return {
            "items": [],
            "total": 0,
            "page_size": page_size,
            "has_more": False,
            "cursor": None,
            "error": str(e)
        }
    finally:
        db.close()


@router.get("/kanban/virtualized")
async def get_kanban_virtualized(
    project_id: str,
    visible_columns: str = Query("backlog,ready,in_progress,review,testing,done"),
    items_per_column: int = Query(20)
):
    """Retorna dados do Kanban otimizados para virtualizacao."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    columns = visible_columns.split(",")

    db = SessionLocal()
    try:
        result = {}

        for col in columns:
            query = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == col
            ).order_by(Story.updated_at.desc())

            total = query.count()
            items = query.limit(items_per_column).all()

            result[col] = {
                "items": [story_to_dict(s) for s in items],
                "total": total,
                "loaded": len(items),
                "has_more": total > len(items)
            }

        return result
    except Exception as e:
        return {"error": str(e)}
    finally:
        db.close()


@router.get("/kanban/column/{status}")
async def get_kanban_column_items(
    status: str,
    project_id: str = Query(...),
    offset: int = Query(0),
    limit: int = Query(20)
):
    """Carrega mais itens de uma coluna especifica do Kanban."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        query = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == status
        ).order_by(Story.updated_at.desc())

        total = query.count()
        items = query.offset(offset).limit(limit).all()

        return {
            "status": status,
            "items": [story_to_dict(s) for s in items],
            "total": total,
            "offset": offset,
            "has_more": offset + len(items) < total
        }
    except Exception as e:
        return {"error": str(e)}
    finally:
        db.close()


def story_to_dict(story) -> dict:
    """Convert Story model to dict."""
    return {
        "story_id": story.story_id,
        "title": story.title,
        "description": story.description,
        "status": story.status,
        "priority": story.priority,
        "story_points": story.story_points,
        "assignee": story.assignee,
        "created_at": story.created_at.isoformat() if story.created_at else None,
        "updated_at": story.updated_at.isoformat() if story.updated_at else None
    }


def task_to_dict(task) -> dict:
    """Convert Task model to dict."""
    return {
        "task_id": task.task_id,
        "story_id": task.story_id,
        "title": task.title,
        "status": task.status,
        "progress": task.progress,
        "created_at": task.created_at.isoformat() if task.created_at else None
    }


def get_lazy_loading_html():
    """Retorna componentes HTML para lazy loading."""
    return '''
    <!-- Skeleton Loaders -->
    <template id="story-card-skeleton">
        <div class="skeleton-card bg-white rounded-lg shadow p-4 animate-pulse">
            <div class="flex items-center justify-between mb-3">
                <div class="skeleton skeleton-text" style="width: 60px; height: 16px;"></div>
                <div class="skeleton skeleton-circle" style="width: 24px; height: 24px;"></div>
            </div>
            <div class="skeleton skeleton-text" style="width: 100%; height: 20px; margin-bottom: 8px;"></div>
            <div class="skeleton skeleton-text" style="width: 80%; height: 16px; margin-bottom: 12px;"></div>
            <div class="flex items-center gap-2">
                <div class="skeleton" style="width: 60px; height: 24px; border-radius: 12px;"></div>
                <div class="skeleton" style="width: 80px; height: 24px; border-radius: 12px;"></div>
            </div>
        </div>
    </template>

    <template id="table-row-skeleton">
        <tr class="animate-pulse">
            <td class="px-4 py-3"><div class="skeleton skeleton-text" style="width: 80%;"></div></td>
            <td class="px-4 py-3"><div class="skeleton skeleton-text" style="width: 60%;"></div></td>
            <td class="px-4 py-3"><div class="skeleton" style="width: 60px; height: 24px; border-radius: 12px;"></div></td>
            <td class="px-4 py-3"><div class="skeleton skeleton-circle" style="width: 32px; height: 32px;"></div></td>
        </tr>
    </template>

    <!-- Intersection Observer Target -->
    <div ref="infiniteScrollTrigger"
         class="infinite-scroll-trigger h-4 flex items-center justify-center"
         v-show="hasMore && !isLoadingMore">
        <span class="text-sm text-gray-400">Carregando mais...</span>
    </div>

    <!-- Loading More Indicator -->
    <div v-if="isLoadingMore" class="flex items-center justify-center py-4">
        <svg class="animate-spin h-6 w-6 text-blue-600" fill="none" viewBox="0 0 24 24">
            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
        </svg>
    </div>
    '''


def get_lazy_loading_css():
    """Retorna CSS para skeleton loaders e lazy loading."""
    return '''
    <style>
    /* Skeleton Loaders */
    .skeleton {
        background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
        background-size: 200% 100%;
        animation: skeleton-loading 1.5s infinite;
        border-radius: 4px;
    }

    .skeleton-text {
        height: 16px;
        border-radius: 4px;
    }

    .skeleton-circle {
        border-radius: 50%;
    }

    .skeleton-card {
        min-height: 120px;
    }

    @keyframes skeleton-loading {
        0% { background-position: 200% 0; }
        100% { background-position: -200% 0; }
    }

    /* Fade in for lazy loaded content */
    .lazy-loaded {
        animation: fadeIn 0.3s ease-in;
    }

    @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
    }

    /* Virtual list container */
    .virtual-list {
        overflow-y: auto;
        contain: strict;
    }

    .virtual-list-item {
        contain: layout style;
    }

    /* Smooth scroll */
    .smooth-scroll {
        scroll-behavior: smooth;
    }

    /* Reduce motion preference */
    @media (prefers-reduced-motion: reduce) {
        .skeleton,
        .lazy-loaded {
            animation: none;
        }
        .smooth-scroll {
            scroll-behavior: auto;
        }
    }
    </style>
    '''


def get_lazy_loading_js():
    """Retorna JavaScript para lazy loading e virtualizacao."""
    return '''
    // Lazy Loading & Virtualization State
    isLoadingMore: false,
    hasMore: true,
    currentCursor: null,
    pageSize: 20,
    loadedItems: [],
    virtualizedColumns: {},
    intersectionObserver: null,

    // Debounce utility
    debounce(func, wait) {
        let timeout;
        return function executedFunction(...args) {
            const later = () => {
                clearTimeout(timeout);
                func(...args);
            };
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
        };
    },

    // Throttle utility
    throttle(func, limit) {
        let inThrottle;
        return function(...args) {
            if (!inThrottle) {
                func.apply(this, args);
                inThrottle = true;
                setTimeout(() => inThrottle = false, limit);
            }
        };
    },

    // Initialize Infinite Scroll
    initInfiniteScroll() {
        const options = {
            root: null,
            rootMargin: '100px',
            threshold: 0.1
        };

        this.intersectionObserver = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting && this.hasMore && !this.isLoadingMore) {
                    this.loadMore();
                }
            });
        }, options);

        // Observe the trigger element
        this.$nextTick(() => {
            const trigger = this.$refs.infiniteScrollTrigger;
            if (trigger) {
                this.intersectionObserver.observe(trigger);
            }
        });
    },

    // Load more items (paginated)
    async loadMore() {
        if (this.isLoadingMore || !this.hasMore) return;

        this.isLoadingMore = true;

        try {
            const params = new URLSearchParams({
                project_id: this.currentProject,
                page_size: this.pageSize.toString()
            });

            if (this.currentCursor) {
                params.append('cursor', this.currentCursor);
            }

            const response = await fetch(`/api/perf/stories/paginated?${params}`);
            const data = await response.json();

            // Append new items
            this.loadedItems = [...this.loadedItems, ...data.items];
            this.currentCursor = data.cursor;
            this.hasMore = data.has_more;

        } catch (e) {
            console.error('Error loading more items:', e);
        } finally {
            this.isLoadingMore = false;
        }
    },

    // Load Kanban with virtualization
    async loadVirtualizedKanban() {
        try {
            const response = await fetch(
                `/api/perf/kanban/virtualized?project_id=${this.currentProject}&items_per_column=15`
            );
            const data = await response.json();
            this.virtualizedColumns = data;
        } catch (e) {
            console.error('Error loading virtualized kanban:', e);
        }
    },

    // Load more items for a specific column
    async loadMoreColumn(status) {
        const column = this.virtualizedColumns[status];
        if (!column || !column.has_more) return;

        try {
            const response = await fetch(
                `/api/perf/kanban/column/${status}?project_id=${this.currentProject}&offset=${column.loaded}&limit=15`
            );
            const data = await response.json();

            // Append to column
            column.items = [...column.items, ...data.items];
            column.loaded = column.items.length;
            column.has_more = data.has_more;

        } catch (e) {
            console.error('Error loading more column items:', e);
        }
    },

    // Lazy load images
    initLazyImages() {
        const imageObserver = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    const img = entry.target;
                    img.src = img.dataset.src;
                    img.classList.add('lazy-loaded');
                    imageObserver.unobserve(img);
                }
            });
        });

        document.querySelectorAll('img[data-src]').forEach(img => {
            imageObserver.observe(img);
        });
    },

    // Reset pagination (on filter change)
    resetPagination() {
        this.loadedItems = [];
        this.currentCursor = null;
        this.hasMore = true;
        this.loadMore();
    },

    // Debounced search
    debouncedSearch: null,

    initDebouncedSearch() {
        this.debouncedSearch = this.debounce((query) => {
            this.performSearch(query);
        }, 300);
    },

    // Cleanup on unmount
    cleanupLazyLoading() {
        if (this.intersectionObserver) {
            this.intersectionObserver.disconnect();
        }
    }
    '''


def register_lazy_loading(app):
    """Registra os endpoints de performance no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Lazy Loading/Virtualization endpoints loaded: /api/perf/*")
