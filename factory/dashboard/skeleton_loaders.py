# -*- coding: utf-8 -*-
"""
Skeleton Loaders - Issue #218
==============================
Implementacao de Skeleton Loaders e estados de carregamento elegantes.

Componentes:
- SkeletonCard: Placeholder para cards de Story/Task
- SkeletonList: Placeholder para listas
- SkeletonTable: Placeholder para tabelas
- SkeletonText: Placeholder para linhas de texto
- SkeletonAvatar: Placeholder para avatares
- SkeletonKanban: Placeholder para Kanban board completo
- SkeletonChart: Placeholder para graficos

Uso:
    from factory.dashboard.skeleton_loaders import (
        SKELETON_CSS,
        SKELETON_COMPONENTS,
        register_skeleton_endpoints
    )
"""

from fastapi import FastAPI
from fastapi.responses import HTMLResponse


# =============================================================================
# CSS STYLES FOR SKELETON LOADERS
# =============================================================================

SKELETON_CSS = """
/* =============================================================================
   SKELETON LOADERS - Issue #218
   Elegant loading states with shimmer animation
   ============================================================================= */

/* Base Skeleton Animation */
.skeleton-base {
    position: relative;
    overflow: hidden;
    background: #E5E7EB;
    border-radius: 4px;
}

.skeleton-base::after {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: linear-gradient(
        90deg,
        transparent 0%,
        rgba(255, 255, 255, 0.4) 50%,
        transparent 100%
    );
    animation: skeleton-shimmer 1.5s ease-in-out infinite;
}

@keyframes skeleton-shimmer {
    0% {
        transform: translateX(-100%);
    }
    100% {
        transform: translateX(100%);
    }
}

/* Alternative pulse animation */
.skeleton-pulse {
    background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
    background-size: 200% 100%;
    animation: skeleton-pulse-anim 1.5s ease-in-out infinite;
}

@keyframes skeleton-pulse-anim {
    0% { background-position: 200% 0; }
    100% { background-position: -200% 0; }
}

/* Dark Mode Support */
[data-theme="dark"] .skeleton-base,
.dark .skeleton-base {
    background: #374151;
}

[data-theme="dark"] .skeleton-base::after,
.dark .skeleton-base::after {
    background: linear-gradient(
        90deg,
        transparent 0%,
        rgba(255, 255, 255, 0.1) 50%,
        transparent 100%
    );
}

[data-theme="dark"] .skeleton-pulse,
.dark .skeleton-pulse {
    background: linear-gradient(90deg, #374151 25%, #4b5563 50%, #374151 75%);
}

/* Skeleton Card - Story/Task placeholder */
.skeleton-card {
    background: white;
    border: 1px solid #E5E7EB;
    border-radius: 8px;
    padding: 16px;
    margin-bottom: 8px;
}

.dark .skeleton-card {
    background: #1F2937;
    border-color: #374151;
}

.skeleton-card-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 12px;
}

.skeleton-card-badge {
    width: 60px;
    height: 20px;
}

.skeleton-card-points {
    width: 40px;
    height: 20px;
    border-radius: 10px;
}

.skeleton-card-title {
    height: 20px;
    width: 85%;
    margin-bottom: 8px;
}

.skeleton-card-subtitle {
    height: 14px;
    width: 65%;
    margin-bottom: 12px;
}

.skeleton-card-progress {
    height: 8px;
    width: 100%;
    margin-bottom: 8px;
    border-radius: 4px;
}

.skeleton-card-footer {
    display: flex;
    justify-content: space-between;
    align-items: center;
}

.skeleton-card-meta {
    width: 80px;
    height: 14px;
}

.skeleton-card-avatar {
    width: 24px;
    height: 24px;
    border-radius: 50%;
}

/* Skeleton List Item */
.skeleton-list-item {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 12px 16px;
    border-bottom: 1px solid #E5E7EB;
}

.dark .skeleton-list-item {
    border-color: #374151;
}

.skeleton-list-icon {
    width: 40px;
    height: 40px;
    border-radius: 8px;
    flex-shrink: 0;
}

.skeleton-list-content {
    flex: 1;
}

.skeleton-list-title {
    height: 16px;
    width: 70%;
    margin-bottom: 6px;
}

.skeleton-list-desc {
    height: 12px;
    width: 50%;
}

.skeleton-list-action {
    width: 60px;
    height: 28px;
    border-radius: 6px;
}

/* Skeleton Table */
.skeleton-table {
    width: 100%;
    border-collapse: collapse;
}

.skeleton-table-header {
    display: flex;
    gap: 16px;
    padding: 12px 16px;
    background: #F9FAFB;
    border-bottom: 2px solid #E5E7EB;
}

.dark .skeleton-table-header {
    background: #374151;
    border-color: #4B5563;
}

.skeleton-table-header-cell {
    height: 14px;
    flex: 1;
}

.skeleton-table-row {
    display: flex;
    gap: 16px;
    padding: 16px;
    border-bottom: 1px solid #E5E7EB;
}

.dark .skeleton-table-row {
    border-color: #374151;
}

.skeleton-table-cell {
    height: 16px;
    flex: 1;
}

.skeleton-table-cell.narrow {
    flex: 0.5;
}

.skeleton-table-cell.wide {
    flex: 2;
}

/* Skeleton Text */
.skeleton-text-line {
    height: 14px;
    margin-bottom: 8px;
    border-radius: 3px;
}

.skeleton-text-line:last-child {
    margin-bottom: 0;
}

.skeleton-text-line.title {
    height: 24px;
    width: 60%;
    margin-bottom: 16px;
}

.skeleton-text-line.subtitle {
    height: 18px;
    width: 40%;
    margin-bottom: 12px;
}

.skeleton-text-paragraph {
    margin-bottom: 16px;
}

.skeleton-text-paragraph .skeleton-text-line:nth-child(1) { width: 100%; }
.skeleton-text-paragraph .skeleton-text-line:nth-child(2) { width: 95%; }
.skeleton-text-paragraph .skeleton-text-line:nth-child(3) { width: 88%; }
.skeleton-text-paragraph .skeleton-text-line:nth-child(4) { width: 92%; }
.skeleton-text-paragraph .skeleton-text-line:nth-child(5) { width: 70%; }

/* Skeleton Avatar */
.skeleton-avatar {
    border-radius: 50%;
    flex-shrink: 0;
}

.skeleton-avatar.xs { width: 24px; height: 24px; }
.skeleton-avatar.sm { width: 32px; height: 32px; }
.skeleton-avatar.md { width: 40px; height: 40px; }
.skeleton-avatar.lg { width: 56px; height: 56px; }
.skeleton-avatar.xl { width: 80px; height: 80px; }

.skeleton-avatar-group {
    display: flex;
    margin-left: 8px;
}

.skeleton-avatar-group .skeleton-avatar {
    margin-left: -8px;
    border: 2px solid white;
}

.dark .skeleton-avatar-group .skeleton-avatar {
    border-color: #1F2937;
}

/* Skeleton Kanban Board */
.skeleton-kanban {
    display: flex;
    gap: 16px;
    padding: 16px;
    overflow-x: auto;
}

.skeleton-kanban-column {
    min-width: 280px;
    flex-shrink: 0;
}

.skeleton-kanban-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 16px;
    background: #F3F4F6;
    border-radius: 8px 8px 0 0;
    margin-bottom: 1px;
}

.dark .skeleton-kanban-header {
    background: #374151;
}

.skeleton-kanban-title {
    height: 18px;
    width: 100px;
}

.skeleton-kanban-count {
    width: 28px;
    height: 28px;
    border-radius: 50%;
}

.skeleton-kanban-cards {
    background: #F9FAFB;
    border-radius: 0 0 8px 8px;
    padding: 12px;
    min-height: 300px;
}

.dark .skeleton-kanban-cards {
    background: #1F2937;
}

/* Skeleton Chart */
.skeleton-chart {
    background: white;
    border: 1px solid #E5E7EB;
    border-radius: 12px;
    padding: 24px;
}

.dark .skeleton-chart {
    background: #1F2937;
    border-color: #374151;
}

.skeleton-chart-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 24px;
}

.skeleton-chart-title {
    height: 24px;
    width: 150px;
}

.skeleton-chart-legend {
    display: flex;
    gap: 16px;
}

.skeleton-chart-legend-item {
    display: flex;
    align-items: center;
    gap: 8px;
}

.skeleton-chart-legend-dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
}

.skeleton-chart-legend-label {
    width: 60px;
    height: 12px;
}

.skeleton-chart-body {
    position: relative;
    height: 250px;
}

/* Bar Chart Skeleton */
.skeleton-chart-bars {
    display: flex;
    align-items: flex-end;
    justify-content: space-around;
    height: 100%;
    padding: 0 20px;
}

.skeleton-chart-bar {
    width: 40px;
    border-radius: 4px 4px 0 0;
}

/* Line Chart Skeleton */
.skeleton-chart-line {
    position: absolute;
    bottom: 20%;
    left: 10%;
    right: 10%;
    height: 3px;
    border-radius: 2px;
}

.skeleton-chart-line::before,
.skeleton-chart-line::after {
    content: '';
    position: absolute;
    width: 12px;
    height: 12px;
    background: inherit;
    border-radius: 50%;
    top: 50%;
    transform: translateY(-50%);
}

.skeleton-chart-line::before { left: 0; }
.skeleton-chart-line::after { right: 0; }

/* Donut Chart Skeleton */
.skeleton-chart-donut {
    width: 200px;
    height: 200px;
    border-radius: 50%;
    margin: 0 auto;
    position: relative;
}

.skeleton-chart-donut::after {
    content: '';
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 100px;
    height: 100px;
    background: white;
    border-radius: 50%;
}

.dark .skeleton-chart-donut::after {
    background: #1F2937;
}

/* Skeleton Form */
.skeleton-form-group {
    margin-bottom: 20px;
}

.skeleton-form-label {
    height: 14px;
    width: 100px;
    margin-bottom: 8px;
}

.skeleton-form-input {
    height: 40px;
    width: 100%;
    border-radius: 6px;
}

.skeleton-form-textarea {
    height: 100px;
    width: 100%;
    border-radius: 6px;
}

.skeleton-form-button {
    height: 44px;
    width: 120px;
    border-radius: 6px;
}

/* Skeleton Stats Card */
.skeleton-stats-card {
    background: white;
    border: 1px solid #E5E7EB;
    border-radius: 12px;
    padding: 20px;
}

.dark .skeleton-stats-card {
    background: #1F2937;
    border-color: #374151;
}

.skeleton-stats-icon {
    width: 48px;
    height: 48px;
    border-radius: 12px;
    margin-bottom: 16px;
}

.skeleton-stats-value {
    height: 32px;
    width: 80px;
    margin-bottom: 8px;
}

.skeleton-stats-label {
    height: 14px;
    width: 100px;
}

.skeleton-stats-trend {
    height: 20px;
    width: 60px;
    border-radius: 10px;
    margin-top: 12px;
}

/* Skeleton Profile/User Card */
.skeleton-profile {
    display: flex;
    align-items: center;
    gap: 16px;
    padding: 16px;
}

.skeleton-profile-info {
    flex: 1;
}

.skeleton-profile-name {
    height: 18px;
    width: 120px;
    margin-bottom: 6px;
}

.skeleton-profile-role {
    height: 14px;
    width: 80px;
}

/* Skeleton Navigation/Tabs */
.skeleton-tabs {
    display: flex;
    gap: 8px;
    border-bottom: 2px solid #E5E7EB;
    padding-bottom: 2px;
}

.dark .skeleton-tabs {
    border-color: #374151;
}

.skeleton-tab {
    height: 36px;
    width: 100px;
    border-radius: 6px 6px 0 0;
}

/* Skeleton Breadcrumb */
.skeleton-breadcrumb {
    display: flex;
    align-items: center;
    gap: 8px;
}

.skeleton-breadcrumb-item {
    height: 16px;
    width: 80px;
}

.skeleton-breadcrumb-separator {
    height: 16px;
    width: 16px;
}

/* Gradual Reveal Animation */
.skeleton-reveal {
    animation: skeleton-reveal 0.5s ease forwards;
}

@keyframes skeleton-reveal {
    from {
        opacity: 0;
        transform: translateY(10px);
    }
    to {
        opacity: 1;
        transform: translateY(0);
    }
}

/* Skeleton Container with fade transition */
.skeleton-container {
    position: relative;
}

.skeleton-container .skeleton-content {
    transition: opacity 0.3s ease;
}

.skeleton-container.loaded .skeleton-content {
    opacity: 0;
    pointer-events: none;
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
}

.skeleton-container .real-content {
    opacity: 0;
    transition: opacity 0.3s ease 0.1s;
}

.skeleton-container.loaded .real-content {
    opacity: 1;
}

/* Utility classes for skeleton widths */
.skeleton-w-1\\/4 { width: 25%; }
.skeleton-w-1\\/3 { width: 33.333%; }
.skeleton-w-1\\/2 { width: 50%; }
.skeleton-w-2\\/3 { width: 66.666%; }
.skeleton-w-3\\/4 { width: 75%; }
.skeleton-w-full { width: 100%; }

/* Random widths for more natural look */
.skeleton-w-random-1 { width: 92%; }
.skeleton-w-random-2 { width: 78%; }
.skeleton-w-random-3 { width: 85%; }
.skeleton-w-random-4 { width: 68%; }
.skeleton-w-random-5 { width: 95%; }
"""


# =============================================================================
# VUE.JS COMPONENTS FOR SKELETON LOADERS
# =============================================================================

SKELETON_COMPONENTS = """
// =============================================================================
// SKELETON LOADER COMPONENTS - Issue #218
// Vue.js components for elegant loading states
// =============================================================================

// Skeleton Text Component
const SkeletonText = {
    props: {
        lines: { type: Number, default: 3 },
        title: { type: Boolean, default: false },
        paragraph: { type: Boolean, default: false }
    },
    template: `
        <div class="skeleton-text">
            <div v-if="title" class="skeleton-text-line title skeleton-base"></div>
            <div v-if="paragraph" class="skeleton-text-paragraph">
                <div v-for="i in lines" :key="i"
                     class="skeleton-text-line skeleton-base"
                     :style="{ width: getRandomWidth(i) }">
                </div>
            </div>
            <template v-else-if="!title">
                <div v-for="i in lines" :key="i"
                     class="skeleton-text-line skeleton-base"
                     :style="{ width: getRandomWidth(i) }">
                </div>
            </template>
        </div>
    `,
    methods: {
        getRandomWidth(index) {
            const widths = ['100%', '95%', '88%', '92%', '70%', '85%', '78%'];
            return widths[(index - 1) % widths.length];
        }
    }
};

// Skeleton Avatar Component
const SkeletonAvatar = {
    props: {
        size: { type: String, default: 'md' }, // xs, sm, md, lg, xl
        count: { type: Number, default: 1 },
        group: { type: Boolean, default: false }
    },
    template: `
        <div :class="{ 'skeleton-avatar-group': group }">
            <div v-for="i in count" :key="i"
                 :class="['skeleton-avatar', 'skeleton-base', size]">
            </div>
        </div>
    `
};

// Skeleton Card Component (Story/Task)
const SkeletonCard = {
    props: {
        count: { type: Number, default: 1 },
        showProgress: { type: Boolean, default: true },
        showFooter: { type: Boolean, default: true }
    },
    template: `
        <div v-for="i in count" :key="i" class="skeleton-card">
            <div class="skeleton-card-header">
                <div class="skeleton-card-badge skeleton-base"></div>
                <div class="skeleton-card-points skeleton-base"></div>
            </div>
            <div class="skeleton-card-title skeleton-base"></div>
            <div class="skeleton-card-subtitle skeleton-base"></div>
            <div v-if="showProgress" class="skeleton-card-progress skeleton-base"></div>
            <div v-if="showFooter" class="skeleton-card-footer">
                <div class="skeleton-card-meta skeleton-base"></div>
                <div class="skeleton-card-avatar skeleton-base"></div>
            </div>
        </div>
    `
};

// Skeleton List Component
const SkeletonList = {
    props: {
        count: { type: Number, default: 5 },
        showIcon: { type: Boolean, default: true },
        showAction: { type: Boolean, default: false }
    },
    template: `
        <div class="skeleton-list">
            <div v-for="i in count" :key="i" class="skeleton-list-item">
                <div v-if="showIcon" class="skeleton-list-icon skeleton-base"></div>
                <div class="skeleton-list-content">
                    <div class="skeleton-list-title skeleton-base"></div>
                    <div class="skeleton-list-desc skeleton-base"></div>
                </div>
                <div v-if="showAction" class="skeleton-list-action skeleton-base"></div>
            </div>
        </div>
    `
};

// Skeleton Table Component
const SkeletonTable = {
    props: {
        rows: { type: Number, default: 5 },
        columns: { type: Number, default: 4 },
        showHeader: { type: Boolean, default: true }
    },
    template: `
        <div class="skeleton-table">
            <div v-if="showHeader" class="skeleton-table-header">
                <div v-for="col in columns" :key="'h'+col"
                     class="skeleton-table-header-cell skeleton-base">
                </div>
            </div>
            <div v-for="row in rows" :key="row" class="skeleton-table-row">
                <div v-for="col in columns" :key="row+'-'+col"
                     :class="['skeleton-table-cell', 'skeleton-base', getCellClass(col)]">
                </div>
            </div>
        </div>
    `,
    methods: {
        getCellClass(col) {
            if (col === 1) return 'narrow';
            if (col === 2) return 'wide';
            return '';
        }
    }
};

// Skeleton Kanban Component
const SkeletonKanban = {
    props: {
        columns: { type: Number, default: 6 },
        cardsPerColumn: { type: Number, default: 3 }
    },
    computed: {
        columnNames() {
            return ['Backlog', 'Ready', 'In Progress', 'Review', 'Testing', 'Done'].slice(0, this.columns);
        }
    },
    template: `
        <div class="skeleton-kanban">
            <div v-for="(name, idx) in columnNames" :key="idx" class="skeleton-kanban-column">
                <div class="skeleton-kanban-header">
                    <div class="skeleton-kanban-title skeleton-base"></div>
                    <div class="skeleton-kanban-count skeleton-base"></div>
                </div>
                <div class="skeleton-kanban-cards">
                    <skeleton-card :count="getCardCount(idx)" :show-footer="true"></skeleton-card>
                </div>
            </div>
        </div>
    `,
    methods: {
        getCardCount(idx) {
            // Vary card count per column for more realistic look
            const counts = [2, 3, 2, 1, 1, 4];
            return Math.min(counts[idx] || 2, this.cardsPerColumn);
        }
    }
};

// Skeleton Chart Component
const SkeletonChart = {
    props: {
        type: { type: String, default: 'bar' }, // bar, line, donut
        showLegend: { type: Boolean, default: true },
        showTitle: { type: Boolean, default: true }
    },
    template: `
        <div class="skeleton-chart">
            <div v-if="showTitle" class="skeleton-chart-header">
                <div class="skeleton-chart-title skeleton-base"></div>
                <div v-if="showLegend" class="skeleton-chart-legend">
                    <div v-for="i in 3" :key="i" class="skeleton-chart-legend-item">
                        <div class="skeleton-chart-legend-dot skeleton-base"></div>
                        <div class="skeleton-chart-legend-label skeleton-base"></div>
                    </div>
                </div>
            </div>
            <div class="skeleton-chart-body">
                <!-- Bar Chart -->
                <div v-if="type === 'bar'" class="skeleton-chart-bars">
                    <div v-for="i in 7" :key="i"
                         class="skeleton-chart-bar skeleton-base"
                         :style="{ height: getBarHeight(i) }">
                    </div>
                </div>
                <!-- Line Chart -->
                <div v-else-if="type === 'line'">
                    <div class="skeleton-chart-line skeleton-base"></div>
                </div>
                <!-- Donut Chart -->
                <div v-else-if="type === 'donut'">
                    <div class="skeleton-chart-donut skeleton-base"></div>
                </div>
            </div>
        </div>
    `,
    methods: {
        getBarHeight(index) {
            const heights = ['60%', '80%', '45%', '90%', '70%', '55%', '75%'];
            return heights[(index - 1) % heights.length];
        }
    }
};

// Skeleton Stats Card Component
const SkeletonStats = {
    props: {
        count: { type: Number, default: 4 },
        showIcon: { type: Boolean, default: true },
        showTrend: { type: Boolean, default: true }
    },
    template: `
        <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
            <div v-for="i in count" :key="i" class="skeleton-stats-card">
                <div v-if="showIcon" class="skeleton-stats-icon skeleton-base"></div>
                <div class="skeleton-stats-value skeleton-base"></div>
                <div class="skeleton-stats-label skeleton-base"></div>
                <div v-if="showTrend" class="skeleton-stats-trend skeleton-base"></div>
            </div>
        </div>
    `
};

// Skeleton Form Component
const SkeletonForm = {
    props: {
        fields: { type: Number, default: 4 },
        showTextarea: { type: Boolean, default: true },
        showButton: { type: Boolean, default: true }
    },
    template: `
        <div class="skeleton-form">
            <div v-for="i in fields" :key="i" class="skeleton-form-group">
                <div class="skeleton-form-label skeleton-base"></div>
                <div v-if="showTextarea && i === fields"
                     class="skeleton-form-textarea skeleton-base">
                </div>
                <div v-else class="skeleton-form-input skeleton-base"></div>
            </div>
            <div v-if="showButton" class="skeleton-form-button skeleton-base"></div>
        </div>
    `
};

// Skeleton Profile Component
const SkeletonProfile = {
    props: {
        size: { type: String, default: 'lg' }
    },
    template: `
        <div class="skeleton-profile">
            <skeleton-avatar :size="size"></skeleton-avatar>
            <div class="skeleton-profile-info">
                <div class="skeleton-profile-name skeleton-base"></div>
                <div class="skeleton-profile-role skeleton-base"></div>
            </div>
        </div>
    `
};

// Skeleton Tabs Component
const SkeletonTabs = {
    props: {
        count: { type: Number, default: 4 }
    },
    template: `
        <div class="skeleton-tabs">
            <div v-for="i in count" :key="i" class="skeleton-tab skeleton-base"></div>
        </div>
    `
};

// Skeleton Breadcrumb Component
const SkeletonBreadcrumb = {
    props: {
        items: { type: Number, default: 3 }
    },
    template: `
        <div class="skeleton-breadcrumb">
            <template v-for="i in items" :key="i">
                <div class="skeleton-breadcrumb-item skeleton-base"></div>
                <div v-if="i < items" class="skeleton-breadcrumb-separator skeleton-base"></div>
            </template>
        </div>
    `
};

// Loading Container Component with transition
const SkeletonContainer = {
    props: {
        loading: { type: Boolean, default: true },
        minHeight: { type: String, default: '200px' }
    },
    template: `
        <div :class="['skeleton-container', { loaded: !loading }]" :style="{ minHeight }">
            <div class="skeleton-content">
                <slot name="skeleton"></slot>
            </div>
            <div class="real-content" :class="{ 'skeleton-reveal': !loading }">
                <slot></slot>
            </div>
        </div>
    `
};

// Register all skeleton components
function registerSkeletonComponents(app) {
    app.component('skeleton-text', SkeletonText);
    app.component('skeleton-avatar', SkeletonAvatar);
    app.component('skeleton-card', SkeletonCard);
    app.component('skeleton-list', SkeletonList);
    app.component('skeleton-table', SkeletonTable);
    app.component('skeleton-kanban', SkeletonKanban);
    app.component('skeleton-chart', SkeletonChart);
    app.component('skeleton-stats', SkeletonStats);
    app.component('skeleton-form', SkeletonForm);
    app.component('skeleton-profile', SkeletonProfile);
    app.component('skeleton-tabs', SkeletonTabs);
    app.component('skeleton-breadcrumb', SkeletonBreadcrumb);
    app.component('skeleton-container', SkeletonContainer);

    console.log('[Skeleton] All skeleton loader components registered');
}
"""


# =============================================================================
# DEMO/DOCUMENTATION PAGE
# =============================================================================

SKELETON_DEMO_HTML = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Skeleton Loaders - Demo</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <style>
        * { font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif; }
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
        }
        """ + SKELETON_CSS + """
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <div id="app" class="max-w-7xl mx-auto px-4 py-8">
        <!-- Header -->
        <div class="mb-8">
            <h1 class="text-3xl font-bold text-gray-900 mb-2">Skeleton Loaders</h1>
            <p class="text-gray-600">Issue #218 - Elegant loading states for UI components</p>
        </div>

        <!-- Dark Mode Toggle -->
        <div class="mb-6">
            <button @click="darkMode = !darkMode"
                    class="px-4 py-2 rounded-lg border"
                    :class="darkMode ? 'bg-gray-800 text-white' : 'bg-white'">
                {{ darkMode ? 'Light Mode' : 'Dark Mode' }}
            </button>
        </div>

        <div :class="{ dark: darkMode }">
            <!-- Stats Cards -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Stats Cards (skeleton-stats)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                    <skeleton-stats :count="4"></skeleton-stats>
                </div>
            </section>

            <!-- Cards -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Story/Task Cards (skeleton-card)</h2>
                <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <skeleton-card :count="1"></skeleton-card>
                    <skeleton-card :count="1" :show-progress="false"></skeleton-card>
                    <skeleton-card :count="1" :show-footer="false"></skeleton-card>
                </div>
            </section>

            <!-- Kanban Board -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Kanban Board (skeleton-kanban)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl overflow-hidden">
                    <skeleton-kanban :columns="6" :cards-per-column="3"></skeleton-kanban>
                </div>
            </section>

            <!-- Lists -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">List Items (skeleton-list)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl">
                    <skeleton-list :count="4" :show-action="true"></skeleton-list>
                </div>
            </section>

            <!-- Table -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Table (skeleton-table)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl overflow-hidden">
                    <skeleton-table :rows="5" :columns="5"></skeleton-table>
                </div>
            </section>

            <!-- Charts -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Charts (skeleton-chart)</h2>
                <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <skeleton-chart type="bar"></skeleton-chart>
                    <skeleton-chart type="line"></skeleton-chart>
                    <skeleton-chart type="donut"></skeleton-chart>
                </div>
            </section>

            <!-- Text -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Text Content (skeleton-text)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                    <skeleton-text :title="true"></skeleton-text>
                    <skeleton-text :paragraph="true" :lines="5"></skeleton-text>
                </div>
            </section>

            <!-- Form -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Form (skeleton-form)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6 max-w-md">
                    <skeleton-form :fields="4"></skeleton-form>
                </div>
            </section>

            <!-- Avatars -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Avatars (skeleton-avatar)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6 flex items-center gap-8">
                    <skeleton-avatar size="xs"></skeleton-avatar>
                    <skeleton-avatar size="sm"></skeleton-avatar>
                    <skeleton-avatar size="md"></skeleton-avatar>
                    <skeleton-avatar size="lg"></skeleton-avatar>
                    <skeleton-avatar size="xl"></skeleton-avatar>
                    <skeleton-avatar :count="4" :group="true" size="sm"></skeleton-avatar>
                </div>
            </section>

            <!-- Profile -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Profile (skeleton-profile)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl max-w-xs">
                    <skeleton-profile size="lg"></skeleton-profile>
                </div>
            </section>

            <!-- Tabs -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Tabs (skeleton-tabs)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                    <skeleton-tabs :count="5"></skeleton-tabs>
                </div>
            </section>

            <!-- Breadcrumb -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Breadcrumb (skeleton-breadcrumb)</h2>
                <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                    <skeleton-breadcrumb :items="4"></skeleton-breadcrumb>
                </div>
            </section>

            <!-- Live Demo with Loading Container -->
            <section class="mb-12">
                <h2 class="text-xl font-semibold mb-4">Loading Container Demo</h2>
                <div class="flex gap-4 mb-4">
                    <button @click="simulateLoad"
                            class="px-4 py-2 rounded-lg text-white"
                            :style="{ background: '#FF6C00' }">
                        Simulate Load ({{ loadingDemo ? 'Loading...' : 'Ready' }})
                    </button>
                </div>
                <skeleton-container :loading="loadingDemo" min-height="200px">
                    <template #skeleton>
                        <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                            <skeleton-stats :count="3"></skeleton-stats>
                        </div>
                    </template>
                    <div class="bg-white dark:bg-gray-800 rounded-xl p-6">
                        <div class="grid grid-cols-3 gap-4">
                            <div class="text-center">
                                <div class="text-3xl font-bold text-blue-600">127</div>
                                <div class="text-gray-500">Total Stories</div>
                            </div>
                            <div class="text-center">
                                <div class="text-3xl font-bold text-green-600">89</div>
                                <div class="text-gray-500">Completed</div>
                            </div>
                            <div class="text-center">
                                <div class="text-3xl font-bold text-orange-600">38</div>
                                <div class="text-gray-500">In Progress</div>
                            </div>
                        </div>
                    </div>
                </skeleton-container>
            </section>
        </div>
    </div>

    <script>
        const { createApp, ref } = Vue;

        """ + SKELETON_COMPONENTS + """

        const app = createApp({
            setup() {
                const darkMode = ref(false);
                const loadingDemo = ref(true);

                const simulateLoad = () => {
                    loadingDemo.value = true;
                    setTimeout(() => {
                        loadingDemo.value = false;
                    }, 2000);
                };

                return {
                    darkMode,
                    loadingDemo,
                    simulateLoad
                };
            }
        });

        registerSkeletonComponents(app);
        app.mount('#app');
    </script>
</body>
</html>
"""


def register_skeleton_endpoints(app: FastAPI):
    """
    Register skeleton loader endpoints
    """

    @app.get("/skeleton-demo", response_class=HTMLResponse)
    def skeleton_demo():
        """Demo page for skeleton loaders"""
        return SKELETON_DEMO_HTML

    @app.get("/api/skeleton/css")
    def get_skeleton_css():
        """Get skeleton loader CSS"""
        return {"css": SKELETON_CSS}

    @app.get("/api/skeleton/components")
    def get_skeleton_components():
        """Get skeleton loader Vue.js components"""
        return {"components": SKELETON_COMPONENTS}

    print("[Dashboard] Skeleton Loaders endpoints loaded: /skeleton-demo")


# Export for use in main app
__all__ = [
    'SKELETON_CSS',
    'SKELETON_COMPONENTS',
    'SKELETON_DEMO_HTML',
    'register_skeleton_endpoints'
]
