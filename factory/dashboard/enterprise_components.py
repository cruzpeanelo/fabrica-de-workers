# -*- coding: utf-8 -*-
"""
Enterprise Components CSS - Issue #421
=======================================
Componentes visuais para dashboards enterprise:
- Dependency Graph
- Roadmap Timeline
- Sprint Planning
- Team Health Dashboard

Terminal C (UI/UX)
"""

from fastapi import APIRouter
from fastapi.responses import HTMLResponse

router = APIRouter(prefix="/api/enterprise-css", tags=["Enterprise CSS"])


def get_enterprise_css() -> str:
    """Retorna CSS para componentes enterprise."""
    return '''
/* ============================================================
   ENTERPRISE COMPONENTS CSS - Issue #421
   Terminal C (UI/UX)
   ============================================================ */

/* =============================================================
   1. DEPENDENCY GRAPH - Issue #243
   ============================================================= */

.dependency-graph-container {
    width: 100%;
    height: 600px;
    background: var(--bg-secondary, #F9FAFB);
    border-radius: 12px;
    overflow: hidden;
    position: relative;
}

.dark .dependency-graph-container {
    background: #1F2937;
}

/* Graph Canvas */
.graph-canvas {
    width: 100%;
    height: 100%;
    cursor: grab;
}

.graph-canvas:active {
    cursor: grabbing;
}

/* Graph Nodes (Stories) */
.graph-node {
    position: absolute;
    min-width: 180px;
    padding: 12px 16px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    cursor: pointer;
    transition: all 0.2s ease;
    border-left: 4px solid var(--node-color, #6B7280);
}

.dark .graph-node {
    background: #374151;
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
}

.graph-node:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 16px rgba(0,0,0,0.15);
    z-index: 10;
}

.graph-node.selected {
    ring: 2px solid var(--primary, #003B4A);
    box-shadow: 0 0 0 3px rgba(0,59,74,0.2);
}

/* Node Status Colors */
.graph-node.status-backlog { --node-color: #6B7280; }
.graph-node.status-ready { --node-color: #3B82F6; }
.graph-node.status-in_progress { --node-color: #F59E0B; }
.graph-node.status-review { --node-color: #8B5CF6; }
.graph-node.status-testing { --node-color: #EC4899; }
.graph-node.status-done { --node-color: #10B981; }
.graph-node.status-blocked { --node-color: #EF4444; }

.graph-node-id {
    font-size: 11px;
    font-weight: 600;
    color: var(--node-color);
    margin-bottom: 4px;
}

.graph-node-title {
    font-size: 13px;
    font-weight: 500;
    color: var(--text, #1F2937);
    line-height: 1.3;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
}

.dark .graph-node-title {
    color: #F3F4F6;
}

.graph-node-meta {
    display: flex;
    align-items: center;
    gap: 8px;
    margin-top: 8px;
    font-size: 11px;
    color: var(--text-secondary, #6B7280);
}

.graph-node-points {
    background: var(--node-color);
    color: white;
    padding: 2px 6px;
    border-radius: 4px;
    font-weight: 600;
}

/* Graph Edges (Connections) */
.graph-edge {
    stroke: #CBD5E1;
    stroke-width: 2;
    fill: none;
    transition: stroke 0.2s;
}

.dark .graph-edge {
    stroke: #4B5563;
}

.graph-edge.critical {
    stroke: #EF4444;
    stroke-width: 3;
    stroke-dasharray: none;
}

.graph-edge.blocked {
    stroke: #EF4444;
    stroke-dasharray: 5,5;
}

.graph-edge:hover {
    stroke: var(--primary, #003B4A);
    stroke-width: 3;
}

/* Edge Arrow */
.graph-edge-arrow {
    fill: #CBD5E1;
}

.dark .graph-edge-arrow {
    fill: #4B5563;
}

/* Critical Path Highlight */
.graph-node.critical-path {
    box-shadow: 0 0 0 3px #EF4444, 0 4px 16px rgba(239,68,68,0.3);
}

.graph-node.critical-path::before {
    content: "Critical Path";
    position: absolute;
    top: -24px;
    left: 50%;
    transform: translateX(-50%);
    background: #EF4444;
    color: white;
    font-size: 10px;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 4px;
    white-space: nowrap;
}

/* Graph Controls */
.graph-controls {
    position: absolute;
    top: 16px;
    right: 16px;
    display: flex;
    gap: 8px;
    z-index: 20;
}

.graph-control-btn {
    width: 36px;
    height: 36px;
    background: white;
    border: 1px solid #E5E7EB;
    border-radius: 8px;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    transition: all 0.2s;
}

.dark .graph-control-btn {
    background: #374151;
    border-color: #4B5563;
    color: #F3F4F6;
}

.graph-control-btn:hover {
    background: #F3F4F6;
    border-color: var(--primary);
}

.dark .graph-control-btn:hover {
    background: #4B5563;
}

/* Graph Legend */
.graph-legend {
    position: absolute;
    bottom: 16px;
    left: 16px;
    background: white;
    border-radius: 8px;
    padding: 12px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    z-index: 20;
}

.dark .graph-legend {
    background: #374151;
}

.graph-legend-title {
    font-size: 11px;
    font-weight: 600;
    color: var(--text-secondary);
    margin-bottom: 8px;
    text-transform: uppercase;
}

.graph-legend-item {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 12px;
    color: var(--text);
    margin-bottom: 4px;
}

.graph-legend-color {
    width: 12px;
    height: 12px;
    border-radius: 3px;
}


/* =============================================================
   2. ROADMAP TIMELINE - Issue #230
   ============================================================= */

.roadmap-container {
    width: 100%;
    overflow-x: auto;
    padding: 24px 0;
}

.roadmap-timeline {
    display: flex;
    gap: 0;
    min-width: max-content;
    position: relative;
    padding: 40px 24px 24px;
}

/* Timeline Base Line */
.roadmap-timeline::before {
    content: "";
    position: absolute;
    top: 60px;
    left: 24px;
    right: 24px;
    height: 4px;
    background: linear-gradient(90deg, #E5E7EB 0%, var(--primary) 50%, #E5E7EB 100%);
    border-radius: 2px;
}

.dark .roadmap-timeline::before {
    background: linear-gradient(90deg, #374151 0%, var(--primary) 50%, #374151 100%);
}

/* Quarter/Month Headers */
.roadmap-period {
    min-width: 280px;
    padding: 0 16px;
    position: relative;
}

.roadmap-period-header {
    position: absolute;
    top: -32px;
    left: 16px;
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
    background: var(--bg, #F3F4F6);
    padding: 4px 12px;
    border-radius: 4px;
}

.dark .roadmap-period-header {
    background: #1F2937;
    color: #F3F4F6;
}

/* Milestone Marker */
.roadmap-milestone {
    position: absolute;
    top: 52px;
    width: 20px;
    height: 20px;
    background: var(--primary, #003B4A);
    border: 3px solid white;
    border-radius: 50%;
    box-shadow: 0 2px 8px rgba(0,0,0,0.2);
    z-index: 5;
}

.dark .roadmap-milestone {
    border-color: #1F2937;
}

.roadmap-milestone.completed {
    background: #10B981;
}

.roadmap-milestone.current {
    background: #F59E0B;
    animation: pulse 2s infinite;
}

@keyframes pulse {
    0%, 100% { box-shadow: 0 0 0 0 rgba(245,158,11,0.4); }
    50% { box-shadow: 0 0 0 8px rgba(245,158,11,0); }
}

/* Epic Cards */
.roadmap-epics {
    margin-top: 40px;
    display: flex;
    flex-direction: column;
    gap: 12px;
}

.roadmap-epic-card {
    background: white;
    border-radius: 8px;
    padding: 16px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    border-left: 4px solid var(--epic-color, var(--primary));
    transition: all 0.2s;
    cursor: pointer;
}

.dark .roadmap-epic-card {
    background: #374151;
}

.roadmap-epic-card:hover {
    transform: translateX(4px);
    box-shadow: 0 4px 16px rgba(0,0,0,0.12);
}

.roadmap-epic-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 8px;
}

.roadmap-epic-title {
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
}

.dark .roadmap-epic-title {
    color: #F3F4F6;
}

.roadmap-epic-id {
    font-size: 11px;
    color: var(--epic-color, var(--primary));
    font-weight: 600;
}

.roadmap-epic-progress {
    height: 6px;
    background: #E5E7EB;
    border-radius: 3px;
    overflow: hidden;
    margin-top: 12px;
}

.dark .roadmap-epic-progress {
    background: #4B5563;
}

.roadmap-epic-progress-bar {
    height: 100%;
    background: var(--epic-color, var(--primary));
    border-radius: 3px;
    transition: width 0.3s ease;
}

.roadmap-epic-meta {
    display: flex;
    justify-content: space-between;
    margin-top: 8px;
    font-size: 11px;
    color: var(--text-secondary);
}

/* Today Marker */
.roadmap-today {
    position: absolute;
    top: 40px;
    bottom: 0;
    width: 2px;
    background: #EF4444;
    z-index: 10;
}

.roadmap-today::before {
    content: "Hoje";
    position: absolute;
    top: -20px;
    left: 50%;
    transform: translateX(-50%);
    background: #EF4444;
    color: white;
    font-size: 10px;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 4px;
    white-space: nowrap;
}


/* =============================================================
   3. SPRINT PLANNING VIEW - Issue #229
   ============================================================= */

.sprint-planning-container {
    display: grid;
    grid-template-columns: 300px 1fr;
    gap: 24px;
    height: calc(100vh - 200px);
    min-height: 500px;
}

/* Backlog Panel */
.sprint-backlog-panel {
    background: var(--bg-secondary, #F9FAFB);
    border-radius: 12px;
    padding: 16px;
    overflow-y: auto;
}

.dark .sprint-backlog-panel {
    background: #1F2937;
}

.sprint-backlog-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 16px;
    padding-bottom: 12px;
    border-bottom: 1px solid #E5E7EB;
}

.dark .sprint-backlog-header {
    border-color: #374151;
}

.sprint-backlog-title {
    font-size: 16px;
    font-weight: 600;
    color: var(--text);
}

.sprint-backlog-count {
    background: var(--primary);
    color: white;
    font-size: 12px;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 12px;
}

/* Sprint Board */
.sprint-board {
    display: flex;
    flex-direction: column;
    gap: 16px;
}

.sprint-row {
    background: white;
    border-radius: 12px;
    padding: 16px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
}

.dark .sprint-row {
    background: #374151;
}

.sprint-row-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 12px;
}

.sprint-row-title {
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
}

.sprint-row-capacity {
    display: flex;
    align-items: center;
    gap: 8px;
}

.sprint-capacity-bar {
    width: 120px;
    height: 8px;
    background: #E5E7EB;
    border-radius: 4px;
    overflow: hidden;
}

.dark .sprint-capacity-bar {
    background: #4B5563;
}

.sprint-capacity-fill {
    height: 100%;
    background: var(--primary);
    border-radius: 4px;
    transition: width 0.3s;
}

.sprint-capacity-fill.warning {
    background: #F59E0B;
}

.sprint-capacity-fill.over {
    background: #EF4444;
}

.sprint-capacity-text {
    font-size: 12px;
    font-weight: 500;
    color: var(--text-secondary);
    min-width: 60px;
}

/* Sprint Stories Drop Zone */
.sprint-stories-zone {
    min-height: 80px;
    background: #F9FAFB;
    border: 2px dashed #E5E7EB;
    border-radius: 8px;
    padding: 12px;
    display: flex;
    flex-wrap: wrap;
    gap: 8px;
    transition: all 0.2s;
}

.dark .sprint-stories-zone {
    background: #1F2937;
    border-color: #4B5563;
}

.sprint-stories-zone.drag-over {
    border-color: var(--primary);
    background: rgba(0,59,74,0.05);
}

.sprint-stories-zone.drag-over::after {
    content: "Solte aqui";
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    color: var(--primary);
    font-weight: 500;
}

/* Sprint Story Card (compact) */
.sprint-story-card {
    display: flex;
    align-items: center;
    gap: 8px;
    background: white;
    padding: 8px 12px;
    border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    cursor: grab;
    transition: all 0.2s;
}

.dark .sprint-story-card {
    background: #4B5563;
}

.sprint-story-card:hover {
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    transform: translateY(-1px);
}

.sprint-story-card.dragging {
    opacity: 0.5;
    cursor: grabbing;
}

.sprint-story-id {
    font-size: 11px;
    font-weight: 600;
    color: var(--primary);
}

.sprint-story-title {
    font-size: 12px;
    color: var(--text);
    flex: 1;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 150px;
}

.sprint-story-points {
    background: var(--primary);
    color: white;
    font-size: 10px;
    font-weight: 700;
    width: 22px;
    height: 22px;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
}

/* Velocity Chart */
.velocity-chart {
    background: white;
    border-radius: 12px;
    padding: 16px;
    margin-top: 16px;
}

.dark .velocity-chart {
    background: #374151;
}

.velocity-chart-title {
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
    margin-bottom: 12px;
}

.velocity-bars {
    display: flex;
    align-items: flex-end;
    gap: 8px;
    height: 100px;
}

.velocity-bar {
    flex: 1;
    background: var(--primary);
    border-radius: 4px 4px 0 0;
    min-height: 10px;
    position: relative;
    transition: height 0.3s;
}

.velocity-bar.current {
    background: #F59E0B;
}

.velocity-bar-label {
    position: absolute;
    bottom: -20px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 10px;
    color: var(--text-secondary);
    white-space: nowrap;
}


/* =============================================================
   4. TEAM HEALTH DASHBOARD - Issue #228
   ============================================================= */

.team-health-container {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
    gap: 20px;
}

/* Health Metric Card */
.health-card {
    background: white;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    transition: all 0.2s;
}

.dark .health-card {
    background: #374151;
}

.health-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 24px rgba(0,0,0,0.1);
}

.health-card-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 16px;
}

.health-card-title {
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
}

.dark .health-card-title {
    color: #F3F4F6;
}

.health-card-icon {
    width: 40px;
    height: 40px;
    border-radius: 10px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 20px;
}

.health-card-icon.positive { background: rgba(16,185,129,0.1); }
.health-card-icon.neutral { background: rgba(245,158,11,0.1); }
.health-card-icon.negative { background: rgba(239,68,68,0.1); }

.health-card-value {
    font-size: 32px;
    font-weight: 700;
    color: var(--text);
    line-height: 1;
}

.dark .health-card-value {
    color: #F3F4F6;
}

.health-card-trend {
    display: flex;
    align-items: center;
    gap: 4px;
    margin-top: 8px;
    font-size: 13px;
}

.health-card-trend.up { color: #10B981; }
.health-card-trend.down { color: #EF4444; }
.health-card-trend.stable { color: #6B7280; }

/* Team Mood */
.team-mood-container {
    display: flex;
    gap: 12px;
    margin-top: 16px;
}

.mood-indicator {
    flex: 1;
    text-align: center;
    padding: 12px;
    border-radius: 8px;
    cursor: pointer;
    transition: all 0.2s;
}

.mood-indicator:hover {
    transform: scale(1.05);
}

.mood-indicator.selected {
    box-shadow: 0 0 0 2px var(--primary);
}

.mood-emoji {
    font-size: 28px;
    margin-bottom: 4px;
}

.mood-label {
    font-size: 11px;
    color: var(--text-secondary);
}

.mood-count {
    font-size: 14px;
    font-weight: 600;
    color: var(--text);
}

/* Team Members Grid */
.team-members-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
    gap: 12px;
    margin-top: 16px;
}

.team-member-card {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 16px;
    background: var(--bg-secondary, #F9FAFB);
    border-radius: 10px;
    transition: all 0.2s;
}

.dark .team-member-card {
    background: #1F2937;
}

.team-member-card:hover {
    background: white;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
}

.dark .team-member-card:hover {
    background: #4B5563;
}

.team-member-avatar {
    width: 48px;
    height: 48px;
    border-radius: 50%;
    background: var(--primary);
    color: white;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 18px;
    font-weight: 600;
    margin-bottom: 8px;
}

.team-member-name {
    font-size: 13px;
    font-weight: 500;
    color: var(--text);
    text-align: center;
}

.team-member-role {
    font-size: 11px;
    color: var(--text-secondary);
}

.team-member-status {
    display: flex;
    align-items: center;
    gap: 4px;
    margin-top: 8px;
    font-size: 11px;
}

.team-member-status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
}

.team-member-status-dot.available { background: #10B981; }
.team-member-status-dot.busy { background: #F59E0B; }
.team-member-status-dot.away { background: #6B7280; }

/* Sprint Health Radar */
.sprint-health-radar {
    width: 200px;
    height: 200px;
    margin: 0 auto;
}

/* Burndown Mini Chart */
.burndown-mini {
    height: 80px;
    margin-top: 12px;
    background: linear-gradient(180deg, rgba(16,185,129,0.1) 0%, transparent 100%);
    border-radius: 8px;
    position: relative;
    overflow: hidden;
}

.burndown-line {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    height: 2px;
    background: #10B981;
}

.burndown-ideal {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    border-bottom: 2px dashed #CBD5E1;
    transform: rotate(-15deg);
    transform-origin: top left;
}


/* =============================================================
   5. SHARED COMPONENTS
   ============================================================= */

/* Loading State */
.enterprise-loading {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 40px;
    color: var(--text-secondary);
}

.enterprise-loading-spinner {
    width: 40px;
    height: 40px;
    border: 3px solid #E5E7EB;
    border-top-color: var(--primary);
    border-radius: 50%;
    animation: spin 1s linear infinite;
}

@keyframes spin {
    to { transform: rotate(360deg); }
}

/* Empty State */
.enterprise-empty {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 60px 40px;
    text-align: center;
}

.enterprise-empty-icon {
    font-size: 48px;
    margin-bottom: 16px;
    opacity: 0.5;
}

.enterprise-empty-title {
    font-size: 18px;
    font-weight: 600;
    color: var(--text);
    margin-bottom: 8px;
}

.enterprise-empty-text {
    font-size: 14px;
    color: var(--text-secondary);
    max-width: 300px;
}

/* Responsive */
@media (max-width: 768px) {
    .sprint-planning-container {
        grid-template-columns: 1fr;
    }

    .team-health-container {
        grid-template-columns: 1fr;
    }

    .roadmap-period {
        min-width: 200px;
    }

    .dependency-graph-container {
        height: 400px;
    }
}

'''


@router.get("/styles")
async def get_styles():
    """Retorna CSS para componentes enterprise."""
    return HTMLResponse(
        content=f"<style>{get_enterprise_css()}</style>",
        media_type="text/html"
    )


def get_enterprise_css_inline() -> str:
    """Retorna CSS inline para injetar no dashboard."""
    return get_enterprise_css()


def register_enterprise_components(app):
    """Registra rotas de componentes enterprise."""
    app.include_router(router)
    print("[Dashboard] Enterprise Components CSS loaded")
