/**
 * Roadmap Timeline - Plataforma E v6.5
 *
 * Gantt-style timeline visualization for epics and releases.
 * Supports: drag dates, milestones, dependencies, zoom levels.
 *
 * Issue #230: [FRONT] Implementar Roadmap Timeline com visualizacao de epics
 */

class RoadmapTimeline {
    constructor(containerId, options = {}) {
        this.containerId = containerId;
        this.container = document.getElementById(containerId);

        if (!this.container) {
            console.error(`Container ${containerId} not found`);
            return;
        }

        this.options = {
            projectId: options.projectId || 'default',
            zoom: options.zoom || 'quarter', // month, quarter, year
            startDate: options.startDate || new Date(new Date().getFullYear(), 0, 1),
            endDate: options.endDate || new Date(new Date().getFullYear(), 11, 31),
            rowHeight: options.rowHeight || 80,
            dayWidth: options.dayWidth || 3,
            colors: {
                onTrack: '#10B981',
                atRisk: '#F59E0B',
                delayed: '#EF4444',
                blocked: '#374151',
                notStarted: '#9CA3AF',
                ...options.colors
            }
        };

        this.epics = [];
        this.milestones = [];
        this.dependencies = [];
        this.today = new Date();

        this.init();
    }

    init() {
        this.render();
        this.loadData();
    }

    render() {
        this.container.innerHTML = `
            <div class="roadmap-container">
                <div class="roadmap-header">
                    <div class="roadmap-title">
                        <h2>🗺️ Roadmap ${this.today.getFullYear()}</h2>
                    </div>
                    <div class="roadmap-controls">
                        <select id="roadmap-zoom" class="roadmap-select">
                            <option value="month">Month</option>
                            <option value="quarter" selected>Quarter</option>
                            <option value="year">Year</option>
                        </select>
                        <button class="roadmap-btn" id="roadmap-add-epic" title="Add Epic">
                            + Add Epic
                        </button>
                        <button class="roadmap-btn" id="roadmap-export" title="Export">
                            Export
                        </button>
                    </div>
                </div>
                <div class="roadmap-filters">
                    <div class="filter-group">
                        <label>View:</label>
                        <button class="filter-btn active" data-view="timeline">Timeline</button>
                        <button class="filter-btn" data-view="kanban">Kanban</button>
                        <button class="filter-btn" data-view="list">List</button>
                    </div>
                    <div class="filter-group">
                        <label>Status:</label>
                        <label class="filter-check">
                            <input type="checkbox" checked data-status="done"> ✅ Done
                        </label>
                        <label class="filter-check">
                            <input type="checkbox" checked data-status="in_progress"> 🔄 In Progress
                        </label>
                        <label class="filter-check">
                            <input type="checkbox" checked data-status="not_started"> ⬜ Not Started
                        </label>
                    </div>
                </div>
                <div class="roadmap-timeline" id="roadmap-timeline">
                    <div class="timeline-header" id="timeline-header"></div>
                    <div class="timeline-body" id="timeline-body">
                        <div class="timeline-grid" id="timeline-grid"></div>
                        <div class="timeline-epics" id="timeline-epics"></div>
                        <div class="timeline-today" id="timeline-today"></div>
                        <div class="timeline-milestones" id="timeline-milestones"></div>
                    </div>
                </div>
                <div class="roadmap-legend">
                    <div class="legend-item"><span class="legend-dot on-track"></span> On Track</div>
                    <div class="legend-item"><span class="legend-dot at-risk"></span> At Risk</div>
                    <div class="legend-item"><span class="legend-dot delayed"></span> Delayed</div>
                    <div class="legend-item"><span class="legend-dot blocked"></span> Blocked</div>
                </div>
            </div>
        `;

        // Event listeners
        document.getElementById('roadmap-zoom')?.addEventListener('change', (e) => {
            this.options.zoom = e.target.value;
            this.updateTimeline();
        });

        document.getElementById('roadmap-add-epic')?.addEventListener('click', () => this.addEpic());
        document.getElementById('roadmap-export')?.addEventListener('click', () => this.export());

        // Filter buttons
        document.querySelectorAll('.filter-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
                e.target.classList.add('active');
            });
        });
    }

    async loadData() {
        try {
            const response = await fetch(`/api/projects/${this.options.projectId}/roadmap`);
            if (!response.ok) throw new Error('Failed to load roadmap');

            const data = await response.json();
            this.epics = data.epics || [];
            this.milestones = data.milestones || [];
            this.dependencies = data.dependencies || [];

            this.updateTimeline();
        } catch (error) {
            console.error('Error loading roadmap:', error);
            this.showError('Failed to load roadmap data');
        }
    }

    updateTimeline() {
        this.renderHeader();
        this.renderGrid();
        this.renderEpics();
        this.renderToday();
        this.renderMilestones();
    }

    renderHeader() {
        const header = document.getElementById('timeline-header');
        const { startDate, endDate, zoom, dayWidth } = this.options;

        let html = '<div class="header-row">';

        if (zoom === 'month') {
            // Show days
            const current = new Date(startDate);
            while (current <= endDate) {
                const width = dayWidth;
                html += `<div class="header-cell" style="width: ${width}px">
                    ${current.getDate()}
                </div>`;
                current.setDate(current.getDate() + 1);
            }
        } else if (zoom === 'quarter') {
            // Show weeks grouped by month
            const months = this.getMonthsInRange(startDate, endDate);
            for (const month of months) {
                const daysInMonth = new Date(month.year, month.month + 1, 0).getDate();
                const width = daysInMonth * dayWidth;
                html += `<div class="header-cell month-header" style="width: ${width}px">
                    ${this.getMonthName(month.month)} ${month.year}
                </div>`;
            }
        } else {
            // Show months
            const months = this.getMonthsInRange(startDate, endDate);
            for (const month of months) {
                const daysInMonth = new Date(month.year, month.month + 1, 0).getDate();
                const width = daysInMonth * dayWidth;
                html += `<div class="header-cell month-header" style="width: ${width}px">
                    ${this.getMonthName(month.month).substring(0, 3)}
                </div>`;
            }
        }

        html += '</div>';
        header.innerHTML = html;
    }

    renderGrid() {
        const grid = document.getElementById('timeline-grid');
        const { startDate, endDate, dayWidth } = this.options;
        const totalDays = this.getDaysBetween(startDate, endDate);
        const width = totalDays * dayWidth;

        let html = `<div class="grid-container" style="width: ${width}px">`;

        // Monthly grid lines
        const months = this.getMonthsInRange(startDate, endDate);
        let offset = 0;
        for (const month of months) {
            const daysInMonth = new Date(month.year, month.month + 1, 0).getDate();
            const monthWidth = daysInMonth * dayWidth;
            html += `<div class="grid-line" style="left: ${offset}px"></div>`;
            offset += monthWidth;
        }

        html += '</div>';
        grid.innerHTML = html;
    }

    renderEpics() {
        const container = document.getElementById('timeline-epics');
        const { startDate, dayWidth, rowHeight } = this.options;

        let html = '';
        this.epics.forEach((epic, index) => {
            const epicStart = new Date(epic.start_date);
            const epicEnd = new Date(epic.target_date);
            const left = this.getDaysBetween(startDate, epicStart) * dayWidth;
            const width = Math.max(50, this.getDaysBetween(epicStart, epicEnd) * dayWidth);
            const top = index * rowHeight + 10;

            const status = this.calculateStatus(epic);
            const progress = epic.progress || 0;

            html += `
                <div class="epic-bar ${status}" style="left: ${left}px; width: ${width}px; top: ${top}px"
                     data-epic-id="${epic.id}" draggable="true">
                    <div class="epic-header">
                        <span class="epic-icon">${epic.icon || '📦'}</span>
                        <span class="epic-name">${epic.name}</span>
                    </div>
                    <div class="epic-meta">
                        ${epic.stories_count || 0} stories | ${epic.story_points || 0} pts
                    </div>
                    <div class="epic-progress-bar">
                        <div class="epic-progress-fill" style="width: ${progress}%"></div>
                    </div>
                    <div class="epic-progress-text">${progress}%</div>
                    <div class="epic-status-badge ${status}">${this.getStatusLabel(status)}</div>
                </div>
            `;
        });

        container.innerHTML = html;
        container.style.height = `${this.epics.length * rowHeight + 20}px`;

        // Add drag handlers
        document.querySelectorAll('.epic-bar').forEach(bar => {
            bar.addEventListener('click', (e) => this.onEpicClick(e.target.closest('.epic-bar').dataset.epicId));
            bar.addEventListener('dragstart', (e) => this.onDragStart(e));
            bar.addEventListener('dragend', (e) => this.onDragEnd(e));
        });
    }

    renderToday() {
        const todayLine = document.getElementById('timeline-today');
        const { startDate, dayWidth } = this.options;
        const daysFromStart = this.getDaysBetween(startDate, this.today);
        const left = daysFromStart * dayWidth;

        todayLine.innerHTML = `
            <div class="today-line" style="left: ${left}px">
                <div class="today-marker">📍 Today</div>
            </div>
        `;
    }

    renderMilestones() {
        const container = document.getElementById('timeline-milestones');
        const { startDate, dayWidth } = this.options;

        let html = '';
        this.milestones.forEach(milestone => {
            const milestoneDate = new Date(milestone.date);
            const left = this.getDaysBetween(startDate, milestoneDate) * dayWidth;

            html += `
                <div class="milestone" style="left: ${left}px" data-milestone-id="${milestone.id}">
                    <div class="milestone-icon">${milestone.icon || '🎯'}</div>
                    <div class="milestone-name">${milestone.name}</div>
                    <div class="milestone-date">${this.formatDate(milestoneDate)}</div>
                </div>
            `;
        });

        container.innerHTML = html;
    }

    calculateStatus(epic) {
        const today = new Date();
        const targetDate = new Date(epic.target_date);
        const progress = epic.progress || 0;
        const daysRemaining = this.getDaysBetween(today, targetDate);

        if (epic.blocked) return 'blocked';
        if (progress === 100) return 'done';
        if (today > targetDate) return 'delayed';

        // Calculate expected progress based on time
        const startDate = new Date(epic.start_date);
        const totalDays = this.getDaysBetween(startDate, targetDate);
        const elapsedDays = this.getDaysBetween(startDate, today);
        const expectedProgress = Math.min(100, (elapsedDays / totalDays) * 100);

        if (progress >= expectedProgress - 10) return 'on-track';
        if (progress >= expectedProgress - 25) return 'at-risk';
        return 'delayed';
    }

    getStatusLabel(status) {
        const labels = {
            'on-track': '🟢 On Track',
            'at-risk': '🟡 At Risk',
            'delayed': '🔴 Delayed',
            'blocked': '⚫ Blocked',
            'done': '✅ Done',
            'not-started': '⚪ Not Started'
        };
        return labels[status] || status;
    }

    // Utility methods
    getDaysBetween(start, end) {
        const oneDay = 24 * 60 * 60 * 1000;
        return Math.round((end - start) / oneDay);
    }

    getMonthsInRange(start, end) {
        const months = [];
        const current = new Date(start.getFullYear(), start.getMonth(), 1);
        while (current <= end) {
            months.push({ month: current.getMonth(), year: current.getFullYear() });
            current.setMonth(current.getMonth() + 1);
        }
        return months;
    }

    getMonthName(month) {
        const names = ['January', 'February', 'March', 'April', 'May', 'June',
                       'July', 'August', 'September', 'October', 'November', 'December'];
        return names[month];
    }

    formatDate(date) {
        return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
    }

    // Event handlers
    onEpicClick(epicId) {
        const epic = this.epics.find(e => e.id === epicId);
        if (epic) {
            const event = new CustomEvent('epicClick', { detail: epic });
            this.container.dispatchEvent(event);
        }
    }

    onDragStart(e) {
        e.dataTransfer.setData('text/plain', e.target.dataset.epicId);
        e.target.classList.add('dragging');
    }

    onDragEnd(e) {
        e.target.classList.remove('dragging');
    }

    // Actions
    async addEpic() {
        const name = prompt('Epic name:');
        if (!name) return;

        try {
            const response = await fetch('/api/epics', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    name,
                    project_id: this.options.projectId,
                    start_date: new Date().toISOString(),
                    target_date: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString()
                })
            });

            if (!response.ok) throw new Error('Failed to create epic');
            await this.loadData();
        } catch (error) {
            console.error('Error creating epic:', error);
            alert('Failed to create epic');
        }
    }

    export() {
        // Export as PNG
        const timeline = document.getElementById('roadmap-timeline');
        html2canvas(timeline).then(canvas => {
            const link = document.createElement('a');
            link.download = `roadmap-${this.today.toISOString().split('T')[0]}.png`;
            link.href = canvas.toDataURL();
            link.click();
        }).catch(() => {
            alert('Export requires html2canvas library');
        });
    }

    showError(message) {
        document.getElementById('timeline-epics').innerHTML = `
            <div class="roadmap-error">
                <span>⚠️ ${message}</span>
            </div>
        `;
    }

    destroy() {
        this.container.innerHTML = '';
    }
}

// CSS Styles
const roadmapStyles = `
<style>
.roadmap-container {
    font-family: system-ui, -apple-system, sans-serif;
    background: #F8FAFC;
    border-radius: 12px;
    overflow: hidden;
}

.roadmap-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px 20px;
    background: white;
    border-bottom: 1px solid #E5E7EB;
}

.roadmap-title h2 {
    margin: 0;
    font-size: 20px;
    color: #003B4A;
}

.roadmap-controls {
    display: flex;
    gap: 8px;
}

.roadmap-select {
    padding: 8px 12px;
    border: 1px solid #E5E7EB;
    border-radius: 6px;
    background: white;
    cursor: pointer;
}

.roadmap-btn {
    padding: 8px 16px;
    border: 1px solid #E5E7EB;
    border-radius: 6px;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
}

.roadmap-btn:hover {
    background: #F1F5F9;
}

.roadmap-filters {
    display: flex;
    gap: 24px;
    padding: 12px 20px;
    background: white;
    border-bottom: 1px solid #E5E7EB;
}

.filter-group {
    display: flex;
    align-items: center;
    gap: 8px;
}

.filter-group label {
    font-size: 12px;
    color: #6B7280;
}

.filter-btn {
    padding: 4px 12px;
    border: 1px solid #E5E7EB;
    border-radius: 4px;
    background: white;
    font-size: 12px;
    cursor: pointer;
}

.filter-btn.active {
    background: #003B4A;
    color: white;
    border-color: #003B4A;
}

.filter-check {
    display: flex;
    align-items: center;
    gap: 4px;
    font-size: 12px;
    cursor: pointer;
}

.roadmap-timeline {
    position: relative;
    overflow-x: auto;
    background: white;
}

.timeline-header {
    position: sticky;
    top: 0;
    background: #F8FAFC;
    border-bottom: 1px solid #E5E7EB;
    z-index: 10;
}

.header-row {
    display: flex;
}

.header-cell {
    padding: 8px 4px;
    text-align: center;
    font-size: 12px;
    color: #6B7280;
    border-right: 1px solid #E5E7EB;
}

.month-header {
    font-weight: 600;
    color: #374151;
}

.timeline-body {
    position: relative;
    min-height: 300px;
}

.timeline-grid {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    pointer-events: none;
}

.grid-line {
    position: absolute;
    top: 0;
    bottom: 0;
    width: 1px;
    background: #E5E7EB;
}

.timeline-epics {
    position: relative;
    padding: 10px 0;
}

.epic-bar {
    position: absolute;
    height: 60px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    padding: 8px 12px;
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
    border-left: 4px solid #3B82F6;
}

.epic-bar:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 8px rgba(0,0,0,0.15);
}

.epic-bar.on-track { border-left-color: #10B981; }
.epic-bar.at-risk { border-left-color: #F59E0B; }
.epic-bar.delayed { border-left-color: #EF4444; }
.epic-bar.blocked { border-left-color: #374151; }
.epic-bar.done { border-left-color: #10B981; opacity: 0.7; }
.epic-bar.dragging { opacity: 0.5; }

.epic-header {
    display: flex;
    align-items: center;
    gap: 6px;
}

.epic-icon {
    font-size: 14px;
}

.epic-name {
    font-weight: 600;
    font-size: 13px;
    color: #1F2937;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
}

.epic-meta {
    font-size: 11px;
    color: #6B7280;
    margin-top: 2px;
}

.epic-progress-bar {
    height: 4px;
    background: #E5E7EB;
    border-radius: 2px;
    margin-top: 6px;
    overflow: hidden;
}

.epic-progress-fill {
    height: 100%;
    background: #3B82F6;
    border-radius: 2px;
    transition: width 0.3s;
}

.epic-progress-text {
    position: absolute;
    right: 8px;
    bottom: 8px;
    font-size: 10px;
    color: #6B7280;
}

.epic-status-badge {
    position: absolute;
    top: 4px;
    right: 4px;
    font-size: 10px;
    padding: 2px 6px;
    border-radius: 10px;
    background: #F3F4F6;
}

.timeline-today {
    position: absolute;
    top: 0;
    bottom: 0;
    pointer-events: none;
    z-index: 5;
}

.today-line {
    position: absolute;
    top: 0;
    bottom: 0;
    width: 2px;
    background: #EF4444;
}

.today-marker {
    position: absolute;
    top: -20px;
    left: -30px;
    background: #EF4444;
    color: white;
    padding: 2px 8px;
    border-radius: 10px;
    font-size: 10px;
    white-space: nowrap;
}

.timeline-milestones {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    height: 60px;
    background: #F8FAFC;
    border-top: 1px dashed #E5E7EB;
}

.milestone {
    position: absolute;
    bottom: 10px;
    text-align: center;
    transform: translateX(-50%);
}

.milestone-icon {
    font-size: 20px;
}

.milestone-name {
    font-size: 11px;
    font-weight: 600;
    color: #374151;
}

.milestone-date {
    font-size: 10px;
    color: #6B7280;
}

.roadmap-legend {
    display: flex;
    gap: 16px;
    padding: 12px 20px;
    background: white;
    border-top: 1px solid #E5E7EB;
}

.legend-item {
    display: flex;
    align-items: center;
    gap: 6px;
    font-size: 12px;
    color: #6B7280;
}

.legend-dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
}

.legend-dot.on-track { background: #10B981; }
.legend-dot.at-risk { background: #F59E0B; }
.legend-dot.delayed { background: #EF4444; }
.legend-dot.blocked { background: #374151; }

.roadmap-error {
    padding: 40px;
    text-align: center;
    color: #EF4444;
}
</style>
`;

// Inject styles
if (!document.getElementById('roadmap-timeline-styles')) {
    const styleElement = document.createElement('div');
    styleElement.id = 'roadmap-timeline-styles';
    styleElement.innerHTML = roadmapStyles;
    document.head.appendChild(styleElement.querySelector('style'));
}

// Export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = RoadmapTimeline;
}
