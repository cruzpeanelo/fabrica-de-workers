/**
 * Dependency Graph - Plataforma E v6.5
 *
 * Visualization of story dependencies using D3.js force-directed graph.
 * Supports: drag nodes, zoom/pan, critical path highlighting, export.
 *
 * Issue #243: [FRONT] Implementar Dependency Graph visual para stories
 */

class DependencyGraph {
    constructor(containerId, options = {}) {
        this.containerId = containerId;
        this.container = document.getElementById(containerId);

        if (!this.container) {
            console.error(`Container ${containerId} not found`);
            return;
        }

        // Default options
        this.options = {
            width: options.width || this.container.clientWidth || 800,
            height: options.height || 600,
            nodeRadius: options.nodeRadius || 30,
            linkDistance: options.linkDistance || 150,
            colors: {
                backlog: '#94A3B8',     // gray
                ready: '#3B82F6',       // blue
                in_progress: '#F59E0B', // amber
                review: '#8B5CF6',      // purple
                testing: '#06B6D4',     // cyan
                done: '#10B981',        // green
                blocked: '#EF4444',     // red
                critical: '#FF6C00'     // Belgo orange
            },
            ...options
        };

        this.nodes = [];
        this.links = [];
        this.simulation = null;
        this.svg = null;
        this.g = null;
        this.zoom = null;
        this.criticalPath = [];

        this.init();
    }

    init() {
        // Clear container
        this.container.innerHTML = '';

        // Create SVG
        this.svg = d3.select(`#${this.containerId}`)
            .append('svg')
            .attr('width', this.options.width)
            .attr('height', this.options.height)
            .attr('class', 'dependency-graph-svg');

        // Create zoom behavior
        this.zoom = d3.zoom()
            .scaleExtent([0.1, 4])
            .on('zoom', (event) => {
                this.g.attr('transform', event.transform);
            });

        this.svg.call(this.zoom);

        // Create main group for all elements
        this.g = this.svg.append('g')
            .attr('class', 'graph-container');

        // Create arrow marker for links
        this.svg.append('defs').append('marker')
            .attr('id', 'arrowhead')
            .attr('viewBox', '-0 -5 10 10')
            .attr('refX', 35)
            .attr('refY', 0)
            .attr('orient', 'auto')
            .attr('markerWidth', 8)
            .attr('markerHeight', 8)
            .append('path')
            .attr('d', 'M 0,-5 L 10,0 L 0,5')
            .attr('fill', '#64748B');

        // Critical path arrow
        this.svg.select('defs').append('marker')
            .attr('id', 'arrowhead-critical')
            .attr('viewBox', '-0 -5 10 10')
            .attr('refX', 35)
            .attr('refY', 0)
            .attr('orient', 'auto')
            .attr('markerWidth', 8)
            .attr('markerHeight', 8)
            .append('path')
            .attr('d', 'M 0,-5 L 10,0 L 0,5')
            .attr('fill', this.options.colors.critical);

        // Create groups for links and nodes
        this.linkGroup = this.g.append('g').attr('class', 'links');
        this.nodeGroup = this.g.append('g').attr('class', 'nodes');

        // Initialize force simulation
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink().id(d => d.id).distance(this.options.linkDistance))
            .force('charge', d3.forceManyBody().strength(-400))
            .force('center', d3.forceCenter(this.options.width / 2, this.options.height / 2))
            .force('collision', d3.forceCollide().radius(this.options.nodeRadius + 10));

        // Add controls
        this.addControls();
    }

    addControls() {
        const controls = document.createElement('div');
        controls.className = 'graph-controls';
        controls.innerHTML = `
            <button class="graph-btn" id="graph-zoom-in" title="Zoom In">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/>
                    <line x1="11" y1="8" x2="11" y2="14"/><line x1="8" y1="11" x2="14" y2="11"/>
                </svg>
            </button>
            <button class="graph-btn" id="graph-zoom-out" title="Zoom Out">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/>
                    <line x1="8" y1="11" x2="14" y2="11"/>
                </svg>
            </button>
            <button class="graph-btn" id="graph-reset" title="Reset View">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"/>
                    <path d="M3 3v5h5"/>
                </svg>
            </button>
            <button class="graph-btn" id="graph-export" title="Export PNG">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/>
                    <polyline points="7 10 12 15 17 10"/>
                    <line x1="12" y1="15" x2="12" y2="3"/>
                </svg>
            </button>
            <button class="graph-btn" id="graph-critical-path" title="Highlight Critical Path">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/>
                    <polyline points="22 4 12 14.01 9 11.01"/>
                </svg>
            </button>
        `;
        this.container.style.position = 'relative';
        this.container.appendChild(controls);

        // Add event listeners
        document.getElementById('graph-zoom-in').addEventListener('click', () => this.zoomIn());
        document.getElementById('graph-zoom-out').addEventListener('click', () => this.zoomOut());
        document.getElementById('graph-reset').addEventListener('click', () => this.resetView());
        document.getElementById('graph-export').addEventListener('click', () => this.exportPNG());
        document.getElementById('graph-critical-path').addEventListener('click', () => this.toggleCriticalPath());
    }

    /**
     * Load data from API and render graph
     */
    async loadData(projectId) {
        try {
            const response = await fetch(`/api/projects/${projectId}/dependency-graph`);
            if (!response.ok) throw new Error('Failed to load dependency graph');

            const data = await response.json();
            this.setData(data.nodes, data.links);

            if (data.critical_path) {
                this.criticalPath = data.critical_path;
            }
        } catch (error) {
            console.error('Error loading dependency graph:', error);
            this.showError('Failed to load dependency graph');
        }
    }

    /**
     * Set graph data and render
     */
    setData(nodes, links) {
        this.nodes = nodes;
        this.links = links;
        this.render();
    }

    /**
     * Render the graph
     */
    render() {
        // Clear existing elements
        this.linkGroup.selectAll('*').remove();
        this.nodeGroup.selectAll('*').remove();

        // Create links
        const link = this.linkGroup.selectAll('line')
            .data(this.links)
            .enter()
            .append('line')
            .attr('class', d => `link link-${d.type || 'blocks'}`)
            .attr('stroke', d => this.getLinkColor(d))
            .attr('stroke-width', d => d.isCritical ? 3 : 2)
            .attr('marker-end', d => d.isCritical ? 'url(#arrowhead-critical)' : 'url(#arrowhead)');

        // Create node groups
        const node = this.nodeGroup.selectAll('g')
            .data(this.nodes)
            .enter()
            .append('g')
            .attr('class', 'node')
            .call(d3.drag()
                .on('start', (event, d) => this.dragStarted(event, d))
                .on('drag', (event, d) => this.dragged(event, d))
                .on('end', (event, d) => this.dragEnded(event, d)));

        // Node circles
        node.append('circle')
            .attr('r', this.options.nodeRadius)
            .attr('fill', d => this.getNodeColor(d))
            .attr('stroke', d => d.isCritical ? this.options.colors.critical : '#fff')
            .attr('stroke-width', d => d.isCritical ? 3 : 2)
            .attr('class', 'node-circle');

        // Story ID text
        node.append('text')
            .attr('dy', -5)
            .attr('text-anchor', 'middle')
            .attr('class', 'node-id')
            .attr('fill', '#fff')
            .attr('font-size', '10px')
            .attr('font-weight', 'bold')
            .text(d => d.story_id || d.id);

        // Story points badge
        node.append('text')
            .attr('dy', 10)
            .attr('text-anchor', 'middle')
            .attr('class', 'node-points')
            .attr('fill', '#fff')
            .attr('font-size', '12px')
            .text(d => d.story_points ? `${d.story_points}pts` : '');

        // Tooltip on hover
        node.append('title')
            .text(d => `${d.story_id || d.id}\n${d.title || ''}\nStatus: ${d.status}\nPoints: ${d.story_points || 'N/A'}`);

        // Click handler
        node.on('click', (event, d) => this.onNodeClick(d));

        // Update simulation
        this.simulation.nodes(this.nodes).on('tick', () => this.ticked(link, node));
        this.simulation.force('link').links(this.links);
        this.simulation.alpha(1).restart();
    }

    ticked(link, node) {
        link
            .attr('x1', d => d.source.x)
            .attr('y1', d => d.source.y)
            .attr('x2', d => d.target.x)
            .attr('y2', d => d.target.y);

        node.attr('transform', d => `translate(${d.x},${d.y})`);
    }

    dragStarted(event, d) {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
    }

    dragged(event, d) {
        d.fx = event.x;
        d.fy = event.y;
    }

    dragEnded(event, d) {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
    }

    getNodeColor(node) {
        if (node.isBlocked || node.status === 'blocked') {
            return this.options.colors.blocked;
        }
        return this.options.colors[node.status] || this.options.colors.backlog;
    }

    getLinkColor(link) {
        if (link.isCritical) return this.options.colors.critical;
        switch (link.type) {
            case 'blocks': return '#EF4444';
            case 'relates_to': return '#3B82F6';
            case 'duplicates': return '#8B5CF6';
            default: return '#64748B';
        }
    }

    onNodeClick(node) {
        // Emit custom event for external handling
        const event = new CustomEvent('nodeClick', { detail: node });
        this.container.dispatchEvent(event);

        // Open story details if story_id exists
        if (node.story_id) {
            window.location.href = `/stories/${node.story_id}`;
        }
    }

    // Zoom controls
    zoomIn() {
        this.svg.transition().call(this.zoom.scaleBy, 1.3);
    }

    zoomOut() {
        this.svg.transition().call(this.zoom.scaleBy, 0.7);
    }

    resetView() {
        this.svg.transition().call(
            this.zoom.transform,
            d3.zoomIdentity.translate(this.options.width / 2, this.options.height / 2).scale(1)
        );
    }

    /**
     * Toggle critical path highlighting
     */
    toggleCriticalPath() {
        const isHighlighted = this.g.classed('critical-path-highlighted');
        this.g.classed('critical-path-highlighted', !isHighlighted);

        if (!isHighlighted && this.criticalPath.length > 0) {
            // Highlight critical path nodes
            this.nodeGroup.selectAll('.node')
                .classed('on-critical-path', d => this.criticalPath.includes(d.id));

            // Highlight critical path links
            this.linkGroup.selectAll('line')
                .classed('on-critical-path', d =>
                    this.criticalPath.includes(d.source.id) &&
                    this.criticalPath.includes(d.target.id)
                );
        } else {
            this.nodeGroup.selectAll('.node').classed('on-critical-path', false);
            this.linkGroup.selectAll('line').classed('on-critical-path', false);
        }
    }

    /**
     * Calculate critical path (longest path in DAG)
     */
    calculateCriticalPath() {
        const nodeMap = new Map(this.nodes.map(n => [n.id, { ...n, weight: n.story_points || 1 }]));
        const inDegree = new Map();
        const dist = new Map();
        const parent = new Map();

        // Initialize
        this.nodes.forEach(n => {
            inDegree.set(n.id, 0);
            dist.set(n.id, 0);
            parent.set(n.id, null);
        });

        // Calculate in-degrees
        this.links.forEach(l => {
            const targetId = typeof l.target === 'object' ? l.target.id : l.target;
            inDegree.set(targetId, (inDegree.get(targetId) || 0) + 1);
        });

        // Topological sort with longest path
        const queue = [];
        this.nodes.forEach(n => {
            if (inDegree.get(n.id) === 0) queue.push(n.id);
        });

        while (queue.length > 0) {
            const current = queue.shift();
            const currentNode = nodeMap.get(current);

            this.links.forEach(l => {
                const sourceId = typeof l.source === 'object' ? l.source.id : l.source;
                const targetId = typeof l.target === 'object' ? l.target.id : l.target;

                if (sourceId === current) {
                    const targetNode = nodeMap.get(targetId);
                    const newDist = dist.get(current) + (targetNode?.weight || 1);

                    if (newDist > dist.get(targetId)) {
                        dist.set(targetId, newDist);
                        parent.set(targetId, current);
                    }

                    inDegree.set(targetId, inDegree.get(targetId) - 1);
                    if (inDegree.get(targetId) === 0) queue.push(targetId);
                }
            });
        }

        // Find end node with max distance
        let maxDist = 0;
        let endNode = null;
        dist.forEach((d, id) => {
            if (d > maxDist) {
                maxDist = d;
                endNode = id;
            }
        });

        // Reconstruct path
        const path = [];
        let current = endNode;
        while (current !== null) {
            path.unshift(current);
            current = parent.get(current);
        }

        this.criticalPath = path;
        return { path, totalWeight: maxDist };
    }

    /**
     * Export graph as PNG
     */
    exportPNG() {
        const svgElement = this.svg.node();
        const serializer = new XMLSerializer();
        let svgString = serializer.serializeToString(svgElement);

        // Add styles inline for export
        svgString = svgString.replace('<svg', `<svg style="background: white"`);

        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        canvas.width = this.options.width * 2;
        canvas.height = this.options.height * 2;

        const img = new Image();
        const svgBlob = new Blob([svgString], { type: 'image/svg+xml;charset=utf-8' });
        const url = URL.createObjectURL(svgBlob);

        img.onload = () => {
            ctx.fillStyle = 'white';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
            URL.revokeObjectURL(url);

            const pngUrl = canvas.toDataURL('image/png');
            const link = document.createElement('a');
            link.download = 'dependency-graph.png';
            link.href = pngUrl;
            link.click();
        };

        img.src = url;
    }

    /**
     * Export graph as SVG
     */
    exportSVG() {
        const svgElement = this.svg.node();
        const serializer = new XMLSerializer();
        const svgString = serializer.serializeToString(svgElement);

        const blob = new Blob([svgString], { type: 'image/svg+xml;charset=utf-8' });
        const url = URL.createObjectURL(blob);

        const link = document.createElement('a');
        link.download = 'dependency-graph.svg';
        link.href = url;
        link.click();

        URL.revokeObjectURL(url);
    }

    showError(message) {
        this.container.innerHTML = `
            <div class="graph-error">
                <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="#EF4444" stroke-width="2">
                    <circle cx="12" cy="12" r="10"/>
                    <line x1="15" y1="9" x2="9" y2="15"/>
                    <line x1="9" y1="9" x2="15" y2="15"/>
                </svg>
                <p>${message}</p>
            </div>
        `;
    }

    /**
     * Add dependency between two stories
     */
    addDependency(sourceId, targetId, type = 'blocks') {
        const link = { source: sourceId, target: targetId, type };
        this.links.push(link);
        this.render();

        // Notify server
        return fetch('/api/dependencies', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ source_id: sourceId, target_id: targetId, type })
        });
    }

    /**
     * Remove dependency between two stories
     */
    removeDependency(sourceId, targetId) {
        this.links = this.links.filter(l =>
            !(l.source.id === sourceId && l.target.id === targetId) &&
            !(l.source === sourceId && l.target === targetId)
        );
        this.render();

        // Notify server
        return fetch(`/api/dependencies/${sourceId}/${targetId}`, {
            method: 'DELETE'
        });
    }

    /**
     * Destroy graph and clean up
     */
    destroy() {
        if (this.simulation) {
            this.simulation.stop();
        }
        this.container.innerHTML = '';
    }
}

// CSS styles for the graph
const graphStyles = `
<style>
.dependency-graph-svg {
    background: #F8FAFC;
    border-radius: 8px;
}

.graph-controls {
    position: absolute;
    top: 10px;
    right: 10px;
    display: flex;
    gap: 4px;
    background: white;
    padding: 4px;
    border-radius: 6px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.graph-btn {
    width: 32px;
    height: 32px;
    border: none;
    background: transparent;
    cursor: pointer;
    border-radius: 4px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: #64748B;
    transition: all 0.2s;
}

.graph-btn:hover {
    background: #F1F5F9;
    color: #003B4A;
}

.node-circle {
    cursor: grab;
    transition: filter 0.2s;
}

.node-circle:hover {
    filter: brightness(1.1);
}

.node:active .node-circle {
    cursor: grabbing;
}

.link {
    stroke-opacity: 0.6;
    transition: stroke-width 0.2s;
}

.link:hover {
    stroke-opacity: 1;
    stroke-width: 3 !important;
}

.critical-path-highlighted .node:not(.on-critical-path) {
    opacity: 0.3;
}

.critical-path-highlighted .link:not(.on-critical-path) {
    opacity: 0.2;
}

.on-critical-path .node-circle {
    stroke: #FF6C00 !important;
    stroke-width: 4px !important;
}

.link.on-critical-path {
    stroke: #FF6C00 !important;
    stroke-width: 4px !important;
}

.graph-error {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100%;
    color: #64748B;
}

.graph-error p {
    margin-top: 16px;
    font-size: 14px;
}

/* Legend */
.graph-legend {
    position: absolute;
    bottom: 10px;
    left: 10px;
    background: white;
    padding: 8px 12px;
    border-radius: 6px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    font-size: 12px;
}

.legend-item {
    display: flex;
    align-items: center;
    gap: 6px;
    margin: 4px 0;
}

.legend-dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
}
</style>
`;

// Inject styles
if (!document.getElementById('dependency-graph-styles')) {
    const styleElement = document.createElement('div');
    styleElement.id = 'dependency-graph-styles';
    styleElement.innerHTML = graphStyles;
    document.head.appendChild(styleElement.querySelector('style'));
}

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = DependencyGraph;
}
