/**
 * Quality Dashboard - Plataforma E v6.5
 *
 * Dashboard showing code quality metrics:
 * - Test coverage
 * - Bug counts and trends
 * - Code smells
 * - Security vulnerabilities
 * - Quality gates
 *
 * Issue #231: [FRONT] Implementar Quality Dashboard com metricas de codigo
 */

class QualityDashboard {
    constructor(containerId, options = {}) {
        this.containerId = containerId;
        this.container = document.getElementById(containerId);

        if (!this.container) {
            console.error(`Container ${containerId} not found`);
            return;
        }

        this.options = {
            projectId: options.projectId || 'default',
            refreshInterval: options.refreshInterval || 60000, // 1 minute
            thresholds: {
                coverageGood: 80,
                coverageWarning: 60,
                bugsMax: 5,
                smellsMax: 30,
                ...options.thresholds
            }
        };

        this.metrics = null;
        this.refreshTimer = null;

        this.init();
    }

    async init() {
        this.render();
        await this.loadMetrics();
        this.startAutoRefresh();
    }

    render() {
        this.container.innerHTML = `
            <div class="quality-dashboard">
                <div class="qd-header">
                    <h2>Quality Dashboard</h2>
                    <div class="qd-actions">
                        <button class="qd-btn" id="qd-refresh" title="Refresh">
                            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <path d="M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"/>
                                <path d="M3 3v5h5"/>
                            </svg>
                        </button>
                        <button class="qd-btn" id="qd-run-tests" title="Run Tests">
                            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <polygon points="5 3 19 12 5 21 5 3"/>
                            </svg>
                        </button>
                        <button class="qd-btn" id="qd-export" title="Export Report">
                            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/>
                                <polyline points="7 10 12 15 17 10"/>
                                <line x1="12" y1="15" x2="12" y2="3"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <div class="qd-overview" id="qd-overview">
                    <div class="qd-loading">Loading metrics...</div>
                </div>

                <div class="qd-grid">
                    <div class="qd-card" id="qd-coverage">
                        <h3>Test Coverage</h3>
                        <div class="qd-card-content"></div>
                    </div>
                    <div class="qd-card" id="qd-bugs">
                        <h3>Bugs</h3>
                        <div class="qd-card-content"></div>
                    </div>
                    <div class="qd-card" id="qd-smells">
                        <h3>Code Smells</h3>
                        <div class="qd-card-content"></div>
                    </div>
                    <div class="qd-card" id="qd-security">
                        <h3>Security</h3>
                        <div class="qd-card-content"></div>
                    </div>
                </div>

                <div class="qd-sections">
                    <div class="qd-section" id="qd-coverage-details">
                        <h3>Coverage by Module</h3>
                        <div class="qd-section-content"></div>
                    </div>
                    <div class="qd-section" id="qd-test-results">
                        <h3>Test Results</h3>
                        <div class="qd-section-content"></div>
                    </div>
                    <div class="qd-section" id="qd-quality-gates">
                        <h3>Quality Gates</h3>
                        <div class="qd-section-content"></div>
                    </div>
                </div>
            </div>
        `;

        // Add event listeners
        document.getElementById('qd-refresh')?.addEventListener('click', () => this.loadMetrics());
        document.getElementById('qd-run-tests')?.addEventListener('click', () => this.runTests());
        document.getElementById('qd-export')?.addEventListener('click', () => this.exportReport());
    }

    async loadMetrics() {
        try {
            const response = await fetch(`/api/projects/${this.options.projectId}/quality-metrics`);
            if (!response.ok) throw new Error('Failed to load metrics');

            this.metrics = await response.json();
            this.updateUI();
        } catch (error) {
            console.error('Error loading quality metrics:', error);
            this.showError('Failed to load quality metrics');
        }
    }

    updateUI() {
        if (!this.metrics) return;

        this.renderOverview();
        this.renderCoverage();
        this.renderBugs();
        this.renderSmells();
        this.renderSecurity();
        this.renderCoverageDetails();
        this.renderTestResults();
        this.renderQualityGates();
    }

    renderOverview() {
        const { coverage, bugs, smells, security, quality_score, grade } = this.metrics;
        const overview = document.getElementById('qd-overview');

        overview.innerHTML = `
            <div class="qd-metric-cards">
                <div class="qd-metric-card ${this.getCoverageClass(coverage.percentage)}">
                    <div class="qd-metric-icon">📊</div>
                    <div class="qd-metric-value">${coverage.percentage}%</div>
                    <div class="qd-metric-label">Coverage</div>
                    <div class="qd-metric-trend ${coverage.trend >= 0 ? 'up' : 'down'}">
                        ${coverage.trend >= 0 ? '▲' : '▼'} ${Math.abs(coverage.trend)}%
                    </div>
                </div>
                <div class="qd-metric-card ${bugs.count > this.options.thresholds.bugsMax ? 'danger' : 'success'}">
                    <div class="qd-metric-icon">🐛</div>
                    <div class="qd-metric-value">${bugs.count}</div>
                    <div class="qd-metric-label">Bugs</div>
                    <div class="qd-metric-trend ${bugs.trend <= 0 ? 'up' : 'down'}">
                        ${bugs.trend <= 0 ? '▼' : '▲'} ${Math.abs(bugs.trend)}
                    </div>
                </div>
                <div class="qd-metric-card ${smells.count > this.options.thresholds.smellsMax ? 'warning' : 'success'}">
                    <div class="qd-metric-icon">⚠️</div>
                    <div class="qd-metric-value">${smells.count}</div>
                    <div class="qd-metric-label">Smells</div>
                    <div class="qd-metric-trend ${smells.trend <= 0 ? 'up' : 'down'}">
                        ${smells.trend <= 0 ? '▼' : '▲'} ${Math.abs(smells.trend)}
                    </div>
                </div>
                <div class="qd-metric-card ${this.getSecurityClass(security.grade)}">
                    <div class="qd-metric-icon">🔒</div>
                    <div class="qd-metric-value">${security.grade}</div>
                    <div class="qd-metric-label">Security</div>
                    <div class="qd-metric-detail">${security.vulnerabilities} issues</div>
                </div>
            </div>
            <div class="qd-score">
                <div class="qd-score-bar">
                    <div class="qd-score-fill" style="width: ${quality_score}%"></div>
                </div>
                <div class="qd-score-label">
                    Quality Score: <strong>${quality_score}/100</strong> | Grade: <strong>${grade}</strong>
                </div>
            </div>
        `;
    }

    renderCoverage() {
        const { coverage } = this.metrics;
        const card = document.querySelector('#qd-coverage .qd-card-content');

        card.innerHTML = `
            <div class="qd-gauge" data-value="${coverage.percentage}">
                <svg viewBox="0 0 100 50" class="qd-gauge-svg">
                    <path d="M 10 45 A 35 35 0 0 1 90 45" fill="none" stroke="#E5E7EB" stroke-width="8"/>
                    <path d="M 10 45 A 35 35 0 0 1 90 45" fill="none"
                          stroke="${this.getCoverageColor(coverage.percentage)}"
                          stroke-width="8"
                          stroke-dasharray="${coverage.percentage * 1.1} 110"/>
                </svg>
                <div class="qd-gauge-value">${coverage.percentage}%</div>
            </div>
            <div class="qd-coverage-stats">
                <div>Lines: ${coverage.lines_covered}/${coverage.lines_total}</div>
                <div>Branches: ${coverage.branches_covered}/${coverage.branches_total}</div>
            </div>
        `;
    }

    renderBugs() {
        const { bugs } = this.metrics;
        const card = document.querySelector('#qd-bugs .qd-card-content');

        card.innerHTML = `
            <div class="qd-bug-count">${bugs.count}</div>
            <div class="qd-bug-breakdown">
                <div class="qd-bug-item critical">🔴 ${bugs.critical || 0} Critical</div>
                <div class="qd-bug-item high">🟠 ${bugs.high || 0} High</div>
                <div class="qd-bug-item medium">🟡 ${bugs.medium || 0} Medium</div>
                <div class="qd-bug-item low">🟢 ${bugs.low || 0} Low</div>
            </div>
        `;
    }

    renderSmells() {
        const { smells } = this.metrics;
        const card = document.querySelector('#qd-smells .qd-card-content');

        const categories = smells.categories || [];
        card.innerHTML = `
            <div class="qd-smell-count">${smells.count}</div>
            <div class="qd-smell-breakdown">
                ${categories.map(cat => `
                    <div class="qd-smell-item">
                        <span class="qd-smell-name">${cat.name}</span>
                        <span class="qd-smell-value">${cat.count}</span>
                    </div>
                `).join('')}
            </div>
        `;
    }

    renderSecurity() {
        const { security } = this.metrics;
        const card = document.querySelector('#qd-security .qd-card-content');

        card.innerHTML = `
            <div class="qd-security-grade ${this.getSecurityClass(security.grade)}">${security.grade}</div>
            <div class="qd-security-breakdown">
                <div class="qd-vuln-item">🔴 ${security.critical || 0} Critical</div>
                <div class="qd-vuln-item">🟠 ${security.high || 0} High</div>
                <div class="qd-vuln-item">🟡 ${security.medium || 0} Medium</div>
                <div class="qd-vuln-item">🟢 ${security.low || 0} Low</div>
            </div>
            <button class="qd-scan-btn" onclick="qualityDashboard.runSecurityScan()">
                🔄 Scan Now
            </button>
        `;
    }

    renderCoverageDetails() {
        const { coverage } = this.metrics;
        const section = document.querySelector('#qd-coverage-details .qd-section-content');

        const modules = coverage.modules || [];
        section.innerHTML = `
            <div class="qd-module-list">
                ${modules.map(mod => `
                    <div class="qd-module-item">
                        <div class="qd-module-name">${mod.name}</div>
                        <div class="qd-module-bar">
                            <div class="qd-module-fill ${this.getCoverageClass(mod.percentage)}"
                                 style="width: ${mod.percentage}%"></div>
                        </div>
                        <div class="qd-module-value">${mod.percentage}%</div>
                        ${mod.percentage < 50 ? '<span class="qd-module-warn">🔴</span>' : ''}
                    </div>
                `).join('')}
            </div>
            ${coverage.uncovered_files?.length > 0 ? `
                <div class="qd-uncovered">
                    <h4>Uncovered Files</h4>
                    <ul>
                        ${coverage.uncovered_files.map(f => `<li>${f}</li>`).join('')}
                    </ul>
                </div>
            ` : ''}
        `;
    }

    renderTestResults() {
        const { tests } = this.metrics;
        const section = document.querySelector('#qd-test-results .qd-section-content');

        section.innerHTML = `
            <div class="qd-test-summary">
                <span class="qd-test-passed">✅ ${tests.passed} Passed</span>
                <span class="qd-test-failed">❌ ${tests.failed} Failed</span>
                <span class="qd-test-skipped">⏭️ ${tests.skipped} Skipped</span>
            </div>
            <div class="qd-test-info">
                Duration: ${tests.duration}s | Last run: ${tests.last_run || 'Never'}
            </div>
            ${tests.failures?.length > 0 ? `
                <div class="qd-test-failures">
                    <h4>Failed Tests</h4>
                    ${tests.failures.map(f => `
                        <div class="qd-failure-item">
                            <div class="qd-failure-name">❌ ${f.name}</div>
                            <div class="qd-failure-msg">${f.message}</div>
                            <div class="qd-failure-loc">${f.file}:${f.line}</div>
                        </div>
                    `).join('')}
                </div>
            ` : ''}
            <div class="qd-test-actions">
                <button class="qd-btn-primary" onclick="qualityDashboard.runTests()">
                    ▶️ Run All Tests
                </button>
                <button class="qd-btn-secondary" onclick="qualityDashboard.viewCoverageReport()">
                    📊 Coverage Report
                </button>
            </div>
        `;
    }

    renderQualityGates() {
        const { quality_gates } = this.metrics;
        const section = document.querySelector('#qd-quality-gates .qd-section-content');

        const allPassed = quality_gates.every(g => g.passed);

        section.innerHTML = `
            <div class="qd-gates-status ${allPassed ? 'passed' : 'failed'}">
                Status: ${allPassed ? 'PASS ✅' : 'FAIL ❌'}
            </div>
            <div class="qd-gates-list">
                ${quality_gates.map(gate => `
                    <div class="qd-gate-item ${gate.passed ? 'passed' : 'failed'}">
                        <span class="qd-gate-icon">${gate.passed ? '✅' : '❌'}</span>
                        <span class="qd-gate-name">${gate.name}</span>
                        <span class="qd-gate-value">${gate.actual} ${gate.operator} ${gate.threshold}</span>
                    </div>
                `).join('')}
            </div>
            <button class="qd-btn-link" onclick="qualityDashboard.configureGates()">
                ⚙️ Configure Gates
            </button>
        `;
    }

    getCoverageClass(percentage) {
        if (percentage >= this.options.thresholds.coverageGood) return 'success';
        if (percentage >= this.options.thresholds.coverageWarning) return 'warning';
        return 'danger';
    }

    getCoverageColor(percentage) {
        if (percentage >= this.options.thresholds.coverageGood) return '#10B981';
        if (percentage >= this.options.thresholds.coverageWarning) return '#F59E0B';
        return '#EF4444';
    }

    getSecurityClass(grade) {
        if (grade === 'A' || grade === 'B') return 'success';
        if (grade === 'C') return 'warning';
        return 'danger';
    }

    async runTests() {
        try {
            const response = await fetch(`/api/projects/${this.options.projectId}/run-tests`, {
                method: 'POST'
            });
            if (!response.ok) throw new Error('Failed to run tests');

            const result = await response.json();
            alert(`Tests completed: ${result.passed} passed, ${result.failed} failed`);
            await this.loadMetrics();
        } catch (error) {
            console.error('Error running tests:', error);
            alert('Failed to run tests: ' + error.message);
        }
    }

    async runSecurityScan() {
        try {
            const response = await fetch(`/api/projects/${this.options.projectId}/security-scan`, {
                method: 'POST'
            });
            if (!response.ok) throw new Error('Failed to run security scan');

            alert('Security scan completed');
            await this.loadMetrics();
        } catch (error) {
            console.error('Error running security scan:', error);
            alert('Failed to run security scan: ' + error.message);
        }
    }

    viewCoverageReport() {
        window.open(`/api/projects/${this.options.projectId}/coverage-report`, '_blank');
    }

    configureGates() {
        window.location.href = `/projects/${this.options.projectId}/settings/quality-gates`;
    }

    exportReport() {
        window.open(`/api/projects/${this.options.projectId}/quality-report?format=pdf`, '_blank');
    }

    startAutoRefresh() {
        if (this.refreshTimer) clearInterval(this.refreshTimer);
        this.refreshTimer = setInterval(() => this.loadMetrics(), this.options.refreshInterval);
    }

    stopAutoRefresh() {
        if (this.refreshTimer) {
            clearInterval(this.refreshTimer);
            this.refreshTimer = null;
        }
    }

    showError(message) {
        const overview = document.getElementById('qd-overview');
        if (overview) {
            overview.innerHTML = `
                <div class="qd-error">
                    <span class="qd-error-icon">⚠️</span>
                    <span>${message}</span>
                </div>
            `;
        }
    }

    destroy() {
        this.stopAutoRefresh();
        this.container.innerHTML = '';
    }
}

// CSS Styles
const qualityStyles = `
<style>
.quality-dashboard {
    font-family: system-ui, -apple-system, sans-serif;
    padding: 20px;
}

.qd-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 20px;
}

.qd-header h2 {
    margin: 0;
    color: #003B4A;
}

.qd-actions {
    display: flex;
    gap: 8px;
}

.qd-btn {
    width: 36px;
    height: 36px;
    border: 1px solid #E5E7EB;
    background: white;
    border-radius: 6px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    color: #64748B;
    transition: all 0.2s;
}

.qd-btn:hover {
    background: #F1F5F9;
    color: #003B4A;
}

.qd-overview {
    background: white;
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}

.qd-metric-cards {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 16px;
    margin-bottom: 20px;
}

.qd-metric-card {
    text-align: center;
    padding: 16px;
    border-radius: 8px;
    background: #F8FAFC;
}

.qd-metric-card.success { background: #ECFDF5; }
.qd-metric-card.warning { background: #FFFBEB; }
.qd-metric-card.danger { background: #FEF2F2; }

.qd-metric-icon {
    font-size: 24px;
    margin-bottom: 8px;
}

.qd-metric-value {
    font-size: 32px;
    font-weight: bold;
    color: #1F2937;
}

.qd-metric-label {
    color: #6B7280;
    font-size: 14px;
}

.qd-metric-trend {
    font-size: 12px;
    margin-top: 4px;
}

.qd-metric-trend.up { color: #10B981; }
.qd-metric-trend.down { color: #EF4444; }

.qd-score {
    margin-top: 16px;
}

.qd-score-bar {
    height: 8px;
    background: #E5E7EB;
    border-radius: 4px;
    overflow: hidden;
}

.qd-score-fill {
    height: 100%;
    background: linear-gradient(90deg, #10B981, #3B82F6);
    border-radius: 4px;
    transition: width 0.5s;
}

.qd-score-label {
    text-align: center;
    margin-top: 8px;
    color: #6B7280;
}

.qd-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 16px;
    margin-bottom: 20px;
}

.qd-card {
    background: white;
    border-radius: 12px;
    padding: 16px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}

.qd-card h3 {
    margin: 0 0 12px;
    font-size: 14px;
    color: #6B7280;
}

.qd-sections {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 16px;
}

.qd-section {
    background: white;
    border-radius: 12px;
    padding: 16px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}

.qd-section h3 {
    margin: 0 0 16px;
    font-size: 14px;
    color: #003B4A;
    border-bottom: 1px solid #E5E7EB;
    padding-bottom: 8px;
}

.qd-gauge {
    position: relative;
    width: 120px;
    margin: 0 auto;
}

.qd-gauge-value {
    position: absolute;
    bottom: 0;
    left: 50%;
    transform: translateX(-50%);
    font-size: 24px;
    font-weight: bold;
}

.qd-module-item {
    display: flex;
    align-items: center;
    gap: 8px;
    margin-bottom: 8px;
}

.qd-module-name {
    flex: 0 0 150px;
    font-size: 12px;
    overflow: hidden;
    text-overflow: ellipsis;
}

.qd-module-bar {
    flex: 1;
    height: 8px;
    background: #E5E7EB;
    border-radius: 4px;
    overflow: hidden;
}

.qd-module-fill {
    height: 100%;
    border-radius: 4px;
}

.qd-module-fill.success { background: #10B981; }
.qd-module-fill.warning { background: #F59E0B; }
.qd-module-fill.danger { background: #EF4444; }

.qd-module-value {
    flex: 0 0 40px;
    text-align: right;
    font-size: 12px;
}

.qd-test-summary {
    display: flex;
    gap: 16px;
    margin-bottom: 12px;
}

.qd-gates-status {
    text-align: center;
    padding: 12px;
    border-radius: 8px;
    font-weight: bold;
    margin-bottom: 16px;
}

.qd-gates-status.passed { background: #ECFDF5; color: #059669; }
.qd-gates-status.failed { background: #FEF2F2; color: #DC2626; }

.qd-gate-item {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px;
    border-radius: 4px;
    margin-bottom: 4px;
}

.qd-gate-item.passed { background: #F0FDF4; }
.qd-gate-item.failed { background: #FEF2F2; }

.qd-btn-primary {
    background: #003B4A;
    color: white;
    border: none;
    padding: 8px 16px;
    border-radius: 6px;
    cursor: pointer;
}

.qd-btn-secondary {
    background: white;
    color: #003B4A;
    border: 1px solid #003B4A;
    padding: 8px 16px;
    border-radius: 6px;
    cursor: pointer;
}

.qd-loading {
    text-align: center;
    color: #6B7280;
    padding: 40px;
}

.qd-error {
    text-align: center;
    color: #EF4444;
    padding: 20px;
}

@media (max-width: 1200px) {
    .qd-metric-cards { grid-template-columns: repeat(2, 1fr); }
    .qd-grid { grid-template-columns: repeat(2, 1fr); }
    .qd-sections { grid-template-columns: 1fr; }
}

@media (max-width: 768px) {
    .qd-metric-cards { grid-template-columns: 1fr; }
    .qd-grid { grid-template-columns: 1fr; }
}
</style>
`;

// Inject styles
if (!document.getElementById('quality-dashboard-styles')) {
    const styleElement = document.createElement('div');
    styleElement.id = 'quality-dashboard-styles';
    styleElement.innerHTML = qualityStyles;
    document.head.appendChild(styleElement.querySelector('style'));
}

// Global reference for button handlers
let qualityDashboard = null;

// Export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = QualityDashboard;
}
