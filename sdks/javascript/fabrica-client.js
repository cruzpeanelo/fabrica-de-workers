/**
 * Fabrica de Agentes - JavaScript SDK
 * ====================================
 *
 * Cliente JavaScript oficial para a API da Fabrica de Agentes.
 *
 * Instalacao:
 *     npm install @fabrica-agentes/sdk
 *
 * Uso (ESM):
 *     import { FabricaClient } from '@fabrica-agentes/sdk';
 *
 *     const client = new FabricaClient({ apiKey: 'sua-api-key' });
 *
 *     // Listar projetos
 *     const projects = await client.projects.list();
 *
 *     // Criar uma story
 *     const story = await client.stories.create({
 *         projectId: 'proj-001',
 *         title: 'Implementar login',
 *         persona: 'usuario',
 *         action: 'fazer login com email',
 *         benefit: 'acessar minha conta'
 *     });
 *
 *     // Executar job
 *     const job = await client.jobs.create({
 *         description: 'Criar API REST para usuarios',
 *         techStack: 'python,fastapi',
 *         features: ['crud', 'auth', 'pagination']
 *     });
 *
 *     // Acompanhar progresso
 *     const completed = await client.jobs.waitForCompletion(job.jobId);
 *
 * @version 1.0.0
 * @author Fabrica de Agentes Team
 */

'use strict';

// =============================================================================
// Constants and Types
// =============================================================================

const VERSION = '1.0.0';

/**
 * Job status enum
 * @readonly
 * @enum {string}
 */
const JobStatus = Object.freeze({
    PENDING: 'pending',
    RUNNING: 'running',
    COMPLETED: 'completed',
    FAILED: 'failed',
    CANCELLED: 'cancelled'
});

/**
 * Story status enum
 * @readonly
 * @enum {string}
 */
const StoryStatus = Object.freeze({
    BACKLOG: 'backlog',
    READY: 'ready',
    IN_PROGRESS: 'in_progress',
    REVIEW: 'review',
    TESTING: 'testing',
    DONE: 'done'
});

/**
 * Priority enum
 * @readonly
 * @enum {string}
 */
const Priority = Object.freeze({
    LOW: 'low',
    MEDIUM: 'medium',
    HIGH: 'high',
    URGENT: 'urgent'
});

/**
 * Complexity enum
 * @readonly
 * @enum {string}
 */
const Complexity = Object.freeze({
    SIMPLE: 'simple',
    MEDIUM: 'medium',
    COMPLEX: 'complex',
    VERY_COMPLEX: 'very_complex'
});

// =============================================================================
// Exceptions
// =============================================================================

/**
 * Base error class for SDK
 */
class FabricaError extends Error {
    /**
     * @param {string} message - Error message
     * @param {Object} [options] - Additional options
     */
    constructor(message, options = {}) {
        super(message);
        this.name = 'FabricaError';
        this.statusCode = options.statusCode || null;
        this.errorCode = options.errorCode || null;
        this.details = options.details || null;
    }
}

/**
 * API error returned by the server
 */
class APIError extends FabricaError {
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'APIError';
    }
}

/**
 * Authentication error
 */
class AuthenticationError extends APIError {
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'AuthenticationError';
    }
}

/**
 * Rate limit exceeded
 */
class RateLimitError extends APIError {
    /**
     * @param {string} message - Error message
     * @param {Object} options - Options including retryAfter
     */
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'RateLimitError';
        this.retryAfter = options.retryAfter || null;
    }
}

/**
 * Validation error
 */
class ValidationError extends APIError {
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'ValidationError';
    }
}

/**
 * Resource not found
 */
class NotFoundError extends APIError {
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'NotFoundError';
    }
}

/**
 * Request timeout
 */
class TimeoutError extends FabricaError {
    constructor(message, options = {}) {
        super(message, options);
        this.name = 'TimeoutError';
    }
}

// =============================================================================
// Models
// =============================================================================

/**
 * Project model
 * @typedef {Object} Project
 * @property {string} projectId - Project ID
 * @property {string} name - Project name
 * @property {string|null} description - Description
 * @property {string|null} techStack - Tech stack
 * @property {string} status - Status
 * @property {Date|null} createdAt - Creation date
 * @property {Date|null} updatedAt - Last update date
 */

/**
 * Story model
 * @typedef {Object} Story
 * @property {string} storyId - Story ID
 * @property {string} title - Story title
 * @property {string|null} projectId - Project ID
 * @property {string|null} persona - Persona
 * @property {string|null} action - Action
 * @property {string|null} benefit - Benefit
 * @property {string[]} acceptanceCriteria - Acceptance criteria
 * @property {string[]} definitionOfDone - Definition of done
 * @property {number|null} storyPoints - Story points
 * @property {string|null} complexity - Complexity
 * @property {string} status - Status
 * @property {string} priority - Priority
 * @property {string|null} epicId - Epic ID
 * @property {string|null} sprintId - Sprint ID
 * @property {number} progress - Progress percentage
 * @property {Date|null} createdAt - Creation date
 * @property {Date|null} updatedAt - Last update date
 */

/**
 * Job model
 * @typedef {Object} Job
 * @property {string} jobId - Job ID
 * @property {string} description - Job description
 * @property {string|null} techStack - Tech stack
 * @property {string[]} features - Features
 * @property {string} status - Status
 * @property {string} currentStep - Current step
 * @property {number} progress - Progress percentage
 * @property {string|null} workerId - Worker ID
 * @property {string|null} outputPath - Output path
 * @property {string|null} errorMessage - Error message
 * @property {string|null} model - Claude model
 * @property {Date|null} queuedAt - Queued at
 * @property {Date|null} startedAt - Started at
 * @property {Date|null} completedAt - Completed at
 */

/**
 * Worker model
 * @typedef {Object} Worker
 * @property {string} workerId - Worker ID
 * @property {string} status - Status
 * @property {string|null} currentJobId - Current job ID
 * @property {string} model - Claude model
 * @property {string[]} mcpTools - MCP tools
 * @property {number} jobsCompleted - Jobs completed
 * @property {number} jobsFailed - Jobs failed
 * @property {number} avgJobDuration - Average job duration
 * @property {Date|null} lastHeartbeat - Last heartbeat
 */

/**
 * Agent model
 * @typedef {Object} Agent
 * @property {string} agentId - Agent ID
 * @property {string} name - Agent name
 * @property {string} role - Role
 * @property {string|null} department - Department
 * @property {string[]} skills - Skills
 * @property {string} level - Level
 * @property {string} status - Status
 */

/**
 * Paginated response
 * @typedef {Object} PaginatedResponse
 * @property {Array} items - Items
 * @property {string|null} cursor - Next cursor
 * @property {boolean} hasMore - Has more items
 * @property {number|null} totalCount - Total count
 */

// =============================================================================
// Utilities
// =============================================================================

/**
 * Parse datetime string
 * @param {string|null} value - ISO datetime string
 * @returns {Date|null}
 */
function parseDateTime(value) {
    if (!value) return null;
    if (value instanceof Date) return value;
    try {
        return new Date(value);
    } catch {
        return null;
    }
}

/**
 * Convert camelCase to snake_case
 * @param {string} str - camelCase string
 * @returns {string} snake_case string
 */
function toSnakeCase(str) {
    return str.replace(/[A-Z]/g, letter => `_${letter.toLowerCase()}`);
}

/**
 * Convert snake_case to camelCase
 * @param {string} str - snake_case string
 * @returns {string} camelCase string
 */
function toCamelCase(str) {
    return str.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
}

/**
 * Convert object keys to snake_case
 * @param {Object} obj - Object with camelCase keys
 * @returns {Object} Object with snake_case keys
 */
function keysToSnakeCase(obj) {
    if (Array.isArray(obj)) {
        return obj.map(keysToSnakeCase);
    }
    if (obj !== null && typeof obj === 'object') {
        return Object.fromEntries(
            Object.entries(obj).map(([key, value]) => [
                toSnakeCase(key),
                keysToSnakeCase(value)
            ])
        );
    }
    return obj;
}

/**
 * Convert object keys to camelCase
 * @param {Object} obj - Object with snake_case keys
 * @returns {Object} Object with camelCase keys
 */
function keysToCamelCase(obj) {
    if (Array.isArray(obj)) {
        return obj.map(keysToCamelCase);
    }
    if (obj !== null && typeof obj === 'object') {
        return Object.fromEntries(
            Object.entries(obj).map(([key, value]) => [
                toCamelCase(key),
                keysToCamelCase(value)
            ])
        );
    }
    return obj;
}

/**
 * Sleep for specified milliseconds
 * @param {number} ms - Milliseconds
 * @returns {Promise<void>}
 */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// HTTP Client
// =============================================================================

/**
 * HTTP Client for API requests
 */
class HTTPClient {
    /**
     * @param {Object} options - Client options
     * @param {string} options.baseUrl - Base URL
     * @param {string} [options.apiKey] - API Key
     * @param {string} [options.token] - JWT Token
     * @param {number} [options.timeout] - Request timeout in ms
     * @param {string} [options.apiVersion] - API version
     */
    constructor(options) {
        this.baseUrl = options.baseUrl.replace(/\/$/, '');
        this.apiKey = options.apiKey || null;
        this.token = options.token || null;
        this.timeout = options.timeout || 30000;
        this.apiVersion = options.apiVersion || 'v1';
        this.extraHeaders = options.headers || {};
    }

    /**
     * Get request headers
     * @param {Object} [extra] - Extra headers
     * @returns {Object} Headers object
     */
    _getHeaders(extra = {}) {
        const headers = {
            'Content-Type': 'application/json',
            'Accept': 'application/json',
            'User-Agent': `FabricaSDK-JavaScript/${VERSION}`,
            'X-API-Version': this.apiVersion,
            ...this.extraHeaders
        };

        if (this.apiKey) {
            headers['X-API-Key'] = this.apiKey;
        }

        if (this.token) {
            headers['Authorization'] = `Bearer ${this.token}`;
        }

        return { ...headers, ...extra };
    }

    /**
     * Handle API response
     * @param {Response} response - Fetch response
     * @returns {Promise<Object>}
     * @throws {APIError}
     */
    async _handleResponse(response) {
        let body;
        try {
            body = await response.json();
        } catch {
            body = { message: await response.text() };
        }

        if (!response.ok) {
            const errorCode = body.error_code || 'UNKNOWN_ERROR';
            const message = body.message || body.detail || 'Unknown error';

            if (response.status === 401) {
                throw new AuthenticationError(message, {
                    statusCode: response.status,
                    errorCode
                });
            } else if (response.status === 404) {
                throw new NotFoundError(message, {
                    statusCode: response.status,
                    errorCode
                });
            } else if (response.status === 422) {
                throw new ValidationError(message, {
                    statusCode: response.status,
                    errorCode,
                    details: body.detail
                });
            } else if (response.status === 429) {
                const retryAfter = response.headers.get('Retry-After');
                throw new RateLimitError(message, {
                    statusCode: response.status,
                    errorCode,
                    retryAfter: retryAfter ? parseInt(retryAfter) : null
                });
            } else {
                throw new APIError(message, {
                    statusCode: response.status,
                    errorCode,
                    details: body
                });
            }
        }

        return keysToCamelCase(body);
    }

    /**
     * Make HTTP request
     * @param {string} method - HTTP method
     * @param {string} path - Request path
     * @param {Object} [options] - Request options
     * @returns {Promise<Object>}
     */
    async request(method, path, options = {}) {
        const url = new URL(path, this.baseUrl);

        if (options.params) {
            Object.entries(options.params).forEach(([key, value]) => {
                if (value !== null && value !== undefined) {
                    url.searchParams.append(toSnakeCase(key), value);
                }
            });
        }

        const headers = this._getHeaders(options.headers);

        if (options.idempotencyKey) {
            headers['Idempotency-Key'] = options.idempotencyKey;
        }

        const fetchOptions = {
            method,
            headers
        };

        if (options.body) {
            fetchOptions.body = JSON.stringify(keysToSnakeCase(options.body));
        }

        // Timeout handling
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.timeout);
        fetchOptions.signal = controller.signal;

        try {
            const response = await fetch(url.toString(), fetchOptions);
            clearTimeout(timeoutId);
            return await this._handleResponse(response);
        } catch (error) {
            clearTimeout(timeoutId);
            if (error.name === 'AbortError') {
                throw new TimeoutError(`Request timeout after ${this.timeout}ms`);
            }
            throw error;
        }
    }

    async get(path, options) {
        return this.request('GET', path, options);
    }

    async post(path, options) {
        return this.request('POST', path, options);
    }

    async put(path, options) {
        return this.request('PUT', path, options);
    }

    async patch(path, options) {
        return this.request('PATCH', path, options);
    }

    async delete(path, options) {
        return this.request('DELETE', path, options);
    }
}

// =============================================================================
// Resource Handlers
// =============================================================================

/**
 * Projects resource handler
 */
class ProjectsResource {
    /**
     * @param {HTTPClient} client - HTTP client
     */
    constructor(client) {
        this._client = client;
    }

    /**
     * List projects
     * @param {Object} [options] - List options
     * @param {string} [options.status] - Filter by status
     * @param {string} [options.cursor] - Pagination cursor
     * @param {number} [options.limit] - Items per page
     * @returns {Promise<PaginatedResponse>}
     */
    async list(options = {}) {
        const data = await this._client.get('/api/v1/projects', {
            params: {
                status: options.status,
                cursor: options.cursor,
                limit: options.limit || 20
            }
        });

        const items = (data.items || data).map(p => ({
            projectId: p.projectId || p.id,
            name: p.name,
            description: p.description,
            techStack: p.techStack,
            status: p.status || 'active',
            createdAt: parseDateTime(p.createdAt),
            updatedAt: parseDateTime(p.updatedAt)
        }));

        return {
            items,
            cursor: data.cursor,
            hasMore: data.hasMore || false,
            totalCount: data.totalCount
        };
    }

    /**
     * Get project by ID
     * @param {string} projectId - Project ID
     * @returns {Promise<Project>}
     */
    async get(projectId) {
        const data = await this._client.get(`/api/v1/projects/${projectId}`);
        return {
            projectId: data.projectId || data.id,
            name: data.name,
            description: data.description,
            techStack: data.techStack,
            status: data.status || 'active',
            createdAt: parseDateTime(data.createdAt),
            updatedAt: parseDateTime(data.updatedAt)
        };
    }

    /**
     * Create project
     * @param {Object} options - Project data
     * @param {string} options.name - Project name
     * @param {string} [options.description] - Description
     * @param {string} [options.techStack] - Tech stack
     * @returns {Promise<Project>}
     */
    async create(options) {
        const data = await this._client.post('/api/v1/projects', {
            body: options
        });
        return {
            projectId: data.projectId || data.id,
            name: data.name,
            description: data.description,
            techStack: data.techStack,
            status: data.status || 'active',
            createdAt: parseDateTime(data.createdAt),
            updatedAt: parseDateTime(data.updatedAt)
        };
    }

    /**
     * Update project
     * @param {string} projectId - Project ID
     * @param {Object} updates - Updates
     * @returns {Promise<Project>}
     */
    async update(projectId, updates) {
        const data = await this._client.put(`/api/v1/projects/${projectId}`, {
            body: updates
        });
        return {
            projectId: data.projectId || data.id,
            name: data.name,
            description: data.description,
            techStack: data.techStack,
            status: data.status || 'active',
            createdAt: parseDateTime(data.createdAt),
            updatedAt: parseDateTime(data.updatedAt)
        };
    }

    /**
     * Delete project
     * @param {string} projectId - Project ID
     * @returns {Promise<boolean>}
     */
    async delete(projectId) {
        await this._client.delete(`/api/v1/projects/${projectId}`);
        return true;
    }
}

/**
 * Stories resource handler
 */
class StoriesResource {
    constructor(client) {
        this._client = client;
    }

    /**
     * List stories
     * @param {Object} [options] - List options
     * @returns {Promise<PaginatedResponse>}
     */
    async list(options = {}) {
        const data = await this._client.get('/api/stories', {
            params: {
                projectId: options.projectId,
                status: options.status,
                epicId: options.epicId,
                sprintId: options.sprintId,
                cursor: options.cursor,
                limit: options.limit || 20
            }
        });

        const items = (data.items || data).map(this._mapStory);

        return {
            items,
            cursor: data.cursor,
            hasMore: data.hasMore || false,
            totalCount: data.totalCount
        };
    }

    /**
     * Get story by ID
     * @param {string} storyId - Story ID
     * @returns {Promise<Story>}
     */
    async get(storyId) {
        const data = await this._client.get(`/api/stories/${storyId}`);
        return this._mapStory(data);
    }

    /**
     * Create story
     * @param {Object} options - Story data
     * @returns {Promise<Story>}
     */
    async create(options) {
        const data = await this._client.post('/api/stories', {
            body: options
        });
        return this._mapStory(data);
    }

    /**
     * Update story
     * @param {string} storyId - Story ID
     * @param {Object} updates - Updates
     * @returns {Promise<Story>}
     */
    async update(storyId, updates) {
        const data = await this._client.put(`/api/stories/${storyId}`, {
            body: updates
        });
        return this._mapStory(data);
    }

    /**
     * Delete story
     * @param {string} storyId - Story ID
     * @returns {Promise<boolean>}
     */
    async delete(storyId) {
        await this._client.delete(`/api/stories/${storyId}`);
        return true;
    }

    /**
     * Move story in Kanban
     * @param {string} storyId - Story ID
     * @param {string} status - New status
     * @returns {Promise<Story>}
     */
    async move(storyId, status) {
        const data = await this._client.patch(`/api/stories/${storyId}/move`, {
            body: { status }
        });
        return this._mapStory(data);
    }

    _mapStory(data) {
        return {
            storyId: data.storyId || data.id,
            title: data.title,
            projectId: data.projectId,
            persona: data.persona,
            action: data.action,
            benefit: data.benefit,
            acceptanceCriteria: data.acceptanceCriteria || [],
            definitionOfDone: data.definitionOfDone || [],
            storyPoints: data.storyPoints,
            complexity: data.complexity,
            status: data.status || 'backlog',
            priority: data.priority || 'medium',
            epicId: data.epicId,
            sprintId: data.sprintId,
            progress: data.progress || 0,
            createdAt: parseDateTime(data.createdAt),
            updatedAt: parseDateTime(data.updatedAt)
        };
    }
}

/**
 * Jobs resource handler
 */
class JobsResource {
    constructor(client) {
        this._client = client;
    }

    /**
     * List jobs
     * @param {Object} [options] - List options
     * @returns {Promise<PaginatedResponse>}
     */
    async list(options = {}) {
        const data = await this._client.get('/api/v1/jobs', {
            params: {
                status: options.status,
                cursor: options.cursor,
                limit: options.limit || 50
            }
        });

        const items = (data.items || data).map(this._mapJob);

        return {
            items,
            cursor: data.cursor,
            hasMore: data.hasMore || false,
            totalCount: data.totalCount
        };
    }

    /**
     * Get job by ID
     * @param {string} jobId - Job ID
     * @returns {Promise<Job>}
     */
    async get(jobId) {
        const data = await this._client.get(`/api/v1/jobs/${jobId}`);
        return this._mapJob(data);
    }

    /**
     * Create job
     * @param {Object} options - Job options
     * @param {string} options.description - Job description
     * @param {string} [options.techStack] - Tech stack
     * @param {string[]} [options.features] - Features
     * @param {string} [options.projectId] - Project ID
     * @param {string} [options.model] - Claude model
     * @param {string} [options.complexity] - Complexity
     * @param {number} [options.storyPoints] - Story points
     * @param {string} [options.idempotencyKey] - Idempotency key
     * @returns {Promise<Job>}
     */
    async create(options) {
        const { idempotencyKey, ...body } = options;

        const data = await this._client.post('/api/v1/jobs', {
            body,
            idempotencyKey
        });
        return this._mapJob(data);
    }

    /**
     * Cancel job
     * @param {string} jobId - Job ID
     * @returns {Promise<boolean>}
     */
    async cancel(jobId) {
        await this._client.delete(`/api/v1/jobs/${jobId}`);
        return true;
    }

    /**
     * Wait for job completion
     * @param {string} jobId - Job ID
     * @param {Object} [options] - Wait options
     * @param {number} [options.timeout] - Timeout in ms (default 600000)
     * @param {number} [options.pollInterval] - Poll interval in ms (default 5000)
     * @param {Function} [options.onProgress] - Progress callback
     * @returns {Promise<Job>}
     */
    async waitForCompletion(jobId, options = {}) {
        const timeout = options.timeout || 600000;
        const pollInterval = options.pollInterval || 5000;
        const startTime = Date.now();

        while (true) {
            const job = await this.get(jobId);

            if (options.onProgress) {
                options.onProgress(job);
            }

            if (job.status === JobStatus.COMPLETED) {
                return job;
            }

            if (job.status === JobStatus.FAILED) {
                throw new APIError(`Job failed: ${job.errorMessage}`, {
                    errorCode: 'JOB_FAILED'
                });
            }

            if (job.status === JobStatus.CANCELLED) {
                throw new APIError('Job was cancelled', {
                    errorCode: 'JOB_CANCELLED'
                });
            }

            const elapsed = Date.now() - startTime;
            if (elapsed >= timeout) {
                throw new TimeoutError(
                    `Timeout waiting for job ${jobId} (elapsed: ${elapsed}ms)`
                );
            }

            await sleep(pollInterval);
        }
    }

    _mapJob(data) {
        return {
            jobId: data.jobId || data.id,
            description: data.description,
            techStack: data.techStack,
            features: data.features || [],
            status: data.status || 'pending',
            currentStep: data.currentStep || '',
            progress: data.progress || 0,
            workerId: data.workerId,
            outputPath: data.outputPath,
            errorMessage: data.errorMessage,
            model: data.model,
            queuedAt: parseDateTime(data.queuedAt),
            startedAt: parseDateTime(data.startedAt),
            completedAt: parseDateTime(data.completedAt)
        };
    }
}

/**
 * Workers resource handler
 */
class WorkersResource {
    constructor(client) {
        this._client = client;
    }

    /**
     * List workers
     * @returns {Promise<Worker[]>}
     */
    async list() {
        const data = await this._client.get('/api/v1/workers');
        return data.map(this._mapWorker);
    }

    /**
     * Get worker by ID
     * @param {string} workerId - Worker ID
     * @returns {Promise<Worker>}
     */
    async get(workerId) {
        const data = await this._client.get(`/api/v1/workers/${workerId}`);
        return this._mapWorker(data);
    }

    /**
     * List active workers
     * @returns {Promise<Worker[]>}
     */
    async listActive() {
        const data = await this._client.get('/api/v1/workers/active');
        return data.map(this._mapWorker);
    }

    _mapWorker(data) {
        return {
            workerId: data.workerId || data.id,
            status: data.status,
            currentJobId: data.currentJobId,
            model: data.model || 'sonnet',
            mcpTools: data.mcpTools || [],
            jobsCompleted: data.jobsCompleted || 0,
            jobsFailed: data.jobsFailed || 0,
            avgJobDuration: data.avgJobDuration || 0,
            lastHeartbeat: parseDateTime(data.lastHeartbeat)
        };
    }
}

/**
 * Agents resource handler
 */
class AgentsResource {
    constructor(client) {
        this._client = client;
    }

    /**
     * List agents
     * @param {Object} [options] - List options
     * @returns {Promise<PaginatedResponse>}
     */
    async list(options = {}) {
        const data = await this._client.get('/api/v1/agents', {
            params: {
                department: options.department,
                role: options.role,
                cursor: options.cursor,
                limit: options.limit || 50
            }
        });

        const items = (data.items || data).map(this._mapAgent);

        return {
            items,
            cursor: data.cursor,
            hasMore: data.hasMore || false,
            totalCount: data.totalCount
        };
    }

    /**
     * Get agent by ID
     * @param {string} agentId - Agent ID
     * @returns {Promise<Agent>}
     */
    async get(agentId) {
        const data = await this._client.get(`/api/v1/agents/${agentId}`);
        return this._mapAgent(data);
    }

    _mapAgent(data) {
        return {
            agentId: data.agentId || data.id,
            name: data.name,
            role: data.role,
            department: data.department,
            skills: data.skills || [],
            level: data.level || 'analyst',
            status: data.status || 'available'
        };
    }
}

/**
 * Queue resource handler
 */
class QueueResource {
    constructor(client) {
        this._client = client;
    }

    /**
     * Get queue statistics
     * @returns {Promise<Object>}
     */
    async stats() {
        return this._client.get('/api/v1/queue/stats');
    }

    /**
     * Peek at queue
     * @param {number} [limit=10] - Number of items
     * @returns {Promise<Job[]>}
     */
    async peek(limit = 10) {
        const data = await this._client.get('/api/v1/queue/peek', {
            params: { limit }
        });
        return data.map(j => ({
            jobId: j.jobId || j.id,
            description: j.description,
            status: j.status,
            progress: j.progress
        }));
    }
}

// =============================================================================
// Main Client
// =============================================================================

/**
 * Main client for Fabrica de Agentes API
 */
class FabricaClient {
    /**
     * Create a new client
     * @param {Object} options - Client options
     * @param {string} [options.apiKey] - API Key for authentication
     * @param {string} [options.token] - JWT Token for authentication
     * @param {string} [options.baseUrl='http://localhost:9001'] - Base URL
     * @param {number} [options.timeout=30000] - Request timeout in ms
     * @param {string} [options.apiVersion='v1'] - API version
     */
    constructor(options = {}) {
        const baseUrl = options.baseUrl || 'http://localhost:9001';

        this._http = new HTTPClient({
            baseUrl,
            apiKey: options.apiKey,
            token: options.token,
            timeout: options.timeout || 30000,
            apiVersion: options.apiVersion || 'v1',
            headers: options.headers
        });

        // Resources
        this.projects = new ProjectsResource(this._http);
        this.stories = new StoriesResource(this._http);
        this.jobs = new JobsResource(this._http);
        this.workers = new WorkersResource(this._http);
        this.agents = new AgentsResource(this._http);
        this.queue = new QueueResource(this._http);
    }

    /**
     * Health check
     * @returns {Promise<Object>}
     */
    async health() {
        return this._http.get('/api/v1/health');
    }

    /**
     * Detailed health check
     * @returns {Promise<Object>}
     */
    async healthDetailed() {
        return this._http.get('/api/v1/health/detailed');
    }

    /**
     * Login with username and password
     * @param {string} username - Username
     * @param {string} password - Password
     * @returns {Promise<string>} JWT token
     */
    async login(username, password) {
        const data = await this._http.post('/api/v1/auth/login', {
            body: { username, password }
        });
        const token = data.accessToken;
        this._http.token = token;
        return token;
    }

    /**
     * Logout
     * @returns {Promise<boolean>}
     */
    async logout() {
        try {
            await this._http.post('/api/v1/auth/logout');
        } catch {
            // Ignore errors
        }
        this._http.token = null;
        return true;
    }

    /**
     * Set API key
     * @param {string} apiKey - API key
     */
    setApiKey(apiKey) {
        this._http.apiKey = apiKey;
    }

    /**
     * Set JWT token
     * @param {string} token - JWT token
     */
    setToken(token) {
        this._http.token = token;
    }
}

// =============================================================================
// Exports
// =============================================================================

// CommonJS export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        FabricaClient,
        // Enums
        JobStatus,
        StoryStatus,
        Priority,
        Complexity,
        // Errors
        FabricaError,
        APIError,
        AuthenticationError,
        RateLimitError,
        ValidationError,
        NotFoundError,
        TimeoutError,
        // Version
        VERSION
    };
}

// ES Module export (for bundlers)
if (typeof globalThis !== 'undefined') {
    globalThis.FabricaClient = FabricaClient;
    globalThis.FabricaSDK = {
        FabricaClient,
        JobStatus,
        StoryStatus,
        Priority,
        Complexity,
        FabricaError,
        APIError,
        AuthenticationError,
        RateLimitError,
        ValidationError,
        NotFoundError,
        TimeoutError,
        VERSION
    };
}
