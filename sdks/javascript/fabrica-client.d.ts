/**
 * Fabrica de Agentes - TypeScript Definitions
 * ============================================
 *
 * Type definitions for the JavaScript SDK.
 */

export const VERSION: string;

// =============================================================================
// Enums
// =============================================================================

export enum JobStatus {
    PENDING = 'pending',
    RUNNING = 'running',
    COMPLETED = 'completed',
    FAILED = 'failed',
    CANCELLED = 'cancelled'
}

export enum StoryStatus {
    BACKLOG = 'backlog',
    READY = 'ready',
    IN_PROGRESS = 'in_progress',
    REVIEW = 'review',
    TESTING = 'testing',
    DONE = 'done'
}

export enum Priority {
    LOW = 'low',
    MEDIUM = 'medium',
    HIGH = 'high',
    URGENT = 'urgent'
}

export enum Complexity {
    SIMPLE = 'simple',
    MEDIUM = 'medium',
    COMPLEX = 'complex',
    VERY_COMPLEX = 'very_complex'
}

// =============================================================================
// Errors
// =============================================================================

export class FabricaError extends Error {
    statusCode: number | null;
    errorCode: string | null;
    details: Record<string, any> | null;

    constructor(message: string, options?: {
        statusCode?: number;
        errorCode?: string;
        details?: Record<string, any>;
    });
}

export class APIError extends FabricaError {}

export class AuthenticationError extends APIError {}

export class RateLimitError extends APIError {
    retryAfter: number | null;
}

export class ValidationError extends APIError {}

export class NotFoundError extends APIError {}

export class TimeoutError extends FabricaError {}

// =============================================================================
// Models
// =============================================================================

export interface Project {
    projectId: string;
    name: string;
    description: string | null;
    techStack: string | null;
    status: string;
    createdAt: Date | null;
    updatedAt: Date | null;
}

export interface Story {
    storyId: string;
    title: string;
    projectId: string | null;
    persona: string | null;
    action: string | null;
    benefit: string | null;
    acceptanceCriteria: string[];
    definitionOfDone: string[];
    storyPoints: number | null;
    complexity: string | null;
    status: string;
    priority: string;
    epicId: string | null;
    sprintId: string | null;
    progress: number;
    createdAt: Date | null;
    updatedAt: Date | null;
}

export interface Job {
    jobId: string;
    description: string;
    techStack: string | null;
    features: string[];
    status: string;
    currentStep: string;
    progress: number;
    workerId: string | null;
    outputPath: string | null;
    errorMessage: string | null;
    model: string | null;
    queuedAt: Date | null;
    startedAt: Date | null;
    completedAt: Date | null;
}

export interface Worker {
    workerId: string;
    status: string;
    currentJobId: string | null;
    model: string;
    mcpTools: string[];
    jobsCompleted: number;
    jobsFailed: number;
    avgJobDuration: number;
    lastHeartbeat: Date | null;
}

export interface Agent {
    agentId: string;
    name: string;
    role: string;
    department: string | null;
    skills: string[];
    level: string;
    status: string;
}

export interface PaginatedResponse<T> {
    items: T[];
    cursor: string | null;
    hasMore: boolean;
    totalCount: number | null;
}

export interface QueueStats {
    pending: number;
    running: number;
    completed: number;
    failed: number;
    cancelled: number;
    total: number;
    queueLength: number;
}

export interface HealthResponse {
    status: string;
    service: string;
    version: string;
    timestamp: string;
}

// =============================================================================
// Resource Handlers
// =============================================================================

export interface ListProjectsOptions {
    status?: string;
    cursor?: string;
    limit?: number;
}

export interface CreateProjectOptions {
    name: string;
    description?: string;
    techStack?: string;
    [key: string]: any;
}

export interface ProjectsResource {
    list(options?: ListProjectsOptions): Promise<PaginatedResponse<Project>>;
    get(projectId: string): Promise<Project>;
    create(options: CreateProjectOptions): Promise<Project>;
    update(projectId: string, updates: Partial<Project>): Promise<Project>;
    delete(projectId: string): Promise<boolean>;
}

export interface ListStoriesOptions {
    projectId?: string;
    status?: string;
    epicId?: string;
    sprintId?: string;
    cursor?: string;
    limit?: number;
}

export interface CreateStoryOptions {
    title: string;
    projectId?: string;
    persona?: string;
    action?: string;
    benefit?: string;
    acceptanceCriteria?: string[];
    storyPoints?: number;
    priority?: string;
    [key: string]: any;
}

export interface StoriesResource {
    list(options?: ListStoriesOptions): Promise<PaginatedResponse<Story>>;
    get(storyId: string): Promise<Story>;
    create(options: CreateStoryOptions): Promise<Story>;
    update(storyId: string, updates: Partial<Story>): Promise<Story>;
    delete(storyId: string): Promise<boolean>;
    move(storyId: string, status: string): Promise<Story>;
}

export interface ListJobsOptions {
    status?: string;
    cursor?: string;
    limit?: number;
}

export interface CreateJobOptions {
    description: string;
    techStack?: string;
    features?: string[];
    projectId?: string;
    model?: string;
    complexity?: string;
    storyPoints?: number;
    idempotencyKey?: string;
}

export interface WaitForCompletionOptions {
    timeout?: number;
    pollInterval?: number;
    onProgress?: (job: Job) => void;
}

export interface JobsResource {
    list(options?: ListJobsOptions): Promise<PaginatedResponse<Job>>;
    get(jobId: string): Promise<Job>;
    create(options: CreateJobOptions): Promise<Job>;
    cancel(jobId: string): Promise<boolean>;
    waitForCompletion(jobId: string, options?: WaitForCompletionOptions): Promise<Job>;
}

export interface WorkersResource {
    list(): Promise<Worker[]>;
    get(workerId: string): Promise<Worker>;
    listActive(): Promise<Worker[]>;
}

export interface ListAgentsOptions {
    department?: string;
    role?: string;
    cursor?: string;
    limit?: number;
}

export interface AgentsResource {
    list(options?: ListAgentsOptions): Promise<PaginatedResponse<Agent>>;
    get(agentId: string): Promise<Agent>;
}

export interface QueueResource {
    stats(): Promise<QueueStats>;
    peek(limit?: number): Promise<Job[]>;
}

// =============================================================================
// Main Client
// =============================================================================

export interface FabricaClientOptions {
    apiKey?: string;
    token?: string;
    baseUrl?: string;
    timeout?: number;
    apiVersion?: string;
    headers?: Record<string, string>;
}

export class FabricaClient {
    projects: ProjectsResource;
    stories: StoriesResource;
    jobs: JobsResource;
    workers: WorkersResource;
    agents: AgentsResource;
    queue: QueueResource;

    constructor(options?: FabricaClientOptions);

    health(): Promise<HealthResponse>;
    healthDetailed(): Promise<Record<string, any>>;
    login(username: string, password: string): Promise<string>;
    logout(): Promise<boolean>;
    setApiKey(apiKey: string): void;
    setToken(token: string): void;
}

export default FabricaClient;
