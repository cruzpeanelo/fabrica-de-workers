/**
 * API Service - Comunicacao com backend
 * Conecta ao servidor FastAPI da Plataforma E
 */

import axios from 'axios';
import AsyncStorage from '@react-native-async-storage/async-storage';

// URL base do servidor (configuravel)
const API_URL = process.env.EXPO_PUBLIC_API_URL || 'http://localhost:9001';

export const api = axios.create({
  baseURL: API_URL,
  timeout: 30000,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Interceptor para adicionar token
api.interceptors.request.use(async (config) => {
  const token = await AsyncStorage.getItem('@FabricaAgentes:token');
  if (token) {
    config.headers.Authorization = `Bearer ${token}`;
  }
  return config;
});

// Interceptor para tratar erros
api.interceptors.response.use(
  (response) => response,
  async (error) => {
    if (error.response?.status === 401) {
      // Token expirado - fazer logout
      await AsyncStorage.multiRemove(['@FabricaAgentes:token', '@FabricaAgentes:user']);
    }
    return Promise.reject(error);
  }
);

// =============================================================================
// STORIES API
// =============================================================================

export interface Story {
  story_id: string;
  title: string;
  persona: string;
  action: string;
  benefit: string;
  status: string;
  priority: string;
  story_points: number;
  progress: number;
  // Campos adicionais para fidelidade com dashboard web
  tasks_total?: number;
  tasks_completed?: number;
  epic_id?: string;
  epic_name?: string;
  sprint_id?: string;
  assignee?: string;
  complexity?: string;
  acceptance_criteria?: string[];
  definition_of_done?: string[];
  created_at?: string;
  updated_at?: string;
}

export const storiesApi = {
  list: async (projectId: string = 'default'): Promise<Story[]> => {
    const response = await api.get(`/api/stories?project_id=${projectId}`);
    return response.data.stories || response.data;
  },

  get: async (storyId: string): Promise<Story> => {
    const response = await api.get(`/api/stories/${storyId}`);
    return response.data;
  },

  create: async (story: Partial<Story>): Promise<Story> => {
    const response = await api.post('/api/stories', story);
    return response.data;
  },

  update: async (storyId: string, data: Partial<Story>): Promise<Story> => {
    const response = await api.put(`/api/stories/${storyId}`, data);
    return response.data;
  },

  move: async (storyId: string, newStatus: string): Promise<Story> => {
    const response = await api.patch(`/api/stories/${storyId}/move`, { status: newStatus });
    return response.data;
  },

  delete: async (storyId: string): Promise<void> => {
    await api.delete(`/api/stories/${storyId}`);
  },
};

// =============================================================================
// TASKS API
// =============================================================================

export interface Task {
  task_id: string;
  story_id: string;
  title: string;
  status: string;
  progress: number;
  task_type: string;
}

export const tasksApi = {
  listByStory: async (storyId: string): Promise<Task[]> => {
    const response = await api.get(`/api/stories/${storyId}/tasks`);
    return response.data.tasks || response.data;
  },

  create: async (storyId: string, task: Partial<Task>): Promise<Task> => {
    const response = await api.post(`/api/stories/${storyId}/tasks`, task);
    return response.data;
  },

  update: async (taskId: string, data: Partial<Task>): Promise<Task> => {
    const response = await api.put(`/api/story-tasks/${taskId}`, data);
    return response.data;
  },

  complete: async (taskId: string): Promise<Task> => {
    const response = await api.patch(`/api/story-tasks/${taskId}/complete`);
    return response.data;
  },
};

// =============================================================================
// PROJECTS API
// =============================================================================

export interface Project {
  project_id: string;
  name: string;
  description: string;
  status: string;
  progress: number;
}

export const projectsApi = {
  list: async (): Promise<Project[]> => {
    const response = await api.get('/api/projects');
    return response.data.projects || response.data;
  },

  get: async (projectId: string): Promise<Project> => {
    const response = await api.get(`/api/projects/${projectId}`);
    return response.data;
  },
};

// =============================================================================
// CHAT API
// =============================================================================

export interface ChatMessage {
  id: string;
  role: 'user' | 'assistant';
  content: string;
  created_at: string;
}

export const chatApi = {
  sendMessage: async (message: string, storyId?: string): Promise<ChatMessage> => {
    const response = await api.post('/api/chat/message', {
      message,
      story_id: storyId,
    });
    return response.data;
  },

  getHistory: async (storyId?: string): Promise<ChatMessage[]> => {
    const params = storyId ? `?story_id=${storyId}` : '';
    const response = await api.get(`/api/chat/history${params}`);
    return response.data.messages || response.data;
  },
};

// =============================================================================
// EPICS API
// =============================================================================

export interface Epic {
  epic_id: string;
  name: string;
  description?: string;
  color?: string;
  stories_count?: number;
}

export const epicsApi = {
  list: async (projectId: string = 'default'): Promise<Epic[]> => {
    const response = await api.get(`/api/projects/${projectId}/epics`);
    return response.data.epics || response.data;
  },

  get: async (epicId: string): Promise<Epic> => {
    const response = await api.get(`/api/epics/${epicId}`);
    return response.data;
  },
};

// =============================================================================
// SPRINTS API
// =============================================================================

export interface Sprint {
  sprint_id: string;
  name: string;
  start_date?: string;
  end_date?: string;
  status?: string;
  goal?: string;
}

export const sprintsApi = {
  list: async (projectId: string = 'default'): Promise<Sprint[]> => {
    const response = await api.get(`/api/projects/${projectId}/sprints`);
    return response.data.sprints || response.data;
  },

  get: async (sprintId: string): Promise<Sprint> => {
    const response = await api.get(`/api/sprints/${sprintId}`);
    return response.data;
  },
};

// =============================================================================
// UPLOAD API
// =============================================================================

export const uploadApi = {
  uploadImage: async (uri: string, filename: string): Promise<string> => {
    const formData = new FormData();
    formData.append('file', {
      uri,
      name: filename,
      type: 'image/jpeg',
    } as any);

    const response = await api.post('/api/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
    });
    return response.data.url || response.data.path;
  },
};
