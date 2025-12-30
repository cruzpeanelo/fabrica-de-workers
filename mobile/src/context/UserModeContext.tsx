/**
 * User Mode Context - Toggle Basico/Avancado
 * Issue #368: Suporte a modo simplificado do sistema
 */

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { api } from '../services/api';

// Labels para modo basico (usuario nao-tecnico)
const BASIC_LABELS: Record<string, string> = {
  stories: 'Requisitos',
  tasks: 'Atividades',
  backlog: 'Lista de Espera',
  sprint: 'Ciclo',
  epic: 'Tema',
  story_points: 'Esforco',
  velocity: 'Ritmo',
  burndown: 'Progresso',
  acceptance_criteria: 'O que precisa ter',
  definition_of_done: 'Quando esta pronto',
  assignee: 'Responsavel',
  priority: 'Importancia',
  complexity: 'Dificuldade',
};

// Labels para modo avancado (tecnico)
const ADVANCED_LABELS: Record<string, string> = {
  stories: 'User Stories',
  tasks: 'Tasks',
  backlog: 'Backlog',
  sprint: 'Sprint',
  epic: 'Epic',
  story_points: 'Story Points',
  velocity: 'Velocity',
  burndown: 'Burndown',
  acceptance_criteria: 'Acceptance Criteria',
  definition_of_done: 'Definition of Done',
  assignee: 'Assignee',
  priority: 'Priority',
  complexity: 'Complexity',
};

export type UserMode = 'basic' | 'advanced';

interface UserModeContextData {
  mode: UserMode;
  labels: Record<string, string>;
  showTour: boolean;
  setMode: (mode: UserMode) => Promise<void>;
  getLabel: (key: string) => string;
  completeTour: () => Promise<void>;
  resetTour: () => Promise<void>;
}

const UserModeContext = createContext<UserModeContextData>({} as UserModeContextData);

const MODE_KEY = '@FabricaAgentes:userMode';
const TOUR_KEY = '@FabricaAgentes:tourCompleted';

export function UserModeProvider({ children }: { children: ReactNode }) {
  const [mode, setModeState] = useState<UserMode>('basic');
  const [showTour, setShowTour] = useState(false);

  useEffect(() => {
    loadSettings();
  }, []);

  async function loadSettings() {
    try {
      const [savedMode, tourCompleted] = await Promise.all([
        AsyncStorage.getItem(MODE_KEY),
        AsyncStorage.getItem(TOUR_KEY),
      ]);

      if (savedMode === 'basic' || savedMode === 'advanced') {
        setModeState(savedMode);
      }

      // Mostrar tour se nunca foi completado
      if (!tourCompleted) {
        setShowTour(true);
      }
    } catch (error) {
      console.error('Erro ao carregar configuracoes:', error);
    }
  }

  async function setMode(newMode: UserMode) {
    setModeState(newMode);
    await AsyncStorage.setItem(MODE_KEY, newMode);

    // Sincronizar com backend
    try {
      await api.post('/api/user-mode', { mode: newMode });
    } catch (error) {
      // Continuar mesmo se API falhar
      console.warn('Erro ao sincronizar modo:', error);
    }
  }

  function getLabel(key: string): string {
    const labels = mode === 'basic' ? BASIC_LABELS : ADVANCED_LABELS;
    return labels[key] || key;
  }

  async function completeTour() {
    setShowTour(false);
    await AsyncStorage.setItem(TOUR_KEY, 'true');

    // Sincronizar com backend
    try {
      await api.post('/api/tour/complete');
    } catch (error) {
      console.warn('Erro ao sincronizar tour:', error);
    }
  }

  async function resetTour() {
    setShowTour(true);
    await AsyncStorage.removeItem(TOUR_KEY);
  }

  const labels = mode === 'basic' ? BASIC_LABELS : ADVANCED_LABELS;

  return (
    <UserModeContext.Provider
      value={{
        mode,
        labels,
        showTour,
        setMode,
        getLabel,
        completeTour,
        resetTour,
      }}
    >
      {children}
    </UserModeContext.Provider>
  );
}

export function useUserMode() {
  const context = useContext(UserModeContext);
  if (!context) {
    throw new Error('useUserMode deve ser usado dentro de UserModeProvider');
  }
  return context;
}
