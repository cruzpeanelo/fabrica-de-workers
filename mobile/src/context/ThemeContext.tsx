/**
 * Theme Context - Gerenciamento de tema (Light/Dark) com White Label
 * Issue #368: Suporte a branding dinamico por tenant
 */

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { useColorScheme } from 'react-native';
import { TenantBranding } from './AuthContext';

// Cores padrao (Belgo como fallback)
const DEFAULT_BRANDING = {
  primary: '#003B4A',      // Azul Belgo
  secondary: '#FF6C00',    // Laranja Belgo
};

// Cores fixas do sistema
const SYSTEM_COLORS = {
  success: '#10B981',
  warning: '#F59E0B',
  error: '#EF4444',
  info: '#3B82F6',
};

// Gerar paleta de cores baseada em primary/secondary
function generatePalette(primary: string, secondary: string, isDark: boolean) {
  if (isDark) {
    return {
      primary: lightenColor(primary, 20),
      secondary: lightenColor(secondary, 15),
      background: '#111827',
      surface: '#1F2937',
      text: '#F9FAFB',
      textSecondary: '#9CA3AF',
      border: '#374151',
      ...SYSTEM_COLORS,
      success: '#34D399',
      warning: '#FBBF24',
      error: '#F87171',
      info: '#60A5FA',
    };
  }
  return {
    primary,
    secondary,
    background: '#F3F4F6',
    surface: '#FFFFFF',
    text: '#1F2937',
    textSecondary: '#6B7280',
    border: '#E5E7EB',
    ...SYSTEM_COLORS,
  };
}

// Clarear cor para dark mode
function lightenColor(hex: string, percent: number): string {
  try {
    const num = parseInt(hex.replace('#', ''), 16);
    const r = Math.min(255, ((num >> 16) & 255) + Math.floor(255 * percent / 100));
    const g = Math.min(255, ((num >> 8) & 255) + Math.floor(255 * percent / 100));
    const b = Math.min(255, (num & 255) + Math.floor(255 * percent / 100));
    return `#${((r << 16) | (g << 8) | b).toString(16).padStart(6, '0')}`;
  } catch {
    return hex;
  }
}

export interface ThemeColors {
  primary: string;
  secondary: string;
  background: string;
  surface: string;
  text: string;
  textSecondary: string;
  border: string;
  success: string;
  warning: string;
  error: string;
  info: string;
}

interface ThemeContextData {
  isDark: boolean;
  colors: ThemeColors;
  branding: {
    companyName: string;
    logoUrl?: string;
  };
  toggleTheme: () => void;
  applyBranding: (branding: TenantBranding) => void;
  resetBranding: () => void;
}

const ThemeContext = createContext<ThemeContextData>({} as ThemeContextData);

const THEME_KEY = '@FabricaAgentes:theme';
const BRANDING_KEY = '@FabricaAgentes:branding';

export function ThemeProvider({ children }: { children: ReactNode }) {
  const systemColorScheme = useColorScheme();
  const [isDark, setIsDark] = useState(systemColorScheme === 'dark');
  const [customBranding, setCustomBranding] = useState<{
    primary: string;
    secondary: string;
    companyName: string;
    logoUrl?: string;
  } | null>(null);

  useEffect(() => {
    loadSettings();
  }, []);

  async function loadSettings() {
    try {
      const [savedTheme, savedBranding] = await Promise.all([
        AsyncStorage.getItem(THEME_KEY),
        AsyncStorage.getItem(BRANDING_KEY),
      ]);

      if (savedTheme !== null) {
        setIsDark(savedTheme === 'dark');
      }

      if (savedBranding) {
        setCustomBranding(JSON.parse(savedBranding));
      }
    } catch (error) {
      console.error('Erro ao carregar configuracoes:', error);
    }
  }

  async function toggleTheme() {
    const newTheme = !isDark;
    setIsDark(newTheme);
    await AsyncStorage.setItem(THEME_KEY, newTheme ? 'dark' : 'light');
  }

  async function applyBranding(branding: TenantBranding) {
    const newBranding = {
      primary: branding.primary_color || DEFAULT_BRANDING.primary,
      secondary: branding.secondary_color || DEFAULT_BRANDING.secondary,
      companyName: branding.company_name || 'Fabrica de Agentes',
      logoUrl: branding.logo_url,
    };
    setCustomBranding(newBranding);
    await AsyncStorage.setItem(BRANDING_KEY, JSON.stringify(newBranding));
  }

  async function resetBranding() {
    setCustomBranding(null);
    await AsyncStorage.removeItem(BRANDING_KEY);
  }

  // Gerar cores baseadas no branding
  const primary = customBranding?.primary || DEFAULT_BRANDING.primary;
  const secondary = customBranding?.secondary || DEFAULT_BRANDING.secondary;
  const themeColors = generatePalette(primary, secondary, isDark);

  const branding = {
    companyName: customBranding?.companyName || 'Fabrica de Agentes',
    logoUrl: customBranding?.logoUrl,
  };

  return (
    <ThemeContext.Provider
      value={{
        isDark,
        colors: themeColors,
        branding,
        toggleTheme,
        applyBranding,
        resetBranding,
      }}
    >
      {children}
    </ThemeContext.Provider>
  );
}

export function useTheme() {
  const context = useContext(ThemeContext);
  if (!context) {
    throw new Error('useTheme deve ser usado dentro de ThemeProvider');
  }
  return context;
}

// Re-export para compatibilidade
export const colors = {
  light: generatePalette(DEFAULT_BRANDING.primary, DEFAULT_BRANDING.secondary, false),
  dark: generatePalette(DEFAULT_BRANDING.primary, DEFAULT_BRANDING.secondary, true),
};
