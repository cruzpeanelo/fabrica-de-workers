/**
 * Theme Context - Gerenciamento de tema (Light/Dark)
 * Cores baseadas na identidade Belgo
 */

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { useColorScheme } from 'react-native';

// Cores Belgo
export const colors = {
  light: {
    primary: '#003B4A',      // Azul Belgo
    secondary: '#FF6C00',    // Laranja Belgo
    background: '#F3F4F6',
    surface: '#FFFFFF',
    text: '#1F2937',
    textSecondary: '#6B7280',
    border: '#E5E7EB',
    success: '#10B981',
    warning: '#F59E0B',
    error: '#EF4444',
    info: '#3B82F6',
  },
  dark: {
    primary: '#00A3CC',      // Azul claro para dark mode
    secondary: '#FF8533',    // Laranja claro
    background: '#111827',
    surface: '#1F2937',
    text: '#F9FAFB',
    textSecondary: '#9CA3AF',
    border: '#374151',
    success: '#34D399',
    warning: '#FBBF24',
    error: '#F87171',
    info: '#60A5FA',
  },
};

interface ThemeContextData {
  isDark: boolean;
  colors: typeof colors.light;
  toggleTheme: () => void;
}

const ThemeContext = createContext<ThemeContextData>({} as ThemeContextData);

const THEME_KEY = '@FabricaAgentes:theme';

export function ThemeProvider({ children }: { children: ReactNode }) {
  const systemColorScheme = useColorScheme();
  const [isDark, setIsDark] = useState(systemColorScheme === 'dark');

  useEffect(() => {
    loadTheme();
  }, []);

  async function loadTheme() {
    try {
      const savedTheme = await AsyncStorage.getItem(THEME_KEY);
      if (savedTheme !== null) {
        setIsDark(savedTheme === 'dark');
      }
    } catch (error) {
      console.error('Erro ao carregar tema:', error);
    }
  }

  async function toggleTheme() {
    const newTheme = !isDark;
    setIsDark(newTheme);
    await AsyncStorage.setItem(THEME_KEY, newTheme ? 'dark' : 'light');
  }

  const themeColors = isDark ? colors.dark : colors.light;

  return (
    <ThemeContext.Provider value={{ isDark, colors: themeColors, toggleTheme }}>
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
