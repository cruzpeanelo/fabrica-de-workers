/**
 * Cores da aplicacao - Identidade Belgo
 */

export const Colors = {
  // Cores principais Belgo
  belgoBlue: '#003B4A',
  belgoOrange: '#FF6C00',

  // Light Theme
  light: {
    primary: '#003B4A',
    secondary: '#FF6C00',
    background: '#F3F4F6',
    surface: '#FFFFFF',
    text: '#1F2937',
    textSecondary: '#6B7280',
    textTertiary: '#9CA3AF',
    border: '#E5E7EB',
    divider: '#F3F4F6',

    // Status
    success: '#10B981',
    successLight: '#D1FAE5',
    warning: '#F59E0B',
    warningLight: '#FEF3C7',
    error: '#EF4444',
    errorLight: '#FEE2E2',
    info: '#3B82F6',
    infoLight: '#DBEAFE',

    // Kanban columns
    kanbanBacklog: '#6B7280',
    kanbanReady: '#3B82F6',
    kanbanInProgress: '#F59E0B',
    kanbanReview: '#8B5CF6',
    kanbanTesting: '#EC4899',
    kanbanDone: '#10B981',
  },

  // Dark Theme
  dark: {
    primary: '#00A3CC',
    secondary: '#FF8533',
    background: '#111827',
    surface: '#1F2937',
    text: '#F9FAFB',
    textSecondary: '#9CA3AF',
    textTertiary: '#6B7280',
    border: '#374151',
    divider: '#1F2937',

    // Status
    success: '#34D399',
    successLight: '#064E3B',
    warning: '#FBBF24',
    warningLight: '#78350F',
    error: '#F87171',
    errorLight: '#7F1D1D',
    info: '#60A5FA',
    infoLight: '#1E3A8A',

    // Kanban columns
    kanbanBacklog: '#9CA3AF',
    kanbanReady: '#60A5FA',
    kanbanInProgress: '#FBBF24',
    kanbanReview: '#A78BFA',
    kanbanTesting: '#F472B6',
    kanbanDone: '#34D399',
  },
} as const;

// Priority colors
export const PriorityColors = {
  urgent: '#EF4444',
  high: '#F59E0B',
  medium: '#3B82F6',
  low: '#6B7280',
} as const;

// Story points badge colors
export const PointsColors = {
  1: '#10B981',
  2: '#34D399',
  3: '#3B82F6',
  5: '#8B5CF6',
  8: '#F59E0B',
  13: '#EF4444',
  21: '#DC2626',
} as const;
