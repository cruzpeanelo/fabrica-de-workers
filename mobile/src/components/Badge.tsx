/**
 * Badge Component - Issue #423
 * Badge de status para stories, tasks e prioridades
 */

import React from 'react';
import { View, Text, StyleSheet, ViewStyle } from 'react-native';
import { useTheme } from '../context/ThemeContext';

type BadgeVariant = 'default' | 'primary' | 'success' | 'warning' | 'error' | 'info';
type BadgeSize = 'sm' | 'md' | 'lg';

interface BadgeProps {
  label: string;
  variant?: BadgeVariant;
  size?: BadgeSize;
  icon?: React.ReactNode;
  outlined?: boolean;
  style?: ViewStyle;
}

export function Badge({
  label,
  variant = 'default',
  size = 'md',
  icon,
  outlined = false,
  style,
}: BadgeProps) {
  const { colors } = useTheme();

  const getColors = () => {
    const variants = {
      default: { bg: colors.border, text: colors.text },
      primary: { bg: colors.primary, text: '#FFFFFF' },
      success: { bg: colors.success, text: '#FFFFFF' },
      warning: { bg: colors.warning, text: '#000000' },
      error: { bg: colors.error, text: '#FFFFFF' },
      info: { bg: '#3B82F6', text: '#FFFFFF' },
    };
    return variants[variant] || variants.default;
  };

  const getSizeStyles = () => {
    switch (size) {
      case 'sm':
        return { paddingVertical: 2, paddingHorizontal: 6, fontSize: 10 };
      case 'lg':
        return { paddingVertical: 6, paddingHorizontal: 14, fontSize: 14 };
      default:
        return { paddingVertical: 4, paddingHorizontal: 10, fontSize: 12 };
    }
  };

  const colorScheme = getColors();
  const sizeStyles = getSizeStyles();

  return (
    <View
      style={[
        styles.badge,
        {
          paddingVertical: sizeStyles.paddingVertical,
          paddingHorizontal: sizeStyles.paddingHorizontal,
          backgroundColor: outlined ? 'transparent' : colorScheme.bg,
          borderColor: colorScheme.bg,
          borderWidth: outlined ? 1.5 : 0,
        },
        style,
      ]}
    >
      {icon && <View style={styles.icon}>{icon}</View>}
      <Text
        style={[
          styles.label,
          {
            fontSize: sizeStyles.fontSize,
            color: outlined ? colorScheme.bg : colorScheme.text,
          },
        ]}
      >
        {label}
      </Text>
    </View>
  );
}

// Pre-configured status badges
export function StatusBadge({ status }: { status: string }) {
  const statusMap: Record<string, { label: string; variant: BadgeVariant }> = {
    backlog: { label: 'Backlog', variant: 'default' },
    ready: { label: 'Pronto', variant: 'info' },
    in_progress: { label: 'Em Progresso', variant: 'primary' },
    review: { label: 'Revisao', variant: 'warning' },
    testing: { label: 'Testando', variant: 'info' },
    done: { label: 'Concluido', variant: 'success' },
    blocked: { label: 'Bloqueado', variant: 'error' },
    pending: { label: 'Pendente', variant: 'default' },
    completed: { label: 'Completo', variant: 'success' },
  };

  const config = statusMap[status] || { label: status, variant: 'default' as BadgeVariant };
  return <Badge label={config.label} variant={config.variant} />;
}

export function PriorityBadge({ priority }: { priority: string }) {
  const priorityMap: Record<string, { label: string; variant: BadgeVariant }> = {
    low: { label: 'Baixa', variant: 'default' },
    medium: { label: 'Media', variant: 'info' },
    high: { label: 'Alta', variant: 'warning' },
    urgent: { label: 'Urgente', variant: 'error' },
  };

  const config = priorityMap[priority] || { label: priority, variant: 'default' as BadgeVariant };
  return <Badge label={config.label} variant={config.variant} size="sm" />;
}

export function StoryPointsBadge({ points }: { points: number }) {
  return (
    <Badge
      label={`${points} pts`}
      variant="primary"
      size="sm"
      outlined
    />
  );
}

const styles = StyleSheet.create({
  badge: {
    flexDirection: 'row',
    alignItems: 'center',
    borderRadius: 16,
    alignSelf: 'flex-start',
  },
  icon: {
    marginRight: 4,
  },
  label: {
    fontWeight: '600',
  },
});
