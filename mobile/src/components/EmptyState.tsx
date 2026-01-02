/**
 * EmptyState Component - Issue #423
 * Estado vazio para listas com ilustracao e acoes
 */

import React from 'react';
import { View, Text, StyleSheet, ViewStyle } from 'react-native';
import {
  FileText,
  FolderOpen,
  Search,
  Inbox,
  Calendar,
  Users,
  CheckSquare,
  AlertCircle,
  Wifi,
  LucideIcon,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { Button } from './Button';

type EmptyStateType =
  | 'no-data'
  | 'no-results'
  | 'no-projects'
  | 'no-stories'
  | 'no-tasks'
  | 'no-team'
  | 'no-events'
  | 'error'
  | 'offline'
  | 'custom';

interface EmptyStateProps {
  type?: EmptyStateType;
  title?: string;
  description?: string;
  icon?: LucideIcon;
  actionLabel?: string;
  onAction?: () => void;
  secondaryActionLabel?: string;
  onSecondaryAction?: () => void;
  style?: ViewStyle;
}

export function EmptyState({
  type = 'no-data',
  title,
  description,
  icon: CustomIcon,
  actionLabel,
  onAction,
  secondaryActionLabel,
  onSecondaryAction,
  style,
}: EmptyStateProps) {
  const { colors } = useTheme();

  const getContent = () => {
    const configs: Record<
      EmptyStateType,
      { icon: LucideIcon; title: string; description: string }
    > = {
      'no-data': {
        icon: Inbox,
        title: 'Nenhum dado encontrado',
        description: 'Parece que nao ha itens para exibir no momento.',
      },
      'no-results': {
        icon: Search,
        title: 'Nenhum resultado',
        description: 'Tente ajustar os filtros ou termos de busca.',
      },
      'no-projects': {
        icon: FolderOpen,
        title: 'Nenhum projeto',
        description: 'Crie seu primeiro projeto para comecar.',
      },
      'no-stories': {
        icon: FileText,
        title: 'Nenhuma story',
        description: 'Adicione user stories ao backlog do projeto.',
      },
      'no-tasks': {
        icon: CheckSquare,
        title: 'Nenhuma tarefa',
        description: 'Todas as tarefas foram concluidas!',
      },
      'no-team': {
        icon: Users,
        title: 'Equipe vazia',
        description: 'Convide membros para colaborar no projeto.',
      },
      'no-events': {
        icon: Calendar,
        title: 'Sem eventos',
        description: 'Nenhum evento agendado para este periodo.',
      },
      error: {
        icon: AlertCircle,
        title: 'Algo deu errado',
        description: 'Ocorreu um erro ao carregar os dados. Tente novamente.',
      },
      offline: {
        icon: Wifi,
        title: 'Sem conexao',
        description: 'Verifique sua conexao com a internet e tente novamente.',
      },
      custom: {
        icon: Inbox,
        title: title || 'Vazio',
        description: description || '',
      },
    };

    return configs[type];
  };

  const content = getContent();
  const Icon = CustomIcon || content.icon;
  const displayTitle = title || content.title;
  const displayDescription = description || content.description;

  const styles = createStyles(colors);

  return (
    <View style={[styles.container, style]}>
      <View style={styles.iconContainer}>
        <Icon color={colors.textSecondary} size={64} strokeWidth={1.5} />
      </View>

      <Text style={styles.title}>{displayTitle}</Text>
      <Text style={styles.description}>{displayDescription}</Text>

      {(actionLabel || secondaryActionLabel) && (
        <View style={styles.actions}>
          {actionLabel && onAction && (
            <Button
              title={actionLabel}
              onPress={onAction}
              variant="primary"
              size="md"
            />
          )}
          {secondaryActionLabel && onSecondaryAction && (
            <Button
              title={secondaryActionLabel}
              onPress={onSecondaryAction}
              variant="ghost"
              size="md"
            />
          )}
        </View>
      )}
    </View>
  );
}

// Convenience components for common states
export function NoResultsState({
  searchTerm,
  onClear,
}: {
  searchTerm?: string;
  onClear?: () => void;
}) {
  return (
    <EmptyState
      type="no-results"
      description={
        searchTerm
          ? `Nenhum resultado para "${searchTerm}"`
          : 'Tente ajustar os filtros ou termos de busca.'
      }
      actionLabel={onClear ? 'Limpar busca' : undefined}
      onAction={onClear}
    />
  );
}

export function ErrorState({ onRetry }: { onRetry?: () => void }) {
  return (
    <EmptyState
      type="error"
      actionLabel={onRetry ? 'Tentar novamente' : undefined}
      onAction={onRetry}
    />
  );
}

export function OfflineState({ onRetry }: { onRetry?: () => void }) {
  return (
    <EmptyState
      type="offline"
      actionLabel={onRetry ? 'Tentar novamente' : undefined}
      onAction={onRetry}
    />
  );
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      justifyContent: 'center',
      alignItems: 'center',
      padding: 32,
    },
    iconContainer: {
      marginBottom: 24,
      opacity: 0.5,
    },
    title: {
      fontSize: 20,
      fontWeight: '600',
      color: colors.text,
      textAlign: 'center',
      marginBottom: 8,
    },
    description: {
      fontSize: 15,
      color: colors.textSecondary,
      textAlign: 'center',
      lineHeight: 22,
      maxWidth: 280,
    },
    actions: {
      marginTop: 24,
      gap: 12,
      alignItems: 'center',
    },
  });
