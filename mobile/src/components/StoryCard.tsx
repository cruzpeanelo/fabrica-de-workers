/**
 * StoryCard Component - Card de story fiel ao dashboard web
 * Exibe: Epic, Points, Titulo, Progress, Tasks, Assignee
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Alert,
} from 'react-native';
import {
  ChevronRight,
  Trash2,
  User,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { Story } from '../services/api';

interface StoryCardProps {
  story: Story;
  onPress: () => void;
  onMoveNext?: () => void;
  onDelete?: () => void;
  showQuickActions?: boolean;
  columnColor?: string;
}

export function StoryCard({
  story,
  onPress,
  onMoveNext,
  onDelete,
  showQuickActions = true,
  columnColor,
}: StoryCardProps) {
  const { colors } = useTheme();

  const tasksTotal = story.tasks_total || 0;
  const tasksCompleted = story.tasks_completed || 0;
  const progress = story.progress || 0;

  function handleDelete() {
    Alert.alert(
      'Excluir Requisito',
      `Tem certeza que deseja excluir "${story.title}"?`,
      [
        { text: 'Cancelar', style: 'cancel' },
        {
          text: 'Excluir',
          style: 'destructive',
          onPress: onDelete,
        },
      ]
    );
  }

  const styles = createStyles(colors, columnColor);

  return (
    <TouchableOpacity
      style={[
        styles.card,
        story.priority === 'urgent' && styles.cardUrgent,
        story.priority === 'high' && styles.cardHigh,
      ]}
      onPress={onPress}
      activeOpacity={0.7}
    >
      {/* Quick Actions - aparecem no hover no web, aqui sempre visiveis */}
      {showQuickActions && (onMoveNext || onDelete) && (
        <View style={styles.quickActions}>
          {onMoveNext && story.status !== 'done' && (
            <TouchableOpacity
              style={[styles.quickBtn, styles.quickBtnSuccess]}
              onPress={onMoveNext}
            >
              <ChevronRight color={colors.success} size={14} />
            </TouchableOpacity>
          )}
          {onDelete && (
            <TouchableOpacity
              style={[styles.quickBtn, styles.quickBtnDanger]}
              onPress={handleDelete}
            >
              <Trash2 color={colors.error} size={14} />
            </TouchableOpacity>
          )}
        </View>
      )}

      {/* Epic + Points - igual ao web */}
      <View style={styles.headerRow}>
        {story.epic_name || story.epic_id ? (
          <View style={styles.epicBadge}>
            <Text style={styles.epicText}>
              {story.epic_name || story.epic_id}
            </Text>
          </View>
        ) : (
          <Text style={styles.noEpic}>Sem Epic</Text>
        )}
        <Text style={styles.points}>{story.story_points} pts</Text>
      </View>

      {/* Titulo */}
      <Text style={styles.title} numberOfLines={2}>
        {story.title}
      </Text>

      {/* Progress Bar - igual ao web */}
      <View style={styles.progressSection}>
        <View style={styles.progressHeader}>
          <Text style={styles.progressText}>
            {tasksCompleted}/{tasksTotal} tasks
          </Text>
          <Text style={styles.progressText}>{Math.round(progress)}%</Text>
        </View>
        <View style={styles.progressBar}>
          <View
            style={[
              styles.progressFill,
              {
                width: `${progress}%`,
                backgroundColor: columnColor || colors.success,
              },
            ]}
          />
        </View>
      </View>

      {/* Footer: ID + Assignee */}
      <View style={styles.footer}>
        <Text style={styles.storyId}>{story.story_id}</Text>
        {story.assignee ? (
          <View style={styles.assignee}>
            <View style={styles.avatar}>
              <Text style={styles.avatarText}>
                {story.assignee.charAt(0).toUpperCase()}
              </Text>
            </View>
          </View>
        ) : null}
      </View>
    </TouchableOpacity>
  );
}

const createStyles = (colors: any, columnColor?: string) =>
  StyleSheet.create({
    card: {
      backgroundColor: colors.surface,
      borderRadius: 12,
      padding: 14,
      marginBottom: 10,
      shadowColor: '#000',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.08,
      shadowRadius: 4,
      elevation: 2,
      borderLeftWidth: 3,
      borderLeftColor: columnColor || 'transparent',
    },
    cardUrgent: {
      borderLeftColor: colors.error,
    },
    cardHigh: {
      borderLeftColor: colors.warning,
    },
    quickActions: {
      position: 'absolute',
      top: 8,
      right: 8,
      flexDirection: 'row',
      gap: 6,
      zIndex: 10,
    },
    quickBtn: {
      width: 28,
      height: 28,
      borderRadius: 6,
      justifyContent: 'center',
      alignItems: 'center',
    },
    quickBtnSuccess: {
      backgroundColor: `${colors.success}20`,
    },
    quickBtnDanger: {
      backgroundColor: `${colors.error}20`,
    },
    headerRow: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 8,
      paddingRight: 60, // Espaco para quick actions
    },
    epicBadge: {
      backgroundColor: colors.info + '20',
      paddingHorizontal: 8,
      paddingVertical: 3,
      borderRadius: 4,
    },
    epicText: {
      fontSize: 11,
      fontWeight: '600',
      color: colors.info,
    },
    noEpic: {
      fontSize: 11,
      color: colors.textSecondary,
    },
    points: {
      fontSize: 12,
      fontWeight: '600',
      color: colors.textSecondary,
    },
    title: {
      fontSize: 14,
      fontWeight: '600',
      color: colors.text,
      marginBottom: 10,
      lineHeight: 20,
    },
    progressSection: {
      marginBottom: 10,
    },
    progressHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      marginBottom: 4,
    },
    progressText: {
      fontSize: 11,
      color: colors.textSecondary,
    },
    progressBar: {
      height: 6,
      backgroundColor: colors.border,
      borderRadius: 3,
      overflow: 'hidden',
    },
    progressFill: {
      height: '100%',
      borderRadius: 3,
    },
    footer: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
    },
    storyId: {
      fontSize: 11,
      color: colors.textSecondary,
    },
    assignee: {
      flexDirection: 'row',
      alignItems: 'center',
    },
    avatar: {
      width: 22,
      height: 22,
      borderRadius: 11,
      backgroundColor: colors.border,
      justifyContent: 'center',
      alignItems: 'center',
    },
    avatarText: {
      fontSize: 10,
      fontWeight: '600',
      color: colors.textSecondary,
    },
  });
