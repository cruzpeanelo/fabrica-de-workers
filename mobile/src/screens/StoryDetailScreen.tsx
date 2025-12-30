/**
 * Story Detail Screen - Detalhes do requisito
 * Mostra narrativa, tasks, progresso e acoes
 */

import React, { useEffect, useState } from 'react';
import {
  View,
  Text,
  ScrollView,
  StyleSheet,
  TouchableOpacity,
  ActivityIndicator,
} from 'react-native';
import {
  User,
  Target,
  Gift,
  CheckSquare,
  Square,
  ChevronDown,
  ChevronUp,
  MessageCircle,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { storiesApi, tasksApi, Story, Task } from '../services/api';

export function StoryDetailScreen({ route, navigation }: any) {
  const { storyId } = route.params;
  const { colors } = useTheme();
  const [story, setStory] = useState<Story | null>(null);
  const [tasks, setTasks] = useState<Task[]>([]);
  const [loading, setLoading] = useState(true);
  const [expandedSections, setExpandedSections] = useState({
    narrative: true,
    tasks: true,
    criteria: false,
  });

  useEffect(() => {
    loadData();
  }, [storyId]);

  async function loadData() {
    try {
      const [storyData, tasksData] = await Promise.all([
        storiesApi.get(storyId),
        tasksApi.listByStory(storyId),
      ]);
      setStory(storyData);
      setTasks(tasksData);
    } catch (error) {
      console.error('Erro ao carregar dados:', error);
    } finally {
      setLoading(false);
    }
  }

  async function toggleTaskComplete(taskId: string, currentStatus: string) {
    try {
      if (currentStatus === 'completed') {
        await tasksApi.update(taskId, { status: 'pending' });
      } else {
        await tasksApi.complete(taskId);
      }
      // Recarregar
      const updatedTasks = await tasksApi.listByStory(storyId);
      setTasks(updatedTasks);
    } catch (error) {
      console.error('Erro ao atualizar task:', error);
    }
  }

  function toggleSection(section: keyof typeof expandedSections) {
    setExpandedSections((prev) => ({
      ...prev,
      [section]: !prev[section],
    }));
  }

  const styles = createStyles(colors);

  if (loading) {
    return (
      <View style={styles.loadingContainer}>
        <ActivityIndicator size="large" color={colors.primary} />
      </View>
    );
  }

  if (!story) {
    return (
      <View style={styles.errorContainer}>
        <Text style={styles.errorText}>Requisito nao encontrado</Text>
      </View>
    );
  }

  const completedTasks = tasks.filter((t) => t.status === 'completed').length;
  const progress = tasks.length > 0 ? Math.round((completedTasks / tasks.length) * 100) : 0;

  return (
    <ScrollView style={styles.container}>
      {/* Header Card */}
      <View style={styles.headerCard}>
        <View style={styles.headerTop}>
          <Text style={styles.storyId}>{story.story_id}</Text>
          <View
            style={[
              styles.statusBadge,
              { backgroundColor: getStatusColor(story.status, colors) },
            ]}
          >
            <Text style={styles.statusText}>{getStatusLabel(story.status)}</Text>
          </View>
        </View>
        <Text style={styles.storyTitle}>{story.title}</Text>

        {/* Progress */}
        <View style={styles.progressSection}>
          <View style={styles.progressHeader}>
            <Text style={styles.progressLabel}>Progresso</Text>
            <Text style={styles.progressPercent}>{progress}%</Text>
          </View>
          <View style={styles.progressBar}>
            <View
              style={[styles.progressFill, { width: `${progress}%` }]}
            />
          </View>
          <Text style={styles.progressSubtext}>
            {completedTasks} de {tasks.length} tarefas concluidas
          </Text>
        </View>

        {/* Points */}
        <View style={styles.metaRow}>
          <View style={styles.metaItem}>
            <Text style={styles.metaLabel}>Pontos</Text>
            <Text style={styles.metaValue}>{story.story_points}</Text>
          </View>
          <View style={styles.metaItem}>
            <Text style={styles.metaLabel}>Prioridade</Text>
            <Text style={styles.metaValue}>{story.priority || 'Media'}</Text>
          </View>
        </View>
      </View>

      {/* Narrative Section */}
      <TouchableOpacity
        style={styles.sectionHeader}
        onPress={() => toggleSection('narrative')}
      >
        <Text style={styles.sectionTitle}>Narrativa</Text>
        {expandedSections.narrative ? (
          <ChevronUp color={colors.textSecondary} size={20} />
        ) : (
          <ChevronDown color={colors.textSecondary} size={20} />
        )}
      </TouchableOpacity>

      {expandedSections.narrative && (
        <View style={styles.sectionContent}>
          <View style={styles.narrativeItem}>
            <User color={colors.primary} size={20} />
            <View style={styles.narrativeText}>
              <Text style={styles.narrativeLabel}>Como um(a)</Text>
              <Text style={styles.narrativeValue}>
                {story.persona || 'usuario'}
              </Text>
            </View>
          </View>

          <View style={styles.narrativeItem}>
            <Target color={colors.secondary} size={20} />
            <View style={styles.narrativeText}>
              <Text style={styles.narrativeLabel}>Eu quero</Text>
              <Text style={styles.narrativeValue}>
                {story.action || story.title}
              </Text>
            </View>
          </View>

          <View style={styles.narrativeItem}>
            <Gift color={colors.success} size={20} />
            <View style={styles.narrativeText}>
              <Text style={styles.narrativeLabel}>Para que</Text>
              <Text style={styles.narrativeValue}>
                {story.benefit || 'eu possa ter uma melhor experiencia'}
              </Text>
            </View>
          </View>
        </View>
      )}

      {/* Tasks Section */}
      <TouchableOpacity
        style={styles.sectionHeader}
        onPress={() => toggleSection('tasks')}
      >
        <Text style={styles.sectionTitle}>Tarefas ({tasks.length})</Text>
        {expandedSections.tasks ? (
          <ChevronUp color={colors.textSecondary} size={20} />
        ) : (
          <ChevronDown color={colors.textSecondary} size={20} />
        )}
      </TouchableOpacity>

      {expandedSections.tasks && (
        <View style={styles.sectionContent}>
          {tasks.map((task) => (
            <TouchableOpacity
              key={task.task_id}
              style={styles.taskItem}
              onPress={() => toggleTaskComplete(task.task_id, task.status)}
            >
              {task.status === 'completed' ? (
                <CheckSquare color={colors.success} size={22} />
              ) : (
                <Square color={colors.textSecondary} size={22} />
              )}
              <View style={styles.taskContent}>
                <Text
                  style={[
                    styles.taskTitle,
                    task.status === 'completed' && styles.taskCompleted,
                  ]}
                >
                  {task.title}
                </Text>
                <Text style={styles.taskType}>{task.task_type}</Text>
              </View>
            </TouchableOpacity>
          ))}

          {tasks.length === 0 && (
            <Text style={styles.emptyText}>Nenhuma tarefa cadastrada</Text>
          )}
        </View>
      )}

      {/* Chat Button */}
      <TouchableOpacity
        style={styles.chatButton}
        onPress={() => navigation.navigate('Chat', { storyId })}
      >
        <MessageCircle color="#FFF" size={20} />
        <Text style={styles.chatButtonText}>Conversar sobre este requisito</Text>
      </TouchableOpacity>

      <View style={{ height: 40 }} />
    </ScrollView>
  );
}

function getStatusColor(status: string, colors: any): string {
  const map: Record<string, string> = {
    backlog: colors.textSecondary,
    ready: colors.info,
    in_progress: colors.warning,
    review: colors.primary,
    testing: colors.secondary,
    done: colors.success,
  };
  return map[status] || colors.textSecondary;
}

function getStatusLabel(status: string): string {
  const map: Record<string, string> = {
    backlog: 'Backlog',
    ready: 'Pronto',
    in_progress: 'Em Andamento',
    review: 'Revisao',
    testing: 'Teste',
    done: 'Concluido',
  };
  return map[status] || status;
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    loadingContainer: {
      flex: 1,
      justifyContent: 'center',
      alignItems: 'center',
      backgroundColor: colors.background,
    },
    errorContainer: {
      flex: 1,
      justifyContent: 'center',
      alignItems: 'center',
      backgroundColor: colors.background,
    },
    errorText: {
      fontSize: 16,
      color: colors.error,
    },
    headerCard: {
      backgroundColor: colors.surface,
      padding: 20,
      margin: 16,
      borderRadius: 16,
    },
    headerTop: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 8,
    },
    storyId: {
      fontSize: 12,
      fontWeight: '600',
      color: colors.textSecondary,
    },
    statusBadge: {
      paddingHorizontal: 10,
      paddingVertical: 4,
      borderRadius: 12,
    },
    statusText: {
      fontSize: 11,
      fontWeight: '600',
      color: '#FFF',
    },
    storyTitle: {
      fontSize: 20,
      fontWeight: '700',
      color: colors.text,
      marginBottom: 16,
    },
    progressSection: {
      marginBottom: 16,
    },
    progressHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      marginBottom: 8,
    },
    progressLabel: {
      fontSize: 13,
      color: colors.textSecondary,
    },
    progressPercent: {
      fontSize: 13,
      fontWeight: '600',
      color: colors.primary,
    },
    progressBar: {
      height: 8,
      backgroundColor: colors.border,
      borderRadius: 4,
      overflow: 'hidden',
    },
    progressFill: {
      height: '100%',
      backgroundColor: colors.success,
      borderRadius: 4,
    },
    progressSubtext: {
      fontSize: 12,
      color: colors.textSecondary,
      marginTop: 6,
    },
    metaRow: {
      flexDirection: 'row',
      gap: 16,
    },
    metaItem: {
      flex: 1,
      backgroundColor: colors.background,
      padding: 12,
      borderRadius: 8,
    },
    metaLabel: {
      fontSize: 11,
      color: colors.textSecondary,
      marginBottom: 4,
    },
    metaValue: {
      fontSize: 16,
      fontWeight: '600',
      color: colors.text,
    },
    sectionHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      backgroundColor: colors.surface,
      padding: 16,
      marginHorizontal: 16,
      marginTop: 8,
      borderRadius: 12,
    },
    sectionTitle: {
      fontSize: 16,
      fontWeight: '600',
      color: colors.text,
    },
    sectionContent: {
      backgroundColor: colors.surface,
      marginHorizontal: 16,
      marginTop: 2,
      padding: 16,
      borderBottomLeftRadius: 12,
      borderBottomRightRadius: 12,
    },
    narrativeItem: {
      flexDirection: 'row',
      alignItems: 'flex-start',
      gap: 12,
      marginBottom: 16,
    },
    narrativeText: {
      flex: 1,
    },
    narrativeLabel: {
      fontSize: 12,
      color: colors.textSecondary,
      marginBottom: 2,
    },
    narrativeValue: {
      fontSize: 15,
      color: colors.text,
      lineHeight: 22,
    },
    taskItem: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 12,
      paddingVertical: 12,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    taskContent: {
      flex: 1,
    },
    taskTitle: {
      fontSize: 14,
      color: colors.text,
    },
    taskCompleted: {
      textDecorationLine: 'line-through',
      color: colors.textSecondary,
    },
    taskType: {
      fontSize: 11,
      color: colors.textSecondary,
      marginTop: 2,
    },
    emptyText: {
      textAlign: 'center',
      color: colors.textSecondary,
      padding: 20,
    },
    chatButton: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'center',
      backgroundColor: colors.primary,
      margin: 16,
      padding: 16,
      borderRadius: 12,
      gap: 8,
    },
    chatButtonText: {
      color: '#FFF',
      fontSize: 15,
      fontWeight: '600',
    },
  });
