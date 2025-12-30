/**
 * Home Screen - Dashboard principal
 * Mostra resumo do projeto e metricas
 */

import React, { useEffect, useState } from 'react';
import {
  View,
  Text,
  ScrollView,
  StyleSheet,
  RefreshControl,
  TouchableOpacity,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import {
  TrendingUp,
  CheckCircle,
  Clock,
  AlertCircle,
  ChevronRight,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { useAuth } from '../context/AuthContext';
import { storiesApi, Story } from '../services/api';

export function HomeScreen({ navigation }: any) {
  const { colors } = useTheme();
  const { user } = useAuth();
  const [stories, setStories] = useState<Story[]>([]);
  const [refreshing, setRefreshing] = useState(false);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadData();
  }, []);

  async function loadData() {
    try {
      const data = await storiesApi.list();
      setStories(data);
    } catch (error) {
      console.error('Erro ao carregar dados:', error);
    } finally {
      setLoading(false);
    }
  }

  async function onRefresh() {
    setRefreshing(true);
    await loadData();
    setRefreshing(false);
  }

  // Calcular metricas
  const totalStories = stories.length;
  const doneStories = stories.filter((s) => s.status === 'done').length;
  const inProgressStories = stories.filter((s) => s.status === 'in_progress').length;
  const blockedStories = stories.filter((s) => s.status === 'blocked').length;
  const progress = totalStories > 0 ? Math.round((doneStories / totalStories) * 100) : 0;

  const recentStories = stories
    .filter((s) => s.status !== 'done')
    .slice(0, 5);

  const styles = createStyles(colors);

  return (
    <SafeAreaView style={styles.container} edges={['top']}>
      <ScrollView
        style={styles.scrollView}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} />
        }
      >
        {/* Header */}
        <View style={styles.header}>
          <Text style={styles.greeting}>
            Ola, {user?.username || 'Usuario'}!
          </Text>
          <Text style={styles.subtitle}>Acompanhe o progresso do projeto</Text>
        </View>

        {/* Progress Card */}
        <View style={styles.progressCard}>
          <View style={styles.progressHeader}>
            <Text style={styles.progressTitle}>Progresso Geral</Text>
            <Text style={styles.progressPercent}>{progress}%</Text>
          </View>
          <View style={styles.progressBarBg}>
            <View style={[styles.progressBarFill, { width: `${progress}%` }]} />
          </View>
          <Text style={styles.progressSubtext}>
            {doneStories} de {totalStories} requisitos concluidos
          </Text>
        </View>

        {/* Stats Grid */}
        <View style={styles.statsGrid}>
          <View style={[styles.statCard, { backgroundColor: colors.success + '20' }]}>
            <CheckCircle color={colors.success} size={24} />
            <Text style={[styles.statNumber, { color: colors.success }]}>
              {doneStories}
            </Text>
            <Text style={styles.statLabel}>Concluidos</Text>
          </View>

          <View style={[styles.statCard, { backgroundColor: colors.info + '20' }]}>
            <Clock color={colors.info} size={24} />
            <Text style={[styles.statNumber, { color: colors.info }]}>
              {inProgressStories}
            </Text>
            <Text style={styles.statLabel}>Em Andamento</Text>
          </View>

          <View style={[styles.statCard, { backgroundColor: colors.warning + '20' }]}>
            <TrendingUp color={colors.warning} size={24} />
            <Text style={[styles.statNumber, { color: colors.warning }]}>
              {totalStories - doneStories - inProgressStories}
            </Text>
            <Text style={styles.statLabel}>Pendentes</Text>
          </View>

          <View style={[styles.statCard, { backgroundColor: colors.error + '20' }]}>
            <AlertCircle color={colors.error} size={24} />
            <Text style={[styles.statNumber, { color: colors.error }]}>
              {blockedStories}
            </Text>
            <Text style={styles.statLabel}>Bloqueados</Text>
          </View>
        </View>

        {/* Recent Stories */}
        <View style={styles.section}>
          <View style={styles.sectionHeader}>
            <Text style={styles.sectionTitle}>Requisitos Recentes</Text>
            <TouchableOpacity onPress={() => navigation.navigate('Stories')}>
              <Text style={styles.seeAll}>Ver todos</Text>
            </TouchableOpacity>
          </View>

          {recentStories.map((story) => (
            <TouchableOpacity
              key={story.story_id}
              style={styles.storyItem}
              onPress={() =>
                navigation.navigate('StoryDetail', { storyId: story.story_id })
              }
            >
              <View style={styles.storyInfo}>
                <Text style={styles.storyTitle} numberOfLines={1}>
                  {story.title}
                </Text>
                <View style={styles.storyMeta}>
                  <View
                    style={[
                      styles.statusBadge,
                      { backgroundColor: getStatusColor(story.status, colors) + '20' },
                    ]}
                  >
                    <Text
                      style={[
                        styles.statusText,
                        { color: getStatusColor(story.status, colors) },
                      ]}
                    >
                      {getStatusLabel(story.status)}
                    </Text>
                  </View>
                  <Text style={styles.storyPoints}>{story.story_points} pts</Text>
                </View>
              </View>
              <ChevronRight color={colors.textSecondary} size={20} />
            </TouchableOpacity>
          ))}

          {recentStories.length === 0 && (
            <Text style={styles.emptyText}>Nenhum requisito pendente</Text>
          )}
        </View>
      </ScrollView>
    </SafeAreaView>
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
    blocked: colors.error,
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
    blocked: 'Bloqueado',
  };
  return map[status] || status;
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    scrollView: {
      flex: 1,
    },
    header: {
      padding: 20,
      backgroundColor: colors.primary,
    },
    greeting: {
      fontSize: 24,
      fontWeight: 'bold',
      color: '#FFF',
    },
    subtitle: {
      fontSize: 14,
      color: 'rgba(255,255,255,0.8)',
      marginTop: 4,
    },
    progressCard: {
      backgroundColor: colors.surface,
      margin: 16,
      padding: 20,
      borderRadius: 16,
      shadowColor: '#000',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.1,
      shadowRadius: 8,
      elevation: 3,
    },
    progressHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 12,
    },
    progressTitle: {
      fontSize: 16,
      fontWeight: '600',
      color: colors.text,
    },
    progressPercent: {
      fontSize: 24,
      fontWeight: 'bold',
      color: colors.primary,
    },
    progressBarBg: {
      height: 8,
      backgroundColor: colors.border,
      borderRadius: 4,
      overflow: 'hidden',
    },
    progressBarFill: {
      height: '100%',
      backgroundColor: colors.success,
      borderRadius: 4,
    },
    progressSubtext: {
      fontSize: 13,
      color: colors.textSecondary,
      marginTop: 8,
    },
    statsGrid: {
      flexDirection: 'row',
      flexWrap: 'wrap',
      padding: 8,
    },
    statCard: {
      width: '46%',
      margin: '2%',
      padding: 16,
      borderRadius: 12,
      alignItems: 'center',
    },
    statNumber: {
      fontSize: 28,
      fontWeight: 'bold',
      marginTop: 8,
    },
    statLabel: {
      fontSize: 12,
      color: colors.textSecondary,
      marginTop: 4,
    },
    section: {
      padding: 16,
    },
    sectionHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 12,
    },
    sectionTitle: {
      fontSize: 18,
      fontWeight: '600',
      color: colors.text,
    },
    seeAll: {
      fontSize: 14,
      color: colors.primary,
      fontWeight: '500',
    },
    storyItem: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: colors.surface,
      padding: 16,
      borderRadius: 12,
      marginBottom: 8,
    },
    storyInfo: {
      flex: 1,
    },
    storyTitle: {
      fontSize: 15,
      fontWeight: '500',
      color: colors.text,
      marginBottom: 8,
    },
    storyMeta: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 8,
    },
    statusBadge: {
      paddingHorizontal: 8,
      paddingVertical: 4,
      borderRadius: 6,
    },
    statusText: {
      fontSize: 11,
      fontWeight: '600',
    },
    storyPoints: {
      fontSize: 12,
      color: colors.textSecondary,
    },
    emptyText: {
      textAlign: 'center',
      color: colors.textSecondary,
      padding: 20,
    },
  });
