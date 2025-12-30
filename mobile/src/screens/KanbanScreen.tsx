/**
 * Kanban Screen - Quadro de tarefas mobile-first
 * Scroll horizontal entre colunas, drag-and-drop futuro
 */

import React, { useEffect, useState, useRef } from 'react';
import {
  View,
  Text,
  ScrollView,
  StyleSheet,
  Dimensions,
  TouchableOpacity,
  RefreshControl,
  Animated,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import {
  Inbox,
  PlayCircle,
  Eye,
  TestTube,
  CheckCircle,
  AlertCircle,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { storiesApi, Story } from '../services/api';
import { StoryCard } from '../components';

const { width: SCREEN_WIDTH } = Dimensions.get('window');
const COLUMN_WIDTH = SCREEN_WIDTH * 0.8;

interface KanbanColumn {
  id: string;
  title: string;
  icon: any;
  color: string;
}

const COLUMNS: KanbanColumn[] = [
  { id: 'backlog', title: 'Backlog', icon: Inbox, color: '#6B7280' },
  { id: 'ready', title: 'Pronto', icon: PlayCircle, color: '#3B82F6' },
  { id: 'in_progress', title: 'Em Andamento', icon: PlayCircle, color: '#F59E0B' },
  { id: 'review', title: 'Revisao', icon: Eye, color: '#8B5CF6' },
  { id: 'testing', title: 'Teste', icon: TestTube, color: '#EC4899' },
  { id: 'done', title: 'Concluido', icon: CheckCircle, color: '#10B981' },
];

export function KanbanScreen({ navigation }: any) {
  const { colors } = useTheme();
  const [stories, setStories] = useState<Story[]>([]);
  const [refreshing, setRefreshing] = useState(false);
  const [activeColumn, setActiveColumn] = useState(0);
  const scrollRef = useRef<ScrollView>(null);

  useEffect(() => {
    loadData();
  }, []);

  async function loadData() {
    try {
      const data = await storiesApi.list();
      setStories(data);
    } catch (error) {
      console.error('Erro ao carregar stories:', error);
    }
  }

  async function onRefresh() {
    setRefreshing(true);
    await loadData();
    setRefreshing(false);
  }

  async function moveStory(storyId: string, newStatus: string) {
    try {
      await storiesApi.move(storyId, newStatus);
      // Atualizar localmente
      setStories((prev) =>
        prev.map((s) =>
          s.story_id === storyId ? { ...s, status: newStatus } : s
        )
      );
    } catch (error) {
      console.error('Erro ao mover story:', error);
    }
  }

  function getStoriesByStatus(status: string): Story[] {
    return stories.filter((s) => s.status === status);
  }

  const handleScroll = (event: any) => {
    const offsetX = event.nativeEvent.contentOffset.x;
    const index = Math.round(offsetX / COLUMN_WIDTH);
    setActiveColumn(index);
  };

  const styles = createStyles(colors);

  return (
    <SafeAreaView style={styles.container} edges={['top']}>
      {/* Header */}
      <View style={styles.header}>
        <Text style={styles.title}>Quadro de Tarefas</Text>
        <Text style={styles.subtitle}>{stories.length} requisitos</Text>
      </View>

      {/* Column Indicators */}
      <View style={styles.indicators}>
        {COLUMNS.map((col, index) => (
          <TouchableOpacity
            key={col.id}
            style={[
              styles.indicator,
              activeColumn === index && { backgroundColor: col.color },
            ]}
            onPress={() => {
              scrollRef.current?.scrollTo({
                x: index * COLUMN_WIDTH,
                animated: true,
              });
            }}
          >
            <Text
              style={[
                styles.indicatorText,
                activeColumn === index && { color: '#FFF' },
              ]}
            >
              {getStoriesByStatus(col.id).length}
            </Text>
          </TouchableOpacity>
        ))}
      </View>

      {/* Kanban Columns */}
      <ScrollView
        ref={scrollRef}
        horizontal
        pagingEnabled
        showsHorizontalScrollIndicator={false}
        onScroll={handleScroll}
        scrollEventThrottle={16}
        decelerationRate="fast"
        snapToInterval={COLUMN_WIDTH}
        contentContainerStyle={styles.columnsContainer}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} />
        }
      >
        {COLUMNS.map((column) => {
          const Icon = column.icon;
          const columnStories = getStoriesByStatus(column.id);

          return (
            <View key={column.id} style={styles.column}>
              {/* Column Header */}
              <View
                style={[
                  styles.columnHeader,
                  { backgroundColor: column.color + '15' },
                ]}
              >
                <Icon color={column.color} size={20} />
                <Text style={[styles.columnTitle, { color: column.color }]}>
                  {column.title}
                </Text>
                <View
                  style={[
                    styles.columnCount,
                    { backgroundColor: column.color },
                  ]}
                >
                  <Text style={styles.columnCountText}>
                    {columnStories.length}
                  </Text>
                </View>
              </View>

              {/* Cards - Usando StoryCard fiel ao dashboard web */}
              <ScrollView
                style={styles.cardsContainer}
                showsVerticalScrollIndicator={false}
              >
                {columnStories.map((story) => (
                  <StoryCard
                    key={story.story_id}
                    story={story}
                    columnColor={column.color}
                    onPress={() =>
                      navigation.navigate('StoryDetail', {
                        storyId: story.story_id,
                      })
                    }
                    onMoveNext={
                      column.id !== 'done'
                        ? () => {
                            const nextStatus = getNextStatus(column.id);
                            if (nextStatus) {
                              moveStory(story.story_id, nextStatus);
                            }
                          }
                        : undefined
                    }
                    onDelete={async () => {
                      try {
                        await storiesApi.delete(story.story_id);
                        setStories((prev) =>
                          prev.filter((s) => s.story_id !== story.story_id)
                        );
                      } catch (error) {
                        console.error('Erro ao deletar story:', error);
                      }
                    }}
                  />
                ))}

                {columnStories.length === 0 && (
                  <View style={styles.emptyColumn}>
                    <Icon color={colors.textSecondary} size={32} />
                    <Text style={styles.emptyText}>Nenhum item</Text>
                  </View>
                )}
              </ScrollView>
            </View>
          );
        })}
      </ScrollView>
    </SafeAreaView>
  );
}

function getNextStatus(current: string): string | null {
  const flow: Record<string, string> = {
    backlog: 'ready',
    ready: 'in_progress',
    in_progress: 'review',
    review: 'testing',
    testing: 'done',
  };
  return flow[current] || null;
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    header: {
      padding: 20,
      backgroundColor: colors.primary,
    },
    title: {
      fontSize: 24,
      fontWeight: 'bold',
      color: '#FFF',
    },
    subtitle: {
      fontSize: 14,
      color: 'rgba(255,255,255,0.8)',
      marginTop: 4,
    },
    indicators: {
      flexDirection: 'row',
      justifyContent: 'center',
      padding: 12,
      backgroundColor: colors.surface,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    indicator: {
      width: 32,
      height: 32,
      borderRadius: 16,
      backgroundColor: colors.border,
      justifyContent: 'center',
      alignItems: 'center',
      marginHorizontal: 4,
    },
    indicatorText: {
      fontSize: 12,
      fontWeight: '600',
      color: colors.textSecondary,
    },
    columnsContainer: {
      paddingHorizontal: (SCREEN_WIDTH - COLUMN_WIDTH) / 2,
    },
    column: {
      width: COLUMN_WIDTH,
      paddingHorizontal: 8,
    },
    columnHeader: {
      flexDirection: 'row',
      alignItems: 'center',
      padding: 12,
      borderRadius: 12,
      marginVertical: 12,
      gap: 8,
    },
    columnTitle: {
      flex: 1,
      fontSize: 16,
      fontWeight: '600',
    },
    columnCount: {
      paddingHorizontal: 10,
      paddingVertical: 4,
      borderRadius: 12,
    },
    columnCountText: {
      fontSize: 12,
      fontWeight: 'bold',
      color: '#FFF',
    },
    cardsContainer: {
      flex: 1,
    },
    card: {
      backgroundColor: colors.surface,
      borderRadius: 12,
      padding: 16,
      marginBottom: 12,
      shadowColor: '#000',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.1,
      shadowRadius: 4,
      elevation: 2,
    },
    cardTitle: {
      fontSize: 15,
      fontWeight: '600',
      color: colors.text,
      marginBottom: 12,
    },
    progressContainer: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 8,
      marginBottom: 12,
    },
    progressBg: {
      flex: 1,
      height: 6,
      backgroundColor: colors.border,
      borderRadius: 3,
      overflow: 'hidden',
    },
    progressFill: {
      height: '100%',
      borderRadius: 3,
    },
    progressText: {
      fontSize: 12,
      fontWeight: '500',
      color: colors.textSecondary,
      width: 40,
      textAlign: 'right',
    },
    cardFooter: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 8,
    },
    pointsBadge: {
      backgroundColor: colors.primary + '15',
      paddingHorizontal: 8,
      paddingVertical: 4,
      borderRadius: 6,
    },
    pointsText: {
      fontSize: 11,
      fontWeight: '600',
      color: colors.primary,
    },
    cardId: {
      fontSize: 11,
      color: colors.textSecondary,
    },
    quickActions: {
      flexDirection: 'row',
      justifyContent: 'flex-end',
    },
    actionBtn: {
      paddingHorizontal: 12,
      paddingVertical: 6,
      borderRadius: 6,
    },
    actionText: {
      fontSize: 12,
      fontWeight: '600',
    },
    emptyColumn: {
      alignItems: 'center',
      padding: 40,
    },
    emptyText: {
      fontSize: 14,
      color: colors.textSecondary,
      marginTop: 8,
    },
  });
