/**
 * Stories Screen - Lista de requisitos
 * Filtros, busca e criacao de novas stories
 */

import React, { useEffect, useState } from 'react';
import {
  View,
  Text,
  FlatList,
  StyleSheet,
  TouchableOpacity,
  TextInput,
  RefreshControl,
  Modal,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import {
  Search,
  Filter,
  Plus,
  ChevronRight,
  X,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { storiesApi, Story } from '../services/api';

const FILTERS = ['Todos', 'Backlog', 'Em Andamento', 'Concluidos'];

export function StoriesScreen({ navigation }: any) {
  const { colors } = useTheme();
  const [stories, setStories] = useState<Story[]>([]);
  const [filteredStories, setFilteredStories] = useState<Story[]>([]);
  const [refreshing, setRefreshing] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [activeFilter, setActiveFilter] = useState('Todos');
  const [showCreateModal, setShowCreateModal] = useState(false);

  useEffect(() => {
    loadData();
  }, []);

  useEffect(() => {
    filterStories();
  }, [stories, searchQuery, activeFilter]);

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

  function filterStories() {
    let result = [...stories];

    // Filtro por status
    if (activeFilter === 'Backlog') {
      result = result.filter((s) => ['backlog', 'ready'].includes(s.status));
    } else if (activeFilter === 'Em Andamento') {
      result = result.filter((s) =>
        ['in_progress', 'review', 'testing'].includes(s.status)
      );
    } else if (activeFilter === 'Concluidos') {
      result = result.filter((s) => s.status === 'done');
    }

    // Filtro por busca
    if (searchQuery.trim()) {
      const query = searchQuery.toLowerCase();
      result = result.filter(
        (s) =>
          s.title.toLowerCase().includes(query) ||
          s.story_id.toLowerCase().includes(query)
      );
    }

    setFilteredStories(result);
  }

  const styles = createStyles(colors);

  const renderStoryItem = ({ item }: { item: Story }) => (
    <TouchableOpacity
      style={styles.storyCard}
      onPress={() =>
        navigation.navigate('StoryDetail', { storyId: item.story_id })
      }
    >
      <View style={styles.storyHeader}>
        <Text style={styles.storyId}>{item.story_id}</Text>
        <View
          style={[
            styles.priorityBadge,
            { backgroundColor: getPriorityColor(item.priority, colors) + '20' },
          ]}
        >
          <Text
            style={[
              styles.priorityText,
              { color: getPriorityColor(item.priority, colors) },
            ]}
          >
            {item.priority || 'medium'}
          </Text>
        </View>
      </View>

      <Text style={styles.storyTitle} numberOfLines={2}>
        {item.title}
      </Text>

      {item.persona && (
        <Text style={styles.storyNarrative} numberOfLines={1}>
          Como {item.persona}, eu quero {item.action}
        </Text>
      )}

      <View style={styles.storyFooter}>
        <View
          style={[
            styles.statusBadge,
            { backgroundColor: getStatusColor(item.status, colors) + '20' },
          ]}
        >
          <Text
            style={[
              styles.statusText,
              { color: getStatusColor(item.status, colors) },
            ]}
          >
            {getStatusLabel(item.status)}
          </Text>
        </View>

        <View style={styles.storyMeta}>
          <Text style={styles.storyPoints}>{item.story_points} pts</Text>
          <View style={styles.progressMini}>
            <View
              style={[
                styles.progressMiniFill,
                { width: `${item.progress || 0}%` },
              ]}
            />
          </View>
        </View>

        <ChevronRight color={colors.textSecondary} size={20} />
      </View>
    </TouchableOpacity>
  );

  return (
    <SafeAreaView style={styles.container} edges={['top']}>
      {/* Header */}
      <View style={styles.header}>
        <Text style={styles.title}>Requisitos</Text>
        <TouchableOpacity
          style={styles.addButton}
          onPress={() => setShowCreateModal(true)}
        >
          <Plus color="#FFF" size={20} />
        </TouchableOpacity>
      </View>

      {/* Search */}
      <View style={styles.searchContainer}>
        <View style={styles.searchBox}>
          <Search color={colors.textSecondary} size={20} />
          <TextInput
            style={styles.searchInput}
            placeholder="Buscar requisito..."
            placeholderTextColor={colors.textSecondary}
            value={searchQuery}
            onChangeText={setSearchQuery}
          />
          {searchQuery.length > 0 && (
            <TouchableOpacity onPress={() => setSearchQuery('')}>
              <X color={colors.textSecondary} size={18} />
            </TouchableOpacity>
          )}
        </View>
      </View>

      {/* Filters */}
      <View style={styles.filtersContainer}>
        {FILTERS.map((filter) => (
          <TouchableOpacity
            key={filter}
            style={[
              styles.filterBtn,
              activeFilter === filter && styles.filterBtnActive,
            ]}
            onPress={() => setActiveFilter(filter)}
          >
            <Text
              style={[
                styles.filterText,
                activeFilter === filter && styles.filterTextActive,
              ]}
            >
              {filter}
            </Text>
          </TouchableOpacity>
        ))}
      </View>

      {/* Stories List */}
      <FlatList
        data={filteredStories}
        keyExtractor={(item) => item.story_id}
        renderItem={renderStoryItem}
        contentContainerStyle={styles.listContent}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} />
        }
        ListEmptyComponent={
          <View style={styles.emptyState}>
            <Text style={styles.emptyText}>Nenhum requisito encontrado</Text>
          </View>
        }
      />

      {/* Create Story Modal */}
      <CreateStoryModal
        visible={showCreateModal}
        onClose={() => setShowCreateModal(false)}
        onCreated={(story) => {
          setStories([story, ...stories]);
          setShowCreateModal(false);
        }}
        colors={colors}
      />
    </SafeAreaView>
  );
}

// Modal de criacao simplificado
function CreateStoryModal({
  visible,
  onClose,
  onCreated,
  colors,
}: {
  visible: boolean;
  onClose: () => void;
  onCreated: (story: Story) => void;
  colors: any;
}) {
  const [title, setTitle] = useState('');
  const [persona, setPersona] = useState('');
  const [action, setAction] = useState('');
  const [loading, setLoading] = useState(false);

  async function handleCreate() {
    if (!title.trim()) return;

    setLoading(true);
    try {
      const story = await storiesApi.create({
        title: title.trim(),
        persona: persona.trim() || 'usuario',
        action: action.trim() || title.trim(),
        benefit: 'melhorar a experiencia',
        status: 'backlog',
        priority: 'medium',
        story_points: 3,
      });
      onCreated(story);
      setTitle('');
      setPersona('');
      setAction('');
    } catch (error) {
      console.error('Erro ao criar story:', error);
    } finally {
      setLoading(false);
    }
  }

  return (
    <Modal visible={visible} animationType="slide" transparent>
      <View style={modalStyles(colors).overlay}>
        <View style={modalStyles(colors).content}>
          <View style={modalStyles(colors).header}>
            <Text style={modalStyles(colors).title}>Novo Requisito</Text>
            <TouchableOpacity onPress={onClose}>
              <X color={colors.text} size={24} />
            </TouchableOpacity>
          </View>

          <TextInput
            style={modalStyles(colors).input}
            placeholder="Titulo do requisito *"
            placeholderTextColor={colors.textSecondary}
            value={title}
            onChangeText={setTitle}
          />

          <TextInput
            style={modalStyles(colors).input}
            placeholder="Como um(a)... (ex: cliente, admin)"
            placeholderTextColor={colors.textSecondary}
            value={persona}
            onChangeText={setPersona}
          />

          <TextInput
            style={[modalStyles(colors).input, { height: 80 }]}
            placeholder="Eu quero... (descreva a funcionalidade)"
            placeholderTextColor={colors.textSecondary}
            value={action}
            onChangeText={setAction}
            multiline
          />

          <TouchableOpacity
            style={[
              modalStyles(colors).createBtn,
              !title.trim() && { opacity: 0.5 },
            ]}
            onPress={handleCreate}
            disabled={!title.trim() || loading}
          >
            <Text style={modalStyles(colors).createBtnText}>
              {loading ? 'Criando...' : 'Criar Requisito'}
            </Text>
          </TouchableOpacity>
        </View>
      </View>
    </Modal>
  );
}

const modalStyles = (colors: any) =>
  StyleSheet.create({
    overlay: {
      flex: 1,
      backgroundColor: 'rgba(0,0,0,0.5)',
      justifyContent: 'flex-end',
    },
    content: {
      backgroundColor: colors.surface,
      borderTopLeftRadius: 24,
      borderTopRightRadius: 24,
      padding: 24,
    },
    header: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 20,
    },
    title: {
      fontSize: 20,
      fontWeight: '600',
      color: colors.text,
    },
    input: {
      backgroundColor: colors.background,
      borderRadius: 12,
      padding: 16,
      fontSize: 15,
      color: colors.text,
      marginBottom: 12,
    },
    createBtn: {
      backgroundColor: colors.primary,
      padding: 16,
      borderRadius: 12,
      alignItems: 'center',
      marginTop: 8,
    },
    createBtnText: {
      color: '#FFF',
      fontSize: 16,
      fontWeight: '600',
    },
  });

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

function getPriorityColor(priority: string, colors: any): string {
  const map: Record<string, string> = {
    urgent: colors.error,
    high: colors.warning,
    medium: colors.info,
    low: colors.textSecondary,
  };
  return map[priority] || colors.textSecondary;
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    header: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      padding: 20,
      backgroundColor: colors.primary,
    },
    title: {
      fontSize: 24,
      fontWeight: 'bold',
      color: '#FFF',
    },
    addButton: {
      width: 40,
      height: 40,
      borderRadius: 20,
      backgroundColor: 'rgba(255,255,255,0.2)',
      justifyContent: 'center',
      alignItems: 'center',
    },
    searchContainer: {
      padding: 16,
      backgroundColor: colors.surface,
    },
    searchBox: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: colors.background,
      borderRadius: 12,
      paddingHorizontal: 12,
      gap: 8,
    },
    searchInput: {
      flex: 1,
      paddingVertical: 12,
      fontSize: 15,
      color: colors.text,
    },
    filtersContainer: {
      flexDirection: 'row',
      paddingHorizontal: 16,
      paddingVertical: 8,
      backgroundColor: colors.surface,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
      gap: 8,
    },
    filterBtn: {
      paddingHorizontal: 16,
      paddingVertical: 8,
      borderRadius: 20,
      backgroundColor: colors.background,
    },
    filterBtnActive: {
      backgroundColor: colors.primary,
    },
    filterText: {
      fontSize: 13,
      fontWeight: '500',
      color: colors.textSecondary,
    },
    filterTextActive: {
      color: '#FFF',
    },
    listContent: {
      padding: 16,
    },
    storyCard: {
      backgroundColor: colors.surface,
      borderRadius: 12,
      padding: 16,
      marginBottom: 12,
    },
    storyHeader: {
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
    priorityBadge: {
      paddingHorizontal: 8,
      paddingVertical: 2,
      borderRadius: 4,
    },
    priorityText: {
      fontSize: 10,
      fontWeight: '600',
      textTransform: 'uppercase',
    },
    storyTitle: {
      fontSize: 16,
      fontWeight: '600',
      color: colors.text,
      marginBottom: 4,
    },
    storyNarrative: {
      fontSize: 13,
      color: colors.textSecondary,
      fontStyle: 'italic',
      marginBottom: 12,
    },
    storyFooter: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 12,
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
    storyMeta: {
      flex: 1,
      flexDirection: 'row',
      alignItems: 'center',
      gap: 8,
    },
    storyPoints: {
      fontSize: 12,
      color: colors.textSecondary,
    },
    progressMini: {
      flex: 1,
      height: 4,
      backgroundColor: colors.border,
      borderRadius: 2,
      overflow: 'hidden',
    },
    progressMiniFill: {
      height: '100%',
      backgroundColor: colors.success,
    },
    emptyState: {
      alignItems: 'center',
      padding: 40,
    },
    emptyText: {
      fontSize: 14,
      color: colors.textSecondary,
    },
  });
