/**
 * Scroll Components - Issue #429
 * PullToRefresh e InfiniteScroll
 */

import React, { useState, useCallback, useRef } from 'react';
import {
  View,
  Text,
  ScrollView,
  FlatList,
  RefreshControl,
  ActivityIndicator,
  StyleSheet,
  Animated,
  ViewStyle,
  NativeScrollEvent,
  NativeSyntheticEvent,
} from 'react-native';
import { RefreshCw, ArrowDown } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

// Pull To Refresh Wrapper
interface PullToRefreshProps {
  children: React.ReactNode;
  onRefresh: () => Promise<void>;
  refreshingText?: string;
  pullText?: string;
  releaseText?: string;
  style?: ViewStyle;
}

export function PullToRefresh({
  children,
  onRefresh,
  refreshingText = 'Atualizando...',
  pullText = 'Puxe para atualizar',
  releaseText = 'Solte para atualizar',
  style,
}: PullToRefreshProps) {
  const { colors } = useTheme();
  const [refreshing, setRefreshing] = useState(false);

  const handleRefresh = useCallback(async () => {
    setRefreshing(true);
    try {
      await onRefresh();
    } finally {
      setRefreshing(false);
    }
  }, [onRefresh]);

  return (
    <ScrollView
      style={style}
      refreshControl={
        <RefreshControl
          refreshing={refreshing}
          onRefresh={handleRefresh}
          tintColor={colors.primary}
          colors={[colors.primary]}
          progressBackgroundColor={colors.surface}
        />
      }
    >
      {children}
    </ScrollView>
  );
}

// Infinite Scroll List
interface InfiniteScrollProps<T> {
  data: T[];
  renderItem: (item: T, index: number) => React.ReactNode;
  keyExtractor: (item: T, index: number) => string;
  onLoadMore: () => Promise<void>;
  onRefresh?: () => Promise<void>;
  hasMore?: boolean;
  isLoading?: boolean;
  loadingComponent?: React.ReactNode;
  emptyComponent?: React.ReactNode;
  headerComponent?: React.ReactNode;
  footerComponent?: React.ReactNode;
  threshold?: number;
  numColumns?: number;
  horizontal?: boolean;
  style?: ViewStyle;
  contentContainerStyle?: ViewStyle;
}

export function InfiniteScroll<T>({
  data,
  renderItem,
  keyExtractor,
  onLoadMore,
  onRefresh,
  hasMore = true,
  isLoading = false,
  loadingComponent,
  emptyComponent,
  headerComponent,
  footerComponent,
  threshold = 0.5,
  numColumns,
  horizontal = false,
  style,
  contentContainerStyle,
}: InfiniteScrollProps<T>) {
  const { colors } = useTheme();
  const [refreshing, setRefreshing] = useState(false);
  const isLoadingMore = useRef(false);

  const handleRefresh = useCallback(async () => {
    if (!onRefresh) return;
    setRefreshing(true);
    try {
      await onRefresh();
    } finally {
      setRefreshing(false);
    }
  }, [onRefresh]);

  const handleEndReached = useCallback(async () => {
    if (isLoadingMore.current || !hasMore || isLoading) return;

    isLoadingMore.current = true;
    try {
      await onLoadMore();
    } finally {
      isLoadingMore.current = false;
    }
  }, [hasMore, isLoading, onLoadMore]);

  const renderFooter = () => {
    if (footerComponent) return footerComponent;

    if (isLoading && data.length > 0) {
      return (
        loadingComponent || (
          <View style={styles.loadingFooter}>
            <ActivityIndicator color={colors.primary} size="small" />
            <Text style={[styles.loadingText, { color: colors.textSecondary }]}>
              Carregando mais...
            </Text>
          </View>
        )
      );
    }

    if (!hasMore && data.length > 0) {
      return (
        <View style={styles.endMessage}>
          <Text style={[styles.endText, { color: colors.textSecondary }]}>
            Fim da lista
          </Text>
        </View>
      );
    }

    return null;
  };

  const renderEmpty = () => {
    if (isLoading) {
      return (
        loadingComponent || (
          <View style={styles.emptyContainer}>
            <ActivityIndicator color={colors.primary} size="large" />
            <Text style={[styles.loadingText, { color: colors.textSecondary }]}>
              Carregando...
            </Text>
          </View>
        )
      );
    }

    return emptyComponent || (
      <View style={styles.emptyContainer}>
        <Text style={[styles.emptyText, { color: colors.textSecondary }]}>
          Nenhum item encontrado
        </Text>
      </View>
    );
  };

  return (
    <FlatList
      data={data}
      renderItem={({ item, index }) => (
        <View key={keyExtractor(item, index)}>
          {renderItem(item, index)}
        </View>
      )}
      keyExtractor={keyExtractor}
      style={style}
      contentContainerStyle={[
        data.length === 0 && styles.emptyList,
        contentContainerStyle,
      ]}
      horizontal={horizontal}
      numColumns={numColumns}
      onEndReached={handleEndReached}
      onEndReachedThreshold={threshold}
      ListHeaderComponent={headerComponent as React.ComponentType<any>}
      ListFooterComponent={renderFooter}
      ListEmptyComponent={renderEmpty}
      refreshControl={
        onRefresh ? (
          <RefreshControl
            refreshing={refreshing}
            onRefresh={handleRefresh}
            tintColor={colors.primary}
            colors={[colors.primary]}
          />
        ) : undefined
      }
    />
  );
}

// Load More Button - Alternative to infinite scroll
interface LoadMoreButtonProps {
  onPress: () => void;
  isLoading?: boolean;
  hasMore?: boolean;
  text?: string;
  loadingText?: string;
  style?: ViewStyle;
}

export function LoadMoreButton({
  onPress,
  isLoading = false,
  hasMore = true,
  text = 'Carregar mais',
  loadingText = 'Carregando...',
  style,
}: LoadMoreButtonProps) {
  const { colors } = useTheme();

  if (!hasMore) return null;

  return (
    <View style={[styles.loadMoreContainer, style]}>
      {isLoading ? (
        <View style={styles.loadMoreContent}>
          <ActivityIndicator color={colors.primary} size="small" />
          <Text style={[styles.loadMoreText, { color: colors.textSecondary }]}>
            {loadingText}
          </Text>
        </View>
      ) : (
        <View
          style={[
            styles.loadMoreButton,
            { borderColor: colors.primary },
          ]}
        >
          <Text
            style={[styles.loadMoreButtonText, { color: colors.primary }]}
            onPress={onPress}
          >
            {text}
          </Text>
          <ArrowDown color={colors.primary} size={16} />
        </View>
      )}
    </View>
  );
}

// Scroll To Top Button
interface ScrollToTopProps {
  scrollRef: React.RefObject<ScrollView | FlatList<any>>;
  threshold?: number;
  style?: ViewStyle;
}

export function ScrollToTop({
  scrollRef,
  threshold = 300,
  style,
}: ScrollToTopProps) {
  const { colors } = useTheme();
  const [visible, setVisible] = useState(false);
  const opacity = useRef(new Animated.Value(0)).current;

  const handleScroll = useCallback((event: NativeSyntheticEvent<NativeScrollEvent>) => {
    const offsetY = event.nativeEvent.contentOffset.y;
    const shouldShow = offsetY > threshold;

    if (shouldShow !== visible) {
      setVisible(shouldShow);
      Animated.timing(opacity, {
        toValue: shouldShow ? 1 : 0,
        duration: 200,
        useNativeDriver: true,
      }).start();
    }
  }, [threshold, visible, opacity]);

  const scrollToTop = () => {
    if (scrollRef.current) {
      if ('scrollToOffset' in scrollRef.current) {
        (scrollRef.current as FlatList).scrollToOffset({ offset: 0, animated: true });
      } else {
        (scrollRef.current as ScrollView).scrollTo({ y: 0, animated: true });
      }
    }
  };

  return (
    <Animated.View
      style={[
        styles.scrollToTop,
        { backgroundColor: colors.primary, opacity },
        style,
      ]}
      pointerEvents={visible ? 'auto' : 'none'}
    >
      <Text onPress={scrollToTop}>
        <ArrowDown
          color="#FFF"
          size={24}
          style={{ transform: [{ rotate: '180deg' }] }}
        />
      </Text>
    </Animated.View>
  );
}

// Pagination Component
interface PaginationProps {
  currentPage: number;
  totalPages: number;
  onPageChange: (page: number) => void;
  showFirstLast?: boolean;
  maxVisible?: number;
  style?: ViewStyle;
}

export function Pagination({
  currentPage,
  totalPages,
  onPageChange,
  showFirstLast = true,
  maxVisible = 5,
  style,
}: PaginationProps) {
  const { colors } = useTheme();

  if (totalPages <= 1) return null;

  const getVisiblePages = () => {
    const pages: (number | 'ellipsis')[] = [];
    const halfVisible = Math.floor(maxVisible / 2);

    let start = Math.max(1, currentPage - halfVisible);
    let end = Math.min(totalPages, start + maxVisible - 1);

    if (end - start < maxVisible - 1) {
      start = Math.max(1, end - maxVisible + 1);
    }

    if (showFirstLast && start > 1) {
      pages.push(1);
      if (start > 2) pages.push('ellipsis');
    }

    for (let i = start; i <= end; i++) {
      pages.push(i);
    }

    if (showFirstLast && end < totalPages) {
      if (end < totalPages - 1) pages.push('ellipsis');
      pages.push(totalPages);
    }

    return pages;
  };

  const pages = getVisiblePages();

  return (
    <View style={[styles.pagination, style]}>
      {pages.map((page, index) =>
        page === 'ellipsis' ? (
          <Text
            key={`ellipsis-${index}`}
            style={[styles.ellipsis, { color: colors.textSecondary }]}
          >
            ...
          </Text>
        ) : (
          <Text
            key={page}
            style={[
              styles.pageButton,
              {
                backgroundColor: page === currentPage ? colors.primary : colors.surface,
                borderColor: colors.border,
                color: page === currentPage ? '#FFF' : colors.text,
              },
            ]}
            onPress={() => onPageChange(page)}
          >
            {page}
          </Text>
        )
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  loadingFooter: {
    flexDirection: 'row',
    justifyContent: 'center',
    alignItems: 'center',
    paddingVertical: 16,
    gap: 8,
  },
  loadingText: {
    fontSize: 14,
    marginTop: 8,
  },
  endMessage: {
    paddingVertical: 16,
    alignItems: 'center',
  },
  endText: {
    fontSize: 13,
  },
  emptyList: {
    flexGrow: 1,
  },
  emptyContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    paddingVertical: 48,
  },
  emptyText: {
    fontSize: 15,
  },
  loadMoreContainer: {
    paddingVertical: 16,
    alignItems: 'center',
  },
  loadMoreContent: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  loadMoreText: {
    fontSize: 14,
  },
  loadMoreButton: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 10,
    paddingHorizontal: 20,
    borderRadius: 20,
    borderWidth: 1,
    gap: 6,
  },
  loadMoreButtonText: {
    fontSize: 14,
    fontWeight: '500',
  },
  scrollToTop: {
    position: 'absolute',
    right: 16,
    bottom: 80,
    width: 48,
    height: 48,
    borderRadius: 24,
    justifyContent: 'center',
    alignItems: 'center',
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.2,
    shadowRadius: 4,
    elevation: 4,
  },
  pagination: {
    flexDirection: 'row',
    justifyContent: 'center',
    alignItems: 'center',
    gap: 4,
    paddingVertical: 16,
  },
  pageButton: {
    minWidth: 36,
    height: 36,
    borderRadius: 8,
    borderWidth: 1,
    textAlign: 'center',
    textAlignVertical: 'center',
    lineHeight: 36,
    fontSize: 14,
    fontWeight: '500',
  },
  ellipsis: {
    paddingHorizontal: 8,
    fontSize: 14,
  },
});
