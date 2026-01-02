/**
 * LoadingSpinner Component - Issue #423
 * Indicador de carregamento com tamanhos e estilos
 */

import React from 'react';
import {
  View,
  ActivityIndicator,
  Text,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

type SpinnerSize = 'small' | 'medium' | 'large';

interface LoadingSpinnerProps {
  size?: SpinnerSize;
  color?: string;
  message?: string;
  overlay?: boolean;
  style?: ViewStyle;
}

export function LoadingSpinner({
  size = 'medium',
  color,
  message,
  overlay = false,
  style,
}: LoadingSpinnerProps) {
  const { colors } = useTheme();

  const getSize = (): 'small' | 'large' => {
    return size === 'small' ? 'small' : 'large';
  };

  const getScale = () => {
    switch (size) {
      case 'small':
        return 1;
      case 'large':
        return 1.5;
      default:
        return 1.2;
    }
  };

  const spinnerColor = color || colors.primary;

  const content = (
    <View style={[styles.content, style]}>
      <View style={{ transform: [{ scale: getScale() }] }}>
        <ActivityIndicator size={getSize()} color={spinnerColor} />
      </View>
      {message && (
        <Text style={[styles.message, { color: colors.textSecondary }]}>
          {message}
        </Text>
      )}
    </View>
  );

  if (overlay) {
    return (
      <View style={styles.overlay}>
        <View style={[styles.overlayBox, { backgroundColor: colors.surface }]}>
          {content}
        </View>
      </View>
    );
  }

  return content;
}

// Full screen loading state
interface LoadingScreenProps {
  message?: string;
}

export function LoadingScreen({ message = 'Carregando...' }: LoadingScreenProps) {
  const { colors } = useTheme();

  return (
    <View style={[styles.fullScreen, { backgroundColor: colors.background }]}>
      <LoadingSpinner size="large" message={message} />
    </View>
  );
}

// Inline loading for lists
interface LoadingMoreProps {
  message?: string;
}

export function LoadingMore({ message = 'Carregando mais...' }: LoadingMoreProps) {
  const { colors } = useTheme();

  return (
    <View style={styles.loadingMore}>
      <ActivityIndicator size="small" color={colors.primary} />
      <Text style={[styles.loadingMoreText, { color: colors.textSecondary }]}>
        {message}
      </Text>
    </View>
  );
}

// Skeleton placeholder for loading content
interface SkeletonProps {
  width?: number | string;
  height?: number;
  borderRadius?: number;
  style?: ViewStyle;
}

export function Skeleton({
  width = '100%',
  height = 16,
  borderRadius = 4,
  style,
}: SkeletonProps) {
  const { colors } = useTheme();

  return (
    <View
      style={[
        styles.skeleton,
        {
          width,
          height,
          borderRadius,
          backgroundColor: colors.border,
        },
        style,
      ]}
    />
  );
}

// Skeleton for a card
export function SkeletonCard() {
  const { colors } = useTheme();

  return (
    <View style={[styles.skeletonCard, { backgroundColor: colors.surface }]}>
      <View style={styles.skeletonCardHeader}>
        <Skeleton width={40} height={40} borderRadius={20} />
        <View style={styles.skeletonCardHeaderText}>
          <Skeleton width={120} height={14} />
          <Skeleton width={80} height={12} style={{ marginTop: 6 }} />
        </View>
      </View>
      <Skeleton width="100%" height={14} style={{ marginTop: 16 }} />
      <Skeleton width="80%" height={14} style={{ marginTop: 8 }} />
      <Skeleton width="60%" height={14} style={{ marginTop: 8 }} />
    </View>
  );
}

// Skeleton for a list item
export function SkeletonListItem() {
  const { colors } = useTheme();

  return (
    <View style={[styles.skeletonListItem, { borderBottomColor: colors.border }]}>
      <Skeleton width={48} height={48} borderRadius={8} />
      <View style={styles.skeletonListItemContent}>
        <Skeleton width="70%" height={14} />
        <Skeleton width="50%" height={12} style={{ marginTop: 6 }} />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  content: {
    alignItems: 'center',
    justifyContent: 'center',
    padding: 16,
  },
  message: {
    marginTop: 12,
    fontSize: 14,
    textAlign: 'center',
  },
  overlay: {
    ...StyleSheet.absoluteFillObject,
    backgroundColor: 'rgba(0, 0, 0, 0.4)',
    justifyContent: 'center',
    alignItems: 'center',
    zIndex: 1000,
  },
  overlayBox: {
    padding: 24,
    borderRadius: 12,
    minWidth: 120,
    alignItems: 'center',
  },
  fullScreen: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  loadingMore: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    padding: 16,
    gap: 8,
  },
  loadingMoreText: {
    fontSize: 13,
  },
  skeleton: {
    overflow: 'hidden',
  },
  skeletonCard: {
    padding: 16,
    borderRadius: 12,
    marginBottom: 12,
  },
  skeletonCardHeader: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  skeletonCardHeaderText: {
    marginLeft: 12,
    flex: 1,
  },
  skeletonListItem: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 12,
    borderBottomWidth: 1,
  },
  skeletonListItemContent: {
    flex: 1,
    marginLeft: 12,
  },
});
