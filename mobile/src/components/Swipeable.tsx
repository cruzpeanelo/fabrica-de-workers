/**
 * Swipeable Component - Issue #428
 * Componente com acoes em swipe
 */

import React, { useRef, useCallback } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Animated,
  PanResponder,
  ViewStyle,
  Dimensions,
} from 'react-native';
import { Trash2, Edit3, Archive, Star, MoreHorizontal } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

const { width: SCREEN_WIDTH } = Dimensions.get('window');
const ACTION_WIDTH = 80;

interface SwipeAction {
  key: string;
  icon: React.ReactNode;
  label?: string;
  backgroundColor: string;
  onPress: () => void;
}

interface SwipeableProps {
  children: React.ReactNode;
  leftActions?: SwipeAction[];
  rightActions?: SwipeAction[];
  onSwipeLeft?: () => void;
  onSwipeRight?: () => void;
  threshold?: number;
  disabled?: boolean;
  style?: ViewStyle;
}

export function Swipeable({
  children,
  leftActions = [],
  rightActions = [],
  onSwipeLeft,
  onSwipeRight,
  threshold = 0.3,
  disabled = false,
  style,
}: SwipeableProps) {
  const { colors } = useTheme();
  const translateX = useRef(new Animated.Value(0)).current;
  const lastOffset = useRef(0);

  const leftActionsWidth = leftActions.length * ACTION_WIDTH;
  const rightActionsWidth = rightActions.length * ACTION_WIDTH;

  const resetPosition = useCallback(() => {
    Animated.spring(translateX, {
      toValue: 0,
      useNativeDriver: true,
    }).start();
    lastOffset.current = 0;
  }, [translateX]);

  const panResponder = useRef(
    PanResponder.create({
      onStartShouldSetPanResponder: () => false,
      onMoveShouldSetPanResponder: (_, gestureState) => {
        return !disabled && Math.abs(gestureState.dx) > 10 && Math.abs(gestureState.dy) < 10;
      },
      onPanResponderGrant: () => {
        translateX.setOffset(lastOffset.current);
        translateX.setValue(0);
      },
      onPanResponderMove: (_, gestureState) => {
        let newX = gestureState.dx;

        // Limit swipe distance
        if (newX > 0 && leftActions.length === 0) {
          newX = 0;
        } else if (newX > leftActionsWidth) {
          newX = leftActionsWidth + (newX - leftActionsWidth) * 0.2;
        }

        if (newX < 0 && rightActions.length === 0) {
          newX = 0;
        } else if (newX < -rightActionsWidth) {
          newX = -rightActionsWidth + (newX + rightActionsWidth) * 0.2;
        }

        translateX.setValue(newX);
      },
      onPanResponderRelease: (_, gestureState) => {
        translateX.flattenOffset();
        const { dx, vx } = gestureState;

        // Full swipe actions
        if (dx > SCREEN_WIDTH * threshold && onSwipeRight) {
          Animated.timing(translateX, {
            toValue: SCREEN_WIDTH,
            duration: 200,
            useNativeDriver: true,
          }).start(() => {
            onSwipeRight();
            resetPosition();
          });
          return;
        }

        if (dx < -SCREEN_WIDTH * threshold && onSwipeLeft) {
          Animated.timing(translateX, {
            toValue: -SCREEN_WIDTH,
            duration: 200,
            useNativeDriver: true,
          }).start(() => {
            onSwipeLeft();
            resetPosition();
          });
          return;
        }

        // Show actions
        if (dx > leftActionsWidth * 0.5 || (vx > 0.5 && leftActions.length > 0)) {
          Animated.spring(translateX, {
            toValue: leftActionsWidth,
            useNativeDriver: true,
          }).start();
          lastOffset.current = leftActionsWidth;
        } else if (dx < -rightActionsWidth * 0.5 || (vx < -0.5 && rightActions.length > 0)) {
          Animated.spring(translateX, {
            toValue: -rightActionsWidth,
            useNativeDriver: true,
          }).start();
          lastOffset.current = -rightActionsWidth;
        } else {
          resetPosition();
        }
      },
    })
  ).current;

  const handleActionPress = (action: SwipeAction) => {
    action.onPress();
    resetPosition();
  };

  const renderActions = (actions: SwipeAction[], side: 'left' | 'right') => {
    if (actions.length === 0) return null;

    return (
      <View
        style={[
          styles.actionsContainer,
          side === 'left' ? styles.leftActions : styles.rightActions,
        ]}
      >
        {actions.map((action, index) => (
          <TouchableOpacity
            key={action.key}
            style={[
              styles.action,
              { backgroundColor: action.backgroundColor, width: ACTION_WIDTH },
            ]}
            onPress={() => handleActionPress(action)}
          >
            {action.icon}
            {action.label && (
              <Text style={styles.actionLabel}>{action.label}</Text>
            )}
          </TouchableOpacity>
        ))}
      </View>
    );
  };

  return (
    <View style={[styles.container, style]}>
      {renderActions(leftActions, 'left')}
      {renderActions(rightActions, 'right')}

      <Animated.View
        style={[
          styles.content,
          { backgroundColor: colors.surface },
          { transform: [{ translateX }] },
        ]}
        {...panResponder.panHandlers}
      >
        {children}
      </Animated.View>
    </View>
  );
}

// Pre-configured swipeable for common use cases
interface SwipeableListItemProps {
  children: React.ReactNode;
  onEdit?: () => void;
  onDelete?: () => void;
  onArchive?: () => void;
  onFavorite?: () => void;
  style?: ViewStyle;
}

export function SwipeableListItem({
  children,
  onEdit,
  onDelete,
  onArchive,
  onFavorite,
  style,
}: SwipeableListItemProps) {
  const { colors } = useTheme();

  const leftActions: SwipeAction[] = [];
  const rightActions: SwipeAction[] = [];

  if (onFavorite) {
    leftActions.push({
      key: 'favorite',
      icon: <Star color="#FFF" size={24} />,
      label: 'Favorito',
      backgroundColor: '#FBBF24',
      onPress: onFavorite,
    });
  }

  if (onArchive) {
    leftActions.push({
      key: 'archive',
      icon: <Archive color="#FFF" size={24} />,
      label: 'Arquivar',
      backgroundColor: colors.info,
      onPress: onArchive,
    });
  }

  if (onEdit) {
    rightActions.push({
      key: 'edit',
      icon: <Edit3 color="#FFF" size={24} />,
      label: 'Editar',
      backgroundColor: colors.primary,
      onPress: onEdit,
    });
  }

  if (onDelete) {
    rightActions.push({
      key: 'delete',
      icon: <Trash2 color="#FFF" size={24} />,
      label: 'Excluir',
      backgroundColor: colors.error,
      onPress: onDelete,
    });
  }

  return (
    <Swipeable
      leftActions={leftActions}
      rightActions={rightActions}
      style={style}
    >
      {children}
    </Swipeable>
  );
}

// Swipe to Delete
interface SwipeToDeleteProps {
  children: React.ReactNode;
  onDelete: () => void;
  confirmDelete?: boolean;
  style?: ViewStyle;
}

export function SwipeToDelete({
  children,
  onDelete,
  confirmDelete = false,
  style,
}: SwipeToDeleteProps) {
  const { colors } = useTheme();

  const handleDelete = () => {
    if (confirmDelete) {
      // Would trigger a confirm dialog
      onDelete();
    } else {
      onDelete();
    }
  };

  return (
    <Swipeable
      rightActions={[
        {
          key: 'delete',
          icon: <Trash2 color="#FFF" size={24} />,
          label: 'Excluir',
          backgroundColor: colors.error,
          onPress: handleDelete,
        },
      ]}
      onSwipeLeft={handleDelete}
      style={style}
    >
      {children}
    </Swipeable>
  );
}

const styles = StyleSheet.create({
  container: {
    overflow: 'hidden',
  },
  content: {
    zIndex: 1,
  },
  actionsContainer: {
    position: 'absolute',
    top: 0,
    bottom: 0,
    flexDirection: 'row',
  },
  leftActions: {
    left: 0,
  },
  rightActions: {
    right: 0,
  },
  action: {
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 16,
  },
  actionLabel: {
    color: '#FFF',
    fontSize: 11,
    marginTop: 4,
    fontWeight: '500',
  },
});
