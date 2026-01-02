/**
 * FloatingActionButton Component - Issue #426
 * FAB para acoes rapidas com animacoes
 */

import React, { useState, useRef } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Animated,
  ViewStyle,
} from 'react-native';
import { Plus, X } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type FABSize = 'small' | 'regular' | 'large';
type FABPosition = 'bottom-right' | 'bottom-left' | 'bottom-center';

interface FloatingActionButtonProps {
  onPress: () => void;
  icon?: React.ReactNode;
  label?: string; // Extended FAB
  size?: FABSize;
  position?: FABPosition;
  color?: string;
  disabled?: boolean;
  style?: ViewStyle;
}

export function FloatingActionButton({
  onPress,
  icon,
  label,
  size = 'regular',
  position = 'bottom-right',
  color,
  disabled = false,
  style,
}: FloatingActionButtonProps) {
  const { colors } = useTheme();
  const scaleAnim = useRef(new Animated.Value(1)).current;

  const backgroundColor = color || colors.primary;

  const getSizeStyles = () => {
    switch (size) {
      case 'small':
        return { width: 40, height: 40, iconSize: 20 };
      case 'large':
        return { width: 64, height: 64, iconSize: 28 };
      default:
        return { width: 56, height: 56, iconSize: 24 };
    }
  };

  const getPositionStyles = (): ViewStyle => {
    const base = { position: 'absolute' as const, bottom: 24 };
    switch (position) {
      case 'bottom-left':
        return { ...base, left: 24 };
      case 'bottom-center':
        return { ...base, left: '50%', marginLeft: -28 };
      default:
        return { ...base, right: 24 };
    }
  };

  const handlePressIn = () => {
    Animated.spring(scaleAnim, {
      toValue: 0.9,
      useNativeDriver: true,
    }).start();
  };

  const handlePressOut = () => {
    Animated.spring(scaleAnim, {
      toValue: 1,
      friction: 3,
      useNativeDriver: true,
    }).start();
  };

  const sizeStyles = getSizeStyles();
  const positionStyles = getPositionStyles();

  // Extended FAB with label
  if (label) {
    return (
      <Animated.View
        style={[
          positionStyles,
          { transform: [{ scale: scaleAnim }] },
          style,
        ]}
      >
        <TouchableOpacity
          onPress={onPress}
          onPressIn={handlePressIn}
          onPressOut={handlePressOut}
          disabled={disabled}
          activeOpacity={0.8}
          style={[
            styles.extendedFab,
            {
              backgroundColor,
              opacity: disabled ? 0.5 : 1,
            },
          ]}
        >
          {icon || <Plus color="#FFF" size={sizeStyles.iconSize} />}
          <Text style={styles.label}>{label}</Text>
        </TouchableOpacity>
      </Animated.View>
    );
  }

  return (
    <Animated.View
      style={[
        positionStyles,
        { transform: [{ scale: scaleAnim }] },
        style,
      ]}
    >
      <TouchableOpacity
        onPress={onPress}
        onPressIn={handlePressIn}
        onPressOut={handlePressOut}
        disabled={disabled}
        activeOpacity={0.8}
        style={[
          styles.fab,
          {
            width: sizeStyles.width,
            height: sizeStyles.height,
            borderRadius: sizeStyles.width / 2,
            backgroundColor,
            opacity: disabled ? 0.5 : 1,
          },
        ]}
      >
        {icon || <Plus color="#FFF" size={sizeStyles.iconSize} />}
      </TouchableOpacity>
    </Animated.View>
  );
}

// FAB with expandable actions
interface FABAction {
  icon: React.ReactNode;
  label?: string;
  onPress: () => void;
}

interface ExpandableFABProps {
  actions: FABAction[];
  mainIcon?: React.ReactNode;
  position?: FABPosition;
  color?: string;
  style?: ViewStyle;
}

export function ExpandableFAB({
  actions,
  mainIcon,
  position = 'bottom-right',
  color,
  style,
}: ExpandableFABProps) {
  const { colors } = useTheme();
  const [isOpen, setIsOpen] = useState(false);
  const rotateAnim = useRef(new Animated.Value(0)).current;
  const actionsAnim = useRef(new Animated.Value(0)).current;

  const backgroundColor = color || colors.primary;

  const toggleOpen = () => {
    const toValue = isOpen ? 0 : 1;

    Animated.parallel([
      Animated.spring(rotateAnim, {
        toValue,
        useNativeDriver: true,
      }),
      Animated.spring(actionsAnim, {
        toValue,
        useNativeDriver: true,
      }),
    ]).start();

    setIsOpen(!isOpen);
  };

  const getPositionStyles = (): ViewStyle => {
    const base = { position: 'absolute' as const, bottom: 24 };
    switch (position) {
      case 'bottom-left':
        return { ...base, left: 24 };
      case 'bottom-center':
        return { ...base, alignSelf: 'center' };
      default:
        return { ...base, right: 24 };
    }
  };

  const rotation = rotateAnim.interpolate({
    inputRange: [0, 1],
    outputRange: ['0deg', '45deg'],
  });

  return (
    <View style={[getPositionStyles(), style]}>
      {/* Actions */}
      {actions.map((action, index) => {
        const translateY = actionsAnim.interpolate({
          inputRange: [0, 1],
          outputRange: [0, -(60 * (index + 1))],
        });

        const opacity = actionsAnim.interpolate({
          inputRange: [0, 0.5, 1],
          outputRange: [0, 0, 1],
        });

        return (
          <Animated.View
            key={index}
            style={[
              styles.actionContainer,
              {
                transform: [{ translateY }],
                opacity,
              },
            ]}
          >
            {action.label && (
              <View style={[styles.actionLabel, { backgroundColor: colors.surface }]}>
                <Text style={[styles.actionLabelText, { color: colors.text }]}>
                  {action.label}
                </Text>
              </View>
            )}
            <TouchableOpacity
              onPress={() => {
                action.onPress();
                toggleOpen();
              }}
              style={[
                styles.actionButton,
                { backgroundColor: colors.surface },
              ]}
            >
              {action.icon}
            </TouchableOpacity>
          </Animated.View>
        );
      })}

      {/* Main FAB */}
      <Animated.View style={{ transform: [{ rotate: rotation }] }}>
        <TouchableOpacity
          onPress={toggleOpen}
          style={[styles.fab, { backgroundColor }]}
        >
          {mainIcon || (isOpen ? <X color="#FFF" size={24} /> : <Plus color="#FFF" size={24} />)}
        </TouchableOpacity>
      </Animated.View>

      {/* Backdrop */}
      {isOpen && (
        <TouchableOpacity
          style={styles.backdrop}
          onPress={toggleOpen}
          activeOpacity={1}
        />
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  fab: {
    width: 56,
    height: 56,
    borderRadius: 28,
    justifyContent: 'center',
    alignItems: 'center',
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.3,
    shadowRadius: 4,
    elevation: 6,
  },
  extendedFab: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingHorizontal: 20,
    paddingVertical: 16,
    borderRadius: 28,
    gap: 8,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.3,
    shadowRadius: 4,
    elevation: 6,
  },
  label: {
    color: '#FFF',
    fontSize: 14,
    fontWeight: '600',
  },
  actionContainer: {
    position: 'absolute',
    bottom: 0,
    right: 0,
    flexDirection: 'row',
    alignItems: 'center',
    gap: 12,
  },
  actionButton: {
    width: 48,
    height: 48,
    borderRadius: 24,
    justifyContent: 'center',
    alignItems: 'center',
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.2,
    shadowRadius: 3,
    elevation: 4,
  },
  actionLabel: {
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 4,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 1 },
    shadowOpacity: 0.1,
    shadowRadius: 2,
    elevation: 2,
  },
  actionLabelText: {
    fontSize: 13,
    fontWeight: '500',
  },
  backdrop: {
    position: 'absolute',
    top: -1000,
    left: -1000,
    right: -1000,
    bottom: -100,
    zIndex: -1,
  },
});
