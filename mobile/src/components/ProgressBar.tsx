/**
 * ProgressBar Component - Issue #426
 * Barra de progresso com animacao
 */

import React, { useEffect, useRef } from 'react';
import {
  View,
  Text,
  StyleSheet,
  Animated,
  ViewStyle,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

type ProgressVariant = 'default' | 'success' | 'warning' | 'error';
type ProgressSize = 'sm' | 'md' | 'lg';

interface ProgressBarProps {
  progress: number; // 0-100
  variant?: ProgressVariant;
  size?: ProgressSize;
  showLabel?: boolean;
  labelPosition?: 'top' | 'right' | 'inside';
  animated?: boolean;
  color?: string;
  backgroundColor?: string;
  style?: ViewStyle;
}

export function ProgressBar({
  progress,
  variant = 'default',
  size = 'md',
  showLabel = false,
  labelPosition = 'right',
  animated = true,
  color,
  backgroundColor,
  style,
}: ProgressBarProps) {
  const { colors } = useTheme();
  const animatedWidth = useRef(new Animated.Value(0)).current;

  const clampedProgress = Math.min(100, Math.max(0, progress));

  useEffect(() => {
    if (animated) {
      Animated.timing(animatedWidth, {
        toValue: clampedProgress,
        duration: 500,
        useNativeDriver: false,
      }).start();
    } else {
      animatedWidth.setValue(clampedProgress);
    }
  }, [clampedProgress, animated]);

  const getColor = () => {
    if (color) return color;
    switch (variant) {
      case 'success':
        return colors.success;
      case 'warning':
        return colors.warning;
      case 'error':
        return colors.error;
      default:
        return colors.primary;
    }
  };

  const getHeight = () => {
    switch (size) {
      case 'sm':
        return 4;
      case 'lg':
        return 12;
      default:
        return 8;
    }
  };

  const height = getHeight();
  const progressColor = getColor();
  const bgColor = backgroundColor || colors.border;

  const widthInterpolation = animatedWidth.interpolate({
    inputRange: [0, 100],
    outputRange: ['0%', '100%'],
  });

  const renderLabel = () => {
    if (!showLabel) return null;
    return (
      <Text style={[styles.label, { color: colors.textSecondary }]}>
        {Math.round(clampedProgress)}%
      </Text>
    );
  };

  if (labelPosition === 'top') {
    return (
      <View style={style}>
        <View style={styles.topLabelContainer}>
          {renderLabel()}
        </View>
        <View style={[styles.track, { height, backgroundColor: bgColor }]}>
          <Animated.View
            style={[
              styles.fill,
              {
                height,
                backgroundColor: progressColor,
                width: widthInterpolation,
              },
            ]}
          />
        </View>
      </View>
    );
  }

  if (labelPosition === 'inside' && size === 'lg') {
    return (
      <View style={[styles.track, { height, backgroundColor: bgColor }, style]}>
        <Animated.View
          style={[
            styles.fill,
            {
              height,
              backgroundColor: progressColor,
              width: widthInterpolation,
            },
          ]}
        />
        {showLabel && (
          <Text style={[styles.insideLabel, { lineHeight: height }]}>
            {Math.round(clampedProgress)}%
          </Text>
        )}
      </View>
    );
  }

  return (
    <View style={[styles.container, style]}>
      <View style={[styles.track, { height, backgroundColor: bgColor, flex: 1 }]}>
        <Animated.View
          style={[
            styles.fill,
            {
              height,
              backgroundColor: progressColor,
              width: widthInterpolation,
            },
          ]}
        />
      </View>
      {labelPosition === 'right' && renderLabel()}
    </View>
  );
}

// Circular Progress
interface CircularProgressProps {
  progress: number;
  size?: number;
  strokeWidth?: number;
  color?: string;
  showLabel?: boolean;
  style?: ViewStyle;
}

export function CircularProgress({
  progress,
  size = 48,
  strokeWidth = 4,
  color,
  showLabel = true,
  style,
}: CircularProgressProps) {
  const { colors } = useTheme();
  const clampedProgress = Math.min(100, Math.max(0, progress));
  const progressColor = color || colors.primary;

  // Simple circular representation using View
  const radius = (size - strokeWidth) / 2;
  const circumference = 2 * Math.PI * radius;
  const strokeDashoffset = circumference - (clampedProgress / 100) * circumference;

  return (
    <View style={[{ width: size, height: size }, style]}>
      <View
        style={[
          styles.circularTrack,
          {
            width: size,
            height: size,
            borderRadius: size / 2,
            borderWidth: strokeWidth,
            borderColor: colors.border,
          },
        ]}
      />
      <View
        style={[
          styles.circularFill,
          {
            width: size,
            height: size,
            borderRadius: size / 2,
            borderWidth: strokeWidth,
            borderColor: progressColor,
            borderTopColor: clampedProgress > 25 ? progressColor : 'transparent',
            borderRightColor: clampedProgress > 50 ? progressColor : 'transparent',
            borderBottomColor: clampedProgress > 75 ? progressColor : 'transparent',
            transform: [{ rotate: '-90deg' }],
          },
        ]}
      />
      {showLabel && (
        <View style={styles.circularLabel}>
          <Text style={[styles.circularLabelText, { color: colors.text }]}>
            {Math.round(clampedProgress)}%
          </Text>
        </View>
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  track: {
    borderRadius: 100,
    overflow: 'hidden',
  },
  fill: {
    borderRadius: 100,
  },
  label: {
    fontSize: 12,
    fontWeight: '500',
    minWidth: 36,
    textAlign: 'right',
  },
  topLabelContainer: {
    marginBottom: 4,
  },
  insideLabel: {
    position: 'absolute',
    left: 0,
    right: 0,
    textAlign: 'center',
    fontSize: 10,
    fontWeight: '600',
    color: '#FFF',
  },
  circularTrack: {
    position: 'absolute',
  },
  circularFill: {
    position: 'absolute',
  },
  circularLabel: {
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    justifyContent: 'center',
    alignItems: 'center',
  },
  circularLabelText: {
    fontSize: 12,
    fontWeight: '600',
  },
});
