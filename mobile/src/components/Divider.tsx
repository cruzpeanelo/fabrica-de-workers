/**
 * Divider Component - Issue #426
 * Divisor simples horizontal/vertical
 */

import React from 'react';
import { View, Text, StyleSheet, ViewStyle } from 'react-native';
import { useTheme } from '../context/ThemeContext';

type DividerOrientation = 'horizontal' | 'vertical';
type DividerVariant = 'full' | 'inset' | 'middle';

interface DividerProps {
  orientation?: DividerOrientation;
  variant?: DividerVariant;
  label?: string;
  color?: string;
  thickness?: number;
  spacing?: number;
  style?: ViewStyle;
}

export function Divider({
  orientation = 'horizontal',
  variant = 'full',
  label,
  color,
  thickness = 1,
  spacing = 0,
  style,
}: DividerProps) {
  const { colors } = useTheme();
  const dividerColor = color || colors.border;

  const getInset = () => {
    switch (variant) {
      case 'inset':
        return { marginLeft: 16 };
      case 'middle':
        return { marginHorizontal: 16 };
      default:
        return {};
    }
  };

  if (orientation === 'vertical') {
    return (
      <View
        style={[
          styles.vertical,
          {
            width: thickness,
            backgroundColor: dividerColor,
            marginHorizontal: spacing,
          },
          style,
        ]}
      />
    );
  }

  // Horizontal with optional label
  if (label) {
    return (
      <View
        style={[
          styles.labelContainer,
          { marginVertical: spacing },
          style,
        ]}
      >
        <View
          style={[
            styles.line,
            { backgroundColor: dividerColor, height: thickness },
          ]}
        />
        <Text style={[styles.label, { color: colors.textSecondary }]}>
          {label}
        </Text>
        <View
          style={[
            styles.line,
            { backgroundColor: dividerColor, height: thickness },
          ]}
        />
      </View>
    );
  }

  return (
    <View
      style={[
        styles.horizontal,
        getInset(),
        {
          height: thickness,
          backgroundColor: dividerColor,
          marginVertical: spacing,
        },
        style,
      ]}
    />
  );
}

// List Divider - specialized for lists
interface ListDividerProps {
  inset?: number;
  color?: string;
}

export function ListDivider({ inset = 0, color }: ListDividerProps) {
  const { colors } = useTheme();

  return (
    <View
      style={[
        styles.listDivider,
        {
          marginLeft: inset,
          backgroundColor: color || colors.border,
        },
      ]}
    />
  );
}

// Section Divider - with title
interface SectionDividerProps {
  title: string;
  action?: React.ReactNode;
  style?: ViewStyle;
}

export function SectionDivider({ title, action, style }: SectionDividerProps) {
  const { colors } = useTheme();

  return (
    <View style={[styles.sectionDivider, style]}>
      <View style={[styles.sectionLine, { backgroundColor: colors.border }]} />
      <View style={styles.sectionContent}>
        <Text style={[styles.sectionTitle, { color: colors.textSecondary }]}>
          {title}
        </Text>
        {action}
      </View>
      <View style={[styles.sectionLine, { backgroundColor: colors.border }]} />
    </View>
  );
}

// Spacer - for adding vertical/horizontal space
interface SpacerProps {
  size?: number;
  horizontal?: boolean;
}

export function Spacer({ size = 16, horizontal = false }: SpacerProps) {
  return (
    <View
      style={horizontal ? { width: size } : { height: size }}
    />
  );
}

const styles = StyleSheet.create({
  horizontal: {
    width: '100%',
  },
  vertical: {
    alignSelf: 'stretch',
  },
  labelContainer: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  line: {
    flex: 1,
  },
  label: {
    paddingHorizontal: 16,
    fontSize: 13,
    fontWeight: '500',
  },
  listDivider: {
    height: 1,
  },
  sectionDivider: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 16,
  },
  sectionLine: {
    flex: 1,
    height: 1,
  },
  sectionContent: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingHorizontal: 16,
    gap: 8,
  },
  sectionTitle: {
    fontSize: 12,
    fontWeight: '600',
    textTransform: 'uppercase',
    letterSpacing: 0.5,
  },
});
