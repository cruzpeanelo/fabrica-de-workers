/**
 * Radio Component - Issue #427
 * RadioButton e RadioGroup para selecao unica
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

interface RadioButtonProps {
  selected?: boolean;
  onPress?: () => void;
  label?: string;
  description?: string;
  disabled?: boolean;
  size?: 'sm' | 'md' | 'lg';
  color?: string;
  style?: ViewStyle;
}

export function RadioButton({
  selected = false,
  onPress,
  label,
  description,
  disabled = false,
  size = 'md',
  color,
  style,
}: RadioButtonProps) {
  const { colors } = useTheme();
  const radioColor = color || colors.primary;

  const getSizeStyles = () => {
    switch (size) {
      case 'sm':
        return { outer: 18, inner: 8, fontSize: 13 };
      case 'lg':
        return { outer: 28, inner: 14, fontSize: 16 };
      default:
        return { outer: 22, inner: 10, fontSize: 15 };
    }
  };

  const sizeStyles = getSizeStyles();

  return (
    <TouchableOpacity
      style={[styles.container, style]}
      onPress={onPress}
      disabled={disabled}
      activeOpacity={0.7}
    >
      <View
        style={[
          styles.outer,
          {
            width: sizeStyles.outer,
            height: sizeStyles.outer,
            borderRadius: sizeStyles.outer / 2,
            borderColor: selected ? radioColor : colors.border,
          },
          disabled && styles.disabled,
        ]}
      >
        {selected && (
          <View
            style={[
              styles.inner,
              {
                width: sizeStyles.inner,
                height: sizeStyles.inner,
                borderRadius: sizeStyles.inner / 2,
                backgroundColor: radioColor,
              },
            ]}
          />
        )}
      </View>

      {(label || description) && (
        <View style={styles.textContainer}>
          {label && (
            <Text
              style={[
                styles.label,
                { fontSize: sizeStyles.fontSize, color: colors.text },
                disabled && { color: colors.textSecondary },
              ]}
            >
              {label}
            </Text>
          )}
          {description && (
            <Text
              style={[
                styles.description,
                { color: colors.textSecondary },
              ]}
            >
              {description}
            </Text>
          )}
        </View>
      )}
    </TouchableOpacity>
  );
}

// Radio Group
interface RadioOption {
  value: string;
  label: string;
  description?: string;
  disabled?: boolean;
}

interface RadioGroupProps {
  options: RadioOption[];
  value?: string;
  onChange: (value: string) => void;
  label?: string;
  orientation?: 'vertical' | 'horizontal';
  size?: 'sm' | 'md' | 'lg';
  color?: string;
  style?: ViewStyle;
}

export function RadioGroup({
  options,
  value,
  onChange,
  label,
  orientation = 'vertical',
  size = 'md',
  color,
  style,
}: RadioGroupProps) {
  const { colors } = useTheme();

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.groupLabel, { color: colors.text }]}>{label}</Text>
      )}
      <View
        style={[
          styles.group,
          orientation === 'horizontal' && styles.groupHorizontal,
        ]}
      >
        {options.map((option) => (
          <RadioButton
            key={option.value}
            selected={value === option.value}
            onPress={() => !option.disabled && onChange(option.value)}
            label={option.label}
            description={option.description}
            disabled={option.disabled}
            size={size}
            color={color}
          />
        ))}
      </View>
    </View>
  );
}

// Radio Card - styled as selectable cards
interface RadioCardProps {
  selected?: boolean;
  onPress?: () => void;
  title: string;
  description?: string;
  icon?: React.ReactNode;
  disabled?: boolean;
  style?: ViewStyle;
}

export function RadioCard({
  selected = false,
  onPress,
  title,
  description,
  icon,
  disabled = false,
  style,
}: RadioCardProps) {
  const { colors } = useTheme();

  return (
    <TouchableOpacity
      style={[
        styles.card,
        {
          backgroundColor: colors.surface,
          borderColor: selected ? colors.primary : colors.border,
          borderWidth: selected ? 2 : 1,
        },
        disabled && styles.disabled,
        style,
      ]}
      onPress={onPress}
      disabled={disabled}
      activeOpacity={0.7}
    >
      {icon && <View style={styles.cardIcon}>{icon}</View>}
      <View style={styles.cardContent}>
        <Text
          style={[
            styles.cardTitle,
            { color: colors.text },
            selected && { color: colors.primary },
          ]}
        >
          {title}
        </Text>
        {description && (
          <Text style={[styles.cardDescription, { color: colors.textSecondary }]}>
            {description}
          </Text>
        )}
      </View>
      <View
        style={[
          styles.cardRadio,
          {
            borderColor: selected ? colors.primary : colors.border,
          },
        ]}
      >
        {selected && (
          <View
            style={[styles.cardRadioInner, { backgroundColor: colors.primary }]}
          />
        )}
      </View>
    </TouchableOpacity>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'flex-start',
    gap: 12,
  },
  outer: {
    borderWidth: 2,
    justifyContent: 'center',
    alignItems: 'center',
  },
  inner: {},
  disabled: {
    opacity: 0.5,
  },
  textContainer: {
    flex: 1,
    paddingTop: 1,
  },
  label: {
    fontWeight: '500',
  },
  description: {
    fontSize: 13,
    marginTop: 2,
  },
  groupLabel: {
    fontSize: 14,
    fontWeight: '600',
    marginBottom: 12,
  },
  group: {
    gap: 12,
  },
  groupHorizontal: {
    flexDirection: 'row',
    flexWrap: 'wrap',
  },
  card: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 16,
    borderRadius: 12,
    gap: 12,
  },
  cardIcon: {
    width: 40,
    height: 40,
    borderRadius: 8,
    justifyContent: 'center',
    alignItems: 'center',
  },
  cardContent: {
    flex: 1,
  },
  cardTitle: {
    fontSize: 15,
    fontWeight: '600',
  },
  cardDescription: {
    fontSize: 13,
    marginTop: 2,
  },
  cardRadio: {
    width: 22,
    height: 22,
    borderRadius: 11,
    borderWidth: 2,
    justifyContent: 'center',
    alignItems: 'center',
  },
  cardRadioInner: {
    width: 10,
    height: 10,
    borderRadius: 5,
  },
});
