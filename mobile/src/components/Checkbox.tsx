/**
 * Checkbox Component - Issue #427
 * Checkbox com estados e grupos
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { Check, Minus } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type CheckboxState = 'checked' | 'unchecked' | 'indeterminate';

interface CheckboxProps {
  checked?: boolean;
  indeterminate?: boolean;
  onChange?: (checked: boolean) => void;
  label?: string;
  description?: string;
  disabled?: boolean;
  size?: 'sm' | 'md' | 'lg';
  color?: string;
  style?: ViewStyle;
}

export function Checkbox({
  checked = false,
  indeterminate = false,
  onChange,
  label,
  description,
  disabled = false,
  size = 'md',
  color,
  style,
}: CheckboxProps) {
  const { colors } = useTheme();
  const checkColor = color || colors.primary;

  const getState = (): CheckboxState => {
    if (indeterminate) return 'indeterminate';
    return checked ? 'checked' : 'unchecked';
  };

  const getSizeStyles = () => {
    switch (size) {
      case 'sm':
        return { box: 18, icon: 12, fontSize: 13 };
      case 'lg':
        return { box: 28, icon: 18, fontSize: 16 };
      default:
        return { box: 22, icon: 14, fontSize: 15 };
    }
  };

  const state = getState();
  const sizeStyles = getSizeStyles();
  const isActive = state !== 'unchecked';

  const handlePress = () => {
    if (!disabled && onChange) {
      onChange(!checked);
    }
  };

  return (
    <TouchableOpacity
      style={[styles.container, style]}
      onPress={handlePress}
      disabled={disabled}
      activeOpacity={0.7}
    >
      <View
        style={[
          styles.box,
          {
            width: sizeStyles.box,
            height: sizeStyles.box,
            borderRadius: sizeStyles.box * 0.2,
            borderColor: isActive ? checkColor : colors.border,
            backgroundColor: isActive ? checkColor : 'transparent',
          },
          disabled && styles.disabled,
        ]}
      >
        {state === 'checked' && (
          <Check color="#FFF" size={sizeStyles.icon} strokeWidth={3} />
        )}
        {state === 'indeterminate' && (
          <Minus color="#FFF" size={sizeStyles.icon} strokeWidth={3} />
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

// Checkbox Group
interface CheckboxOption {
  value: string;
  label: string;
  description?: string;
  disabled?: boolean;
}

interface CheckboxGroupProps {
  options: CheckboxOption[];
  value: string[];
  onChange: (value: string[]) => void;
  label?: string;
  orientation?: 'vertical' | 'horizontal';
  size?: 'sm' | 'md' | 'lg';
  style?: ViewStyle;
}

export function CheckboxGroup({
  options,
  value,
  onChange,
  label,
  orientation = 'vertical',
  size = 'md',
  style,
}: CheckboxGroupProps) {
  const { colors } = useTheme();

  const handleChange = (optionValue: string, checked: boolean) => {
    if (checked) {
      onChange([...value, optionValue]);
    } else {
      onChange(value.filter(v => v !== optionValue));
    }
  };

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
          <Checkbox
            key={option.value}
            checked={value.includes(option.value)}
            onChange={(checked) => handleChange(option.value, checked)}
            label={option.label}
            description={option.description}
            disabled={option.disabled}
            size={size}
          />
        ))}
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'flex-start',
    gap: 12,
  },
  box: {
    borderWidth: 2,
    justifyContent: 'center',
    alignItems: 'center',
  },
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
});
