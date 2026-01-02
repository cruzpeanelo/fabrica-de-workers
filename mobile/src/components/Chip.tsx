/**
 * Chip Component - Issue #426
 * Chips para filtros, tags e selecao
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { X, Check } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type ChipVariant = 'filled' | 'outlined';
type ChipSize = 'sm' | 'md' | 'lg';

interface ChipProps {
  label: string;
  variant?: ChipVariant;
  size?: ChipSize;
  selected?: boolean;
  disabled?: boolean;
  icon?: React.ReactNode;
  onPress?: () => void;
  onDelete?: () => void;
  color?: string;
  style?: ViewStyle;
}

export function Chip({
  label,
  variant = 'filled',
  size = 'md',
  selected = false,
  disabled = false,
  icon,
  onPress,
  onDelete,
  color,
  style,
}: ChipProps) {
  const { colors } = useTheme();

  const chipColor = color || colors.primary;

  const getSizeStyles = () => {
    switch (size) {
      case 'sm':
        return { paddingVertical: 4, paddingHorizontal: 8, fontSize: 12, iconSize: 14 };
      case 'lg':
        return { paddingVertical: 10, paddingHorizontal: 16, fontSize: 15, iconSize: 20 };
      default:
        return { paddingVertical: 6, paddingHorizontal: 12, fontSize: 13, iconSize: 16 };
    }
  };

  const getBackgroundColor = () => {
    if (disabled) return colors.border;
    if (variant === 'outlined') {
      return selected ? chipColor + '15' : 'transparent';
    }
    return selected ? chipColor : colors.border;
  };

  const getTextColor = () => {
    if (disabled) return colors.textSecondary;
    if (variant === 'outlined') {
      return selected ? chipColor : colors.text;
    }
    return selected ? '#FFFFFF' : colors.text;
  };

  const getBorderColor = () => {
    if (variant === 'outlined') {
      return selected ? chipColor : colors.border;
    }
    return 'transparent';
  };

  const sizeStyles = getSizeStyles();

  const Container = onPress ? TouchableOpacity : View;

  return (
    <Container
      onPress={disabled ? undefined : onPress}
      disabled={disabled}
      activeOpacity={0.7}
      style={[
        styles.chip,
        {
          paddingVertical: sizeStyles.paddingVertical,
          paddingHorizontal: sizeStyles.paddingHorizontal,
          backgroundColor: getBackgroundColor(),
          borderColor: getBorderColor(),
          borderWidth: variant === 'outlined' ? 1.5 : 0,
          opacity: disabled ? 0.6 : 1,
        },
        style,
      ]}
    >
      {selected && variant === 'filled' && (
        <Check color={getTextColor()} size={sizeStyles.iconSize} />
      )}
      {icon && !selected && <View style={styles.icon}>{icon}</View>}
      <Text
        style={[
          styles.label,
          {
            fontSize: sizeStyles.fontSize,
            color: getTextColor(),
          },
        ]}
        numberOfLines={1}
      >
        {label}
      </Text>
      {onDelete && (
        <TouchableOpacity
          onPress={onDelete}
          disabled={disabled}
          style={styles.deleteButton}
          hitSlop={{ top: 8, bottom: 8, left: 8, right: 8 }}
        >
          <X color={getTextColor()} size={sizeStyles.iconSize} />
        </TouchableOpacity>
      )}
    </Container>
  );
}

// Chip Group for multiple selection
interface ChipOption {
  value: string;
  label: string;
  icon?: React.ReactNode;
}

interface ChipGroupProps {
  options: ChipOption[];
  value: string | string[];
  onChange: (value: string | string[]) => void;
  multiple?: boolean;
  variant?: ChipVariant;
  size?: ChipSize;
  style?: ViewStyle;
}

export function ChipGroup({
  options,
  value,
  onChange,
  multiple = false,
  variant = 'outlined',
  size = 'md',
  style,
}: ChipGroupProps) {
  const selectedValues = Array.isArray(value) ? value : [value];

  const handlePress = (optionValue: string) => {
    if (multiple) {
      const newValues = selectedValues.includes(optionValue)
        ? selectedValues.filter((v) => v !== optionValue)
        : [...selectedValues, optionValue];
      onChange(newValues);
    } else {
      onChange(optionValue);
    }
  };

  return (
    <View style={[styles.group, style]}>
      {options.map((option) => (
        <Chip
          key={option.value}
          label={option.label}
          icon={option.icon}
          variant={variant}
          size={size}
          selected={selectedValues.includes(option.value)}
          onPress={() => handlePress(option.value)}
        />
      ))}
    </View>
  );
}

// Filter Chip - specialized for filtering
interface FilterChipProps {
  label: string;
  active?: boolean;
  count?: number;
  onPress?: () => void;
  onClear?: () => void;
}

export function FilterChip({
  label,
  active = false,
  count,
  onPress,
  onClear,
}: FilterChipProps) {
  const { colors } = useTheme();

  return (
    <TouchableOpacity
      onPress={onPress}
      style={[
        styles.filterChip,
        {
          backgroundColor: active ? colors.primary + '15' : colors.surface,
          borderColor: active ? colors.primary : colors.border,
        },
      ]}
    >
      <Text
        style={[
          styles.filterLabel,
          { color: active ? colors.primary : colors.text },
        ]}
      >
        {label}
      </Text>
      {count !== undefined && count > 0 && (
        <View
          style={[
            styles.countBadge,
            { backgroundColor: active ? colors.primary : colors.textSecondary },
          ]}
        >
          <Text style={styles.countText}>{count}</Text>
        </View>
      )}
      {active && onClear && (
        <TouchableOpacity onPress={onClear} style={styles.clearButton}>
          <X color={colors.primary} size={14} />
        </TouchableOpacity>
      )}
    </TouchableOpacity>
  );
}

const styles = StyleSheet.create({
  chip: {
    flexDirection: 'row',
    alignItems: 'center',
    borderRadius: 100,
    gap: 4,
  },
  icon: {
    marginRight: 2,
  },
  label: {
    fontWeight: '500',
  },
  deleteButton: {
    marginLeft: 2,
  },
  group: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    gap: 8,
  },
  filterChip: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 8,
    paddingHorizontal: 14,
    borderRadius: 20,
    borderWidth: 1,
    gap: 6,
  },
  filterLabel: {
    fontSize: 14,
    fontWeight: '500',
  },
  countBadge: {
    minWidth: 18,
    height: 18,
    borderRadius: 9,
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 4,
  },
  countText: {
    fontSize: 11,
    fontWeight: '600',
    color: '#FFF',
  },
  clearButton: {
    marginLeft: 2,
  },
});
