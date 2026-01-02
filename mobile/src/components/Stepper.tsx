/**
 * Stepper Component - Issue #428
 * Input numerico com botoes +/-
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { Plus, Minus } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type StepperSize = 'sm' | 'md' | 'lg';
type StepperVariant = 'default' | 'outlined' | 'filled';

interface StepperProps {
  value: number;
  onValueChange: (value: number) => void;
  min?: number;
  max?: number;
  step?: number;
  size?: StepperSize;
  variant?: StepperVariant;
  label?: string;
  formatValue?: (value: number) => string;
  disabled?: boolean;
  color?: string;
  style?: ViewStyle;
}

export function Stepper({
  value,
  onValueChange,
  min = 0,
  max = 100,
  step = 1,
  size = 'md',
  variant = 'default',
  label,
  formatValue,
  disabled = false,
  color,
  style,
}: StepperProps) {
  const { colors } = useTheme();
  const stepperColor = color || colors.primary;

  const canDecrement = value > min;
  const canIncrement = value < max;

  const handleDecrement = () => {
    if (canDecrement && !disabled) {
      onValueChange(Math.max(min, value - step));
    }
  };

  const handleIncrement = () => {
    if (canIncrement && !disabled) {
      onValueChange(Math.min(max, value + step));
    }
  };

  const getSizeStyles = () => {
    switch (size) {
      case 'sm':
        return { button: 32, icon: 16, fontSize: 14, padding: 8 };
      case 'lg':
        return { button: 48, icon: 24, fontSize: 20, padding: 16 };
      default:
        return { button: 40, icon: 20, fontSize: 16, padding: 12 };
    }
  };

  const sizeStyles = getSizeStyles();
  const displayValue = formatValue ? formatValue(value) : value.toString();

  const getButtonStyle = (isActive: boolean): ViewStyle => {
    const baseStyle: ViewStyle = {
      width: sizeStyles.button,
      height: sizeStyles.button,
      borderRadius: sizeStyles.button / 2,
      justifyContent: 'center',
      alignItems: 'center',
    };

    if (!isActive || disabled) {
      return {
        ...baseStyle,
        backgroundColor: colors.border,
        opacity: disabled ? 0.5 : 0.6,
      };
    }

    switch (variant) {
      case 'filled':
        return { ...baseStyle, backgroundColor: stepperColor };
      case 'outlined':
        return {
          ...baseStyle,
          backgroundColor: 'transparent',
          borderWidth: 2,
          borderColor: stepperColor,
        };
      default:
        return { ...baseStyle, backgroundColor: stepperColor + '15' };
    }
  };

  const getIconColor = (isActive: boolean) => {
    if (!isActive || disabled) return colors.textSecondary;
    if (variant === 'filled') return '#FFFFFF';
    return stepperColor;
  };

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}
      <View style={styles.container}>
        <TouchableOpacity
          style={getButtonStyle(canDecrement)}
          onPress={handleDecrement}
          disabled={!canDecrement || disabled}
          activeOpacity={0.7}
        >
          <Minus color={getIconColor(canDecrement)} size={sizeStyles.icon} />
        </TouchableOpacity>

        <View style={[styles.valueContainer, { minWidth: sizeStyles.button * 1.5 }]}>
          <Text
            style={[
              styles.value,
              {
                fontSize: sizeStyles.fontSize,
                color: disabled ? colors.textSecondary : colors.text,
              },
            ]}
          >
            {displayValue}
          </Text>
        </View>

        <TouchableOpacity
          style={getButtonStyle(canIncrement)}
          onPress={handleIncrement}
          disabled={!canIncrement || disabled}
          activeOpacity={0.7}
        >
          <Plus color={getIconColor(canIncrement)} size={sizeStyles.icon} />
        </TouchableOpacity>
      </View>
    </View>
  );
}

// Quantity Stepper - for e-commerce style
interface QuantityStepperProps {
  value: number;
  onValueChange: (value: number) => void;
  min?: number;
  max?: number;
  label?: string;
  unit?: string;
  disabled?: boolean;
  style?: ViewStyle;
}

export function QuantityStepper({
  value,
  onValueChange,
  min = 1,
  max = 99,
  label,
  unit,
  disabled = false,
  style,
}: QuantityStepperProps) {
  const { colors } = useTheme();

  const canDecrement = value > min;
  const canIncrement = value < max;

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}
      <View
        style={[
          styles.quantityContainer,
          {
            backgroundColor: colors.surface,
            borderColor: colors.border,
          },
        ]}
      >
        <TouchableOpacity
          style={styles.quantityButton}
          onPress={() => canDecrement && !disabled && onValueChange(value - 1)}
          disabled={!canDecrement || disabled}
        >
          <Minus
            color={canDecrement && !disabled ? colors.text : colors.textSecondary}
            size={18}
          />
        </TouchableOpacity>

        <View style={[styles.quantityValue, { borderColor: colors.border }]}>
          <Text
            style={[
              styles.quantityText,
              { color: disabled ? colors.textSecondary : colors.text },
            ]}
          >
            {value}
            {unit && <Text style={styles.unit}> {unit}</Text>}
          </Text>
        </View>

        <TouchableOpacity
          style={styles.quantityButton}
          onPress={() => canIncrement && !disabled && onValueChange(value + 1)}
          disabled={!canIncrement || disabled}
        >
          <Plus
            color={canIncrement && !disabled ? colors.text : colors.textSecondary}
            size={18}
          />
        </TouchableOpacity>
      </View>
    </View>
  );
}

// Inline Stepper - compact inline version
interface InlineStepperProps {
  value: number;
  onValueChange: (value: number) => void;
  min?: number;
  max?: number;
  disabled?: boolean;
}

export function InlineStepper({
  value,
  onValueChange,
  min = 0,
  max = 100,
  disabled = false,
}: InlineStepperProps) {
  const { colors } = useTheme();

  return (
    <View style={styles.inlineContainer}>
      <TouchableOpacity
        onPress={() => value > min && !disabled && onValueChange(value - 1)}
        disabled={value <= min || disabled}
        style={styles.inlineButton}
      >
        <Minus
          color={value > min && !disabled ? colors.primary : colors.textSecondary}
          size={16}
        />
      </TouchableOpacity>

      <Text
        style={[
          styles.inlineValue,
          { color: disabled ? colors.textSecondary : colors.text },
        ]}
      >
        {value}
      </Text>

      <TouchableOpacity
        onPress={() => value < max && !disabled && onValueChange(value + 1)}
        disabled={value >= max || disabled}
        style={styles.inlineButton}
      >
        <Plus
          color={value < max && !disabled ? colors.primary : colors.textSecondary}
          size={16}
        />
      </TouchableOpacity>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  label: {
    fontSize: 14,
    fontWeight: '500',
    marginBottom: 8,
  },
  valueContainer: {
    alignItems: 'center',
    justifyContent: 'center',
  },
  value: {
    fontWeight: '600',
    textAlign: 'center',
  },
  quantityContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    borderWidth: 1,
    borderRadius: 8,
    overflow: 'hidden',
  },
  quantityButton: {
    padding: 12,
  },
  quantityValue: {
    paddingHorizontal: 16,
    paddingVertical: 8,
    borderLeftWidth: 1,
    borderRightWidth: 1,
    minWidth: 60,
    alignItems: 'center',
  },
  quantityText: {
    fontSize: 16,
    fontWeight: '600',
  },
  unit: {
    fontWeight: '400',
  },
  inlineContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  inlineButton: {
    padding: 4,
  },
  inlineValue: {
    fontSize: 14,
    fontWeight: '600',
    minWidth: 24,
    textAlign: 'center',
  },
});
