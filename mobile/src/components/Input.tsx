/**
 * Input Component - Issue #423
 * Campo de entrada reutilizavel com validacao e icones
 */

import React, { useState, forwardRef } from 'react';
import {
  View,
  TextInput,
  Text,
  StyleSheet,
  TouchableOpacity,
  TextInputProps,
  ViewStyle,
} from 'react-native';
import { Eye, EyeOff, AlertCircle, CheckCircle } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

interface InputProps extends TextInputProps {
  label?: string;
  error?: string;
  hint?: string;
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
  isValid?: boolean;
  containerStyle?: ViewStyle;
}

export const Input = forwardRef<TextInput, InputProps>(
  (
    {
      label,
      error,
      hint,
      leftIcon,
      rightIcon,
      isValid,
      containerStyle,
      secureTextEntry,
      ...props
    },
    ref
  ) => {
    const { colors } = useTheme();
    const [isFocused, setIsFocused] = useState(false);
    const [showPassword, setShowPassword] = useState(false);

    const getBorderColor = () => {
      if (error) return colors.error;
      if (isValid) return colors.success;
      if (isFocused) return colors.primary;
      return colors.border;
    };

    const styles = createStyles(colors);

    return (
      <View style={[styles.container, containerStyle]}>
        {label && <Text style={styles.label}>{label}</Text>}

        <View
          style={[
            styles.inputContainer,
            {
              borderColor: getBorderColor(),
              backgroundColor: props.editable === false ? colors.border + '30' : colors.surface,
            },
          ]}
        >
          {leftIcon && <View style={styles.iconLeft}>{leftIcon}</View>}

          <TextInput
            ref={ref}
            style={[
              styles.input,
              leftIcon && { paddingLeft: 8 },
              (rightIcon || secureTextEntry || isValid || error) && { paddingRight: 8 },
            ]}
            placeholderTextColor={colors.textSecondary}
            onFocus={() => setIsFocused(true)}
            onBlur={() => setIsFocused(false)}
            secureTextEntry={secureTextEntry && !showPassword}
            {...props}
          />

          {secureTextEntry && (
            <TouchableOpacity
              onPress={() => setShowPassword(!showPassword)}
              style={styles.iconRight}
            >
              {showPassword ? (
                <EyeOff color={colors.textSecondary} size={20} />
              ) : (
                <Eye color={colors.textSecondary} size={20} />
              )}
            </TouchableOpacity>
          )}

          {!secureTextEntry && error && (
            <View style={styles.iconRight}>
              <AlertCircle color={colors.error} size={20} />
            </View>
          )}

          {!secureTextEntry && !error && isValid && (
            <View style={styles.iconRight}>
              <CheckCircle color={colors.success} size={20} />
            </View>
          )}

          {!secureTextEntry && !error && !isValid && rightIcon && (
            <View style={styles.iconRight}>{rightIcon}</View>
          )}
        </View>

        {error && <Text style={styles.error}>{error}</Text>}
        {!error && hint && <Text style={styles.hint}>{hint}</Text>}
      </View>
    );
  }
);

Input.displayName = 'Input';

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      marginBottom: 16,
    },
    label: {
      fontSize: 14,
      fontWeight: '500',
      color: colors.text,
      marginBottom: 6,
    },
    inputContainer: {
      flexDirection: 'row',
      alignItems: 'center',
      borderWidth: 1.5,
      borderRadius: 8,
      paddingHorizontal: 12,
      minHeight: 48,
    },
    input: {
      flex: 1,
      fontSize: 15,
      color: colors.text,
      paddingVertical: 12,
    },
    iconLeft: {
      marginRight: 8,
    },
    iconRight: {
      marginLeft: 8,
    },
    error: {
      fontSize: 12,
      color: colors.error,
      marginTop: 4,
    },
    hint: {
      fontSize: 12,
      color: colors.textSecondary,
      marginTop: 4,
    },
  });
