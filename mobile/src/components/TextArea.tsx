/**
 * TextArea Component - Issue #427
 * Campo de texto multi-linha com auto-resize
 */

import React, { useState, useCallback } from 'react';
import {
  View,
  TextInput,
  Text,
  StyleSheet,
  TextInputProps,
  ViewStyle,
  NativeSyntheticEvent,
  TextInputContentSizeChangeEventData,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

interface TextAreaProps extends Omit<TextInputProps, 'multiline'> {
  label?: string;
  error?: string;
  hint?: string;
  maxLength?: number;
  showCharacterCount?: boolean;
  minHeight?: number;
  maxHeight?: number;
  autoGrow?: boolean;
  containerStyle?: ViewStyle;
}

export function TextArea({
  label,
  error,
  hint,
  maxLength,
  showCharacterCount = false,
  minHeight = 100,
  maxHeight = 300,
  autoGrow = true,
  containerStyle,
  value,
  onChangeText,
  ...props
}: TextAreaProps) {
  const { colors } = useTheme();
  const [isFocused, setIsFocused] = useState(false);
  const [height, setHeight] = useState(minHeight);

  const handleContentSizeChange = useCallback(
    (e: NativeSyntheticEvent<TextInputContentSizeChangeEventData>) => {
      if (autoGrow) {
        const newHeight = Math.max(
          minHeight,
          Math.min(maxHeight, e.nativeEvent.contentSize.height + 24)
        );
        setHeight(newHeight);
      }
    },
    [autoGrow, minHeight, maxHeight]
  );

  const getBorderColor = () => {
    if (error) return colors.error;
    if (isFocused) return colors.primary;
    return colors.border;
  };

  const characterCount = value?.length || 0;

  const styles = createStyles(colors);

  return (
    <View style={containerStyle}>
      {label && <Text style={styles.label}>{label}</Text>}

      <View
        style={[
          styles.inputContainer,
          {
            borderColor: getBorderColor(),
            height: autoGrow ? height : minHeight,
            minHeight,
            maxHeight,
          },
          props.editable === false && styles.disabled,
        ]}
      >
        <TextInput
          style={[styles.input, { minHeight: minHeight - 24 }]}
          value={value}
          onChangeText={onChangeText}
          multiline
          textAlignVertical="top"
          placeholderTextColor={colors.textSecondary}
          onFocus={() => setIsFocused(true)}
          onBlur={() => setIsFocused(false)}
          onContentSizeChange={handleContentSizeChange}
          maxLength={maxLength}
          {...props}
        />
      </View>

      <View style={styles.footer}>
        {error && <Text style={styles.error}>{error}</Text>}
        {!error && hint && <Text style={styles.hint}>{hint}</Text>}
        {!error && !hint && <View />}

        {showCharacterCount && (
          <Text
            style={[
              styles.charCount,
              maxLength && characterCount >= maxLength && { color: colors.error },
            ]}
          >
            {characterCount}
            {maxLength && `/${maxLength}`}
          </Text>
        )}
      </View>
    </View>
  );
}

// Mention TextArea - for @mentions
interface MentionTextAreaProps extends TextAreaProps {
  mentions?: string[];
  onMentionSearch?: (query: string) => void;
  onMentionSelect?: (mention: string) => void;
}

export function MentionTextArea({
  mentions = [],
  onMentionSearch,
  onMentionSelect,
  value,
  onChangeText,
  ...props
}: MentionTextAreaProps) {
  const { colors } = useTheme();
  const [showMentions, setShowMentions] = useState(false);

  const handleChange = (text: string) => {
    onChangeText?.(text);

    // Check for @ trigger
    const lastAtIndex = text.lastIndexOf('@');
    if (lastAtIndex !== -1) {
      const query = text.slice(lastAtIndex + 1);
      if (query.length > 0 && !query.includes(' ')) {
        onMentionSearch?.(query);
        setShowMentions(true);
      } else {
        setShowMentions(false);
      }
    } else {
      setShowMentions(false);
    }
  };

  const handleMentionSelect = (mention: string) => {
    if (value) {
      const lastAtIndex = value.lastIndexOf('@');
      const newValue = value.slice(0, lastAtIndex) + `@${mention} `;
      onChangeText?.(newValue);
    }
    setShowMentions(false);
    onMentionSelect?.(mention);
  };

  const styles = createStyles(colors);

  return (
    <View>
      <TextArea
        value={value}
        onChangeText={handleChange}
        {...props}
      />

      {showMentions && mentions.length > 0 && (
        <View style={styles.mentionsContainer}>
          {mentions.map((mention, index) => (
            <Text
              key={index}
              style={styles.mentionItem}
              onPress={() => handleMentionSelect(mention)}
            >
              @{mention}
            </Text>
          ))}
        </View>
      )}
    </View>
  );
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    label: {
      fontSize: 14,
      fontWeight: '500',
      color: colors.text,
      marginBottom: 6,
    },
    inputContainer: {
      backgroundColor: colors.surface,
      borderWidth: 1.5,
      borderRadius: 8,
      overflow: 'hidden',
    },
    input: {
      flex: 1,
      fontSize: 15,
      color: colors.text,
      padding: 12,
      lineHeight: 22,
    },
    disabled: {
      backgroundColor: colors.border + '30',
    },
    footer: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginTop: 4,
    },
    error: {
      fontSize: 12,
      color: colors.error,
    },
    hint: {
      fontSize: 12,
      color: colors.textSecondary,
    },
    charCount: {
      fontSize: 12,
      color: colors.textSecondary,
    },
    mentionsContainer: {
      position: 'absolute',
      top: '100%',
      left: 0,
      right: 0,
      backgroundColor: colors.surface,
      borderRadius: 8,
      borderWidth: 1,
      borderColor: colors.border,
      shadowColor: '#000',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.1,
      shadowRadius: 4,
      elevation: 3,
      zIndex: 1000,
      padding: 8,
    },
    mentionItem: {
      fontSize: 14,
      color: colors.primary,
      padding: 8,
    },
  });
