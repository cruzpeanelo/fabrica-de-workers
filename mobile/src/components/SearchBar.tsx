/**
 * SearchBar Component - Issue #426
 * Campo de busca com debounce e filtros
 */

import React, { useState, useEffect, useCallback } from 'react';
import {
  View,
  TextInput,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
  Animated,
} from 'react-native';
import { Search, X, Filter, Mic } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

interface SearchBarProps {
  value?: string;
  onChangeText?: (text: string) => void;
  onSearch?: (text: string) => void;
  placeholder?: string;
  debounceMs?: number;
  showFilterButton?: boolean;
  onFilterPress?: () => void;
  showVoiceButton?: boolean;
  onVoicePress?: () => void;
  autoFocus?: boolean;
  style?: ViewStyle;
}

export function SearchBar({
  value,
  onChangeText,
  onSearch,
  placeholder = 'Buscar...',
  debounceMs = 300,
  showFilterButton = false,
  onFilterPress,
  showVoiceButton = false,
  onVoicePress,
  autoFocus = false,
  style,
}: SearchBarProps) {
  const { colors } = useTheme();
  const [localValue, setLocalValue] = useState(value || '');
  const [isFocused, setIsFocused] = useState(false);

  // Sync external value
  useEffect(() => {
    if (value !== undefined) {
      setLocalValue(value);
    }
  }, [value]);

  // Debounced search
  useEffect(() => {
    if (!onSearch) return;

    const timer = setTimeout(() => {
      onSearch(localValue);
    }, debounceMs);

    return () => clearTimeout(timer);
  }, [localValue, debounceMs, onSearch]);

  const handleChange = useCallback(
    (text: string) => {
      setLocalValue(text);
      onChangeText?.(text);
    },
    [onChangeText]
  );

  const handleClear = useCallback(() => {
    setLocalValue('');
    onChangeText?.('');
    onSearch?.('');
  }, [onChangeText, onSearch]);

  const styles = createStyles(colors);

  return (
    <View
      style={[
        styles.container,
        isFocused && { borderColor: colors.primary },
        style,
      ]}
    >
      <Search color={colors.textSecondary} size={20} />

      <TextInput
        style={styles.input}
        value={localValue}
        onChangeText={handleChange}
        placeholder={placeholder}
        placeholderTextColor={colors.textSecondary}
        onFocus={() => setIsFocused(true)}
        onBlur={() => setIsFocused(false)}
        autoFocus={autoFocus}
        autoCapitalize="none"
        autoCorrect={false}
        returnKeyType="search"
        onSubmitEditing={() => onSearch?.(localValue)}
      />

      {localValue.length > 0 && (
        <TouchableOpacity onPress={handleClear} style={styles.iconButton}>
          <X color={colors.textSecondary} size={18} />
        </TouchableOpacity>
      )}

      {showVoiceButton && !localValue && (
        <TouchableOpacity onPress={onVoicePress} style={styles.iconButton}>
          <Mic color={colors.textSecondary} size={20} />
        </TouchableOpacity>
      )}

      {showFilterButton && (
        <TouchableOpacity
          onPress={onFilterPress}
          style={[styles.iconButton, styles.filterButton]}
        >
          <Filter color={colors.primary} size={20} />
        </TouchableOpacity>
      )}
    </View>
  );
}

// Search with suggestions
interface SearchWithSuggestionsProps extends SearchBarProps {
  suggestions?: string[];
  onSuggestionPress?: (suggestion: string) => void;
  showSuggestions?: boolean;
}

export function SearchWithSuggestions({
  suggestions = [],
  onSuggestionPress,
  showSuggestions = true,
  ...props
}: SearchWithSuggestionsProps) {
  const { colors } = useTheme();
  const [isOpen, setIsOpen] = useState(false);

  const styles = createStyles(colors);

  return (
    <View>
      <SearchBar
        {...props}
        onChangeText={(text) => {
          props.onChangeText?.(text);
          setIsOpen(text.length > 0 && showSuggestions);
        }}
      />
      {isOpen && suggestions.length > 0 && (
        <View style={styles.suggestionsContainer}>
          {suggestions.slice(0, 5).map((suggestion, index) => (
            <TouchableOpacity
              key={index}
              style={styles.suggestionItem}
              onPress={() => {
                onSuggestionPress?.(suggestion);
                setIsOpen(false);
              }}
            >
              <Search color={colors.textSecondary} size={16} />
              <TextInput
                style={styles.suggestionText}
                value={suggestion}
                editable={false}
              />
            </TouchableOpacity>
          ))}
        </View>
      )}
    </View>
  );
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: colors.surface,
      borderRadius: 12,
      paddingHorizontal: 12,
      height: 48,
      borderWidth: 1.5,
      borderColor: colors.border,
      gap: 8,
    },
    input: {
      flex: 1,
      fontSize: 15,
      color: colors.text,
      paddingVertical: 0,
    },
    iconButton: {
      padding: 4,
    },
    filterButton: {
      marginLeft: 4,
      paddingLeft: 8,
      borderLeftWidth: 1,
      borderLeftColor: colors.border,
    },
    suggestionsContainer: {
      position: 'absolute',
      top: 52,
      left: 0,
      right: 0,
      backgroundColor: colors.surface,
      borderRadius: 12,
      borderWidth: 1,
      borderColor: colors.border,
      shadowColor: '#000',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.15,
      shadowRadius: 8,
      elevation: 5,
      zIndex: 1000,
    },
    suggestionItem: {
      flexDirection: 'row',
      alignItems: 'center',
      padding: 12,
      gap: 12,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    suggestionText: {
      flex: 1,
      fontSize: 14,
      color: colors.text,
    },
  });
