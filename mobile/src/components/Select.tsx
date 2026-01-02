/**
 * Select Component - Issue #427
 * Dropdown/picker com busca e multi-select
 */

import React, { useState, useCallback } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Modal,
  FlatList,
  TextInput,
  ViewStyle,
} from 'react-native';
import { ChevronDown, Check, Search, X } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

interface SelectOption {
  value: string;
  label: string;
  disabled?: boolean;
}

interface SelectProps {
  options: SelectOption[];
  value?: string | string[];
  onChange: (value: string | string[]) => void;
  placeholder?: string;
  label?: string;
  error?: string;
  disabled?: boolean;
  multiple?: boolean;
  searchable?: boolean;
  containerStyle?: ViewStyle;
}

export function Select({
  options,
  value,
  onChange,
  placeholder = 'Selecione...',
  label,
  error,
  disabled = false,
  multiple = false,
  searchable = false,
  containerStyle,
}: SelectProps) {
  const { colors } = useTheme();
  const [isOpen, setIsOpen] = useState(false);
  const [searchText, setSearchText] = useState('');

  const selectedValues = multiple
    ? (Array.isArray(value) ? value : [])
    : (value ? [value as string] : []);

  const getDisplayText = () => {
    if (selectedValues.length === 0) return placeholder;
    if (multiple) {
      if (selectedValues.length === 1) {
        const opt = options.find(o => o.value === selectedValues[0]);
        return opt?.label || selectedValues[0];
      }
      return `${selectedValues.length} selecionados`;
    }
    const opt = options.find(o => o.value === selectedValues[0]);
    return opt?.label || selectedValues[0];
  };

  const filteredOptions = searchable && searchText
    ? options.filter(o =>
        o.label.toLowerCase().includes(searchText.toLowerCase())
      )
    : options;

  const handleSelect = useCallback((optionValue: string) => {
    if (multiple) {
      const newValues = selectedValues.includes(optionValue)
        ? selectedValues.filter(v => v !== optionValue)
        : [...selectedValues, optionValue];
      onChange(newValues);
    } else {
      onChange(optionValue);
      setIsOpen(false);
    }
  }, [multiple, selectedValues, onChange]);

  const styles = createStyles(colors);

  return (
    <View style={containerStyle}>
      {label && <Text style={styles.label}>{label}</Text>}

      <TouchableOpacity
        style={[
          styles.trigger,
          error && { borderColor: colors.error },
          disabled && styles.disabled,
        ]}
        onPress={() => !disabled && setIsOpen(true)}
        activeOpacity={0.7}
      >
        <Text
          style={[
            styles.triggerText,
            selectedValues.length === 0 && { color: colors.textSecondary },
          ]}
          numberOfLines={1}
        >
          {getDisplayText()}
        </Text>
        <ChevronDown color={colors.textSecondary} size={20} />
      </TouchableOpacity>

      {error && <Text style={styles.error}>{error}</Text>}

      <Modal
        visible={isOpen}
        transparent
        animationType="fade"
        onRequestClose={() => setIsOpen(false)}
      >
        <TouchableOpacity
          style={styles.overlay}
          activeOpacity={1}
          onPress={() => setIsOpen(false)}
        >
          <View style={styles.dropdown}>
            {/* Header */}
            <View style={styles.dropdownHeader}>
              <Text style={styles.dropdownTitle}>{label || 'Selecione'}</Text>
              <TouchableOpacity onPress={() => setIsOpen(false)}>
                <X color={colors.text} size={24} />
              </TouchableOpacity>
            </View>

            {/* Search */}
            {searchable && (
              <View style={styles.searchContainer}>
                <Search color={colors.textSecondary} size={18} />
                <TextInput
                  style={styles.searchInput}
                  placeholder="Buscar..."
                  placeholderTextColor={colors.textSecondary}
                  value={searchText}
                  onChangeText={setSearchText}
                  autoFocus
                />
                {searchText.length > 0 && (
                  <TouchableOpacity onPress={() => setSearchText('')}>
                    <X color={colors.textSecondary} size={18} />
                  </TouchableOpacity>
                )}
              </View>
            )}

            {/* Options */}
            <FlatList
              data={filteredOptions}
              keyExtractor={(item) => item.value}
              renderItem={({ item }) => {
                const isSelected = selectedValues.includes(item.value);
                return (
                  <TouchableOpacity
                    style={[
                      styles.option,
                      isSelected && styles.optionSelected,
                      item.disabled && styles.optionDisabled,
                    ]}
                    onPress={() => !item.disabled && handleSelect(item.value)}
                    disabled={item.disabled}
                  >
                    <Text
                      style={[
                        styles.optionText,
                        isSelected && { color: colors.primary },
                        item.disabled && { color: colors.textSecondary },
                      ]}
                    >
                      {item.label}
                    </Text>
                    {isSelected && (
                      <Check color={colors.primary} size={20} />
                    )}
                  </TouchableOpacity>
                );
              }}
              ListEmptyComponent={
                <View style={styles.empty}>
                  <Text style={styles.emptyText}>Nenhuma opcao encontrada</Text>
                </View>
              }
              style={styles.optionsList}
            />

            {/* Done button for multiple */}
            {multiple && (
              <TouchableOpacity
                style={[styles.doneButton, { backgroundColor: colors.primary }]}
                onPress={() => setIsOpen(false)}
              >
                <Text style={styles.doneButtonText}>
                  Confirmar ({selectedValues.length})
                </Text>
              </TouchableOpacity>
            )}
          </View>
        </TouchableOpacity>
      </Modal>
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
    trigger: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'space-between',
      backgroundColor: colors.surface,
      borderWidth: 1.5,
      borderColor: colors.border,
      borderRadius: 8,
      paddingHorizontal: 12,
      height: 48,
    },
    triggerText: {
      flex: 1,
      fontSize: 15,
      color: colors.text,
    },
    disabled: {
      opacity: 0.5,
    },
    error: {
      fontSize: 12,
      color: colors.error,
      marginTop: 4,
    },
    overlay: {
      flex: 1,
      backgroundColor: 'rgba(0,0,0,0.5)',
      justifyContent: 'center',
      padding: 20,
    },
    dropdown: {
      backgroundColor: colors.surface,
      borderRadius: 16,
      maxHeight: '80%',
      overflow: 'hidden',
    },
    dropdownHeader: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'space-between',
      padding: 16,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    dropdownTitle: {
      fontSize: 18,
      fontWeight: '600',
      color: colors.text,
    },
    searchContainer: {
      flexDirection: 'row',
      alignItems: 'center',
      margin: 12,
      paddingHorizontal: 12,
      height: 44,
      backgroundColor: colors.background,
      borderRadius: 8,
      gap: 8,
    },
    searchInput: {
      flex: 1,
      fontSize: 15,
      color: colors.text,
    },
    optionsList: {
      maxHeight: 300,
    },
    option: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'space-between',
      padding: 16,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    optionSelected: {
      backgroundColor: colors.primary + '10',
    },
    optionDisabled: {
      opacity: 0.5,
    },
    optionText: {
      fontSize: 15,
      color: colors.text,
    },
    empty: {
      padding: 24,
      alignItems: 'center',
    },
    emptyText: {
      fontSize: 14,
      color: colors.textSecondary,
    },
    doneButton: {
      margin: 12,
      padding: 16,
      borderRadius: 8,
      alignItems: 'center',
    },
    doneButtonText: {
      fontSize: 16,
      fontWeight: '600',
      color: '#FFF',
    },
  });
