/**
 * ListItem Component - Issue #427
 * Item de lista estruturado com acoes
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { ChevronRight } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

interface ListItemProps {
  title: string;
  subtitle?: string;
  description?: string;
  leading?: React.ReactNode;
  trailing?: React.ReactNode;
  showChevron?: boolean;
  onPress?: () => void;
  disabled?: boolean;
  selected?: boolean;
  divider?: boolean;
  compact?: boolean;
  style?: ViewStyle;
}

export function ListItem({
  title,
  subtitle,
  description,
  leading,
  trailing,
  showChevron = false,
  onPress,
  disabled = false,
  selected = false,
  divider = true,
  compact = false,
  style,
}: ListItemProps) {
  const { colors } = useTheme();

  const Container = onPress ? TouchableOpacity : View;

  return (
    <Container
      style={[
        styles.container,
        compact && styles.containerCompact,
        divider && { borderBottomWidth: 1, borderBottomColor: colors.border },
        selected && { backgroundColor: colors.primary + '10' },
        disabled && styles.disabled,
        style,
      ]}
      onPress={disabled ? undefined : onPress}
      activeOpacity={0.7}
    >
      {leading && <View style={styles.leading}>{leading}</View>}

      <View style={styles.content}>
        <Text
          style={[
            styles.title,
            { color: colors.text },
            selected && { color: colors.primary },
          ]}
          numberOfLines={1}
        >
          {title}
        </Text>
        {subtitle && (
          <Text
            style={[styles.subtitle, { color: colors.textSecondary }]}
            numberOfLines={1}
          >
            {subtitle}
          </Text>
        )}
        {description && (
          <Text
            style={[styles.description, { color: colors.textSecondary }]}
            numberOfLines={2}
          >
            {description}
          </Text>
        )}
      </View>

      {trailing && <View style={styles.trailing}>{trailing}</View>}

      {showChevron && (
        <ChevronRight color={colors.textSecondary} size={20} />
      )}
    </Container>
  );
}

// List Section Header
interface ListSectionHeaderProps {
  title: string;
  action?: React.ReactNode;
  style?: ViewStyle;
}

export function ListSectionHeader({ title, action, style }: ListSectionHeaderProps) {
  const { colors } = useTheme();

  return (
    <View style={[styles.sectionHeader, { backgroundColor: colors.background }, style]}>
      <Text style={[styles.sectionTitle, { color: colors.textSecondary }]}>
        {title}
      </Text>
      {action}
    </View>
  );
}

// List Item with Switch
interface ListItemSwitchProps {
  title: string;
  subtitle?: string;
  leading?: React.ReactNode;
  value: boolean;
  onValueChange: (value: boolean) => void;
  disabled?: boolean;
  divider?: boolean;
}

export function ListItemSwitch({
  title,
  subtitle,
  leading,
  value,
  onValueChange,
  disabled = false,
  divider = true,
}: ListItemSwitchProps) {
  const { colors } = useTheme();

  return (
    <View
      style={[
        styles.container,
        divider && { borderBottomWidth: 1, borderBottomColor: colors.border },
        disabled && styles.disabled,
      ]}
    >
      {leading && <View style={styles.leading}>{leading}</View>}

      <View style={styles.content}>
        <Text style={[styles.title, { color: colors.text }]}>{title}</Text>
        {subtitle && (
          <Text style={[styles.subtitle, { color: colors.textSecondary }]}>
            {subtitle}
          </Text>
        )}
      </View>

      <TouchableOpacity
        style={[
          styles.switch,
          {
            backgroundColor: value ? colors.primary : colors.border,
          },
        ]}
        onPress={() => !disabled && onValueChange(!value)}
        activeOpacity={0.8}
      >
        <View
          style={[
            styles.switchThumb,
            {
              transform: [{ translateX: value ? 20 : 0 }],
            },
          ]}
        />
      </TouchableOpacity>
    </View>
  );
}

// List Item with Badge
interface ListItemBadgeProps {
  title: string;
  subtitle?: string;
  leading?: React.ReactNode;
  badge: number | string;
  badgeColor?: string;
  onPress?: () => void;
  divider?: boolean;
}

export function ListItemBadge({
  title,
  subtitle,
  leading,
  badge,
  badgeColor,
  onPress,
  divider = true,
}: ListItemBadgeProps) {
  const { colors } = useTheme();
  const bgColor = badgeColor || colors.primary;

  return (
    <ListItem
      title={title}
      subtitle={subtitle}
      leading={leading}
      onPress={onPress}
      divider={divider}
      trailing={
        <View style={[styles.badge, { backgroundColor: bgColor }]}>
          <Text style={styles.badgeText}>{badge}</Text>
        </View>
      }
    />
  );
}

// Menu List Item - for settings/menu screens
interface MenuListItemProps {
  title: string;
  icon: React.ReactNode;
  value?: string;
  onPress: () => void;
  destructive?: boolean;
  divider?: boolean;
}

export function MenuListItem({
  title,
  icon,
  value,
  onPress,
  destructive = false,
  divider = true,
}: MenuListItemProps) {
  const { colors } = useTheme();

  return (
    <TouchableOpacity
      style={[
        styles.menuItem,
        divider && { borderBottomWidth: 1, borderBottomColor: colors.border },
      ]}
      onPress={onPress}
      activeOpacity={0.7}
    >
      <View
        style={[
          styles.menuIcon,
          { backgroundColor: destructive ? colors.error + '15' : colors.primary + '15' },
        ]}
      >
        {React.cloneElement(icon as React.ReactElement, {
          color: destructive ? colors.error : colors.primary,
          size: 20,
        })}
      </View>
      <Text
        style={[
          styles.menuTitle,
          { color: destructive ? colors.error : colors.text },
        ]}
      >
        {title}
      </Text>
      {value && (
        <Text style={[styles.menuValue, { color: colors.textSecondary }]}>
          {value}
        </Text>
      )}
      <ChevronRight color={colors.textSecondary} size={20} />
    </TouchableOpacity>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 12,
    paddingHorizontal: 16,
    gap: 12,
  },
  containerCompact: {
    paddingVertical: 8,
  },
  disabled: {
    opacity: 0.5,
  },
  leading: {
    width: 40,
    alignItems: 'center',
  },
  content: {
    flex: 1,
  },
  title: {
    fontSize: 15,
    fontWeight: '500',
  },
  subtitle: {
    fontSize: 13,
    marginTop: 2,
  },
  description: {
    fontSize: 13,
    marginTop: 4,
    lineHeight: 18,
  },
  trailing: {
    marginLeft: 8,
  },
  sectionHeader: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between',
    paddingHorizontal: 16,
    paddingVertical: 8,
  },
  sectionTitle: {
    fontSize: 13,
    fontWeight: '600',
    textTransform: 'uppercase',
    letterSpacing: 0.5,
  },
  switch: {
    width: 50,
    height: 30,
    borderRadius: 15,
    padding: 2,
  },
  switchThumb: {
    width: 26,
    height: 26,
    borderRadius: 13,
    backgroundColor: '#FFF',
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.2,
    shadowRadius: 2,
    elevation: 2,
  },
  badge: {
    minWidth: 24,
    height: 24,
    borderRadius: 12,
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 8,
  },
  badgeText: {
    color: '#FFF',
    fontSize: 12,
    fontWeight: '600',
  },
  menuItem: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 12,
    paddingHorizontal: 16,
    gap: 12,
  },
  menuIcon: {
    width: 36,
    height: 36,
    borderRadius: 8,
    justifyContent: 'center',
    alignItems: 'center',
  },
  menuTitle: {
    flex: 1,
    fontSize: 15,
    fontWeight: '500',
  },
  menuValue: {
    fontSize: 14,
    marginRight: 4,
  },
});
