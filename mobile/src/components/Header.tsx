/**
 * Header Component - Issue #427
 * Header/AppBar customizavel
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
  TextStyle,
  StatusBar,
  Platform,
} from 'react-native';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { ArrowLeft, Menu, MoreVertical, Search, X } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type HeaderVariant = 'solid' | 'transparent' | 'gradient';

interface HeaderProps {
  title?: string;
  subtitle?: string;
  variant?: HeaderVariant;
  showBack?: boolean;
  onBack?: () => void;
  showMenu?: boolean;
  onMenu?: () => void;
  leftIcon?: React.ReactNode;
  onLeftPress?: () => void;
  rightActions?: React.ReactNode;
  centerContent?: React.ReactNode;
  backgroundColor?: string;
  titleColor?: string;
  large?: boolean;
  style?: ViewStyle;
  titleStyle?: TextStyle;
}

export function Header({
  title,
  subtitle,
  variant = 'solid',
  showBack = false,
  onBack,
  showMenu = false,
  onMenu,
  leftIcon,
  onLeftPress,
  rightActions,
  centerContent,
  backgroundColor,
  titleColor,
  large = false,
  style,
  titleStyle,
}: HeaderProps) {
  const { colors } = useTheme();
  const insets = useSafeAreaInsets();

  const getBackgroundColor = () => {
    if (backgroundColor) return backgroundColor;
    switch (variant) {
      case 'transparent':
        return 'transparent';
      default:
        return colors.primary;
    }
  };

  const getTextColor = () => {
    if (titleColor) return titleColor;
    if (variant === 'transparent') return colors.text;
    return '#FFFFFF';
  };

  const bgColor = getBackgroundColor();
  const textColor = getTextColor();

  return (
    <View
      style={[
        styles.container,
        { backgroundColor: bgColor, paddingTop: insets.top },
        style,
      ]}
    >
      <StatusBar
        barStyle={variant === 'transparent' ? 'dark-content' : 'light-content'}
        backgroundColor="transparent"
        translucent
      />

      <View style={[styles.content, large && styles.contentLarge]}>
        {/* Left Section */}
        <View style={styles.left}>
          {showBack && (
            <TouchableOpacity
              onPress={onBack}
              style={styles.iconButton}
              hitSlop={{ top: 10, bottom: 10, left: 10, right: 10 }}
            >
              <ArrowLeft color={textColor} size={24} />
            </TouchableOpacity>
          )}

          {showMenu && !showBack && (
            <TouchableOpacity
              onPress={onMenu}
              style={styles.iconButton}
              hitSlop={{ top: 10, bottom: 10, left: 10, right: 10 }}
            >
              <Menu color={textColor} size={24} />
            </TouchableOpacity>
          )}

          {leftIcon && !showBack && !showMenu && (
            <TouchableOpacity
              onPress={onLeftPress}
              style={styles.iconButton}
              hitSlop={{ top: 10, bottom: 10, left: 10, right: 10 }}
            >
              {leftIcon}
            </TouchableOpacity>
          )}
        </View>

        {/* Center Section */}
        <View style={styles.center}>
          {centerContent || (
            <View style={large ? styles.titleContainerLarge : styles.titleContainer}>
              <Text
                style={[
                  large ? styles.titleLarge : styles.title,
                  { color: textColor },
                  titleStyle,
                ]}
                numberOfLines={1}
              >
                {title}
              </Text>
              {subtitle && (
                <Text
                  style={[styles.subtitle, { color: textColor + 'CC' }]}
                  numberOfLines={1}
                >
                  {subtitle}
                </Text>
              )}
            </View>
          )}
        </View>

        {/* Right Section */}
        <View style={styles.right}>{rightActions}</View>
      </View>
    </View>
  );
}

// Search Header - header with integrated search
interface SearchHeaderProps {
  placeholder?: string;
  value?: string;
  onChangeText?: (text: string) => void;
  onBack?: () => void;
  onClear?: () => void;
  autoFocus?: boolean;
  style?: ViewStyle;
}

export function SearchHeader({
  placeholder = 'Buscar...',
  value,
  onChangeText,
  onBack,
  onClear,
  autoFocus = true,
  style,
}: SearchHeaderProps) {
  const { colors } = useTheme();
  const insets = useSafeAreaInsets();

  return (
    <View
      style={[
        styles.container,
        { backgroundColor: colors.surface, paddingTop: insets.top },
        style,
      ]}
    >
      <View style={styles.searchContent}>
        <TouchableOpacity onPress={onBack} style={styles.iconButton}>
          <ArrowLeft color={colors.text} size={24} />
        </TouchableOpacity>

        <View style={[styles.searchInput, { backgroundColor: colors.background }]}>
          <Search color={colors.textSecondary} size={20} />
          <Text
            style={[styles.searchText, { color: colors.text }]}
            numberOfLines={1}
          >
            {value || placeholder}
          </Text>
        </View>

        {value && value.length > 0 && (
          <TouchableOpacity onPress={onClear} style={styles.iconButton}>
            <X color={colors.textSecondary} size={20} />
          </TouchableOpacity>
        )}
      </View>
    </View>
  );
}

// Header Action Button
interface HeaderActionProps {
  icon: React.ReactNode;
  onPress: () => void;
  badge?: number;
  color?: string;
}

export function HeaderAction({ icon, onPress, badge, color }: HeaderActionProps) {
  const { colors } = useTheme();

  return (
    <TouchableOpacity
      onPress={onPress}
      style={styles.actionButton}
      hitSlop={{ top: 10, bottom: 10, left: 10, right: 10 }}
    >
      {icon}
      {badge !== undefined && badge > 0 && (
        <View style={[styles.badge, { backgroundColor: colors.error }]}>
          <Text style={styles.badgeText}>
            {badge > 99 ? '99+' : badge}
          </Text>
        </View>
      )}
    </TouchableOpacity>
  );
}

const styles = StyleSheet.create({
  container: {
    width: '100%',
  },
  content: {
    flexDirection: 'row',
    alignItems: 'center',
    height: 56,
    paddingHorizontal: 4,
  },
  contentLarge: {
    height: 72,
  },
  left: {
    width: 48,
    alignItems: 'flex-start',
  },
  center: {
    flex: 1,
    alignItems: 'center',
  },
  right: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'flex-end',
    minWidth: 48,
  },
  iconButton: {
    padding: 12,
  },
  titleContainer: {
    alignItems: 'center',
  },
  titleContainerLarge: {
    alignItems: 'flex-start',
    width: '100%',
    paddingHorizontal: 16,
  },
  title: {
    fontSize: 17,
    fontWeight: '600',
  },
  titleLarge: {
    fontSize: 28,
    fontWeight: 'bold',
  },
  subtitle: {
    fontSize: 12,
    marginTop: 2,
  },
  searchContent: {
    flexDirection: 'row',
    alignItems: 'center',
    height: 56,
    paddingHorizontal: 4,
    gap: 8,
  },
  searchInput: {
    flex: 1,
    flexDirection: 'row',
    alignItems: 'center',
    height: 40,
    borderRadius: 20,
    paddingHorizontal: 12,
    gap: 8,
  },
  searchText: {
    flex: 1,
    fontSize: 15,
  },
  actionButton: {
    padding: 12,
    position: 'relative',
  },
  badge: {
    position: 'absolute',
    top: 6,
    right: 6,
    minWidth: 18,
    height: 18,
    borderRadius: 9,
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 4,
  },
  badgeText: {
    color: '#FFF',
    fontSize: 10,
    fontWeight: 'bold',
  },
});
