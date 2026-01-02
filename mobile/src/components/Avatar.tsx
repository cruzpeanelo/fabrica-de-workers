/**
 * Avatar Component - Issue #423
 * Avatar de usuario com imagem ou iniciais
 */

import React from 'react';
import { View, Text, Image, StyleSheet, ViewStyle } from 'react-native';
import { User } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type AvatarSize = 'xs' | 'sm' | 'md' | 'lg' | 'xl';

interface AvatarProps {
  source?: string | null;
  name?: string;
  size?: AvatarSize;
  backgroundColor?: string;
  style?: ViewStyle;
  showOnlineStatus?: boolean;
  isOnline?: boolean;
}

export function Avatar({
  source,
  name,
  size = 'md',
  backgroundColor,
  style,
  showOnlineStatus = false,
  isOnline = false,
}: AvatarProps) {
  const { colors } = useTheme();

  const getSizeValue = () => {
    const sizes = {
      xs: 24,
      sm: 32,
      md: 40,
      lg: 56,
      xl: 80,
    };
    return sizes[size] || sizes.md;
  };

  const getFontSize = () => {
    const fontSizes = {
      xs: 10,
      sm: 12,
      md: 16,
      lg: 22,
      xl: 32,
    };
    return fontSizes[size] || fontSizes.md;
  };

  const getInitials = () => {
    if (!name) return '';
    const parts = name.trim().split(' ');
    if (parts.length === 1) {
      return parts[0].charAt(0).toUpperCase();
    }
    return (parts[0].charAt(0) + parts[parts.length - 1].charAt(0)).toUpperCase();
  };

  const getBackgroundColor = () => {
    if (backgroundColor) return backgroundColor;
    if (!name) return colors.border;

    // Generate consistent color from name
    const colorPalette = [
      '#F87171', '#FB923C', '#FBBF24', '#A3E635',
      '#34D399', '#22D3EE', '#60A5FA', '#A78BFA',
      '#F472B6', '#E879F9',
    ];

    let hash = 0;
    for (let i = 0; i < name.length; i++) {
      hash = name.charCodeAt(i) + ((hash << 5) - hash);
    }
    return colorPalette[Math.abs(hash) % colorPalette.length];
  };

  const sizeValue = getSizeValue();
  const fontSize = getFontSize();
  const initials = getInitials();
  const bgColor = getBackgroundColor();

  const statusSize = Math.max(8, sizeValue * 0.25);

  return (
    <View style={[styles.container, style]}>
      <View
        style={[
          styles.avatar,
          {
            width: sizeValue,
            height: sizeValue,
            borderRadius: sizeValue / 2,
            backgroundColor: source ? 'transparent' : bgColor,
          },
        ]}
      >
        {source ? (
          <Image
            source={{ uri: source }}
            style={[
              styles.image,
              {
                width: sizeValue,
                height: sizeValue,
                borderRadius: sizeValue / 2,
              },
            ]}
          />
        ) : initials ? (
          <Text style={[styles.initials, { fontSize }]}>{initials}</Text>
        ) : (
          <User color="#FFFFFF" size={fontSize} />
        )}
      </View>

      {showOnlineStatus && (
        <View
          style={[
            styles.statusIndicator,
            {
              width: statusSize,
              height: statusSize,
              borderRadius: statusSize / 2,
              backgroundColor: isOnline ? colors.success : colors.textSecondary,
              borderColor: colors.surface,
              borderWidth: 2,
              right: 0,
              bottom: 0,
            },
          ]}
        />
      )}
    </View>
  );
}

// Avatar group for showing multiple users
interface AvatarGroupProps {
  users: Array<{ name?: string; avatar?: string }>;
  max?: number;
  size?: AvatarSize;
}

export function AvatarGroup({ users, max = 4, size = 'sm' }: AvatarGroupProps) {
  const { colors } = useTheme();
  const displayUsers = users.slice(0, max);
  const remaining = users.length - max;

  const getSizeValue = () => {
    const sizes = { xs: 24, sm: 32, md: 40, lg: 56, xl: 80 };
    return sizes[size] || sizes.sm;
  };

  const sizeValue = getSizeValue();
  const overlap = sizeValue * 0.3;

  return (
    <View style={styles.group}>
      {displayUsers.map((user, index) => (
        <View
          key={index}
          style={{
            marginLeft: index > 0 ? -overlap : 0,
            zIndex: displayUsers.length - index,
          }}
        >
          <Avatar
            source={user.avatar}
            name={user.name}
            size={size}
            style={{
              borderWidth: 2,
              borderColor: colors.surface,
              borderRadius: sizeValue / 2 + 2,
            }}
          />
        </View>
      ))}
      {remaining > 0 && (
        <View
          style={[
            styles.remaining,
            {
              marginLeft: -overlap,
              width: sizeValue,
              height: sizeValue,
              borderRadius: sizeValue / 2,
              backgroundColor: colors.border,
              borderWidth: 2,
              borderColor: colors.surface,
            },
          ]}
        >
          <Text style={[styles.remainingText, { fontSize: sizeValue * 0.35 }]}>
            +{remaining}
          </Text>
        </View>
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    position: 'relative',
  },
  avatar: {
    justifyContent: 'center',
    alignItems: 'center',
    overflow: 'hidden',
  },
  image: {
    resizeMode: 'cover',
  },
  initials: {
    color: '#FFFFFF',
    fontWeight: '600',
  },
  statusIndicator: {
    position: 'absolute',
  },
  group: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  remaining: {
    justifyContent: 'center',
    alignItems: 'center',
  },
  remainingText: {
    color: '#666',
    fontWeight: '600',
  },
});
