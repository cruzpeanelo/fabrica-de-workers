/**
 * TabBar Component - Issue #427
 * Tab navigation com badges e animacoes
 */

import React, { useRef, useEffect } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Animated,
  ScrollView,
  ViewStyle,
  Dimensions,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

interface Tab {
  key: string;
  label: string;
  icon?: React.ReactNode;
  badge?: number;
  disabled?: boolean;
}

interface TabBarProps {
  tabs: Tab[];
  activeTab: string;
  onTabChange: (tabKey: string) => void;
  variant?: 'default' | 'pills' | 'underline';
  scrollable?: boolean;
  showLabels?: boolean;
  showIcons?: boolean;
  style?: ViewStyle;
}

export function TabBar({
  tabs,
  activeTab,
  onTabChange,
  variant = 'default',
  scrollable = false,
  showLabels = true,
  showIcons = true,
  style,
}: TabBarProps) {
  const { colors } = useTheme();
  const indicatorPosition = useRef(new Animated.Value(0)).current;
  const scrollViewRef = useRef<ScrollView>(null);

  const activeIndex = tabs.findIndex((t) => t.key === activeTab);
  const tabWidth = scrollable ? 120 : Dimensions.get('window').width / tabs.length;

  useEffect(() => {
    Animated.spring(indicatorPosition, {
      toValue: activeIndex * tabWidth,
      useNativeDriver: true,
    }).start();

    // Scroll to active tab if scrollable
    if (scrollable && scrollViewRef.current) {
      scrollViewRef.current.scrollTo({
        x: Math.max(0, activeIndex * tabWidth - tabWidth),
        animated: true,
      });
    }
  }, [activeIndex, tabWidth, scrollable]);

  const renderTab = (tab: Tab, index: number) => {
    const isActive = tab.key === activeTab;

    return (
      <TouchableOpacity
        key={tab.key}
        style={[
          styles.tab,
          { width: scrollable ? tabWidth : undefined, flex: scrollable ? undefined : 1 },
          variant === 'pills' && isActive && {
            backgroundColor: colors.primary + '15',
            borderRadius: 20,
          },
        ]}
        onPress={() => !tab.disabled && onTabChange(tab.key)}
        disabled={tab.disabled}
        activeOpacity={0.7}
      >
        {showIcons && tab.icon && (
          <View style={styles.iconContainer}>
            {React.cloneElement(tab.icon as React.ReactElement, {
              color: isActive ? colors.primary : colors.textSecondary,
              size: 22,
            })}
            {tab.badge !== undefined && tab.badge > 0 && (
              <View style={[styles.badge, { backgroundColor: colors.error }]}>
                <Text style={styles.badgeText}>
                  {tab.badge > 99 ? '99+' : tab.badge}
                </Text>
              </View>
            )}
          </View>
        )}
        {showLabels && (
          <Text
            style={[
              styles.label,
              {
                color: isActive ? colors.primary : colors.textSecondary,
                fontWeight: isActive ? '600' : '500',
              },
              tab.disabled && { opacity: 0.5 },
            ]}
            numberOfLines={1}
          >
            {tab.label}
          </Text>
        )}
      </TouchableOpacity>
    );
  };

  const renderIndicator = () => {
    if (variant === 'pills') return null;

    return (
      <Animated.View
        style={[
          variant === 'underline' ? styles.underlineIndicator : styles.indicator,
          {
            width: tabWidth - (variant === 'underline' ? 0 : 16),
            backgroundColor: colors.primary,
            transform: [{ translateX: indicatorPosition }],
            marginLeft: variant === 'underline' ? 0 : 8,
          },
        ]}
      />
    );
  };

  const Container = scrollable ? ScrollView : View;
  const containerProps = scrollable
    ? {
        horizontal: true,
        showsHorizontalScrollIndicator: false,
        ref: scrollViewRef,
      }
    : {};

  return (
    <View style={[styles.container, { backgroundColor: colors.surface }, style]}>
      <Container {...containerProps} style={styles.tabsContainer}>
        {tabs.map(renderTab)}
      </Container>
      {renderIndicator()}
    </View>
  );
}

// Segmented Control - alternative style
interface SegmentedControlProps {
  segments: { key: string; label: string }[];
  activeSegment: string;
  onSegmentChange: (key: string) => void;
  style?: ViewStyle;
}

export function SegmentedControl({
  segments,
  activeSegment,
  onSegmentChange,
  style,
}: SegmentedControlProps) {
  const { colors } = useTheme();
  const activeIndex = segments.findIndex((s) => s.key === activeSegment);
  const translateX = useRef(new Animated.Value(0)).current;

  const segmentWidth = 100 / segments.length;

  useEffect(() => {
    Animated.spring(translateX, {
      toValue: activeIndex * segmentWidth,
      useNativeDriver: false,
    }).start();
  }, [activeIndex, segmentWidth]);

  return (
    <View
      style={[
        styles.segmentedContainer,
        { backgroundColor: colors.border },
        style,
      ]}
    >
      <Animated.View
        style={[
          styles.segmentedIndicator,
          {
            backgroundColor: colors.surface,
            width: `${segmentWidth}%`,
            left: translateX.interpolate({
              inputRange: [0, 100],
              outputRange: ['0%', '100%'],
            }),
          },
        ]}
      />
      {segments.map((segment) => {
        const isActive = segment.key === activeSegment;
        return (
          <TouchableOpacity
            key={segment.key}
            style={[styles.segment, { width: `${segmentWidth}%` }]}
            onPress={() => onSegmentChange(segment.key)}
            activeOpacity={0.7}
          >
            <Text
              style={[
                styles.segmentLabel,
                {
                  color: isActive ? colors.text : colors.textSecondary,
                  fontWeight: isActive ? '600' : '500',
                },
              ]}
            >
              {segment.label}
            </Text>
          </TouchableOpacity>
        );
      })}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    position: 'relative',
  },
  tabsContainer: {
    flexDirection: 'row',
  },
  tab: {
    alignItems: 'center',
    justifyContent: 'center',
    paddingVertical: 12,
    paddingHorizontal: 16,
    gap: 4,
  },
  iconContainer: {
    position: 'relative',
  },
  label: {
    fontSize: 13,
  },
  badge: {
    position: 'absolute',
    top: -4,
    right: -8,
    minWidth: 16,
    height: 16,
    borderRadius: 8,
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 4,
  },
  badgeText: {
    color: '#FFF',
    fontSize: 10,
    fontWeight: 'bold',
  },
  indicator: {
    position: 'absolute',
    bottom: 0,
    height: 3,
    borderTopLeftRadius: 2,
    borderTopRightRadius: 2,
  },
  underlineIndicator: {
    position: 'absolute',
    bottom: 0,
    height: 2,
  },
  segmentedContainer: {
    flexDirection: 'row',
    borderRadius: 8,
    padding: 2,
    position: 'relative',
  },
  segmentedIndicator: {
    position: 'absolute',
    top: 2,
    bottom: 2,
    borderRadius: 6,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 1 },
    shadowOpacity: 0.1,
    shadowRadius: 2,
    elevation: 2,
  },
  segment: {
    paddingVertical: 8,
    alignItems: 'center',
    zIndex: 1,
  },
  segmentLabel: {
    fontSize: 14,
  },
});
