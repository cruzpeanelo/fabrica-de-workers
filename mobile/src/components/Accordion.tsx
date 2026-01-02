/**
 * Accordion Component - Issue #427
 * Secoes colapsaveis com animacao
 */

import React, { useState, useRef, useCallback } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Animated,
  LayoutAnimation,
  Platform,
  UIManager,
  ViewStyle,
} from 'react-native';
import { ChevronDown, ChevronRight } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

// Enable LayoutAnimation for Android
if (Platform.OS === 'android' && UIManager.setLayoutAnimationEnabledExperimental) {
  UIManager.setLayoutAnimationEnabledExperimental(true);
}

interface AccordionItemProps {
  title: string;
  children: React.ReactNode;
  isExpanded?: boolean;
  onToggle?: () => void;
  icon?: React.ReactNode;
  subtitle?: string;
  disabled?: boolean;
  style?: ViewStyle;
}

export function AccordionItem({
  title,
  children,
  isExpanded = false,
  onToggle,
  icon,
  subtitle,
  disabled = false,
  style,
}: AccordionItemProps) {
  const { colors } = useTheme();
  const rotateAnim = useRef(new Animated.Value(isExpanded ? 1 : 0)).current;

  React.useEffect(() => {
    Animated.timing(rotateAnim, {
      toValue: isExpanded ? 1 : 0,
      duration: 200,
      useNativeDriver: true,
    }).start();
  }, [isExpanded]);

  const rotation = rotateAnim.interpolate({
    inputRange: [0, 1],
    outputRange: ['0deg', '180deg'],
  });

  const handleToggle = () => {
    LayoutAnimation.configureNext(LayoutAnimation.Presets.easeInEaseOut);
    onToggle?.();
  };

  return (
    <View style={[styles.item, { borderColor: colors.border }, style]}>
      <TouchableOpacity
        style={[
          styles.header,
          disabled && styles.disabled,
        ]}
        onPress={handleToggle}
        disabled={disabled}
        activeOpacity={0.7}
      >
        {icon && <View style={styles.icon}>{icon}</View>}
        <View style={styles.titleContainer}>
          <Text style={[styles.title, { color: colors.text }]}>{title}</Text>
          {subtitle && (
            <Text style={[styles.subtitle, { color: colors.textSecondary }]}>
              {subtitle}
            </Text>
          )}
        </View>
        <Animated.View style={{ transform: [{ rotate: rotation }] }}>
          <ChevronDown color={colors.textSecondary} size={20} />
        </Animated.View>
      </TouchableOpacity>

      {isExpanded && (
        <View style={[styles.content, { borderTopColor: colors.border }]}>
          {children}
        </View>
      )}
    </View>
  );
}

// Accordion Group - manages multiple items
interface AccordionSection {
  key: string;
  title: string;
  content: React.ReactNode;
  icon?: React.ReactNode;
  subtitle?: string;
  disabled?: boolean;
}

interface AccordionProps {
  sections: AccordionSection[];
  expandedKeys?: string[];
  onExpandChange?: (keys: string[]) => void;
  allowMultiple?: boolean;
  style?: ViewStyle;
}

export function Accordion({
  sections,
  expandedKeys = [],
  onExpandChange,
  allowMultiple = false,
  style,
}: AccordionProps) {
  const [localExpandedKeys, setLocalExpandedKeys] = useState<string[]>(expandedKeys);

  const handleToggle = useCallback((key: string) => {
    let newKeys: string[];

    if (allowMultiple) {
      newKeys = localExpandedKeys.includes(key)
        ? localExpandedKeys.filter(k => k !== key)
        : [...localExpandedKeys, key];
    } else {
      newKeys = localExpandedKeys.includes(key) ? [] : [key];
    }

    setLocalExpandedKeys(newKeys);
    onExpandChange?.(newKeys);
  }, [localExpandedKeys, allowMultiple, onExpandChange]);

  return (
    <View style={style}>
      {sections.map((section) => (
        <AccordionItem
          key={section.key}
          title={section.title}
          subtitle={section.subtitle}
          icon={section.icon}
          isExpanded={localExpandedKeys.includes(section.key)}
          onToggle={() => handleToggle(section.key)}
          disabled={section.disabled}
        >
          {section.content}
        </AccordionItem>
      ))}
    </View>
  );
}

// FAQ Accordion - styled for FAQ sections
interface FAQItem {
  question: string;
  answer: string;
}

interface FAQAccordionProps {
  items: FAQItem[];
  style?: ViewStyle;
}

export function FAQAccordion({ items, style }: FAQAccordionProps) {
  const { colors } = useTheme();
  const [expandedIndex, setExpandedIndex] = useState<number | null>(null);

  return (
    <View style={style}>
      {items.map((item, index) => (
        <AccordionItem
          key={index}
          title={item.question}
          isExpanded={expandedIndex === index}
          onToggle={() => setExpandedIndex(expandedIndex === index ? null : index)}
          style={styles.faqItem}
        >
          <Text style={[styles.faqAnswer, { color: colors.textSecondary }]}>
            {item.answer}
          </Text>
        </AccordionItem>
      ))}
    </View>
  );
}

const styles = StyleSheet.create({
  item: {
    borderWidth: 1,
    borderRadius: 8,
    marginBottom: 8,
    overflow: 'hidden',
  },
  header: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 16,
    gap: 12,
  },
  disabled: {
    opacity: 0.5,
  },
  icon: {
    width: 24,
    alignItems: 'center',
  },
  titleContainer: {
    flex: 1,
  },
  title: {
    fontSize: 15,
    fontWeight: '600',
  },
  subtitle: {
    fontSize: 13,
    marginTop: 2,
  },
  content: {
    padding: 16,
    paddingTop: 0,
    borderTopWidth: 1,
  },
  faqItem: {
    marginBottom: 12,
  },
  faqAnswer: {
    fontSize: 14,
    lineHeight: 22,
    paddingTop: 16,
  },
});
