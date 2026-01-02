/**
 * Tooltip Component - Issue #428
 * Dicas de contexto com posicionamento
 */

import React, { useState, useRef, useCallback } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  Modal,
  StyleSheet,
  Dimensions,
  ViewStyle,
  LayoutChangeEvent,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

const { width: SCREEN_WIDTH, height: SCREEN_HEIGHT } = Dimensions.get('window');

type TooltipPosition = 'top' | 'bottom' | 'left' | 'right' | 'auto';

interface TooltipProps {
  content: string | React.ReactNode;
  children: React.ReactNode;
  position?: TooltipPosition;
  backgroundColor?: string;
  textColor?: string;
  delay?: number;
  width?: number;
  disabled?: boolean;
  onShow?: () => void;
  onHide?: () => void;
}

export function Tooltip({
  content,
  children,
  position = 'top',
  backgroundColor,
  textColor,
  delay = 0,
  width = 200,
  disabled = false,
  onShow,
  onHide,
}: TooltipProps) {
  const { colors } = useTheme();
  const [visible, setVisible] = useState(false);
  const [tooltipPosition, setTooltipPosition] = useState({ x: 0, y: 0 });
  const [actualPosition, setActualPosition] = useState<TooltipPosition>(position);
  const triggerRef = useRef<View>(null);
  const timeoutRef = useRef<NodeJS.Timeout>();

  const bgColor = backgroundColor || colors.text;
  const txtColor = textColor || colors.background;

  const showTooltip = useCallback(() => {
    if (disabled) return;

    triggerRef.current?.measureInWindow((x, y, triggerWidth, triggerHeight) => {
      let tooltipX = x;
      let tooltipY = y;
      let finalPosition = position;

      // Auto position based on available space
      if (position === 'auto') {
        const spaceTop = y;
        const spaceBottom = SCREEN_HEIGHT - y - triggerHeight;
        const spaceLeft = x;
        const spaceRight = SCREEN_WIDTH - x - triggerWidth;

        if (spaceTop >= 60) {
          finalPosition = 'top';
        } else if (spaceBottom >= 60) {
          finalPosition = 'bottom';
        } else if (spaceRight >= width + 20) {
          finalPosition = 'right';
        } else {
          finalPosition = 'left';
        }
      }

      // Calculate position based on direction
      switch (finalPosition) {
        case 'top':
          tooltipX = x + triggerWidth / 2 - width / 2;
          tooltipY = y - 8;
          break;
        case 'bottom':
          tooltipX = x + triggerWidth / 2 - width / 2;
          tooltipY = y + triggerHeight + 8;
          break;
        case 'left':
          tooltipX = x - width - 8;
          tooltipY = y + triggerHeight / 2;
          break;
        case 'right':
          tooltipX = x + triggerWidth + 8;
          tooltipY = y + triggerHeight / 2;
          break;
      }

      // Keep tooltip on screen
      tooltipX = Math.max(8, Math.min(SCREEN_WIDTH - width - 8, tooltipX));

      setTooltipPosition({ x: tooltipX, y: tooltipY });
      setActualPosition(finalPosition);

      if (delay > 0) {
        timeoutRef.current = setTimeout(() => {
          setVisible(true);
          onShow?.();
        }, delay);
      } else {
        setVisible(true);
        onShow?.();
      }
    });
  }, [disabled, position, width, delay, onShow]);

  const hideTooltip = useCallback(() => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }
    setVisible(false);
    onHide?.();
  }, [onHide]);

  const getArrowStyle = () => {
    const arrowSize = 8;
    const baseStyle = {
      width: 0,
      height: 0,
      borderStyle: 'solid' as const,
      position: 'absolute' as const,
    };

    switch (actualPosition) {
      case 'top':
        return {
          ...baseStyle,
          bottom: -arrowSize,
          left: '50%',
          marginLeft: -arrowSize,
          borderTopWidth: arrowSize,
          borderRightWidth: arrowSize,
          borderBottomWidth: 0,
          borderLeftWidth: arrowSize,
          borderTopColor: bgColor,
          borderRightColor: 'transparent',
          borderBottomColor: 'transparent',
          borderLeftColor: 'transparent',
        };
      case 'bottom':
        return {
          ...baseStyle,
          top: -arrowSize,
          left: '50%',
          marginLeft: -arrowSize,
          borderTopWidth: 0,
          borderRightWidth: arrowSize,
          borderBottomWidth: arrowSize,
          borderLeftWidth: arrowSize,
          borderTopColor: 'transparent',
          borderRightColor: 'transparent',
          borderBottomColor: bgColor,
          borderLeftColor: 'transparent',
        };
      case 'left':
        return {
          ...baseStyle,
          right: -arrowSize,
          top: '50%',
          marginTop: -arrowSize,
          borderTopWidth: arrowSize,
          borderRightWidth: 0,
          borderBottomWidth: arrowSize,
          borderLeftWidth: arrowSize,
          borderTopColor: 'transparent',
          borderRightColor: 'transparent',
          borderBottomColor: 'transparent',
          borderLeftColor: bgColor,
        };
      case 'right':
        return {
          ...baseStyle,
          left: -arrowSize,
          top: '50%',
          marginTop: -arrowSize,
          borderTopWidth: arrowSize,
          borderRightWidth: arrowSize,
          borderBottomWidth: arrowSize,
          borderLeftWidth: 0,
          borderTopColor: 'transparent',
          borderRightColor: bgColor,
          borderBottomColor: 'transparent',
          borderLeftColor: 'transparent',
        };
      default:
        return baseStyle;
    }
  };

  return (
    <>
      <TouchableOpacity
        ref={triggerRef}
        onPress={showTooltip}
        onLongPress={showTooltip}
        activeOpacity={0.8}
      >
        {children}
      </TouchableOpacity>

      <Modal visible={visible} transparent animationType="fade">
        <TouchableOpacity
          style={styles.overlay}
          activeOpacity={1}
          onPress={hideTooltip}
        >
          <View
            style={[
              styles.tooltip,
              {
                left: tooltipPosition.x,
                top: tooltipPosition.y,
                width,
                backgroundColor: bgColor,
              },
              actualPosition === 'top' && { transform: [{ translateY: -40 }] },
              (actualPosition === 'left' || actualPosition === 'right') && {
                transform: [{ translateY: -20 }],
              },
            ]}
          >
            {typeof content === 'string' ? (
              <Text style={[styles.text, { color: txtColor }]}>{content}</Text>
            ) : (
              content
            )}
            <View style={getArrowStyle()} />
          </View>
        </TouchableOpacity>
      </Modal>
    </>
  );
}

// Info Tooltip - with info icon
interface InfoTooltipProps {
  content: string;
  size?: number;
  color?: string;
}

export function InfoTooltip({ content, size = 16, color }: InfoTooltipProps) {
  const { colors } = useTheme();
  const iconColor = color || colors.textSecondary;

  return (
    <Tooltip content={content} position="top">
      <View style={[styles.infoIcon, { width: size, height: size }]}>
        <Text style={[styles.infoIconText, { color: iconColor, fontSize: size * 0.7 }]}>
          ?
        </Text>
      </View>
    </Tooltip>
  );
}

// Label with Tooltip
interface LabelWithTooltipProps {
  label: string;
  tooltip: string;
  required?: boolean;
  style?: ViewStyle;
}

export function LabelWithTooltip({
  label,
  tooltip,
  required = false,
  style,
}: LabelWithTooltipProps) {
  const { colors } = useTheme();

  return (
    <View style={[styles.labelContainer, style]}>
      <Text style={[styles.label, { color: colors.text }]}>
        {label}
        {required && <Text style={{ color: colors.error }}> *</Text>}
      </Text>
      <InfoTooltip content={tooltip} />
    </View>
  );
}

const styles = StyleSheet.create({
  overlay: {
    flex: 1,
  },
  tooltip: {
    position: 'absolute',
    padding: 12,
    borderRadius: 8,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.2,
    shadowRadius: 8,
    elevation: 5,
  },
  text: {
    fontSize: 13,
    lineHeight: 18,
  },
  infoIcon: {
    borderRadius: 100,
    borderWidth: 1.5,
    borderColor: 'currentColor',
    justifyContent: 'center',
    alignItems: 'center',
    marginLeft: 4,
  },
  infoIconText: {
    fontWeight: '600',
  },
  labelContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    marginBottom: 6,
  },
  label: {
    fontSize: 14,
    fontWeight: '500',
  },
});
