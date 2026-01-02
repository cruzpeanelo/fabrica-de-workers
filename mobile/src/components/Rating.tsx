/**
 * Rating Component - Issue #428
 * Avaliacao com estrelas
 */

import React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  ViewStyle,
} from 'react-native';
import { Star } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type RatingSize = 'sm' | 'md' | 'lg';

interface RatingProps {
  value: number;
  onValueChange?: (value: number) => void;
  maxStars?: number;
  size?: RatingSize;
  allowHalf?: boolean;
  readonly?: boolean;
  showValue?: boolean;
  label?: string;
  color?: string;
  emptyColor?: string;
  style?: ViewStyle;
}

export function Rating({
  value,
  onValueChange,
  maxStars = 5,
  size = 'md',
  allowHalf = false,
  readonly = false,
  showValue = false,
  label,
  color,
  emptyColor,
  style,
}: RatingProps) {
  const { colors } = useTheme();
  const starColor = color || '#FBBF24'; // Yellow/gold
  const emptyStarColor = emptyColor || colors.border;

  const getSizeValue = () => {
    switch (size) {
      case 'sm':
        return 18;
      case 'lg':
        return 32;
      default:
        return 24;
    }
  };

  const starSize = getSizeValue();

  const handlePress = (starIndex: number, isHalf: boolean = false) => {
    if (readonly || !onValueChange) return;

    let newValue: number;
    if (allowHalf && isHalf) {
      newValue = starIndex + 0.5;
    } else {
      newValue = starIndex + 1;
    }

    // Toggle off if clicking same value
    if (newValue === value) {
      onValueChange(0);
    } else {
      onValueChange(newValue);
    }
  };

  const renderStar = (index: number) => {
    const filled = value >= index + 1;
    const halfFilled = allowHalf && value >= index + 0.5 && value < index + 1;

    return (
      <TouchableOpacity
        key={index}
        onPress={() => handlePress(index)}
        disabled={readonly}
        activeOpacity={readonly ? 1 : 0.7}
        style={styles.starContainer}
      >
        {/* Empty star background */}
        <Star
          color={emptyStarColor}
          size={starSize}
          fill={emptyStarColor}
        />

        {/* Filled star overlay */}
        {(filled || halfFilled) && (
          <View
            style={[
              styles.filledOverlay,
              halfFilled && { width: '50%' },
            ]}
          >
            <Star
              color={starColor}
              size={starSize}
              fill={starColor}
            />
          </View>
        )}

        {/* Half star touch area */}
        {allowHalf && !readonly && (
          <TouchableOpacity
            style={styles.halfTouchArea}
            onPress={() => handlePress(index, true)}
          />
        )}
      </TouchableOpacity>
    );
  };

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}
      <View style={styles.container}>
        <View style={styles.starsRow}>
          {Array.from({ length: maxStars }, (_, i) => renderStar(i))}
        </View>
        {showValue && (
          <Text style={[styles.valueText, { color: colors.textSecondary }]}>
            {value.toFixed(allowHalf ? 1 : 0)} / {maxStars}
          </Text>
        )}
      </View>
    </View>
  );
}

// Compact Rating Display - for showing ratings inline
interface RatingDisplayProps {
  value: number;
  maxStars?: number;
  size?: RatingSize;
  showCount?: boolean;
  count?: number;
  color?: string;
  style?: ViewStyle;
}

export function RatingDisplay({
  value,
  maxStars = 5,
  size = 'sm',
  showCount = false,
  count,
  color,
  style,
}: RatingDisplayProps) {
  const { colors } = useTheme();
  const starColor = color || '#FBBF24';

  const getSizeValue = () => {
    switch (size) {
      case 'sm':
        return 14;
      case 'lg':
        return 20;
      default:
        return 16;
    }
  };

  const starSize = getSizeValue();

  return (
    <View style={[styles.displayContainer, style]}>
      <Star color={starColor} size={starSize} fill={starColor} />
      <Text style={[styles.displayValue, { color: colors.text, fontSize: starSize - 2 }]}>
        {value.toFixed(1)}
      </Text>
      {showCount && count !== undefined && (
        <Text style={[styles.displayCount, { color: colors.textSecondary }]}>
          ({count})
        </Text>
      )}
    </View>
  );
}

// Rating Input with Labels
interface LabeledRatingProps {
  value: number;
  onValueChange: (value: number) => void;
  labels?: string[];
  size?: RatingSize;
  color?: string;
  style?: ViewStyle;
}

export function LabeledRating({
  value,
  onValueChange,
  labels = ['Ruim', 'Regular', 'Bom', 'Muito Bom', 'Excelente'],
  size = 'md',
  color,
  style,
}: LabeledRatingProps) {
  const { colors } = useTheme();
  const currentLabel = value > 0 ? labels[Math.min(Math.floor(value) - 1, labels.length - 1)] : '';

  return (
    <View style={style}>
      <Rating
        value={value}
        onValueChange={onValueChange}
        maxStars={labels.length}
        size={size}
        color={color}
      />
      {currentLabel && (
        <Text style={[styles.ratingLabel, { color: colors.primary }]}>
          {currentLabel}
        </Text>
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  label: {
    fontSize: 14,
    fontWeight: '500',
    marginBottom: 8,
  },
  starsRow: {
    flexDirection: 'row',
    gap: 4,
  },
  starContainer: {
    position: 'relative',
  },
  filledOverlay: {
    position: 'absolute',
    top: 0,
    left: 0,
    overflow: 'hidden',
  },
  halfTouchArea: {
    position: 'absolute',
    top: 0,
    left: 0,
    width: '50%',
    height: '100%',
  },
  valueText: {
    fontSize: 14,
    fontWeight: '500',
  },
  displayContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 4,
  },
  displayValue: {
    fontWeight: '600',
  },
  displayCount: {
    fontSize: 12,
  },
  ratingLabel: {
    fontSize: 14,
    fontWeight: '500',
    textAlign: 'center',
    marginTop: 8,
  },
});
