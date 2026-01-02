/**
 * Slider Component - Issue #428
 * Slider de valor e range
 */

import React, { useRef, useCallback } from 'react';
import {
  View,
  Text,
  StyleSheet,
  PanResponder,
  Animated,
  ViewStyle,
  Dimensions,
} from 'react-native';
import { useTheme } from '../context/ThemeContext';

const { width: SCREEN_WIDTH } = Dimensions.get('window');

interface SliderProps {
  value: number;
  onValueChange: (value: number) => void;
  min?: number;
  max?: number;
  step?: number;
  showValue?: boolean;
  showMarkers?: boolean;
  markerCount?: number;
  label?: string;
  disabled?: boolean;
  color?: string;
  trackHeight?: number;
  thumbSize?: number;
  style?: ViewStyle;
}

export function Slider({
  value,
  onValueChange,
  min = 0,
  max = 100,
  step = 1,
  showValue = true,
  showMarkers = false,
  markerCount = 5,
  label,
  disabled = false,
  color,
  trackHeight = 4,
  thumbSize = 24,
  style,
}: SliderProps) {
  const { colors } = useTheme();
  const sliderColor = color || colors.primary;
  const trackRef = useRef<View>(null);
  const trackWidth = useRef(0);
  const translateX = useRef(new Animated.Value(0)).current;

  const clamp = (val: number) => Math.min(max, Math.max(min, val));
  const snapToStep = (val: number) => Math.round(val / step) * step;

  const valueToPosition = useCallback((val: number) => {
    const percentage = (val - min) / (max - min);
    return percentage * trackWidth.current;
  }, [min, max]);

  const positionToValue = useCallback((pos: number) => {
    const percentage = pos / trackWidth.current;
    const rawValue = min + percentage * (max - min);
    return snapToStep(clamp(rawValue));
  }, [min, max, step]);

  const panResponder = useRef(
    PanResponder.create({
      onStartShouldSetPanResponder: () => !disabled,
      onMoveShouldSetPanResponder: () => !disabled,
      onPanResponderGrant: (evt) => {
        const touchX = evt.nativeEvent.locationX;
        const newValue = positionToValue(touchX);
        onValueChange(newValue);
        translateX.setValue(valueToPosition(newValue));
      },
      onPanResponderMove: (evt) => {
        const touchX = evt.nativeEvent.locationX;
        const clampedX = Math.max(0, Math.min(trackWidth.current, touchX));
        const newValue = positionToValue(clampedX);
        onValueChange(newValue);
        translateX.setValue(valueToPosition(newValue));
      },
    })
  ).current;

  const handleLayout = (event: any) => {
    trackWidth.current = event.nativeEvent.layout.width;
    translateX.setValue(valueToPosition(value));
  };

  const percentage = ((value - min) / (max - min)) * 100;

  const markers = showMarkers
    ? Array.from({ length: markerCount }, (_, i) => {
        const markerValue = min + (i / (markerCount - 1)) * (max - min);
        return snapToStep(markerValue);
      })
    : [];

  return (
    <View style={[styles.container, style]}>
      {label && (
        <View style={styles.labelRow}>
          <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
          {showValue && (
            <Text style={[styles.valueLabel, { color: sliderColor }]}>
              {value}
            </Text>
          )}
        </View>
      )}

      <View
        ref={trackRef}
        style={[styles.track, { height: trackHeight, backgroundColor: colors.border }]}
        onLayout={handleLayout}
        {...panResponder.panHandlers}
      >
        {/* Filled Track */}
        <View
          style={[
            styles.filledTrack,
            {
              height: trackHeight,
              width: `${percentage}%`,
              backgroundColor: disabled ? colors.textSecondary : sliderColor,
            },
          ]}
        />

        {/* Thumb */}
        <Animated.View
          style={[
            styles.thumb,
            {
              width: thumbSize,
              height: thumbSize,
              borderRadius: thumbSize / 2,
              backgroundColor: disabled ? colors.textSecondary : sliderColor,
              transform: [{ translateX }],
              marginLeft: -thumbSize / 2,
            },
          ]}
        />
      </View>

      {/* Markers */}
      {showMarkers && (
        <View style={styles.markersContainer}>
          {markers.map((markerValue, index) => (
            <Text
              key={index}
              style={[styles.marker, { color: colors.textSecondary }]}
            >
              {markerValue}
            </Text>
          ))}
        </View>
      )}
    </View>
  );
}

// Range Slider
interface RangeSliderProps {
  values: [number, number];
  onValuesChange: (values: [number, number]) => void;
  min?: number;
  max?: number;
  step?: number;
  showValues?: boolean;
  label?: string;
  disabled?: boolean;
  color?: string;
  style?: ViewStyle;
}

export function RangeSlider({
  values,
  onValuesChange,
  min = 0,
  max = 100,
  step = 1,
  showValues = true,
  label,
  disabled = false,
  color,
  style,
}: RangeSliderProps) {
  const { colors } = useTheme();
  const sliderColor = color || colors.primary;
  const trackWidth = useRef(0);

  const clamp = (val: number) => Math.min(max, Math.max(min, val));
  const snapToStep = (val: number) => Math.round(val / step) * step;

  const leftPercentage = ((values[0] - min) / (max - min)) * 100;
  const rightPercentage = ((values[1] - min) / (max - min)) * 100;

  const positionToValue = useCallback((pos: number) => {
    const percentage = pos / trackWidth.current;
    const rawValue = min + percentage * (max - min);
    return snapToStep(clamp(rawValue));
  }, [min, max, step]);

  const handleLayout = (event: any) => {
    trackWidth.current = event.nativeEvent.layout.width;
  };

  const createPanResponder = (isLeft: boolean) =>
    PanResponder.create({
      onStartShouldSetPanResponder: () => !disabled,
      onMoveShouldSetPanResponder: () => !disabled,
      onPanResponderMove: (evt) => {
        const touchX = evt.nativeEvent.pageX;
        const newValue = positionToValue(Math.max(0, Math.min(trackWidth.current, touchX)));

        if (isLeft) {
          if (newValue < values[1]) {
            onValuesChange([newValue, values[1]]);
          }
        } else {
          if (newValue > values[0]) {
            onValuesChange([values[0], newValue]);
          }
        }
      },
    });

  const leftPanResponder = useRef(createPanResponder(true)).current;
  const rightPanResponder = useRef(createPanResponder(false)).current;

  return (
    <View style={[styles.container, style]}>
      {label && (
        <View style={styles.labelRow}>
          <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
          {showValues && (
            <Text style={[styles.valueLabel, { color: sliderColor }]}>
              {values[0]} - {values[1]}
            </Text>
          )}
        </View>
      )}

      <View
        style={[styles.track, { backgroundColor: colors.border }]}
        onLayout={handleLayout}
      >
        {/* Filled Track */}
        <View
          style={[
            styles.rangeFilledTrack,
            {
              left: `${leftPercentage}%`,
              width: `${rightPercentage - leftPercentage}%`,
              backgroundColor: disabled ? colors.textSecondary : sliderColor,
            },
          ]}
        />

        {/* Left Thumb */}
        <View
          style={[
            styles.thumb,
            styles.rangeThumb,
            {
              left: `${leftPercentage}%`,
              backgroundColor: disabled ? colors.textSecondary : sliderColor,
            },
          ]}
          {...leftPanResponder.panHandlers}
        />

        {/* Right Thumb */}
        <View
          style={[
            styles.thumb,
            styles.rangeThumb,
            {
              left: `${rightPercentage}%`,
              backgroundColor: disabled ? colors.textSecondary : sliderColor,
            },
          ]}
          {...rightPanResponder.panHandlers}
        />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    marginVertical: 8,
  },
  labelRow: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: 12,
  },
  label: {
    fontSize: 14,
    fontWeight: '500',
  },
  valueLabel: {
    fontSize: 14,
    fontWeight: '600',
  },
  track: {
    height: 4,
    borderRadius: 2,
    justifyContent: 'center',
  },
  filledTrack: {
    position: 'absolute',
    left: 0,
    borderRadius: 2,
  },
  rangeFilledTrack: {
    position: 'absolute',
    height: 4,
    borderRadius: 2,
  },
  thumb: {
    position: 'absolute',
    width: 24,
    height: 24,
    borderRadius: 12,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.2,
    shadowRadius: 3,
    elevation: 3,
  },
  rangeThumb: {
    marginLeft: -12,
    top: -10,
  },
  markersContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    marginTop: 8,
  },
  marker: {
    fontSize: 11,
  },
});
