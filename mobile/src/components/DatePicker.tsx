/**
 * DatePicker Component - Issue #429
 * Seletor de data com calendario
 */

import React, { useState, useCallback, useMemo } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  Modal,
  StyleSheet,
  ScrollView,
  ViewStyle,
} from 'react-native';
import { ChevronLeft, ChevronRight, Calendar, Clock, X } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

const DAYS = ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab'];
const MONTHS = [
  'Janeiro', 'Fevereiro', 'Marco', 'Abril', 'Maio', 'Junho',
  'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro',
];

interface DatePickerProps {
  value?: Date;
  onChange: (date: Date) => void;
  minDate?: Date;
  maxDate?: Date;
  placeholder?: string;
  label?: string;
  disabled?: boolean;
  error?: string;
  format?: 'short' | 'medium' | 'long';
  style?: ViewStyle;
}

export function DatePicker({
  value,
  onChange,
  minDate,
  maxDate,
  placeholder = 'Selecionar data',
  label,
  disabled = false,
  error,
  format = 'medium',
  style,
}: DatePickerProps) {
  const { colors } = useTheme();
  const [visible, setVisible] = useState(false);
  const [viewDate, setViewDate] = useState(value || new Date());

  const formatDate = (date: Date) => {
    const day = date.getDate().toString().padStart(2, '0');
    const month = (date.getMonth() + 1).toString().padStart(2, '0');
    const year = date.getFullYear();

    switch (format) {
      case 'short':
        return `${day}/${month}`;
      case 'long':
        return `${day} de ${MONTHS[date.getMonth()]} de ${year}`;
      default:
        return `${day}/${month}/${year}`;
    }
  };

  const getDaysInMonth = (year: number, month: number) => {
    return new Date(year, month + 1, 0).getDate();
  };

  const getFirstDayOfMonth = (year: number, month: number) => {
    return new Date(year, month, 1).getDay();
  };

  const isDateDisabled = (date: Date) => {
    if (minDate && date < minDate) return true;
    if (maxDate && date > maxDate) return true;
    return false;
  };

  const isSameDay = (date1: Date, date2: Date) => {
    return (
      date1.getDate() === date2.getDate() &&
      date1.getMonth() === date2.getMonth() &&
      date1.getFullYear() === date2.getFullYear()
    );
  };

  const isToday = (date: Date) => {
    return isSameDay(date, new Date());
  };

  const calendarDays = useMemo(() => {
    const year = viewDate.getFullYear();
    const month = viewDate.getMonth();
    const daysInMonth = getDaysInMonth(year, month);
    const firstDay = getFirstDayOfMonth(year, month);
    const days: (Date | null)[] = [];

    // Empty days before first day
    for (let i = 0; i < firstDay; i++) {
      days.push(null);
    }

    // Days of month
    for (let i = 1; i <= daysInMonth; i++) {
      days.push(new Date(year, month, i));
    }

    return days;
  }, [viewDate]);

  const navigateMonth = (direction: -1 | 1) => {
    setViewDate((prev) => {
      const newDate = new Date(prev);
      newDate.setMonth(newDate.getMonth() + direction);
      return newDate;
    });
  };

  const selectDate = (date: Date) => {
    if (isDateDisabled(date)) return;
    onChange(date);
    setVisible(false);
  };

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}

      <TouchableOpacity
        style={[
          styles.trigger,
          { backgroundColor: colors.surface, borderColor: error ? colors.error : colors.border },
        ]}
        onPress={() => !disabled && setVisible(true)}
        disabled={disabled}
        activeOpacity={0.7}
      >
        <Calendar color={colors.textSecondary} size={20} />
        <Text
          style={[
            styles.triggerText,
            { color: value ? colors.text : colors.textSecondary },
          ]}
        >
          {value ? formatDate(value) : placeholder}
        </Text>
      </TouchableOpacity>

      {error && (
        <Text style={[styles.error, { color: colors.error }]}>{error}</Text>
      )}

      <Modal visible={visible} transparent animationType="fade">
        <View style={styles.overlay}>
          <View style={[styles.modal, { backgroundColor: colors.surface }]}>
            {/* Header */}
            <View style={styles.header}>
              <TouchableOpacity onPress={() => navigateMonth(-1)}>
                <ChevronLeft color={colors.text} size={24} />
              </TouchableOpacity>
              <Text style={[styles.headerTitle, { color: colors.text }]}>
                {MONTHS[viewDate.getMonth()]} {viewDate.getFullYear()}
              </Text>
              <TouchableOpacity onPress={() => navigateMonth(1)}>
                <ChevronRight color={colors.text} size={24} />
              </TouchableOpacity>
            </View>

            {/* Day names */}
            <View style={styles.daysHeader}>
              {DAYS.map((day) => (
                <Text
                  key={day}
                  style={[styles.dayName, { color: colors.textSecondary }]}
                >
                  {day}
                </Text>
              ))}
            </View>

            {/* Calendar grid */}
            <View style={styles.calendarGrid}>
              {calendarDays.map((date, index) => {
                if (!date) {
                  return <View key={`empty-${index}`} style={styles.dayCell} />;
                }

                const isSelected = value && isSameDay(date, value);
                const isDisabled = isDateDisabled(date);
                const today = isToday(date);

                return (
                  <TouchableOpacity
                    key={date.toISOString()}
                    style={[
                      styles.dayCell,
                      isSelected && { backgroundColor: colors.primary },
                      today && !isSelected && { borderColor: colors.primary, borderWidth: 1 },
                    ]}
                    onPress={() => selectDate(date)}
                    disabled={isDisabled}
                  >
                    <Text
                      style={[
                        styles.dayText,
                        { color: isSelected ? '#FFF' : isDisabled ? colors.border : colors.text },
                      ]}
                    >
                      {date.getDate()}
                    </Text>
                  </TouchableOpacity>
                );
              })}
            </View>

            {/* Footer */}
            <View style={styles.footer}>
              <TouchableOpacity
                style={[styles.footerButton, { borderColor: colors.border }]}
                onPress={() => setVisible(false)}
              >
                <Text style={{ color: colors.text }}>Cancelar</Text>
              </TouchableOpacity>
              <TouchableOpacity
                style={[styles.footerButton, { backgroundColor: colors.primary }]}
                onPress={() => {
                  onChange(new Date());
                  setVisible(false);
                }}
              >
                <Text style={{ color: '#FFF' }}>Hoje</Text>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </Modal>
    </View>
  );
}

// TimePicker Component
interface TimePickerProps {
  value?: Date;
  onChange: (date: Date) => void;
  placeholder?: string;
  label?: string;
  disabled?: boolean;
  minuteStep?: number;
  format24h?: boolean;
  style?: ViewStyle;
}

export function TimePicker({
  value,
  onChange,
  placeholder = 'Selecionar hora',
  label,
  disabled = false,
  minuteStep = 5,
  format24h = true,
  style,
}: TimePickerProps) {
  const { colors } = useTheme();
  const [visible, setVisible] = useState(false);
  const [selectedHour, setSelectedHour] = useState(value?.getHours() || 12);
  const [selectedMinute, setSelectedMinute] = useState(value?.getMinutes() || 0);

  const hours = Array.from({ length: format24h ? 24 : 12 }, (_, i) => i);
  const minutes = Array.from({ length: 60 / minuteStep }, (_, i) => i * minuteStep);

  const formatTime = (date: Date) => {
    const hours = date.getHours().toString().padStart(2, '0');
    const mins = date.getMinutes().toString().padStart(2, '0');
    return `${hours}:${mins}`;
  };

  const confirmTime = () => {
    const newDate = value ? new Date(value) : new Date();
    newDate.setHours(selectedHour, selectedMinute, 0, 0);
    onChange(newDate);
    setVisible(false);
  };

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}

      <TouchableOpacity
        style={[
          styles.trigger,
          { backgroundColor: colors.surface, borderColor: colors.border },
        ]}
        onPress={() => !disabled && setVisible(true)}
        disabled={disabled}
        activeOpacity={0.7}
      >
        <Clock color={colors.textSecondary} size={20} />
        <Text
          style={[
            styles.triggerText,
            { color: value ? colors.text : colors.textSecondary },
          ]}
        >
          {value ? formatTime(value) : placeholder}
        </Text>
      </TouchableOpacity>

      <Modal visible={visible} transparent animationType="fade">
        <View style={styles.overlay}>
          <View style={[styles.modal, { backgroundColor: colors.surface }]}>
            <Text style={[styles.headerTitle, { color: colors.text, marginBottom: 16 }]}>
              Selecionar Hora
            </Text>

            <View style={styles.timePickerContainer}>
              {/* Hours */}
              <ScrollView style={styles.timeColumn} showsVerticalScrollIndicator={false}>
                {hours.map((hour) => (
                  <TouchableOpacity
                    key={hour}
                    style={[
                      styles.timeOption,
                      selectedHour === hour && { backgroundColor: colors.primary },
                    ]}
                    onPress={() => setSelectedHour(hour)}
                  >
                    <Text
                      style={[
                        styles.timeOptionText,
                        { color: selectedHour === hour ? '#FFF' : colors.text },
                      ]}
                    >
                      {hour.toString().padStart(2, '0')}
                    </Text>
                  </TouchableOpacity>
                ))}
              </ScrollView>

              <Text style={[styles.timeSeparator, { color: colors.text }]}>:</Text>

              {/* Minutes */}
              <ScrollView style={styles.timeColumn} showsVerticalScrollIndicator={false}>
                {minutes.map((minute) => (
                  <TouchableOpacity
                    key={minute}
                    style={[
                      styles.timeOption,
                      selectedMinute === minute && { backgroundColor: colors.primary },
                    ]}
                    onPress={() => setSelectedMinute(minute)}
                  >
                    <Text
                      style={[
                        styles.timeOptionText,
                        { color: selectedMinute === minute ? '#FFF' : colors.text },
                      ]}
                    >
                      {minute.toString().padStart(2, '0')}
                    </Text>
                  </TouchableOpacity>
                ))}
              </ScrollView>
            </View>

            <View style={styles.footer}>
              <TouchableOpacity
                style={[styles.footerButton, { borderColor: colors.border }]}
                onPress={() => setVisible(false)}
              >
                <Text style={{ color: colors.text }}>Cancelar</Text>
              </TouchableOpacity>
              <TouchableOpacity
                style={[styles.footerButton, { backgroundColor: colors.primary }]}
                onPress={confirmTime}
              >
                <Text style={{ color: '#FFF' }}>Confirmar</Text>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </Modal>
    </View>
  );
}

// DateTimePicker - Combined
interface DateTimePickerProps {
  value?: Date;
  onChange: (date: Date) => void;
  label?: string;
  disabled?: boolean;
  style?: ViewStyle;
}

export function DateTimePicker({
  value,
  onChange,
  label,
  disabled = false,
  style,
}: DateTimePickerProps) {
  const { colors } = useTheme();
  const [tempDate, setTempDate] = useState<Date | undefined>(value);

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}
      <View style={styles.dateTimeRow}>
        <View style={{ flex: 1 }}>
          <DatePicker
            value={tempDate}
            onChange={(date) => {
              const newDate = new Date(date);
              if (tempDate) {
                newDate.setHours(tempDate.getHours(), tempDate.getMinutes());
              }
              setTempDate(newDate);
              onChange(newDate);
            }}
            disabled={disabled}
          />
        </View>
        <View style={{ flex: 1, marginLeft: 8 }}>
          <TimePicker
            value={tempDate}
            onChange={(date) => {
              const newDate = tempDate ? new Date(tempDate) : new Date();
              newDate.setHours(date.getHours(), date.getMinutes());
              setTempDate(newDate);
              onChange(newDate);
            }}
            disabled={disabled}
          />
        </View>
      </View>
    </View>
  );
}

// DateRangePicker
interface DateRangePickerProps {
  startDate?: Date;
  endDate?: Date;
  onChangeStart: (date: Date) => void;
  onChangeEnd: (date: Date) => void;
  label?: string;
  disabled?: boolean;
  style?: ViewStyle;
}

export function DateRangePicker({
  startDate,
  endDate,
  onChangeStart,
  onChangeEnd,
  label,
  disabled = false,
  style,
}: DateRangePickerProps) {
  const { colors } = useTheme();

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}
      <View style={styles.dateTimeRow}>
        <View style={{ flex: 1 }}>
          <DatePicker
            value={startDate}
            onChange={onChangeStart}
            maxDate={endDate}
            placeholder="Data inicio"
            disabled={disabled}
          />
        </View>
        <Text style={[styles.rangeSeparator, { color: colors.textSecondary }]}>
          ate
        </Text>
        <View style={{ flex: 1 }}>
          <DatePicker
            value={endDate}
            onChange={onChangeEnd}
            minDate={startDate}
            placeholder="Data fim"
            disabled={disabled}
          />
        </View>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  label: {
    fontSize: 14,
    fontWeight: '500',
    marginBottom: 6,
  },
  trigger: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingHorizontal: 12,
    paddingVertical: 14,
    borderRadius: 8,
    borderWidth: 1,
    gap: 10,
  },
  triggerText: {
    fontSize: 15,
    flex: 1,
  },
  error: {
    fontSize: 12,
    marginTop: 4,
  },
  overlay: {
    flex: 1,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    justifyContent: 'center',
    alignItems: 'center',
    padding: 24,
  },
  modal: {
    width: '100%',
    maxWidth: 340,
    borderRadius: 16,
    padding: 16,
  },
  header: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: 16,
  },
  headerTitle: {
    fontSize: 16,
    fontWeight: '600',
  },
  daysHeader: {
    flexDirection: 'row',
    marginBottom: 8,
  },
  dayName: {
    flex: 1,
    textAlign: 'center',
    fontSize: 12,
    fontWeight: '500',
  },
  calendarGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
  },
  dayCell: {
    width: '14.28%',
    aspectRatio: 1,
    justifyContent: 'center',
    alignItems: 'center',
    borderRadius: 20,
  },
  dayText: {
    fontSize: 14,
  },
  footer: {
    flexDirection: 'row',
    gap: 12,
    marginTop: 16,
  },
  footerButton: {
    flex: 1,
    paddingVertical: 12,
    borderRadius: 8,
    alignItems: 'center',
    borderWidth: 1,
    borderColor: 'transparent',
  },
  timePickerContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    height: 200,
  },
  timeColumn: {
    width: 60,
    maxHeight: 200,
  },
  timeOption: {
    paddingVertical: 12,
    paddingHorizontal: 8,
    borderRadius: 8,
    marginVertical: 2,
    alignItems: 'center',
  },
  timeOptionText: {
    fontSize: 18,
    fontWeight: '500',
  },
  timeSeparator: {
    fontSize: 24,
    fontWeight: '600',
    marginHorizontal: 16,
  },
  dateTimeRow: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  rangeSeparator: {
    marginHorizontal: 8,
    fontSize: 14,
  },
});
