/**
 * Toast Component - Issue #426
 * Notificacoes toast/snackbar
 */

import React, { useEffect, useRef, useState, createContext, useContext } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  StyleSheet,
  Animated,
  Dimensions,
} from 'react-native';
import { CheckCircle, AlertCircle, AlertTriangle, Info, X } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type ToastType = 'success' | 'error' | 'warning' | 'info';
type ToastPosition = 'top' | 'bottom';

interface ToastConfig {
  id: string;
  message: string;
  type: ToastType;
  duration?: number;
  action?: {
    label: string;
    onPress: () => void;
  };
}

interface ToastProps extends ToastConfig {
  position: ToastPosition;
  onHide: () => void;
}

function Toast({
  message,
  type,
  duration = 3000,
  action,
  position,
  onHide,
}: ToastProps) {
  const { colors } = useTheme();
  const translateY = useRef(new Animated.Value(position === 'top' ? -100 : 100)).current;
  const opacity = useRef(new Animated.Value(0)).current;

  useEffect(() => {
    // Animate in
    Animated.parallel([
      Animated.spring(translateY, {
        toValue: 0,
        useNativeDriver: true,
      }),
      Animated.timing(opacity, {
        toValue: 1,
        duration: 200,
        useNativeDriver: true,
      }),
    ]).start();

    // Auto dismiss
    if (duration > 0) {
      const timer = setTimeout(() => {
        hideToast();
      }, duration);
      return () => clearTimeout(timer);
    }
  }, []);

  const hideToast = () => {
    Animated.parallel([
      Animated.timing(translateY, {
        toValue: position === 'top' ? -100 : 100,
        duration: 200,
        useNativeDriver: true,
      }),
      Animated.timing(opacity, {
        toValue: 0,
        duration: 200,
        useNativeDriver: true,
      }),
    ]).start(() => onHide());
  };

  const getTypeConfig = () => {
    switch (type) {
      case 'success':
        return { icon: CheckCircle, color: colors.success, bg: colors.success + '15' };
      case 'error':
        return { icon: AlertCircle, color: colors.error, bg: colors.error + '15' };
      case 'warning':
        return { icon: AlertTriangle, color: colors.warning, bg: colors.warning + '15' };
      case 'info':
      default:
        return { icon: Info, color: colors.primary, bg: colors.primary + '15' };
    }
  };

  const config = getTypeConfig();
  const Icon = config.icon;

  return (
    <Animated.View
      style={[
        styles.toast,
        {
          backgroundColor: colors.surface,
          borderLeftColor: config.color,
          transform: [{ translateY }],
          opacity,
        },
        position === 'top' ? styles.toastTop : styles.toastBottom,
      ]}
    >
      <View style={[styles.iconContainer, { backgroundColor: config.bg }]}>
        <Icon color={config.color} size={20} />
      </View>

      <Text style={[styles.message, { color: colors.text }]} numberOfLines={2}>
        {message}
      </Text>

      {action && (
        <TouchableOpacity
          onPress={() => {
            action.onPress();
            hideToast();
          }}
          style={styles.actionButton}
        >
          <Text style={[styles.actionText, { color: config.color }]}>
            {action.label}
          </Text>
        </TouchableOpacity>
      )}

      <TouchableOpacity onPress={hideToast} style={styles.closeButton}>
        <X color={colors.textSecondary} size={18} />
      </TouchableOpacity>
    </Animated.View>
  );
}

// Toast Context for global usage
interface ToastContextValue {
  show: (config: Omit<ToastConfig, 'id'>) => void;
  success: (message: string, options?: Partial<ToastConfig>) => void;
  error: (message: string, options?: Partial<ToastConfig>) => void;
  warning: (message: string, options?: Partial<ToastConfig>) => void;
  info: (message: string, options?: Partial<ToastConfig>) => void;
}

const ToastContext = createContext<ToastContextValue | null>(null);

export function useToast() {
  const context = useContext(ToastContext);
  if (!context) {
    throw new Error('useToast must be used within ToastProvider');
  }
  return context;
}

interface ToastProviderProps {
  children: React.ReactNode;
  position?: ToastPosition;
  maxToasts?: number;
}

export function ToastProvider({
  children,
  position = 'bottom',
  maxToasts = 3,
}: ToastProviderProps) {
  const [toasts, setToasts] = useState<ToastConfig[]>([]);

  const show = (config: Omit<ToastConfig, 'id'>) => {
    const id = Date.now().toString();
    setToasts((prev) => {
      const newToasts = [...prev, { ...config, id }];
      return newToasts.slice(-maxToasts);
    });
  };

  const hide = (id: string) => {
    setToasts((prev) => prev.filter((t) => t.id !== id));
  };

  const value: ToastContextValue = {
    show,
    success: (message, options) => show({ ...options, message, type: 'success' }),
    error: (message, options) => show({ ...options, message, type: 'error' }),
    warning: (message, options) => show({ ...options, message, type: 'warning' }),
    info: (message, options) => show({ ...options, message, type: 'info' }),
  };

  return (
    <ToastContext.Provider value={value}>
      {children}
      <View
        style={[
          styles.container,
          position === 'top' ? styles.containerTop : styles.containerBottom,
        ]}
        pointerEvents="box-none"
      >
        {toasts.map((toast) => (
          <Toast
            key={toast.id}
            {...toast}
            position={position}
            onHide={() => hide(toast.id)}
          />
        ))}
      </View>
    </ToastContext.Provider>
  );
}

const { width } = Dimensions.get('window');

const styles = StyleSheet.create({
  container: {
    position: 'absolute',
    left: 16,
    right: 16,
    zIndex: 9999,
  },
  containerTop: {
    top: 60,
  },
  containerBottom: {
    bottom: 100,
  },
  toast: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 12,
    borderRadius: 12,
    borderLeftWidth: 4,
    marginBottom: 8,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.15,
    shadowRadius: 8,
    elevation: 5,
    gap: 12,
  },
  toastTop: {},
  toastBottom: {},
  iconContainer: {
    width: 36,
    height: 36,
    borderRadius: 18,
    justifyContent: 'center',
    alignItems: 'center',
  },
  message: {
    flex: 1,
    fontSize: 14,
    lineHeight: 20,
  },
  actionButton: {
    paddingHorizontal: 8,
  },
  actionText: {
    fontSize: 14,
    fontWeight: '600',
  },
  closeButton: {
    padding: 4,
  },
});

export { Toast };
