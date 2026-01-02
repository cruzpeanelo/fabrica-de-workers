/**
 * AlertDialog Component - Issue #428
 * Dialogos de confirmacao e alertas
 */

import React, { useState } from 'react';
import {
  View,
  Text,
  Modal,
  TouchableOpacity,
  TextInput,
  StyleSheet,
  ViewStyle,
  Animated,
} from 'react-native';
import { AlertTriangle, CheckCircle, Info, XCircle } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

type AlertType = 'info' | 'success' | 'warning' | 'error' | 'confirm';

interface AlertButton {
  text: string;
  onPress?: () => void;
  style?: 'default' | 'cancel' | 'destructive';
}

interface AlertDialogProps {
  visible: boolean;
  onClose: () => void;
  title: string;
  message?: string;
  type?: AlertType;
  buttons?: AlertButton[];
  showIcon?: boolean;
  style?: ViewStyle;
}

export function AlertDialog({
  visible,
  onClose,
  title,
  message,
  type = 'info',
  buttons = [{ text: 'OK', style: 'default' }],
  showIcon = true,
  style,
}: AlertDialogProps) {
  const { colors } = useTheme();

  const getIcon = () => {
    const iconProps = { size: 48 };
    switch (type) {
      case 'success':
        return <CheckCircle color={colors.success} {...iconProps} />;
      case 'warning':
        return <AlertTriangle color={colors.warning} {...iconProps} />;
      case 'error':
        return <XCircle color={colors.error} {...iconProps} />;
      case 'confirm':
        return <AlertTriangle color={colors.warning} {...iconProps} />;
      default:
        return <Info color={colors.primary} {...iconProps} />;
    }
  };

  const getButtonStyle = (buttonStyle?: 'default' | 'cancel' | 'destructive') => {
    switch (buttonStyle) {
      case 'cancel':
        return {
          backgroundColor: colors.border,
          textColor: colors.text,
        };
      case 'destructive':
        return {
          backgroundColor: colors.error,
          textColor: '#FFFFFF',
        };
      default:
        return {
          backgroundColor: colors.primary,
          textColor: '#FFFFFF',
        };
    }
  };

  const handleButtonPress = (button: AlertButton) => {
    button.onPress?.();
    onClose();
  };

  return (
    <Modal
      visible={visible}
      transparent
      animationType="fade"
      onRequestClose={onClose}
    >
      <View style={styles.overlay}>
        <View style={[styles.dialog, { backgroundColor: colors.surface }, style]}>
          {showIcon && <View style={styles.iconContainer}>{getIcon()}</View>}

          <Text style={[styles.title, { color: colors.text }]}>{title}</Text>

          {message && (
            <Text style={[styles.message, { color: colors.textSecondary }]}>
              {message}
            </Text>
          )}

          <View style={[styles.buttonsContainer, buttons.length === 1 && styles.singleButton]}>
            {buttons.map((button, index) => {
              const buttonStyle = getButtonStyle(button.style);
              return (
                <TouchableOpacity
                  key={index}
                  style={[
                    styles.button,
                    { backgroundColor: buttonStyle.backgroundColor },
                    buttons.length > 1 && { flex: 1 },
                  ]}
                  onPress={() => handleButtonPress(button)}
                  activeOpacity={0.8}
                >
                  <Text style={[styles.buttonText, { color: buttonStyle.textColor }]}>
                    {button.text}
                  </Text>
                </TouchableOpacity>
              );
            })}
          </View>
        </View>
      </View>
    </Modal>
  );
}

// Confirm Dialog - simplified confirm/cancel
interface ConfirmDialogProps {
  visible: boolean;
  onConfirm: () => void;
  onCancel: () => void;
  title: string;
  message?: string;
  confirmText?: string;
  cancelText?: string;
  destructive?: boolean;
}

export function ConfirmDialog({
  visible,
  onConfirm,
  onCancel,
  title,
  message,
  confirmText = 'Confirmar',
  cancelText = 'Cancelar',
  destructive = false,
}: ConfirmDialogProps) {
  return (
    <AlertDialog
      visible={visible}
      onClose={onCancel}
      title={title}
      message={message}
      type="confirm"
      buttons={[
        { text: cancelText, style: 'cancel', onPress: onCancel },
        { text: confirmText, style: destructive ? 'destructive' : 'default', onPress: onConfirm },
      ]}
    />
  );
}

// Prompt Dialog - with text input
interface PromptDialogProps {
  visible: boolean;
  onSubmit: (value: string) => void;
  onCancel: () => void;
  title: string;
  message?: string;
  placeholder?: string;
  defaultValue?: string;
  submitText?: string;
  cancelText?: string;
  inputType?: 'text' | 'password' | 'email' | 'number';
}

export function PromptDialog({
  visible,
  onSubmit,
  onCancel,
  title,
  message,
  placeholder = '',
  defaultValue = '',
  submitText = 'Enviar',
  cancelText = 'Cancelar',
  inputType = 'text',
}: PromptDialogProps) {
  const { colors } = useTheme();
  const [value, setValue] = useState(defaultValue);

  const handleSubmit = () => {
    onSubmit(value);
    setValue('');
  };

  const handleCancel = () => {
    onCancel();
    setValue('');
  };

  return (
    <Modal
      visible={visible}
      transparent
      animationType="fade"
      onRequestClose={handleCancel}
    >
      <View style={styles.overlay}>
        <View style={[styles.dialog, { backgroundColor: colors.surface }]}>
          <Text style={[styles.title, { color: colors.text }]}>{title}</Text>

          {message && (
            <Text style={[styles.message, { color: colors.textSecondary }]}>
              {message}
            </Text>
          )}

          <TextInput
            style={[
              styles.input,
              {
                backgroundColor: colors.background,
                borderColor: colors.border,
                color: colors.text,
              },
            ]}
            value={value}
            onChangeText={setValue}
            placeholder={placeholder}
            placeholderTextColor={colors.textSecondary}
            secureTextEntry={inputType === 'password'}
            keyboardType={inputType === 'email' ? 'email-address' : inputType === 'number' ? 'numeric' : 'default'}
            autoFocus
          />

          <View style={styles.buttonsContainer}>
            <TouchableOpacity
              style={[styles.button, { backgroundColor: colors.border, flex: 1 }]}
              onPress={handleCancel}
            >
              <Text style={[styles.buttonText, { color: colors.text }]}>
                {cancelText}
              </Text>
            </TouchableOpacity>
            <TouchableOpacity
              style={[styles.button, { backgroundColor: colors.primary, flex: 1 }]}
              onPress={handleSubmit}
            >
              <Text style={[styles.buttonText, { color: '#FFFFFF' }]}>
                {submitText}
              </Text>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    </Modal>
  );
}

// Delete Confirm Dialog - pre-styled for delete actions
interface DeleteDialogProps {
  visible: boolean;
  onConfirm: () => void;
  onCancel: () => void;
  itemName?: string;
}

export function DeleteDialog({
  visible,
  onConfirm,
  onCancel,
  itemName,
}: DeleteDialogProps) {
  return (
    <AlertDialog
      visible={visible}
      onClose={onCancel}
      title="Confirmar exclusao"
      message={
        itemName
          ? `Tem certeza que deseja excluir "${itemName}"? Esta acao nao pode ser desfeita.`
          : 'Tem certeza que deseja excluir este item? Esta acao nao pode ser desfeita.'
      }
      type="error"
      buttons={[
        { text: 'Cancelar', style: 'cancel', onPress: onCancel },
        { text: 'Excluir', style: 'destructive', onPress: onConfirm },
      ]}
    />
  );
}

const styles = StyleSheet.create({
  overlay: {
    flex: 1,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    justifyContent: 'center',
    alignItems: 'center',
    padding: 24,
  },
  dialog: {
    width: '100%',
    maxWidth: 320,
    borderRadius: 16,
    padding: 24,
    alignItems: 'center',
  },
  iconContainer: {
    marginBottom: 16,
  },
  title: {
    fontSize: 18,
    fontWeight: '600',
    textAlign: 'center',
    marginBottom: 8,
  },
  message: {
    fontSize: 14,
    textAlign: 'center',
    lineHeight: 20,
    marginBottom: 24,
  },
  buttonsContainer: {
    flexDirection: 'row',
    gap: 12,
    width: '100%',
  },
  singleButton: {
    justifyContent: 'center',
  },
  button: {
    paddingVertical: 14,
    paddingHorizontal: 24,
    borderRadius: 8,
    alignItems: 'center',
    minWidth: 100,
  },
  buttonText: {
    fontSize: 15,
    fontWeight: '600',
  },
  input: {
    width: '100%',
    height: 48,
    borderWidth: 1,
    borderRadius: 8,
    paddingHorizontal: 16,
    fontSize: 15,
    marginBottom: 24,
  },
});
