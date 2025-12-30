/**
 * Settings Screen - Configuracoes do app
 * Tema, notificacoes, biometria e logout
 */

import React, { useState } from 'react';
import {
  View,
  Text,
  ScrollView,
  StyleSheet,
  TouchableOpacity,
  Switch,
  Alert,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import {
  Moon,
  Bell,
  Fingerprint,
  Globe,
  HelpCircle,
  LogOut,
  ChevronRight,
  Shield,
  Info,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { useAuth } from '../context/AuthContext';

export function SettingsScreen() {
  const { colors, isDark, toggleTheme } = useTheme();
  const { user, signOut, biometricAvailable, enableBiometric } = useAuth();

  const [notifications, setNotifications] = useState(true);
  const [biometricEnabled, setBiometricEnabled] = useState(false);

  async function handleBiometricToggle() {
    if (!biometricEnabled) {
      const success = await enableBiometric();
      if (success) {
        setBiometricEnabled(true);
      }
    } else {
      setBiometricEnabled(false);
    }
  }

  function handleLogout() {
    Alert.alert(
      'Sair da Conta',
      'Tem certeza que deseja sair?',
      [
        { text: 'Cancelar', style: 'cancel' },
        {
          text: 'Sair',
          style: 'destructive',
          onPress: signOut,
        },
      ]
    );
  }

  const styles = createStyles(colors);

  return (
    <SafeAreaView style={styles.container} edges={['top']}>
      <ScrollView>
        {/* Header */}
        <View style={styles.header}>
          <Text style={styles.title}>Ajustes</Text>
        </View>

        {/* User Info */}
        <View style={styles.userCard}>
          <View style={styles.userAvatar}>
            <Text style={styles.userInitial}>
              {user?.username?.charAt(0).toUpperCase() || 'U'}
            </Text>
          </View>
          <View style={styles.userInfo}>
            <Text style={styles.userName}>{user?.username || 'Usuario'}</Text>
            <Text style={styles.userEmail}>{user?.email || ''}</Text>
          </View>
        </View>

        {/* Aparencia */}
        <Text style={styles.sectionTitle}>Aparencia</Text>
        <View style={styles.section}>
          <View style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Moon color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Modo Escuro</Text>
            </View>
            <Switch
              value={isDark}
              onValueChange={toggleTheme}
              trackColor={{ false: colors.border, true: colors.primary }}
              thumbColor="#FFF"
            />
          </View>
        </View>

        {/* Notificacoes */}
        <Text style={styles.sectionTitle}>Notificacoes</Text>
        <View style={styles.section}>
          <View style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Bell color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Notificacoes Push</Text>
            </View>
            <Switch
              value={notifications}
              onValueChange={setNotifications}
              trackColor={{ false: colors.border, true: colors.primary }}
              thumbColor="#FFF"
            />
          </View>
        </View>

        {/* Seguranca */}
        <Text style={styles.sectionTitle}>Seguranca</Text>
        <View style={styles.section}>
          {biometricAvailable && (
            <View style={styles.settingItem}>
              <View style={styles.settingLeft}>
                <Fingerprint color={colors.text} size={22} />
                <Text style={styles.settingLabel}>Login Biometrico</Text>
              </View>
              <Switch
                value={biometricEnabled}
                onValueChange={handleBiometricToggle}
                trackColor={{ false: colors.border, true: colors.primary }}
                thumbColor="#FFF"
              />
            </View>
          )}

          <TouchableOpacity style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Shield color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Alterar Senha</Text>
            </View>
            <ChevronRight color={colors.textSecondary} size={20} />
          </TouchableOpacity>
        </View>

        {/* Geral */}
        <Text style={styles.sectionTitle}>Geral</Text>
        <View style={styles.section}>
          <TouchableOpacity style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Globe color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Idioma</Text>
            </View>
            <View style={styles.settingRight}>
              <Text style={styles.settingValue}>Portugues</Text>
              <ChevronRight color={colors.textSecondary} size={20} />
            </View>
          </TouchableOpacity>

          <TouchableOpacity style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <HelpCircle color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Ajuda e Suporte</Text>
            </View>
            <ChevronRight color={colors.textSecondary} size={20} />
          </TouchableOpacity>

          <TouchableOpacity style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Info color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Sobre o App</Text>
            </View>
            <View style={styles.settingRight}>
              <Text style={styles.settingValue}>v1.0.0</Text>
              <ChevronRight color={colors.textSecondary} size={20} />
            </View>
          </TouchableOpacity>
        </View>

        {/* Logout */}
        <TouchableOpacity style={styles.logoutButton} onPress={handleLogout}>
          <LogOut color={colors.error} size={22} />
          <Text style={[styles.logoutText, { color: colors.error }]}>
            Sair da Conta
          </Text>
        </TouchableOpacity>

        {/* Footer */}
        <View style={styles.footer}>
          <Text style={styles.footerText}>
            Fabrica de Agentes v1.0.0
          </Text>
          <Text style={styles.footerText}>
            Belgo Arames - 2024
          </Text>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}

const createStyles = (colors: any) =>
  StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    header: {
      padding: 20,
      backgroundColor: colors.primary,
    },
    title: {
      fontSize: 24,
      fontWeight: 'bold',
      color: '#FFF',
    },
    userCard: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: colors.surface,
      padding: 16,
      margin: 16,
      borderRadius: 12,
      gap: 12,
    },
    userAvatar: {
      width: 56,
      height: 56,
      borderRadius: 28,
      backgroundColor: colors.primary,
      justifyContent: 'center',
      alignItems: 'center',
    },
    userInitial: {
      fontSize: 24,
      fontWeight: 'bold',
      color: '#FFF',
    },
    userInfo: {
      flex: 1,
    },
    userName: {
      fontSize: 18,
      fontWeight: '600',
      color: colors.text,
    },
    userEmail: {
      fontSize: 14,
      color: colors.textSecondary,
      marginTop: 2,
    },
    sectionTitle: {
      fontSize: 13,
      fontWeight: '600',
      color: colors.textSecondary,
      textTransform: 'uppercase',
      marginLeft: 16,
      marginTop: 24,
      marginBottom: 8,
    },
    section: {
      backgroundColor: colors.surface,
      marginHorizontal: 16,
      borderRadius: 12,
      overflow: 'hidden',
    },
    settingItem: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'space-between',
      padding: 16,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    settingLeft: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 12,
    },
    settingRight: {
      flexDirection: 'row',
      alignItems: 'center',
      gap: 8,
    },
    settingLabel: {
      fontSize: 15,
      color: colors.text,
    },
    settingValue: {
      fontSize: 14,
      color: colors.textSecondary,
    },
    logoutButton: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'center',
      backgroundColor: colors.surface,
      margin: 16,
      padding: 16,
      borderRadius: 12,
      gap: 8,
    },
    logoutText: {
      fontSize: 16,
      fontWeight: '600',
    },
    footer: {
      alignItems: 'center',
      padding: 24,
    },
    footerText: {
      fontSize: 12,
      color: colors.textSecondary,
    },
  });
