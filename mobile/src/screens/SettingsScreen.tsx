/**
 * Settings Screen - Configuracoes do app
 * Issue #368: Multi-Tenant, White Label, User Mode
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
  Modal,
  Image,
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
  Building2,
  Layers,
  BookOpen,
  X,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { useAuth } from '../context/AuthContext';
import { useUserMode } from '../context/UserModeContext';

export function SettingsScreen() {
  const { colors, isDark, toggleTheme, branding } = useTheme();
  const { user, signOut, biometricAvailable, enableBiometric, currentTenant, availableTenants, switchTenant } = useAuth();
  const { mode, setMode, resetTour } = useUserMode();

  const [notifications, setNotifications] = useState(true);
  const [biometricEnabled, setBiometricEnabled] = useState(false);
  const [showTenantModal, setShowTenantModal] = useState(false);

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

        {/* User Info with Tenant */}
        <View style={styles.userCard}>
          <View style={styles.userAvatar}>
            <Text style={styles.userInitial}>
              {user?.username?.charAt(0).toUpperCase() || 'U'}
            </Text>
          </View>
          <View style={styles.userInfo}>
            <Text style={styles.userName}>{user?.username || 'Usuario'}</Text>
            <Text style={styles.userEmail}>{user?.email || ''}</Text>
            {currentTenant && (
              <View style={styles.tenantBadge}>
                <Building2 color={colors.primary} size={12} />
                <Text style={styles.tenantText}>{currentTenant.name}</Text>
              </View>
            )}
          </View>
        </View>

        {/* Tenant Selector - Multi-Tenant */}
        {availableTenants.length > 1 && (
          <>
            <Text style={styles.sectionTitle}>Organizacao</Text>
            <View style={styles.section}>
              <TouchableOpacity
                style={styles.settingItem}
                onPress={() => setShowTenantModal(true)}
              >
                <View style={styles.settingLeft}>
                  <Building2 color={colors.text} size={22} />
                  <Text style={styles.settingLabel}>Trocar Organizacao</Text>
                </View>
                <View style={styles.settingRight}>
                  <Text style={styles.settingValue}>
                    {currentTenant?.name || 'Selecionar'}
                  </Text>
                  <ChevronRight color={colors.textSecondary} size={20} />
                </View>
              </TouchableOpacity>
            </View>
          </>
        )}

        {/* User Mode - Basico/Avancado */}
        <Text style={styles.sectionTitle}>Modo de Uso</Text>
        <View style={styles.section}>
          <View style={styles.settingItem}>
            <View style={styles.settingLeft}>
              <Layers color={colors.text} size={22} />
              <View>
                <Text style={styles.settingLabel}>Modo Avancado</Text>
                <Text style={styles.settingHint}>
                  {mode === 'basic' ? 'Termos simplificados' : 'Termos tecnicos'}
                </Text>
              </View>
            </View>
            <Switch
              value={mode === 'advanced'}
              onValueChange={(value) => setMode(value ? 'advanced' : 'basic')}
              trackColor={{ false: colors.border, true: colors.primary }}
              thumbColor="#FFF"
            />
          </View>

          <TouchableOpacity
            style={styles.settingItem}
            onPress={() => {
              resetTour();
              Alert.alert('Tour Reiniciado', 'O tour de introducao sera exibido novamente.');
            }}
          >
            <View style={styles.settingLeft}>
              <BookOpen color={colors.text} size={22} />
              <Text style={styles.settingLabel}>Reiniciar Tour</Text>
            </View>
            <ChevronRight color={colors.textSecondary} size={20} />
          </TouchableOpacity>
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

        {/* Footer - White Label */}
        <View style={styles.footer}>
          {branding.logoUrl && (
            <Image
              source={{ uri: branding.logoUrl }}
              style={styles.footerLogo}
              resizeMode="contain"
            />
          )}
          <Text style={styles.footerText}>
            {branding.companyName} v1.0.0
          </Text>
          <Text style={styles.footerText}>
            {new Date().getFullYear()}
          </Text>
        </View>
      </ScrollView>

      {/* Tenant Selection Modal */}
      <Modal
        visible={showTenantModal}
        animationType="slide"
        transparent
        onRequestClose={() => setShowTenantModal(false)}
      >
        <View style={styles.modalOverlay}>
          <View style={styles.modalContent}>
            <View style={styles.modalHeader}>
              <Text style={styles.modalTitle}>Selecionar Organizacao</Text>
              <TouchableOpacity onPress={() => setShowTenantModal(false)}>
                <X color={colors.text} size={24} />
              </TouchableOpacity>
            </View>

            {availableTenants.map((tenant) => (
              <TouchableOpacity
                key={tenant.tenant_id}
                style={[
                  styles.tenantOption,
                  currentTenant?.tenant_id === tenant.tenant_id && styles.tenantOptionActive,
                ]}
                onPress={async () => {
                  await switchTenant(tenant.tenant_id);
                  setShowTenantModal(false);
                }}
              >
                <Building2
                  color={
                    currentTenant?.tenant_id === tenant.tenant_id
                      ? colors.primary
                      : colors.textSecondary
                  }
                  size={20}
                />
                <View style={styles.tenantOptionInfo}>
                  <Text
                    style={[
                      styles.tenantOptionName,
                      currentTenant?.tenant_id === tenant.tenant_id && {
                        color: colors.primary,
                      },
                    ]}
                  >
                    {tenant.name}
                  </Text>
                  <Text style={styles.tenantOptionSlug}>{tenant.slug}</Text>
                </View>
                {currentTenant?.tenant_id === tenant.tenant_id && (
                  <View style={styles.tenantCheck}>
                    <Text style={{ color: '#FFF' }}>✓</Text>
                  </View>
                )}
              </TouchableOpacity>
            ))}
          </View>
        </View>
      </Modal>
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
    tenantBadge: {
      flexDirection: 'row',
      alignItems: 'center',
      marginTop: 6,
      backgroundColor: colors.primary + '15',
      paddingHorizontal: 8,
      paddingVertical: 4,
      borderRadius: 6,
      gap: 4,
      alignSelf: 'flex-start',
    },
    tenantText: {
      fontSize: 12,
      color: colors.primary,
      fontWeight: '500',
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
    settingHint: {
      fontSize: 12,
      color: colors.textSecondary,
      marginTop: 2,
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
    footerLogo: {
      width: 120,
      height: 40,
      marginBottom: 8,
    },
    modalOverlay: {
      flex: 1,
      backgroundColor: 'rgba(0,0,0,0.5)',
      justifyContent: 'flex-end',
    },
    modalContent: {
      backgroundColor: colors.surface,
      borderTopLeftRadius: 24,
      borderTopRightRadius: 24,
      padding: 24,
    },
    modalHeader: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: 20,
    },
    modalTitle: {
      fontSize: 20,
      fontWeight: '600',
      color: colors.text,
    },
    tenantOption: {
      flexDirection: 'row',
      alignItems: 'center',
      padding: 16,
      borderRadius: 12,
      marginBottom: 8,
      backgroundColor: colors.background,
      gap: 12,
    },
    tenantOptionActive: {
      backgroundColor: colors.primary + '15',
      borderWidth: 1,
      borderColor: colors.primary,
    },
    tenantOptionInfo: {
      flex: 1,
    },
    tenantOptionName: {
      fontSize: 16,
      fontWeight: '500',
      color: colors.text,
    },
    tenantOptionSlug: {
      fontSize: 12,
      color: colors.textSecondary,
      marginTop: 2,
    },
    tenantCheck: {
      width: 24,
      height: 24,
      borderRadius: 12,
      backgroundColor: colors.primary,
      justifyContent: 'center',
      alignItems: 'center',
    },
  });
