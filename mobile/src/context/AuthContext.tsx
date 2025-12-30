/**
 * Auth Context - Gerenciamento de autenticacao
 * Suporta login biometrico, JWT, Multi-Tenant e Refresh Token
 * Issue #368: Implementacao Multi-Tenant Enterprise
 */

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as LocalAuthentication from 'expo-local-authentication';
import { api } from '../services/api';

// Interface Multi-Tenant
export interface Tenant {
  tenant_id: string;
  name: string;
  slug: string;
  branding?: TenantBranding;
}

export interface TenantBranding {
  primary_color: string;
  secondary_color: string;
  logo_url?: string;
  favicon_url?: string;
  company_name: string;
}

interface User {
  id: string;
  username: string;
  email: string;
  role: string;
  tenant_id?: string;
  tenants?: Tenant[];
  force_password_change?: boolean;
}

interface AuthContextData {
  user: User | null;
  currentTenant: Tenant | null;
  availableTenants: Tenant[];
  isLoading: boolean;
  isAuthenticated: boolean;
  biometricAvailable: boolean;
  signIn: (username: string, password: string, tenantId?: string) => Promise<void>;
  signInWithBiometric: () => Promise<void>;
  signOut: () => Promise<void>;
  enableBiometric: () => Promise<boolean>;
  switchTenant: (tenantId: string) => Promise<void>;
  refreshToken: () => Promise<boolean>;
}

const AuthContext = createContext<AuthContextData>({} as AuthContextData);

const TOKEN_KEY = '@FabricaAgentes:token';
const REFRESH_TOKEN_KEY = '@FabricaAgentes:refreshToken';
const USER_KEY = '@FabricaAgentes:user';
const TENANT_KEY = '@FabricaAgentes:tenant';
const BIOMETRIC_KEY = '@FabricaAgentes:biometric';

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [currentTenant, setCurrentTenant] = useState<Tenant | null>(null);
  const [availableTenants, setAvailableTenants] = useState<Tenant[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [biometricAvailable, setBiometricAvailable] = useState(false);

  useEffect(() => {
    loadStoredData();
    checkBiometricAvailability();
  }, []);

  async function loadStoredData() {
    try {
      const [storedToken, storedUser, storedTenant] = await Promise.all([
        AsyncStorage.getItem(TOKEN_KEY),
        AsyncStorage.getItem(USER_KEY),
        AsyncStorage.getItem(TENANT_KEY),
      ]);

      if (storedToken && storedUser) {
        const userData = JSON.parse(storedUser);
        api.defaults.headers.common['Authorization'] = `Bearer ${storedToken}`;
        setUser(userData);

        // Carregar tenant
        if (storedTenant) {
          const tenantData = JSON.parse(storedTenant);
          setCurrentTenant(tenantData);
          api.defaults.headers.common['X-Tenant-ID'] = tenantData.tenant_id;
        }

        // Carregar lista de tenants do usuario
        if (userData.tenants) {
          setAvailableTenants(userData.tenants);
        }
      }
    } catch (error) {
      console.error('Erro ao carregar dados:', error);
    } finally {
      setIsLoading(false);
    }
  }

  async function checkBiometricAvailability() {
    const compatible = await LocalAuthentication.hasHardwareAsync();
    const enrolled = await LocalAuthentication.isEnrolledAsync();
    setBiometricAvailable(compatible && enrolled);
  }

  async function signIn(username: string, password: string, tenantId?: string) {
    try {
      setIsLoading(true);

      // Incluir tenant_id se fornecido
      const payload: any = { username, password };
      if (tenantId) {
        payload.tenant_id = tenantId;
      }

      const response = await api.post('/api/auth/login', payload);
      const { access_token, refresh_token, user: userData, tenant } = response.data;

      // Salvar tokens
      await AsyncStorage.setItem(TOKEN_KEY, access_token);
      if (refresh_token) {
        await AsyncStorage.setItem(REFRESH_TOKEN_KEY, refresh_token);
      }
      await AsyncStorage.setItem(USER_KEY, JSON.stringify(userData));

      // Configurar headers
      api.defaults.headers.common['Authorization'] = `Bearer ${access_token}`;
      setUser(userData);

      // Configurar tenant
      if (tenant) {
        await AsyncStorage.setItem(TENANT_KEY, JSON.stringify(tenant));
        setCurrentTenant(tenant);
        api.defaults.headers.common['X-Tenant-ID'] = tenant.tenant_id;
      } else if (userData.tenants && userData.tenants.length > 0) {
        // Usar primeiro tenant disponivel
        const defaultTenant = userData.tenants[0];
        await AsyncStorage.setItem(TENANT_KEY, JSON.stringify(defaultTenant));
        setCurrentTenant(defaultTenant);
        api.defaults.headers.common['X-Tenant-ID'] = defaultTenant.tenant_id;
      }

      // Atualizar lista de tenants
      if (userData.tenants) {
        setAvailableTenants(userData.tenants);
      }
    } catch (error) {
      throw new Error('Credenciais invalidas');
    } finally {
      setIsLoading(false);
    }
  }

  async function switchTenant(tenantId: string) {
    try {
      const tenant = availableTenants.find(t => t.tenant_id === tenantId);
      if (!tenant) {
        throw new Error('Tenant nao encontrado');
      }

      // Atualizar tenant
      await AsyncStorage.setItem(TENANT_KEY, JSON.stringify(tenant));
      setCurrentTenant(tenant);
      api.defaults.headers.common['X-Tenant-ID'] = tenantId;
    } catch (error) {
      throw error;
    }
  }

  async function refreshToken(): Promise<boolean> {
    try {
      const storedRefreshToken = await AsyncStorage.getItem(REFRESH_TOKEN_KEY);
      if (!storedRefreshToken) {
        return false;
      }

      const response = await api.post('/api/auth/refresh', {
        refresh_token: storedRefreshToken,
      });

      const { access_token, refresh_token: newRefreshToken } = response.data;

      await AsyncStorage.setItem(TOKEN_KEY, access_token);
      if (newRefreshToken) {
        await AsyncStorage.setItem(REFRESH_TOKEN_KEY, newRefreshToken);
      }

      api.defaults.headers.common['Authorization'] = `Bearer ${access_token}`;
      return true;
    } catch (error) {
      // Refresh falhou - fazer logout
      await signOut();
      return false;
    }
  }

  async function signInWithBiometric() {
    try {
      const biometricEnabled = await AsyncStorage.getItem(BIOMETRIC_KEY);
      if (!biometricEnabled) {
        throw new Error('Biometria nao habilitada');
      }

      const result = await LocalAuthentication.authenticateAsync({
        promptMessage: 'Autentique-se para continuar',
        cancelLabel: 'Cancelar',
        disableDeviceFallback: false,
      });

      if (result.success) {
        // Recuperar credenciais salvas e fazer login
        const storedToken = await AsyncStorage.getItem(TOKEN_KEY);
        const storedUser = await AsyncStorage.getItem(USER_KEY);

        if (storedToken && storedUser) {
          api.defaults.headers.common['Authorization'] = `Bearer ${storedToken}`;
          setUser(JSON.parse(storedUser));
        } else {
          throw new Error('Dados de login nao encontrados');
        }
      } else {
        throw new Error('Autenticacao biometrica falhou');
      }
    } catch (error) {
      throw error;
    }
  }

  async function enableBiometric(): Promise<boolean> {
    try {
      const result = await LocalAuthentication.authenticateAsync({
        promptMessage: 'Confirme sua identidade para habilitar biometria',
        cancelLabel: 'Cancelar',
      });

      if (result.success) {
        await AsyncStorage.setItem(BIOMETRIC_KEY, 'true');
        return true;
      }
      return false;
    } catch (error) {
      return false;
    }
  }

  async function signOut() {
    try {
      await AsyncStorage.multiRemove([
        TOKEN_KEY,
        REFRESH_TOKEN_KEY,
        USER_KEY,
        TENANT_KEY,
      ]);
      api.defaults.headers.common['Authorization'] = '';
      delete api.defaults.headers.common['X-Tenant-ID'];
      setUser(null);
      setCurrentTenant(null);
      setAvailableTenants([]);
    } catch (error) {
      console.error('Erro ao fazer logout:', error);
    }
  }

  return (
    <AuthContext.Provider
      value={{
        user,
        currentTenant,
        availableTenants,
        isLoading,
        isAuthenticated: !!user,
        biometricAvailable,
        signIn,
        signInWithBiometric,
        signOut,
        enableBiometric,
        switchTenant,
        refreshToken,
      }}
    >
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useAuth deve ser usado dentro de AuthProvider');
  }
  return context;
}
