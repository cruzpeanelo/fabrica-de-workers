/**
 * Auth Context - Gerenciamento de autenticacao
 * Suporta login biometrico e JWT
 */

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as LocalAuthentication from 'expo-local-authentication';
import { api } from '../services/api';

interface User {
  id: string;
  username: string;
  email: string;
  role: string;
}

interface AuthContextData {
  user: User | null;
  isLoading: boolean;
  isAuthenticated: boolean;
  biometricAvailable: boolean;
  signIn: (username: string, password: string) => Promise<void>;
  signInWithBiometric: () => Promise<void>;
  signOut: () => Promise<void>;
  enableBiometric: () => Promise<boolean>;
}

const AuthContext = createContext<AuthContextData>({} as AuthContextData);

const TOKEN_KEY = '@FabricaAgentes:token';
const USER_KEY = '@FabricaAgentes:user';
const BIOMETRIC_KEY = '@FabricaAgentes:biometric';

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [biometricAvailable, setBiometricAvailable] = useState(false);

  useEffect(() => {
    loadStoredData();
    checkBiometricAvailability();
  }, []);

  async function loadStoredData() {
    try {
      const [storedToken, storedUser] = await Promise.all([
        AsyncStorage.getItem(TOKEN_KEY),
        AsyncStorage.getItem(USER_KEY),
      ]);

      if (storedToken && storedUser) {
        api.defaults.headers.common['Authorization'] = `Bearer ${storedToken}`;
        setUser(JSON.parse(storedUser));
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

  async function signIn(username: string, password: string) {
    try {
      setIsLoading(true);
      const response = await api.post('/api/auth/login', { username, password });
      const { access_token, user: userData } = response.data;

      await AsyncStorage.setItem(TOKEN_KEY, access_token);
      await AsyncStorage.setItem(USER_KEY, JSON.stringify(userData));

      api.defaults.headers.common['Authorization'] = `Bearer ${access_token}`;
      setUser(userData);
    } catch (error) {
      throw new Error('Credenciais invalidas');
    } finally {
      setIsLoading(false);
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
      await AsyncStorage.multiRemove([TOKEN_KEY, USER_KEY]);
      api.defaults.headers.common['Authorization'] = '';
      setUser(null);
    } catch (error) {
      console.error('Erro ao fazer logout:', error);
    }
  }

  return (
    <AuthContext.Provider
      value={{
        user,
        isLoading,
        isAuthenticated: !!user,
        biometricAvailable,
        signIn,
        signInWithBiometric,
        signOut,
        enableBiometric,
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
