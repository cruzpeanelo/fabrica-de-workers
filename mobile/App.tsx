/**
 * Plataforma E - App Mobile
 * ================================
 * App React Native para gestao Agile
 * Issue #262, #368 - Multi-Tenant, White Label, User Mode
 */

import React from 'react';
import { StatusBar } from 'expo-status-bar';
import { SafeAreaProvider } from 'react-native-safe-area-context';
import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { NavigationContainer } from '@react-navigation/native';
import { AuthProvider } from './src/context/AuthContext';
import { ThemeProvider } from './src/context/ThemeContext';
import { UserModeProvider } from './src/context/UserModeContext';
import { RootNavigator } from './src/navigation/RootNavigator';

export default function App() {
  return (
    <GestureHandlerRootView style={{ flex: 1 }}>
      <SafeAreaProvider>
        <ThemeProvider>
          <AuthProvider>
            <UserModeProvider>
              <NavigationContainer>
                <StatusBar style="auto" />
                <RootNavigator />
              </NavigationContainer>
            </UserModeProvider>
          </AuthProvider>
        </ThemeProvider>
      </SafeAreaProvider>
    </GestureHandlerRootView>
  );
}
