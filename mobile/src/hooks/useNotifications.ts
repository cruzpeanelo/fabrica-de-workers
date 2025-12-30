/**
 * useNotifications Hook - Gerenciamento de notificacoes push
 * Configuracao para iOS e Android com Expo Notifications
 */

import { useState, useEffect, useRef } from 'react';
import { Platform } from 'react-native';
import * as Notifications from 'expo-notifications';
import * as Device from 'expo-device';
import Constants from 'expo-constants';
import AsyncStorage from '@react-native-async-storage/async-storage';

// Configurar comportamento das notificacoes
Notifications.setNotificationHandler({
  handleNotification: async () => ({
    shouldShowAlert: true,
    shouldPlaySound: true,
    shouldSetBadge: true,
  }),
});

interface NotificationData {
  type: string;
  storyId?: string;
  taskId?: string;
  message?: string;
}

const PUSH_TOKEN_KEY = '@FabricaAgentes:pushToken';

export function useNotifications() {
  const [expoPushToken, setExpoPushToken] = useState<string | null>(null);
  const [notification, setNotification] = useState<Notifications.Notification | null>(null);
  const [permissionGranted, setPermissionGranted] = useState(false);

  const notificationListener = useRef<Notifications.Subscription>();
  const responseListener = useRef<Notifications.Subscription>();

  useEffect(() => {
    // Carregar token salvo
    loadSavedToken();

    // Registrar para notificacoes
    registerForPushNotifications();

    // Listener para notificacoes recebidas (app em foreground)
    notificationListener.current = Notifications.addNotificationReceivedListener(
      (notification) => {
        setNotification(notification);
        handleNotificationReceived(notification);
      }
    );

    // Listener para quando usuario toca na notificacao
    responseListener.current = Notifications.addNotificationResponseReceivedListener(
      (response) => {
        handleNotificationResponse(response);
      }
    );

    return () => {
      if (notificationListener.current) {
        Notifications.removeNotificationSubscription(notificationListener.current);
      }
      if (responseListener.current) {
        Notifications.removeNotificationSubscription(responseListener.current);
      }
    };
  }, []);

  async function loadSavedToken() {
    try {
      const savedToken = await AsyncStorage.getItem(PUSH_TOKEN_KEY);
      if (savedToken) {
        setExpoPushToken(savedToken);
      }
    } catch (error) {
      console.error('Erro ao carregar push token:', error);
    }
  }

  async function registerForPushNotifications() {
    if (!Device.isDevice) {
      console.log('Notificacoes push requerem dispositivo fisico');
      return;
    }

    try {
      // Verificar permissoes existentes
      const { status: existingStatus } = await Notifications.getPermissionsAsync();
      let finalStatus = existingStatus;

      // Solicitar permissao se necessario
      if (existingStatus !== 'granted') {
        const { status } = await Notifications.requestPermissionsAsync();
        finalStatus = status;
      }

      if (finalStatus !== 'granted') {
        console.log('Permissao para notificacoes negada');
        setPermissionGranted(false);
        return;
      }

      setPermissionGranted(true);

      // Obter token
      const projectId = Constants.expoConfig?.extra?.eas?.projectId;
      const token = await Notifications.getExpoPushTokenAsync({
        projectId: projectId || undefined,
      });

      setExpoPushToken(token.data);
      await AsyncStorage.setItem(PUSH_TOKEN_KEY, token.data);

      // Configurar canal para Android
      if (Platform.OS === 'android') {
        await setupAndroidChannel();
      }

      console.log('Push token registrado:', token.data);
    } catch (error) {
      console.error('Erro ao registrar push token:', error);
    }
  }

  async function setupAndroidChannel() {
    await Notifications.setNotificationChannelAsync('default', {
      name: 'Notificacoes Gerais',
      importance: Notifications.AndroidImportance.MAX,
      vibrationPattern: [0, 250, 250, 250],
      lightColor: '#003B4A',
    });

    await Notifications.setNotificationChannelAsync('stories', {
      name: 'Atualizacoes de Requisitos',
      importance: Notifications.AndroidImportance.HIGH,
      vibrationPattern: [0, 250],
      lightColor: '#FF6C00',
    });

    await Notifications.setNotificationChannelAsync('tasks', {
      name: 'Tarefas',
      importance: Notifications.AndroidImportance.DEFAULT,
    });
  }

  function handleNotificationReceived(notification: Notifications.Notification) {
    const data = notification.request.content.data as NotificationData;
    console.log('Notificacao recebida:', data);

    // Pode emitir evento ou atualizar estado global aqui
  }

  function handleNotificationResponse(response: Notifications.NotificationResponse) {
    const data = response.notification.request.content.data as NotificationData;
    console.log('Usuario tocou na notificacao:', data);

    // Navegar para tela especifica baseado no tipo
    if (data.storyId) {
      // Navegar para detalhes da story
      // navigation.navigate('StoryDetail', { storyId: data.storyId });
    }
  }

  // Enviar notificacao local (para testes)
  async function sendLocalNotification(
    title: string,
    body: string,
    data?: NotificationData
  ) {
    await Notifications.scheduleNotificationAsync({
      content: {
        title,
        body,
        data: data || {},
        sound: true,
      },
      trigger: null, // Imediato
    });
  }

  // Agendar notificacao
  async function scheduleNotification(
    title: string,
    body: string,
    triggerDate: Date,
    data?: NotificationData
  ) {
    const id = await Notifications.scheduleNotificationAsync({
      content: {
        title,
        body,
        data: data || {},
        sound: true,
      },
      trigger: {
        date: triggerDate,
      },
    });
    return id;
  }

  // Cancelar notificacao agendada
  async function cancelScheduledNotification(notificationId: string) {
    await Notifications.cancelScheduledNotificationAsync(notificationId);
  }

  // Cancelar todas notificacoes
  async function cancelAllNotifications() {
    await Notifications.cancelAllScheduledNotificationsAsync();
  }

  // Limpar badge
  async function clearBadge() {
    await Notifications.setBadgeCountAsync(0);
  }

  return {
    expoPushToken,
    notification,
    permissionGranted,
    sendLocalNotification,
    scheduleNotification,
    cancelScheduledNotification,
    cancelAllNotifications,
    clearBadge,
    registerForPushNotifications,
  };
}

// Tipos de notificacoes da aplicacao
export const NotificationTypes = {
  STORY_CREATED: 'story_created',
  STORY_UPDATED: 'story_updated',
  STORY_MOVED: 'story_moved',
  TASK_COMPLETED: 'task_completed',
  CHAT_MESSAGE: 'chat_message',
  MENTION: 'mention',
  DEADLINE_REMINDER: 'deadline_reminder',
} as const;

// Helper para enviar notificacao para o servidor
export async function sendPushTokenToServer(token: string, userId: string) {
  try {
    // Implementar chamada ao backend para salvar o token
    // await api.post('/api/notifications/register', { token, userId });
    console.log('Token enviado ao servidor:', token);
  } catch (error) {
    console.error('Erro ao enviar token ao servidor:', error);
  }
}
