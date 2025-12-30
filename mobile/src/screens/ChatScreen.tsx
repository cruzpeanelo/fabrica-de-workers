/**
 * Chat Screen - Assistente IA
 * Conversa com Claude para ajuda e geracao de codigo
 */

import React, { useState, useRef, useEffect } from 'react';
import {
  View,
  Text,
  FlatList,
  TextInput,
  TouchableOpacity,
  StyleSheet,
  KeyboardAvoidingView,
  Platform,
  ActivityIndicator,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { Send, Bot, User, Sparkles } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';
import { chatApi, ChatMessage } from '../services/api';

const QUICK_PROMPTS = [
  'Como posso melhorar este requisito?',
  'Gere testes para esta funcionalidade',
  'Sugira criterios de aceite',
  'Explique este conceito',
];

export function ChatScreen({ route }: any) {
  const storyId = route?.params?.storyId;
  const { colors } = useTheme();
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [inputText, setInputText] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const flatListRef = useRef<FlatList>(null);

  useEffect(() => {
    loadHistory();
  }, []);

  async function loadHistory() {
    try {
      const history = await chatApi.getHistory(storyId);
      setMessages(history);
    } catch (error) {
      console.error('Erro ao carregar historico:', error);
    }
  }

  async function sendMessage(text: string = inputText) {
    if (!text.trim() || isLoading) return;

    const userMessage: ChatMessage = {
      id: Date.now().toString(),
      role: 'user',
      content: text.trim(),
      created_at: new Date().toISOString(),
    };

    setMessages((prev) => [...prev, userMessage]);
    setInputText('');
    setIsLoading(true);

    try {
      const response = await chatApi.sendMessage(text.trim(), storyId);
      setMessages((prev) => [...prev, response]);
    } catch (error) {
      console.error('Erro ao enviar mensagem:', error);
      // Mensagem de erro
      setMessages((prev) => [
        ...prev,
        {
          id: Date.now().toString() + '-error',
          role: 'assistant',
          content: 'Desculpe, ocorreu um erro ao processar sua mensagem. Tente novamente.',
          created_at: new Date().toISOString(),
        },
      ]);
    } finally {
      setIsLoading(false);
    }
  }

  const styles = createStyles(colors);

  const renderMessage = ({ item }: { item: ChatMessage }) => {
    const isUser = item.role === 'user';

    return (
      <View
        style={[
          styles.messageContainer,
          isUser ? styles.userMessage : styles.assistantMessage,
        ]}
      >
        <View
          style={[
            styles.avatar,
            { backgroundColor: isUser ? colors.primary : colors.secondary },
          ]}
        >
          {isUser ? (
            <User color="#FFF" size={16} />
          ) : (
            <Bot color="#FFF" size={16} />
          )}
        </View>
        <View
          style={[
            styles.messageBubble,
            isUser ? styles.userBubble : styles.assistantBubble,
          ]}
        >
          <Text
            style={[
              styles.messageText,
              isUser && { color: '#FFF' },
            ]}
          >
            {item.content}
          </Text>
        </View>
      </View>
    );
  };

  return (
    <SafeAreaView style={styles.container} edges={['top']}>
      {/* Header */}
      <View style={styles.header}>
        <Sparkles color="#FFF" size={24} />
        <View style={styles.headerText}>
          <Text style={styles.headerTitle}>Assistente IA</Text>
          <Text style={styles.headerSubtitle}>
            {storyId ? `Sobre: ${storyId}` : 'Pergunte qualquer coisa'}
          </Text>
        </View>
      </View>

      {/* Messages */}
      <FlatList
        ref={flatListRef}
        data={messages}
        keyExtractor={(item) => item.id}
        renderItem={renderMessage}
        contentContainerStyle={styles.messagesList}
        onContentSizeChange={() =>
          flatListRef.current?.scrollToEnd({ animated: true })
        }
        ListEmptyComponent={
          <View style={styles.emptyState}>
            <Bot color={colors.textSecondary} size={48} />
            <Text style={styles.emptyTitle}>Como posso ajudar?</Text>
            <Text style={styles.emptySubtitle}>
              Pergunte sobre requisitos, codigo, testes ou qualquer duvida
            </Text>

            {/* Quick Prompts */}
            <View style={styles.quickPrompts}>
              {QUICK_PROMPTS.map((prompt, index) => (
                <TouchableOpacity
                  key={index}
                  style={styles.quickPromptBtn}
                  onPress={() => sendMessage(prompt)}
                >
                  <Text style={styles.quickPromptText}>{prompt}</Text>
                </TouchableOpacity>
              ))}
            </View>
          </View>
        }
      />

      {/* Loading Indicator */}
      {isLoading && (
        <View style={styles.loadingContainer}>
          <ActivityIndicator color={colors.primary} />
          <Text style={styles.loadingText}>Pensando...</Text>
        </View>
      )}

      {/* Input */}
      <KeyboardAvoidingView
        behavior={Platform.OS === 'ios' ? 'padding' : undefined}
      >
        <View style={styles.inputContainer}>
          <TextInput
            style={styles.input}
            placeholder="Digite sua mensagem..."
            placeholderTextColor={colors.textSecondary}
            value={inputText}
            onChangeText={setInputText}
            multiline
            maxLength={2000}
            editable={!isLoading}
          />
          <TouchableOpacity
            style={[
              styles.sendButton,
              (!inputText.trim() || isLoading) && styles.sendButtonDisabled,
            ]}
            onPress={() => sendMessage()}
            disabled={!inputText.trim() || isLoading}
          >
            <Send color="#FFF" size={20} />
          </TouchableOpacity>
        </View>
      </KeyboardAvoidingView>
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
      flexDirection: 'row',
      alignItems: 'center',
      padding: 16,
      backgroundColor: colors.primary,
      gap: 12,
    },
    headerText: {
      flex: 1,
    },
    headerTitle: {
      fontSize: 18,
      fontWeight: '600',
      color: '#FFF',
    },
    headerSubtitle: {
      fontSize: 12,
      color: 'rgba(255,255,255,0.8)',
    },
    messagesList: {
      padding: 16,
      flexGrow: 1,
    },
    messageContainer: {
      flexDirection: 'row',
      marginBottom: 16,
      gap: 8,
    },
    userMessage: {
      flexDirection: 'row-reverse',
    },
    assistantMessage: {
      flexDirection: 'row',
    },
    avatar: {
      width: 32,
      height: 32,
      borderRadius: 16,
      justifyContent: 'center',
      alignItems: 'center',
    },
    messageBubble: {
      maxWidth: '80%',
      padding: 12,
      borderRadius: 16,
    },
    userBubble: {
      backgroundColor: colors.primary,
      borderTopRightRadius: 4,
    },
    assistantBubble: {
      backgroundColor: colors.surface,
      borderTopLeftRadius: 4,
    },
    messageText: {
      fontSize: 15,
      color: colors.text,
      lineHeight: 22,
    },
    emptyState: {
      flex: 1,
      alignItems: 'center',
      justifyContent: 'center',
      padding: 32,
    },
    emptyTitle: {
      fontSize: 20,
      fontWeight: '600',
      color: colors.text,
      marginTop: 16,
    },
    emptySubtitle: {
      fontSize: 14,
      color: colors.textSecondary,
      textAlign: 'center',
      marginTop: 8,
    },
    quickPrompts: {
      marginTop: 24,
      width: '100%',
    },
    quickPromptBtn: {
      backgroundColor: colors.surface,
      padding: 14,
      borderRadius: 12,
      marginBottom: 8,
    },
    quickPromptText: {
      fontSize: 14,
      color: colors.primary,
      textAlign: 'center',
    },
    loadingContainer: {
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'center',
      padding: 12,
      gap: 8,
    },
    loadingText: {
      fontSize: 13,
      color: colors.textSecondary,
    },
    inputContainer: {
      flexDirection: 'row',
      alignItems: 'flex-end',
      padding: 12,
      backgroundColor: colors.surface,
      borderTopWidth: 1,
      borderTopColor: colors.border,
      gap: 8,
    },
    input: {
      flex: 1,
      backgroundColor: colors.background,
      borderRadius: 20,
      paddingHorizontal: 16,
      paddingVertical: 10,
      fontSize: 15,
      color: colors.text,
      maxHeight: 100,
    },
    sendButton: {
      width: 44,
      height: 44,
      borderRadius: 22,
      backgroundColor: colors.primary,
      justifyContent: 'center',
      alignItems: 'center',
    },
    sendButtonDisabled: {
      backgroundColor: colors.border,
    },
  });
