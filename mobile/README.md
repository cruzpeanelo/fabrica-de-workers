# Fabrica de Agentes - App Mobile

App React Native para gestao Agile da Fabrica de Agentes.

## Funcionalidades

- **Dashboard**: Visao geral do projeto com metricas
- **Kanban**: Quadro de tarefas com scroll horizontal
- **Stories**: Lista de requisitos com filtros e busca
- **Chat**: Assistente IA para ajuda e geracao de codigo
- **Settings**: Tema escuro, notificacoes, biometria

## Tecnologias

- React Native (Expo)
- TypeScript
- React Navigation
- Zustand (state management)
- Expo Local Authentication (biometria)
- Expo Notifications

## Instalacao

```bash
# Instalar dependencias
cd mobile
npm install

# Iniciar em modo desenvolvimento
npm start

# Rodar no Android
npm run android

# Rodar no iOS
npm run ios
```

## Estrutura

```
mobile/
├── App.tsx                    # Entry point
├── src/
│   ├── components/            # Componentes reutilizaveis
│   ├── context/               # Contextos (Auth, Theme)
│   ├── hooks/                 # Custom hooks
│   ├── navigation/            # Navegacao
│   ├── screens/               # Telas
│   ├── services/              # API e servicos
│   └── styles/                # Estilos globais
└── assets/                    # Icones e imagens
```

## Configuracao

Crie um arquivo `.env` na raiz do projeto mobile:

```env
EXPO_PUBLIC_API_URL=http://seu-servidor:9001
```

## Cores (Identidade Belgo)

| Cor | Hex | Uso |
|-----|-----|-----|
| Azul Belgo | #003B4A | Primaria |
| Laranja Belgo | #FF6C00 | Secundaria |
| Verde Sucesso | #10B981 | Sucesso |
| Vermelho Erro | #EF4444 | Erro |

## Telas

1. **Login** - Autenticacao com credenciais ou biometria
2. **Home** - Dashboard com metricas e stories recentes
3. **Kanban** - Quadro de tarefas mobile-first
4. **Stories** - Lista de requisitos com CRUD
5. **Chat** - Assistente IA
6. **Settings** - Configuracoes do app

---

Issue #262 - Fabrica de Agentes
