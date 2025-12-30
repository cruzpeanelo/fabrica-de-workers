# Checklist de Testes - App Mobile Multi-Tenant

Este documento descreve os testes manuais a serem realizados para validar
o funcionamento correto do app mobile com multi-tenant e white label.

## Pre-requisitos

- [ ] Backend rodando em `localhost:9001`
- [ ] App mobile compilado (Expo ou build nativo)
- [ ] Pelo menos 2 tenants configurados no banco
- [ ] Usuario de teste para cada tenant

---

## 1. Autenticacao

### 1.1 Login
- [ ] Tela de login carrega sem erros
- [ ] Campo de usuario aceita input
- [ ] Campo de senha aceita input (mascarado)
- [ ] Botao de login esta habilitado
- [ ] Login com credenciais validas funciona
- [ ] Login com credenciais invalidas mostra erro
- [ ] Token JWT e armazenado apos login
- [ ] `tenant_id` e armazenado apos login

### 1.2 Logout
- [ ] Botao de logout visivel nas configuracoes
- [ ] Logout remove token do storage
- [ ] Logout redireciona para tela de login
- [ ] Tentar acessar tela protegida apos logout redireciona

### 1.3 Refresh Token
- [ ] Token expira apos tempo configurado
- [ ] App tenta refresh automaticamente
- [ ] Novo token e armazenado apos refresh
- [ ] Refresh token invalido faz logout

### 1.4 Biometria (se disponivel)
- [ ] Opcao de habilitar biometria nas configuracoes
- [ ] Biometria funciona no login subsequente
- [ ] Falha de biometria mostra opcao de senha

---

## 2. Multi-Tenant

### 2.1 Headers de Requisicao
- [ ] Todas requisicoes enviam `Authorization` header
- [ ] Todas requisicoes enviam `X-Tenant-ID` header
- [ ] `X-Device-ID` enviado (se registrado)

### 2.2 Isolamento de Dados
- [ ] Usuario do Tenant A nao ve dados do Tenant B
- [ ] Stories listadas sao apenas do tenant do usuario
- [ ] Projects listados sao apenas do tenant do usuario

### 2.3 Troca de Tenant (Admin)
- [ ] Admin pode trocar de tenant (se implementado)
- [ ] Dados atualizam apos troca
- [ ] Header `X-Switch-Tenant-ID` enviado

---

## 3. White Label / Branding

### 3.1 Cores
- [ ] Cor primaria aplicada no header
- [ ] Cor primaria aplicada nos botoes principais
- [ ] Cor secundaria aplicada em CTAs
- [ ] Cor de fundo correta

### 3.2 Logo
- [ ] Logo do tenant exibido no header
- [ ] Logo carrega da URL correta
- [ ] Fallback para logo padrao se URL falhar

### 3.3 Nome da Empresa
- [ ] Nome do tenant exibido no header
- [ ] Nome exibido na tela de splash (se houver)

### 3.4 Temas
- [ ] Tema claro funciona
- [ ] Tema escuro funciona (se implementado)
- [ ] Toggle de tema persiste preferencia

---

## 4. User Mode (Basico/Avancado)

### 4.1 Toggle
- [ ] Toggle visivel nas configuracoes
- [ ] Toggle alterna entre Basico e Avancado
- [ ] Preferencia persiste apos fechar app

### 4.2 Modo Basico
- [ ] Labels simplificados exibidos:
  - [ ] "Stories" -> "Requisitos"
  - [ ] "Tasks" -> "Atividades"
  - [ ] "Sprint" -> "Ciclo de Trabalho"
  - [ ] "Backlog" -> "Lista de Espera"
- [ ] Features avancadas ocultas

### 4.3 Modo Avancado
- [ ] Labels tecnicos exibidos
- [ ] Todas features visiveis
- [ ] Metricas e logs disponiveis

---

## 5. Onboarding Tour

### 5.1 Primeiro Acesso
- [ ] Tour inicia automaticamente para novo usuario
- [ ] Steps do tour aparecem em sequencia
- [ ] Botao "Proximo" avanca step
- [ ] Botao "Pular" fecha tour
- [ ] Botao "Anterior" volta step

### 5.2 Tour Completado
- [ ] Tour nao aparece em acessos subsequentes
- [ ] Flag `completed_tour` salvo no backend
- [ ] Opcao de refazer tour nas configuracoes

### 5.3 Steps do Tour
- [ ] Step 1: Boas-vindas
- [ ] Step 2: Menu de navegacao
- [ ] Step 3: Quadro Kanban
- [ ] Step 4: Lista de Stories
- [ ] Step 5: Criar Story
- [ ] Step 6: Chat/Assistente
- [ ] Step 7: Barra de progresso
- [ ] Step 8: Toggle de modo
- [ ] Step 9: Conclusao

---

## 6. Funcionalidades Core

### 6.1 Dashboard/Home
- [ ] Tela inicial carrega
- [ ] Resumo de projetos exibido
- [ ] Metricas de progresso corretas
- [ ] Navegacao para outras telas funciona

### 6.2 Stories
- [ ] Lista de stories carrega
- [ ] Pull-to-refresh funciona
- [ ] Filtros funcionam (se houver)
- [ ] Tap em story abre detalhes
- [ ] Criar nova story funciona
- [ ] Editar story funciona

### 6.3 Kanban
- [ ] Board carrega com colunas
- [ ] Scroll horizontal nas colunas
- [ ] Cards exibem informacoes corretas
- [ ] Arrastar card entre colunas (se implementado)

### 6.4 Chat/Assistente
- [ ] Tela de chat carrega
- [ ] Historico exibido
- [ ] Enviar mensagem funciona
- [ ] Resposta do assistente exibida
- [ ] Scroll automatico para nova mensagem

---

## 7. Offline/Conectividade

### 7.1 Sem Conexao
- [ ] App detecta falta de conexao
- [ ] Mensagem de erro amigavel exibida
- [ ] Dados em cache ainda acessiveis (se implementado)

### 7.2 Reconexao
- [ ] App detecta retorno de conexao
- [ ] Dados sincronizam automaticamente
- [ ] Acoes pendentes sao enviadas

---

## 8. Performance

### 8.1 Carregamento
- [ ] Splash screen exibida durante load
- [ ] Tempo de login < 3 segundos
- [ ] Lista de stories carrega < 2 segundos
- [ ] Navegacao entre telas fluida

### 8.2 Memoria
- [ ] App nao trava com uso prolongado
- [ ] Imagens carregam sem travamentos
- [ ] Scroll em listas longas e suave

---

## 9. Seguranca

### 9.1 Armazenamento
- [ ] Token armazenado de forma segura (SecureStore/Keychain)
- [ ] Dados sensiveis nao em AsyncStorage plano
- [ ] Logout limpa todos dados locais

### 9.2 Rede
- [ ] Todas chamadas usam HTTPS
- [ ] Certificados SSL validados
- [ ] Timeout configurado para requisicoes

### 9.3 Validacao
- [ ] Inputs sanitizados antes de envio
- [ ] Erros de API tratados graciosamente
- [ ] Nenhuma informacao sensivel em logs

---

## Resultados

| Categoria | Passou | Falhou | N/A |
|-----------|--------|--------|-----|
| Autenticacao | | | |
| Multi-Tenant | | | |
| White Label | | | |
| User Mode | | | |
| Onboarding | | | |
| Core | | | |
| Offline | | | |
| Performance | | | |
| Seguranca | | | |
| **TOTAL** | | | |

## Observacoes

```
Data do teste: ___/___/______
Testador: _________________
Versao do app: ____________
Versao do backend: ________
Dispositivo: ______________
OS: ______________________
```

### Bugs Encontrados

1.

### Melhorias Sugeridas

1.

---

*Checklist criado para Issue #386 - Testes Mobile Multi-Tenant*
