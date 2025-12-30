# Comandos de Inicialização dos Terminais

> Cole o prompt correspondente em cada terminal Claude Code

---

## Terminal 0 - TESTES (Cole primeiro)

```
Leia o arquivo prompt_terminal_0.txt e siga as instruções.

CONTEXTO: Você é o Terminal 0 responsável por TESTES.
Os outros 4 terminais (A, B, C, D) estão desenvolvendo em paralelo.
Teste continuamente e reporte bugs via GitHub Issues.

Comece executando:
cat prompt_terminal_0.txt
```

---

## Terminal A - INTEGRAÇÕES

```
Leia o arquivo prompt_terminal_a.txt e siga as instruções.

⚠️ AVISO IMPORTANTE:
O Terminal 0 está testando a aplicação continuamente.
- Faça commits frequentes para ele testar suas mudanças
- Se ele reportar bug na sua área (factory/integrations/), corrija com prioridade
- Não edite arquivos fora da sua área para evitar conflitos

SEUS ISSUES: gh issue list --search "[TA] in:title" --state open

Comece executando:
cat prompt_terminal_a.txt
```

---

## Terminal B - SEGURANÇA

```
Leia o arquivo prompt_terminal_b.txt e siga as instruções.

⚠️ AVISO IMPORTANTE:
O Terminal 0 está testando a aplicação continuamente.
- Faça commits frequentes para ele testar suas mudanças
- Se ele reportar bug na sua área (auth, middleware, permissões), corrija com prioridade
- Não edite arquivos fora da sua área para evitar conflitos

SEUS ISSUES: gh issue list --search "[TB] in:title" --state open

Comece executando:
cat prompt_terminal_b.txt
```

---

## Terminal C - UI/UX

```
Leia o arquivo prompt_terminal_c.txt e siga as instruções.

⚠️ AVISO IMPORTANTE:
O Terminal 0 está testando a aplicação continuamente.
- Faça commits frequentes para ele testar suas mudanças
- Se ele reportar bug na sua área (CSS, JS, templates), corrija com prioridade
- PRIORIDADE MÁXIMA: Bug #308 do modal - resolva primeiro!
- Não edite arquivos fora da sua área para evitar conflitos

SEUS ISSUES: gh issue list --search "[TC] in:title" --state open

Comece executando:
cat prompt_terminal_c.txt
```

---

## Terminal D - FEATURES AGILE

```
Leia o arquivo prompt_terminal_d.txt e siga as instruções.

⚠️ AVISO IMPORTANTE:
O Terminal 0 está testando a aplicação continuamente.
- Faça commits frequentes para ele testar suas mudanças
- Se ele reportar bug na sua área (factory/core/, features), corrija com prioridade
- Escreva testes para cada feature (Terminal 0 vai executá-los)
- Não edite arquivos fora da sua área para evitar conflitos

SEUS ISSUES: gh issue list --search "[TD] in:title" --state open

Comece executando:
cat prompt_terminal_d.txt
```

---

## Resumo Visual

```
┌─────────────────────────────────────────────────────────────────┐
│                    TERMINAL 0 - TESTES                         │
│         Monitora todos, reporta bugs, valida qualidade         │
└─────────────────────────────────────────────────────────────────┘
        │              │              │              │
        ▼              ▼              ▼              ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ TERMINAL A   │ │ TERMINAL B   │ │ TERMINAL C   │ │ TERMINAL D   │
│ Integrações  │ │ Segurança    │ │ UI/UX        │ │ Features     │
│              │ │              │ │              │ │              │
│ SAP, Jira    │ │ Auth, RBAC   │ │ CSS, Dark    │ │ Kanban, AI   │
│ Azure DevOps │ │ Multi-tenant │ │ Mode, i18n   │ │ Sprint, Test │
│ SharePoint   │ │ Rate Limit   │ │ Mobile       │ │ Planning     │
│              │ │              │ │              │ │              │
│ 13 issues    │ │ 14 issues    │ │ 13 issues    │ │ 19 issues    │
│ [TA]         │ │ [TB]         │ │ [TC]         │ │ [TD]         │
└──────────────┘ └──────────────┘ └──────────────┘ └──────────────┘
```

## Ordem de Inicialização Recomendada

1. **Terminal 0** - Inicia testes e dashboard
2. **Terminal C** - Corrige bug #308 primeiro
3. **Terminal B** - Segurança é base para outros
4. **Terminal A** - Integrações independentes
5. **Terminal D** - Features dependem de base estável

## Comunicação Entre Terminais

Se precisar avisar outro terminal:
```bash
# Criar issue mencionando o terminal
gh issue create --title "[TX] Aviso para Terminal X" --body "Mensagem"

# Ou adicionar comentário em issue existente
gh issue comment NUMERO -b "Mensagem para Terminal X"
```

---

*Gerado em 2024-12-30 | Fábrica de Agentes v6.5*
