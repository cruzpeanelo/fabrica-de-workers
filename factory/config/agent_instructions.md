# Instruções Padrão para Todos os Agentes

## Ao encontrar bugs ou melhorias:
1. Criar issue no GitHub:
   ```bash
   gh issue create --title "[AGENTE] Descrição do problema" --body "Detalhes..."
   ```

## Ao completar uma task:
1. Atualizar documentação relevante em `docs/`
2. Fechar issue se aplicável:
   ```bash
   gh issue close <numero> -c "Implementado no commit <hash>"
   ```

## Ao fazer commits:
1. Usar prefixo do agente: `[BACK]`, `[FRONT]`, `[QA]`, etc.
2. Referenciar issue: `Issue #123`
