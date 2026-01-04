# Workflow Padrão dos Agentes

## 1. AO INICIAR TRABALHO:
- Criar issue se não existir:
  gh issue create --title "[AGENTE] Tarefa X" --body "Descrição..."
- Ou pegar issue existente e comentar:
  gh issue comment <num> -b "Iniciando trabalho..."

## 2. AO COMPLETAR:
- Fazer commit referenciando issue:
  git commit -m "[AGENTE] Implementa X - Closes #123"
- Fechar issue:
  gh issue close <num> -c "Implementado em <commit>"
- HANDOFF para QA:
  gh issue create --title "[QA] Testar: <descrição>" --body "Testar implementação do commit X. Issue original: #123" --label "testing"

## 3. QA AO TESTAR:
- Se PASSAR: Fechar issue de teste
- Se FALHAR: Criar issue para agente corrigir:
  gh issue create --title "[AGENTE] Fix: <problema>" --body "Teste falhou: <detalhes>" --label "bug"
