#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Script para reclassificar issues com novos prefixos de agentes.

Mapeamento:
- T0, T0-TEST, T0-FIX, T0-CRITICAL -> ORCH (Orquestrador)
- TA -> BACK ou DEVOPS (baseado em conteudo)
- TB -> SEC (Security)
- TC -> FRONT (Frontend)
- TD -> BACK ou PROD (baseado em conteudo)
- TX -> QA (Documentacao)
- TZ -> DEVOPS (Infraestrutura)
- Outros -> Classificar por keywords
"""

import json
import subprocess
import re
import sys

# Padroes antigos para remover
OLD_PREFIXES = [
    r'\[T0-CRITICAL\]',
    r'\[T0-TEST\]',
    r'\[T0-FIX\]',
    r'\[T0\]',
    r'\[TA\]',
    r'\[TB\]',
    r'\[TC\]',
    r'\[TD\]',
    r'\[TX\]',
    r'\[TZ\]',
    r'\[SECURITY-CRITICAL\]',
    r'\[SECURITY-HIGH\]',
    r'\[SECURITY-MEDIUM\]',
    r'\[SECURITY\]',
    r'\[INFRA-CRITICAL\]',
    r'\[INFRA-HIGH\]',
    r'\[INFRA-MEDIUM\]',
    r'\[INFRA\]',
    r'\[DATABASE-CRITICAL\]',
    r'\[DATABASE-HIGH\]',
    r'\[DATABASE-MEDIUM\]',
    r'\[DATABASE\]',
    r'\[RBAC\]',
    r'\[INTEGRATION\]',
    r'\[FRONTEND\]',
    r'\[Feature\]',
    r'\[Input\]',
    r'\[DevEnv\]',
    r'\[Security\]',
    r'\[Billing\]',
    r'\[Multi-tenant\]',
    r'\[White Label\]',
    r'\[API\]',
    r'\[UX\]',
    r'\[CRITICAL\]',
]

def classify_by_keywords(title):
    """Classificar issue baseado em keywords no titulo."""
    t = title.lower()

    # SECURITY - Alta prioridade
    if any(k in t for k in ['security', 'auth', 'login', 'jwt', 'csrf', 'xss',
        'cors', 'permission', 'rbac', 'vulnerability', 'encryption', 'mfa',
        '2fa', 'token', 'credential', 'password', 'oauth', 'sso', 'session',
        'rate limit', 'brute force', 'injection', 'sanitiz']):
        return '[SEC]'

    # DEVOPS - Infraestrutura
    if any(k in t for k in ['docker', 'kubernetes', 'k8s', 'ci/cd', 'deploy',
        'infra', 'monitoring', 'terraform', 'helm', 'redis', 'postgres',
        'health check', 'migration', 'database schema', 'workflow', 'makefile',
        'observability', 'prometheus', 'grafana', 'scaling', 'replica']):
        return '[DEVOPS]'

    # FRONTEND - UI/UX
    if any(k in t for k in ['ui', 'ux', 'frontend', 'component', 'mobile',
        'pwa', 'css', 'theme', 'dark mode', 'responsive', 'animation',
        'skeleton', 'modal', 'button', 'form', 'layout', 'visual',
        'kanban', 'dashboard', 'notification center', 'fab', 'breadcrumb']):
        return '[FRONT]'

    # QA - Testes e Documentacao
    if any(k in t for k in ['test', 'pytest', 'coverage', 'doc', 'readme',
        'quality', 'validar', 'testar', 'e2e', 'unit test']):
        return '[QA]'

    # ARCHITECTURE - Design
    if any(k in t for k in ['architect', 'design', 'pattern', 'refactor',
        'migration', 'schema', 'structure', 'reorganiz']):
        return '[ARCH]'

    # PRODUCT - Features de negocio
    if any(k in t for k in ['feature', 'user story', 'roadmap', 'backlog',
        'requirement', 'acceptance', 'persona', 'onboarding', 'wizard']):
        return '[PROD]'

    # INNOVATION - Pesquisa
    if any(k in t for k in ['research', 'poc', 'experiment', 'ml', 'ai',
        'nlp', 'trend', 'benchmark', 'innovation', 'machine learning',
        'inteligent', 'smart']):
        return '[INOV]'

    # FINANCIAL - Custos e billing
    if any(k in t for k in ['cost', 'pricing', 'billing', 'subscription',
        'revenue', 'financial', 'scale', 'metering', 'usage']):
        return '[FIN]'

    # GROWTH - Marketing
    if any(k in t for k in ['marketing', 'launch', 'go-to-market', 'sales',
        'acquisition', 'retention', 'onboarding tour']):
        return '[GROWTH]'

    # ORCHESTRATOR - Coordenacao
    if any(k in t for k in ['coordinate', 'review', 'validate', 'approve',
        'disponivel', 'terminal']):
        return '[ORCH]'

    # DEFAULT: Backend
    return '[BACK]'

def get_new_prefix(title, old_prefix):
    """Determinar novo prefixo baseado no antigo e no conteudo."""
    t = title.lower()

    # Mapeamento direto
    if old_prefix in ['[T0]', '[T0-TEST]', '[T0-FIX]', '[T0-CRITICAL]']:
        # T0 era coordenador/testes, mas vamos classificar melhor
        if 'test' in t or 'validar' in t or 'testar' in t:
            return '[QA]'
        return '[ORCH]'

    if old_prefix == '[TB]':
        return '[SEC]'

    if old_prefix == '[TC]':
        return '[FRONT]'

    if old_prefix == '[TZ]':
        return '[DEVOPS]'

    if old_prefix == '[TX]':
        return '[QA]'

    if old_prefix == '[TA]':
        # Integrações podem ser Backend ou DevOps
        if any(k in t for k in ['jira', 'github', 'gitlab', 'azure', 'slack',
                                'webhook', 'api', 'connector', 'sync']):
            return '[BACK]'
        return '[DEVOPS]'

    if old_prefix == '[TD]':
        # Features podem ser Backend, Frontend ou Produto
        return classify_by_keywords(title)

    # Para outros prefixos antigos
    if old_prefix.startswith('[SECURITY'):
        return '[SEC]'

    if old_prefix.startswith('[INFRA'):
        return '[DEVOPS]'

    if old_prefix.startswith('[DATABASE'):
        return '[BACK]'

    if old_prefix == '[RBAC]':
        return '[SEC]'

    if old_prefix == '[INTEGRATION]':
        return '[BACK]'

    if old_prefix == '[FRONTEND]':
        return '[FRONT]'

    if old_prefix in ['[Feature]', '[UX]']:
        return classify_by_keywords(title)

    if old_prefix == '[Billing]':
        return '[FIN]'

    if old_prefix in ['[Multi-tenant]', '[White Label]']:
        return '[BACK]'

    if old_prefix == '[API]':
        return '[BACK]'

    if old_prefix in ['[DevEnv]', '[CRITICAL]']:
        return '[DEVOPS]'

    if old_prefix in ['[Input]', '[Security]']:
        return classify_by_keywords(title)

    # Fallback para classificacao por keywords
    return classify_by_keywords(title)

def clean_title(title):
    """Remover prefixos antigos do titulo."""
    clean = title
    for pattern in OLD_PREFIXES:
        clean = re.sub(pattern + r'\s*', '', clean, flags=re.IGNORECASE)
    return clean.strip()

def extract_old_prefix(title):
    """Extrair prefixo antigo do titulo."""
    for pattern in OLD_PREFIXES:
        match = re.match(pattern, title, re.IGNORECASE)
        if match:
            return match.group(0)
    return None

def main():
    # Carregar issues
    import os
    script_dir = os.path.dirname(os.path.abspath(__file__))
    with open(os.path.join(script_dir, 'issues.json'), 'r', encoding='utf-8') as f:
        issues = json.load(f)

    print(f"Total de issues: {len(issues)}")

    # Processar cada issue
    updates = []
    stats = {
        '[ORCH]': 0, '[ARCH]': 0, '[BACK]': 0, '[FRONT]': 0,
        '[DEVOPS]': 0, '[SEC]': 0, '[QA]': 0, '[PROD]': 0,
        '[INOV]': 0, '[FIN]': 0, '[GROWTH]': 0
    }

    for issue in issues:
        number = issue['number']
        title = issue['title']
        state = issue['state']

        # Extrair prefixo antigo
        old_prefix = extract_old_prefix(title)

        # Limpar titulo
        clean = clean_title(title)

        # Determinar novo prefixo
        if old_prefix:
            new_prefix = get_new_prefix(title, old_prefix)
        else:
            new_prefix = classify_by_keywords(clean)

        # Novo titulo
        new_title = f"{new_prefix} {clean}"

        # Verificar se precisa atualizar
        if title != new_title:
            updates.append({
                'number': number,
                'old_title': title,
                'new_title': new_title,
                'state': state
            })

        stats[new_prefix] = stats.get(new_prefix, 0) + 1

    print(f"\nIssues a atualizar: {len(updates)}")
    print("\nDistribuicao por agente:")
    for agent, count in sorted(stats.items(), key=lambda x: -x[1]):
        print(f"  {agent}: {count}")

    # Salvar para processamento
    output_path = os.path.join(script_dir, 'updates.json')
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(updates, f, indent=2, ensure_ascii=False)

    print(f"\nArquivo salvo em {output_path}")
    print("Execute: python scripts/apply_updates.py para aplicar")

if __name__ == '__main__':
    main()
