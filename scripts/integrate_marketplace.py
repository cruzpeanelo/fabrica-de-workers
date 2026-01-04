# -*- coding: utf-8 -*-
"""
Script de Integracao do Marketplace
=====================================

Execute este script para integrar o Marketplace ao Dashboard Agile.

Uso:
    python scripts/integrate_marketplace.py
"""

import os
import re

DASHBOARD_PATH = r'C:\Users\lcruz\Plataforma E\factory\dashboard\app_v6_agile.py'

MARKETPLACE_INTEGRATION_CODE = '''
# Marketplace endpoints (Issue #56)
try:
    from factory.api.marketplace_routes import router as marketplace_router
    app.include_router(marketplace_router, prefix="/api/marketplace")
except ImportError:
    print("[Dashboard] Marketplace routes not available")
'''

def integrate_marketplace():
    """Integra o Marketplace ao Dashboard"""

    # Ler arquivo atual
    with open(DASHBOARD_PATH, 'r', encoding='utf-8') as f:
        content = f.read()

    # Verificar se ja esta integrado
    if 'marketplace_router' in content:
        print("[OK] Marketplace ja esta integrado ao Dashboard")
        return

    # Encontrar ponto de insercao (apos Code Review ou Preview)
    patterns = [
        (r'(# Code Review.*?app\.include_router\(code_review_router\))',
         r'\1' + MARKETPLACE_INTEGRATION_CODE),
        (r'(# Preview.*?app\.include_router\(preview_router\))',
         r'\1' + MARKETPLACE_INTEGRATION_CODE),
    ]

    for pattern, replacement in patterns:
        if re.search(pattern, content, re.DOTALL):
            content = re.sub(pattern, replacement, content, count=1, flags=re.DOTALL)
            break
    else:
        # Fallback: inserir apos CORSMiddleware
        content = content.replace(
            "# Diretorio de uploads",
            MARKETPLACE_INTEGRATION_CODE + "\n# Diretorio de uploads"
        )

    # Salvar arquivo
    with open(DASHBOARD_PATH, 'w', encoding='utf-8') as f:
        f.write(content)

    print("[OK] Marketplace integrado ao Dashboard com sucesso!")
    print("    - Endpoints disponiveis em /api/marketplace/*")
    print("    - Reinicie o dashboard para aplicar as mudancas")


if __name__ == '__main__':
    integrate_marketplace()
