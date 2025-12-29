# -*- coding: utf-8 -*-
"""
Factory API Module - Fabrica de Agentes v6.5
============================================

Modulos da API REST da Fabrica de Agentes.

Componentes:
- routes: Rotas internas da API (v1)
- public_api_v1: API publica versionada para desenvolvedores
- auth: Autenticacao JWT para dashboard
- api_key_auth: Autenticacao via API Key para API publica
- rate_limit: Rate limiting basico
- rate_limit_v2: Rate limiting avancado por tier
- webhooks: Sistema de notificacoes por webhook
- openapi_v1: Configuracao OpenAPI para documentacao
"""

__version__ = "6.5.0"
__all__ = [
    "routes",
    "public_api_v1",
    "auth",
    "api_key_auth",
    "rate_limit",
    "rate_limit_v2",
    "webhooks",
    "openapi_v1",
    "schemas",
    "middleware",
    "execution_routes",
]
