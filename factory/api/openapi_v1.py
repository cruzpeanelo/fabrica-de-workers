# -*- coding: utf-8 -*-
"""
Configuracao OpenAPI v1 - Fabrica de Agentes v6.5
=================================================

Especificacao OpenAPI completa para a API publica.
Documentacao interativa em /api/v1/docs (Swagger UI)
"""
from fastapi import FastAPI
from fastapi.openapi.utils import get_openapi


# =============================================================================
# METADATA
# =============================================================================

API_TITLE = "Fabrica de Agentes API"
API_VERSION = "1.0.0"
API_DESCRIPTION = """
# Fabrica de Agentes - API Publica

API REST para integracao com a plataforma de desenvolvimento autonomo Fabrica de Agentes.

## Visao Geral

A Fabrica de Agentes permite que voce:
- **Crie projetos** de software automaticamente
- **Defina User Stories** no formato Agile
- **Execute desenvolvimento autonomo** com IA
- **Monitore o progresso** em tempo real
- **Receba notificacoes** via webhooks

## Autenticacao

Todas as requisicoes (exceto health check) requerem autenticacao via API Key.

### Usando a API Key

Forneca sua API Key de uma das seguintes formas:

**Header (recomendado)**:
```
X-API-Key: sk-fab_xxx_yyyyyyy
```

**Query Parameter**:
```
GET /api/v1/projects?api_key=sk-fab_xxx_yyyyyyy
```

### Obtendo uma API Key

1. Acesse o Dashboard em http://localhost:9001
2. Va para Configuracoes > API Keys
3. Clique em "Criar Nova API Key"
4. Guarde a chave em local seguro (ela so e mostrada uma vez!)

## Rate Limiting

Limites variam conforme o tier da sua API Key:

| Tier | Por Minuto | Por Dia |
|------|------------|---------|
| Free | 100 | 1.000 |
| Basic | 500 | 10.000 |
| Pro | 2.000 | 100.000 |
| Enterprise | 10.000 | 1.000.000 |

Headers de rate limit sao incluidos em todas as respostas:
- `X-RateLimit-Limit`: Limite por minuto
- `X-RateLimit-Remaining`: Requisicoes restantes
- `X-RateLimit-Reset`: Unix timestamp do reset

## Scopes

API Keys podem ter diferentes permissoes (scopes):

| Scope | Descricao |
|-------|-----------|
| `read` | Leitura de recursos |
| `write` | Criacao e atualizacao |
| `admin` | Operacoes administrativas |
| `webhooks` | Gerenciamento de webhooks |

## Webhooks

Receba notificacoes em tempo real sobre eventos:

### Eventos Disponiveis

**Projetos**
- `project.created` - Projeto criado
- `project.updated` - Projeto atualizado
- `project.completed` - Projeto concluido

**Stories**
- `story.created` - Story criada
- `story.status_changed` - Status alterado
- `story.completed` - Story concluida

**Jobs**
- `job.created` - Job criado
- `job.started` - Job iniciado
- `job.completed` - Job concluido com sucesso
- `job.failed` - Job falhou

### Verificando Assinaturas

Webhooks incluem assinatura HMAC-SHA256:

```python
import hmac
import hashlib

def verify_signature(payload, signature, secret, timestamp):
    expected = hmac.new(
        secret.encode(),
        f"{timestamp}.{payload}".encode(),
        hashlib.sha256
    ).hexdigest()
    return hmac.compare_digest(f"sha256={expected}", signature)
```

## SDKs

SDKs oficiais disponiveis:

**Python**
```bash
pip install fabrica-sdk
```

```python
from fabrica import FabricaClient

client = FabricaClient(api_key='sk-fab_xxx')
project = client.projects.create(name='Meu App')
```

## Suporte

- Documentacao: https://docs.fabricadeagentes.com
- Email: support@fabricadeagentes.com
- GitHub Issues: https://github.com/cruzpeanelo/fabrica-de-agentes/issues
"""

# =============================================================================
# TAGS
# =============================================================================

TAGS_METADATA = [
    {
        "name": "Info",
        "description": "Informacoes e health check da API",
    },
    {
        "name": "API Keys",
        "description": """
Gerenciamento de API Keys.

API Keys sao necessarias para autenticar requisicoes.
Cada key tem um tier que define limites de rate limiting.
        """,
    },
    {
        "name": "Projetos",
        "description": """
Gerenciamento de projetos.

Projetos sao containers para Stories e Jobs.
Tipos disponiveis: `web-app`, `api-service`, `data-analysis`, `automation`.
        """,
    },
    {
        "name": "Stories",
        "description": """
Gerenciamento de User Stories.

Stories seguem o formato Agile:
"Como um [persona], eu quero [acao], para que [beneficio]"

Status disponiveis: `backlog`, `ready`, `in_progress`, `review`, `testing`, `done`.
        """,
    },
    {
        "name": "Jobs",
        "description": """
Jobs de desenvolvimento autonomo.

Quando voce executa uma Story ou cria um Job diretamente,
a Fabrica de Agentes usa IA para gerar codigo automaticamente.

Status: `pending`, `queued`, `running`, `completed`, `failed`.
        """,
    },
    {
        "name": "Webhooks",
        "description": """
Webhooks para notificacoes em tempo real.

Configure URLs para receber eventos quando algo acontece na Fabrica.
Suporta assinatura HMAC para verificacao de autenticidade.
        """,
    },
]


# =============================================================================
# SECURITY SCHEMES
# =============================================================================

SECURITY_SCHEMES = {
    "ApiKeyHeader": {
        "type": "apiKey",
        "in": "header",
        "name": "X-API-Key",
        "description": "API Key no header X-API-Key"
    },
    "ApiKeyQuery": {
        "type": "apiKey",
        "in": "query",
        "name": "api_key",
        "description": "API Key como query parameter"
    }
}


# =============================================================================
# RESPONSES COMUNS
# =============================================================================

COMMON_RESPONSES = {
    401: {
        "description": "API Key invalida ou ausente",
        "content": {
            "application/json": {
                "example": {
                    "error": "invalid_api_key",
                    "message": "API Key invalida ou nao encontrada"
                }
            }
        }
    },
    403: {
        "description": "Permissao insuficiente",
        "content": {
            "application/json": {
                "example": {
                    "error": "insufficient_scope",
                    "message": "Esta operacao requer scope 'write'",
                    "required_scope": "write",
                    "your_scopes": ["read"]
                }
            }
        }
    },
    404: {
        "description": "Recurso nao encontrado",
        "content": {
            "application/json": {
                "example": {
                    "detail": "Projeto nao encontrado"
                }
            }
        }
    },
    429: {
        "description": "Rate limit excedido",
        "content": {
            "application/json": {
                "example": {
                    "error": "rate_limit_exceeded",
                    "message": "Limite de requisicoes excedido",
                    "retry_after": 60
                }
            }
        }
    },
    500: {
        "description": "Erro interno do servidor",
        "content": {
            "application/json": {
                "example": {
                    "detail": "Erro interno do servidor"
                }
            }
        }
    }
}


# =============================================================================
# CUSTOM OPENAPI GENERATOR
# =============================================================================

def custom_openapi(app: FastAPI) -> dict:
    """
    Gera especificacao OpenAPI customizada.

    Usage:
        app = FastAPI()
        app.openapi = lambda: custom_openapi(app)
    """
    if app.openapi_schema:
        return app.openapi_schema

    openapi_schema = get_openapi(
        title=API_TITLE,
        version=API_VERSION,
        description=API_DESCRIPTION,
        routes=app.routes,
        tags=TAGS_METADATA,
    )

    # Adicionar security schemes
    openapi_schema["components"]["securitySchemes"] = SECURITY_SCHEMES

    # Adicionar seguranca global (exceto /health e /)
    openapi_schema["security"] = [
        {"ApiKeyHeader": []},
        {"ApiKeyQuery": []}
    ]

    # Adicionar informacoes de contato
    openapi_schema["info"]["contact"] = {
        "name": "Fabrica de Agentes",
        "url": "https://github.com/cruzpeanelo/fabrica-de-agentes",
        "email": "support@fabricadeagentes.com"
    }

    # Adicionar licenca
    openapi_schema["info"]["license"] = {
        "name": "MIT",
        "url": "https://opensource.org/licenses/MIT"
    }

    # Adicionar servers
    openapi_schema["servers"] = [
        {
            "url": "http://localhost:9001",
            "description": "Desenvolvimento local"
        },
        {
            "url": "https://api.fabricadeagentes.com",
            "description": "Producao"
        }
    ]

    # Adicionar externalDocs
    openapi_schema["externalDocs"] = {
        "description": "Documentacao Completa",
        "url": "https://docs.fabricadeagentes.com"
    }

    app.openapi_schema = openapi_schema
    return app.openapi_schema


# =============================================================================
# EXEMPLOS DE REQUESTS
# =============================================================================

REQUEST_EXAMPLES = {
    "project_create": {
        "simple": {
            "summary": "Projeto simples",
            "value": {
                "name": "Meu App",
                "project_type": "web-app"
            }
        },
        "complete": {
            "summary": "Projeto completo",
            "value": {
                "name": "E-commerce Platform",
                "description": "Plataforma de e-commerce completa com carrinho, pagamento e dashboard admin",
                "project_type": "web-app"
            }
        }
    },
    "story_create": {
        "simple": {
            "summary": "Story simples",
            "value": {
                "title": "Login com email"
            }
        },
        "agile": {
            "summary": "Story formato Agile",
            "value": {
                "title": "Login com Google",
                "persona": "usuario do sistema",
                "action": "fazer login com minha conta Google",
                "benefit": "nao precise criar nova senha",
                "acceptance_criteria": [
                    "Botao 'Login com Google' visivel",
                    "Redirect para OAuth Google",
                    "Usuario criado apos autenticacao"
                ],
                "story_points": 5,
                "priority": "high"
            }
        }
    },
    "webhook_create": {
        "basic": {
            "summary": "Webhook basico",
            "value": {
                "name": "Notificacoes Slack",
                "url": "https://hooks.slack.com/services/xxx",
                "events": ["job.completed", "job.failed"]
            }
        },
        "complete": {
            "summary": "Webhook com todos eventos",
            "value": {
                "name": "Sistema Integrado",
                "url": "https://meuapp.com/webhooks/fabrica",
                "events": [
                    "project.created",
                    "story.created",
                    "story.completed",
                    "job.started",
                    "job.completed",
                    "job.failed"
                ]
            }
        }
    }
}


# =============================================================================
# WEBHOOK PAYLOAD EXAMPLES
# =============================================================================

WEBHOOK_PAYLOAD_EXAMPLES = {
    "story.completed": {
        "id": "evt_abc123xyz789",
        "event": "story.completed",
        "created_at": "2024-01-15T10:30:00Z",
        "api_version": "2024-01-01",
        "data": {
            "story_id": "STR-0001",
            "project_id": "PRJ-001",
            "title": "Login com Google",
            "status": "done",
            "files_created": [
                "src/auth/google_oauth.py",
                "src/templates/login.html",
                "tests/test_google_auth.py"
            ],
            "duration_seconds": 342,
            "completed_at": "2024-01-15T10:30:00Z"
        }
    },
    "job.completed": {
        "id": "evt_def456abc123",
        "event": "job.completed",
        "created_at": "2024-01-15T10:35:00Z",
        "api_version": "2024-01-01",
        "data": {
            "job_id": "JOB-ABC123",
            "description": "Implementar autenticacao JWT",
            "status": "completed",
            "progress": 100,
            "files_created": [
                "src/auth/jwt_handler.py",
                "src/middleware/auth.py"
            ],
            "output_path": "/projects/PRJ-001/src",
            "started_at": "2024-01-15T10:30:00Z",
            "completed_at": "2024-01-15T10:35:00Z"
        }
    },
    "job.failed": {
        "id": "evt_ghi789def456",
        "event": "job.failed",
        "created_at": "2024-01-15T10:40:00Z",
        "api_version": "2024-01-01",
        "data": {
            "job_id": "JOB-DEF456",
            "description": "Integrar com API externa",
            "status": "failed",
            "progress": 45,
            "error_message": "Timeout ao conectar com API: connection refused",
            "error_step": "testing",
            "attempts": 3,
            "started_at": "2024-01-15T10:35:00Z",
            "failed_at": "2024-01-15T10:40:00Z"
        }
    }
}
