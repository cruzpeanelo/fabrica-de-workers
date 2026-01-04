"""
OpenAPI Configuration for Plataforma E API
=================================================

Configuration for API documentation and metadata.
"""

# API Tags for grouping endpoints
TAGS_METADATA = [
    {
        "name": "Status",
        "description": "Endpoints para verificar status da fabrica e saude da API."
    },
    {
        "name": "Authentication",
        "description": "Endpoints de autenticacao (login, logout, refresh token)."
    },
    {
        "name": "Projects",
        "description": "Gerenciamento de projetos. Criar, listar, atualizar e excluir projetos."
    },
    {
        "name": "Stories",
        "description": "Gerenciamento de User Stories. Baseado em boas praticas Scrum/Agile."
    },
    {
        "name": "Sprints",
        "description": "Gerenciamento de Sprints por projeto."
    },
    {
        "name": "Agents",
        "description": "Gerenciamento de agentes autonomos. 242 agentes disponiveis incluindo hierarquia corporativa."
    },
    {
        "name": "Skills",
        "description": "Skills disponiveis para os agentes. Incluindo skills multimidia e integracao."
    },
    {
        "name": "Tasks",
        "description": "Fila de tarefas para os agentes executarem."
    },
    {
        "name": "Templates",
        "description": "Templates de projetos pre-configurados."
    },
    {
        "name": "Logs",
        "description": "Logs de atividades e eventos da fabrica."
    },
    {
        "name": "Development",
        "description": "Endpoints para desenvolvimento autonomo de projetos."
    },
    {
        "name": "Hierarchy",
        "description": "Hierarquia corporativa de agentes (CEO -> Analistas)."
    }
]

# OpenAPI metadata
OPENAPI_METADATA = {
    "title": "Plataforma E API",
    "description": """
## Plataforma E - Plataforma de Desenvolvimento Autonomo

API REST para a plataforma de construcao de software com agentes autonomos.

### Recursos Principais

* **Projetos** - Crie e gerencie projetos de software
* **User Stories** - Defina requisitos no formato Agile
* **Agentes** - 242 agentes autonomos especializados
* **Skills** - 39+ skills para processamento de documentos, codigo, multimedia
* **Desenvolvimento Autonomo** - Os agentes desenvolvem codigo automaticamente

### Autenticacao

A API usa JWT (JSON Web Tokens) para autenticacao.

1. Faca login em `/api/auth/login`
2. Use o token retornado no header `Authorization: Bearer <token>`

### Modos de Operacao

* **Modo Template** - Geracao de codigo usando templates pre-definidos
* **Modo Inteligente** - Geracao de codigo usando Claude AI (requer API key)

### Links Uteis

* [Dashboard](http://localhost:9000) - Interface visual
* [GitHub](https://github.com/cruzpeanelo/plataforma-e) - Repositorio
    """,
    "version": "3.0.0",
    "terms_of_service": "https://github.com/cruzpeanelo/plataforma-e",
    "contact": {
        "name": "Luis Cruz",
        "url": "https://github.com/cruzpeanelo",
        "email": "support@fabricadeagentes.com"
    },
    "license_info": {
        "name": "MIT",
        "url": "https://opensource.org/licenses/MIT"
    }
}

# Common response examples
RESPONSE_EXAMPLES = {
    "not_found": {
        "description": "Recurso nao encontrado",
        "content": {
            "application/json": {
                "example": {"detail": "Projeto nao encontrado"}
            }
        }
    },
    "unauthorized": {
        "description": "Nao autorizado - token invalido ou ausente",
        "content": {
            "application/json": {
                "example": {"detail": "Credenciais invalidas"}
            }
        }
    },
    "validation_error": {
        "description": "Erro de validacao",
        "content": {
            "application/json": {
                "example": {
                    "detail": [
                        {
                            "loc": ["body", "name"],
                            "msg": "field required",
                            "type": "value_error.missing"
                        }
                    ]
                }
            }
        }
    },
    "success": {
        "description": "Operacao realizada com sucesso",
        "content": {
            "application/json": {
                "example": {"success": True, "message": "Operacao concluida"}
            }
        }
    }
}

# Security scheme
SECURITY_SCHEME = {
    "BearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "bearerFormat": "JWT",
        "description": "JWT token obtido via /api/auth/login"
    }
}
