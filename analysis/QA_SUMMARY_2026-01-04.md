# QA Summary - 2026-01-04

## Bugs Corrigidos

### Issue #461 - Data Segregation (CRITICAL - FIXED)
**Problema**: Todas as rotas eram consideradas publicas devido a bug no `_is_public_path`.
**Causa**: `/` na lista PUBLIC_PATHS e logica `path.startswith(public)` fazia com que TODAS as rotas começando com `/` fossem publicas.
**Correcao**: `factory/middleware/tenant_middleware.py` - Tratar `/` como match exato apenas.
**Validacao**: Teste de segregacao passou - BELGO_ADMIN ve apenas dados BELGO-001, TECH_ADMIN ve apenas TECH-001.

### Login Endpoint com Tenant ID (FIXED)
**Arquivo**: `factory/dashboard/app_v6_agile.py`
**Mudanca**: Token JWT agora inclui `tenant_id` e `tenant_ids` para filtro de dados.

### Rate Limit Paths (FIXED)
**Arquivo**: `factory/api/rate_limit_v2.py`
**Mudanca**: Endpoints de auth adicionados aos EXEMPT_PATHS.

## Issues Criadas

| Issue | Tipo | Titulo | Status |
|-------|------|--------|--------|
| #461 | SEC | Critical: Data segregation issue | CLOSED (fixed) |
| #466 | BACK | Missing /api/kanban/policies endpoint | OPEN |
| #482 | FRONT | Clickable elements smaller than 44px (31 elementos) | OPEN |

## Metricas UX

- **Content Area**: 100% (OK)
- **Z-index Layers**: 10
- **Small Clickables**: 31 elementos < 44px
- **Overlays**: 0 visiveis

## Testes de Segregacao

```
BELGO_ADMIN (BELGO-001): 51 stories - apenas de BELGO-001
TECH_ADMIN (TECH-001): 9 stories - apenas de TECH-001
Request sem auth: 401 AUTH_REQUIRED
```

## Arquivos Modificados

1. `factory/middleware/tenant_middleware.py`
   - `_is_public_path()` corrigido

2. `factory/dashboard/app_v6_agile.py`
   - Login endpoint com tenant_id no JWT

3. `factory/api/rate_limit_v2.py`
   - EXEMPT_PATHS expandido
   - Suporte a QA_MODE/RATE_LIMIT_MODE

## Proximos Passos

1. Implementar endpoint `/api/kanban/policies` (Issue #466)
2. Corrigir elementos clicaveis < 44px (Issue #482)
3. Verificar outras issues abertas de seguranca (Issues #469-480)
