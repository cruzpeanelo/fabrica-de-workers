# API Design Guide - Fabrica de Agentes

Issue #114 - [Arquitetura] API-First Design Pattern

## 1. OpenAPI Specification Standards

All API endpoints must be documented using OpenAPI 3.0+.

### Path Naming Conventions

- Use plural nouns: /projects, /stories, /tasks
- Use kebab-case: /story-tasks, /audit-logs
- Use path parameters: /projects/{project_id}
- Nest related resources: /projects/{project_id}/stories

---

## 2. Error Response Format

All errors follow RFC 7807 Problem Details format.

### HTTP Status Codes

| Code | Usage |
|------|-------|
| 200 | Successful GET, PUT, PATCH |
| 201 | Successful POST |
| 204 | Successful DELETE |
| 400 | Bad Request |
| 401 | Unauthorized |
| 403 | Forbidden |
| 404 | Not Found |
| 422 | Validation errors |
| 429 | Rate limit exceeded |
| 500 | Server error |

---

## 3. Pagination and Filtering

Use cursor-based pagination for large datasets.

### Filtering and Sorting

Query parameters: status, priority, sort, fields

---

## 4. Versioning Policy

URL path versioning: /v1/stories, /v2/stories

| Phase | Duration |
|-------|----------|
| Current | Indefinite |
| Deprecated | 12 months |
| Sunset | 3 months |

Breaking changes require new major version.

---

## 5. Authentication

Supported: Bearer JWT, API Key, OAuth2

---

## 6. Rate Limiting

| Tier | Requests/Hour |
|------|---------------|
| Free | 100 |
| Pro | 1,000 |
| Enterprise | 10,000 |

---

*Fabrica de Agentes v6.5 - API Design Guide*
