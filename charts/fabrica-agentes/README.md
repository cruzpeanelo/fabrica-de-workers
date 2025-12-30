# Fabrica de Agentes - Helm Chart

Helm chart para deploy da Fábrica de Agentes em Kubernetes.

## Pré-requisitos

- Kubernetes 1.23+
- Helm 3.x
- Ingress Controller (nginx-ingress recomendado)
- Opcional: cert-manager para TLS

## Instalação

### Instalação básica

```bash
# Adicionar dependências
helm dependency update ./charts/fabrica-agentes

# Instalar
helm install fabrica ./charts/fabrica-agentes
```

### Instalação com valores customizados

```bash
helm install fabrica ./charts/fabrica-agentes \
  --set secrets.anthropicApiKey="sk-ant-..." \
  --set ingress.hosts[0].host="fabrica.exemplo.com"
```

### Instalação para produção

```bash
helm install fabrica ./charts/fabrica-agentes \
  -f values-prod.yaml \
  --namespace fabrica \
  --create-namespace
```

## Configuração

### Parâmetros Principais

| Parâmetro | Descrição | Default |
|-----------|-----------|---------|
| `global.environment` | Ambiente (development/staging/production) | `development` |
| `api.replicaCount` | Número de réplicas da API | `2` |
| `api.image.repository` | Imagem Docker | `fabrica-agentes` |
| `api.image.tag` | Tag da imagem | `latest` |
| `ingress.enabled` | Habilitar Ingress | `true` |
| `ingress.hosts[0].host` | Hostname | `fabrica.local` |
| `postgresql.enabled` | Usar PostgreSQL interno | `true` |
| `redis.enabled` | Usar Redis interno | `true` |

### Secrets

| Secret | Descrição |
|--------|-----------|
| `secrets.jwtSecret` | Chave para JWT (gerada se vazia) |
| `secrets.anthropicApiKey` | API Key do Claude |
| `secrets.databasePassword` | Senha do PostgreSQL |
| `secrets.redisPassword` | Senha do Redis |

### Banco de Dados Externo

Para usar PostgreSQL externo:

```yaml
postgresql:
  enabled: false

externalDatabase:
  host: "postgres.exemplo.com"
  port: 5432
  database: "fabrica_db"
  username: "fabrica"
  existingSecret: "postgres-credentials"
```

## Upgrade

```bash
helm upgrade fabrica ./charts/fabrica-agentes
```

## Desinstalação

```bash
helm uninstall fabrica
```

## Monitoramento

O chart expõe métricas Prometheus em `/metrics`. Para habilitar ServiceMonitor:

```yaml
metrics:
  enabled: true
  serviceMonitor:
    enabled: true
    namespace: monitoring
```

## Estrutura do Chart

```
charts/fabrica-agentes/
├── Chart.yaml          # Metadata do chart
├── values.yaml         # Valores padrão
├── README.md           # Documentação
└── templates/
    ├── _helpers.tpl    # Template helpers
    ├── deployment-api.yaml
    ├── service.yaml
    ├── ingress.yaml
    ├── configmap.yaml
    ├── secrets.yaml
    ├── hpa.yaml
    ├── pvc.yaml
    └── serviceaccount.yaml
```

## Suporte

Para problemas ou dúvidas, abra uma issue no repositório.
