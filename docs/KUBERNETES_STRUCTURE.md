# Kubernetes Structure - Fabrica de Agentes

## Visão Geral

O projeto possui duas estruturas Kubernetes para diferentes casos de uso:

```
├── k8s/                    # Manifestos completos para produção
├── kubernetes/             # Manifestos simplificados com Kustomize
├── charts/fabrica-agentes/ # Helm Chart principal
└── helm/fabrica-agentes/   # Helm Chart alternativo
```

## Quando Usar Cada Um

| Estrutura | Caso de Uso | Comando |
|-----------|-------------|---------|
| `k8s/` | Produção completa com todos os recursos | `kubectl apply -k k8s/` |
| `kubernetes/overlays/` | Desenvolvimento rápido com Kustomize | `make deploy-dev` |
| `charts/fabrica-agentes/` | Deploy via Helm (recomendado) | `helm install fabrica charts/fabrica-agentes/` |

## Estrutura Detalhada

### k8s/ - Manifestos Completos

Manifestos Kubernetes completos com todas as configurações de produção:

```
k8s/
├── namespace.yaml           # Namespace fabrica-agentes
├── configmap.yaml           # Configurações da aplicação
├── secrets.yaml             # Secrets (com avisos de segurança)
├── ingress.yaml             # Ingress com TLS
├── kustomization.yaml       # Kustomize base
├── network-policy.yaml      # Network policies
├── pod-security-policy.yaml # PSP (deprecated em K8s 1.25+)
├── backup-cronjob.yaml      # Backup automático
│
├── api/
│   ├── deployment.yaml      # 3 réplicas, health checks
│   ├── service.yaml
│   └── hpa.yaml             # Autoscaling
│
├── workers/
│   ├── deployment.yaml
│   └── hpa.yaml
│
├── storage/
│   └── pvc.yaml             # Persistent Volume Claims
│
├── security/
│   ├── pod-security-policy.yaml
│   └── waf-config.yaml      # WAF configuration
│
├── monitoring/
│   ├── prometheus-config.yaml
│   └── grafana-deployment.yaml
│
├── backup/
│   └── velero-schedule.yaml # Velero backup schedule
│
└── ha/
    └── multi-az-config.yaml # Multi-AZ configuration
```

**Usar para:**
- Deploy de produção completo
- Ambientes que precisam de todos os recursos
- Referência de configuração

### kubernetes/overlays/ - Kustomize Simplificado

Manifestos simplificados com overlays Kustomize para diferentes ambientes:

```
kubernetes/
├── deployment-api.yaml      # Deployment base simplificado
├── deployment-workers.yaml
├── configmap.yaml
├── hpa.yaml
├── ingress.yaml
│
└── overlays/
    ├── development/
    │   └── kustomization.yaml  # 1 réplica, DEBUG=true
    ├── staging/
    │   └── kustomization.yaml  # 2 réplicas, LOG_LEVEL=INFO
    └── production/
        └── kustomization.yaml  # 3 réplicas, produção
```

**Usar para:**
- Desenvolvimento local com Kubernetes
- CI/CD com diferentes configurações por ambiente
- Testes rápidos

### charts/fabrica-agentes/ - Helm Chart Principal

Helm Chart completo com dependências:

```
charts/fabrica-agentes/
├── Chart.yaml               # Metadata e dependências
├── values.yaml              # Valores padrão
├── templates/
│   ├── _helpers.tpl
│   ├── deployment-api.yaml
│   ├── deployment-worker.yaml
│   ├── service.yaml
│   ├── ingress.yaml
│   ├── configmap.yaml
│   ├── secrets.yaml
│   ├── hpa.yaml
│   ├── pvc.yaml
│   └── serviceaccount.yaml
└── charts/                  # Dependências (postgresql, redis)
```

**Usar para:**
- Deploy em produção (recomendado)
- Gestão de releases com Helm
- Upgrades e rollbacks fáceis

## Comandos

### Desenvolvimento (Kustomize)

```bash
# Deploy development
make deploy-dev
# ou
kubectl apply -k kubernetes/overlays/development/

# Deploy staging
make deploy-staging

# Deploy production
make deploy-prod
```

### Produção (Helm)

```bash
# Instalar
helm install fabrica charts/fabrica-agentes/ \
  -f charts/fabrica-agentes/values.yaml \
  --set secrets.anthropicApiKey=$ANTHROPIC_API_KEY

# Upgrade
helm upgrade fabrica charts/fabrica-agentes/ \
  --set image.tag=v2.0.0

# Rollback
helm rollback fabrica 1
```

### Produção (kubectl direto)

```bash
# Aplicar todos os recursos
kubectl apply -k k8s/

# Verificar status
kubectl get all -n fabrica-agentes
```

## Migração entre Estruturas

### De Kustomize para Helm

Se você está usando `kubernetes/overlays/` e quer migrar para Helm:

1. Exporte os valores atuais:
```bash
kubectl get configmap -n fabrica-agentes -o yaml > current-config.yaml
```

2. Mapeie para `values.yaml` do Helm

3. Instale com Helm:
```bash
helm install fabrica charts/fabrica-agentes/ -f my-values.yaml
```

4. Delete os recursos antigos:
```bash
kubectl delete -k kubernetes/overlays/production/
```

## Recomendações

1. **Produção**: Use Helm (`charts/fabrica-agentes/`)
2. **Desenvolvimento**: Use Kustomize (`kubernetes/overlays/development/`)
3. **CI/CD**: Use Kustomize para builds, Helm para deploy

## Referências

- Issue #30: Kubernetes Deployment
- Issue #379: Helm Charts
- Issue #420: Consolidar diretórios Kubernetes
