# Kubernetes Deployment - Fabrica de Agentes

Este diretorio contem os manifests Kubernetes para deploy da Fabrica de Agentes em ambientes de producao.

## Estrutura de Arquivos

```
k8s/
├── namespace.yaml          # Namespace, ResourceQuota, LimitRange, NetworkPolicy
├── configmap.yaml          # Configuracoes nao-sensiveis
├── secrets.yaml            # Credenciais e chaves (SUBSTITUIR EM PRODUCAO!)
├── ingress.yaml            # Ingress com TLS (nginx, traefik, ALB, GKE)
├── api/
│   ├── deployment.yaml     # Deployment da API/Dashboard
│   ├── service.yaml        # Services (ClusterIP, NodePort, Headless)
│   └── hpa.yaml            # HorizontalPodAutoscaler e PodDisruptionBudget
├── workers/
│   ├── deployment.yaml     # Workers + Watcher
│   └── hpa.yaml            # Auto-scaling dos workers
└── storage/
    └── pvc.yaml            # PersistentVolumeClaims
```

## Pre-requisitos

- Kubernetes 1.25+
- kubectl configurado
- Ingress Controller instalado (nginx-ingress recomendado)
- cert-manager para certificados TLS automaticos (opcional)
- StorageClass configurado (ou usar default)

## Deploy Rapido

### 1. Criar Namespace

```bash
kubectl apply -f k8s/namespace.yaml
```

### 2. Configurar Secrets

**IMPORTANTE**: Edite `k8s/secrets.yaml` e substitua os valores de exemplo:

```bash
# Gerar valor base64 para ANTHROPIC_API_KEY
echo -n "sk-ant-api-sua-chave-aqui" | base64

# Editar secrets.yaml com seus valores
nano k8s/secrets.yaml

# Aplicar secrets
kubectl apply -f k8s/secrets.yaml
```

### 3. Aplicar ConfigMaps

```bash
kubectl apply -f k8s/configmap.yaml
```

### 4. Criar Storage

```bash
kubectl apply -f k8s/storage/pvc.yaml
```

### 5. Deploy da API

```bash
kubectl apply -f k8s/api/
```

### 6. Deploy dos Workers

```bash
kubectl apply -f k8s/workers/
```

### 7. Configurar Ingress

Edite `k8s/ingress.yaml` com seu dominio e aplique:

```bash
kubectl apply -f k8s/ingress.yaml
```

## Deploy Completo (Script)

```bash
#!/bin/bash
# deploy-k8s.sh

set -e

echo "=== Fabrica de Agentes - Kubernetes Deploy ==="

# Namespace
echo "1. Criando namespace..."
kubectl apply -f k8s/namespace.yaml

# Secrets e ConfigMaps
echo "2. Aplicando configuracoes..."
kubectl apply -f k8s/secrets.yaml
kubectl apply -f k8s/configmap.yaml

# Storage
echo "3. Criando volumes persistentes..."
kubectl apply -f k8s/storage/pvc.yaml

# Aguardar PVCs
echo "4. Aguardando PVCs..."
kubectl wait --for=condition=Bound pvc --all -n fabrica-agentes --timeout=120s

# API
echo "5. Deployando API..."
kubectl apply -f k8s/api/

# Workers
echo "6. Deployando Workers..."
kubectl apply -f k8s/workers/

# Ingress
echo "7. Configurando Ingress..."
kubectl apply -f k8s/ingress.yaml

# Aguardar pods
echo "8. Aguardando pods ficarem prontos..."
kubectl wait --for=condition=Ready pod -l app.kubernetes.io/name=fabrica-agentes -n fabrica-agentes --timeout=300s

echo "=== Deploy concluido! ==="
kubectl get pods -n fabrica-agentes
```

## Helm Chart (Alternativa)

Para deploy mais flexivel, use o Helm chart:

```bash
# Instalar
helm install fabrica-agentes ./helm/fabrica-agentes \
  -n fabrica-agentes \
  --create-namespace \
  -f custom-values.yaml

# Upgrade
helm upgrade fabrica-agentes ./helm/fabrica-agentes \
  -n fabrica-agentes \
  -f custom-values.yaml

# Listar releases
helm list -n fabrica-agentes

# Desinstalar
helm uninstall fabrica-agentes -n fabrica-agentes
```

### Exemplo de custom-values.yaml

```yaml
# custom-values.yaml
global:
  environment: production

secrets:
  anthropicApiKey: "sk-ant-api-sua-chave-real"
  databasePassword: "senha-forte-aqui"

api:
  replicaCount: 3
  resources:
    requests:
      cpu: "500m"
      memory: "512Mi"
    limits:
      cpu: "2000m"
      memory: "2Gi"

ingress:
  hosts:
    - host: fabrica.suaempresa.com.br
      paths:
        - path: /
          pathType: Prefix
          service: api
  tls:
    enabled: true
    hosts:
      - fabrica.suaempresa.com.br

postgresql:
  enabled: false  # Usar RDS/Cloud SQL

externalDatabase:
  enabled: true
  host: "postgres.suaempresa.rds.amazonaws.com"
  port: 5432
  database: fabrica_db
```

## Build das Imagens Docker

```bash
# API
docker build -f Dockerfile.prod --target api -t fabrica-agentes-api:6.0 .

# Worker
docker build -f Dockerfile.prod --target worker -t fabrica-agentes-worker:6.0 .

# Watcher
docker build -f Dockerfile.prod --target watcher -t fabrica-agentes-watcher:6.0 .

# Push para registry (exemplo ECR)
aws ecr get-login-password | docker login --username AWS --password-stdin 123456789.dkr.ecr.us-east-1.amazonaws.com
docker tag fabrica-agentes-api:6.0 123456789.dkr.ecr.us-east-1.amazonaws.com/fabrica-agentes-api:6.0
docker push 123456789.dkr.ecr.us-east-1.amazonaws.com/fabrica-agentes-api:6.0
```

## Verificacao de Saude

```bash
# Status dos pods
kubectl get pods -n fabrica-agentes -w

# Logs da API
kubectl logs -f deployment/fabrica-api -n fabrica-agentes

# Logs dos workers
kubectl logs -f deployment/fabrica-worker -n fabrica-agentes

# Logs do watcher
kubectl logs -f deployment/fabrica-watcher -n fabrica-agentes

# Descrever deployment
kubectl describe deployment fabrica-api -n fabrica-agentes

# Verificar HPA
kubectl get hpa -n fabrica-agentes

# Verificar Ingress
kubectl get ingress -n fabrica-agentes

# Testar health check
kubectl port-forward svc/fabrica-api-service 9001:9001 -n fabrica-agentes
curl http://localhost:9001/health
```

## Troubleshooting

### Pods nao iniciam

```bash
# Verificar eventos
kubectl get events -n fabrica-agentes --sort-by='.lastTimestamp'

# Verificar descricao do pod
kubectl describe pod <pod-name> -n fabrica-agentes

# Verificar logs do init container
kubectl logs <pod-name> -c wait-for-db -n fabrica-agentes
```

### PVC pendente

```bash
# Verificar StorageClass disponivel
kubectl get sc

# Verificar status do PVC
kubectl describe pvc -n fabrica-agentes
```

### Certificado TLS nao gerado

```bash
# Verificar cert-manager
kubectl get certificate -n fabrica-agentes
kubectl describe certificate fabrica-tls -n fabrica-agentes

# Verificar ClusterIssuer
kubectl describe clusterissuer letsencrypt-prod
```

### Workers nao processam

```bash
# Verificar conexao com Redis
kubectl exec -it deployment/fabrica-worker -n fabrica-agentes -- redis-cli -h $REDIS_HOST ping

# Verificar logs detalhados
kubectl logs deployment/fabrica-worker -n fabrica-agentes --tail=100
```

## Ambientes Cloud

### AWS EKS

```bash
# Criar cluster
eksctl create cluster --name fabrica-agentes --region us-east-1 --nodes 3

# Instalar AWS Load Balancer Controller
helm repo add eks https://aws.github.io/eks-charts
helm install aws-load-balancer-controller eks/aws-load-balancer-controller \
  -n kube-system \
  --set clusterName=fabrica-agentes

# Usar ALB Ingress
# Editar k8s/ingress.yaml para usar alb.ingress.kubernetes.io/*
```

### Google GKE

```bash
# Criar cluster
gcloud container clusters create fabrica-agentes --num-nodes=3 --region=us-central1

# GKE ja tem ingress controller (gce)
# Editar k8s/ingress.yaml para usar kubernetes.io/ingress.class: "gce"
```

### Azure AKS

```bash
# Criar cluster
az aks create --resource-group fabrica-rg --name fabrica-agentes --node-count 3

# Instalar NGINX Ingress
helm install nginx-ingress ingress-nginx/ingress-nginx \
  --namespace ingress-nginx \
  --create-namespace
```

## Monitoramento

### Prometheus + Grafana

```bash
# Instalar kube-prometheus-stack
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm install prometheus prometheus-community/kube-prometheus-stack -n monitoring --create-namespace

# Os pods ja tem annotations para scraping automatico:
# prometheus.io/scrape: "true"
# prometheus.io/port: "9001"
```

### Metricas disponiveis

- `http_requests_total` - Total de requisicoes HTTP
- `http_request_duration_seconds` - Duracao das requisicoes
- `active_stories_count` - Stories ativas
- `worker_jobs_processed_total` - Jobs processados pelos workers

## Seguranca

1. **Secrets**: Nunca commitar secrets reais. Use:
   - External Secrets Operator + AWS Secrets Manager
   - HashiCorp Vault
   - Azure Key Vault
   - GCP Secret Manager

2. **Network Policies**: Ja configuradas para isolamento do namespace

3. **Pod Security**: Containers rodam como non-root (UID 1000)

4. **RBAC**: ServiceAccounts criados com permissoes minimas

5. **TLS**: Certificados automaticos via cert-manager + Let's Encrypt

## Autor

Fabrica de Agentes Team - fabrica@belgo.com.br
