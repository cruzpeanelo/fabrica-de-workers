# Kubernetes Manifests - Fabrica de Agentes

Issue #205 - Auto-scaling horizontal

## Quick Start

```bash
# Create namespace and apply configs
kubectl apply -f configmap.yaml

# Deploy services
kubectl apply -f deployment-api.yaml
kubectl apply -f deployment-workers.yaml

# Configure auto-scaling
kubectl apply -f hpa.yaml

# Setup ingress
kubectl apply -f ingress.yaml
```

## Architecture

```
                    ┌─────────────────────────────────────────────────────┐
                    │                   Ingress (nginx)                    │
                    └─────────────────────────────────────────────────────┘
                                              │
                    ┌─────────────────────────┴─────────────────────────┐
                    │                                                   │
        ┌───────────▼───────────┐                       ┌───────────────▼───────────┐
        │    factory-api        │                       │    factory-workers        │
        │    (2-10 replicas)    │                       │    (2-20 replicas)        │
        │    Port: 9001         │                       │    Async processing       │
        └───────────────────────┘                       └───────────────────────────┘
                    │                                               │
        ┌───────────▼───────────────────────────────────────────────▼───────────┐
        │                          Redis (job queue)                             │
        └────────────────────────────────────────────────────────────────────────┘
        │                          PostgreSQL (database)                         │
        └────────────────────────────────────────────────────────────────────────┘
```

## Files

| File | Description |
|------|-------------|
| `configmap.yaml` | Namespace, ConfigMap and Secrets |
| `deployment-api.yaml` | API service deployment |
| `deployment-workers.yaml` | Workers deployment |
| `hpa.yaml` | Horizontal Pod Autoscalers |
| `ingress.yaml` | Ingress and Network Policies |

## Health Checks

The API exposes these health endpoints:

- `GET /health/live` - Liveness probe (app is running)
- `GET /health/ready` - Readiness probe (can handle traffic)
- `GET /health/startup` - Startup probe (initialization complete)
- `GET /health` - Full health summary

## Auto-scaling

### API Service
- Min: 2 replicas
- Max: 10 replicas
- Scale up: CPU > 70% or Memory > 80%
- Scale down: 5 min stabilization

### Workers Service
- Min: 2 replicas
- Max: 20 replicas
- Scale up: CPU > 70% or Memory > 75%
- Scale down: 10 min stabilization

## Secrets Setup

Before deploying, update the secrets in `configmap.yaml`:

```bash
# Create secrets manually (more secure)
kubectl create secret generic factory-secrets \
  --namespace=fabrica-de-agentes \
  --from-literal=DATABASE_URL='postgresql://user:pass@host:5432/db' \
  --from-literal=ANTHROPIC_API_KEY='sk-ant-...' \
  --from-literal=JWT_SECRET_KEY='your-secret'
```

## Local Testing

```bash
# Using minikube
minikube start
minikube addons enable ingress
kubectl apply -f .

# Using kind
kind create cluster
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml
kubectl apply -f .
```

## Monitoring

```bash
# Watch pods
kubectl get pods -n fabrica-de-agentes -w

# Check HPA status
kubectl get hpa -n fabrica-de-agentes

# View logs
kubectl logs -n fabrica-de-agentes -l component=api -f
```
