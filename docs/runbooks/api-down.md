# Runbook: API Down

**Issue #98**: Plano de Disaster Recovery Documentado

**Severidade:** Critical
**RTO:** 15 minutos
**Ultima Atualizacao:** 2024-12-29

---

## Sumario

Este runbook descreve os procedimentos para restaurar a API Plataforma E quando ela esta indisponivel.

---

## 1. Deteccao

### Alertas Prometheus
- `FactoryAPIDown` - API nao responde ao health check
- `FactoryAPIHighErrorRate` - Taxa de erro > 5%
- `FactoryAPIHighLatency` - P99 latency > 2s

### Verificacao Manual

```bash
# Verificar status do endpoint
curl -sf https://fabrica-agentes.com/api/health || echo "API DOWN"

# Verificar pods
kubectl get pods -n fabrica-agentes -l app=fabrica-api

# Verificar eventos recentes
kubectl get events -n fabrica-agentes --sort-by='.lastTimestamp' | tail -20
```

---

## 2. Diagnostico

### 2.1 Verificar Status dos Pods

```bash
# Listar pods da API
kubectl get pods -n fabrica-agentes -l app=fabrica-api -o wide

# Verificar descricao do pod com problema
kubectl describe pod <POD_NAME> -n fabrica-agentes

# Verificar logs do pod
kubectl logs <POD_NAME> -n fabrica-agentes --tail=100

# Verificar logs de todos os pods da API
kubectl logs -l app=fabrica-api -n fabrica-agentes --tail=50 --all-containers
```

### 2.2 Verificar Recursos

```bash
# Verificar uso de recursos
kubectl top pods -n fabrica-agentes -l app=fabrica-api

# Verificar limites e requests
kubectl get pods -n fabrica-agentes -l app=fabrica-api -o jsonpath='{.items[*].spec.containers[*].resources}'
```

### 2.3 Verificar Dependencias

```bash
# Verificar PostgreSQL
kubectl exec -it deploy/fabrica-api -n fabrica-agentes -- \
  python -c "from factory.database.connection import get_db; print('DB OK')"

# Verificar Redis
kubectl exec -it deploy/fabrica-api -n fabrica-agentes -- \
  python -c "import redis; r=redis.from_url('redis://redis:6379'); r.ping() and print('Redis OK')"
```

---

## 3. Resolucao

### Cenario A: Pod em CrashLoopBackOff

**Causa:** Aplicacao falhando ao iniciar

```bash
# 1. Verificar logs do container
kubectl logs <POD_NAME> -n fabrica-agentes --previous

# 2. Verificar variaveis de ambiente
kubectl exec -it <POD_NAME> -n fabrica-agentes -- env | sort

# 3. Se configuracao esta errada, corrigir ConfigMap/Secret
kubectl edit configmap fabrica-agentes-config -n fabrica-agentes

# 4. Reiniciar deployment
kubectl rollout restart deployment/fabrica-api -n fabrica-agentes

# 5. Aguardar rollout
kubectl rollout status deployment/fabrica-api -n fabrica-agentes --timeout=300s
```

### Cenario B: Pod Pendente (Scheduling)

**Causa:** Recursos insuficientes ou node problems

```bash
# 1. Verificar descricao do pod
kubectl describe pod <POD_NAME> -n fabrica-agentes | grep -A 10 Events

# 2. Verificar nodes disponiveis
kubectl get nodes
kubectl describe nodes | grep -A 5 "Allocated resources"

# 3. Se recursos insuficientes, escalar cluster
# (AWS EKS)
eksctl scale nodegroup --cluster=fabrica-cluster --name=workers --nodes=5

# 4. Ou reduzir recursos do deployment
kubectl set resources deployment/fabrica-api -n fabrica-agentes \
  --requests=cpu=200m,memory=256Mi --limits=cpu=500m,memory=512Mi
```

### Cenario C: Nenhum Pod Rodando

**Causa:** Deployment escalado para 0 ou deletado

```bash
# 1. Verificar deployment
kubectl get deployment fabrica-api -n fabrica-agentes

# 2. Se replicas = 0, escalar
kubectl scale deployment/fabrica-api --replicas=3 -n fabrica-agentes

# 3. Se deployment nao existe, reaplicar
kubectl apply -f k8s/api/deployment.yaml

# 4. Verificar status
kubectl rollout status deployment/fabrica-api -n fabrica-agentes
```

### Cenario D: Falha de Conexao com Database

**Causa:** PostgreSQL indisponivel ou credenciais invalidas

```bash
# 1. Verificar PostgreSQL
kubectl get pods -n fabrica-agentes -l app=postgresql

# 2. Testar conexao
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT 1"

# 3. Se PostgreSQL down, seguir runbook de database
# Ver: docs/runbooks/db-failover.md

# 4. Verificar secret de credenciais
kubectl get secret db-credentials -n fabrica-agentes -o yaml

# 5. Reiniciar API apos correcao
kubectl rollout restart deployment/fabrica-api -n fabrica-agentes
```

### Cenario E: Imagem Docker Invalida

**Causa:** Imagem nao encontrada ou tag errada

```bash
# 1. Verificar imagem atual
kubectl get deployment fabrica-api -n fabrica-agentes -o jsonpath='{.spec.template.spec.containers[0].image}'

# 2. Verificar se imagem existe
docker pull ghcr.io/lcruz/fabrica-agentes:latest

# 3. Se tag errada, fazer rollback
kubectl rollout undo deployment/fabrica-api -n fabrica-agentes

# 4. Verificar historico de rollouts
kubectl rollout history deployment/fabrica-api -n fabrica-agentes
```

---

## 4. Verificacao Pos-Recuperacao

```bash
# 1. Verificar pods running
kubectl get pods -n fabrica-agentes -l app=fabrica-api

# 2. Testar health check
curl -sf https://fabrica-agentes.com/api/health | jq .

# 3. Testar endpoint detalhado
curl -sf https://fabrica-agentes.com/api/health/detailed | jq .

# 4. Verificar metricas no Prometheus
curl -s "http://prometheus:9090/api/v1/query?query=up{job='fabrica-api'}" | jq .

# 5. Monitorar por 10 minutos
watch -n 5 kubectl get pods -n fabrica-agentes -l app=fabrica-api
```

---

## 5. Escalacao

| Tempo | Acao |
|-------|------|
| 0-5 min | On-call tenta resolucao |
| 5-15 min | Escalar para Tech Lead se nao resolvido |
| 15-30 min | Envolver Engineering Manager |
| 30+ min | Comunicar stakeholders, considerar DR |

### Contatos

- PagerDuty On-Call: (Auto)
- Tech Lead: tech-lead@fabrica-agentes.com
- Engineering Manager: eng-manager@fabrica-agentes.com

---

## 6. Prevencao

- [ ] Configurar PodDisruptionBudget
- [ ] Implementar health checks adequados
- [ ] Configurar resource requests/limits
- [ ] Habilitar HPA (Horizontal Pod Autoscaler)
- [ ] Monitorar alertas proativamente

---

## Historico de Incidentes

| Data | Causa | Resolucao | Duracao |
|------|-------|-----------|---------|
| - | - | - | - |

---

*Ultima revisao: 2024-12-29*
