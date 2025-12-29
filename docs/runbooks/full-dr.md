# Runbook: Full Disaster Recovery

**Issue #98**: Plano de Disaster Recovery Documentado

**Severidade:** Critical
**RTO:** 4-8 horas
**RPO:** 1 hora
**Ultima Atualizacao:** 2024-12-29

---

## Sumario

Este runbook descreve os procedimentos para recuperacao completa do ambiente Fabrica de Agentes em caso de desastre regional ou perda total do cluster.

---

## 1. Criterios de Ativacao

### Quando Ativar DR Completo

- [ ] Datacenter/regiao inteira indisponivel
- [ ] Cluster Kubernetes irrecuperavel
- [ ] Perda de dados em multiplos componentes
- [ ] Tempo de recuperacao no local > 4 horas
- [ ] Decisao de gestao para ativar DR

### Verificacao Antes de Ativar

```bash
# 1. Confirmar que cluster primario esta indisponivel
kubectl cluster-info --context=fabrica-prod || echo "PRIMARY CLUSTER DOWN"

# 2. Verificar status da regiao AWS
aws health describe-events --filter service=EC2 --region us-east-1

# 3. Verificar backups disponiveis
aws s3 ls s3://fabrica-agentes-backups/db/ | tail -5

# 4. Verificar infraestrutura DR disponivel
kubectl cluster-info --context=fabrica-dr || echo "DR CLUSTER ALSO DOWN"
```

---

## 2. Pre-requisitos DR

### 2.1 Infraestrutura DR

| Componente | Regiao DR | Status |
|------------|-----------|--------|
| EKS Cluster | us-west-2 | Standby |
| RDS Instance | us-west-2 | Read Replica |
| S3 Backups | us-west-2 | Replicado |
| Route53 | Global | Failover configurado |
| CloudFront | Global | Multi-origin |

### 2.2 Verificar Disponibilidade DR

```bash
# Verificar cluster DR
kubectl get nodes --context=fabrica-dr

# Verificar namespace
kubectl get ns fabrica-agentes --context=fabrica-dr

# Verificar secrets
kubectl get secrets -n fabrica-agentes --context=fabrica-dr

# Verificar backups replicados
aws s3 ls s3://fabrica-agentes-backups-dr/db/ --region us-west-2 | tail -5
```

---

## 3. Procedimento de Failover

### Fase 1: Comunicacao (0-15 min)

```bash
# 1. Notificar equipe
# Canal: #incident-dr no Slack
# Titulo: [DR ATIVADO] Fabrica de Agentes - Failover para us-west-2

# 2. Atualizar status page
# https://status.fabrica-agentes.com
# Status: Major Outage
# Mensagem: "Estamos migrando para ambiente de backup. ETA: 4 horas"

# 3. Iniciar bridge de incidente
# Zoom/Meet link no canal de incidente
```

### Fase 2: Preparar Ambiente DR (15-60 min)

```bash
# 1. Mudar contexto kubectl
kubectl config use-context fabrica-dr
export KUBECONFIG=~/.kube/config-dr

# 2. Verificar cluster DR
kubectl get nodes
kubectl get pods -A

# 3. Criar namespace se nao existe
kubectl create ns fabrica-agentes --dry-run=client -o yaml | kubectl apply -f -

# 4. Aplicar secrets (devem estar pre-configurados)
kubectl get secrets -n fabrica-agentes
# Se faltando, restaurar do Vault ou backup de secrets

# 5. Aplicar ConfigMaps
kubectl apply -f k8s/configmap.yaml -n fabrica-agentes

# 6. Verificar PVCs
kubectl get pvc -n fabrica-agentes
```

### Fase 3: Restaurar Database (60-120 min)

```bash
# 1. Identificar ultimo backup
BACKUP_FILE=$(aws s3 ls s3://fabrica-agentes-backups-dr/db/ --region us-west-2 \
  | sort | tail -1 | awk '{print $4}')
echo "Using backup: $BACKUP_FILE"

# 2. Se usando RDS Read Replica, promover
aws rds promote-read-replica \
  --db-instance-identifier fabrica-db-dr \
  --region us-west-2

# Aguardar promocao (pode levar 10-30 minutos)
aws rds wait db-instance-available \
  --db-instance-identifier fabrica-db-dr \
  --region us-west-2

# 3. Se restaurando de backup, executar restore
aws s3 cp s3://fabrica-agentes-backups-dr/$BACKUP_FILE /tmp/ --region us-west-2
python scripts/restore_database.py \
  --file /tmp/$BACKUP_FILE \
  --host fabrica-db-dr.XXXXX.us-west-2.rds.amazonaws.com \
  -y

# 4. Verificar integridade
psql -h fabrica-db-dr.XXXXX.us-west-2.rds.amazonaws.com \
  -U postgres -d fabrica_db \
  -c "SELECT count(*) FROM stories;"

# 5. Atualizar ConfigMap com novo endpoint
kubectl patch configmap fabrica-agentes-config -n fabrica-agentes \
  --type merge -p '{"data":{"DATABASE_HOST":"fabrica-db-dr.XXXXX.us-west-2.rds.amazonaws.com"}}'
```

### Fase 4: Restaurar Aplicacao (120-180 min)

```bash
# 1. Aplicar deployments
kubectl apply -f k8s/ -n fabrica-agentes

# 2. Aguardar pods ready
kubectl wait --for=condition=ready pod -l app=fabrica-api -n fabrica-agentes --timeout=300s
kubectl wait --for=condition=ready pod -l app=fabrica-workers -n fabrica-agentes --timeout=300s

# 3. Verificar pods
kubectl get pods -n fabrica-agentes

# 4. Restaurar projetos do S3
kubectl exec -it deploy/fabrica-api -n fabrica-agentes -- \
  aws s3 sync s3://fabrica-agentes-projects-dr/ /app/projects/ --region us-west-2

# 5. Verificar logs
kubectl logs -l app=fabrica-api -n fabrica-agentes --tail=50

# 6. Testar endpoints internos
kubectl exec -it deploy/fabrica-api -n fabrica-agentes -- \
  curl -sf localhost:9001/api/health
```

### Fase 5: Configurar DNS e Ingress (180-210 min)

```bash
# 1. Verificar Ingress no DR
kubectl get ingress -n fabrica-agentes

# 2. Obter IP do Load Balancer DR
DR_LB=$(kubectl get svc -n ingress-nginx ingress-nginx-controller \
  -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')
echo "DR Load Balancer: $DR_LB"

# 3. Atualizar Route53 (failover manual se automatico falhou)
aws route53 change-resource-record-sets \
  --hosted-zone-id ZXXXXXXXXXXXXX \
  --change-batch '{
    "Changes": [{
      "Action": "UPSERT",
      "ResourceRecordSet": {
        "Name": "fabrica-agentes.com",
        "Type": "A",
        "AliasTarget": {
          "HostedZoneId": "ZXXXXXXXXXXXXX",
          "DNSName": "'$DR_LB'",
          "EvaluateTargetHealth": true
        }
      }
    }]
  }'

# 4. Verificar propagacao DNS
dig fabrica-agentes.com

# 5. Limpar cache CloudFront se necessario
aws cloudfront create-invalidation \
  --distribution-id EXXXXXXXXXXXXX \
  --paths "/*"
```

### Fase 6: Verificacao Final (210-240 min)

```bash
# 1. Testar acesso externo
curl -sf https://fabrica-agentes.com/api/health | jq .

# 2. Testar funcionalidades criticas
curl -sf https://fabrica-agentes.com/api/stories | jq '.[:2]'

# 3. Verificar workers processando
kubectl logs -l app=fabrica-workers -n fabrica-agentes --tail=20

# 4. Verificar metricas
kubectl port-forward svc/prometheus-server 9090:9090 -n monitoring &
# Acessar http://localhost:9090

# 5. Executar smoke tests
python tests/smoke_test_dr.py

# 6. Verificar backups funcionando no novo ambiente
python scripts/backup_database.py --dry-run
```

---

## 4. Comunicacao Pos-Failover

```markdown
# Template: Status Page Update

**Titulo:** Servico Restaurado - Operando em Ambiente de Backup

**Mensagem:**
O servico Fabrica de Agentes foi restaurado com sucesso no ambiente de backup.

- **Inicio do incidente:** [TIMESTAMP]
- **Servico restaurado:** [TIMESTAMP]
- **Duracao total:** [HORAS] horas

Algumas funcionalidades podem apresentar lentidao temporaria.
Dados ate [TIMESTAMP DO ULTIMO BACKUP] foram recuperados.

Continuamos monitorando o ambiente. Atualizacoes em 1 hora.
```

---

## 5. Failback (Retorno ao Primario)

### Quando o Ambiente Primario Estiver Disponivel

```bash
# 1. Verificar ambiente primario recuperado
kubectl cluster-info --context=fabrica-prod

# 2. Sincronizar dados do DR para primario
# Fazer backup do ambiente DR atual
python scripts/backup_database.py --upload s3

# 3. Restaurar no primario
python scripts/restore_database.py --from-s3 s3://fabrica-agentes-backups/latest.sql.gz

# 4. Sincronizar projetos
aws s3 sync s3://fabrica-agentes-projects-dr/ s3://fabrica-agentes-projects/

# 5. Validar dados no primario
# Comparar counts de tabelas

# 6. Planejar failback em horario de baixo uso
# Normalmente: Domingo 2:00 AM

# 7. Executar failback
# (Inverter procedimento de failover)

# 8. Monitorar por 24 horas apos failback
```

---

## 6. Checklist DR

### Pre-Failover
- [ ] Confirmar cluster primario indisponivel
- [ ] Aprovar ativacao DR com gestao
- [ ] Notificar equipe e stakeholders
- [ ] Iniciar bridge de incidente

### Durante Failover
- [ ] Cluster DR acessivel
- [ ] Database restaurado
- [ ] Aplicacoes rodando
- [ ] DNS atualizado
- [ ] SSL funcionando
- [ ] Testes de smoke passando

### Pos-Failover
- [ ] Status page atualizado
- [ ] Comunicado enviado
- [ ] Backups configurados no DR
- [ ] Monitoramento ativo
- [ ] Plano de failback definido
- [ ] Post-mortem agendado

---

## 7. Contatos de Emergencia

| Role | Nome | Telefone | Email |
|------|------|----------|-------|
| DR Coordinator | - | - | dr-team@fabrica-agentes.com |
| AWS Support | - | - | Enterprise Support Console |
| Database Admin | - | - | dba@fabrica-agentes.com |
| Network Admin | - | - | network@fabrica-agentes.com |
| CTO (Escalacao) | - | - | cto@fabrica-agentes.com |

---

## 8. Testes de DR

### Frequencia de Testes

| Teste | Frequencia | Ultimo | Proximo |
|-------|------------|--------|---------|
| Failover parcial | Trimestral | - | - |
| Failover completo | Semestral | - | - |
| Restore de backup | Mensal | - | - |
| DNS failover | Trimestral | - | - |

### Script de Teste DR

```bash
# Executar teste de DR (em ambiente de teste)
python scripts/dr_test.py --test full --environment staging

# Verificar resultados
cat /tmp/dr_test_report.json | jq .
```

---

## Historico de Ativacoes DR

| Data | Motivo | Duracao | RPO Real | RTO Real | Notas |
|------|--------|---------|----------|----------|-------|
| - | - | - | - | - | - |

---

*Ultima revisao: 2024-12-29*
*Proxima revisao: 2025-03-29*
