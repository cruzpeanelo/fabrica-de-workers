# Runbook: Database Failover

**Issue #98**: Plano de Disaster Recovery Documentado

**Severidade:** Critical
**RTO:** 1 hora
**RPO:** 1 hora (ultimo backup)
**Ultima Atualizacao:** 2024-12-29

---

## Sumario

Este runbook descreve os procedimentos para failover do PostgreSQL quando o primary esta indisponivel.

---

## 1. Deteccao

### Alertas Prometheus
- `PostgresDown` - PostgreSQL nao responde
- `PostgresHighConnections` - Conexoes > 80% do maximo
- `PostgresReplicationLag` - Replicacao atrasada > 30s

### Verificacao Manual

```bash
# Verificar pods do PostgreSQL
kubectl get pods -n fabrica-agentes -l app=postgresql

# Testar conexao
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT 1" || echo "PRIMARY DOWN"

# Verificar status do Patroni (se usando Patroni)
kubectl exec -it postgresql-0 -n fabrica-agentes -- patronictl list
```

---

## 2. Diagnostico

### 2.1 Identificar Estado do Cluster

```bash
# Status do StatefulSet
kubectl get statefulset postgresql -n fabrica-agentes

# Verificar eventos
kubectl get events -n fabrica-agentes --field-selector involvedObject.name=postgresql-0

# Logs do primary
kubectl logs postgresql-0 -n fabrica-agentes --tail=100

# Verificar PVC
kubectl get pvc -n fabrica-agentes -l app=postgresql
```

### 2.2 Verificar Replicacao

```bash
# No primary, verificar status de replicacao
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT client_addr, state, sync_state, sent_lsn, write_lsn, replay_lsn FROM pg_stat_replication;"

# Verificar lag de replicacao
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT EXTRACT(EPOCH FROM (now() - pg_last_xact_replay_timestamp()))::INT AS lag_seconds;"
```

---

## 3. Resolucao

### Cenario A: Primary Temporariamente Indisponivel

**Acao:** Aguardar recuperacao automatica

```bash
# 1. Verificar se pod esta reiniciando
kubectl get pods -n fabrica-agentes -l app=postgresql -w

# 2. Se reiniciando, aguardar
# Patroni fara failover automatico se necessario

# 3. Verificar status do cluster
kubectl exec -it postgresql-0 -n fabrica-agentes -- patronictl list

# 4. Se recuperou, verificar integridade
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT count(*) FROM stories;"
```

### Cenario B: Failover Manual com Patroni

**Acao:** Promover replica para primary

```bash
# 1. Verificar cluster status
kubectl exec -it postgresql-0 -n fabrica-agentes -- patronictl list

# 2. Iniciar failover manual
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  patronictl failover --master postgresql-0 --candidate postgresql-1 --force

# 3. Confirmar novo primary
kubectl exec -it postgresql-1 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT pg_is_in_recovery();"
# Deve retornar 'f' (false) para o novo primary

# 4. Atualizar servico se necessario
kubectl patch service postgresql -n fabrica-agentes -p \
  '{"spec":{"selector":{"statefulset.kubernetes.io/pod-name":"postgresql-1"}}}'

# 5. Reiniciar aplicacoes para reconectar
kubectl rollout restart deployment/fabrica-api -n fabrica-agentes
```

### Cenario C: Restaurar de Backup

**Acao:** Quando nao ha replica disponivel

```bash
# 1. Identificar ultimo backup valido
aws s3 ls s3://fabrica-agentes-backups/db/ --recursive | sort | tail -10

# 2. Baixar backup mais recente
BACKUP_FILE=$(aws s3 ls s3://fabrica-agentes-backups/db/ --recursive | sort | tail -1 | awk '{print $4}')
aws s3 cp s3://fabrica-agentes-backups/$BACKUP_FILE /tmp/

# 3. Parar aplicacoes que usam o banco
kubectl scale deployment/fabrica-api --replicas=0 -n fabrica-agentes
kubectl scale deployment/fabrica-workers --replicas=0 -n fabrica-agentes

# 4. Recriar PostgreSQL
kubectl delete statefulset postgresql -n fabrica-agentes --cascade=orphan
kubectl delete pvc -l app=postgresql -n fabrica-agentes

# 5. Recriar StatefulSet
kubectl apply -f k8s/postgresql.yaml

# 6. Aguardar pod ready
kubectl wait --for=condition=ready pod/postgresql-0 -n fabrica-agentes --timeout=300s

# 7. Restaurar backup
python scripts/restore_database.py --file $BACKUP_FILE -y

# 8. Verificar integridade
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -d fabrica_db -c "SELECT count(*) FROM stories;"

# 9. Reiniciar aplicacoes
kubectl scale deployment/fabrica-api --replicas=3 -n fabrica-agentes
kubectl scale deployment/fabrica-workers --replicas=2 -n fabrica-agentes

# 10. Verificar conectividade
kubectl exec -it deploy/fabrica-api -n fabrica-agentes -- \
  python -c "from factory.database.connection import get_db; print('DB OK')"
```

### Cenario D: Point-in-Time Recovery (PITR)

**Acao:** Restaurar para momento especifico

```bash
# 1. Identificar timestamp desejado
# Formato: YYYY-MM-DD HH:MM:SS
TARGET_TIME="2024-12-29 14:30:00"

# 2. Baixar backup base e WAL logs
aws s3 sync s3://fabrica-agentes-backups/db/base/ /tmp/restore/base/
aws s3 sync s3://fabrica-agentes-backups/db/wal/ /tmp/restore/wal/

# 3. Parar PostgreSQL atual
kubectl scale statefulset/postgresql --replicas=0 -n fabrica-agentes

# 4. Configurar recovery.conf (PostgreSQL < 12) ou postgresql.auto.conf
cat << EOF > /tmp/recovery.signal
EOF

cat << EOF >> /tmp/postgresql.auto.conf
restore_command = 'cp /var/lib/postgresql/wal/%f %p'
recovery_target_time = '$TARGET_TIME'
recovery_target_action = 'promote'
EOF

# 5. Copiar arquivos para PVC e reiniciar
# (Procedimento especifico do ambiente)

# 6. Verificar recovery
kubectl logs postgresql-0 -n fabrica-agentes | grep -i recovery
```

---

## 4. Verificacao Pos-Failover

```bash
# 1. Verificar novo primary
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT pg_is_in_recovery();"

# 2. Verificar integridade dos dados
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -d fabrica_db -c "
    SELECT table_name,
           pg_size_pretty(pg_relation_size(quote_ident(table_name))) as size
    FROM information_schema.tables
    WHERE table_schema = 'public'
    ORDER BY pg_relation_size(quote_ident(table_name)) DESC
    LIMIT 10;
  "

# 3. Verificar conexoes das aplicacoes
kubectl exec -it postgresql-0 -n fabrica-agentes -- \
  psql -U postgres -c "SELECT application_name, state, query FROM pg_stat_activity WHERE datname = 'fabrica_db';"

# 4. Testar operacoes CRUD
curl -X POST https://fabrica-agentes.com/api/stories \
  -H "Content-Type: application/json" \
  -d '{"title":"Test Story","persona":"Test"}'

# 5. Verificar backups funcionando
python scripts/backup_database.py --dry-run

# 6. Monitorar metricas
kubectl port-forward svc/prometheus-server 9090:9090 -n monitoring &
# Acessar: http://localhost:9090/graph?g0.expr=pg_up
```

---

## 5. Checklist Pos-Failover

- [ ] Novo primary funcionando
- [ ] Replicacao configurada para nova replica
- [ ] Aplicacoes reconectadas
- [ ] Backups funcionando
- [ ] Monitoramento ativo
- [ ] Documentar incidente

---

## 6. Prevencao

### Configuracoes Recomendadas

```yaml
# postgresql.conf
max_connections = 200
shared_buffers = 256MB
wal_level = replica
max_wal_senders = 5
max_replication_slots = 5
hot_standby = on
synchronous_commit = on
```

### Monitoramento

- Configurar alertas de replicacao lag
- Monitorar espaco em disco
- Testar failover mensalmente
- Verificar backups semanalmente

---

## Historico de Incidentes

| Data | Causa | Resolucao | RPO Real | RTO Real |
|------|-------|-----------|----------|----------|
| - | - | - | - | - |

---

*Ultima revisao: 2024-12-29*
