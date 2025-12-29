# Disaster Recovery Plan - Fabrica de Agentes

**Issue #98**: Plano de Disaster Recovery Documentado

**Versao:** 1.0
**Ultima Atualizacao:** 2024-12-29
**Responsavel:** DevOps Team

---

## Sumario Executivo

Este documento define o Plano de Recuperacao de Desastres (DR) para a plataforma Fabrica de Agentes. O objetivo e garantir a continuidade do negocio em caso de falhas criticas, minimizando o tempo de inatividade e a perda de dados.

---

## 1. Objetivos de Recuperacao

### 1.1 RTO (Recovery Time Objective)

| Cenario | RTO | Descricao |
|---------|-----|-----------|
| Falha de componente unico | 15 minutos | API, Worker, ou servico individual |
| Falha de banco de dados | 1 hora | PostgreSQL primary |
| Falha de cluster completo | 4 horas | Todo o ambiente Kubernetes |
| Desastre regional | 8 horas | Datacenter/regiao inteira |

### 1.2 RPO (Recovery Point Objective)

| Tipo de Dado | RPO | Estrategia |
|--------------|-----|------------|
| Banco de dados | 1 hora | Backups horarios automaticos |
| Configuracoes | 0 | Versionadas no Git |
| Logs e metricas | 24 horas | Retencao em cloud storage |
| Arquivos de projeto | 1 hora | Sync com S3 |

---

## 2. Arquitetura de Alta Disponibilidade

### 2.1 Componentes Criticos

```
                                    ┌─────────────────┐
                                    │   CloudFlare    │
                                    │   (CDN/WAF)     │
                                    └────────┬────────┘
                                             │
                    ┌────────────────────────┼────────────────────────┐
                    │                        │                        │
           ┌────────┴────────┐      ┌────────┴────────┐      ┌────────┴────────┐
           │   Ingress AZ-A  │      │   Ingress AZ-B  │      │   Ingress AZ-C  │
           └────────┬────────┘      └────────┬────────┘      └────────┬────────┘
                    │                        │                        │
           ┌────────┴────────┐      ┌────────┴────────┐      ┌────────┴────────┐
           │    API Pod      │      │    API Pod      │      │    API Pod      │
           │   (replica 1)   │      │   (replica 2)   │      │   (replica 3)   │
           └────────┬────────┘      └────────┬────────┘      └────────┬────────┘
                    │                        │                        │
                    └────────────────────────┼────────────────────────┘
                                             │
                    ┌────────────────────────┼────────────────────────┐
                    │                        │                        │
           ┌────────┴────────┐      ┌────────┴────────┐      ┌────────┴────────┐
           │   PostgreSQL    │      │   PostgreSQL    │      │     Redis       │
           │   (Primary)     │──────│   (Replica)     │      │   (Cluster)     │
           └─────────────────┘      └─────────────────┘      └─────────────────┘
```

### 2.2 Redundancia por Camada

| Camada | Estrategia de HA | Failover |
|--------|------------------|----------|
| Load Balancer | Multi-AZ | Automatico (<30s) |
| API | 3+ replicas, PodDisruptionBudget | Automatico |
| Workers | 2+ replicas por tipo | Automatico |
| PostgreSQL | Primary + Replica sincrona | Automatico (Patroni) |
| Redis | Cluster 3 nodes | Automatico |
| Storage | S3 Cross-Region Replication | Automatico |

---

## 3. Estrategia de Backup

### 3.1 Banco de Dados PostgreSQL

| Tipo | Frequencia | Retencao | Destino |
|------|------------|----------|---------|
| Full Backup | Diario (2:00 AM) | 30 dias | S3 + Cross-region |
| WAL Archiving | Continuo | 7 dias | S3 |
| Snapshot | Semanal | 90 dias | S3 Glacier |

**Comandos de Backup:**

```bash
# Backup manual
python scripts/backup_database.py --upload s3

# Verificar backups
aws s3 ls s3://fabrica-agentes-backups/db/

# Listar backups locais
ls -la /backups/
```

### 3.2 Configuracoes e Codigo

| Recurso | Backup | Versionamento |
|---------|--------|---------------|
| Codigo fonte | GitHub | Git history |
| Kubernetes manifests | GitOps (ArgoCD) | Git history |
| Secrets | Vault / AWS Secrets Manager | Versionado |
| Configuracoes | ConfigMaps versionados | Git |

### 3.3 Arquivos de Projeto

```bash
# Sync automatico para S3
aws s3 sync /app/projects/ s3://fabrica-agentes-projects/ --delete

# Restaurar projetos
aws s3 sync s3://fabrica-agentes-projects/ /app/projects/
```

---

## 4. Procedimentos de Recuperacao

### 4.1 Falha de API/Worker Pod

**Deteccao:** Prometheus alerta `FactoryAPIDown` ou `NoActiveWorkers`

**Recuperacao Automatica:**
- Kubernetes reinicia pod automaticamente
- HPA cria novas replicas se necessario
- Tempo estimado: < 2 minutos

**Recuperacao Manual (se necessario):**

```bash
# Verificar status dos pods
kubectl get pods -n fabrica-agentes

# Reiniciar deployment
kubectl rollout restart deployment/fabrica-api -n fabrica-agentes

# Verificar logs
kubectl logs -l app=fabrica-api -n fabrica-agentes --tail=100

# Escalar manualmente
kubectl scale deployment/fabrica-api --replicas=5 -n fabrica-agentes
```

### 4.2 Falha de Banco de Dados

**Deteccao:** Prometheus alerta `PostgresDown`

**Cenario A: Failover para Replica**

```bash
# Verificar status do cluster Patroni
kubectl exec -it postgres-0 -n fabrica-agentes -- patronictl list

# Promover replica manualmente (se necessario)
kubectl exec -it postgres-1 -n fabrica-agentes -- patronictl failover

# Verificar novo primary
kubectl exec -it postgres-0 -n fabrica-agentes -- psql -c "SELECT pg_is_in_recovery();"
```

**Cenario B: Restaurar de Backup**

```bash
# 1. Identificar ultimo backup valido
aws s3 ls s3://fabrica-agentes-backups/db/ --recursive | sort | tail -10

# 2. Baixar backup
aws s3 cp s3://fabrica-agentes-backups/db/backup_fabrica_db_20241229_020000.sql.gz /backups/

# 3. Restaurar
python scripts/restore_database.py --file backup_fabrica_db_20241229_020000.sql.gz -y

# 4. Verificar integridade
psql -h localhost -U postgres -d fabrica_db -c "SELECT count(*) FROM stories;"
```

### 4.3 Falha de Redis

**Deteccao:** Prometheus alerta `RedisDown`

**Recuperacao:**

```bash
# Verificar status do cluster Redis
kubectl exec -it redis-0 -n fabrica-agentes -- redis-cli cluster info

# Verificar nodes
kubectl exec -it redis-0 -n fabrica-agentes -- redis-cli cluster nodes

# Failover manual (se necessario)
kubectl exec -it redis-0 -n fabrica-agentes -- redis-cli cluster failover

# Recriar cluster (ultimo recurso)
kubectl delete statefulset redis -n fabrica-agentes
kubectl apply -f k8s/redis-cluster.yaml
```

### 4.4 Falha de Cluster Kubernetes Completo

**Deteccao:** Monitoramento externo (Datadog/New Relic)

**Pre-requisitos:**
- Cluster DR em regiao secundaria
- DNS com failover configurado
- Backups atualizados em S3

**Procedimento:**

```bash
# 1. Ativar cluster DR
kubectl config use-context fabrica-dr-cluster

# 2. Verificar namespace
kubectl get ns fabrica-agentes || kubectl create ns fabrica-agentes

# 3. Aplicar configuracoes
kubectl apply -k k8s/overlays/dr/

# 4. Restaurar banco de dados
python scripts/restore_database.py --from-s3 s3://fabrica-agentes-backups/db/latest.sql.gz -y

# 5. Restaurar projetos
aws s3 sync s3://fabrica-agentes-projects/ /app/projects/

# 6. Verificar aplicacao
kubectl get pods -n fabrica-agentes
curl -f https://dr.fabrica-agentes.com/api/health

# 7. Atualizar DNS (Route53 failover ou manual)
aws route53 change-resource-record-sets --hosted-zone-id XXXXX --change-batch file://dns-failover.json
```

### 4.5 Corrupcao de Dados

**Deteccao:** Alertas de inconsistencia ou relato de usuarios

**Procedimento:**

```bash
# 1. IMEDIATAMENTE: Pausar workers para evitar mais corrupcao
kubectl scale deployment/fabrica-workers --replicas=0 -n fabrica-agentes

# 2. Identificar extensao do problema
psql -h localhost -U postgres -d fabrica_db -c "
  SELECT table_name, count(*)
  FROM information_schema.tables t
  JOIN pg_stat_user_tables s ON t.table_name = s.relname
  WHERE table_schema = 'public'
  GROUP BY table_name;
"

# 3. Identificar backup pre-corrupcao
# Analise logs para determinar quando corrupcao iniciou
kubectl logs -l app=fabrica-api --since=24h | grep -i error

# 4. Restaurar backup point-in-time
python scripts/restore_database.py \
  --from-s3 s3://fabrica-agentes-backups/db/backup_fabrica_db_20241228_140000.sql.gz \
  --target-db fabrica_db_recovery \
  -y

# 5. Comparar e migrar dados validos
# (Script customizado baseado no tipo de corrupcao)

# 6. Retomar operacoes
kubectl scale deployment/fabrica-workers --replicas=3 -n fabrica-agentes
```

---

## 5. Comunicacao Durante Incidentes

### 5.1 Cadeia de Escalacao

| Nivel | Tempo | Acao | Contato |
|-------|-------|------|---------|
| L1 | 0-15 min | Alertas automaticos | PagerDuty on-call |
| L2 | 15-30 min | Escalar para engenheiro senior | Tech Lead |
| L3 | 30-60 min | Envolver gestao | Engineering Manager |
| L4 | 1+ hora | Comunicar stakeholders | CTO / CEO |

### 5.2 Templates de Comunicacao

**Status Page (usuarios):**
```
[INCIDENTE] Fabrica de Agentes - Degradacao de Servico

Estamos investigando problemas de conectividade com a plataforma.
Algumas funcionalidades podem estar indisponiveis.

Inicio: [TIMESTAMP]
Status: Investigando
Proximo update: Em 30 minutos

---

[RESOLVIDO] Fabrica de Agentes - Servico Restaurado

O servico foi totalmente restaurado as [TIMESTAMP].
Causa: [DESCRICAO BREVE]
Impacto: [DURACAO] de indisponibilidade parcial

Pedimos desculpas pelo inconveniente.
```

**Comunicacao Interna (Slack):**
```
:rotating_light: **INCIDENTE EM ANDAMENTO** :rotating_light:

**Servico:** Fabrica de Agentes
**Severidade:** [SEV1/SEV2/SEV3]
**Inicio:** [TIMESTAMP]
**Comandante:** @nome

**Status Atual:**
- [Descricao do problema]
- [Acoes sendo tomadas]

**Canal de Comunicacao:** #incident-XXXX
**Bridge:** [Link do meet]

Proximo update em 15 minutos.
```

---

## 6. Testes de DR

### 6.1 Cronograma de Testes

| Teste | Frequencia | Duracao | Ambiente |
|-------|------------|---------|----------|
| Failover de Pod | Semanal (automatico) | 5 min | Producao |
| Restore de Backup | Mensal | 30 min | Staging |
| Failover de DB | Trimestral | 1 hora | Staging |
| DR Completo | Semestral | 4 horas | DR Site |

### 6.2 Executando Testes

```bash
# Rodar suite de testes DR
python scripts/dr_test.py --test all

# Testar apenas backup/restore
python scripts/dr_test.py --test backup

# Testar failover de pods
python scripts/dr_test.py --test failover

# Modo dry-run (sem executar acoes)
python scripts/dr_test.py --test all --dry-run
```

### 6.3 Checklist Pos-Teste

- [ ] Todos os testes passaram?
- [ ] Tempo de recuperacao dentro do RTO?
- [ ] Perda de dados dentro do RPO?
- [ ] Documentacao atualizada?
- [ ] Runbooks precisam de ajuste?
- [ ] Alertas dispararam corretamente?

---

## 7. Manutencao do Plano

### 7.1 Revisao Periodica

- **Mensal:** Revisar metricas de backup e alertas
- **Trimestral:** Revisar e atualizar procedimentos
- **Semestral:** Revisar arquitetura e dependencias
- **Anual:** Auditoria completa do plano DR

### 7.2 Atualizacoes do Documento

Qualquer alteracao neste documento deve:
1. Passar por revisao de pelo menos 2 engenheiros
2. Ser testada em ambiente de staging
3. Ser comunicada ao time via Slack
4. Ser versionada no Git

---

## 8. Contatos de Emergencia

| Role | Nome | Telefone | Email |
|------|------|----------|-------|
| Primary On-Call | (PagerDuty) | - | oncall@fabrica-agentes.com |
| Backup On-Call | (PagerDuty) | - | oncall@fabrica-agentes.com |
| Tech Lead | - | - | tech-lead@fabrica-agentes.com |
| Engineering Manager | - | - | eng-manager@fabrica-agentes.com |
| AWS Support | - | - | (Console AWS) |

---

## Apendice A: Runbooks Detalhados

Links para runbooks especificos:
- [Runbook: API Down](./runbooks/api-down.md)
- [Runbook: Database Failover](./runbooks/db-failover.md)
- [Runbook: Redis Recovery](./runbooks/redis-recovery.md)
- [Runbook: Full DR](./runbooks/full-dr.md)

---

## Apendice B: Ferramentas Necessarias

```bash
# Instalar ferramentas de DR
pip install awscli boto3 psycopg2-binary

# Configurar AWS CLI
aws configure

# Configurar kubectl
aws eks update-kubeconfig --name fabrica-cluster --region us-east-1
```

---

*Documento mantido pelo time de DevOps. Para duvidas, contate devops@fabrica-agentes.com*
