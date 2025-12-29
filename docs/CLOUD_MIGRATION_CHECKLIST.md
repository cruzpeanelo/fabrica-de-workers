# Cloud Migration Checklist

> Issue #126 - Migracao para Cloud (Checklist de Deploy)

This document provides a comprehensive checklist for deploying the Fabrica de Agentes platform to cloud providers (AWS, GCP, Azure).

---

## Table of Contents

1. [Pre-Migration Assessment](#1-pre-migration-assessment)
2. [Infrastructure Setup](#2-infrastructure-setup)
3. [Security Configuration](#3-security-configuration)
4. [Database Migration](#4-database-migration)
5. [Application Deployment](#5-application-deployment)
6. [Networking & DNS](#6-networking--dns)
7. [Monitoring & Logging](#7-monitoring--logging)
8. [Performance Optimization](#8-performance-optimization)
9. [Disaster Recovery](#9-disaster-recovery)
10. [Compliance & Governance](#10-compliance--governance)
11. [Terraform Templates](#11-terraform-templates)
12. [Provider-Specific Guides](#12-provider-specific-guides)

---

## 1. Pre-Migration Assessment

### 1.1 Application Analysis

- [ ] Document all application components and dependencies
- [ ] Identify stateful vs stateless components
- [ ] Map current resource utilization (CPU, Memory, Storage, Network)
- [ ] List all external integrations (APIs, Webhooks, Third-party services)
- [ ] Document database schema and size
- [ ] Identify file storage requirements

### 1.2 Environment Inventory

| Component | Current State | Target State | Notes |
|-----------|--------------|--------------|-------|
| Application Server | Local/VM | Container/K8s | |
| PostgreSQL | Local/Docker | Managed RDS | |
| Redis | Local/Docker | ElastiCache/MemoryStore | |
| Object Storage | Local/MinIO | S3/GCS/Blob | |
| Load Balancer | None/Nginx | ALB/Cloud LB | |
| CDN | None | CloudFront/Cloud CDN | |

### 1.3 Cost Estimation

- [ ] Calculate estimated cloud costs using provider calculator
- [ ] Compare pricing across AWS, GCP, Azure
- [ ] Consider reserved instances vs on-demand
- [ ] Factor in data transfer costs
- [ ] Include backup and DR costs

### 1.4 Migration Timeline

| Phase | Duration | Description |
|-------|----------|-------------|
| Planning | 1-2 weeks | Assessment and architecture |
| Infrastructure | 1-2 weeks | IaC setup, VPC, security |
| Development/Staging | 1-2 weeks | Deploy non-prod environments |
| Data Migration | 1 week | Database and file migration |
| Production | 1 week | Go-live with rollback plan |
| Optimization | 2+ weeks | Performance tuning |

---

## 2. Infrastructure Setup

### 2.1 Network Architecture

- [ ] Design VPC/VNet architecture
- [ ] Plan subnet structure (public, private, database)
- [ ] Configure NAT Gateway for private subnets
- [ ] Set up VPC peering if needed
- [ ] Configure Internet Gateway

```
VPC CIDR: 10.0.0.0/16

Subnets:
├── Public (10.0.1.0/24, 10.0.2.0/24, 10.0.3.0/24)
│   └── Load Balancer, NAT Gateway, Bastion
├── Private (10.0.10.0/24, 10.0.11.0/24, 10.0.12.0/24)
│   └── Application servers, Workers
└── Database (10.0.20.0/24, 10.0.21.0/24, 10.0.22.0/24)
    └── RDS, ElastiCache
```

### 2.2 Compute Resources

- [ ] Choose instance types based on workload
- [ ] Configure Auto Scaling Groups
- [ ] Set up container orchestration (EKS/GKE/AKS)
- [ ] Define scaling policies

**Recommended Instance Types:**

| Workload | AWS | GCP | Azure |
|----------|-----|-----|-------|
| Application | t3.medium, t3.large | e2-medium, e2-standard-2 | B2s, B2ms |
| Workers | c5.large, c5.xlarge | c2-standard-4 | F4s_v2 |
| Database | db.r5.large | db-custom-2-7680 | GP_Gen5_2 |

### 2.3 Container Registry

- [ ] Set up ECR/GCR/ACR
- [ ] Configure image scanning
- [ ] Set up image lifecycle policies
- [ ] Create CI/CD pipeline for image builds

### 2.4 Kubernetes (if applicable)

- [ ] Create managed Kubernetes cluster
- [ ] Configure node pools (system, application, workers)
- [ ] Set up cluster autoscaler
- [ ] Install ingress controller (nginx/traefik)
- [ ] Configure namespaces (development, staging, production)

---

## 3. Security Configuration

### 3.1 Identity & Access Management

- [ ] Create IAM roles with least privilege
- [ ] Set up service accounts for applications
- [ ] Configure IAM policies for each service
- [ ] Enable MFA for all admin accounts
- [ ] Set up SSO integration if needed

**Required IAM Roles:**

| Role | Purpose | Permissions |
|------|---------|-------------|
| fabrica-app-role | Application server | S3, RDS, ElastiCache access |
| fabrica-worker-role | Background workers | S3, SQS, CloudWatch |
| fabrica-ci-role | CI/CD pipeline | ECR push, EKS deploy |
| fabrica-admin-role | Admin operations | Full access with MFA |

### 3.2 Network Security

- [ ] Configure Security Groups / Network Security Groups
- [ ] Set up Web Application Firewall (WAF)
- [ ] Enable DDoS protection
- [ ] Configure VPC Flow Logs
- [ ] Set up AWS Shield / Cloud Armor

**Security Group Rules:**

```yaml
Application SG:
  Inbound:
    - 443 from ALB SG
    - 22 from Bastion SG
  Outbound:
    - 5432 to Database SG
    - 6379 to Redis SG
    - 443 to Internet

Database SG:
  Inbound:
    - 5432 from Application SG
  Outbound:
    - None (or minimal)
```

### 3.3 Secrets Management

- [ ] Set up AWS Secrets Manager / GCP Secret Manager / Azure Key Vault
- [ ] Migrate all secrets from environment variables
- [ ] Configure automatic secret rotation
- [ ] Set up secret access logging

**Secrets to Migrate:**

- [ ] Database credentials
- [ ] API keys (Anthropic, Stripe, etc.)
- [ ] JWT secret key
- [ ] OAuth/SSO credentials
- [ ] S3/Storage access keys

### 3.4 SSL/TLS Certificates

- [ ] Obtain SSL certificates (ACM, Let's Encrypt, etc.)
- [ ] Configure HTTPS on load balancer
- [ ] Set up certificate auto-renewal
- [ ] Enable TLS 1.2+ only

### 3.5 Encryption

- [ ] Enable encryption at rest for all storage
- [ ] Enable encryption in transit (TLS)
- [ ] Configure database encryption
- [ ] Set up KMS keys for sensitive data

---

## 4. Database Migration

### 4.1 Pre-Migration

- [ ] Create database backup
- [ ] Document current schema and indexes
- [ ] Plan for minimal downtime
- [ ] Test migration procedure in staging

### 4.2 Managed Database Setup

- [ ] Create RDS/Cloud SQL/Azure DB instance
- [ ] Configure Multi-AZ deployment
- [ ] Set up automated backups
- [ ] Configure maintenance windows
- [ ] Set up read replicas if needed

**PostgreSQL Configuration:**

```yaml
Engine: PostgreSQL 16
Instance: db.r5.large (or equivalent)
Storage: 100GB gp3 (auto-scaling enabled)
Multi-AZ: Yes
Backup: Daily, 7-day retention
Encryption: AES-256
```

### 4.3 Migration Steps

1. [ ] Create target database instance
2. [ ] Set up replication from source
3. [ ] Verify data consistency
4. [ ] Perform cutover during maintenance window
5. [ ] Update application connection strings
6. [ ] Validate application functionality
7. [ ] Decommission old database

### 4.4 Post-Migration

- [ ] Verify all data migrated correctly
- [ ] Run application tests
- [ ] Monitor performance
- [ ] Update connection pool settings

---

## 5. Application Deployment

### 5.1 Container Build

```dockerfile
# Dockerfile for production
FROM python:3.11-slim

WORKDIR /app

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application
COPY . .

# Set environment
ENV PYTHONUNBUFFERED=1
ENV ENVIRONMENT=production

# Health check
HEALTHCHECK --interval=30s --timeout=10s --retries=3 \
    CMD curl -f http://localhost:9001/health || exit 1

# Run application
CMD ["uvicorn", "factory.dashboard.app_v6_agile:app", "--host", "0.0.0.0", "--port", "9001"]
```

### 5.2 Kubernetes Deployment

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: fabrica-app
  namespace: production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: fabrica
  template:
    metadata:
      labels:
        app: fabrica
    spec:
      containers:
      - name: fabrica
        image: fabrica-agentes:latest
        ports:
        - containerPort: 9001
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        envFrom:
        - secretRef:
            name: fabrica-secrets
        - configMapRef:
            name: fabrica-config
        livenessProbe:
          httpGet:
            path: /health
            port: 9001
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 9001
          initialDelaySeconds: 5
          periodSeconds: 5
```

### 5.3 Environment Variables

```yaml
# Production environment configuration
ENVIRONMENT: production
DEBUG: false
LOG_LEVEL: INFO

# Database
DATABASE_URL: postgresql://...@rds.amazonaws.com:5432/fabrica_db

# Redis
REDIS_URL: redis://...elasticache.amazonaws.com:6379

# S3 Storage
S3_ENDPOINT: https://s3.amazonaws.com
S3_BUCKET: fabrica-prod-uploads
AWS_REGION: us-east-1

# Security
JWT_SECRET_KEY: ${secrets.jwt_secret}
ANTHROPIC_API_KEY: ${secrets.anthropic_api_key}

# Feature Flags
FEATURE_FLAGS_ENABLED: true
FEATURE_FLAGS_ENV: production
```

### 5.4 Deployment Checklist

- [ ] Build production Docker image
- [ ] Push to container registry
- [ ] Update Kubernetes manifests
- [ ] Apply database migrations
- [ ] Deploy to staging first
- [ ] Run integration tests
- [ ] Deploy to production
- [ ] Verify health checks
- [ ] Monitor error rates

---

## 6. Networking & DNS

### 6.1 Load Balancer

- [ ] Create Application Load Balancer
- [ ] Configure health checks
- [ ] Set up target groups
- [ ] Configure SSL termination
- [ ] Set up connection draining

### 6.2 DNS Configuration

- [ ] Create hosted zone in Route 53 / Cloud DNS
- [ ] Configure A/CNAME records
- [ ] Set up health-based routing
- [ ] Configure failover if multi-region

```
fabrica.example.com     -> ALB/Load Balancer
api.fabrica.example.com -> ALB/Load Balancer
*.tenant.fabrica.example.com -> ALB (wildcard for multi-tenant)
```

### 6.3 CDN Setup

- [ ] Create CloudFront / Cloud CDN distribution
- [ ] Configure origin (S3 for static, ALB for dynamic)
- [ ] Set cache policies
- [ ] Configure custom headers
- [ ] Set up geo-restrictions if needed

---

## 7. Monitoring & Logging

### 7.1 Application Monitoring

- [ ] Set up CloudWatch / Stackdriver / Azure Monitor
- [ ] Configure custom metrics
- [ ] Create dashboards
- [ ] Set up alarms for critical metrics

**Key Metrics to Monitor:**

| Metric | Threshold | Action |
|--------|-----------|--------|
| CPU Utilization | > 80% for 5min | Scale out |
| Memory Usage | > 85% | Alert |
| Request Latency P99 | > 2s | Investigate |
| Error Rate | > 1% | Alert + Page |
| Database Connections | > 80% pool | Scale/Investigate |

### 7.2 Log Management

- [ ] Configure centralized logging (CloudWatch Logs, ELK, etc.)
- [ ] Set log retention policies
- [ ] Create log-based metrics
- [ ] Set up log alerts

### 7.3 Tracing

- [ ] Implement distributed tracing (X-Ray, Jaeger)
- [ ] Configure trace sampling
- [ ] Set up trace analysis

### 7.4 Alerting

- [ ] Configure alert channels (PagerDuty, Slack, Email)
- [ ] Define escalation policies
- [ ] Create runbooks for common alerts
- [ ] Set up on-call rotations

---

## 8. Performance Optimization

### 8.1 Application Performance

- [ ] Enable response compression
- [ ] Configure connection pooling
- [ ] Implement caching strategy
- [ ] Optimize database queries
- [ ] Enable HTTP/2

### 8.2 Database Optimization

- [ ] Analyze slow queries
- [ ] Create necessary indexes
- [ ] Configure connection pool size
- [ ] Set up query caching
- [ ] Consider read replicas for scaling

### 8.3 Caching Strategy

```
Cache Hierarchy:
1. Browser Cache (static assets) - 1 year
2. CDN Cache (static + API cache) - 1 hour
3. Redis Cache (session, frequent queries) - 5-60 min
4. Database Query Cache - 1-5 min
```

### 8.4 Auto Scaling

```yaml
# EKS HPA Configuration
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: fabrica-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: fabrica-app
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

---

## 9. Disaster Recovery

### 9.1 Backup Strategy

| Component | Backup Frequency | Retention | RPO |
|-----------|-----------------|-----------|-----|
| Database | Daily + WAL | 30 days | 1 hour |
| Object Storage | Cross-region | Infinite | 0 |
| Configuration | Git | Infinite | 0 |
| Secrets | Versioned | 30 days | 0 |

### 9.2 Recovery Procedures

- [ ] Document database restore procedure
- [ ] Test restore from backup monthly
- [ ] Create runbooks for common failures
- [ ] Define RTO and RPO targets

### 9.3 Multi-Region Setup (Optional)

- [ ] Set up secondary region
- [ ] Configure database replication
- [ ] Set up DNS failover
- [ ] Test failover procedure quarterly

---

## 10. Compliance & Governance

### 10.1 Data Privacy

- [ ] Implement data encryption
- [ ] Configure data retention policies
- [ ] Set up data deletion procedures
- [ ] Document data processing activities

### 10.2 Access Control

- [ ] Implement RBAC
- [ ] Configure audit logging
- [ ] Set up access reviews
- [ ] Document access policies

### 10.3 Compliance Checklist

- [ ] LGPD (Brazil) compliance
- [ ] GDPR (EU) if applicable
- [ ] SOC 2 controls
- [ ] PCI DSS if handling payments

---

## 11. Terraform Templates

### 11.1 AWS Infrastructure

```hcl
# terraform/aws/main.tf

terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
  backend "s3" {
    bucket = "fabrica-terraform-state"
    key    = "production/terraform.tfstate"
    region = "us-east-1"
  }
}

provider "aws" {
  region = var.aws_region
}

# VPC Module
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "5.0.0"

  name = "fabrica-vpc"
  cidr = "10.0.0.0/16"

  azs             = ["us-east-1a", "us-east-1b", "us-east-1c"]
  private_subnets = ["10.0.10.0/24", "10.0.11.0/24", "10.0.12.0/24"]
  public_subnets  = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]

  enable_nat_gateway = true
  single_nat_gateway = false

  tags = {
    Environment = "production"
    Project     = "fabrica-agentes"
  }
}

# EKS Cluster
module "eks" {
  source  = "terraform-aws-modules/eks/aws"
  version = "19.0.0"

  cluster_name    = "fabrica-cluster"
  cluster_version = "1.28"

  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets

  eks_managed_node_groups = {
    main = {
      min_size     = 2
      max_size     = 10
      desired_size = 3

      instance_types = ["t3.large"]
      capacity_type  = "ON_DEMAND"
    }
  }
}

# RDS PostgreSQL
module "rds" {
  source  = "terraform-aws-modules/rds/aws"
  version = "6.0.0"

  identifier = "fabrica-db"

  engine               = "postgres"
  engine_version       = "16"
  family               = "postgres16"
  major_engine_version = "16"
  instance_class       = "db.r5.large"

  allocated_storage     = 100
  max_allocated_storage = 500

  db_name  = "fabrica_db"
  username = "fabrica"
  port     = 5432

  multi_az               = true
  db_subnet_group_name   = module.vpc.database_subnet_group_name
  vpc_security_group_ids = [module.security_group_db.security_group_id]

  backup_retention_period = 7
  deletion_protection     = true
}

# ElastiCache Redis
resource "aws_elasticache_cluster" "redis" {
  cluster_id           = "fabrica-redis"
  engine               = "redis"
  node_type            = "cache.t3.medium"
  num_cache_nodes      = 1
  parameter_group_name = "default.redis7"
  port                 = 6379

  subnet_group_name  = aws_elasticache_subnet_group.redis.name
  security_group_ids = [module.security_group_redis.security_group_id]
}

# S3 Bucket for uploads
resource "aws_s3_bucket" "uploads" {
  bucket = "fabrica-prod-uploads"

  tags = {
    Environment = "production"
  }
}

resource "aws_s3_bucket_versioning" "uploads" {
  bucket = aws_s3_bucket.uploads.id
  versioning_configuration {
    status = "Enabled"
  }
}
```

### 11.2 GCP Infrastructure

```hcl
# terraform/gcp/main.tf

terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
  backend "gcs" {
    bucket = "fabrica-terraform-state"
    prefix = "production"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# VPC Network
resource "google_compute_network" "main" {
  name                    = "fabrica-vpc"
  auto_create_subnetworks = false
}

resource "google_compute_subnetwork" "private" {
  name          = "fabrica-private"
  ip_cidr_range = "10.0.0.0/24"
  region        = var.region
  network       = google_compute_network.main.id
}

# GKE Cluster
resource "google_container_cluster" "main" {
  name     = "fabrica-cluster"
  location = var.region

  network    = google_compute_network.main.name
  subnetwork = google_compute_subnetwork.private.name

  initial_node_count = 1
  remove_default_node_pool = true

  master_auth {
    client_certificate_config {
      issue_client_certificate = false
    }
  }
}

resource "google_container_node_pool" "main" {
  name       = "fabrica-nodes"
  cluster    = google_container_cluster.main.name
  location   = var.region
  node_count = 3

  node_config {
    machine_type = "e2-standard-4"
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]
  }

  autoscaling {
    min_node_count = 2
    max_node_count = 10
  }
}

# Cloud SQL PostgreSQL
resource "google_sql_database_instance" "main" {
  name             = "fabrica-db"
  database_version = "POSTGRES_16"
  region           = var.region

  settings {
    tier = "db-custom-2-7680"

    backup_configuration {
      enabled            = true
      binary_log_enabled = false
      start_time         = "04:00"
    }

    ip_configuration {
      ipv4_enabled    = false
      private_network = google_compute_network.main.id
    }

    availability_type = "REGIONAL"
  }
}

# Memorystore Redis
resource "google_redis_instance" "main" {
  name           = "fabrica-redis"
  tier           = "STANDARD_HA"
  memory_size_gb = 1
  region         = var.region

  authorized_network = google_compute_network.main.id
}

# Cloud Storage
resource "google_storage_bucket" "uploads" {
  name          = "fabrica-prod-uploads"
  location      = var.region
  storage_class = "STANDARD"

  versioning {
    enabled = true
  }
}
```

### 11.3 Azure Infrastructure

```hcl
# terraform/azure/main.tf

terraform {
  required_version = ">= 1.0"
  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
  }
  backend "azurerm" {
    resource_group_name  = "fabrica-terraform"
    storage_account_name = "fabricatfstate"
    container_name       = "tfstate"
    key                  = "production.tfstate"
  }
}

provider "azurerm" {
  features {}
}

# Resource Group
resource "azurerm_resource_group" "main" {
  name     = "fabrica-production"
  location = var.location
}

# Virtual Network
resource "azurerm_virtual_network" "main" {
  name                = "fabrica-vnet"
  address_space       = ["10.0.0.0/16"]
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
}

resource "azurerm_subnet" "private" {
  name                 = "fabrica-private"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = ["10.0.1.0/24"]
}

# AKS Cluster
resource "azurerm_kubernetes_cluster" "main" {
  name                = "fabrica-aks"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  dns_prefix          = "fabrica"

  default_node_pool {
    name       = "default"
    node_count = 3
    vm_size    = "Standard_D2s_v3"
  }

  identity {
    type = "SystemAssigned"
  }
}

# Azure Database for PostgreSQL
resource "azurerm_postgresql_flexible_server" "main" {
  name                   = "fabrica-db"
  resource_group_name    = azurerm_resource_group.main.name
  location               = azurerm_resource_group.main.location
  version                = "16"
  delegated_subnet_id    = azurerm_subnet.private.id
  private_dns_zone_id    = azurerm_private_dns_zone.postgresql.id
  administrator_login    = "fabrica"
  administrator_password = var.db_password
  zone                   = "1"

  storage_mb = 102400
  sku_name   = "GP_Standard_D2s_v3"
}

# Azure Cache for Redis
resource "azurerm_redis_cache" "main" {
  name                = "fabrica-redis"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  capacity            = 1
  family              = "C"
  sku_name            = "Standard"
  enable_non_ssl_port = false
}

# Storage Account
resource "azurerm_storage_account" "uploads" {
  name                     = "fabricauploads"
  resource_group_name      = azurerm_resource_group.main.name
  location                 = azurerm_resource_group.main.location
  account_tier             = "Standard"
  account_replication_type = "GRS"

  blob_properties {
    versioning_enabled = true
  }
}
```

---

## 12. Provider-Specific Guides

### 12.1 AWS Deployment

1. **Prerequisites**
   - AWS Account with billing enabled
   - AWS CLI configured
   - Terraform installed
   - kubectl installed

2. **Quick Start**
   ```bash
   cd terraform/aws
   terraform init
   terraform plan
   terraform apply

   # Configure kubectl
   aws eks update-kubeconfig --name fabrica-cluster --region us-east-1

   # Deploy application
   kubectl apply -f k8s/
   ```

### 12.2 GCP Deployment

1. **Prerequisites**
   - GCP Project created
   - gcloud CLI configured
   - Terraform installed
   - kubectl installed

2. **Quick Start**
   ```bash
   cd terraform/gcp
   terraform init
   terraform plan
   terraform apply

   # Configure kubectl
   gcloud container clusters get-credentials fabrica-cluster --region us-central1

   # Deploy application
   kubectl apply -f k8s/
   ```

### 12.3 Azure Deployment

1. **Prerequisites**
   - Azure Subscription
   - Azure CLI configured
   - Terraform installed
   - kubectl installed

2. **Quick Start**
   ```bash
   cd terraform/azure
   terraform init
   terraform plan
   terraform apply

   # Configure kubectl
   az aks get-credentials --resource-group fabrica-production --name fabrica-aks

   # Deploy application
   kubectl apply -f k8s/
   ```

---

## Final Checklist

### Go-Live Preparation

- [ ] All infrastructure provisioned and tested
- [ ] Database migrated and validated
- [ ] Application deployed and health checks passing
- [ ] SSL certificates installed and tested
- [ ] DNS configured and propagated
- [ ] Monitoring and alerting configured
- [ ] Backups verified
- [ ] Runbooks created
- [ ] Team trained on new procedures
- [ ] Rollback plan documented and tested

### Post-Go-Live

- [ ] Monitor error rates for 24-48 hours
- [ ] Verify backup jobs running
- [ ] Check auto-scaling behavior
- [ ] Review costs vs estimates
- [ ] Document lessons learned
- [ ] Update architecture diagrams

---

*Document Version: 1.0*
*Last Updated: 2024-12-29*
*Author: Fabrica de Agentes Team*
