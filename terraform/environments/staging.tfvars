# =============================================================================
# Staging Environment Configuration
# Fabrica de Agentes - Terraform Variables
# =============================================================================

# =============================================================================
# General Settings
# =============================================================================

environment    = "staging"
cloud_provider = "aws"
owner          = "QA Team"
cost_center    = "engineering-staging"

# =============================================================================
# AWS Settings
# =============================================================================

aws_region         = "us-east-1"
vpc_cidr           = "10.1.0.0/16"
availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]

# =============================================================================
# Azure Settings (if using Azure)
# =============================================================================

azure_location     = "eastus"
azure_node_vm_size = "Standard_D2s_v3"
azure_db_sku_name  = "GP_Standard_D2s_v3"
azure_redis_capacity = 1
azure_redis_family   = "C"
azure_redis_sku_name = "Standard"

# =============================================================================
# GCP Settings (if using GCP)
# =============================================================================

gcp_project_id = ""
gcp_region     = "us-central1"

# =============================================================================
# Container/Application Settings
# =============================================================================

container_image    = "fabrica-agentes:staging"
container_port     = 9001
kubernetes_version = "1.28"

# =============================================================================
# Database Settings
# =============================================================================

db_name   = "fabrica_agentes_staging"
db_username = "fabrica_staging"
db_port     = 5432

# =============================================================================
# Instance Sizes (Staging - Medium)
# =============================================================================

instance_size  = "medium"
database_size  = "medium"

# =============================================================================
# Networking
# =============================================================================

enable_nat_gateway = true
single_nat_gateway = true
enable_vpn_gateway = false

# =============================================================================
# Security
# =============================================================================

allowed_cidr_blocks = ["0.0.0.0/0"]
enable_waf          = true
ssl_certificate_arn = ""

# =============================================================================
# Monitoring
# =============================================================================

enable_enhanced_monitoring = true
log_retention_days         = 14
enable_container_insights  = true

# =============================================================================
# Backup & Protection
# =============================================================================

backup_retention_period    = 7
enable_deletion_protection = false

# =============================================================================
# Auto Scaling
# =============================================================================

min_capacity              = 2
max_capacity              = 6
target_cpu_utilization    = 70
target_memory_utilization = 80
