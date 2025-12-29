# =============================================================================
# Development Environment Configuration
# Fabrica de Agentes - Terraform Variables
# =============================================================================

# =============================================================================
# General Settings
# =============================================================================

environment    = "dev"
cloud_provider = "aws"
owner          = "Development Team"
cost_center    = "engineering-dev"

# =============================================================================
# AWS Settings
# =============================================================================

aws_region         = "us-east-1"
vpc_cidr           = "10.0.0.0/16"
availability_zones = ["us-east-1a", "us-east-1b"]

# =============================================================================
# Azure Settings (if using Azure)
# =============================================================================

azure_location     = "eastus"
azure_node_vm_size = "Standard_B2s"
azure_db_sku_name  = "GP_Standard_D2s_v3"
azure_redis_capacity = 0
azure_redis_family   = "C"
azure_redis_sku_name = "Basic"

# =============================================================================
# GCP Settings (if using GCP)
# =============================================================================

gcp_project_id = ""
gcp_region     = "us-central1"

# =============================================================================
# Container/Application Settings
# =============================================================================

container_image    = "fabrica-agentes:dev"
container_port     = 9001
kubernetes_version = "1.28"

# =============================================================================
# Database Settings
# =============================================================================

db_name   = "fabrica_agentes_dev"
db_username = "fabrica_dev"
db_port     = 5432

# =============================================================================
# Instance Sizes (Development - Minimal)
# =============================================================================

instance_size  = "small"
database_size  = "small"

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
enable_waf          = false
ssl_certificate_arn = ""

# =============================================================================
# Monitoring
# =============================================================================

enable_enhanced_monitoring = false
log_retention_days         = 7
enable_container_insights  = false

# =============================================================================
# Backup & Protection
# =============================================================================

backup_retention_period    = 1
enable_deletion_protection = false

# =============================================================================
# Auto Scaling
# =============================================================================

min_capacity              = 1
max_capacity              = 3
target_cpu_utilization    = 80
target_memory_utilization = 80
