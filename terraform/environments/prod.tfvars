# =============================================================================
# Production Environment Configuration
# Fabrica de Agentes - Terraform Variables
# =============================================================================

# =============================================================================
# General Settings
# =============================================================================

environment    = "prod"
cloud_provider = "aws"
owner          = "Platform Team"
cost_center    = "engineering-prod"

# =============================================================================
# AWS Settings
# =============================================================================

aws_region         = "us-east-1"
vpc_cidr           = "10.2.0.0/16"
availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]

# =============================================================================
# Azure Settings (if using Azure)
# =============================================================================

azure_location     = "eastus"
azure_node_vm_size = "Standard_D4s_v3"
azure_db_sku_name  = "GP_Standard_D4s_v3"
azure_redis_capacity = 2
azure_redis_family   = "P"
azure_redis_sku_name = "Premium"

# =============================================================================
# GCP Settings (if using GCP)
# =============================================================================

gcp_project_id = ""
gcp_region     = "us-central1"

# =============================================================================
# Container/Application Settings
# =============================================================================

container_image    = "fabrica-agentes:latest"
container_port     = 9001
kubernetes_version = "1.28"

# =============================================================================
# Database Settings
# =============================================================================

db_name   = "fabrica_agentes"
db_username = "fabrica_admin"
db_port     = 5432

# =============================================================================
# Instance Sizes (Production - Large)
# =============================================================================

instance_size  = "large"
database_size  = "large"

# =============================================================================
# Networking
# =============================================================================

enable_nat_gateway = true
single_nat_gateway = false  # Multiple NAT gateways for HA
enable_vpn_gateway = true

# =============================================================================
# Security
# =============================================================================

allowed_cidr_blocks = ["0.0.0.0/0"]  # Should be restricted in real production
enable_waf          = true
ssl_certificate_arn = ""  # Add your SSL certificate ARN

# =============================================================================
# Monitoring
# =============================================================================

enable_enhanced_monitoring = true
log_retention_days         = 90
enable_container_insights  = true

# =============================================================================
# Backup & Protection
# =============================================================================

backup_retention_period    = 30
enable_deletion_protection = true

# =============================================================================
# Auto Scaling
# =============================================================================

min_capacity              = 3
max_capacity              = 20
target_cpu_utilization    = 60
target_memory_utilization = 70
