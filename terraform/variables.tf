# =============================================================================
# Fabrica de Agentes - Terraform Variables
# =============================================================================

# =============================================================================
# General Variables
# =============================================================================

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be one of: dev, staging, prod."
  }
}

variable "cloud_provider" {
  description = "Cloud provider to use (aws, azure, gcp, multi)"
  type        = string
  default     = "aws"

  validation {
    condition     = contains(["aws", "azure", "gcp", "multi"], var.cloud_provider)
    error_message = "Cloud provider must be one of: aws, azure, gcp, multi."
  }
}

variable "owner" {
  description = "Owner of the infrastructure"
  type        = string
  default     = "Fabrica de Agentes Team"
}

variable "cost_center" {
  description = "Cost center for billing"
  type        = string
  default     = "engineering"
}

# =============================================================================
# AWS Variables
# =============================================================================

variable "aws_region" {
  description = "AWS region for resources"
  type        = string
  default     = "us-east-1"
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

# =============================================================================
# Azure Variables
# =============================================================================

variable "azure_location" {
  description = "Azure region for resources"
  type        = string
  default     = "eastus"
}

variable "azure_node_vm_size" {
  description = "VM size for AKS nodes"
  type        = string
  default     = "Standard_D2s_v3"
}

variable "azure_db_sku_name" {
  description = "SKU name for Azure PostgreSQL"
  type        = string
  default     = "GP_Gen5_2"
}

variable "azure_redis_capacity" {
  description = "Redis cache capacity"
  type        = number
  default     = 1
}

variable "azure_redis_family" {
  description = "Redis cache family (C for Basic/Standard, P for Premium)"
  type        = string
  default     = "C"
}

variable "azure_redis_sku_name" {
  description = "Redis cache SKU (Basic, Standard, Premium)"
  type        = string
  default     = "Standard"
}

# =============================================================================
# GCP Variables
# =============================================================================

variable "gcp_project_id" {
  description = "GCP Project ID"
  type        = string
  default     = ""
}

variable "gcp_region" {
  description = "GCP region for resources"
  type        = string
  default     = "us-central1"
}

# =============================================================================
# Container/Application Variables
# =============================================================================

variable "container_image" {
  description = "Docker image for the application"
  type        = string
  default     = "fabrica-agentes:latest"
}

variable "container_port" {
  description = "Port the container exposes"
  type        = number
  default     = 9001
}

variable "kubernetes_version" {
  description = "Kubernetes version for managed clusters"
  type        = string
  default     = "1.28"
}

# =============================================================================
# Database Variables
# =============================================================================

variable "db_name" {
  description = "Name of the database"
  type        = string
  default     = "fabrica_agentes"
}

variable "db_username" {
  description = "Database master username"
  type        = string
  default     = "fabrica_admin"
}

variable "db_port" {
  description = "Database port"
  type        = number
  default     = 5432
}

# =============================================================================
# Instance Size Variables (Environment-specific overrides)
# =============================================================================

variable "instance_size" {
  description = "General instance size (small, medium, large)"
  type        = string
  default     = "small"

  validation {
    condition     = contains(["small", "medium", "large"], var.instance_size)
    error_message = "Instance size must be one of: small, medium, large."
  }
}

variable "database_size" {
  description = "Database instance size (small, medium, large)"
  type        = string
  default     = "small"

  validation {
    condition     = contains(["small", "medium", "large"], var.database_size)
    error_message = "Database size must be one of: small, medium, large."
  }
}

# =============================================================================
# Networking Variables
# =============================================================================

variable "enable_nat_gateway" {
  description = "Enable NAT Gateway for private subnets"
  type        = bool
  default     = true
}

variable "single_nat_gateway" {
  description = "Use a single NAT Gateway (cost saving for non-prod)"
  type        = bool
  default     = true
}

variable "enable_vpn_gateway" {
  description = "Enable VPN Gateway"
  type        = bool
  default     = false
}

# =============================================================================
# Security Variables
# =============================================================================

variable "allowed_cidr_blocks" {
  description = "CIDR blocks allowed to access the application"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

variable "enable_waf" {
  description = "Enable Web Application Firewall"
  type        = bool
  default     = false
}

variable "ssl_certificate_arn" {
  description = "ARN of SSL certificate for HTTPS"
  type        = string
  default     = ""
}

# =============================================================================
# Monitoring Variables
# =============================================================================

variable "enable_enhanced_monitoring" {
  description = "Enable enhanced monitoring for RDS"
  type        = bool
  default     = false
}

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

variable "enable_container_insights" {
  description = "Enable Container Insights for ECS"
  type        = bool
  default     = true
}

# =============================================================================
# Backup Variables
# =============================================================================

variable "backup_retention_period" {
  description = "Number of days to retain backups"
  type        = number
  default     = 7
}

variable "enable_deletion_protection" {
  description = "Enable deletion protection for databases"
  type        = bool
  default     = false
}

# =============================================================================
# Auto Scaling Variables
# =============================================================================

variable "min_capacity" {
  description = "Minimum number of tasks/instances"
  type        = number
  default     = 1
}

variable "max_capacity" {
  description = "Maximum number of tasks/instances"
  type        = number
  default     = 10
}

variable "target_cpu_utilization" {
  description = "Target CPU utilization for auto scaling"
  type        = number
  default     = 70
}

variable "target_memory_utilization" {
  description = "Target memory utilization for auto scaling"
  type        = number
  default     = 80
}
