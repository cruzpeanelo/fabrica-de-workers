# =============================================================================
# Fabrica de Agentes - Infrastructure as Code
# Main Terraform Configuration
# =============================================================================
# Issue #169: Remote State com S3 + DynamoDB + KMS Encryption
#
# IMPORTANTE - REMOTE STATE:
# ==========================
# Para usar remote state (recomendado para ambientes compartilhados):
#
#   1. Execute primeiro o bootstrap:
#      cd terraform/bootstrap && terraform init && terraform apply
#
#   2. Inicialize com backend config:
#      terraform init -backend-config=environments/dev.tfbackend
#      terraform init -backend-config=environments/staging.tfbackend
#      terraform init -backend-config=environments/prod.tfbackend
#
# Para desenvolvimento local (não recomendado para prod):
#   terraform init  # Usa backend local por padrão
#
# =============================================================================

terraform {
  required_version = ">= 1.0.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }
  }

  # Issue #169: S3 Backend configuration
  # Valores são injetados via -backend-config=environments/*.tfbackend
  # Isso permite separação de state por ambiente (dev/staging/prod)
  backend "s3" {}
}

# =============================================================================
# Provider Configuration
# =============================================================================

# AWS Provider
provider "aws" {
  region = var.aws_region

  default_tags {
    tags = local.common_tags
  }
}

# Azure Provider
provider "azurerm" {
  features {
    resource_group {
      prevent_deletion_if_contains_resources = false
    }
    key_vault {
      purge_soft_delete_on_destroy = true
    }
  }

  skip_provider_registration = true
}

# GCP Provider
provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# =============================================================================
# Local Variables
# =============================================================================

locals {
  common_tags = {
    Project     = "Fabrica-de-Agentes"
    Environment = var.environment
    ManagedBy   = "Terraform"
    Owner       = var.owner
    CostCenter  = var.cost_center
  }

  name_prefix = "fabrica-agentes-${var.environment}"

  # Database configuration based on environment
  db_config = {
    dev = {
      instance_class    = "db.t3.micro"
      allocated_storage = 20
      multi_az          = false
    }
    staging = {
      instance_class    = "db.t3.small"
      allocated_storage = 50
      multi_az          = false
    }
    prod = {
      instance_class    = "db.r6g.large"
      allocated_storage = 100
      multi_az          = true
    }
  }

  # Redis configuration based on environment
  redis_config = {
    dev = {
      node_type       = "cache.t3.micro"
      num_cache_nodes = 1
    }
    staging = {
      node_type       = "cache.t3.small"
      num_cache_nodes = 1
    }
    prod = {
      node_type       = "cache.r6g.large"
      num_cache_nodes = 2
    }
  }

  # Container configuration based on environment
  container_config = {
    dev = {
      cpu    = 256
      memory = 512
      count  = 1
    }
    staging = {
      cpu    = 512
      memory = 1024
      count  = 2
    }
    prod = {
      cpu    = 1024
      memory = 2048
      count  = 3
    }
  }
}

# =============================================================================
# Random Resources
# =============================================================================

resource "random_password" "db_password" {
  length           = 32
  special          = true
  override_special = "!#$%&*()-_=+[]{}<>:?"
}

resource "random_id" "suffix" {
  byte_length = 4
}

# =============================================================================
# Module Invocations
# =============================================================================

# AWS Infrastructure Module
module "aws" {
  source = "./modules/aws"
  count  = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? 1 : 0

  environment    = var.environment
  name_prefix    = local.name_prefix
  aws_region     = var.aws_region

  # VPC Configuration
  vpc_cidr           = var.vpc_cidr
  availability_zones = var.availability_zones

  # ECS Configuration
  container_image   = var.container_image
  container_port    = var.container_port
  container_cpu     = local.container_config[var.environment].cpu
  container_memory  = local.container_config[var.environment].memory
  desired_count     = local.container_config[var.environment].count

  # RDS Configuration
  db_instance_class    = local.db_config[var.environment].instance_class
  db_allocated_storage = local.db_config[var.environment].allocated_storage
  db_multi_az          = local.db_config[var.environment].multi_az
  db_name              = var.db_name
  db_username          = var.db_username
  db_password          = random_password.db_password.result

  # ElastiCache Configuration
  redis_node_type       = local.redis_config[var.environment].node_type
  redis_num_cache_nodes = local.redis_config[var.environment].num_cache_nodes

  # S3 Configuration
  s3_bucket_name = "${local.name_prefix}-uploads-${random_id.suffix.hex}"

  # Tags
  tags = local.common_tags
}

# Azure Infrastructure Module
module "azure" {
  source = "./modules/azure"
  count  = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? 1 : 0

  environment     = var.environment
  name_prefix     = local.name_prefix
  azure_location  = var.azure_location

  # AKS Configuration
  kubernetes_version = var.kubernetes_version
  node_count         = local.container_config[var.environment].count
  node_vm_size       = var.azure_node_vm_size

  # PostgreSQL Configuration
  db_sku_name     = var.azure_db_sku_name
  db_storage_mb   = local.db_config[var.environment].allocated_storage * 1024
  db_name         = var.db_name
  db_admin_login  = var.db_username
  db_admin_password = random_password.db_password.result

  # Redis Configuration
  redis_capacity = var.azure_redis_capacity
  redis_family   = var.azure_redis_family
  redis_sku_name = var.azure_redis_sku_name

  # Tags
  tags = local.common_tags
}

# GCP Infrastructure Module (Placeholder)
module "gcp" {
  source = "./modules/gcp"
  count  = var.cloud_provider == "gcp" || var.cloud_provider == "multi" ? 1 : 0

  environment    = var.environment
  name_prefix    = local.name_prefix
  gcp_project_id = var.gcp_project_id
  gcp_region     = var.gcp_region

  # Tags
  labels = local.common_tags
}
