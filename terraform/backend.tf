# =============================================================================
# Plataforma E - Terraform Remote State Backend Configuration
# =============================================================================
# Issue #95: Terraform Remote State com Locking
#
# Este arquivo configura:
#   - S3 Backend para armazenamento de state
#   - DynamoDB para state locking
#   - Encryption at rest com KMS
#   - Separacao por environment (dev/staging/prod)
#
# Uso:
#   # Inicializar para um environment especifico
#   terraform init -backend-config=environments/dev.tfbackend
#   terraform init -backend-config=environments/prod.tfbackend
#
# Pre-requisitos:
#   1. Rodar terraform apply em terraform/bootstrap/ primeiro
#   2. Bucket S3 e tabela DynamoDB devem existir
# =============================================================================

# =============================================================================
# Backend Configuration
# =============================================================================
# NOTA: Para ativar, remova o backend local do main.tf e descomente abaixo
# Cada environment usa seu proprio state file

terraform {
  backend "s3" {
    # Bucket de state - criado pelo bootstrap
    bucket = "fabrica-agentes-terraform-state"

    # Key do state - sera override por environment
    # dev:     terraform/dev/terraform.tfstate
    # staging: terraform/staging/terraform.tfstate
    # prod:    terraform/prod/terraform.tfstate
    key = "terraform/terraform.tfstate"

    # Regiao do bucket
    region = "us-east-1"

    # Encryption at rest
    encrypt = true

    # KMS key para encryption (opcional - SSE-S3 por default)
    # kms_key_id = "alias/fabrica-terraform-state"

    # DynamoDB table para state locking
    dynamodb_table = "fabrica-agentes-terraform-locks"

    # Prevenir acidentes
    skip_metadata_api_check = false

    # Workspace prefix para multiplos ambientes
    workspace_key_prefix = "workspaces"
  }
}

# =============================================================================
# Bootstrap Resources (Rodar separadamente antes de usar o backend)
# =============================================================================
# Estes recursos devem ser criados ANTES de migrar para remote state
# Coloque em terraform/bootstrap/main.tf e execute separadamente

# resource "aws_s3_bucket" "terraform_state" {
#   bucket = "fabrica-agentes-terraform-state"
#
#   lifecycle {
#     prevent_destroy = true
#   }
#
#   tags = {
#     Name        = "Terraform State"
#     Environment = "shared"
#     ManagedBy   = "terraform-bootstrap"
#   }
# }
#
# resource "aws_s3_bucket_versioning" "terraform_state" {
#   bucket = aws_s3_bucket.terraform_state.id
#   versioning_configuration {
#     status = "Enabled"
#   }
# }
#
# resource "aws_s3_bucket_server_side_encryption_configuration" "terraform_state" {
#   bucket = aws_s3_bucket.terraform_state.id
#
#   rule {
#     apply_server_side_encryption_by_default {
#       sse_algorithm     = "aws:kms"
#       kms_master_key_id = aws_kms_key.terraform_state.arn
#     }
#     bucket_key_enabled = true
#   }
# }
#
# resource "aws_s3_bucket_public_access_block" "terraform_state" {
#   bucket = aws_s3_bucket.terraform_state.id
#
#   block_public_acls       = true
#   block_public_policy     = true
#   ignore_public_acls      = true
#   restrict_public_buckets = true
# }
#
# resource "aws_dynamodb_table" "terraform_locks" {
#   name         = "fabrica-agentes-terraform-locks"
#   billing_mode = "PAY_PER_REQUEST"
#   hash_key     = "LockID"
#
#   attribute {
#     name = "LockID"
#     type = "S"
#   }
#
#   point_in_time_recovery {
#     enabled = true
#   }
#
#   tags = {
#     Name        = "Terraform State Locks"
#     Environment = "shared"
#     ManagedBy   = "terraform-bootstrap"
#   }
# }
#
# resource "aws_kms_key" "terraform_state" {
#   description             = "KMS key for Terraform state encryption"
#   deletion_window_in_days = 30
#   enable_key_rotation     = true
#
#   tags = {
#     Name        = "terraform-state-key"
#     Environment = "shared"
#     ManagedBy   = "terraform-bootstrap"
#   }
# }
#
# resource "aws_kms_alias" "terraform_state" {
#   name          = "alias/fabrica-terraform-state"
#   target_key_id = aws_kms_key.terraform_state.key_id
# }
