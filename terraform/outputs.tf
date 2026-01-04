# =============================================================================
# Plataforma E - Terraform Outputs
# =============================================================================

# =============================================================================
# General Outputs
# =============================================================================

output "environment" {
  description = "Current environment"
  value       = var.environment
}

output "cloud_provider" {
  description = "Cloud provider being used"
  value       = var.cloud_provider
}

# =============================================================================
# AWS Outputs
# =============================================================================

output "aws_dashboard_url" {
  description = "URL of the dashboard (AWS ALB)"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].alb_dns_name : null
}

output "aws_dashboard_endpoint" {
  description = "Full dashboard endpoint with protocol"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "http://${module.aws[0].alb_dns_name}:${var.container_port}" : null
}

output "aws_database_endpoint" {
  description = "RDS PostgreSQL endpoint"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].rds_endpoint : null
  sensitive   = true
}

output "aws_database_connection_string" {
  description = "PostgreSQL connection string (AWS)"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "postgresql://${var.db_username}:${random_password.db_password.result}@${module.aws[0].rds_endpoint}/${var.db_name}" : null
  sensitive   = true
}

output "aws_redis_endpoint" {
  description = "ElastiCache Redis endpoint"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].redis_endpoint : null
  sensitive   = true
}

output "aws_redis_connection_string" {
  description = "Redis connection string (AWS)"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "redis://${module.aws[0].redis_endpoint}:6379" : null
  sensitive   = true
}

output "aws_s3_bucket_name" {
  description = "S3 bucket name for uploads"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].s3_bucket_name : null
}

output "aws_s3_bucket_arn" {
  description = "S3 bucket ARN"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].s3_bucket_arn : null
}

output "aws_cloudwatch_log_group" {
  description = "CloudWatch log group name"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].cloudwatch_log_group : null
}

output "aws_vpc_id" {
  description = "VPC ID"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].vpc_id : null
}

output "aws_ecs_cluster_name" {
  description = "ECS Cluster name"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].ecs_cluster_name : null
}

output "aws_ecs_service_name" {
  description = "ECS Service name"
  value       = var.cloud_provider == "aws" || var.cloud_provider == "multi" ? module.aws[0].ecs_service_name : null
}

# =============================================================================
# Azure Outputs
# =============================================================================

output "azure_dashboard_url" {
  description = "URL of the dashboard (Azure Application Gateway)"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].application_gateway_ip : null
}

output "azure_dashboard_endpoint" {
  description = "Full dashboard endpoint with protocol (Azure)"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "http://${module.azure[0].application_gateway_ip}:${var.container_port}" : null
}

output "azure_database_endpoint" {
  description = "Azure PostgreSQL endpoint"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].postgresql_fqdn : null
  sensitive   = true
}

output "azure_database_connection_string" {
  description = "PostgreSQL connection string (Azure)"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "postgresql://${var.db_username}@${module.azure[0].postgresql_server_name}:${random_password.db_password.result}@${module.azure[0].postgresql_fqdn}/${var.db_name}?sslmode=require" : null
  sensitive   = true
}

output "azure_redis_endpoint" {
  description = "Azure Cache for Redis endpoint"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].redis_hostname : null
  sensitive   = true
}

output "azure_redis_connection_string" {
  description = "Redis connection string (Azure)"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].redis_connection_string : null
  sensitive   = true
}

output "azure_aks_cluster_name" {
  description = "AKS Cluster name"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].aks_cluster_name : null
}

output "azure_aks_kube_config" {
  description = "AKS Kubernetes config"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].kube_config : null
  sensitive   = true
}

output "azure_resource_group_name" {
  description = "Azure Resource Group name"
  value       = var.cloud_provider == "azure" || var.cloud_provider == "multi" ? module.azure[0].resource_group_name : null
}

# =============================================================================
# GCP Outputs
# =============================================================================

output "gcp_dashboard_url" {
  description = "URL of the dashboard (GCP)"
  value       = var.cloud_provider == "gcp" || var.cloud_provider == "multi" ? module.gcp[0].load_balancer_ip : null
}

output "gcp_project_id" {
  description = "GCP Project ID"
  value       = var.cloud_provider == "gcp" || var.cloud_provider == "multi" ? var.gcp_project_id : null
}

# =============================================================================
# Combined Outputs (for easy reference)
# =============================================================================

output "connection_strings" {
  description = "All connection strings for the application"
  value = {
    database = var.cloud_provider == "aws" ? (
      var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "postgresql://${var.db_username}:****@${module.aws[0].rds_endpoint}/${var.db_name}" : null
    ) : (
      var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "postgresql://${var.db_username}@****:****@${module.azure[0].postgresql_fqdn}/${var.db_name}?sslmode=require" : null
    )
    redis = var.cloud_provider == "aws" ? (
      var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "redis://${module.aws[0].redis_endpoint}:6379" : null
    ) : (
      var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "rediss://${module.azure[0].redis_hostname}:6380" : null
    )
  }
  sensitive = true
}

output "endpoints" {
  description = "All service endpoints"
  value = {
    dashboard = var.cloud_provider == "aws" ? (
      var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "http://${module.aws[0].alb_dns_name}" : null
    ) : (
      var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "http://${module.azure[0].application_gateway_ip}" : null
    )
    api = var.cloud_provider == "aws" ? (
      var.cloud_provider == "aws" || var.cloud_provider == "multi" ? "http://${module.aws[0].alb_dns_name}/api" : null
    ) : (
      var.cloud_provider == "azure" || var.cloud_provider == "multi" ? "http://${module.azure[0].application_gateway_ip}/api" : null
    )
  }
}

output "infrastructure_summary" {
  description = "Summary of deployed infrastructure"
  value = {
    environment    = var.environment
    cloud_provider = var.cloud_provider
    region = var.cloud_provider == "aws" ? var.aws_region : (
      var.cloud_provider == "azure" ? var.azure_location : var.gcp_region
    )
    database_name = var.db_name
    container_port = var.container_port
  }
}
