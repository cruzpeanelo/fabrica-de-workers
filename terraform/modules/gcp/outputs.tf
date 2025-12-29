# =============================================================================
# GCP Module - Outputs
# =============================================================================

# =============================================================================
# Network Outputs
# =============================================================================

output "vpc_id" {
  description = "VPC network ID"
  value       = google_compute_network.main.id
}

output "vpc_name" {
  description = "VPC network name"
  value       = google_compute_network.main.name
}

output "subnet_id" {
  description = "Subnet ID"
  value       = google_compute_subnetwork.main.id
}

# =============================================================================
# GKE Outputs
# =============================================================================

output "gke_cluster_id" {
  description = "GKE cluster ID"
  value       = google_container_cluster.main.id
}

output "gke_cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.main.name
}

output "gke_cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = google_container_cluster.main.endpoint
  sensitive   = true
}

output "gke_cluster_ca_certificate" {
  description = "GKE cluster CA certificate"
  value       = google_container_cluster.main.master_auth[0].cluster_ca_certificate
  sensitive   = true
}

# =============================================================================
# Cloud SQL Outputs
# =============================================================================

output "sql_instance_name" {
  description = "Cloud SQL instance name"
  value       = google_sql_database_instance.main.name
}

output "sql_instance_connection_name" {
  description = "Cloud SQL instance connection name"
  value       = google_sql_database_instance.main.connection_name
}

output "sql_database_name" {
  description = "Cloud SQL database name"
  value       = google_sql_database.main.name
}

output "sql_private_ip" {
  description = "Cloud SQL private IP"
  value       = google_sql_database_instance.main.private_ip_address
}

# =============================================================================
# Redis Outputs
# =============================================================================

output "redis_host" {
  description = "Memorystore Redis host"
  value       = google_redis_instance.main.host
}

output "redis_port" {
  description = "Memorystore Redis port"
  value       = google_redis_instance.main.port
}

output "redis_current_location_id" {
  description = "Redis current location"
  value       = google_redis_instance.main.current_location_id
}

# =============================================================================
# Storage Outputs
# =============================================================================

output "storage_bucket_name" {
  description = "Cloud Storage bucket name"
  value       = google_storage_bucket.uploads.name
}

output "storage_bucket_url" {
  description = "Cloud Storage bucket URL"
  value       = google_storage_bucket.uploads.url
}

# =============================================================================
# Load Balancer Outputs
# =============================================================================

output "load_balancer_ip" {
  description = "Load balancer IP address"
  value       = google_compute_global_address.main.address
}

# =============================================================================
# Service Account Outputs
# =============================================================================

output "gke_service_account_email" {
  description = "GKE service account email"
  value       = google_service_account.gke.email
}
