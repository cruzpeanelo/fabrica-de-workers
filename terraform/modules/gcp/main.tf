# =============================================================================
# GCP Module - Main Configuration
# Fabrica de Agentes Infrastructure
# =============================================================================

# NOTE: This is a placeholder module for GCP infrastructure.
# Full implementation can be added when GCP support is needed.

# =============================================================================
# VPC Network
# =============================================================================

resource "google_compute_network" "main" {
  name                    = "${var.name_prefix}-vpc"
  auto_create_subnetworks = false
  project                 = var.gcp_project_id
}

resource "google_compute_subnetwork" "main" {
  name          = "${var.name_prefix}-subnet"
  ip_cidr_range = "10.0.0.0/24"
  region        = var.gcp_region
  network       = google_compute_network.main.id
  project       = var.gcp_project_id

  secondary_ip_range {
    range_name    = "pods"
    ip_cidr_range = "10.1.0.0/16"
  }

  secondary_ip_range {
    range_name    = "services"
    ip_cidr_range = "10.2.0.0/16"
  }
}

# =============================================================================
# Firewall Rules
# =============================================================================

resource "google_compute_firewall" "allow_http" {
  name    = "${var.name_prefix}-allow-http"
  network = google_compute_network.main.name
  project = var.gcp_project_id

  allow {
    protocol = "tcp"
    ports    = ["80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["web"]
}

resource "google_compute_firewall" "allow_internal" {
  name    = "${var.name_prefix}-allow-internal"
  network = google_compute_network.main.name
  project = var.gcp_project_id

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "icmp"
  }

  source_ranges = ["10.0.0.0/8"]
}

# =============================================================================
# GKE Cluster (Google Kubernetes Engine)
# =============================================================================

resource "google_container_cluster" "main" {
  name     = "${var.name_prefix}-gke"
  location = var.gcp_region
  project  = var.gcp_project_id

  # We can't create a cluster with no node pool defined, but we want to only use
  # separately managed node pools. So we create the smallest possible default
  # node pool and immediately delete it.
  remove_default_node_pool = true
  initial_node_count       = 1

  network    = google_compute_network.main.name
  subnetwork = google_compute_subnetwork.main.name

  ip_allocation_policy {
    cluster_secondary_range_name  = "pods"
    services_secondary_range_name = "services"
  }

  workload_identity_config {
    workload_pool = "${var.gcp_project_id}.svc.id.goog"
  }

  resource_labels = var.labels
}

resource "google_container_node_pool" "main" {
  name       = "${var.name_prefix}-node-pool"
  location   = var.gcp_region
  cluster    = google_container_cluster.main.name
  project    = var.gcp_project_id
  node_count = var.environment == "prod" ? 3 : 1

  autoscaling {
    min_node_count = 1
    max_node_count = 10
  }

  node_config {
    preemptible  = var.environment != "prod"
    machine_type = var.environment == "prod" ? "e2-standard-4" : "e2-medium"

    service_account = google_service_account.gke.email
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    labels = var.labels

    tags = ["gke-node", var.name_prefix]
  }
}

# =============================================================================
# Cloud SQL PostgreSQL
# =============================================================================

resource "google_sql_database_instance" "main" {
  name             = "${var.name_prefix}-postgres"
  database_version = "POSTGRES_15"
  region           = var.gcp_region
  project          = var.gcp_project_id

  settings {
    tier = var.environment == "prod" ? "db-custom-2-7680" : "db-f1-micro"

    ip_configuration {
      ipv4_enabled    = false
      private_network = google_compute_network.main.id
    }

    backup_configuration {
      enabled                        = true
      start_time                     = "03:00"
      point_in_time_recovery_enabled = var.environment == "prod"
    }

    maintenance_window {
      day  = 1
      hour = 4
    }

    user_labels = var.labels
  }

  deletion_protection = var.environment == "prod"
}

resource "google_sql_database" "main" {
  name     = "fabrica_agentes"
  instance = google_sql_database_instance.main.name
  project  = var.gcp_project_id
}

resource "google_sql_user" "main" {
  name     = "fabrica_admin"
  instance = google_sql_database_instance.main.name
  password = var.db_password
  project  = var.gcp_project_id
}

# =============================================================================
# Memorystore Redis
# =============================================================================

resource "google_redis_instance" "main" {
  name           = "${var.name_prefix}-redis"
  tier           = var.environment == "prod" ? "STANDARD_HA" : "BASIC"
  memory_size_gb = var.environment == "prod" ? 5 : 1
  region         = var.gcp_region
  project        = var.gcp_project_id

  authorized_network = google_compute_network.main.id

  redis_version = "REDIS_7_0"

  labels = var.labels
}

# =============================================================================
# Cloud Storage Bucket
# =============================================================================

resource "google_storage_bucket" "uploads" {
  name          = "${var.name_prefix}-uploads"
  location      = var.gcp_region
  project       = var.gcp_project_id
  force_destroy = var.environment != "prod"

  uniform_bucket_level_access = true

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      age = 30
    }
    action {
      type = "Delete"
    }
  }

  labels = var.labels
}

# =============================================================================
# Cloud Load Balancer
# =============================================================================

resource "google_compute_global_address" "main" {
  name    = "${var.name_prefix}-lb-ip"
  project = var.gcp_project_id
}

# =============================================================================
# Service Account
# =============================================================================

resource "google_service_account" "gke" {
  account_id   = "${var.name_prefix}-gke-sa"
  display_name = "GKE Service Account"
  project      = var.gcp_project_id
}

resource "google_project_iam_member" "gke_log_writer" {
  project = var.gcp_project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.gke.email}"
}

resource "google_project_iam_member" "gke_metric_writer" {
  project = var.gcp_project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.gke.email}"
}

resource "google_project_iam_member" "gke_storage_viewer" {
  project = var.gcp_project_id
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.gke.email}"
}

# =============================================================================
# Secret Manager
# =============================================================================

resource "google_secret_manager_secret" "db_password" {
  secret_id = "${var.name_prefix}-db-password"
  project   = var.gcp_project_id

  replication {
    auto {}
  }

  labels = var.labels
}

resource "google_secret_manager_secret_version" "db_password" {
  secret      = google_secret_manager_secret.db_password.id
  secret_data = var.db_password
}
