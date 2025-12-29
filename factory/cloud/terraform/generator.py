# -*- coding: utf-8 -*-
"""
Terraform Generator
===================
Gerador de configuracoes Terraform para deploy multi-cloud.
"""

from typing import Any, Dict, List, Optional
import json
import os
import logging
from pathlib import Path

from ..base_provider import ProviderType, StackType

logger = logging.getLogger(__name__)

# Diretorio de templates
TEMPLATES_DIR = Path(__file__).parent / "templates"


class TerraformGenerator:
    """
    Gerador de configuracoes Terraform.

    Gera arquivos .tf baseados em templates para
    diferentes providers e tipos de stack.
    """

    def __init__(self, output_dir: str = "./terraform"):
        """
        Inicializa o gerador.

        Args:
            output_dir: Diretorio de saida dos arquivos Terraform
        """
        self.output_dir = Path(output_dir)

    def generate(
        self,
        project_name: str,
        provider: ProviderType,
        stack_type: StackType,
        config: Dict[str, Any]
    ) -> str:
        """
        Gera arquivos Terraform para um projeto.

        Args:
            project_name: Nome do projeto
            provider: Provider cloud
            stack_type: Tipo da stack
            config: Configuracoes especificas

        Returns:
            Caminho do diretorio com os arquivos gerados
        """
        # Criar diretorio do projeto
        project_dir = self.output_dir / project_name
        project_dir.mkdir(parents=True, exist_ok=True)

        # Gerar arquivos
        self._generate_main(project_dir, provider, project_name, config)
        self._generate_variables(project_dir, provider, config)
        self._generate_outputs(project_dir, provider)
        self._generate_provider(project_dir, provider, config)

        if stack_type == StackType.SIMPLE:
            self._generate_simple_stack(project_dir, provider, project_name, config)
        elif stack_type == StackType.SERVERLESS:
            self._generate_serverless_stack(project_dir, provider, project_name, config)
        elif stack_type == StackType.MICROSERVICES:
            self._generate_microservices_stack(project_dir, provider, project_name, config)

        logger.info(f"Terraform gerado em {project_dir}")
        return str(project_dir)

    def _generate_main(
        self,
        project_dir: Path,
        provider: ProviderType,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera arquivo main.tf"""
        content = f'''# =============================================================================
# Fabrica de Agentes - Terraform Configuration
# Project: {project_name}
# Provider: {provider.value.upper()}
# Generated automatically - DO NOT EDIT
# =============================================================================

terraform {{
  required_version = ">= 1.0"

  required_providers {{
'''

        if provider == ProviderType.AWS:
            content += '''    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
'''
        elif provider == ProviderType.AZURE:
            content += '''    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
'''
        elif provider == ProviderType.GCP:
            content += '''    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
'''

        content += '''  }

  # Backend configuration (uncomment and configure for remote state)
  # backend "s3" {
  #   bucket = "terraform-state-bucket"
  #   key    = "''' + project_name + '''/terraform.tfstate"
  #   region = "us-east-1"
  # }
}

# Local values
locals {
  project_name = var.project_name
  environment  = var.environment

  common_tags = {
    Project     = local.project_name
    Environment = local.environment
    ManagedBy   = "Terraform"
    Generator   = "FabricaDeAgentes"
  }
}
'''

        (project_dir / "main.tf").write_text(content, encoding="utf-8")

    def _generate_variables(
        self,
        project_dir: Path,
        provider: ProviderType,
        config: Dict[str, Any]
    ):
        """Gera arquivo variables.tf"""
        content = '''# =============================================================================
# Variables
# =============================================================================

variable "project_name" {
  description = "Nome do projeto"
  type        = string
}

variable "environment" {
  description = "Ambiente (dev, staging, prod)"
  type        = string
  default     = "dev"
}

'''

        if provider == ProviderType.AWS:
            content += '''variable "aws_region" {
  description = "Regiao AWS"
  type        = string
  default     = "us-east-1"
}

variable "instance_type" {
  description = "Tipo da instancia EC2"
  type        = string
  default     = "t3.micro"
}

variable "db_instance_class" {
  description = "Classe da instancia RDS"
  type        = string
  default     = "db.t3.micro"
}
'''

        elif provider == ProviderType.AZURE:
            content += '''variable "azure_location" {
  description = "Localizacao Azure"
  type        = string
  default     = "eastus"
}

variable "resource_group_name" {
  description = "Nome do Resource Group"
  type        = string
}

variable "vm_size" {
  description = "Tamanho da VM"
  type        = string
  default     = "Standard_B1s"
}
'''

        elif provider == ProviderType.GCP:
            content += '''variable "gcp_project" {
  description = "ID do projeto GCP"
  type        = string
}

variable "gcp_region" {
  description = "Regiao GCP"
  type        = string
  default     = "us-central1"
}

variable "gcp_zone" {
  description = "Zona GCP"
  type        = string
  default     = "us-central1-a"
}

variable "machine_type" {
  description = "Tipo da maquina"
  type        = string
  default     = "e2-micro"
}
'''

        (project_dir / "variables.tf").write_text(content, encoding="utf-8")

    def _generate_outputs(
        self,
        project_dir: Path,
        provider: ProviderType
    ):
        """Gera arquivo outputs.tf"""
        content = '''# =============================================================================
# Outputs
# =============================================================================

'''

        if provider == ProviderType.AWS:
            content += '''output "instance_public_ip" {
  description = "IP publico da instancia"
  value       = try(aws_instance.main[0].public_ip, null)
}

output "instance_public_dns" {
  description = "DNS publico da instancia"
  value       = try(aws_instance.main[0].public_dns, null)
}

output "s3_bucket_name" {
  description = "Nome do bucket S3"
  value       = try(aws_s3_bucket.assets[0].id, null)
}

output "rds_endpoint" {
  description = "Endpoint do RDS"
  value       = try(aws_db_instance.main[0].endpoint, null)
  sensitive   = true
}

output "application_url" {
  description = "URL da aplicacao"
  value       = try("http://${aws_instance.main[0].public_dns}", null)
}
'''

        elif provider == ProviderType.AZURE:
            content += '''output "vm_public_ip" {
  description = "IP publico da VM"
  value       = try(azurerm_public_ip.main[0].ip_address, null)
}

output "storage_account_name" {
  description = "Nome da Storage Account"
  value       = try(azurerm_storage_account.main[0].name, null)
}

output "database_fqdn" {
  description = "FQDN do banco de dados"
  value       = try(azurerm_postgresql_flexible_server.main[0].fqdn, null)
  sensitive   = true
}
'''

        elif provider == ProviderType.GCP:
            content += '''output "instance_external_ip" {
  description = "IP externo da instancia"
  value       = try(google_compute_instance.main[0].network_interface[0].access_config[0].nat_ip, null)
}

output "bucket_url" {
  description = "URL do bucket"
  value       = try("gs://${google_storage_bucket.assets[0].name}", null)
}

output "sql_connection_name" {
  description = "Connection name do Cloud SQL"
  value       = try(google_sql_database_instance.main[0].connection_name, null)
  sensitive   = true
}
'''

        (project_dir / "outputs.tf").write_text(content, encoding="utf-8")

    def _generate_provider(
        self,
        project_dir: Path,
        provider: ProviderType,
        config: Dict[str, Any]
    ):
        """Gera arquivo provider.tf"""
        content = '''# =============================================================================
# Provider Configuration
# =============================================================================

'''

        if provider == ProviderType.AWS:
            content += '''provider "aws" {
  region = var.aws_region

  default_tags {
    tags = local.common_tags
  }
}
'''

        elif provider == ProviderType.AZURE:
            content += '''provider "azurerm" {
  features {}
}

resource "azurerm_resource_group" "main" {
  name     = var.resource_group_name
  location = var.azure_location
  tags     = local.common_tags
}
'''

        elif provider == ProviderType.GCP:
            content += '''provider "google" {
  project = var.gcp_project
  region  = var.gcp_region
  zone    = var.gcp_zone
}
'''

        (project_dir / "provider.tf").write_text(content, encoding="utf-8")

    def _generate_simple_stack(
        self,
        project_dir: Path,
        provider: ProviderType,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack simples (monolito)"""
        if provider == ProviderType.AWS:
            self._generate_aws_simple_stack(project_dir, project_name, config)
        elif provider == ProviderType.AZURE:
            self._generate_azure_simple_stack(project_dir, project_name, config)
        elif provider == ProviderType.GCP:
            self._generate_gcp_simple_stack(project_dir, project_name, config)

    def _generate_aws_simple_stack(
        self,
        project_dir: Path,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack AWS simples"""
        content = '''# =============================================================================
# AWS Simple Stack - EC2 + S3 + RDS
# =============================================================================

# Data sources
data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"] # Canonical

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }
}

data "aws_vpc" "default" {
  default = true
}

data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

# Security Group
resource "aws_security_group" "main" {
  name        = "${local.project_name}-sg"
  description = "Security group for ${local.project_name}"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "SSH"
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "HTTP"
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "HTTPS"
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.project_name}-sg"
  })
}

# EC2 Instance
resource "aws_instance" "main" {
  count = 1

  ami           = data.aws_ami.ubuntu.id
  instance_type = var.instance_type

  vpc_security_group_ids = [aws_security_group.main.id]

  root_block_device {
    volume_size = 20
    volume_type = "gp3"
  }

  tags = merge(local.common_tags, {
    Name = "${local.project_name}-server"
  })
}

# S3 Bucket
resource "aws_s3_bucket" "assets" {
  count  = 1
  bucket = "${local.project_name}-assets-${random_string.bucket_suffix.result}"

  tags = local.common_tags
}

resource "random_string" "bucket_suffix" {
  length  = 8
  special = false
  upper   = false
}

resource "aws_s3_bucket_public_access_block" "assets" {
  count  = 1
  bucket = aws_s3_bucket.assets[0].id

  block_public_acls       = false
  block_public_policy     = false
  ignore_public_acls      = false
  restrict_public_buckets = false
}

# RDS PostgreSQL (optional)
variable "create_database" {
  description = "Criar banco de dados RDS"
  type        = bool
  default     = false
}

resource "aws_db_subnet_group" "main" {
  count      = var.create_database ? 1 : 0
  name       = "${local.project_name}-db-subnet"
  subnet_ids = data.aws_subnets.default.ids

  tags = local.common_tags
}

resource "aws_security_group" "db" {
  count       = var.create_database ? 1 : 0
  name        = "${local.project_name}-db-sg"
  description = "Security group for RDS"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.main.id]
  }

  tags = local.common_tags
}

resource "aws_db_instance" "main" {
  count = var.create_database ? 1 : 0

  identifier     = "${local.project_name}-db"
  engine         = "postgres"
  engine_version = "15"
  instance_class = var.db_instance_class

  allocated_storage = 20
  storage_type      = "gp3"
  storage_encrypted = true

  db_name  = replace(local.project_name, "-", "_")
  username = "admin"
  password = random_password.db_password[0].result

  db_subnet_group_name   = aws_db_subnet_group.main[0].name
  vpc_security_group_ids = [aws_security_group.db[0].id]

  skip_final_snapshot = true

  tags = local.common_tags
}

resource "random_password" "db_password" {
  count   = var.create_database ? 1 : 0
  length  = 24
  special = false
}
'''

        (project_dir / "stack.tf").write_text(content, encoding="utf-8")

    def _generate_azure_simple_stack(
        self,
        project_dir: Path,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack Azure simples"""
        content = '''# =============================================================================
# Azure Simple Stack - VM + Storage + PostgreSQL
# =============================================================================

# Network
resource "azurerm_virtual_network" "main" {
  name                = "${local.project_name}-vnet"
  address_space       = ["10.0.0.0/16"]
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  tags                = local.common_tags
}

resource "azurerm_subnet" "main" {
  name                 = "${local.project_name}-subnet"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = ["10.0.1.0/24"]
}

# Public IP
resource "azurerm_public_ip" "main" {
  count               = 1
  name                = "${local.project_name}-pip"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  allocation_method   = "Dynamic"
  tags                = local.common_tags
}

# Network Interface
resource "azurerm_network_interface" "main" {
  count               = 1
  name                = "${local.project_name}-nic"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name

  ip_configuration {
    name                          = "internal"
    subnet_id                     = azurerm_subnet.main.id
    private_ip_address_allocation = "Dynamic"
    public_ip_address_id          = azurerm_public_ip.main[0].id
  }

  tags = local.common_tags
}

# Network Security Group
resource "azurerm_network_security_group" "main" {
  name                = "${local.project_name}-nsg"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name

  security_rule {
    name                       = "SSH"
    priority                   = 1001
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "22"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }

  security_rule {
    name                       = "HTTP"
    priority                   = 1002
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "80"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }

  tags = local.common_tags
}

# Virtual Machine
resource "azurerm_linux_virtual_machine" "main" {
  count               = 1
  name                = "${local.project_name}-vm"
  resource_group_name = azurerm_resource_group.main.name
  location            = azurerm_resource_group.main.location
  size                = var.vm_size
  admin_username      = "azureuser"

  network_interface_ids = [azurerm_network_interface.main[0].id]

  admin_ssh_key {
    username   = "azureuser"
    public_key = file("~/.ssh/id_rsa.pub")
  }

  os_disk {
    caching              = "ReadWrite"
    storage_account_type = "Standard_LRS"
  }

  source_image_reference {
    publisher = "Canonical"
    offer     = "0001-com-ubuntu-server-jammy"
    sku       = "22_04-lts-gen2"
    version   = "latest"
  }

  tags = local.common_tags
}

# Storage Account
resource "azurerm_storage_account" "main" {
  count                    = 1
  name                     = replace("${local.project_name}storage", "-", "")
  resource_group_name      = azurerm_resource_group.main.name
  location                 = azurerm_resource_group.main.location
  account_tier             = "Standard"
  account_replication_type = "LRS"
  tags                     = local.common_tags
}

# PostgreSQL (optional)
variable "create_database" {
  description = "Criar banco de dados PostgreSQL"
  type        = bool
  default     = false
}

resource "azurerm_postgresql_flexible_server" "main" {
  count               = var.create_database ? 1 : 0
  name                = "${local.project_name}-pgsql"
  resource_group_name = azurerm_resource_group.main.name
  location            = azurerm_resource_group.main.location
  version             = "15"

  administrator_login    = "pgadmin"
  administrator_password = random_password.db_password[0].result

  sku_name   = "B_Standard_B1ms"
  storage_mb = 32768

  tags = local.common_tags
}

resource "random_password" "db_password" {
  count   = var.create_database ? 1 : 0
  length  = 24
  special = false
}
'''

        (project_dir / "stack.tf").write_text(content, encoding="utf-8")

    def _generate_gcp_simple_stack(
        self,
        project_dir: Path,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack GCP simples"""
        content = '''# =============================================================================
# GCP Simple Stack - Compute Engine + Cloud Storage + Cloud SQL
# =============================================================================

# Firewall Rules
resource "google_compute_firewall" "allow_http" {
  name    = "${local.project_name}-allow-http"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["http-server"]
}

resource "google_compute_firewall" "allow_ssh" {
  name    = "${local.project_name}-allow-ssh"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["ssh-server"]
}

# Compute Instance
resource "google_compute_instance" "main" {
  count        = 1
  name         = "${local.project_name}-server"
  machine_type = var.machine_type
  zone         = var.gcp_zone

  boot_disk {
    initialize_params {
      image = "ubuntu-os-cloud/ubuntu-2204-lts"
      size  = 20
    }
  }

  network_interface {
    network = "default"
    access_config {}
  }

  tags = ["http-server", "https-server", "ssh-server"]

  labels = {
    project     = lower(replace(local.project_name, "-", "_"))
    environment = var.environment
    managed_by  = "terraform"
  }
}

# Cloud Storage Bucket
resource "google_storage_bucket" "assets" {
  count         = 1
  name          = "${local.project_name}-assets-${random_string.bucket_suffix.result}"
  location      = var.gcp_region
  force_destroy = true

  uniform_bucket_level_access = true

  labels = {
    project = lower(replace(local.project_name, "-", "_"))
  }
}

resource "random_string" "bucket_suffix" {
  length  = 8
  special = false
  upper   = false
}

# Cloud SQL (optional)
variable "create_database" {
  description = "Criar banco de dados Cloud SQL"
  type        = bool
  default     = false
}

resource "google_sql_database_instance" "main" {
  count            = var.create_database ? 1 : 0
  name             = "${local.project_name}-db"
  database_version = "POSTGRES_15"
  region           = var.gcp_region

  settings {
    tier = "db-f1-micro"

    ip_configuration {
      ipv4_enabled = true
      authorized_networks {
        name  = "all"
        value = "0.0.0.0/0"
      }
    }

    user_labels = {
      project = lower(replace(local.project_name, "-", "_"))
    }
  }

  deletion_protection = false
}

resource "google_sql_user" "admin" {
  count    = var.create_database ? 1 : 0
  name     = "admin"
  instance = google_sql_database_instance.main[0].name
  password = random_password.db_password[0].result
}

resource "random_password" "db_password" {
  count   = var.create_database ? 1 : 0
  length  = 24
  special = false
}
'''

        (project_dir / "stack.tf").write_text(content, encoding="utf-8")

    def _generate_serverless_stack(
        self,
        project_dir: Path,
        provider: ProviderType,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack serverless"""
        if provider == ProviderType.AWS:
            content = '''# =============================================================================
# AWS Serverless Stack - Lambda + API Gateway + S3
# =============================================================================

# S3 Bucket for code
resource "aws_s3_bucket" "lambda_code" {
  bucket = "${local.project_name}-lambda-code-${random_string.bucket_suffix.result}"
  tags   = local.common_tags
}

resource "random_string" "bucket_suffix" {
  length  = 8
  special = false
  upper   = false
}

# IAM Role for Lambda
resource "aws_iam_role" "lambda" {
  name = "${local.project_name}-lambda-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "lambda.amazonaws.com"
      }
    }]
  })

  tags = local.common_tags
}

resource "aws_iam_role_policy_attachment" "lambda_basic" {
  role       = aws_iam_role.lambda.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

# Lambda Function (placeholder)
resource "aws_lambda_function" "api" {
  count         = 0 # Set to 1 and provide code
  function_name = "${local.project_name}-api"
  role          = aws_iam_role.lambda.arn
  handler       = "handler.main"
  runtime       = "python3.11"
  memory_size   = 256
  timeout       = 30

  s3_bucket = aws_s3_bucket.lambda_code.id
  s3_key    = "function.zip"

  tags = local.common_tags
}
'''
        elif provider == ProviderType.AZURE:
            content = '''# =============================================================================
# Azure Serverless Stack - Functions + Storage
# =============================================================================

# Storage Account for Functions
resource "azurerm_storage_account" "functions" {
  name                     = replace("${local.project_name}func", "-", "")
  resource_group_name      = azurerm_resource_group.main.name
  location                 = azurerm_resource_group.main.location
  account_tier             = "Standard"
  account_replication_type = "LRS"
  tags                     = local.common_tags
}

# App Service Plan (Consumption)
resource "azurerm_service_plan" "functions" {
  name                = "${local.project_name}-plan"
  resource_group_name = azurerm_resource_group.main.name
  location            = azurerm_resource_group.main.location
  os_type             = "Linux"
  sku_name            = "Y1"
  tags                = local.common_tags
}

# Function App (placeholder)
resource "azurerm_linux_function_app" "api" {
  count               = 0 # Set to 1 when ready
  name                = "${local.project_name}-func"
  resource_group_name = azurerm_resource_group.main.name
  location            = azurerm_resource_group.main.location

  storage_account_name       = azurerm_storage_account.functions.name
  storage_account_access_key = azurerm_storage_account.functions.primary_access_key
  service_plan_id            = azurerm_service_plan.functions.id

  site_config {
    application_stack {
      python_version = "3.11"
    }
  }

  tags = local.common_tags
}
'''
        else:  # GCP
            content = '''# =============================================================================
# GCP Serverless Stack - Cloud Functions + Cloud Storage
# =============================================================================

# Storage Bucket for code
resource "google_storage_bucket" "functions" {
  name          = "${local.project_name}-functions-${random_string.bucket_suffix.result}"
  location      = var.gcp_region
  force_destroy = true

  uniform_bucket_level_access = true

  labels = {
    project = lower(replace(local.project_name, "-", "_"))
  }
}

resource "random_string" "bucket_suffix" {
  length  = 8
  special = false
  upper   = false
}

# Cloud Function (placeholder - requires source code)
# resource "google_cloudfunctions2_function" "api" {
#   name     = "${local.project_name}-api"
#   location = var.gcp_region
#
#   build_config {
#     runtime     = "python311"
#     entry_point = "main"
#     source {
#       storage_source {
#         bucket = google_storage_bucket.functions.name
#         object = "function.zip"
#       }
#     }
#   }
#
#   service_config {
#     max_instance_count = 100
#     available_memory   = "256M"
#     timeout_seconds    = 60
#   }
# }
'''

        (project_dir / "stack.tf").write_text(content, encoding="utf-8")

    def _generate_microservices_stack(
        self,
        project_dir: Path,
        provider: ProviderType,
        project_name: str,
        config: Dict[str, Any]
    ):
        """Gera stack de microservicos (Kubernetes)"""
        content = f'''# =============================================================================
# Microservices Stack - Kubernetes
# Provider: {provider.value.upper()}
# =============================================================================

# NOTE: Esta configuracao requer um cluster Kubernetes.
# Para producao, considere usar:
# - AWS: EKS
# - Azure: AKS
# - GCP: GKE

# Placeholder para configuracao Kubernetes
# Consulte a documentacao do provider para detalhes de configuracao.
'''

        (project_dir / "stack.tf").write_text(content, encoding="utf-8")

    def generate_tfvars(
        self,
        project_name: str,
        provider: ProviderType,
        config: Dict[str, Any]
    ) -> str:
        """
        Gera arquivo terraform.tfvars.

        Args:
            project_name: Nome do projeto
            provider: Provider cloud
            config: Configuracoes

        Returns:
            Conteudo do arquivo tfvars
        """
        content = f'''# =============================================================================
# Terraform Variables
# Project: {project_name}
# =============================================================================

project_name = "{project_name}"
environment  = "dev"

'''

        if provider == ProviderType.AWS:
            content += f'''aws_region        = "{config.get('region', 'us-east-1')}"
instance_type     = "{config.get('instance_type', 't3.micro')}"
db_instance_class = "{config.get('db_instance_class', 'db.t3.micro')}"
create_database   = {str(config.get('create_database', False)).lower()}
'''

        elif provider == ProviderType.AZURE:
            content += f'''azure_location       = "{config.get('location', 'eastus')}"
resource_group_name  = "{config.get('resource_group', 'rg-' + project_name)}"
vm_size              = "{config.get('vm_size', 'Standard_B1s')}"
create_database      = {str(config.get('create_database', False)).lower()}
'''

        elif provider == ProviderType.GCP:
            content += f'''gcp_project     = "{config.get('project_id', '')}"
gcp_region      = "{config.get('region', 'us-central1')}"
gcp_zone        = "{config.get('zone', 'us-central1-a')}"
machine_type    = "{config.get('machine_type', 'e2-micro')}"
create_database = {str(config.get('create_database', False)).lower()}
'''

        return content
