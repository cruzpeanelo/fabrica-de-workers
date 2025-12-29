# =============================================================================
# Fabrica de Agentes - Multi-AZ Infrastructure Configuration
# =============================================================================
# Issue #110: Multi-AZ Real para Todos Componentes
#
# Este arquivo configura:
#   - VPC com subnets em multiplas AZs
#   - RDS Multi-AZ com failover automatico
#   - ElastiCache Multi-AZ com replicacao
#   - EKS nodes distribuidos em multiplas AZs
#   - Application Load Balancer cross-zone
#
# Uso:
#   terraform plan -var="environment=prod" -var="enable_multi_az=true"
#   terraform apply -var="environment=prod" -var="enable_multi_az=true"
# =============================================================================

# =============================================================================
# Variables
# =============================================================================

variable "enable_multi_az" {
  description = "Enable Multi-AZ deployment for all components"
  type        = bool
  default     = true
}

variable "multi_az_zones" {
  description = "List of availability zones for Multi-AZ deployment"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

variable "rds_multi_az" {
  description = "Enable Multi-AZ for RDS"
  type        = bool
  default     = true
}

variable "elasticache_multi_az" {
  description = "Enable Multi-AZ for ElastiCache"
  type        = bool
  default     = true
}

variable "eks_node_groups_per_az" {
  description = "Number of node groups per AZ"
  type        = number
  default     = 1
}

# =============================================================================
# Data Sources
# =============================================================================

data "aws_availability_zones" "available" {
  state = "available"

  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

data "aws_caller_identity" "current" {}

# =============================================================================
# Locals
# =============================================================================

locals {
  # Usar AZs disponiveis ou as especificadas
  azs = var.enable_multi_az ? slice(data.aws_availability_zones.available.names, 0, 3) : [data.aws_availability_zones.available.names[0]]

  # CIDR blocks para subnets publicas e privadas
  public_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i)
  ]

  private_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i + length(local.azs))
  ]

  database_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i + length(local.azs) * 2)
  ]

  # Tags comuns
  multi_az_tags = {
    MultiAZ     = var.enable_multi_az ? "true" : "false"
    Zones       = join(",", local.azs)
    Environment = var.environment
  }
}

# =============================================================================
# VPC Multi-AZ
# =============================================================================

resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(local.common_tags, local.multi_az_tags, {
    Name = "${local.name_prefix}-vpc"
  })
}

# =============================================================================
# Public Subnets (Multi-AZ)
# =============================================================================

resource "aws_subnet" "public" {
  count = length(local.azs)

  vpc_id                  = aws_vpc.main.id
  cidr_block              = local.public_subnet_cidrs[count.index]
  availability_zone       = local.azs[count.index]
  map_public_ip_on_launch = true

  tags = merge(local.common_tags, {
    Name                                           = "${local.name_prefix}-public-${local.azs[count.index]}"
    "kubernetes.io/role/elb"                       = "1"
    "kubernetes.io/cluster/${local.name_prefix}"   = "shared"
    Type                                           = "public"
    AZ                                             = local.azs[count.index]
  })
}

# =============================================================================
# Private Subnets (Multi-AZ) - For Application
# =============================================================================

resource "aws_subnet" "private" {
  count = length(local.azs)

  vpc_id            = aws_vpc.main.id
  cidr_block        = local.private_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = merge(local.common_tags, {
    Name                                           = "${local.name_prefix}-private-${local.azs[count.index]}"
    "kubernetes.io/role/internal-elb"              = "1"
    "kubernetes.io/cluster/${local.name_prefix}"   = "shared"
    Type                                           = "private"
    AZ                                             = local.azs[count.index]
  })
}

# =============================================================================
# Database Subnets (Multi-AZ) - For RDS/ElastiCache
# =============================================================================

resource "aws_subnet" "database" {
  count = length(local.azs)

  vpc_id            = aws_vpc.main.id
  cidr_block        = local.database_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-database-${local.azs[count.index]}"
    Type = "database"
    AZ   = local.azs[count.index]
  })
}

# =============================================================================
# Internet Gateway
# =============================================================================

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-igw"
  })
}

# =============================================================================
# NAT Gateways (One per AZ for HA)
# =============================================================================

resource "aws_eip" "nat" {
  count  = var.enable_multi_az ? length(local.azs) : 1
  domain = "vpc"

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-nat-eip-${count.index}"
    AZ   = local.azs[count.index]
  })

  depends_on = [aws_internet_gateway.main]
}

resource "aws_nat_gateway" "main" {
  count = var.enable_multi_az ? length(local.azs) : 1

  allocation_id = aws_eip.nat[count.index].id
  subnet_id     = aws_subnet.public[count.index].id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-nat-${local.azs[count.index]}"
    AZ   = local.azs[count.index]
  })

  depends_on = [aws_internet_gateway.main]
}

# =============================================================================
# Route Tables
# =============================================================================

# Public Route Table
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-public-rt"
    Type = "public"
  })
}

resource "aws_route_table_association" "public" {
  count = length(aws_subnet.public)

  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

# Private Route Tables (One per AZ for isolation)
resource "aws_route_table" "private" {
  count = length(local.azs)

  vpc_id = aws_vpc.main.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.main[var.enable_multi_az ? count.index : 0].id
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-private-rt-${local.azs[count.index]}"
    Type = "private"
    AZ   = local.azs[count.index]
  })
}

resource "aws_route_table_association" "private" {
  count = length(aws_subnet.private)

  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_route_table_association" "database" {
  count = length(aws_subnet.database)

  subnet_id      = aws_subnet.database[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

# =============================================================================
# RDS Multi-AZ
# =============================================================================

resource "aws_db_subnet_group" "main" {
  name       = "${local.name_prefix}-db-subnet-group"
  subnet_ids = aws_subnet.database[*].id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-db-subnet-group"
  })
}

resource "aws_db_instance" "main" {
  identifier = "${local.name_prefix}-postgres"

  # Engine
  engine         = "postgres"
  engine_version = "15.4"

  # Instance
  instance_class        = local.db_config[var.environment].instance_class
  allocated_storage     = local.db_config[var.environment].allocated_storage
  max_allocated_storage = local.db_config[var.environment].allocated_storage * 2
  storage_type          = "gp3"
  storage_encrypted     = true

  # Database
  db_name  = var.db_name
  username = var.db_username
  password = random_password.db_password.result
  port     = var.db_port

  # Multi-AZ
  multi_az               = var.rds_multi_az && var.enable_multi_az
  db_subnet_group_name   = aws_db_subnet_group.main.name
  vpc_security_group_ids = [aws_security_group.rds.id]

  # Backup
  backup_retention_period   = var.backup_retention_period
  backup_window             = "03:00-04:00"
  maintenance_window        = "sun:04:00-sun:05:00"
  delete_automated_backups  = false
  copy_tags_to_snapshot     = true
  final_snapshot_identifier = "${local.name_prefix}-final-snapshot"
  skip_final_snapshot       = var.environment != "prod"

  # Performance Insights
  performance_insights_enabled          = var.environment == "prod"
  performance_insights_retention_period = var.environment == "prod" ? 7 : 0

  # Enhanced Monitoring
  monitoring_interval = var.enable_enhanced_monitoring ? 60 : 0
  monitoring_role_arn = var.enable_enhanced_monitoring ? aws_iam_role.rds_monitoring[0].arn : null

  # Options
  auto_minor_version_upgrade  = true
  deletion_protection         = var.environment == "prod"
  publicly_accessible         = false
  apply_immediately           = var.environment != "prod"
  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]

  tags = merge(local.common_tags, local.multi_az_tags, {
    Name = "${local.name_prefix}-postgres"
  })
}

# RDS Read Replica (for read scaling in prod)
resource "aws_db_instance" "replica" {
  count = var.environment == "prod" && var.enable_multi_az ? 1 : 0

  identifier = "${local.name_prefix}-postgres-replica"

  # Replica config
  replicate_source_db = aws_db_instance.main.identifier
  instance_class      = local.db_config[var.environment].instance_class
  storage_encrypted   = true

  # Network
  vpc_security_group_ids = [aws_security_group.rds.id]
  publicly_accessible    = false

  # Place in different AZ
  availability_zone = local.azs[1]

  # Options
  auto_minor_version_upgrade = true
  skip_final_snapshot        = true
  apply_immediately          = true

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-postgres-replica"
    Role = "read-replica"
    AZ   = local.azs[1]
  })
}

# =============================================================================
# ElastiCache Multi-AZ (Redis)
# =============================================================================

resource "aws_elasticache_subnet_group" "main" {
  name       = "${local.name_prefix}-redis-subnet-group"
  subnet_ids = aws_subnet.database[*].id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-redis-subnet-group"
  })
}

resource "aws_elasticache_replication_group" "main" {
  replication_group_id = "${local.name_prefix}-redis"
  description          = "Redis cluster for Fabrica de Agentes"

  # Engine
  engine               = "redis"
  engine_version       = "7.0"
  node_type            = local.redis_config[var.environment].node_type
  port                 = 6379
  parameter_group_name = "default.redis7"

  # Multi-AZ
  automatic_failover_enabled = var.elasticache_multi_az && var.enable_multi_az
  multi_az_enabled           = var.elasticache_multi_az && var.enable_multi_az
  num_cache_clusters         = var.enable_multi_az ? 2 : 1
  subnet_group_name          = aws_elasticache_subnet_group.main.name
  security_group_ids         = [aws_security_group.redis.id]

  # Encryption
  at_rest_encryption_enabled = true
  transit_encryption_enabled = true

  # Backup
  snapshot_retention_limit = 7
  snapshot_window          = "05:00-06:00"
  maintenance_window       = "sun:06:00-sun:07:00"

  # Options
  auto_minor_version_upgrade = true
  apply_immediately          = var.environment != "prod"

  # Notifications
  notification_topic_arn = aws_sns_topic.alerts.arn

  tags = merge(local.common_tags, local.multi_az_tags, {
    Name = "${local.name_prefix}-redis"
  })
}

# =============================================================================
# EKS Multi-AZ
# =============================================================================

resource "aws_eks_cluster" "main" {
  name     = "${local.name_prefix}-cluster"
  role_arn = aws_iam_role.eks_cluster.arn
  version  = var.kubernetes_version

  vpc_config {
    subnet_ids              = concat(aws_subnet.private[*].id, aws_subnet.public[*].id)
    endpoint_private_access = true
    endpoint_public_access  = true
    security_group_ids      = [aws_security_group.eks_cluster.id]
  }

  # Enable all log types
  enabled_cluster_log_types = ["api", "audit", "authenticator", "controllerManager", "scheduler"]

  # Encryption
  encryption_config {
    provider {
      key_arn = aws_kms_key.eks.arn
    }
    resources = ["secrets"]
  }

  tags = merge(local.common_tags, local.multi_az_tags, {
    Name = "${local.name_prefix}-cluster"
  })

  depends_on = [
    aws_iam_role_policy_attachment.eks_cluster_policy,
    aws_iam_role_policy_attachment.eks_vpc_resource_controller,
  ]
}

# EKS Node Groups (Spread across AZs)
resource "aws_eks_node_group" "main" {
  count = length(local.azs)

  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "${local.name_prefix}-nodes-${local.azs[count.index]}"
  node_role_arn   = aws_iam_role.eks_node.arn
  subnet_ids      = [aws_subnet.private[count.index].id]

  # Instance configuration
  instance_types = var.environment == "prod" ? ["m5.large"] : ["t3.medium"]
  capacity_type  = var.environment == "prod" ? "ON_DEMAND" : "SPOT"
  disk_size      = 50

  # Scaling
  scaling_config {
    desired_size = local.container_config[var.environment].count
    min_size     = 1
    max_size     = local.container_config[var.environment].count * 2
  }

  # Update config
  update_config {
    max_unavailable = 1
  }

  # Labels
  labels = {
    "node-type" = "general"
    "az"        = local.azs[count.index]
  }

  # Taints (optional)
  # taint {
  #   key    = "dedicated"
  #   value  = "workers"
  #   effect = "NO_SCHEDULE"
  # }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-nodes-${local.azs[count.index]}"
    AZ   = local.azs[count.index]
  })

  depends_on = [
    aws_iam_role_policy_attachment.eks_worker_node_policy,
    aws_iam_role_policy_attachment.eks_cni_policy,
    aws_iam_role_policy_attachment.eks_ecr_policy,
  ]

  lifecycle {
    ignore_changes = [scaling_config[0].desired_size]
  }
}

# =============================================================================
# Application Load Balancer (Cross-Zone)
# =============================================================================

resource "aws_lb" "main" {
  name               = "${local.name_prefix}-alb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.alb.id]
  subnets            = aws_subnet.public[*].id

  # Cross-zone load balancing
  enable_cross_zone_load_balancing = true

  # Access logs
  access_logs {
    bucket  = aws_s3_bucket.logs.id
    prefix  = "alb-logs"
    enabled = true
  }

  # Deletion protection
  enable_deletion_protection = var.environment == "prod"

  tags = merge(local.common_tags, local.multi_az_tags, {
    Name = "${local.name_prefix}-alb"
  })
}

# =============================================================================
# Security Groups
# =============================================================================

resource "aws_security_group" "rds" {
  name        = "${local.name_prefix}-rds-sg"
  description = "Security group for RDS"
  vpc_id      = aws_vpc.main.id

  ingress {
    description     = "PostgreSQL from EKS"
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.eks_node.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-rds-sg"
  })
}

resource "aws_security_group" "redis" {
  name        = "${local.name_prefix}-redis-sg"
  description = "Security group for Redis"
  vpc_id      = aws_vpc.main.id

  ingress {
    description     = "Redis from EKS"
    from_port       = 6379
    to_port         = 6379
    protocol        = "tcp"
    security_groups = [aws_security_group.eks_node.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-redis-sg"
  })
}

resource "aws_security_group" "eks_cluster" {
  name        = "${local.name_prefix}-eks-cluster-sg"
  description = "Security group for EKS cluster"
  vpc_id      = aws_vpc.main.id

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-eks-cluster-sg"
  })
}

resource "aws_security_group" "eks_node" {
  name        = "${local.name_prefix}-eks-node-sg"
  description = "Security group for EKS nodes"
  vpc_id      = aws_vpc.main.id

  ingress {
    description = "Node to node"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    self        = true
  }

  ingress {
    description     = "From cluster"
    from_port       = 1025
    to_port         = 65535
    protocol        = "tcp"
    security_groups = [aws_security_group.eks_cluster.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-eks-node-sg"
  })
}

resource "aws_security_group" "alb" {
  name        = "${local.name_prefix}-alb-sg"
  description = "Security group for ALB"
  vpc_id      = aws_vpc.main.id

  ingress {
    description = "HTTP"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTPS"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-alb-sg"
  })
}

# =============================================================================
# SNS Topic for Alerts
# =============================================================================

resource "aws_sns_topic" "alerts" {
  name = "${local.name_prefix}-alerts"

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-alerts"
  })
}

# =============================================================================
# KMS Key for EKS
# =============================================================================

resource "aws_kms_key" "eks" {
  description             = "KMS key for EKS secrets encryption"
  deletion_window_in_days = 7
  enable_key_rotation     = true

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-eks-key"
  })
}

# =============================================================================
# S3 Bucket for Logs
# =============================================================================

resource "aws_s3_bucket" "logs" {
  bucket = "${local.name_prefix}-logs-${random_id.suffix.hex}"

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-logs"
  })
}

resource "aws_s3_bucket_lifecycle_configuration" "logs" {
  bucket = aws_s3_bucket.logs.id

  rule {
    id     = "expire-logs"
    status = "Enabled"

    expiration {
      days = var.log_retention_days
    }

    transition {
      days          = 30
      storage_class = "STANDARD_IA"
    }
  }
}

# =============================================================================
# IAM Roles
# =============================================================================

resource "aws_iam_role" "eks_cluster" {
  name = "${local.name_prefix}-eks-cluster-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "eks.amazonaws.com"
      }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "eks_cluster_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.eks_cluster.name
}

resource "aws_iam_role_policy_attachment" "eks_vpc_resource_controller" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
  role       = aws_iam_role.eks_cluster.name
}

resource "aws_iam_role" "eks_node" {
  name = "${local.name_prefix}-eks-node-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "eks_worker_node_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.eks_node.name
}

resource "aws_iam_role_policy_attachment" "eks_cni_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.eks_node.name
}

resource "aws_iam_role_policy_attachment" "eks_ecr_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.eks_node.name
}

resource "aws_iam_role" "rds_monitoring" {
  count = var.enable_enhanced_monitoring ? 1 : 0

  name = "${local.name_prefix}-rds-monitoring-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "monitoring.rds.amazonaws.com"
      }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "rds_monitoring" {
  count = var.enable_enhanced_monitoring ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonRDSEnhancedMonitoringRole"
  role       = aws_iam_role.rds_monitoring[0].name
}

# =============================================================================
# Outputs
# =============================================================================

output "vpc_id" {
  description = "VPC ID"
  value       = aws_vpc.main.id
}

output "availability_zones" {
  description = "Availability zones used"
  value       = local.azs
}

output "public_subnet_ids" {
  description = "Public subnet IDs"
  value       = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  description = "Private subnet IDs"
  value       = aws_subnet.private[*].id
}

output "database_subnet_ids" {
  description = "Database subnet IDs"
  value       = aws_subnet.database[*].id
}

output "rds_endpoint" {
  description = "RDS endpoint"
  value       = aws_db_instance.main.endpoint
}

output "rds_replica_endpoint" {
  description = "RDS replica endpoint"
  value       = var.environment == "prod" && var.enable_multi_az ? aws_db_instance.replica[0].endpoint : null
}

output "redis_endpoint" {
  description = "Redis primary endpoint"
  value       = aws_elasticache_replication_group.main.primary_endpoint_address
}

output "redis_reader_endpoint" {
  description = "Redis reader endpoint"
  value       = aws_elasticache_replication_group.main.reader_endpoint_address
}

output "eks_cluster_endpoint" {
  description = "EKS cluster endpoint"
  value       = aws_eks_cluster.main.endpoint
}

output "eks_cluster_name" {
  description = "EKS cluster name"
  value       = aws_eks_cluster.main.name
}

output "alb_dns_name" {
  description = "ALB DNS name"
  value       = aws_lb.main.dns_name
}

output "multi_az_enabled" {
  description = "Whether Multi-AZ is enabled"
  value       = var.enable_multi_az
}
