# Fabrica de Agentes - Infrastructure as Code

Terraform configuration for deploying Fabrica de Agentes on AWS, Azure, or GCP.

## Directory Structure

```
terraform/
├── main.tf                 # Main configuration with provider and module setup
├── variables.tf            # Input variable definitions
├── outputs.tf              # Output value definitions
├── modules/
│   ├── aws/                # AWS infrastructure module
│   │   ├── main.tf         # ECS Fargate, RDS, ElastiCache, ALB, S3, CloudWatch
│   │   ├── variables.tf
│   │   └── outputs.tf
│   ├── azure/              # Azure infrastructure module
│   │   ├── main.tf         # AKS, PostgreSQL, Redis, Application Gateway
│   │   ├── variables.tf
│   │   └── outputs.tf
│   └── gcp/                # GCP infrastructure module
│       ├── main.tf         # GKE, Cloud SQL, Memorystore, Cloud Load Balancing
│       ├── variables.tf
│       └── outputs.tf
└── environments/
    ├── dev.tfvars          # Development environment variables
    ├── staging.tfvars      # Staging environment variables
    └── prod.tfvars         # Production environment variables
```

## Prerequisites

1. **Terraform** >= 1.0.0
2. **Cloud Provider CLI**:
   - AWS: `aws configure`
   - Azure: `az login`
   - GCP: `gcloud auth application-default login`
3. **Required Permissions**: Admin access to create VPC, containers, databases, etc.

## Quick Start

### 1. Initialize Terraform

```bash
cd terraform
terraform init
```

### 2. Select Environment and Deploy

```bash
# Development
terraform plan -var-file=environments/dev.tfvars
terraform apply -var-file=environments/dev.tfvars

# Staging
terraform plan -var-file=environments/staging.tfvars
terraform apply -var-file=environments/staging.tfvars

# Production
terraform plan -var-file=environments/prod.tfvars
terraform apply -var-file=environments/prod.tfvars
```

### 3. Switch Cloud Provider

Edit the environment file or pass directly:

```bash
# Deploy to Azure
terraform apply -var-file=environments/dev.tfvars -var="cloud_provider=azure"

# Deploy to GCP
terraform apply -var-file=environments/dev.tfvars -var="cloud_provider=gcp" -var="gcp_project_id=your-project-id"

# Deploy to multiple clouds
terraform apply -var-file=environments/prod.tfvars -var="cloud_provider=multi"
```

## AWS Resources

| Resource | Description |
|----------|-------------|
| VPC | Virtual Private Cloud with public/private subnets |
| ECS Fargate | Serverless container orchestration |
| RDS PostgreSQL | Managed PostgreSQL database |
| ElastiCache Redis | Managed Redis for caching |
| ALB | Application Load Balancer |
| S3 | Object storage for uploads |
| CloudWatch | Logging and monitoring |
| Secrets Manager | Secure secrets storage |

## Azure Resources

| Resource | Description |
|----------|-------------|
| Virtual Network | Azure VNet with subnets |
| AKS | Azure Kubernetes Service |
| PostgreSQL Flexible | Managed PostgreSQL database |
| Azure Cache for Redis | Managed Redis cache |
| Application Gateway | Layer 7 load balancer |
| Storage Account | Blob storage for uploads |
| Log Analytics | Centralized logging |
| Key Vault | Secure secrets storage |
| Container Registry | Private Docker registry |

## GCP Resources

| Resource | Description |
|----------|-------------|
| VPC Network | Google Cloud VPC |
| GKE | Google Kubernetes Engine |
| Cloud SQL | Managed PostgreSQL |
| Memorystore | Managed Redis |
| Cloud Load Balancing | Global load balancer |
| Cloud Storage | Object storage |
| Secret Manager | Secure secrets |

## Environment Configuration

### Development (dev.tfvars)

- Minimal resources for cost savings
- Single NAT gateway
- No deletion protection
- 7-day log retention

### Staging (staging.tfvars)

- Medium-sized resources
- WAF enabled
- Enhanced monitoring
- 14-day log retention

### Production (prod.tfvars)

- High-availability configuration
- Multi-AZ deployments
- Deletion protection enabled
- 90-day log retention
- Auto-scaling configured

## Outputs

After deployment, you can retrieve important values:

```bash
# Get all outputs
terraform output

# Get specific outputs
terraform output aws_dashboard_url
terraform output aws_database_connection_string
terraform output aws_redis_endpoint

# Get sensitive values
terraform output -raw aws_database_connection_string
```

### Available Outputs

| Output | Description |
|--------|-------------|
| `aws_dashboard_url` | ALB DNS name |
| `aws_dashboard_endpoint` | Full URL with port |
| `aws_database_endpoint` | RDS endpoint |
| `aws_database_connection_string` | PostgreSQL connection string |
| `aws_redis_endpoint` | ElastiCache endpoint |
| `aws_redis_connection_string` | Redis connection string |
| `aws_s3_bucket_name` | S3 bucket for uploads |
| `aws_cloudwatch_log_group` | CloudWatch log group |

## Remote State (Recommended for Production)

Uncomment and configure in `main.tf`:

```hcl
backend "s3" {
  bucket         = "fabrica-agentes-terraform-state"
  key            = "terraform.tfstate"
  region         = "us-east-1"
  encrypt        = true
  dynamodb_table = "terraform-state-lock"
}
```

## Security Considerations

1. **Secrets**: Never commit `.tfvars` files with sensitive data
2. **State**: Use remote state with encryption
3. **Access**: Restrict `allowed_cidr_blocks` in production
4. **SSL**: Configure `ssl_certificate_arn` for HTTPS
5. **WAF**: Enable WAF in production environments

## Destroying Infrastructure

```bash
# Destroy specific environment
terraform destroy -var-file=environments/dev.tfvars

# Force destroy (be careful!)
terraform destroy -var-file=environments/dev.tfvars -auto-approve
```

## Troubleshooting

### Common Issues

1. **Provider authentication**: Ensure cloud CLI is configured
2. **Resource limits**: Check cloud provider quotas
3. **Name conflicts**: Resources with same names may conflict

### Logs

```bash
# Enable debug logging
export TF_LOG=DEBUG
terraform apply -var-file=environments/dev.tfvars
```

## Contributing

1. Create feature branch
2. Make changes
3. Run `terraform fmt` to format code
4. Run `terraform validate` to check syntax
5. Create pull request

---

*Fabrica de Agentes v6.0 - Infrastructure as Code*
