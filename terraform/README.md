# Terraform Infrastructure - Fabrica de Agentes

## Issue #169: Remote State Configuration

Este diretório contém a configuração Terraform para a infraestrutura da Fábrica de Agentes.

## Quick Start

### 1. Bootstrap (Primeira vez apenas)

Antes de usar remote state, crie a infraestrutura de state:

```bash
cd terraform/bootstrap
terraform init
terraform plan
terraform apply
```

Isso cria:
- S3 Bucket: `fabrica-agentes-terraform-state`
- DynamoDB Table: `fabrica-agentes-terraform-locks`
- KMS Key para encryption

### 2. Inicializar com Remote State

Após o bootstrap, inicialize o Terraform principal com remote state:

```bash
cd terraform

# Para desenvolvimento
terraform init -backend-config=environments/dev.tfbackend

# Para staging
terraform init -backend-config=environments/staging.tfbackend

# Para produção
terraform init -backend-config=environments/prod.tfbackend
```

### 3. Migrar State Local para Remote

Se você já tem state local e quer migrar:

```bash
terraform init -migrate-state -backend-config=environments/dev.tfbackend
```

## Estrutura de Diretórios

```
terraform/
├── main.tf                 # Configuração principal
├── variables.tf            # Variáveis
├── outputs.tf              # Outputs
├── backend.tf              # Configuração de backend S3
├── multi-az.tf             # Configuração multi-AZ
├── environments/           # Backend configs por ambiente
│   ├── dev.tfbackend
│   ├── staging.tfbackend
│   └── prod.tfbackend
├── bootstrap/              # Recursos para remote state
│   └── main.tf
└── modules/                # Módulos por cloud provider
    ├── aws/
    ├── azure/
    └── gcp/
```

## Segurança do State

O state remoto inclui:

- **Encryption at Rest**: KMS encryption no S3
- **Encryption in Transit**: TLS obrigatório (bucket policy)
- **Locking**: DynamoDB para prevenir conflitos
- **Versioning**: Histórico de versões no S3
- **Access Logging**: Logs de acesso no S3

## Variáveis de Ambiente

```bash
# AWS Credentials
export AWS_ACCESS_KEY_ID="..."
export AWS_SECRET_ACCESS_KEY="..."
export AWS_REGION="us-east-1"

# Ou use AWS profiles
export AWS_PROFILE="fabrica-agentes"
```

## Comandos Úteis

```bash
# Ver estado atual
terraform state list

# Ver recursos específicos
terraform state show module.aws[0].aws_ecs_cluster.main

# Importar recurso existente
terraform import module.aws[0].aws_s3_bucket.uploads BUCKET_NAME

# Refresh state
terraform refresh

# Plan com output
terraform plan -out=tfplan
terraform apply tfplan
```

## Troubleshooting

### Lock não liberado

Se um lock ficar preso:

```bash
terraform force-unlock LOCK_ID
```

### State corrompido

Restaure uma versão anterior do S3:

1. Vá para o console S3
2. Navegue até o bucket de state
3. Selecione o arquivo .tfstate
4. "Versions" → Restaure versão anterior

## Contribuindo

1. Sempre use remote state em ambientes compartilhados
2. Nunca commite arquivos `.tfstate` ou `.tfbackend` com credenciais
3. Use `terraform plan` antes de `apply`
4. Documente mudanças significativas
