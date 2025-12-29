# Configuracao de Provedores Cloud

Este documento descreve como configurar cada provedor cloud suportado pela Fabrica de Agentes.

## Visao Geral

A Fabrica de Agentes suporta deploy automatico em tres provedores cloud:

| Provider | Servicos Suportados |
|----------|---------------------|
| **AWS** | EC2, S3, Lambda, RDS |
| **Azure** | VMs, Blob Storage, Functions, PostgreSQL/MySQL |
| **GCP** | Compute Engine, Cloud Storage, Cloud Functions, Cloud SQL |

## Configuracao Rapida

### Via Variaveis de Ambiente

A forma mais simples de configurar e via variaveis de ambiente:

```bash
# AWS
export AWS_ACCESS_KEY_ID="sua-access-key"
export AWS_SECRET_ACCESS_KEY="sua-secret-key"
export AWS_DEFAULT_REGION="us-east-1"

# Azure
export AZURE_SUBSCRIPTION_ID="sua-subscription-id"
export AZURE_TENANT_ID="seu-tenant-id"
export AZURE_CLIENT_ID="seu-client-id"
export AZURE_CLIENT_SECRET="seu-client-secret"
export AZURE_RESOURCE_GROUP="rg-fabrica"
export AZURE_LOCATION="eastus"

# GCP
export GCP_PROJECT_ID="seu-project-id"
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/credentials.json"
export GCP_REGION="us-central1"
export GCP_ZONE="us-central1-a"
```

### Via Codigo Python

```python
from factory.cloud import CloudProvisioner

# Criar provisioner
provisioner = CloudProvisioner()

# Configurar AWS
provisioner.configure_aws(
    access_key_id="xxx",
    secret_access_key="xxx",
    region="us-east-1"
)

# Configurar Azure
provisioner.configure_azure(
    subscription_id="xxx",
    tenant_id="xxx",
    client_id="xxx",
    client_secret="xxx",
    resource_group="rg-fabrica",
    location="eastus"
)

# Configurar GCP
provisioner.configure_gcp(
    project_id="xxx",
    credentials_file="/path/to/credentials.json",
    region="us-central1",
    zone="us-central1-a"
)
```

---

## AWS - Amazon Web Services

### Pre-requisitos

1. Conta AWS ativa
2. Usuario IAM com permissoes adequadas
3. AWS CLI instalado (opcional)
4. boto3 instalado: `pip install boto3`

### Obtendo Credenciais

1. Acesse o [Console AWS](https://console.aws.amazon.com/)
2. Va para **IAM** > **Users** > **Security credentials**
3. Crie um novo **Access Key**
4. Salve o **Access Key ID** e **Secret Access Key**

### Permissoes Necessarias

O usuario IAM precisa das seguintes politicas:

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "ec2:*",
                "s3:*",
                "lambda:*",
                "rds:*",
                "iam:PassRole",
                "iam:CreateRole",
                "iam:AttachRolePolicy"
            ],
            "Resource": "*"
        }
    ]
}
```

### Configuracao

```python
from factory.cloud.aws import AWSProvider
from factory.cloud.base_provider import AWSConfig

config = AWSConfig(
    access_key_id="AKIAIOSFODNN7EXAMPLE",
    secret_access_key="wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
    region="us-east-1"
)

provider = AWSProvider(config)
await provider.connect()
```

### Servicos Suportados

| Servico | Descricao | Classe |
|---------|-----------|--------|
| EC2 | Maquinas virtuais | `EC2Manager` |
| S3 | Armazenamento de objetos | `S3Manager` |
| Lambda | Funcoes serverless | `LambdaManager` |
| RDS | Banco de dados gerenciado | `RDSManager` |

### Exemplo de Uso

```python
# Criar instancia EC2
result = await provider.ec2.create_instance(
    name="meu-servidor",
    instance_type="t3.micro",
    key_name="minha-chave"
)

# Criar bucket S3
result = await provider.s3.create_bucket(
    name="meu-bucket-unico",
    public=True
)

# Criar funcao Lambda
result = await provider.lambda_manager.create_function(
    name="minha-funcao",
    runtime="python3.11",
    handler="handler.main",
    code_path="./codigo"
)
```

---

## Azure - Microsoft Azure

### Pre-requisitos

1. Conta Azure ativa
2. Service Principal configurado
3. Azure CLI instalado (opcional)
4. Azure SDK instalado: `pip install azure-identity azure-mgmt-compute azure-mgmt-network azure-mgmt-storage azure-mgmt-web azure-mgmt-rdbms azure-storage-blob`

### Obtendo Credenciais

1. Acesse o [Portal Azure](https://portal.azure.com/)
2. Va para **Azure Active Directory** > **App registrations**
3. Crie um novo registro de aplicativo
4. Va para **Certificates & secrets** e crie um Client Secret
5. Anote: **Application (client) ID**, **Directory (tenant) ID**, **Client Secret**
6. Va para **Subscriptions** e anote o **Subscription ID**

### Configuracao

```python
from factory.cloud.azure import AzureProvider
from factory.cloud.base_provider import AzureConfig

config = AzureConfig(
    subscription_id="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    tenant_id="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    client_id="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    client_secret="seu-client-secret",
    resource_group="rg-fabrica",
    location="eastus"
)

provider = AzureProvider(config)
await provider.connect()
```

### Permissoes Necessarias

O Service Principal precisa da role **Contributor** no Resource Group ou Subscription.

```bash
# Via Azure CLI
az role assignment create \
    --assignee <client-id> \
    --role Contributor \
    --scope /subscriptions/<subscription-id>
```

### Servicos Suportados

| Servico | Descricao | Classe |
|---------|-----------|--------|
| Virtual Machines | Maquinas virtuais | `VMManager` |
| Blob Storage | Armazenamento de objetos | `StorageManager` |
| Functions | Funcoes serverless | `FunctionsManager` |
| PostgreSQL/MySQL | Banco de dados gerenciado | `DatabaseManager` |

### Exemplo de Uso

```python
# Criar VM
result = await provider.vm.create_vm(
    name="minha-vm",
    vm_size="Standard_B1s",
    admin_username="azureuser",
    admin_password="MinhaS3nha!Forte"
)

# Criar Storage Account e Container
result = await provider.storage.create_storage_account(
    name="meustorage"
)

# Criar Function App
result = await provider.functions.create_function_app(
    name="minha-function",
    runtime="python",
    runtime_version="3.11"
)
```

---

## GCP - Google Cloud Platform

### Pre-requisitos

1. Conta GCP ativa
2. Projeto criado
3. Service Account com chave JSON
4. gcloud CLI instalado (opcional)
5. GCP SDK instalado: `pip install google-cloud-storage google-cloud-functions google-api-python-client google-auth`

### Obtendo Credenciais

1. Acesse o [Console GCP](https://console.cloud.google.com/)
2. Va para **IAM & Admin** > **Service Accounts**
3. Crie uma nova Service Account
4. Va para **Keys** e crie uma nova chave JSON
5. Salve o arquivo JSON em local seguro

### Configuracao

```python
from factory.cloud.gcp import GCPProvider
from factory.cloud.base_provider import GCPConfig

config = GCPConfig(
    project_id="meu-projeto-123456",
    credentials_file="/path/to/service-account.json",
    region="us-central1",
    zone="us-central1-a"
)

provider = GCPProvider(config)
await provider.connect()
```

### Permissoes Necessarias

A Service Account precisa das seguintes roles:

- `roles/compute.admin` - Compute Engine
- `roles/storage.admin` - Cloud Storage
- `roles/cloudfunctions.admin` - Cloud Functions
- `roles/cloudsql.admin` - Cloud SQL
- `roles/iam.serviceAccountUser` - Para usar outras service accounts

```bash
# Via gcloud
gcloud projects add-iam-policy-binding SEU_PROJETO \
    --member="serviceAccount:SERVICE_ACCOUNT_EMAIL" \
    --role="roles/compute.admin"
```

### Servicos Suportados

| Servico | Descricao | Classe |
|---------|-----------|--------|
| Compute Engine | Maquinas virtuais | `ComputeManager` |
| Cloud Storage | Armazenamento de objetos | `CloudStorageManager` |
| Cloud Functions | Funcoes serverless | `CloudFunctionsManager` |
| Cloud SQL | Banco de dados gerenciado | `CloudSQLManager` |

### Exemplo de Uso

```python
# Criar instancia Compute Engine
result = await provider.compute.create_instance(
    name="minha-instancia",
    machine_type="e2-micro"
)

# Criar bucket Cloud Storage
result = await provider.storage.create_bucket(
    name="meu-bucket-unico"
)

# Criar Cloud Function
result = await provider.functions.create_function(
    name="minha-funcao",
    runtime="python311",
    entry_point="main"
)
```

---

## Usando o Provisioner Unificado

O `CloudProvisioner` fornece uma interface unica para todos os providers:

```python
from factory.cloud import CloudProvisioner

# Criar e configurar automaticamente via env vars
provisioner = CloudProvisioner()
provisioner.configure_from_env()

# Conectar a todos os providers
await provisioner.connect()

# Comparar custos entre providers
estimates = await provisioner.compare_costs(
    stack_type="simple",
    config={"instance_type": "small"}
)

# Ver o mais barato
cheapest = provisioner.get_cheapest_provider(estimates)
print(f"Provider mais barato: {cheapest}")

# Fazer deploy
result = await provisioner.deploy(
    project_name="meu-projeto",
    provider=cheapest,
    stack_type="simple",
    config={
        "database": {
            "engine": "postgres",
            "version": "15"
        }
    }
)

# Ver recursos criados
for resource in result.resources:
    print(f"- {resource.name}: {resource.public_ip}")

# Remover tudo
await provisioner.teardown("meu-projeto")
```

---

## Gerando Configuracao Terraform

Para usar Terraform ao inves do SDK:

```python
from factory.cloud.terraform import TerraformGenerator
from factory.cloud.base_provider import ProviderType, StackType

generator = TerraformGenerator(output_dir="./terraform")

# Gerar arquivos Terraform
project_dir = generator.generate(
    project_name="meu-projeto",
    provider=ProviderType.AWS,
    stack_type=StackType.SIMPLE,
    config={}
)

# Gerar terraform.tfvars
tfvars = generator.generate_tfvars(
    project_name="meu-projeto",
    provider=ProviderType.AWS,
    config={
        "region": "us-east-1",
        "instance_type": "t3.micro",
        "create_database": True
    }
)

print(f"Arquivos gerados em: {project_dir}")
```

Depois, aplique com:

```bash
cd terraform/meu-projeto
terraform init
terraform plan
terraform apply
```

---

## Estimativa de Custos

Cada provider fornece estimativas de custo:

```python
# Estimar custo de uma stack
estimate = await provider.estimate_cost(
    stack_type=StackType.SIMPLE,
    config={
        "instance_type": "t3.micro",
        "database": {
            "engine": "postgres",
            "instance_class": "db.t3.micro"
        }
    }
)

print(f"Custo estimado:")
print(f"  Por hora: ${estimate.hourly_cost:.4f}")
print(f"  Por dia: ${estimate.daily_cost:.2f}")
print(f"  Por mes: ${estimate.monthly_cost:.2f}")
print(f"  Por ano: ${estimate.yearly_cost:.2f}")

print(f"\nBreakdown:")
for service, cost in estimate.breakdown.items():
    print(f"  {service}: ${cost:.4f}/hora")
```

---

## Troubleshooting

### Erros Comuns

**AWS: "NoCredentialsError"**
- Verifique se as variaveis de ambiente estao definidas
- Verifique se as credenciais estao corretas

**Azure: "ClientAuthenticationError"**
- Verifique se o Service Principal esta ativo
- Verifique se o Client Secret nao expirou

**GCP: "DefaultCredentialsError"**
- Verifique se o arquivo de credenciais existe
- Verifique se `GOOGLE_APPLICATION_CREDENTIALS` esta definido

### Logs

Habilite logs para debug:

```python
import logging
logging.basicConfig(level=logging.DEBUG)
```

---

## Seguranca

**IMPORTANTE**: Nunca commite credenciais no codigo!

- Use variaveis de ambiente ou arquivos de configuracao externos
- Use IAM roles quando possivel (ex: EC2 Instance Profiles)
- Rotacione credenciais regularmente
- Use o principio do menor privilegio

---

*Fabrica de Agentes - Cloud Integration v1.0*
