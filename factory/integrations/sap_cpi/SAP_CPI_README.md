# SAP Cloud Platform Integration (CPI)

## Visao Geral

Integracao completa com SAP Cloud Platform Integration para gerenciamento de iFlows, packages, mapeamentos e scripts Groovy.

## Funcionalidades

### 1. Leitura e Analise
- Conectar via SAP CPI OData API
- Listar Integration Packages
- Listar Integration Flows (iFlows)
- Analisar mapeamentos
- Extrair configuracoes de adaptadores
- Analisar scripts Groovy
- Ler logs de execucao
- Monitorar mensagens

### 2. Desenvolvimento
- Criar Integration Packages
- Criar iFlows
- Criar/modificar mapeamentos
- Criar scripts Groovy
- Configurar adaptadores (HTTP, SFTP, SOAP, etc)
- Criar value mappings
- Configurar credenciais

### 3. Deploy e Operacao
- Deploy de iFlows
- Undeploy de iFlows
- Restart de iFlows
- Monitoramento de execucao
- Analise de erros

## Configuracao

### Variaveis de Ambiente

```bash
# Habilitar integracao
SAP_CPI_ENABLED=true

# URL do tenant CPI
SAP_CPI_TENANT_URL=https://xxx.it-cpi001.cfapps.us10.hana.ondemand.com

# OAuth 2.0 (Client Credentials Flow)
SAP_CPI_TOKEN_URL=https://xxx.authentication.us10.hana.ondemand.com/oauth/token
SAP_CPI_CLIENT_ID=seu-client-id
SAP_CPI_CLIENT_SECRET=seu-client-secret

# Opcoes adicionais
SAP_CPI_API_VERSION=1.0
SAP_CPI_TIMEOUT=30
SAP_CPI_MAX_RETRIES=3
SAP_CPI_VERIFY_SSL=true
SAP_CPI_AUTO_SYNC=false
SAP_CPI_SYNC_INTERVAL=30
```

### Configuracao via Codigo

```python
from factory.integrations.sap_cpi import SAPCPIConfig, SAPCPIIntegration

# Configuracao manual
config = SAPCPIConfig(
    enabled=True,
    tenant_url="https://xxx.it-cpi001.cfapps.us10.hana.ondemand.com",
    token_url="https://xxx.authentication.us10.hana.ondemand.com/oauth/token",
    client_id="seu-client-id",
    client_secret="seu-client-secret",
    api_version="1.0",
    timeout_seconds=30
)

# OU configuracao via ambiente
config = SAPCPIConfig.from_env()
```

## Uso Basico

### Conectando ao CPI

```python
from factory.integrations.sap_cpi import (
    SAPCPIIntegration,
    SAPCPIConfig,
    get_sap_cpi_integration
)

# Opcao 1: Instancia global
cpi = get_sap_cpi_integration()

# Opcao 2: Instancia propria
config = SAPCPIConfig.from_env()
cpi = SAPCPIIntegration(config)

# Conecta
if await cpi.connect():
    print("Conectado ao SAP CPI!")
else:
    print(f"Erro: {cpi.last_error}")
```

### Listando Packages

```python
# Lista todos os packages
packages = await cpi.package_manager.list_packages()

for pkg in packages:
    print(f"{pkg.id}: {pkg.name} (v{pkg.version})")

# Busca packages
packages = await cpi.package_manager.search_packages("SAP")

# Detalhes de um package
package = await cpi.package_manager.get_package("MyPackageId", expand_artifacts=True)
print(f"Artefatos: {package.artifact_count}")
```

### Trabalhando com iFlows

```python
# Lista iFlows de um package
iflows = await cpi.iflow_manager.list_iflows(package_id="MyPackage")

for iflow in iflows:
    print(f"{iflow.id}: {iflow.name}")

# Detalhes de um iFlow
iflow = await cpi.iflow_manager.get_iflow("MyIFlowId", expand_configurations=True)

# Configuracoes externalizadas
for config in iflow.configurations:
    print(f"{config.parameter_key} = {config.parameter_value}")

# Download do conteudo
content = await cpi.iflow_manager.download_iflow("MyIFlowId")
with open("iflow_backup.zip", "wb") as f:
    f.write(content)
```

### Deploy de iFlows

```python
# Deploy
result = await cpi.deployer.deploy_iflow(
    package_id="MyPackage",
    iflow_id="MyIFlow",
    wait_completion=True,
    timeout_seconds=300
)

if result.success:
    print(f"Deployado em: {result.deployed_on}")
else:
    print(f"Erro: {result.error_message}")

# Undeploy
result = await cpi.deployer.undeploy_iflow("MyIFlow")

# Restart
result = await cpi.deployer.restart_iflow("MyIFlow")
```

### Monitoramento

```python
# Status de runtime
status = await cpi.deployer.get_runtime_status("MyIFlow")
print(f"Status: {status['status']}")

# Logs de mensagens
logs = await cpi.deployer.get_message_logs(
    iflow_name="MyIFlow",
    hours=24,
    top=100
)

for log in logs:
    print(f"{log.message_guid}: {log.status}")

# Mensagens com falha
failed = await cpi.deployer.get_failed_messages(hours=24)
print(f"{len(failed)} mensagens com falha")

# Estatisticas
stats = await cpi.deployer.get_message_statistics(hours=24)
print(f"Taxa de sucesso: {stats['success_rate']}%")
```

## Analisadores

### Analisando iFlows

```python
from factory.integrations.sap_cpi import IFlowAnalyzer

analyzer = IFlowAnalyzer()

# Analisa a partir de ZIP
with open("iflow.zip", "rb") as f:
    content = f.read()

analysis = analyzer.analyze_from_zip(content)

# Resumo
print(analysis.get_summary())

# Diagrama ASCII
print(analyzer.get_flow_diagram(analysis))

# Verificar problemas
if analysis.errors:
    print("ERROS:")
    for error in analysis.errors:
        print(f"  - {error}")

if analysis.warnings:
    print("AVISOS:")
    for warning in analysis.warnings:
        print(f"  - {warning}")

if analysis.best_practices_violations:
    print("VIOLACOES DE BOAS PRATICAS:")
    for violation in analysis.best_practices_violations:
        print(f"  - {violation}")
```

### Analisando Scripts Groovy

```python
from factory.integrations.sap_cpi import ScriptAnalyzer

analyzer = ScriptAnalyzer()

# Analisa script
with open("script.groovy", "r") as f:
    content = f.read()

analysis = analyzer.analyze(content, "meu_script.groovy")

print(analysis.get_summary())

# Problemas de seguranca
for issue in analysis.security_issues:
    print(f"[{issue.risk_level}] {issue.description}")
    print(f"  Linha: {issue.line_number}")
    print(f"  Recomendacao: {issue.recommendation}")
```

## Geradores

### Gerando iFlows

```python
from factory.integrations.sap_cpi import IFlowGenerator
from factory.integrations.sap_cpi.generators.iflow_generator import (
    IFlowDefinition, AdapterConfig, ProcessStep, AdapterType, StepType
)

generator = IFlowGenerator()

# iFlow HTTP simples
definition = IFlowGenerator.create_http_to_http_iflow(
    iflow_id="CustomerReplication",
    name="Customer Replication",
    source_path="/api/customer",
    target_url="https://erp.example.com/api/customer"
)

# Gera ZIP
zip_content = generator.generate(definition)

with open("iflow.zip", "wb") as f:
    f.write(zip_content)
```

### Gerando Scripts Groovy

```python
from factory.integrations.sap_cpi import GroovyGenerator

generator = GroovyGenerator()

# Script de validacao
script = generator.generate_validation_script(
    name="validate_order",
    required_fields=["orderId", "customerId", "totalAmount"],
    field_validators={
        "totalAmount": "value.toDouble() > 0",
        "orderId": "value.matches('[A-Z]{2}[0-9]{8}')"
    },
    message_format="xml"
)

print(script)

# Script de transformacao
script = generator.generate_transformation_script(
    name="transform_order",
    source_format="xml",
    target_format="json",
    field_mappings={
        "order_id": "/order/id",
        "customer_name": "/order/customer/name",
        "total": "/order/totalAmount"
    }
)
```

## Skills para Agentes IA

### CPIReadSkill

```python
from factory.integrations.sap_cpi.skills import CPIReadSkill

skill = CPIReadSkill()
await skill.connect()

# Lista packages
result = await skill.list_packages()
if result.success:
    for pkg in result.data:
        print(pkg['name'])

# Analisa iFlow
result = await skill.analyze_iflow("MyPackage", "MyIFlow")
if result.success:
    print(result.data['summary'])
```

### CPIIFlowSkill

```python
from factory.integrations.sap_cpi.skills import CPIIFlowSkill

skill = CPIIFlowSkill()

# Gera iFlow HTTP
result = skill.generate_http_iflow(
    iflow_id="CustomerAPI",
    name="Customer API",
    source_path="/api/customer",
    target_url="https://backend.example.com/customer"
)

if result.success:
    zip_content = result.data['content']
```

### CPIDeploySkill

```python
from factory.integrations.sap_cpi.skills import CPIDeploySkill

skill = CPIDeploySkill()
await skill.connect()

# Deploy
result = await skill.deploy_iflow("MyPackage", "MyIFlow")
print(result.message)

# Status de saude
result = await skill.get_health_report()
print(result.data['summary'])
```

## Arquitetura

```
factory/integrations/sap_cpi/
├── __init__.py              # Exports principais
├── client.py                # Cliente OData API
├── sap_cpi.py               # Classe principal de integracao
├── package_manager.py       # Gerencia packages
├── iflow_manager.py         # Gerencia iFlows
├── deployer.py              # Deploy e operacao
├── analyzers/
│   ├── __init__.py
│   ├── iflow_analyzer.py    # Analisa iFlows
│   ├── mapping_analyzer.py  # Analisa mapeamentos
│   └── script_analyzer.py   # Analisa Groovy
├── generators/
│   ├── __init__.py
│   ├── iflow_generator.py   # Gera iFlows
│   ├── mapping_generator.py # Gera mapeamentos
│   └── groovy_generator.py  # Gera scripts
└── skills/
    ├── __init__.py
    ├── cpi_read_skill.py    # Skill de leitura
    ├── cpi_iflow_skill.py   # Skill de iFlows
    └── cpi_deploy_skill.py  # Skill de deploy
```

## Dependencias

```bash
pip install aiohttp    # Cliente HTTP async
pip install lxml       # Parsing XML (opcional, para validacao)
```

## Troubleshooting

### Erro de Autenticacao (401)
- Verifique client_id e client_secret
- Confirme que o token_url esta correto
- Verifique se as credenciais tem permissao para API

### Erro de Acesso (403)
- Verifique permissoes do usuario no CPI
- Confirme que o tenant_url esta correto
- Verifique roles atribuidas as credenciais

### Timeout no Deploy
- Aumente timeout_seconds
- Verifique conectividade de rede
- Verifique se nao ha deploy em andamento

### iFlow nao Inicia
- Verifique logs de erro no CPI
- Confirme configuracoes de adaptadores
- Verifique credenciais referenciadas

## Versao

v1.0.0 - Implementacao inicial
- Suporte a packages e iFlows
- Analisadores de artefatos
- Geradores de codigo
- Deploy e monitoramento
- Skills para agentes IA
