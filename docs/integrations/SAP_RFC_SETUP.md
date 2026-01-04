# SAP NW RFC SDK Setup Guide

## Issue #180: SAP RFC Integration Documentation

Este guia explica como configurar o SAP NetWeaver RFC SDK para usar a integração SAP ECC da Plataforma E.

## Pré-requisitos

- SAP S-User ID com acesso ao SAP Support Portal
- Credenciais RFC do sistema SAP (fornecidas pelo time SAP Basis)
- Python 3.8+
- Visual C++ Redistributable (Windows) ou glibc 2.17+ (Linux)

## 1. Download do SAP NW RFC SDK

### Passo 1: Acesse o SAP Support Portal

1. Vá para [SAP Support Portal](https://support.sap.com/en/product/connectors/nwrfcsdk.html)
2. Faça login com seu SAP S-User ID
3. Clique em "SAP NW RFC SDK" > "Tools & Services"

### Passo 2: Baixe a Versão Correta

| Sistema Operacional | Arquivo | Arquitetura |
|---------------------|---------|-------------|
| Windows 64-bit | `nwrfc750P_*-70002752.zip` | x64 |
| Linux 64-bit | `nwrfc750P_*-70002755.zip` | x64 |
| macOS (M1/M2) | `nwrfc750P_*-70002756.zip` | ARM64 |
| macOS (Intel) | `nwrfc750P_*-70002756.zip` | x64 |

**Nota**: Versão mínima recomendada: 7.50 PL12 ou superior.

## 2. Instalação

### Windows

```powershell
# 1. Extrair para C:\nwrfcsdk
Expand-Archive -Path nwrfc750P_*-70002752.zip -DestinationPath C:\nwrfcsdk

# 2. Adicionar ao PATH do sistema
$env:PATH = "C:\nwrfcsdk\lib;$env:PATH"
[System.Environment]::SetEnvironmentVariable("PATH", "C:\nwrfcsdk\lib;$env:PATH", "Machine")

# 3. Verificar instalação
C:\nwrfcsdk\bin\sapnwrfc.dll --version
```

**Estrutura esperada:**
```
C:\nwrfcsdk\
├── bin\
│   ├── sapnwrfc.dll
│   ├── icuin*.dll
│   ├── icuuc*.dll
│   └── icudt*.dll
├── lib\
│   ├── sapnwrfc.lib
│   └── *.lib
└── include\
    ├── sapnwrfc.h
    └── *.h
```

### Linux

```bash
# 1. Extrair para /usr/local/sap/nwrfcsdk
sudo mkdir -p /usr/local/sap/nwrfcsdk
sudo unzip nwrfc750P_*-70002755.zip -d /usr/local/sap/nwrfcsdk

# 2. Adicionar ao LD_LIBRARY_PATH
echo 'export SAPNWRFC_HOME=/usr/local/sap/nwrfcsdk' >> ~/.bashrc
echo 'export LD_LIBRARY_PATH=$SAPNWRFC_HOME/lib:$LD_LIBRARY_PATH' >> ~/.bashrc
source ~/.bashrc

# 3. Atualizar cache de bibliotecas
echo '/usr/local/sap/nwrfcsdk/lib' | sudo tee /etc/ld.so.conf.d/nwrfcsdk.conf
sudo ldconfig

# 4. Verificar instalação
ldconfig -p | grep sapnwrfc
```

### macOS

```bash
# 1. Extrair para /usr/local/sap/nwrfcsdk
sudo mkdir -p /usr/local/sap/nwrfcsdk
sudo unzip nwrfc750P_*-70002756.zip -d /usr/local/sap/nwrfcsdk

# 2. Adicionar ao DYLD_LIBRARY_PATH
echo 'export SAPNWRFC_HOME=/usr/local/sap/nwrfcsdk' >> ~/.zshrc
echo 'export DYLD_LIBRARY_PATH=$SAPNWRFC_HOME/lib:$DYLD_LIBRARY_PATH' >> ~/.zshrc
source ~/.zshrc

# 3. Verificar instalação
otool -L /usr/local/sap/nwrfcsdk/lib/libsapnwrfc.dylib
```

## 3. Instalação do PyRFC

```bash
# Instalar PyRFC
pip install pyrfc

# Verificar instalação
python -c "import pyrfc; print(pyrfc.__version__)"
```

### Troubleshooting PyRFC

**Erro: "Unable to load library"**
- Verifique se `SAPNWRFC_HOME` está definido
- Verifique se as bibliotecas estão no PATH/LD_LIBRARY_PATH

**Erro: "cython_runtime.pxd not found"**
```bash
pip install cython
pip install pyrfc --no-cache-dir
```

**Erro no Windows: "DLL load failed"**
- Instale Visual C++ Redistributable 2019
- Verifique se todas as DLLs estão em `C:\nwrfcsdk\lib`

## 4. Configuração de Conexão

### Variáveis de Ambiente

```bash
# .env
SAP_RFC_ASHOST=sap-server.company.com
SAP_RFC_SYSNR=00
SAP_RFC_CLIENT=100
SAP_RFC_USER=RFC_USER
SAP_RFC_PASSWD=your_password
SAP_RFC_LANG=PT

# Opcional: Pool de conexões
SAP_RFC_POOL_SIZE=5
SAP_RFC_POOL_TIMEOUT=60
```

### Arquivo de Configuração (sapnwrfc.ini)

Crie `sapnwrfc.ini` no diretório de trabalho:

```ini
[SAP_SYSTEM_DEV]
ashost=sap-dev.company.com
sysnr=00
client=100

[SAP_SYSTEM_QAS]
ashost=sap-qas.company.com
sysnr=00
client=200

[SAP_SYSTEM_PRD]
ashost=sap-prd.company.com
sysnr=00
client=300
```

## 5. Uso na Plataforma E

### Exemplo Básico

```python
from factory.integrations.sap_ecc.rfc_client import SAPRFCClient, SAPRFCConfig

# Configuração
config = SAPRFCConfig(
    ashost="sap-server.company.com",
    sysnr="00",
    client="100",
    user="RFC_USER",
    passwd="xxx",
    lang="PT"
)

# Conexão e chamada de função
async with SAPRFCClient(config) as client:
    # Chamar BAPI
    result = await client.call_function(
        "BAPI_MATERIAL_GETLIST",
        {"MAXROWS": 100}
    )
    print(result)

    # Ler tabela diretamente
    materials = await client.read_table(
        "MARA",
        fields=["MATNR", "MTART", "MATKL"],
        options=[{"TEXT": "MATNR LIKE 'A%'"}]
    )
```

### Usando Environment Variables

```python
import os
from factory.integrations.sap_ecc.rfc_client import SAPRFCClient, SAPRFCConfig

config = SAPRFCConfig(
    ashost=os.getenv("SAP_RFC_ASHOST"),
    sysnr=os.getenv("SAP_RFC_SYSNR", "00"),
    client=os.getenv("SAP_RFC_CLIENT"),
    user=os.getenv("SAP_RFC_USER"),
    passwd=os.getenv("SAP_RFC_PASSWD"),
    lang=os.getenv("SAP_RFC_LANG", "EN")
)
```

## 6. Permissões RFC Necessárias

O usuário RFC precisa das seguintes autorizações:

| Object | Campo | Valor |
|--------|-------|-------|
| S_RFC | RFC_TYPE | FUNC |
| S_RFC | RFC_NAME | RFC_* (ou funções específicas) |
| S_RFC | ACTVT | 16 |
| S_TABU_DIS | DICBERCLS | * ou classes específicas |

### Function Modules Comuns

```
RFC_READ_TABLE         - Leitura de tabelas
BAPI_MATERIAL_GETLIST  - Lista de materiais
BAPI_COMPANY_GETLIST   - Lista de empresas
BAPI_CUSTOMER_GETLIST  - Lista de clientes
RFC_FUNCTION_SEARCH    - Busca de funções
```

## 7. Troubleshooting

### Verificar Conectividade

```python
from factory.integrations.sap_ecc.rfc_client import SAPRFCClient, SAPRFCConfig

config = SAPRFCConfig(...)

async def test_connection():
    async with SAPRFCClient(config) as client:
        result = await client.call_function("RFC_SYSTEM_INFO", {})
        print(f"Sistema: {result['RFCSI_EXPORT']['RFCHOST']}")
        print(f"Instância: {result['RFCSI_EXPORT']['RFCSYSID']}")
        print(f"Versão: {result['RFCSI_EXPORT']['RFCKERNRL']}")

import asyncio
asyncio.run(test_connection())
```

### Logs Detalhados

```python
import logging
logging.basicConfig(level=logging.DEBUG)
logging.getLogger('pyrfc').setLevel(logging.DEBUG)
```

### Erros Comuns

| Erro | Causa | Solução |
|------|-------|---------|
| `RFC_COMMUNICATION_FAILURE` | Host inacessível | Verificar hostname/firewall |
| `RFC_LOGON_FAILURE` | Credenciais inválidas | Verificar user/password |
| `RFC_AUTHORIZATION_FAILURE` | Sem permissão | Solicitar autorizações ao Basis |
| `RFC_NOT_FOUND` | Função não existe | Verificar nome da função |

## 8. Docker/Container

Para usar em containers, monte o SDK como volume:

```dockerfile
FROM python:3.10-slim

# Copiar SDK
COPY nwrfcsdk /usr/local/sap/nwrfcsdk

# Configurar ambiente
ENV SAPNWRFC_HOME=/usr/local/sap/nwrfcsdk
ENV LD_LIBRARY_PATH=$SAPNWRFC_HOME/lib:$LD_LIBRARY_PATH

# Instalar dependências
RUN apt-get update && apt-get install -y libstdc++6
RUN pip install pyrfc
```

```yaml
# docker-compose.yml
services:
  app:
    build: .
    volumes:
      - /path/to/nwrfcsdk:/usr/local/sap/nwrfcsdk:ro
    environment:
      - SAPNWRFC_HOME=/usr/local/sap/nwrfcsdk
      - LD_LIBRARY_PATH=/usr/local/sap/nwrfcsdk/lib
```

## Referências

- [SAP NW RFC SDK Documentation](https://support.sap.com/en/product/connectors/nwrfcsdk.html)
- [PyRFC Documentation](https://sap.github.io/PyRFC/)
- [SAP Note 2573790](https://launchpad.support.sap.com/#/notes/2573790) - PyRFC Installation
