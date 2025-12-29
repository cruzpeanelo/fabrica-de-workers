# -*- coding: utf-8 -*-
"""
Cloud Provisioner
=================
Orquestrador unificado de provisionamento multi-cloud.

Este modulo fornece uma interface unica para provisionar infraestrutura
em qualquer provider suportado (AWS, Azure, GCP).
"""

from datetime import datetime
from typing import Any, Dict, List, Optional, Union
import logging
import os

from .base_provider import (
    CloudProvider,
    CloudResource,
    ResourceType,
    ResourceStatus,
    DeploymentResult,
    CostEstimate,
    StackType,
    ProviderType,
    AWSConfig,
    AzureConfig,
    GCPConfig,
)

logger = logging.getLogger(__name__)


class CloudProvisioner:
    """
    Orquestrador de provisionamento multi-cloud.

    Fornece uma interface unica para:
    - Provisionar infraestrutura em qualquer provider
    - Comparar custos entre providers
    - Gerenciar recursos de forma unificada
    - Migrar entre providers

    Exemplo de uso:
        ```python
        # Inicializar provisioner
        provisioner = CloudProvisioner()

        # Configurar providers
        provisioner.configure_aws(
            access_key_id="xxx",
            secret_access_key="xxx",
            region="us-east-1"
        )

        # Deploy de stack
        result = await provisioner.deploy(
            project_name="meu-projeto",
            provider="aws",
            stack_type="simple",
            config={
                "instance_type": "t3.micro",
                "database": {"engine": "postgres"}
            }
        )
        ```
    """

    def __init__(self):
        """Inicializa o provisioner."""
        self.providers: Dict[ProviderType, CloudProvider] = {}
        self.active_deployments: Dict[str, DeploymentResult] = {}

    # =========================================================================
    # Configuracao de Providers
    # =========================================================================

    def configure_aws(
        self,
        access_key_id: Optional[str] = None,
        secret_access_key: Optional[str] = None,
        region: str = "us-east-1",
        session_token: Optional[str] = None,
        profile_name: Optional[str] = None
    ) -> bool:
        """
        Configura o provider AWS.

        Args:
            access_key_id: AWS Access Key ID
            secret_access_key: AWS Secret Access Key
            region: Regiao AWS
            session_token: Token de sessao (opcional)
            profile_name: Nome do profile AWS CLI (opcional)

        Returns:
            True se configurou com sucesso
        """
        try:
            from .aws import AWSProvider

            config = AWSConfig(
                region=region,
                access_key_id=access_key_id or os.getenv("AWS_ACCESS_KEY_ID", ""),
                secret_access_key=secret_access_key or os.getenv("AWS_SECRET_ACCESS_KEY", ""),
                session_token=session_token,
                profile_name=profile_name,
            )

            self.providers[ProviderType.AWS] = AWSProvider(config)
            logger.info("Provider AWS configurado")
            return True

        except ImportError:
            logger.error("AWS SDK (boto3) nao instalado")
            return False
        except Exception as e:
            logger.error(f"Erro ao configurar AWS: {e}")
            return False

    def configure_azure(
        self,
        subscription_id: Optional[str] = None,
        tenant_id: Optional[str] = None,
        client_id: Optional[str] = None,
        client_secret: Optional[str] = None,
        resource_group: str = "rg-fabrica",
        location: str = "eastus"
    ) -> bool:
        """
        Configura o provider Azure.

        Args:
            subscription_id: ID da subscription Azure
            tenant_id: ID do tenant Azure AD
            client_id: Client ID do Service Principal
            client_secret: Client Secret do Service Principal
            resource_group: Resource Group padrao
            location: Localizacao padrao

        Returns:
            True se configurou com sucesso
        """
        try:
            from .azure import AzureProvider

            config = AzureConfig(
                region=location,
                subscription_id=subscription_id or os.getenv("AZURE_SUBSCRIPTION_ID", ""),
                tenant_id=tenant_id or os.getenv("AZURE_TENANT_ID", ""),
                client_id=client_id or os.getenv("AZURE_CLIENT_ID", ""),
                client_secret=client_secret or os.getenv("AZURE_CLIENT_SECRET", ""),
                resource_group=resource_group,
                location=location,
            )

            self.providers[ProviderType.AZURE] = AzureProvider(config)
            logger.info("Provider Azure configurado")
            return True

        except ImportError:
            logger.error("Azure SDK nao instalado")
            return False
        except Exception as e:
            logger.error(f"Erro ao configurar Azure: {e}")
            return False

    def configure_gcp(
        self,
        project_id: Optional[str] = None,
        credentials_file: Optional[str] = None,
        region: str = "us-central1",
        zone: str = "us-central1-a"
    ) -> bool:
        """
        Configura o provider GCP.

        Args:
            project_id: ID do projeto GCP
            credentials_file: Caminho do arquivo de credenciais JSON
            region: Regiao padrao
            zone: Zona padrao

        Returns:
            True se configurou com sucesso
        """
        try:
            from .gcp import GCPProvider

            config = GCPConfig(
                region=region,
                project_id=project_id or os.getenv("GCP_PROJECT_ID", ""),
                credentials_file=credentials_file or os.getenv("GOOGLE_APPLICATION_CREDENTIALS", ""),
                zone=zone,
            )

            self.providers[ProviderType.GCP] = GCPProvider(config)
            logger.info("Provider GCP configurado")
            return True

        except ImportError:
            logger.error("GCP SDK nao instalado")
            return False
        except Exception as e:
            logger.error(f"Erro ao configurar GCP: {e}")
            return False

    def configure_from_env(self) -> Dict[str, bool]:
        """
        Configura todos os providers a partir de variaveis de ambiente.

        Returns:
            Dicionario com status de configuracao de cada provider
        """
        results = {}

        # AWS
        if os.getenv("AWS_ACCESS_KEY_ID"):
            results["aws"] = self.configure_aws()
        else:
            results["aws"] = False

        # Azure
        if os.getenv("AZURE_SUBSCRIPTION_ID"):
            results["azure"] = self.configure_azure()
        else:
            results["azure"] = False

        # GCP
        if os.getenv("GCP_PROJECT_ID") or os.getenv("GOOGLE_APPLICATION_CREDENTIALS"):
            results["gcp"] = self.configure_gcp()
        else:
            results["gcp"] = False

        return results

    # =========================================================================
    # Conexao
    # =========================================================================

    async def connect(
        self,
        provider: Optional[Union[str, ProviderType]] = None
    ) -> Dict[str, bool]:
        """
        Conecta aos providers configurados.

        Args:
            provider: Provider especifico para conectar (ou todos se None)

        Returns:
            Dicionario com status de conexao de cada provider
        """
        results = {}

        if provider:
            provider_type = self._normalize_provider(provider)
            if provider_type in self.providers:
                results[provider_type.value] = await self.providers[provider_type].connect()
        else:
            for ptype, prov in self.providers.items():
                results[ptype.value] = await prov.connect()

        return results

    async def disconnect(
        self,
        provider: Optional[Union[str, ProviderType]] = None
    ) -> Dict[str, bool]:
        """
        Desconecta dos providers.

        Args:
            provider: Provider especifico para desconectar (ou todos se None)

        Returns:
            Dicionario com status de desconexao
        """
        results = {}

        if provider:
            provider_type = self._normalize_provider(provider)
            if provider_type in self.providers:
                results[provider_type.value] = await self.providers[provider_type].disconnect()
        else:
            for ptype, prov in self.providers.items():
                results[ptype.value] = await prov.disconnect()

        return results

    # =========================================================================
    # Deploy
    # =========================================================================

    async def deploy(
        self,
        project_name: str,
        provider: Union[str, ProviderType],
        stack_type: Union[str, StackType] = StackType.SIMPLE,
        config: Optional[Dict[str, Any]] = None
    ) -> DeploymentResult:
        """
        Faz deploy de uma stack em um provider.

        Args:
            project_name: Nome do projeto
            provider: Provider a usar (aws, azure, gcp)
            stack_type: Tipo da stack (simple, serverless, microservices)
            config: Configuracoes especificas

        Returns:
            DeploymentResult com recursos criados

        Raises:
            ValueError: Se provider nao configurado ou tipo invalido
        """
        provider_type = self._normalize_provider(provider)
        stack = self._normalize_stack_type(stack_type)

        if provider_type not in self.providers:
            raise ValueError(f"Provider {provider_type.value} nao configurado")

        prov = self.providers[provider_type]

        # Conectar se necessario
        if not prov.is_connected:
            connected = await prov.connect()
            if not connected:
                result = DeploymentResult(
                    success=False,
                    provider=provider_type,
                    stack_type=stack
                )
                result.add_error(f"Falha ao conectar ao {provider_type.value}")
                return result

        # Fazer deploy
        result = await prov.deploy_stack(
            project_name=project_name,
            stack_type=stack,
            config=config or {}
        )

        # Armazenar deployment
        self.active_deployments[project_name] = result

        return result

    async def teardown(
        self,
        project_name: str,
        provider: Optional[Union[str, ProviderType]] = None,
        force: bool = False
    ) -> bool:
        """
        Remove todos os recursos de um projeto.

        Args:
            project_name: Nome do projeto
            provider: Provider especifico (ou detectar automaticamente)
            force: Forcar remocao mesmo com erros

        Returns:
            True se removeu com sucesso
        """
        # Detectar provider do deployment
        if not provider and project_name in self.active_deployments:
            provider = self.active_deployments[project_name].provider

        if not provider:
            logger.error("Provider nao especificado e projeto nao encontrado")
            return False

        provider_type = self._normalize_provider(provider)

        if provider_type not in self.providers:
            logger.error(f"Provider {provider_type.value} nao configurado")
            return False

        prov = self.providers[provider_type]

        # Conectar se necessario
        if not prov.is_connected:
            await prov.connect()

        result = await prov.teardown_stack(project_name, force)

        # Remover do registro
        if result and project_name in self.active_deployments:
            del self.active_deployments[project_name]

        return result

    # =========================================================================
    # Comparacao de Custos
    # =========================================================================

    async def compare_costs(
        self,
        stack_type: Union[str, StackType],
        config: Optional[Dict[str, Any]] = None,
        providers: Optional[List[Union[str, ProviderType]]] = None
    ) -> Dict[str, CostEstimate]:
        """
        Compara custos entre providers para uma stack.

        Args:
            stack_type: Tipo da stack
            config: Configuracoes da stack
            providers: Providers a comparar (todos se None)

        Returns:
            Dicionario com estimativas de custo por provider
        """
        stack = self._normalize_stack_type(stack_type)
        estimates = {}

        target_providers = providers or list(self.providers.keys())

        for provider in target_providers:
            provider_type = self._normalize_provider(provider)

            if provider_type not in self.providers:
                continue

            prov = self.providers[provider_type]

            # Conectar se necessario
            if not prov.is_connected:
                await prov.connect()

            try:
                estimate = await prov.estimate_cost(stack, config or {})
                estimates[provider_type.value] = estimate
            except Exception as e:
                logger.warning(f"Erro ao estimar custo para {provider_type.value}: {e}")

        return estimates

    def get_cheapest_provider(
        self,
        estimates: Dict[str, CostEstimate]
    ) -> Optional[str]:
        """
        Retorna o provider mais barato baseado nas estimativas.

        Args:
            estimates: Estimativas de custo por provider

        Returns:
            Nome do provider mais barato ou None
        """
        if not estimates:
            return None

        return min(
            estimates.keys(),
            key=lambda p: estimates[p].monthly_cost
        )

    # =========================================================================
    # Listagem de Recursos
    # =========================================================================

    async def list_all_resources(
        self,
        resource_type: Optional[ResourceType] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, List[CloudResource]]:
        """
        Lista recursos de todos os providers configurados.

        Args:
            resource_type: Filtrar por tipo
            tags: Filtrar por tags

        Returns:
            Dicionario com recursos por provider
        """
        all_resources = {}

        for ptype, prov in self.providers.items():
            if not prov.is_connected:
                await prov.connect()

            try:
                resources = await prov.list_resources(resource_type, tags)
                all_resources[ptype.value] = resources
            except Exception as e:
                logger.warning(f"Erro ao listar recursos de {ptype.value}: {e}")
                all_resources[ptype.value] = []

        return all_resources

    async def get_resource(
        self,
        resource_id: str,
        provider: Optional[Union[str, ProviderType]] = None
    ) -> Optional[CloudResource]:
        """
        Obtem um recurso especifico.

        Args:
            resource_id: ID do recurso
            provider: Provider do recurso (busca em todos se None)

        Returns:
            CloudResource ou None
        """
        if provider:
            provider_type = self._normalize_provider(provider)
            if provider_type in self.providers:
                prov = self.providers[provider_type]
                if not prov.is_connected:
                    await prov.connect()
                return await prov.get_resource(resource_id)
        else:
            # Buscar em todos os providers
            for prov in self.providers.values():
                if not prov.is_connected:
                    await prov.connect()
                resource = await prov.get_resource(resource_id)
                if resource:
                    return resource

        return None

    # =========================================================================
    # Status e Informacoes
    # =========================================================================

    def get_configured_providers(self) -> List[str]:
        """Retorna lista de providers configurados."""
        return [p.value for p in self.providers.keys()]

    def get_connected_providers(self) -> List[str]:
        """Retorna lista de providers conectados."""
        return [
            p.value for p, prov in self.providers.items()
            if prov.is_connected
        ]

    def get_status(self) -> Dict[str, Any]:
        """
        Retorna status geral do provisioner.

        Returns:
            Dicionario com status
        """
        return {
            "configured_providers": self.get_configured_providers(),
            "connected_providers": self.get_connected_providers(),
            "active_deployments": list(self.active_deployments.keys()),
            "providers_status": {
                p.value: prov.get_status()
                for p, prov in self.providers.items()
            }
        }

    def get_deployment(self, project_name: str) -> Optional[DeploymentResult]:
        """
        Obtem resultado de um deployment.

        Args:
            project_name: Nome do projeto

        Returns:
            DeploymentResult ou None
        """
        return self.active_deployments.get(project_name)

    # =========================================================================
    # Helpers
    # =========================================================================

    def _normalize_provider(
        self,
        provider: Union[str, ProviderType]
    ) -> ProviderType:
        """Normaliza string para ProviderType."""
        if isinstance(provider, ProviderType):
            return provider

        provider_map = {
            "aws": ProviderType.AWS,
            "azure": ProviderType.AZURE,
            "gcp": ProviderType.GCP,
            "google": ProviderType.GCP,
        }

        provider_lower = provider.lower()
        if provider_lower in provider_map:
            return provider_map[provider_lower]

        raise ValueError(f"Provider desconhecido: {provider}")

    def _normalize_stack_type(
        self,
        stack_type: Union[str, StackType]
    ) -> StackType:
        """Normaliza string para StackType."""
        if isinstance(stack_type, StackType):
            return stack_type

        stack_map = {
            "simple": StackType.SIMPLE,
            "monolith": StackType.SIMPLE,
            "serverless": StackType.SERVERLESS,
            "microservices": StackType.MICROSERVICES,
            "kubernetes": StackType.KUBERNETES,
            "k8s": StackType.KUBERNETES,
        }

        stack_lower = stack_type.lower()
        if stack_lower in stack_map:
            return stack_map[stack_lower]

        raise ValueError(f"Tipo de stack desconhecido: {stack_type}")


# =========================================================================
# Factory Function
# =========================================================================

def create_provisioner(
    auto_configure: bool = True
) -> CloudProvisioner:
    """
    Cria e configura um CloudProvisioner.

    Args:
        auto_configure: Se True, configura automaticamente de variaveis de ambiente

    Returns:
        CloudProvisioner configurado
    """
    provisioner = CloudProvisioner()

    if auto_configure:
        provisioner.configure_from_env()

    return provisioner
