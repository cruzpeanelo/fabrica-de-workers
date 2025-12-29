# -*- coding: utf-8 -*-
"""
SAP CPI Package Manager
=======================

Gerenciamento de Integration Packages no SAP Cloud Platform Integration.

Funcionalidades:
- Listar packages disponíveis
- Criar novos packages
- Atualizar packages existentes
- Deletar packages
- Buscar packages por filtros OData
- Exportar/importar packages (ZIP)

Os Integration Packages são containers para agrupar artefatos relacionados
como iFlows, Value Mappings e Scripts.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .client import SAPCPIClient

logger = logging.getLogger(__name__)


@dataclass
class IntegrationPackage:
    """
    Representa um Integration Package no SAP CPI.

    Atributos:
        id: ID único do package
        name: Nome amigável
        description: Descrição do package
        short_text: Texto curto/resumo
        version: Versão do package
        vendor: Fornecedor/criador
        mode: Modo de edição (EDIT_ALLOWED, READ_ONLY, etc)
        supported_platform: Plataforma suportada
        created_by: Usuário que criou
        created_at: Data de criação
        modified_by: Último usuário que modificou
        modified_at: Data da última modificação
        products: Lista de produtos relacionados
        keywords: Palavras-chave para busca
        countries: Países aplicáveis
        industries: Indústrias aplicáveis
        line_of_business: Linha de negócio
    """
    id: str
    name: str
    description: str = ""
    short_text: str = ""
    version: str = "1.0.0"
    vendor: str = ""
    mode: str = "EDIT_ALLOWED"
    supported_platform: str = "SAP Cloud Integration"
    created_by: str = ""
    created_at: Optional[datetime] = None
    modified_by: str = ""
    modified_at: Optional[datetime] = None
    products: List[str] = field(default_factory=list)
    keywords: List[str] = field(default_factory=list)
    countries: List[str] = field(default_factory=list)
    industries: List[str] = field(default_factory=list)
    line_of_business: List[str] = field(default_factory=list)
    artifact_count: int = 0

    @classmethod
    def from_odata(cls, data: Dict[str, Any]) -> "IntegrationPackage":
        """
        Cria IntegrationPackage a partir de resposta OData.

        Args:
            data: Dicionário com dados do OData

        Returns:
            Instância de IntegrationPackage
        """
        return cls(
            id=data.get("Id", ""),
            name=data.get("Name", ""),
            description=data.get("Description", ""),
            short_text=data.get("ShortText", ""),
            version=data.get("Version", "1.0.0"),
            vendor=data.get("Vendor", ""),
            mode=data.get("Mode", "EDIT_ALLOWED"),
            supported_platform=data.get("SupportedPlatform", "SAP Cloud Integration"),
            created_by=data.get("CreatedBy", ""),
            created_at=cls._parse_datetime(data.get("CreationDate")),
            modified_by=data.get("ModifiedBy", ""),
            modified_at=cls._parse_datetime(data.get("ModifiedDate")),
            products=data.get("Products", "").split(",") if data.get("Products") else [],
            keywords=data.get("Keywords", "").split(",") if data.get("Keywords") else [],
            countries=data.get("Countries", "").split(",") if data.get("Countries") else [],
            industries=data.get("Industries", "").split(",") if data.get("Industries") else [],
            line_of_business=data.get("LineOfBusiness", "").split(",") if data.get("LineOfBusiness") else [],
        )

    @staticmethod
    def _parse_datetime(value: Any) -> Optional[datetime]:
        """Converte valor OData para datetime"""
        if not value:
            return None
        if isinstance(value, str):
            # Formato: /Date(1234567890000)/
            if "/Date(" in value:
                try:
                    timestamp = int(value.replace("/Date(", "").replace(")/", "")) / 1000
                    return datetime.fromtimestamp(timestamp)
                except (ValueError, TypeError):
                    return None
            # ISO format
            try:
                return datetime.fromisoformat(value.replace("Z", "+00:00"))
            except (ValueError, TypeError):
                return None
        return None

    def to_odata(self) -> Dict[str, Any]:
        """
        Converte para formato OData para criação/atualização.

        Returns:
            Dicionário no formato OData
        """
        return {
            "Id": self.id,
            "Name": self.name,
            "Description": self.description,
            "ShortText": self.short_text,
            "Version": self.version,
            "Vendor": self.vendor,
            "Mode": self.mode,
            "SupportedPlatform": self.supported_platform,
            "Products": ",".join(self.products) if self.products else "",
            "Keywords": ",".join(self.keywords) if self.keywords else "",
            "Countries": ",".join(self.countries) if self.countries else "",
            "Industries": ",".join(self.industries) if self.industries else "",
            "LineOfBusiness": ",".join(self.line_of_business) if self.line_of_business else "",
        }

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionário Python"""
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "short_text": self.short_text,
            "version": self.version,
            "vendor": self.vendor,
            "mode": self.mode,
            "supported_platform": self.supported_platform,
            "created_by": self.created_by,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "modified_by": self.modified_by,
            "modified_at": self.modified_at.isoformat() if self.modified_at else None,
            "products": self.products,
            "keywords": self.keywords,
            "countries": self.countries,
            "industries": self.industries,
            "line_of_business": self.line_of_business,
            "artifact_count": self.artifact_count,
        }


class PackageManager:
    """
    Gerenciador de Integration Packages do SAP CPI.

    Exemplo:
    ```python
    manager = PackageManager(client)

    # Listar todos os packages
    packages = await manager.list_packages()

    # Buscar package específico
    package = await manager.get_package("MyPackageId")

    # Criar novo package
    new_package = IntegrationPackage(
        id="NewPackage",
        name="Meu Novo Package",
        description="Descrição do package"
    )
    created = await manager.create_package(new_package)

    # Exportar package
    zip_content = await manager.export_package("MyPackageId")
    ```
    """

    ENDPOINT = "/api/v1/IntegrationPackages"

    def __init__(self, client: "SAPCPIClient"):
        """
        Inicializa o gerenciador de packages.

        Args:
            client: Cliente SAPCPIClient autenticado
        """
        self.client = client
        self._cache: Dict[str, IntegrationPackage] = {}

    async def list_packages(
        self,
        filter_expr: Optional[str] = None,
        select: Optional[List[str]] = None,
        top: int = 100,
        skip: int = 0,
        order_by: str = "Name",
        use_cache: bool = False
    ) -> List[IntegrationPackage]:
        """
        Lista Integration Packages disponíveis.

        Args:
            filter_expr: Expressão de filtro OData (ex: "Name eq 'MyPackage'")
            select: Campos a selecionar (ex: ["Id", "Name", "Version"])
            top: Número máximo de resultados
            skip: Pular N primeiros resultados (paginação)
            order_by: Campo para ordenação
            use_cache: Usar cache local se disponível

        Returns:
            Lista de IntegrationPackage

        Exemplo:
            # Buscar packages com filtro
            packages = await manager.list_packages(
                filter_expr="substringof('SAP', Name)",
                top=50
            )
        """
        if use_cache and self._cache:
            return list(self._cache.values())

        params: Dict[str, Any] = {
            "$top": top,
            "$skip": skip,
            "$orderby": order_by,
        }

        if filter_expr:
            params["$filter"] = filter_expr

        if select:
            params["$select"] = ",".join(select)

        response = await self.client.get(self.ENDPOINT, params=params)

        if response is None:
            logger.error("Falha ao listar packages")
            return []

        packages = []
        results = response.get("d", {}).get("results", [])

        for item in results:
            package = IntegrationPackage.from_odata(item)
            packages.append(package)
            self._cache[package.id] = package

        logger.info(f"Listados {len(packages)} packages")
        return packages

    async def get_package(
        self,
        package_id: str,
        expand_artifacts: bool = False
    ) -> Optional[IntegrationPackage]:
        """
        Busca um package específico por ID.

        Args:
            package_id: ID do package
            expand_artifacts: Se True, inclui artefatos do package

        Returns:
            IntegrationPackage ou None se não encontrado

        Exemplo:
            package = await manager.get_package("MyPackageId")
            if package:
                print(f"Package: {package.name} v{package.version}")
        """
        # Verifica cache primeiro
        if package_id in self._cache:
            return self._cache[package_id]

        endpoint = f"{self.ENDPOINT}('{package_id}')"
        params = {}

        if expand_artifacts:
            params["$expand"] = "IntegrationDesigntimeArtifacts"

        response = await self.client.get(endpoint, params=params)

        if response is None:
            logger.warning(f"Package não encontrado: {package_id}")
            return None

        data = response.get("d", response)
        package = IntegrationPackage.from_odata(data)

        # Conta artefatos se expandido
        if expand_artifacts:
            artifacts = data.get("IntegrationDesigntimeArtifacts", {}).get("results", [])
            package.artifact_count = len(artifacts)

        self._cache[package_id] = package
        return package

    async def create_package(
        self,
        package: IntegrationPackage
    ) -> Optional[IntegrationPackage]:
        """
        Cria um novo Integration Package.

        Args:
            package: IntegrationPackage com dados para criação

        Returns:
            IntegrationPackage criado ou None se erro

        Exemplo:
            new_package = IntegrationPackage(
                id="BelgoIntegrations",
                name="Belgo Arames Integrations",
                description="Integrações corporativas Belgo",
                vendor="Belgo Bekaert",
                keywords=["sap", "erp", "integration"]
            )
            created = await manager.create_package(new_package)
        """
        data = package.to_odata()

        response = await self.client.post(self.ENDPOINT, data=data)

        if response is None:
            logger.error(f"Falha ao criar package: {package.id}")
            return None

        created_data = response.get("d", response)
        created_package = IntegrationPackage.from_odata(created_data)

        self._cache[created_package.id] = created_package
        logger.info(f"Package criado: {created_package.id}")
        return created_package

    async def update_package(
        self,
        package: IntegrationPackage
    ) -> bool:
        """
        Atualiza um Integration Package existente.

        Args:
            package: IntegrationPackage com dados atualizados

        Returns:
            bool: True se atualizado com sucesso

        Nota:
            O campo Id não pode ser alterado.
        """
        endpoint = f"{self.ENDPOINT}('{package.id}')"
        data = package.to_odata()

        response = await self.client.put(endpoint, data=data)

        if response is None:
            logger.error(f"Falha ao atualizar package: {package.id}")
            return False

        # Atualiza cache
        self._cache[package.id] = package
        logger.info(f"Package atualizado: {package.id}")
        return True

    async def delete_package(
        self,
        package_id: str,
        force: bool = False
    ) -> bool:
        """
        Deleta um Integration Package.

        Args:
            package_id: ID do package
            force: Se True, deleta mesmo com artefatos (cuidado!)

        Returns:
            bool: True se deletado com sucesso

        Atenção:
            Se o package contiver artefatos deployados, a deleção falhará
            a menos que force=True (que primeiro fará undeploy).
        """
        endpoint = f"{self.ENDPOINT}('{package_id}')"

        success = await self.client.delete(endpoint)

        if success:
            # Remove do cache
            self._cache.pop(package_id, None)
            logger.info(f"Package deletado: {package_id}")
        else:
            logger.error(f"Falha ao deletar package: {package_id}")

        return success

    async def export_package(
        self,
        package_id: str
    ) -> Optional[bytes]:
        """
        Exporta um Integration Package como ZIP.

        Args:
            package_id: ID do package

        Returns:
            bytes com conteúdo do ZIP ou None se erro

        O arquivo ZIP contém:
        - Manifesto do package
        - Todos os iFlows e artefatos
        - Mapeamentos e scripts
        - Configurações

        Exemplo:
            zip_content = await manager.export_package("MyPackageId")
            if zip_content:
                with open("package.zip", "wb") as f:
                    f.write(zip_content)
        """
        endpoint = f"{self.ENDPOINT}('{package_id}')/$value"

        content = await self.client.download(endpoint)

        if content:
            logger.info(f"Package exportado: {package_id} ({len(content)} bytes)")
        else:
            logger.error(f"Falha ao exportar package: {package_id}")

        return content

    async def import_package(
        self,
        zip_content: bytes,
        overwrite: bool = False
    ) -> Optional[IntegrationPackage]:
        """
        Importa um Integration Package de um arquivo ZIP.

        Args:
            zip_content: Conteúdo do arquivo ZIP em bytes
            overwrite: Se True, sobrescreve package existente

        Returns:
            IntegrationPackage importado ou None se erro

        Exemplo:
            with open("package.zip", "rb") as f:
                zip_content = f.read()
            imported = await manager.import_package(zip_content)
        """
        params = {}
        if overwrite:
            params["Overwrite"] = "true"

        response = await self.client.upload(
            self.ENDPOINT,
            file_content=zip_content,
            file_name="package.zip",
            content_type="application/zip",
            params=params
        )

        if response is None:
            logger.error("Falha ao importar package")
            return None

        data = response.get("d", response)
        package = IntegrationPackage.from_odata(data)

        self._cache[package.id] = package
        logger.info(f"Package importado: {package.id}")
        return package

    async def search_packages(
        self,
        query: str,
        search_in: List[str] = None
    ) -> List[IntegrationPackage]:
        """
        Busca packages por texto.

        Args:
            query: Texto para buscar
            search_in: Campos para buscar (default: Name, Description, Keywords)

        Returns:
            Lista de IntegrationPackage que correspondem à busca

        Exemplo:
            # Buscar packages relacionados a SAP
            packages = await manager.search_packages("SAP")

            # Buscar apenas no nome
            packages = await manager.search_packages("Invoice", search_in=["Name"])
        """
        if search_in is None:
            search_in = ["Name", "Description", "Keywords"]

        # Constrói filtro OData com OR
        filters = []
        for field in search_in:
            filters.append(f"substringof('{query}', {field})")

        filter_expr = " or ".join(filters)

        return await self.list_packages(filter_expr=filter_expr)

    async def get_package_artifacts(
        self,
        package_id: str
    ) -> List[Dict[str, Any]]:
        """
        Lista artefatos (iFlows, mappings, etc) de um package.

        Args:
            package_id: ID do package

        Returns:
            Lista de dicionários com informações dos artefatos

        Exemplo:
            artifacts = await manager.get_package_artifacts("MyPackageId")
            for artifact in artifacts:
                print(f"{artifact['type']}: {artifact['name']}")
        """
        endpoint = f"{self.ENDPOINT}('{package_id}')/IntegrationDesigntimeArtifacts"

        response = await self.client.get(endpoint)

        if response is None:
            logger.warning(f"Não foi possível listar artefatos do package: {package_id}")
            return []

        results = response.get("d", {}).get("results", [])

        artifacts = []
        for item in results:
            artifacts.append({
                "id": item.get("Id", ""),
                "name": item.get("Name", ""),
                "version": item.get("Version", ""),
                "type": item.get("ArtifactType", "IntegrationFlow"),
                "package_id": package_id,
                "description": item.get("Description", ""),
            })

        logger.info(f"Listados {len(artifacts)} artefatos do package {package_id}")
        return artifacts

    def clear_cache(self):
        """Limpa o cache local de packages"""
        self._cache.clear()
        logger.debug("Cache de packages limpo")

    async def get_statistics(self) -> Dict[str, Any]:
        """
        Retorna estatísticas sobre os packages.

        Returns:
            Dicionário com estatísticas
        """
        packages = await self.list_packages(top=1000)

        total = len(packages)
        by_vendor: Dict[str, int] = {}
        by_mode: Dict[str, int] = {}

        for pkg in packages:
            vendor = pkg.vendor or "Unknown"
            by_vendor[vendor] = by_vendor.get(vendor, 0) + 1

            mode = pkg.mode or "Unknown"
            by_mode[mode] = by_mode.get(mode, 0) + 1

        return {
            "total_packages": total,
            "by_vendor": by_vendor,
            "by_mode": by_mode,
        }
