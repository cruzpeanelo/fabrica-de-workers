# -*- coding: utf-8 -*-
"""
GCP Compute Engine Manager
==========================
Gerenciador de VMs Compute Engine para a Fabrica de Agentes.

Funcionalidades:
- Criacao de instancias
- Start/Stop/Delete
- Monitoramento de status
- Gerenciamento de Firewall rules
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)

# Precos por hora (us-central1) - valores aproximados
MACHINE_PRICING = {
    # General Purpose - E2
    "e2-micro": 0.0084,
    "e2-small": 0.0168,
    "e2-medium": 0.0336,
    "e2-standard-2": 0.067,
    "e2-standard-4": 0.134,
    # General Purpose - N2
    "n2-standard-2": 0.0971,
    "n2-standard-4": 0.1942,
    "n2-standard-8": 0.3885,
    # Compute Optimized - C2
    "c2-standard-4": 0.2088,
    "c2-standard-8": 0.4176,
    # Memory Optimized - M2
    "m2-ultramem-208": 42.186,
}

# Imagens populares
DEFAULT_IMAGES = {
    "ubuntu-22.04": "projects/ubuntu-os-cloud/global/images/family/ubuntu-2204-lts",
    "ubuntu-20.04": "projects/ubuntu-os-cloud/global/images/family/ubuntu-2004-lts",
    "debian-12": "projects/debian-cloud/global/images/family/debian-12",
    "debian-11": "projects/debian-cloud/global/images/family/debian-11",
    "centos-stream-9": "projects/centos-cloud/global/images/family/centos-stream-9",
    "windows-2022": "projects/windows-cloud/global/images/family/windows-2022",
}


class ComputeManager:
    """
    Gerenciador de Compute Engine.

    Responsavel por todas as operacoes relacionadas a VMs:
    - Criacao e configuracao de instancias
    - Gerenciamento de ciclo de vida
    - Monitoramento
    """

    def __init__(self, compute_client, config):
        """
        Inicializa o gerenciador de Compute.

        Args:
            compute_client: Cliente Google Compute
            config: Configuracao GCP
        """
        self.client = compute_client
        self.config = config
        self.project_id = config.project_id
        self.zone = config.zone
        self.region = config.region

    async def create_instance(
        self,
        name: str,
        machine_type: str = "e2-micro",
        image: Optional[str] = None,
        disk_size_gb: int = 20,
        network: str = "default",
        external_ip: bool = True,
        metadata: Optional[Dict[str, str]] = None,
        startup_script: Optional[str] = None,
        service_account: Optional[str] = None,
        tags: Optional[List[str]] = None,
        labels: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova instancia Compute Engine.

        Args:
            name: Nome da instancia
            machine_type: Tipo da maquina (e2-micro, etc)
            image: Imagem do SO (usa Ubuntu 22.04 se None)
            disk_size_gb: Tamanho do disco em GB
            network: Nome da rede
            external_ip: Criar IP externo
            metadata: Metadados da instancia
            startup_script: Script de inicializacao
            service_account: Service account a usar
            tags: Tags de rede (para firewall)
            labels: Labels GCP

        Returns:
            Dicionario com informacoes da instancia criada
        """
        try:
            # Usar imagem padrao se nao especificada
            if not image:
                image = DEFAULT_IMAGES["ubuntu-22.04"]

            # Machine type completo
            machine_type_url = f"zones/{self.zone}/machineTypes/{machine_type}"

            # Preparar labels
            instance_labels = {"project": "fabrica-de-agentes"}
            if labels:
                instance_labels.update(labels)

            # Configurar disco boot
            disk_config = {
                "boot": True,
                "auto_delete": True,
                "initialize_params": {
                    "source_image": image,
                    "disk_size_gb": disk_size_gb,
                    "disk_type": f"zones/{self.zone}/diskTypes/pd-standard",
                }
            }

            # Configurar rede
            network_config = {
                "network": f"global/networks/{network}",
                "access_configs": []
            }

            if external_ip:
                network_config["access_configs"].append({
                    "name": "External NAT",
                    "type": "ONE_TO_ONE_NAT",
                })

            # Preparar metadados
            metadata_items = []
            if metadata:
                for key, value in metadata.items():
                    metadata_items.append({"key": key, "value": value})

            if startup_script:
                metadata_items.append({
                    "key": "startup-script",
                    "value": startup_script
                })

            # Configuracao da instancia
            instance_config = {
                "name": name,
                "machine_type": machine_type_url,
                "disks": [disk_config],
                "network_interfaces": [network_config],
                "labels": instance_labels,
                "tags": {"items": tags or ["http-server", "https-server"]},
            }

            if metadata_items:
                instance_config["metadata"] = {"items": metadata_items}

            if service_account:
                instance_config["service_accounts"] = [{
                    "email": service_account,
                    "scopes": [
                        "https://www.googleapis.com/auth/cloud-platform"
                    ]
                }]

            # Criar instancia
            operation = self.client.instances().insert(
                project=self.project_id,
                zone=self.zone,
                body=instance_config
            ).execute()

            # Aguardar operacao
            await self._wait_for_operation(operation["name"], "zone")

            # Obter informacoes da instancia criada
            instance_info = await self.get_instance(name)

            logger.info(f"Instancia GCP criada: {name}")

            return {
                "success": True,
                "instance_id": instance_info.get("id"),
                "name": name,
                "machine_type": machine_type,
                "zone": self.zone,
                "external_ip": instance_info.get("external_ip"),
                "internal_ip": instance_info.get("internal_ip"),
                "status": instance_info.get("status"),
                "hourly_cost": MACHINE_PRICING.get(machine_type, 0.02),
            }

        except Exception as e:
            logger.error(f"Erro ao criar instancia GCP: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def get_instance(self, name: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma instancia.

        Args:
            name: Nome da instancia

        Returns:
            Dicionario com informacoes da instancia
        """
        try:
            instance = self.client.instances().get(
                project=self.project_id,
                zone=self.zone,
                instance=name
            ).execute()

            # Extrair IPs
            external_ip = None
            internal_ip = None
            if instance.get("networkInterfaces"):
                ni = instance["networkInterfaces"][0]
                internal_ip = ni.get("networkIP")
                if ni.get("accessConfigs"):
                    external_ip = ni["accessConfigs"][0].get("natIP")

            return {
                "id": instance["id"],
                "name": instance["name"],
                "machine_type": instance["machineType"].split("/")[-1],
                "zone": self.zone,
                "status": instance["status"],
                "external_ip": external_ip,
                "internal_ip": internal_ip,
                "creation_timestamp": instance.get("creationTimestamp"),
                "labels": instance.get("labels", {}),
                "tags": instance.get("tags", {}).get("items", []),
            }

        except Exception as e:
            logger.error(f"Erro ao obter instancia: {e}")
            return {"error": str(e)}

    async def start_instance(self, name: str) -> bool:
        """
        Inicia uma instancia parada.

        Args:
            name: Nome da instancia

        Returns:
            True se iniciou com sucesso
        """
        try:
            operation = self.client.instances().start(
                project=self.project_id,
                zone=self.zone,
                instance=name
            ).execute()

            await self._wait_for_operation(operation["name"], "zone")
            logger.info(f"Instancia {name} iniciada")
            return True

        except Exception as e:
            logger.error(f"Erro ao iniciar instancia: {e}")
            return False

    async def stop_instance(self, name: str) -> bool:
        """
        Para uma instancia.

        Args:
            name: Nome da instancia

        Returns:
            True se parou com sucesso
        """
        try:
            operation = self.client.instances().stop(
                project=self.project_id,
                zone=self.zone,
                instance=name
            ).execute()

            await self._wait_for_operation(operation["name"], "zone")
            logger.info(f"Instancia {name} parada")
            return True

        except Exception as e:
            logger.error(f"Erro ao parar instancia: {e}")
            return False

    async def delete_instance(self, name: str) -> bool:
        """
        Deleta uma instancia.

        Args:
            name: Nome da instancia

        Returns:
            True se deletou com sucesso
        """
        try:
            operation = self.client.instances().delete(
                project=self.project_id,
                zone=self.zone,
                instance=name
            ).execute()

            await self._wait_for_operation(operation["name"], "zone")
            logger.info(f"Instancia {name} deletada")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar instancia: {e}")
            return False

    async def list_instances(
        self,
        labels: Optional[Dict[str, str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista instancias do projeto.

        Args:
            labels: Filtrar por labels

        Returns:
            Lista de instancias
        """
        try:
            # Construir filtro de labels
            filter_str = None
            if labels:
                filter_parts = [f"labels.{k}={v}" for k, v in labels.items()]
                filter_str = " AND ".join(filter_parts)

            params = {
                "project": self.project_id,
                "zone": self.zone,
            }
            if filter_str:
                params["filter"] = filter_str

            result = self.client.instances().list(**params).execute()

            instances = []
            for instance in result.get("items", []):
                # Extrair IP externo
                external_ip = None
                if instance.get("networkInterfaces"):
                    ni = instance["networkInterfaces"][0]
                    if ni.get("accessConfigs"):
                        external_ip = ni["accessConfigs"][0].get("natIP")

                instances.append({
                    "id": instance["id"],
                    "name": instance["name"],
                    "machine_type": instance["machineType"].split("/")[-1],
                    "status": instance["status"],
                    "external_ip": external_ip,
                    "labels": instance.get("labels", {}),
                })

            return instances

        except Exception as e:
            logger.error(f"Erro ao listar instancias: {e}")
            return []

    async def create_firewall_rule(
        self,
        name: str,
        allowed: List[Dict[str, Any]],
        source_ranges: Optional[List[str]] = None,
        target_tags: Optional[List[str]] = None,
        description: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria uma regra de firewall.

        Args:
            name: Nome da regra
            allowed: Protocolos/portas permitidos
            source_ranges: CIDRs de origem
            target_tags: Tags de destino
            description: Descricao

        Returns:
            Dicionario com informacoes da regra
        """
        try:
            firewall_config = {
                "name": name,
                "allowed": allowed,
                "sourceRanges": source_ranges or ["0.0.0.0/0"],
                "targetTags": target_tags,
                "description": description or f"Firewall rule {name}",
            }

            operation = self.client.firewalls().insert(
                project=self.project_id,
                body=firewall_config
            ).execute()

            await self._wait_for_operation(operation["name"], "global")

            logger.info(f"Firewall rule {name} criada")

            return {
                "success": True,
                "name": name,
            }

        except Exception as e:
            logger.error(f"Erro ao criar firewall rule: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_firewall_rule(self, name: str) -> bool:
        """
        Deleta uma regra de firewall.

        Args:
            name: Nome da regra

        Returns:
            True se deletou com sucesso
        """
        try:
            operation = self.client.firewalls().delete(
                project=self.project_id,
                firewall=name
            ).execute()

            await self._wait_for_operation(operation["name"], "global")
            logger.info(f"Firewall rule {name} deletada")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar firewall rule: {e}")
            return False

    async def _wait_for_operation(
        self,
        operation_name: str,
        operation_type: str = "zone"
    ):
        """
        Aguarda uma operacao completar.

        Args:
            operation_name: Nome da operacao
            operation_type: Tipo (zone ou global)
        """
        import time

        while True:
            if operation_type == "zone":
                result = self.client.zoneOperations().get(
                    project=self.project_id,
                    zone=self.zone,
                    operation=operation_name
                ).execute()
            else:  # global
                result = self.client.globalOperations().get(
                    project=self.project_id,
                    operation=operation_name
                ).execute()

            if result["status"] == "DONE":
                if "error" in result:
                    raise Exception(result["error"])
                return result

            time.sleep(2)

    def get_machine_pricing(self, machine_type: str) -> float:
        """
        Retorna preco por hora de um tipo de maquina.

        Args:
            machine_type: Tipo da maquina

        Returns:
            Preco por hora em USD
        """
        return MACHINE_PRICING.get(machine_type, 0.02)

    def get_available_machine_types(self) -> List[Dict[str, Any]]:
        """
        Retorna lista de tipos de maquina disponiveis.

        Returns:
            Lista de tipos com precos
        """
        return [
            {
                "type": mtype,
                "hourly_cost": price,
                "monthly_cost": round(price * 24 * 30, 2)
            }
            for mtype, price in MACHINE_PRICING.items()
        ]
