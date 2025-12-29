# -*- coding: utf-8 -*-
"""
Azure Virtual Machines Manager
==============================
Gerenciador de maquinas virtuais Azure para a Fabrica de Agentes.

Funcionalidades:
- Criacao de VMs
- Start/Stop/Deallocate
- Monitoramento de status
- Gerenciamento de Network Security Groups
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)

# Precos por hora (East US) - valores aproximados
VM_PRICING = {
    # General Purpose - B Series (Burstable)
    "Standard_B1s": 0.0104,
    "Standard_B1ms": 0.0207,
    "Standard_B2s": 0.0416,
    "Standard_B2ms": 0.0832,
    "Standard_B4ms": 0.166,
    # General Purpose - D Series
    "Standard_D2s_v3": 0.096,
    "Standard_D4s_v3": 0.192,
    "Standard_D8s_v3": 0.384,
    # Compute Optimized - F Series
    "Standard_F2s_v2": 0.085,
    "Standard_F4s_v2": 0.169,
    # Memory Optimized - E Series
    "Standard_E2s_v3": 0.126,
    "Standard_E4s_v3": 0.252,
}

# Imagens populares
DEFAULT_IMAGES = {
    "ubuntu-22.04": {
        "publisher": "Canonical",
        "offer": "0001-com-ubuntu-server-jammy",
        "sku": "22_04-lts-gen2",
        "version": "latest"
    },
    "ubuntu-20.04": {
        "publisher": "Canonical",
        "offer": "0001-com-ubuntu-server-focal",
        "sku": "20_04-lts-gen2",
        "version": "latest"
    },
    "debian-12": {
        "publisher": "Debian",
        "offer": "debian-12",
        "sku": "12-gen2",
        "version": "latest"
    },
    "windows-2022": {
        "publisher": "MicrosoftWindowsServer",
        "offer": "WindowsServer",
        "sku": "2022-datacenter-g2",
        "version": "latest"
    },
}


class VMManager:
    """
    Gerenciador de maquinas virtuais Azure.

    Responsavel por todas as operacoes relacionadas a VMs:
    - Criacao e configuracao
    - Gerenciamento de ciclo de vida
    - Monitoramento
    """

    def __init__(self, compute_client, network_client, config):
        """
        Inicializa o gerenciador de VMs.

        Args:
            compute_client: Cliente Azure Compute
            network_client: Cliente Azure Network
            config: Configuracao Azure
        """
        self.compute_client = compute_client
        self.network_client = network_client
        self.config = config
        self.resource_group = config.resource_group
        self.location = config.location

    async def create_vm(
        self,
        name: str,
        vm_size: str = "Standard_B1s",
        image_reference: Optional[Dict] = None,
        admin_username: str = "azureuser",
        admin_password: Optional[str] = None,
        ssh_public_key: Optional[str] = None,
        subnet_id: Optional[str] = None,
        public_ip: bool = True,
        os_disk_size_gb: int = 30,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova VM Azure.

        Args:
            name: Nome da VM
            vm_size: Tamanho da VM (Standard_B1s, etc)
            image_reference: Referencia da imagem
            admin_username: Usuario admin
            admin_password: Senha (ou usar SSH)
            ssh_public_key: Chave SSH publica
            subnet_id: ID da subnet
            public_ip: Criar IP publico
            os_disk_size_gb: Tamanho do disco OS
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes da VM criada
        """
        try:
            # Usar imagem padrao se nao especificada
            if not image_reference:
                image_reference = DEFAULT_IMAGES["ubuntu-22.04"]

            # Preparar tags
            vm_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                vm_tags.update(tags)

            # 1. Criar IP publico (se solicitado)
            public_ip_address = None
            if public_ip:
                public_ip_name = f"{name}-ip"
                public_ip_result = await self._create_public_ip(public_ip_name, vm_tags)
                if public_ip_result.get("success"):
                    public_ip_address = public_ip_result

            # 2. Criar NIC
            nic_name = f"{name}-nic"
            nic_result = await self._create_nic(
                nic_name,
                subnet_id,
                public_ip_address.get("id") if public_ip_address else None,
                vm_tags
            )
            if not nic_result.get("success"):
                return {"success": False, "error": nic_result.get("error")}

            # 3. Configurar parametros da VM
            vm_parameters = {
                "location": self.location,
                "tags": vm_tags,
                "hardware_profile": {
                    "vm_size": vm_size
                },
                "storage_profile": {
                    "image_reference": image_reference,
                    "os_disk": {
                        "name": f"{name}-osdisk",
                        "caching": "ReadWrite",
                        "create_option": "FromImage",
                        "disk_size_gb": os_disk_size_gb,
                        "managed_disk": {
                            "storage_account_type": "Standard_LRS"
                        }
                    }
                },
                "os_profile": {
                    "computer_name": name,
                    "admin_username": admin_username,
                },
                "network_profile": {
                    "network_interfaces": [
                        {"id": nic_result["id"], "primary": True}
                    ]
                }
            }

            # Configurar autenticacao
            if ssh_public_key:
                vm_parameters["os_profile"]["linux_configuration"] = {
                    "disable_password_authentication": True,
                    "ssh": {
                        "public_keys": [
                            {
                                "path": f"/home/{admin_username}/.ssh/authorized_keys",
                                "key_data": ssh_public_key
                            }
                        ]
                    }
                }
            elif admin_password:
                vm_parameters["os_profile"]["admin_password"] = admin_password

            # 4. Criar VM
            poller = self.compute_client.virtual_machines.begin_create_or_update(
                self.resource_group,
                name,
                vm_parameters
            )
            vm = poller.result()

            logger.info(f"VM Azure criada: {name}")

            # Obter IP publico
            vm_public_ip = None
            if public_ip_address:
                ip_info = self.network_client.public_ip_addresses.get(
                    self.resource_group,
                    public_ip_address["name"]
                )
                vm_public_ip = ip_info.ip_address

            return {
                "success": True,
                "vm_id": vm.id,
                "name": name,
                "vm_size": vm_size,
                "location": self.location,
                "public_ip": vm_public_ip,
                "private_ip": nic_result.get("private_ip"),
                "status": "running",
                "admin_username": admin_username,
                "hourly_cost": VM_PRICING.get(vm_size, 0.05),
            }

        except Exception as e:
            logger.error(f"Erro ao criar VM Azure: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _create_public_ip(
        self,
        name: str,
        tags: Dict[str, str]
    ) -> Dict[str, Any]:
        """Cria IP publico"""
        try:
            poller = self.network_client.public_ip_addresses.begin_create_or_update(
                self.resource_group,
                name,
                {
                    "location": self.location,
                    "sku": {"name": "Basic"},
                    "public_ip_allocation_method": "Dynamic",
                    "tags": tags,
                }
            )
            ip = poller.result()
            return {
                "success": True,
                "id": ip.id,
                "name": name,
            }
        except Exception as e:
            logger.error(f"Erro ao criar IP publico: {e}")
            return {"success": False, "error": str(e)}

    async def _create_nic(
        self,
        name: str,
        subnet_id: Optional[str],
        public_ip_id: Optional[str],
        tags: Dict[str, str]
    ) -> Dict[str, Any]:
        """Cria interface de rede"""
        try:
            # Usar subnet padrao se nao especificada
            if not subnet_id:
                # Tentar obter subnet padrao
                vnets = list(self.network_client.virtual_networks.list(self.resource_group))
                if vnets:
                    subnets = list(vnets[0].subnets)
                    if subnets:
                        subnet_id = subnets[0].id

            if not subnet_id:
                return {
                    "success": False,
                    "error": "Nenhuma subnet disponivel. Crie uma VNet primeiro."
                }

            ip_config = {
                "name": f"{name}-ipconfig",
                "subnet": {"id": subnet_id},
            }

            if public_ip_id:
                ip_config["public_ip_address"] = {"id": public_ip_id}

            poller = self.network_client.network_interfaces.begin_create_or_update(
                self.resource_group,
                name,
                {
                    "location": self.location,
                    "ip_configurations": [ip_config],
                    "tags": tags,
                }
            )
            nic = poller.result()

            # Obter IP privado
            private_ip = None
            if nic.ip_configurations:
                private_ip = nic.ip_configurations[0].private_ip_address

            return {
                "success": True,
                "id": nic.id,
                "name": name,
                "private_ip": private_ip,
            }
        except Exception as e:
            logger.error(f"Erro ao criar NIC: {e}")
            return {"success": False, "error": str(e)}

    async def get_vm(self, name: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma VM.

        Args:
            name: Nome da VM

        Returns:
            Dicionario com informacoes da VM
        """
        try:
            vm = self.compute_client.virtual_machines.get(
                self.resource_group,
                name,
                expand="instanceView"
            )

            # Obter status
            status = "unknown"
            if vm.instance_view and vm.instance_view.statuses:
                for s in vm.instance_view.statuses:
                    if s.code.startswith("PowerState"):
                        status = s.code.split("/")[1]
                        break

            # Obter IPs
            public_ip = None
            private_ip = None
            if vm.network_profile and vm.network_profile.network_interfaces:
                nic_id = vm.network_profile.network_interfaces[0].id
                nic_name = nic_id.split("/")[-1]
                nic = self.network_client.network_interfaces.get(
                    self.resource_group, nic_name
                )
                if nic.ip_configurations:
                    private_ip = nic.ip_configurations[0].private_ip_address
                    if nic.ip_configurations[0].public_ip_address:
                        pip_id = nic.ip_configurations[0].public_ip_address.id
                        pip_name = pip_id.split("/")[-1]
                        pip = self.network_client.public_ip_addresses.get(
                            self.resource_group, pip_name
                        )
                        public_ip = pip.ip_address

            return {
                "vm_id": vm.id,
                "name": vm.name,
                "vm_size": vm.hardware_profile.vm_size,
                "location": vm.location,
                "status": status,
                "public_ip": public_ip,
                "private_ip": private_ip,
                "os_type": vm.storage_profile.os_disk.os_type if vm.storage_profile else None,
                "tags": dict(vm.tags) if vm.tags else {},
            }

        except Exception as e:
            logger.error(f"Erro ao obter VM: {e}")
            return {"error": str(e)}

    async def start_vm(self, name: str) -> bool:
        """
        Inicia uma VM parada.

        Args:
            name: Nome da VM

        Returns:
            True se iniciou com sucesso
        """
        try:
            poller = self.compute_client.virtual_machines.begin_start(
                self.resource_group, name
            )
            poller.result()
            logger.info(f"VM {name} iniciada")
            return True
        except Exception as e:
            logger.error(f"Erro ao iniciar VM: {e}")
            return False

    async def stop_vm(self, name: str, deallocate: bool = True) -> bool:
        """
        Para uma VM.

        Args:
            name: Nome da VM
            deallocate: Se True, desaloca recursos (para de cobrar)

        Returns:
            True se parou com sucesso
        """
        try:
            if deallocate:
                poller = self.compute_client.virtual_machines.begin_deallocate(
                    self.resource_group, name
                )
            else:
                poller = self.compute_client.virtual_machines.begin_power_off(
                    self.resource_group, name
                )
            poller.result()
            logger.info(f"VM {name} parada")
            return True
        except Exception as e:
            logger.error(f"Erro ao parar VM: {e}")
            return False

    async def delete_vm(self, name: str, delete_resources: bool = True) -> bool:
        """
        Deleta uma VM.

        Args:
            name: Nome da VM
            delete_resources: Deletar recursos associados

        Returns:
            True se deletou com sucesso
        """
        try:
            # Obter VM para pegar recursos associados
            vm_info = await self.get_vm(name)

            # Deletar VM
            poller = self.compute_client.virtual_machines.begin_delete(
                self.resource_group, name
            )
            poller.result()
            logger.info(f"VM {name} deletada")

            if delete_resources:
                # Deletar disco OS
                try:
                    self.compute_client.disks.begin_delete(
                        self.resource_group, f"{name}-osdisk"
                    ).result()
                except Exception:
                    pass

                # Deletar NIC
                try:
                    self.network_client.network_interfaces.begin_delete(
                        self.resource_group, f"{name}-nic"
                    ).result()
                except Exception:
                    pass

                # Deletar IP publico
                try:
                    self.network_client.public_ip_addresses.begin_delete(
                        self.resource_group, f"{name}-ip"
                    ).result()
                except Exception:
                    pass

            return True
        except Exception as e:
            logger.error(f"Erro ao deletar VM: {e}")
            return False

    async def list_vms(self, tags: Optional[Dict[str, str]] = None) -> List[Dict[str, Any]]:
        """
        Lista VMs do resource group.

        Args:
            tags: Filtrar por tags

        Returns:
            Lista de VMs
        """
        try:
            vms_list = self.compute_client.virtual_machines.list(self.resource_group)

            vms = []
            for vm in vms_list:
                # Filtrar por tags se especificado
                if tags:
                    if not vm.tags:
                        continue
                    match = all(vm.tags.get(k) == v for k, v in tags.items())
                    if not match:
                        continue

                vms.append({
                    "vm_id": vm.id,
                    "name": vm.name,
                    "vm_size": vm.hardware_profile.vm_size,
                    "location": vm.location,
                    "tags": dict(vm.tags) if vm.tags else {},
                })

            return vms
        except Exception as e:
            logger.error(f"Erro ao listar VMs: {e}")
            return []

    async def create_nsg(
        self,
        name: str,
        rules: Optional[List[Dict]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um Network Security Group.

        Args:
            name: Nome do NSG
            rules: Regras de seguranca
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do NSG
        """
        try:
            nsg_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                nsg_tags.update(tags)

            # Regras padrao
            security_rules = []
            if rules:
                security_rules = rules
            else:
                security_rules = [
                    {
                        "name": "Allow-SSH",
                        "priority": 1000,
                        "direction": "Inbound",
                        "access": "Allow",
                        "protocol": "Tcp",
                        "source_port_range": "*",
                        "destination_port_range": "22",
                        "source_address_prefix": "*",
                        "destination_address_prefix": "*",
                    },
                    {
                        "name": "Allow-HTTP",
                        "priority": 1001,
                        "direction": "Inbound",
                        "access": "Allow",
                        "protocol": "Tcp",
                        "source_port_range": "*",
                        "destination_port_range": "80",
                        "source_address_prefix": "*",
                        "destination_address_prefix": "*",
                    },
                    {
                        "name": "Allow-HTTPS",
                        "priority": 1002,
                        "direction": "Inbound",
                        "access": "Allow",
                        "protocol": "Tcp",
                        "source_port_range": "*",
                        "destination_port_range": "443",
                        "source_address_prefix": "*",
                        "destination_address_prefix": "*",
                    },
                ]

            poller = self.network_client.network_security_groups.begin_create_or_update(
                self.resource_group,
                name,
                {
                    "location": self.location,
                    "security_rules": security_rules,
                    "tags": nsg_tags,
                }
            )
            nsg = poller.result()

            logger.info(f"NSG criado: {name}")

            return {
                "success": True,
                "nsg_id": nsg.id,
                "name": name,
            }
        except Exception as e:
            logger.error(f"Erro ao criar NSG: {e}")
            return {"success": False, "error": str(e)}

    async def delete_nsg(self, name: str) -> bool:
        """
        Deleta um Network Security Group.

        Args:
            name: Nome do NSG

        Returns:
            True se deletou com sucesso
        """
        try:
            poller = self.network_client.network_security_groups.begin_delete(
                self.resource_group, name
            )
            poller.result()
            logger.info(f"NSG {name} deletado")
            return True
        except Exception as e:
            logger.error(f"Erro ao deletar NSG: {e}")
            return False

    def get_vm_pricing(self, vm_size: str) -> float:
        """
        Retorna preco por hora de um tamanho de VM.

        Args:
            vm_size: Tamanho da VM

        Returns:
            Preco por hora em USD
        """
        return VM_PRICING.get(vm_size, 0.05)

    def get_available_vm_sizes(self) -> List[Dict[str, Any]]:
        """
        Retorna lista de tamanhos de VM disponiveis.

        Returns:
            Lista de tamanhos com precos
        """
        return [
            {
                "size": size,
                "hourly_cost": price,
                "monthly_cost": round(price * 24 * 30, 2)
            }
            for size, price in VM_PRICING.items()
        ]
