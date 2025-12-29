# -*- coding: utf-8 -*-
"""
AWS EC2 Manager
===============
Gerenciador de instancias EC2 para a Fabrica de Agentes.

Funcionalidades:
- Criacao de instancias EC2
- Start/Stop/Terminate
- Monitoramento de status
- Gerenciamento de Security Groups
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)

# Precos por hora (us-east-1) - valores aproximados para estimativa
EC2_PRICING = {
    # General Purpose
    "t2.micro": 0.0116,
    "t2.small": 0.023,
    "t2.medium": 0.0464,
    "t2.large": 0.0928,
    "t3.micro": 0.0104,
    "t3.small": 0.0208,
    "t3.medium": 0.0416,
    "t3.large": 0.0832,
    # Compute Optimized
    "c5.large": 0.085,
    "c5.xlarge": 0.17,
    # Memory Optimized
    "r5.large": 0.126,
    "r5.xlarge": 0.252,
}

# AMIs populares (us-east-1)
DEFAULT_AMIS = {
    "amazon-linux-2": "ami-0c55b159cbfafe1f0",
    "ubuntu-22.04": "ami-0c7217cdde317cfec",
    "ubuntu-20.04": "ami-0261755bbcb8c4a84",
    "debian-12": "ami-06db4d78cb1d3bbf9",
    "windows-2022": "ami-0c2b8ca1dad447f8a",
}


class EC2Manager:
    """
    Gerenciador de instancias EC2.

    Responsavel por todas as operacoes relacionadas a EC2:
    - Criacao e configuracao de instancias
    - Gerenciamento de ciclo de vida
    - Monitoramento e metricas
    """

    def __init__(self, ec2_client, ec2_resource, config):
        """
        Inicializa o gerenciador EC2.

        Args:
            ec2_client: Cliente boto3 EC2
            ec2_resource: Resource boto3 EC2
            config: Configuracao AWS
        """
        self.client = ec2_client
        self.resource = ec2_resource
        self.config = config
        self.region = config.region

    async def create_instance(
        self,
        name: str,
        instance_type: str = "t3.micro",
        image_id: Optional[str] = None,
        key_name: Optional[str] = None,
        security_group_ids: Optional[List[str]] = None,
        subnet_id: Optional[str] = None,
        user_data: Optional[str] = None,
        tags: Optional[Dict[str, str]] = None,
        block_devices: Optional[List[Dict]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova instancia EC2.

        Args:
            name: Nome da instancia
            instance_type: Tipo da instancia (ex: t3.micro)
            image_id: ID da AMI (usa Ubuntu 22.04 se None)
            key_name: Nome do key pair para SSH
            security_group_ids: IDs dos security groups
            subnet_id: ID da subnet
            user_data: Script de inicializacao
            tags: Tags adicionais
            block_devices: Configuracao de discos

        Returns:
            Dicionario com informacoes da instancia criada
        """
        try:
            # Usar AMI padrao se nao especificada
            if not image_id:
                image_id = DEFAULT_AMIS.get("ubuntu-22.04")

            # Preparar tags
            instance_tags = {"Name": name, "Project": "FabricaDeAgentes"}
            if tags:
                instance_tags.update(tags)

            tag_specifications = [
                {
                    "ResourceType": "instance",
                    "Tags": [
                        {"Key": k, "Value": v}
                        for k, v in instance_tags.items()
                    ]
                }
            ]

            # Parametros da instancia
            params = {
                "ImageId": image_id,
                "InstanceType": instance_type,
                "MinCount": 1,
                "MaxCount": 1,
                "TagSpecifications": tag_specifications,
            }

            # Adicionar parametros opcionais
            if key_name:
                params["KeyName"] = key_name

            if security_group_ids:
                params["SecurityGroupIds"] = security_group_ids

            if subnet_id:
                params["SubnetId"] = subnet_id

            if user_data:
                params["UserData"] = user_data

            if block_devices:
                params["BlockDeviceMappings"] = block_devices
            else:
                # Disco padrao de 20GB
                params["BlockDeviceMappings"] = [
                    {
                        "DeviceName": "/dev/sda1",
                        "Ebs": {
                            "VolumeSize": 20,
                            "VolumeType": "gp3",
                            "DeleteOnTermination": True
                        }
                    }
                ]

            # Criar instancia
            response = self.client.run_instances(**params)
            instance_data = response["Instances"][0]
            instance_id = instance_data["InstanceId"]

            logger.info(f"Instancia EC2 criada: {instance_id}")

            # Aguardar instancia ficar running
            waiter = self.client.get_waiter("instance_running")
            waiter.wait(InstanceIds=[instance_id])

            # Obter informacoes atualizadas
            instance_info = await self.get_instance(instance_id)

            return {
                "success": True,
                "instance_id": instance_id,
                "name": name,
                "instance_type": instance_type,
                "public_ip": instance_info.get("public_ip"),
                "private_ip": instance_info.get("private_ip"),
                "public_dns": instance_info.get("public_dns"),
                "status": "running",
                "hourly_cost": EC2_PRICING.get(instance_type, 0.05),
            }

        except Exception as e:
            logger.error(f"Erro ao criar instancia EC2: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def get_instance(self, instance_id: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma instancia.

        Args:
            instance_id: ID da instancia EC2

        Returns:
            Dicionario com informacoes da instancia
        """
        try:
            response = self.client.describe_instances(
                InstanceIds=[instance_id]
            )

            if not response["Reservations"]:
                return {"error": "Instancia nao encontrada"}

            instance = response["Reservations"][0]["Instances"][0]

            # Extrair nome das tags
            name = ""
            for tag in instance.get("Tags", []):
                if tag["Key"] == "Name":
                    name = tag["Value"]
                    break

            return {
                "instance_id": instance["InstanceId"],
                "name": name,
                "instance_type": instance["InstanceType"],
                "status": instance["State"]["Name"],
                "public_ip": instance.get("PublicIpAddress"),
                "private_ip": instance.get("PrivateIpAddress"),
                "public_dns": instance.get("PublicDnsName"),
                "private_dns": instance.get("PrivateDnsName"),
                "launch_time": instance.get("LaunchTime"),
                "vpc_id": instance.get("VpcId"),
                "subnet_id": instance.get("SubnetId"),
                "security_groups": [
                    sg["GroupId"]
                    for sg in instance.get("SecurityGroups", [])
                ],
            }

        except Exception as e:
            logger.error(f"Erro ao obter instancia: {e}")
            return {"error": str(e)}

    async def start_instance(self, instance_id: str) -> bool:
        """
        Inicia uma instancia parada.

        Args:
            instance_id: ID da instancia

        Returns:
            True se iniciou com sucesso
        """
        try:
            self.client.start_instances(InstanceIds=[instance_id])

            # Aguardar instancia ficar running
            waiter = self.client.get_waiter("instance_running")
            waiter.wait(InstanceIds=[instance_id])

            logger.info(f"Instancia {instance_id} iniciada")
            return True

        except Exception as e:
            logger.error(f"Erro ao iniciar instancia: {e}")
            return False

    async def stop_instance(self, instance_id: str) -> bool:
        """
        Para uma instancia em execucao.

        Args:
            instance_id: ID da instancia

        Returns:
            True se parou com sucesso
        """
        try:
            self.client.stop_instances(InstanceIds=[instance_id])

            # Aguardar instancia parar
            waiter = self.client.get_waiter("instance_stopped")
            waiter.wait(InstanceIds=[instance_id])

            logger.info(f"Instancia {instance_id} parada")
            return True

        except Exception as e:
            logger.error(f"Erro ao parar instancia: {e}")
            return False

    async def terminate_instance(self, instance_id: str) -> bool:
        """
        Termina (deleta) uma instancia.

        Args:
            instance_id: ID da instancia

        Returns:
            True se terminou com sucesso
        """
        try:
            self.client.terminate_instances(InstanceIds=[instance_id])

            # Aguardar instancia ser terminada
            waiter = self.client.get_waiter("instance_terminated")
            waiter.wait(InstanceIds=[instance_id])

            logger.info(f"Instancia {instance_id} terminada")
            return True

        except Exception as e:
            logger.error(f"Erro ao terminar instancia: {e}")
            return False

    async def list_instances(
        self,
        filters: Optional[List[Dict]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista instancias EC2.

        Args:
            filters: Filtros AWS (ex: [{"Name": "instance-state-name", "Values": ["running"]}])
            tags: Filtrar por tags

        Returns:
            Lista de instancias
        """
        try:
            all_filters = filters or []

            # Adicionar filtros de tags
            if tags:
                for key, value in tags.items():
                    all_filters.append({
                        "Name": f"tag:{key}",
                        "Values": [value]
                    })

            params = {}
            if all_filters:
                params["Filters"] = all_filters

            response = self.client.describe_instances(**params)

            instances = []
            for reservation in response["Reservations"]:
                for instance in reservation["Instances"]:
                    # Extrair nome das tags
                    name = ""
                    tags_dict = {}
                    for tag in instance.get("Tags", []):
                        tags_dict[tag["Key"]] = tag["Value"]
                        if tag["Key"] == "Name":
                            name = tag["Value"]

                    instances.append({
                        "instance_id": instance["InstanceId"],
                        "name": name,
                        "instance_type": instance["InstanceType"],
                        "status": instance["State"]["Name"],
                        "public_ip": instance.get("PublicIpAddress"),
                        "private_ip": instance.get("PrivateIpAddress"),
                        "launch_time": instance.get("LaunchTime"),
                        "tags": tags_dict,
                    })

            return instances

        except Exception as e:
            logger.error(f"Erro ao listar instancias: {e}")
            return []

    async def create_security_group(
        self,
        name: str,
        description: str,
        vpc_id: Optional[str] = None,
        ingress_rules: Optional[List[Dict]] = None,
        egress_rules: Optional[List[Dict]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um security group.

        Args:
            name: Nome do security group
            description: Descricao
            vpc_id: VPC onde criar
            ingress_rules: Regras de entrada
            egress_rules: Regras de saida
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do security group
        """
        try:
            params = {
                "GroupName": name,
                "Description": description,
            }

            if vpc_id:
                params["VpcId"] = vpc_id

            # Preparar tags
            sg_tags = {"Name": name, "Project": "FabricaDeAgentes"}
            if tags:
                sg_tags.update(tags)

            params["TagSpecifications"] = [
                {
                    "ResourceType": "security-group",
                    "Tags": [
                        {"Key": k, "Value": v}
                        for k, v in sg_tags.items()
                    ]
                }
            ]

            response = self.client.create_security_group(**params)
            group_id = response["GroupId"]

            # Adicionar regras de ingress
            if ingress_rules:
                self.client.authorize_security_group_ingress(
                    GroupId=group_id,
                    IpPermissions=ingress_rules
                )
            else:
                # Regras padrao: SSH (22), HTTP (80), HTTPS (443)
                default_rules = [
                    {
                        "IpProtocol": "tcp",
                        "FromPort": 22,
                        "ToPort": 22,
                        "IpRanges": [{"CidrIp": "0.0.0.0/0", "Description": "SSH"}]
                    },
                    {
                        "IpProtocol": "tcp",
                        "FromPort": 80,
                        "ToPort": 80,
                        "IpRanges": [{"CidrIp": "0.0.0.0/0", "Description": "HTTP"}]
                    },
                    {
                        "IpProtocol": "tcp",
                        "FromPort": 443,
                        "ToPort": 443,
                        "IpRanges": [{"CidrIp": "0.0.0.0/0", "Description": "HTTPS"}]
                    },
                ]
                self.client.authorize_security_group_ingress(
                    GroupId=group_id,
                    IpPermissions=default_rules
                )

            logger.info(f"Security group criado: {group_id}")

            return {
                "success": True,
                "group_id": group_id,
                "name": name,
            }

        except Exception as e:
            logger.error(f"Erro ao criar security group: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_security_group(self, group_id: str) -> bool:
        """
        Deleta um security group.

        Args:
            group_id: ID do security group

        Returns:
            True se deletou com sucesso
        """
        try:
            self.client.delete_security_group(GroupId=group_id)
            logger.info(f"Security group {group_id} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar security group: {e}")
            return False

    def get_instance_type_pricing(self, instance_type: str) -> float:
        """
        Retorna preco por hora de um tipo de instancia.

        Args:
            instance_type: Tipo da instancia

        Returns:
            Preco por hora em USD
        """
        return EC2_PRICING.get(instance_type, 0.05)

    def get_available_instance_types(self) -> List[Dict[str, Any]]:
        """
        Retorna lista de tipos de instancia disponiveis.

        Returns:
            Lista de tipos com precos
        """
        return [
            {
                "type": itype,
                "hourly_cost": price,
                "monthly_cost": round(price * 24 * 30, 2)
            }
            for itype, price in EC2_PRICING.items()
        ]
