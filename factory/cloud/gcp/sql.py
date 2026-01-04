# -*- coding: utf-8 -*-
"""
GCP Cloud SQL Manager
=====================
Gerenciador de Cloud SQL para a Plataforma E.

Funcionalidades:
- Criacao de instancias Cloud SQL
- Gerenciamento de databases
- Backup e restore
- Configuracao de usuarios
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import secrets
import string

logger = logging.getLogger(__name__)

# Precos Cloud SQL (us-central1) - por hora
SQL_PRICING = {
    # PostgreSQL
    "db-f1-micro": 0.0105,
    "db-g1-small": 0.025,
    "db-n1-standard-1": 0.05,
    "db-n1-standard-2": 0.10,
    "db-n1-standard-4": 0.20,
    "db-n1-standard-8": 0.40,
    # Storage (por GB/mes)
    "ssd_storage": 0.17,
    "hdd_storage": 0.09,
    # Backup (por GB/mes)
    "backup_storage": 0.08,
}

# Engines suportados
SUPPORTED_ENGINES = {
    "postgresql": {
        "name": "PostgreSQL",
        "default_version": "POSTGRES_15",
        "versions": ["POSTGRES_13", "POSTGRES_14", "POSTGRES_15", "POSTGRES_16"],
        "default_port": 5432,
    },
    "mysql": {
        "name": "MySQL",
        "default_version": "MYSQL_8_0",
        "versions": ["MYSQL_5_7", "MYSQL_8_0"],
        "default_port": 3306,
    },
}


class CloudSQLManager:
    """
    Gerenciador de Cloud SQL.

    Responsavel por todas as operacoes relacionadas ao Cloud SQL:
    - Criacao de instancias
    - Gerenciamento de databases e usuarios
    - Backup e restore
    """

    def __init__(self, sql_admin_client, config):
        """
        Inicializa o gerenciador de Cloud SQL.

        Args:
            sql_admin_client: Cliente Google Cloud SQL Admin
            config: Configuracao GCP
        """
        self.client = sql_admin_client
        self.config = config
        self.project_id = config.project_id
        self.region = config.region

    async def create_instance(
        self,
        name: str,
        database_version: str = "POSTGRES_15",
        tier: str = "db-f1-micro",
        storage_gb: int = 10,
        storage_type: str = "SSD",
        root_password: Optional[str] = None,
        region: Optional[str] = None,
        authorized_networks: Optional[List[Dict]] = None,
        public_ip: bool = True,
        backup_enabled: bool = True,
        backup_start_time: str = "02:00",
        maintenance_window_day: int = 7,
        maintenance_window_hour: int = 3,
        labels: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova instancia Cloud SQL.

        Args:
            name: Nome da instancia
            database_version: Versao do banco (POSTGRES_15, MYSQL_8_0)
            tier: Tier da maquina
            storage_gb: Tamanho do storage em GB
            storage_type: Tipo de storage (SSD ou HDD)
            root_password: Senha do root (gerada se None)
            region: Regiao
            authorized_networks: Redes autorizadas
            public_ip: Habilitar IP publico
            backup_enabled: Habilitar backups automaticos
            backup_start_time: Hora de inicio do backup
            maintenance_window_day: Dia da janela de manutencao (1-7)
            maintenance_window_hour: Hora da janela de manutencao (0-23)
            labels: Labels GCP

        Returns:
            Dicionario com informacoes da instancia criada
        """
        try:
            instance_labels = {"project": "plataforma-e"}
            if labels:
                instance_labels.update(labels)

            if not root_password:
                root_password = self._generate_password()

            # Configuracoes de IP
            ip_configuration = {
                "ipv4Enabled": public_ip,
            }

            if authorized_networks:
                ip_configuration["authorizedNetworks"] = authorized_networks
            elif public_ip:
                # Permitir acesso de qualquer IP (desenvolvimento)
                ip_configuration["authorizedNetworks"] = [
                    {"value": "0.0.0.0/0", "name": "all"}
                ]

            # Configuracao de backup
            backup_configuration = {
                "enabled": backup_enabled,
                "startTime": backup_start_time,
                "pointInTimeRecoveryEnabled": backup_enabled,
            }

            # Configuracao de manutencao
            maintenance_window = {
                "day": maintenance_window_day,
                "hour": maintenance_window_hour,
            }

            # Corpo da requisicao
            instance_body = {
                "name": name,
                "region": region or self.region,
                "databaseVersion": database_version,
                "settings": {
                    "tier": tier,
                    "dataDiskSizeGb": storage_gb,
                    "dataDiskType": f"PD_{storage_type.upper()}",
                    "ipConfiguration": ip_configuration,
                    "backupConfiguration": backup_configuration,
                    "maintenanceWindow": maintenance_window,
                    "userLabels": instance_labels,
                    "availabilityType": "ZONAL",
                },
                "rootPassword": root_password,
            }

            # Criar instancia
            operation = self.client.instances().insert(
                project=self.project_id,
                body=instance_body
            ).execute()

            # Aguardar operacao
            await self._wait_for_operation(operation["name"])

            # Obter informacoes da instancia
            instance_info = await self.get_instance(name)

            # Determinar porta baseado no engine
            port = 5432 if "POSTGRES" in database_version else 3306

            logger.info(f"Instancia Cloud SQL criada: {name}")

            return {
                "success": True,
                "instance_name": name,
                "database_version": database_version,
                "tier": tier,
                "region": region or self.region,
                "public_ip": instance_info.get("public_ip"),
                "private_ip": instance_info.get("private_ip"),
                "port": port,
                "root_password": root_password,
                "status": instance_info.get("status"),
                "connection_name": instance_info.get("connection_name"),
            }

        except Exception as e:
            logger.error(f"Erro ao criar instancia Cloud SQL: {e}")
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
                instance=name
            ).execute()

            # Extrair IPs
            public_ip = None
            private_ip = None
            for ip_addr in instance.get("ipAddresses", []):
                if ip_addr["type"] == "PRIMARY":
                    public_ip = ip_addr["ipAddress"]
                elif ip_addr["type"] == "PRIVATE":
                    private_ip = ip_addr["ipAddress"]

            return {
                "instance_name": instance["name"],
                "database_version": instance["databaseVersion"],
                "tier": instance["settings"]["tier"],
                "region": instance["region"],
                "status": instance["state"],
                "public_ip": public_ip,
                "private_ip": private_ip,
                "connection_name": instance.get("connectionName"),
                "storage_gb": instance["settings"].get("dataDiskSizeGb"),
                "labels": instance["settings"].get("userLabels", {}),
            }

        except Exception as e:
            logger.error(f"Erro ao obter instancia: {e}")
            return {"error": str(e)}

    async def delete_instance(self, name: str) -> bool:
        """
        Deleta uma instancia Cloud SQL.

        Args:
            name: Nome da instancia

        Returns:
            True se deletou com sucesso
        """
        try:
            operation = self.client.instances().delete(
                project=self.project_id,
                instance=name
            ).execute()

            await self._wait_for_operation(operation["name"])

            logger.info(f"Instancia {name} deletada")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar instancia: {e}")
            return False

    async def start_instance(self, name: str) -> bool:
        """
        Inicia uma instancia parada.

        Args:
            name: Nome da instancia

        Returns:
            True se iniciou com sucesso
        """
        try:
            operation = self.client.instances().patch(
                project=self.project_id,
                instance=name,
                body={"settings": {"activationPolicy": "ALWAYS"}}
            ).execute()

            await self._wait_for_operation(operation["name"])

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
            operation = self.client.instances().patch(
                project=self.project_id,
                instance=name,
                body={"settings": {"activationPolicy": "NEVER"}}
            ).execute()

            await self._wait_for_operation(operation["name"])

            logger.info(f"Instancia {name} parada")
            return True

        except Exception as e:
            logger.error(f"Erro ao parar instancia: {e}")
            return False

    async def list_instances(self) -> List[Dict[str, Any]]:
        """
        Lista instancias Cloud SQL do projeto.

        Returns:
            Lista de instancias
        """
        try:
            result = self.client.instances().list(
                project=self.project_id
            ).execute()

            instances = []
            for instance in result.get("items", []):
                # Extrair IP publico
                public_ip = None
                for ip_addr in instance.get("ipAddresses", []):
                    if ip_addr["type"] == "PRIMARY":
                        public_ip = ip_addr["ipAddress"]
                        break

                instances.append({
                    "instance_name": instance["name"],
                    "database_version": instance["databaseVersion"],
                    "tier": instance["settings"]["tier"],
                    "region": instance["region"],
                    "status": instance["state"],
                    "public_ip": public_ip,
                })

            return instances

        except Exception as e:
            logger.error(f"Erro ao listar instancias: {e}")
            return []

    async def create_database(
        self,
        instance_name: str,
        database_name: str,
        charset: str = "UTF8",
        collation: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria um database em uma instancia.

        Args:
            instance_name: Nome da instancia
            database_name: Nome do database
            charset: Charset
            collation: Collation

        Returns:
            Dicionario com informacoes do database
        """
        try:
            database_body = {
                "name": database_name,
                "charset": charset,
            }

            if collation:
                database_body["collation"] = collation

            operation = self.client.databases().insert(
                project=self.project_id,
                instance=instance_name,
                body=database_body
            ).execute()

            await self._wait_for_operation(operation["name"])

            logger.info(f"Database {database_name} criado em {instance_name}")

            return {
                "success": True,
                "database_name": database_name,
                "instance_name": instance_name,
                "charset": charset,
            }

        except Exception as e:
            logger.error(f"Erro ao criar database: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def create_user(
        self,
        instance_name: str,
        username: str,
        password: Optional[str] = None,
        host: str = "%"
    ) -> Dict[str, Any]:
        """
        Cria um usuario em uma instancia.

        Args:
            instance_name: Nome da instancia
            username: Nome do usuario
            password: Senha (gerada se None)
            host: Host permitido

        Returns:
            Dicionario com informacoes do usuario
        """
        try:
            if not password:
                password = self._generate_password()

            user_body = {
                "name": username,
                "password": password,
                "host": host,
            }

            operation = self.client.users().insert(
                project=self.project_id,
                instance=instance_name,
                body=user_body
            ).execute()

            await self._wait_for_operation(operation["name"])

            logger.info(f"Usuario {username} criado em {instance_name}")

            return {
                "success": True,
                "username": username,
                "password": password,
                "instance_name": instance_name,
            }

        except Exception as e:
            logger.error(f"Erro ao criar usuario: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def create_backup(
        self,
        instance_name: str,
        description: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria um backup on-demand.

        Args:
            instance_name: Nome da instancia
            description: Descricao do backup

        Returns:
            Dicionario com informacoes do backup
        """
        try:
            backup_body = {}
            if description:
                backup_body["description"] = description

            operation = self.client.backupRuns().insert(
                project=self.project_id,
                instance=instance_name,
                body=backup_body
            ).execute()

            await self._wait_for_operation(operation["name"])

            logger.info(f"Backup criado para {instance_name}")

            return {
                "success": True,
                "instance_name": instance_name,
                "backup_id": operation.get("targetId"),
            }

        except Exception as e:
            logger.error(f"Erro ao criar backup: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _wait_for_operation(self, operation_name: str, timeout: int = 600):
        """
        Aguarda uma operacao completar.

        Args:
            operation_name: Nome da operacao
            timeout: Timeout em segundos
        """
        import time

        start_time = time.time()

        while True:
            if time.time() - start_time > timeout:
                raise TimeoutError(f"Operacao {operation_name} excedeu timeout")

            result = self.client.operations().get(
                project=self.project_id,
                operation=operation_name
            ).execute()

            if result["status"] == "DONE":
                if "error" in result:
                    raise Exception(result["error"])
                return result

            time.sleep(5)

    def get_connection_string(
        self,
        instance_name: str,
        database_name: str,
        username: str,
        password: str,
        public_ip: str,
        database_version: str
    ) -> str:
        """
        Gera string de conexao.

        Args:
            instance_name: Nome da instancia
            database_name: Nome do database
            username: Usuario
            password: Senha
            public_ip: IP publico
            database_version: Versao do banco

        Returns:
            String de conexao
        """
        if "POSTGRES" in database_version:
            port = 5432
            return f"postgresql://{username}:{password}@{public_ip}:{port}/{database_name}"
        else:  # MySQL
            port = 3306
            return f"mysql://{username}:{password}@{public_ip}:{port}/{database_name}"

    def _generate_password(self, length: int = 24) -> str:
        """Gera senha segura"""
        chars = string.ascii_letters + string.digits + "!#$%&*+-=?"
        return ''.join(secrets.choice(chars) for _ in range(length))

    def get_tier_pricing(self, tier: str) -> float:
        """
        Retorna preco por hora de um tier.

        Args:
            tier: Tier da maquina

        Returns:
            Preco por hora em USD
        """
        return SQL_PRICING.get(tier, 0.02)

    def estimate_monthly_cost(
        self,
        tier: str,
        storage_gb: int,
        storage_type: str = "ssd"
    ) -> Dict[str, float]:
        """
        Estima custo mensal de uma instancia.

        Args:
            tier: Tier da maquina
            storage_gb: Storage em GB
            storage_type: Tipo de storage

        Returns:
            Dicionario com estimativas de custo
        """
        hourly_price = self.get_tier_pricing(tier)
        compute_cost = hourly_price * 24 * 30

        storage_price = SQL_PRICING.get(f"{storage_type}_storage", 0.17)
        storage_cost = storage_gb * storage_price

        total_cost = compute_cost + storage_cost

        return {
            "compute_cost": round(compute_cost, 2),
            "storage_cost": round(storage_cost, 2),
            "total_monthly_cost": round(total_cost, 2),
        }
