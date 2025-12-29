# -*- coding: utf-8 -*-
"""
Azure Database Manager
======================
Gerenciador de bancos de dados Azure para a Fabrica de Agentes.

Suporta:
- Azure Database for PostgreSQL
- Azure Database for MySQL
- Azure SQL Database
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import secrets
import string

logger = logging.getLogger(__name__)

# Precos Azure Database (East US) - por hora
DATABASE_PRICING = {
    # PostgreSQL Flexible Server
    "Standard_B1ms": 0.032,
    "Standard_B2s": 0.065,
    "Standard_D2s_v3": 0.192,
    "Standard_D4s_v3": 0.384,
    # Storage (por GB/mes)
    "storage_per_gb": 0.115,
    "backup_per_gb": 0.095,
}

# Engines suportados
SUPPORTED_ENGINES = {
    "postgresql": {
        "name": "Azure Database for PostgreSQL",
        "default_version": "15",
        "versions": ["13", "14", "15", "16"],
        "default_port": 5432,
    },
    "mysql": {
        "name": "Azure Database for MySQL",
        "default_version": "8.0",
        "versions": ["5.7", "8.0"],
        "default_port": 3306,
    },
}


class DatabaseManager:
    """
    Gerenciador de bancos de dados Azure.

    Responsavel por todas as operacoes relacionadas a bancos:
    - Criacao de servidores
    - Gerenciamento de bancos de dados
    - Backup e restore
    """

    def __init__(self, postgresql_client, mysql_client, config):
        """
        Inicializa o gerenciador de databases.

        Args:
            postgresql_client: Cliente Azure PostgreSQL
            mysql_client: Cliente Azure MySQL
            config: Configuracao Azure
        """
        self.postgresql_client = postgresql_client
        self.mysql_client = mysql_client
        self.config = config
        self.resource_group = config.resource_group
        self.location = config.location

    async def create_postgresql_server(
        self,
        name: str,
        sku_name: str = "Standard_B1ms",
        storage_gb: int = 32,
        version: str = "15",
        admin_username: str = "pgadmin",
        admin_password: Optional[str] = None,
        backup_retention_days: int = 7,
        geo_redundant_backup: bool = False,
        public_access: bool = True,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um servidor PostgreSQL Flexible.

        Args:
            name: Nome do servidor
            sku_name: SKU (Standard_B1ms, etc)
            storage_gb: Tamanho do storage em GB
            version: Versao do PostgreSQL
            admin_username: Usuario admin
            admin_password: Senha (gerada se None)
            backup_retention_days: Dias de retencao de backup
            geo_redundant_backup: Backup geo-redundante
            public_access: Permitir acesso publico
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do servidor
        """
        try:
            db_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                db_tags.update(tags)

            # Gerar senha se nao fornecida
            if not admin_password:
                admin_password = self._generate_password()

            # Parametros do servidor
            server_params = {
                "location": self.location,
                "tags": db_tags,
                "sku": {
                    "name": sku_name,
                    "tier": "Burstable" if "B" in sku_name else "GeneralPurpose",
                },
                "administrator_login": admin_username,
                "administrator_login_password": admin_password,
                "version": version,
                "storage": {
                    "storage_size_gb": storage_gb,
                },
                "backup": {
                    "backup_retention_days": backup_retention_days,
                    "geo_redundant_backup": "Enabled" if geo_redundant_backup else "Disabled",
                },
                "network": {
                    "public_network_access": "Enabled" if public_access else "Disabled",
                },
                "high_availability": {
                    "mode": "Disabled",
                },
            }

            # Criar servidor
            poller = self.postgresql_client.servers.begin_create(
                self.resource_group,
                name,
                server_params
            )
            server = poller.result()

            logger.info(f"Servidor PostgreSQL criado: {name}")

            # Obter FQDN
            fqdn = f"{name}.postgres.database.azure.com"

            return {
                "success": True,
                "server_id": server.id,
                "server_name": name,
                "fqdn": fqdn,
                "port": 5432,
                "version": version,
                "sku": sku_name,
                "storage_gb": storage_gb,
                "admin_username": admin_username,
                "admin_password": admin_password,
                "status": server.state,
                "connection_string": self.get_connection_string(
                    "postgresql", fqdn, 5432, "postgres", admin_username, admin_password
                ),
            }

        except Exception as e:
            logger.error(f"Erro ao criar servidor PostgreSQL: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def create_mysql_server(
        self,
        name: str,
        sku_name: str = "Standard_B1ms",
        storage_gb: int = 32,
        version: str = "8.0",
        admin_username: str = "mysqladmin",
        admin_password: Optional[str] = None,
        backup_retention_days: int = 7,
        public_access: bool = True,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um servidor MySQL Flexible.

        Args:
            name: Nome do servidor
            sku_name: SKU (Standard_B1ms, etc)
            storage_gb: Tamanho do storage em GB
            version: Versao do MySQL
            admin_username: Usuario admin
            admin_password: Senha (gerada se None)
            backup_retention_days: Dias de retencao de backup
            public_access: Permitir acesso publico
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do servidor
        """
        try:
            db_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                db_tags.update(tags)

            if not admin_password:
                admin_password = self._generate_password()

            server_params = {
                "location": self.location,
                "tags": db_tags,
                "sku": {
                    "name": sku_name,
                    "tier": "Burstable" if "B" in sku_name else "GeneralPurpose",
                },
                "administrator_login": admin_username,
                "administrator_login_password": admin_password,
                "version": version,
                "storage": {
                    "storage_size_gb": storage_gb,
                    "auto_grow": "Enabled",
                },
                "backup": {
                    "backup_retention_days": backup_retention_days,
                },
            }

            poller = self.mysql_client.servers.begin_create(
                self.resource_group,
                name,
                server_params
            )
            server = poller.result()

            logger.info(f"Servidor MySQL criado: {name}")

            fqdn = f"{name}.mysql.database.azure.com"

            return {
                "success": True,
                "server_id": server.id,
                "server_name": name,
                "fqdn": fqdn,
                "port": 3306,
                "version": version,
                "sku": sku_name,
                "storage_gb": storage_gb,
                "admin_username": admin_username,
                "admin_password": admin_password,
                "status": server.state,
                "connection_string": self.get_connection_string(
                    "mysql", fqdn, 3306, "mysql", admin_username, admin_password
                ),
            }

        except Exception as e:
            logger.error(f"Erro ao criar servidor MySQL: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def create_database(
        self,
        server_name: str,
        database_name: str,
        engine: str = "postgresql",
        charset: str = "UTF8",
        collation: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria um banco de dados em um servidor existente.

        Args:
            server_name: Nome do servidor
            database_name: Nome do banco
            engine: Engine (postgresql ou mysql)
            charset: Charset
            collation: Collation

        Returns:
            Dicionario com informacoes do banco
        """
        try:
            if engine == "postgresql":
                params = {
                    "charset": charset,
                    "collation": collation or "en_US.utf8",
                }
                poller = self.postgresql_client.databases.begin_create(
                    self.resource_group,
                    server_name,
                    database_name,
                    params
                )
            else:  # mysql
                params = {
                    "charset": charset,
                    "collation": collation or "utf8mb4_unicode_ci",
                }
                poller = self.mysql_client.databases.begin_create(
                    self.resource_group,
                    server_name,
                    database_name,
                    params
                )

            db = poller.result()

            logger.info(f"Banco {database_name} criado no servidor {server_name}")

            return {
                "success": True,
                "database_id": db.id,
                "database_name": database_name,
                "server_name": server_name,
                "charset": charset,
            }

        except Exception as e:
            logger.error(f"Erro ao criar banco de dados: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_server(
        self,
        name: str,
        engine: str = "postgresql"
    ) -> bool:
        """
        Deleta um servidor de banco de dados.

        Args:
            name: Nome do servidor
            engine: Engine (postgresql ou mysql)

        Returns:
            True se deletou com sucesso
        """
        try:
            if engine == "postgresql":
                poller = self.postgresql_client.servers.begin_delete(
                    self.resource_group, name
                )
            else:
                poller = self.mysql_client.servers.begin_delete(
                    self.resource_group, name
                )

            poller.result()
            logger.info(f"Servidor {name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar servidor: {e}")
            return False

    async def get_server(
        self,
        name: str,
        engine: str = "postgresql"
    ) -> Dict[str, Any]:
        """
        Obtem informacoes de um servidor.

        Args:
            name: Nome do servidor
            engine: Engine (postgresql ou mysql)

        Returns:
            Dicionario com informacoes do servidor
        """
        try:
            if engine == "postgresql":
                server = self.postgresql_client.servers.get(
                    self.resource_group, name
                )
                port = 5432
                fqdn = f"{name}.postgres.database.azure.com"
            else:
                server = self.mysql_client.servers.get(
                    self.resource_group, name
                )
                port = 3306
                fqdn = f"{name}.mysql.database.azure.com"

            return {
                "server_id": server.id,
                "server_name": server.name,
                "fqdn": fqdn,
                "port": port,
                "version": server.version,
                "sku": server.sku.name if server.sku else None,
                "status": server.state,
                "location": server.location,
                "admin_username": server.administrator_login,
            }

        except Exception as e:
            logger.error(f"Erro ao obter servidor: {e}")
            return {"error": str(e)}

    async def list_servers(
        self,
        engine: str = "postgresql"
    ) -> List[Dict[str, Any]]:
        """
        Lista servidores de banco de dados.

        Args:
            engine: Engine (postgresql ou mysql)

        Returns:
            Lista de servidores
        """
        try:
            if engine == "postgresql":
                servers_list = self.postgresql_client.servers.list_by_resource_group(
                    self.resource_group
                )
                port = 5432
                suffix = "postgres.database.azure.com"
            else:
                servers_list = self.mysql_client.servers.list_by_resource_group(
                    self.resource_group
                )
                port = 3306
                suffix = "mysql.database.azure.com"

            servers = []
            for server in servers_list:
                servers.append({
                    "server_name": server.name,
                    "fqdn": f"{server.name}.{suffix}",
                    "port": port,
                    "version": server.version,
                    "sku": server.sku.name if server.sku else None,
                    "status": server.state,
                })

            return servers

        except Exception as e:
            logger.error(f"Erro ao listar servidores: {e}")
            return []

    async def start_server(
        self,
        name: str,
        engine: str = "postgresql"
    ) -> bool:
        """
        Inicia um servidor parado.

        Args:
            name: Nome do servidor
            engine: Engine

        Returns:
            True se iniciou com sucesso
        """
        try:
            if engine == "postgresql":
                poller = self.postgresql_client.servers.begin_start(
                    self.resource_group, name
                )
            else:
                poller = self.mysql_client.servers.begin_start(
                    self.resource_group, name
                )

            poller.result()
            logger.info(f"Servidor {name} iniciado")
            return True

        except Exception as e:
            logger.error(f"Erro ao iniciar servidor: {e}")
            return False

    async def stop_server(
        self,
        name: str,
        engine: str = "postgresql"
    ) -> bool:
        """
        Para um servidor.

        Args:
            name: Nome do servidor
            engine: Engine

        Returns:
            True se parou com sucesso
        """
        try:
            if engine == "postgresql":
                poller = self.postgresql_client.servers.begin_stop(
                    self.resource_group, name
                )
            else:
                poller = self.mysql_client.servers.begin_stop(
                    self.resource_group, name
                )

            poller.result()
            logger.info(f"Servidor {name} parado")
            return True

        except Exception as e:
            logger.error(f"Erro ao parar servidor: {e}")
            return False

    async def configure_firewall(
        self,
        server_name: str,
        rule_name: str,
        start_ip: str,
        end_ip: str,
        engine: str = "postgresql"
    ) -> Dict[str, Any]:
        """
        Configura regra de firewall.

        Args:
            server_name: Nome do servidor
            rule_name: Nome da regra
            start_ip: IP inicial
            end_ip: IP final
            engine: Engine

        Returns:
            Dicionario com informacoes da regra
        """
        try:
            params = {
                "start_ip_address": start_ip,
                "end_ip_address": end_ip,
            }

            if engine == "postgresql":
                poller = self.postgresql_client.firewall_rules.begin_create_or_update(
                    self.resource_group,
                    server_name,
                    rule_name,
                    params
                )
            else:
                poller = self.mysql_client.firewall_rules.begin_create_or_update(
                    self.resource_group,
                    server_name,
                    rule_name,
                    params
                )

            rule = poller.result()

            logger.info(f"Regra de firewall {rule_name} criada")

            return {
                "success": True,
                "rule_name": rule_name,
                "start_ip": start_ip,
                "end_ip": end_ip,
            }

        except Exception as e:
            logger.error(f"Erro ao criar regra de firewall: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    def get_connection_string(
        self,
        engine: str,
        host: str,
        port: int,
        database: str,
        username: str,
        password: str
    ) -> str:
        """
        Gera string de conexao.

        Args:
            engine: Engine
            host: Host
            port: Porta
            database: Nome do banco
            username: Usuario
            password: Senha

        Returns:
            String de conexao
        """
        if engine == "postgresql":
            return f"postgresql://{username}%40{host.split('.')[0]}:{password}@{host}:{port}/{database}?sslmode=require"
        elif engine == "mysql":
            return f"mysql://{username}%40{host.split('.')[0]}:{password}@{host}:{port}/{database}?ssl=true"
        else:
            return f"{engine}://{username}:{password}@{host}:{port}/{database}"

    def _generate_password(self, length: int = 24) -> str:
        """Gera senha segura"""
        chars = string.ascii_letters + string.digits + "!#$%&*+-=?"
        return ''.join(secrets.choice(chars) for _ in range(length))

    def get_sku_pricing(self, sku_name: str) -> float:
        """
        Retorna preco por hora de um SKU.

        Args:
            sku_name: Nome do SKU

        Returns:
            Preco por hora em USD
        """
        return DATABASE_PRICING.get(sku_name, 0.05)

    def estimate_monthly_cost(
        self,
        sku_name: str,
        storage_gb: int
    ) -> Dict[str, float]:
        """
        Estima custo mensal de um servidor.

        Args:
            sku_name: SKU do servidor
            storage_gb: Storage em GB

        Returns:
            Dicionario com estimativas de custo
        """
        hourly_price = self.get_sku_pricing(sku_name)
        compute_cost = hourly_price * 24 * 30

        storage_cost = storage_gb * DATABASE_PRICING["storage_per_gb"]

        total_cost = compute_cost + storage_cost

        return {
            "compute_cost": round(compute_cost, 2),
            "storage_cost": round(storage_cost, 2),
            "total_monthly_cost": round(total_cost, 2),
        }
