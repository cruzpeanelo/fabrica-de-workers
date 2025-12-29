# -*- coding: utf-8 -*-
"""
AWS RDS Manager
===============
Gerenciador de bancos de dados RDS para a Fabrica de Agentes.

Funcionalidades:
- Criacao de instancias RDS (PostgreSQL, MySQL, etc)
- Gerenciamento de snapshots
- Monitoramento de status
- Backup e restore
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import secrets
import string

logger = logging.getLogger(__name__)

# Precos RDS (us-east-1) - por hora
RDS_PRICING = {
    # PostgreSQL
    "db.t3.micro": 0.017,
    "db.t3.small": 0.034,
    "db.t3.medium": 0.068,
    "db.t3.large": 0.136,
    "db.r5.large": 0.24,
    "db.r5.xlarge": 0.48,
    # Storage (GB/mes)
    "gp2_storage": 0.115,
    "gp3_storage": 0.08,
    "io1_storage": 0.125,
}

# Engines suportados
SUPPORTED_ENGINES = {
    "postgres": {
        "name": "PostgreSQL",
        "default_version": "15.4",
        "versions": ["13.12", "14.9", "15.4", "16.1"],
        "default_port": 5432,
    },
    "mysql": {
        "name": "MySQL",
        "default_version": "8.0.35",
        "versions": ["5.7.44", "8.0.35"],
        "default_port": 3306,
    },
    "mariadb": {
        "name": "MariaDB",
        "default_version": "10.11.4",
        "versions": ["10.6.14", "10.11.4"],
        "default_port": 3306,
    },
}


class RDSManager:
    """
    Gerenciador de bancos RDS.

    Responsavel por todas as operacoes relacionadas ao RDS:
    - Criacao e configuracao de instancias
    - Gerenciamento de ciclo de vida
    - Snapshots e backups
    - Monitoramento
    """

    def __init__(self, rds_client, config):
        """
        Inicializa o gerenciador RDS.

        Args:
            rds_client: Cliente boto3 RDS
            config: Configuracao AWS
        """
        self.client = rds_client
        self.config = config
        self.region = config.region

    async def create_database(
        self,
        name: str,
        engine: str = "postgres",
        engine_version: Optional[str] = None,
        instance_class: str = "db.t3.micro",
        storage_gb: int = 20,
        storage_type: str = "gp3",
        username: str = "admin",
        password: Optional[str] = None,
        database_name: Optional[str] = None,
        publicly_accessible: bool = False,
        multi_az: bool = False,
        vpc_security_group_ids: Optional[List[str]] = None,
        db_subnet_group_name: Optional[str] = None,
        backup_retention_days: int = 7,
        deletion_protection: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova instancia RDS.

        Args:
            name: Identificador da instancia
            engine: Engine do banco (postgres, mysql, mariadb)
            engine_version: Versao do engine
            instance_class: Classe da instancia
            storage_gb: Tamanho do storage em GB
            storage_type: Tipo do storage (gp2, gp3, io1)
            username: Usuario master
            password: Senha (gerada automaticamente se None)
            database_name: Nome do banco inicial
            publicly_accessible: Se acessivel publicamente
            multi_az: Habilitar Multi-AZ
            vpc_security_group_ids: Security groups
            db_subnet_group_name: Subnet group
            backup_retention_days: Dias de retencao de backup
            deletion_protection: Protecao contra delecao
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do banco criado
        """
        try:
            # Validar engine
            if engine not in SUPPORTED_ENGINES:
                return {
                    "success": False,
                    "error": f"Engine {engine} nao suportado. Use: {list(SUPPORTED_ENGINES.keys())}"
                }

            engine_info = SUPPORTED_ENGINES[engine]

            # Usar versao padrao se nao especificada
            if not engine_version:
                engine_version = engine_info["default_version"]

            # Gerar senha se nao fornecida
            if not password:
                password = self._generate_password()

            # Nome do banco inicial
            if not database_name:
                database_name = name.replace("-", "_").replace(".", "_")

            # Preparar tags
            db_tags = [
                {"Key": "Project", "Value": "FabricaDeAgentes"},
                {"Key": "Name", "Value": name},
            ]
            if tags:
                db_tags.extend([
                    {"Key": k, "Value": v}
                    for k, v in tags.items()
                ])

            # Parametros da instancia
            params = {
                "DBInstanceIdentifier": name,
                "DBInstanceClass": instance_class,
                "Engine": engine,
                "EngineVersion": engine_version,
                "MasterUsername": username,
                "MasterUserPassword": password,
                "DBName": database_name,
                "AllocatedStorage": storage_gb,
                "StorageType": storage_type,
                "PubliclyAccessible": publicly_accessible,
                "MultiAZ": multi_az,
                "BackupRetentionPeriod": backup_retention_days,
                "DeletionProtection": deletion_protection,
                "StorageEncrypted": True,
                "Tags": db_tags,
            }

            if vpc_security_group_ids:
                params["VpcSecurityGroupIds"] = vpc_security_group_ids

            if db_subnet_group_name:
                params["DBSubnetGroupName"] = db_subnet_group_name

            # Criar instancia
            response = self.client.create_db_instance(**params)
            db_instance = response["DBInstance"]

            logger.info(f"Instancia RDS criada: {name}")

            return {
                "success": True,
                "db_instance_id": db_instance["DBInstanceIdentifier"],
                "db_instance_arn": db_instance["DBInstanceArn"],
                "engine": db_instance["Engine"],
                "engine_version": db_instance["EngineVersion"],
                "instance_class": db_instance["DBInstanceClass"],
                "status": db_instance["DBInstanceStatus"],
                "username": username,
                "password": password,
                "database_name": database_name,
                "port": engine_info["default_port"],
                "endpoint": None,  # Disponivel apos ficar available
                "message": "Instancia em criacao. Aguarde alguns minutos para ficar disponivel.",
            }

        except self.client.exceptions.DBInstanceAlreadyExistsFault:
            logger.error(f"Instancia RDS {name} ja existe")
            return {
                "success": False,
                "error": f"Instancia {name} ja existe"
            }

        except Exception as e:
            logger.error(f"Erro ao criar instancia RDS: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def wait_for_available(
        self,
        db_instance_id: str,
        timeout_minutes: int = 20
    ) -> Dict[str, Any]:
        """
        Aguarda instancia ficar disponivel.

        Args:
            db_instance_id: ID da instancia
            timeout_minutes: Timeout em minutos

        Returns:
            Dicionario com informacoes da instancia
        """
        try:
            waiter = self.client.get_waiter("db_instance_available")
            waiter.wait(
                DBInstanceIdentifier=db_instance_id,
                WaiterConfig={
                    "Delay": 30,
                    "MaxAttempts": timeout_minutes * 2
                }
            )

            # Obter informacoes atualizadas
            return await self.get_database(db_instance_id)

        except Exception as e:
            logger.error(f"Timeout aguardando instancia: {e}")
            return {"error": str(e)}

    async def get_database(self, db_instance_id: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma instancia RDS.

        Args:
            db_instance_id: ID da instancia

        Returns:
            Dicionario com informacoes da instancia
        """
        try:
            response = self.client.describe_db_instances(
                DBInstanceIdentifier=db_instance_id
            )

            if not response["DBInstances"]:
                return {"error": "Instancia nao encontrada"}

            db = response["DBInstances"][0]
            endpoint = db.get("Endpoint", {})

            return {
                "db_instance_id": db["DBInstanceIdentifier"],
                "db_instance_arn": db["DBInstanceArn"],
                "engine": db["Engine"],
                "engine_version": db["EngineVersion"],
                "instance_class": db["DBInstanceClass"],
                "status": db["DBInstanceStatus"],
                "endpoint": endpoint.get("Address"),
                "port": endpoint.get("Port"),
                "database_name": db.get("DBName"),
                "username": db["MasterUsername"],
                "storage_gb": db["AllocatedStorage"],
                "storage_type": db["StorageType"],
                "multi_az": db["MultiAZ"],
                "publicly_accessible": db["PubliclyAccessible"],
                "availability_zone": db.get("AvailabilityZone"),
                "vpc_id": db.get("DBSubnetGroup", {}).get("VpcId"),
                "security_groups": [
                    sg["VpcSecurityGroupId"]
                    for sg in db.get("VpcSecurityGroups", [])
                ],
                "created_at": db.get("InstanceCreateTime"),
            }

        except self.client.exceptions.DBInstanceNotFoundFault:
            return {"error": "Instancia nao encontrada"}

        except Exception as e:
            logger.error(f"Erro ao obter instancia RDS: {e}")
            return {"error": str(e)}

    async def delete_database(
        self,
        db_instance_id: str,
        skip_final_snapshot: bool = False,
        final_snapshot_id: Optional[str] = None
    ) -> bool:
        """
        Deleta uma instancia RDS.

        Args:
            db_instance_id: ID da instancia
            skip_final_snapshot: Pular snapshot final
            final_snapshot_id: ID do snapshot final

        Returns:
            True se deletou com sucesso
        """
        try:
            params = {
                "DBInstanceIdentifier": db_instance_id,
                "SkipFinalSnapshot": skip_final_snapshot,
            }

            if not skip_final_snapshot:
                if not final_snapshot_id:
                    final_snapshot_id = f"{db_instance_id}-final-{datetime.now().strftime('%Y%m%d%H%M%S')}"
                params["FinalDBSnapshotIdentifier"] = final_snapshot_id

            # Remover protecao contra delecao se necessario
            try:
                self.client.modify_db_instance(
                    DBInstanceIdentifier=db_instance_id,
                    DeletionProtection=False,
                    ApplyImmediately=True
                )
            except Exception:
                pass  # Ignorar se nao conseguir modificar

            self.client.delete_db_instance(**params)
            logger.info(f"Instancia RDS {db_instance_id} em processo de delecao")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar instancia RDS: {e}")
            return False

    async def start_database(self, db_instance_id: str) -> bool:
        """
        Inicia uma instancia RDS parada.

        Args:
            db_instance_id: ID da instancia

        Returns:
            True se iniciou com sucesso
        """
        try:
            self.client.start_db_instance(DBInstanceIdentifier=db_instance_id)
            logger.info(f"Instancia RDS {db_instance_id} iniciando")
            return True

        except Exception as e:
            logger.error(f"Erro ao iniciar instancia RDS: {e}")
            return False

    async def stop_database(self, db_instance_id: str) -> bool:
        """
        Para uma instancia RDS.

        Args:
            db_instance_id: ID da instancia

        Returns:
            True se parou com sucesso
        """
        try:
            self.client.stop_db_instance(DBInstanceIdentifier=db_instance_id)
            logger.info(f"Instancia RDS {db_instance_id} parando")
            return True

        except Exception as e:
            logger.error(f"Erro ao parar instancia RDS: {e}")
            return False

    async def reboot_database(self, db_instance_id: str) -> bool:
        """
        Reinicia uma instancia RDS.

        Args:
            db_instance_id: ID da instancia

        Returns:
            True se reiniciou com sucesso
        """
        try:
            self.client.reboot_db_instance(DBInstanceIdentifier=db_instance_id)
            logger.info(f"Instancia RDS {db_instance_id} reiniciando")
            return True

        except Exception as e:
            logger.error(f"Erro ao reiniciar instancia RDS: {e}")
            return False

    async def create_snapshot(
        self,
        db_instance_id: str,
        snapshot_id: Optional[str] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria snapshot de uma instancia RDS.

        Args:
            db_instance_id: ID da instancia
            snapshot_id: ID do snapshot (gerado automaticamente se None)
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do snapshot
        """
        try:
            if not snapshot_id:
                snapshot_id = f"{db_instance_id}-snap-{datetime.now().strftime('%Y%m%d%H%M%S')}"

            snapshot_tags = [
                {"Key": "Project", "Value": "FabricaDeAgentes"},
                {"Key": "DBInstance", "Value": db_instance_id},
            ]
            if tags:
                snapshot_tags.extend([
                    {"Key": k, "Value": v}
                    for k, v in tags.items()
                ])

            response = self.client.create_db_snapshot(
                DBSnapshotIdentifier=snapshot_id,
                DBInstanceIdentifier=db_instance_id,
                Tags=snapshot_tags
            )

            snapshot = response["DBSnapshot"]

            logger.info(f"Snapshot {snapshot_id} criado")

            return {
                "success": True,
                "snapshot_id": snapshot["DBSnapshotIdentifier"],
                "snapshot_arn": snapshot["DBSnapshotArn"],
                "db_instance_id": snapshot["DBInstanceIdentifier"],
                "status": snapshot["Status"],
                "engine": snapshot["Engine"],
            }

        except Exception as e:
            logger.error(f"Erro ao criar snapshot: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def list_databases(
        self,
        filters: Optional[List[Dict]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista instancias RDS.

        Args:
            filters: Filtros AWS

        Returns:
            Lista de instancias
        """
        try:
            params = {}
            if filters:
                params["Filters"] = filters

            response = self.client.describe_db_instances(**params)

            instances = []
            for db in response["DBInstances"]:
                endpoint = db.get("Endpoint", {})
                instances.append({
                    "db_instance_id": db["DBInstanceIdentifier"],
                    "engine": db["Engine"],
                    "instance_class": db["DBInstanceClass"],
                    "status": db["DBInstanceStatus"],
                    "endpoint": endpoint.get("Address"),
                    "port": endpoint.get("Port"),
                    "storage_gb": db["AllocatedStorage"],
                    "multi_az": db["MultiAZ"],
                })

            return instances

        except Exception as e:
            logger.error(f"Erro ao listar instancias RDS: {e}")
            return []

    async def list_snapshots(
        self,
        db_instance_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista snapshots RDS.

        Args:
            db_instance_id: Filtrar por instancia

        Returns:
            Lista de snapshots
        """
        try:
            params = {"SnapshotType": "manual"}
            if db_instance_id:
                params["DBInstanceIdentifier"] = db_instance_id

            response = self.client.describe_db_snapshots(**params)

            snapshots = []
            for snap in response["DBSnapshots"]:
                snapshots.append({
                    "snapshot_id": snap["DBSnapshotIdentifier"],
                    "db_instance_id": snap["DBInstanceIdentifier"],
                    "engine": snap["Engine"],
                    "status": snap["Status"],
                    "storage_gb": snap.get("AllocatedStorage"),
                    "created_at": snap.get("SnapshotCreateTime"),
                })

            return snapshots

        except Exception as e:
            logger.error(f"Erro ao listar snapshots: {e}")
            return []

    async def restore_from_snapshot(
        self,
        snapshot_id: str,
        new_instance_id: str,
        instance_class: Optional[str] = None,
        publicly_accessible: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Restaura instancia a partir de snapshot.

        Args:
            snapshot_id: ID do snapshot
            new_instance_id: ID da nova instancia
            instance_class: Classe da instancia (usa original se None)
            publicly_accessible: Se acessivel publicamente
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes da nova instancia
        """
        try:
            db_tags = [
                {"Key": "Project", "Value": "FabricaDeAgentes"},
                {"Key": "RestoredFrom", "Value": snapshot_id},
            ]
            if tags:
                db_tags.extend([
                    {"Key": k, "Value": v}
                    for k, v in tags.items()
                ])

            params = {
                "DBInstanceIdentifier": new_instance_id,
                "DBSnapshotIdentifier": snapshot_id,
                "PubliclyAccessible": publicly_accessible,
                "Tags": db_tags,
            }

            if instance_class:
                params["DBInstanceClass"] = instance_class

            response = self.client.restore_db_instance_from_db_snapshot(**params)
            db_instance = response["DBInstance"]

            logger.info(f"Instancia {new_instance_id} restaurada de {snapshot_id}")

            return {
                "success": True,
                "db_instance_id": db_instance["DBInstanceIdentifier"],
                "status": db_instance["DBInstanceStatus"],
                "message": "Restauracao iniciada. Aguarde alguns minutos.",
            }

        except Exception as e:
            logger.error(f"Erro ao restaurar de snapshot: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    def get_connection_string(
        self,
        engine: str,
        endpoint: str,
        port: int,
        database: str,
        username: str,
        password: str
    ) -> str:
        """
        Gera string de conexao para o banco.

        Args:
            engine: Engine do banco
            endpoint: Endpoint do banco
            port: Porta
            database: Nome do banco
            username: Usuario
            password: Senha

        Returns:
            String de conexao
        """
        if engine in ["postgres", "postgresql"]:
            return f"postgresql://{username}:{password}@{endpoint}:{port}/{database}"
        elif engine == "mysql":
            return f"mysql://{username}:{password}@{endpoint}:{port}/{database}"
        elif engine == "mariadb":
            return f"mariadb://{username}:{password}@{endpoint}:{port}/{database}"
        else:
            return f"{engine}://{username}:{password}@{endpoint}:{port}/{database}"

    def _generate_password(self, length: int = 24) -> str:
        """
        Gera senha segura para o banco.

        Args:
            length: Tamanho da senha

        Returns:
            Senha gerada
        """
        # Caracteres permitidos (evitando caracteres problematicos)
        chars = string.ascii_letters + string.digits + "!#$%&*+-=?"
        return ''.join(secrets.choice(chars) for _ in range(length))

    def get_instance_pricing(self, instance_class: str) -> float:
        """
        Retorna preco por hora de uma classe de instancia.

        Args:
            instance_class: Classe da instancia

        Returns:
            Preco por hora em USD
        """
        return RDS_PRICING.get(instance_class, 0.02)

    def estimate_monthly_cost(
        self,
        instance_class: str,
        storage_gb: int,
        storage_type: str = "gp3",
        multi_az: bool = False
    ) -> Dict[str, float]:
        """
        Estima custo mensal de uma instancia RDS.

        Args:
            instance_class: Classe da instancia
            storage_gb: Tamanho do storage em GB
            storage_type: Tipo do storage
            multi_az: Se Multi-AZ habilitado

        Returns:
            Dicionario com estimativas de custo
        """
        # Custo da instancia
        hourly_price = self.get_instance_pricing(instance_class)
        instance_cost = hourly_price * 24 * 30

        # Custo do storage
        storage_price = RDS_PRICING.get(f"{storage_type}_storage", 0.10)
        storage_cost = storage_gb * storage_price

        # Multi-AZ dobra o custo
        if multi_az:
            instance_cost *= 2
            storage_cost *= 2

        total_cost = instance_cost + storage_cost

        return {
            "instance_cost": round(instance_cost, 2),
            "storage_cost": round(storage_cost, 2),
            "total_monthly_cost": round(total_cost, 2),
            "multi_az": multi_az,
        }
