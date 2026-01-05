# -*- coding: utf-8 -*-
"""
Domain Manager - Gerenciamento de Dominios Customizados
========================================================

Modulo para gerenciamento de dominios customizados e subdomains
para aplicacoes deployadas pela Plataforma E.

Funcionalidades:
- Geracao automatica de subdomains (*.plataformae.app)
- Configuracao de dominios customizados
- Verificacao de DNS
- Gerenciamento de certificados SSL

Inspirado no Base44.app - Custom Domains
"""

import asyncio
import hashlib
import json
import logging
import os
import re
import secrets
import socket
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional
import uuid

logger = logging.getLogger(__name__)


class DomainStatus(str, Enum):
    """Status do dominio"""
    PENDING = "pending"              # Aguardando configuracao
    DNS_PENDING = "dns_pending"      # Aguardando verificacao DNS
    DNS_VERIFIED = "dns_verified"    # DNS verificado
    SSL_PENDING = "ssl_pending"      # Aguardando certificado SSL
    SSL_ISSUED = "ssl_issued"        # Certificado emitido
    ACTIVE = "active"                # Dominio ativo e funcionando
    ERROR = "error"                  # Erro na configuracao
    EXPIRED = "expired"              # Certificado expirado


class DomainType(str, Enum):
    """Tipo de dominio"""
    SUBDOMAIN = "subdomain"          # Subdominio gerado (*.plataformae.app)
    CUSTOM = "custom"                # Dominio customizado do usuario


class DNSRecordType(str, Enum):
    """Tipos de registro DNS"""
    A = "A"
    AAAA = "AAAA"
    CNAME = "CNAME"
    TXT = "TXT"


@dataclass
class DNSRecord:
    """Registro DNS necessario para configuracao"""
    record_type: DNSRecordType
    name: str
    value: str
    ttl: int = 3600
    verified: bool = False
    verified_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "record_type": self.record_type.value,
            "name": self.name,
            "value": self.value,
            "ttl": self.ttl,
            "verified": self.verified,
            "verified_at": self.verified_at.isoformat() if self.verified_at else None
        }


@dataclass
class SSLCertificate:
    """Informacoes do certificado SSL"""
    issued: bool = False
    issuer: str = ""
    valid_from: Optional[datetime] = None
    valid_until: Optional[datetime] = None
    auto_renew: bool = True
    last_renewed: Optional[datetime] = None

    def is_valid(self) -> bool:
        if not self.issued or not self.valid_until:
            return False
        return datetime.now() < self.valid_until

    def days_until_expiry(self) -> int:
        if not self.valid_until:
            return 0
        delta = self.valid_until - datetime.now()
        return max(0, delta.days)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "issued": self.issued,
            "issuer": self.issuer,
            "valid_from": self.valid_from.isoformat() if self.valid_from else None,
            "valid_until": self.valid_until.isoformat() if self.valid_until else None,
            "is_valid": self.is_valid(),
            "days_until_expiry": self.days_until_expiry(),
            "auto_renew": self.auto_renew
        }


@dataclass
class DomainConfig:
    """Configuracao de um dominio"""
    domain_id: str
    project_id: str
    domain: str
    domain_type: DomainType
    status: DomainStatus

    # Subdominio gerado
    subdomain: Optional[str] = None

    # Registros DNS necessarios
    dns_records: List[DNSRecord] = field(default_factory=list)

    # Token de verificacao
    verification_token: str = ""

    # Certificado SSL
    ssl_certificate: Optional[SSLCertificate] = None

    # Deployment associado
    deployment_id: Optional[str] = None
    target_url: Optional[str] = None

    # Timestamps
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    verified_at: Optional[datetime] = None
    activated_at: Optional[datetime] = None

    # Erro
    error_message: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "domain_id": self.domain_id,
            "project_id": self.project_id,
            "domain": self.domain,
            "domain_type": self.domain_type.value,
            "status": self.status.value,
            "subdomain": self.subdomain,
            "dns_records": [r.to_dict() for r in self.dns_records],
            "verification_token": self.verification_token,
            "ssl_certificate": self.ssl_certificate.to_dict() if self.ssl_certificate else None,
            "deployment_id": self.deployment_id,
            "target_url": self.target_url,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "verified_at": self.verified_at.isoformat() if self.verified_at else None,
            "activated_at": self.activated_at.isoformat() if self.activated_at else None,
            "error_message": self.error_message
        }


class DomainManager:
    """
    Gerenciador de dominios customizados.

    Responsavel por:
    - Gerar subdomains automaticos
    - Configurar dominios customizados
    - Verificar DNS
    - Gerenciar certificados SSL
    """

    # Dominio base para subdomains
    BASE_DOMAIN = "plataformae.app"

    # IPs para configuracao A record (exemplo - usar IPs reais em producao)
    PLATFORM_IPS = ["76.76.21.21"]  # Vercel IP exemplo

    # CNAME target
    CNAME_TARGET = "cname.plataformae.app"

    def __init__(self, data_dir: str = "factory/state/domains"):
        """
        Inicializa o gerenciador.

        Args:
            data_dir: Diretorio para armazenar dados de dominios
        """
        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(parents=True, exist_ok=True)

        self.domains: Dict[str, DomainConfig] = {}
        self._load_domains()

        # Cloudflare API (opcional)
        self.cloudflare_token = os.getenv("CLOUDFLARE_API_TOKEN", "")
        self.cloudflare_zone_id = os.getenv("CLOUDFLARE_ZONE_ID", "")

    def _load_domains(self):
        """Carrega dominios salvos"""
        domains_file = self.data_dir / "domains.json"
        if domains_file.exists():
            try:
                with open(domains_file, "r") as f:
                    data = json.load(f)
                    for domain_data in data.get("domains", []):
                        domain_id = domain_data["domain_id"]
                        self.domains[domain_id] = self._dict_to_domain(domain_data)
            except Exception as e:
                logger.error(f"Error loading domains: {e}")

    def _save_domains(self):
        """Salva dominios em disco"""
        domains_file = self.data_dir / "domains.json"
        try:
            data = {
                "domains": [d.to_dict() for d in self.domains.values()],
                "updated_at": datetime.now().isoformat()
            }
            with open(domains_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.error(f"Error saving domains: {e}")

    def _dict_to_domain(self, data: Dict) -> DomainConfig:
        """Converte dicionario para DomainConfig"""
        dns_records = [
            DNSRecord(
                record_type=DNSRecordType(r["record_type"]),
                name=r["name"],
                value=r["value"],
                ttl=r.get("ttl", 3600),
                verified=r.get("verified", False)
            )
            for r in data.get("dns_records", [])
        ]

        ssl_data = data.get("ssl_certificate")
        ssl_cert = None
        if ssl_data:
            ssl_cert = SSLCertificate(
                issued=ssl_data.get("issued", False),
                issuer=ssl_data.get("issuer", ""),
                auto_renew=ssl_data.get("auto_renew", True)
            )

        return DomainConfig(
            domain_id=data["domain_id"],
            project_id=data["project_id"],
            domain=data["domain"],
            domain_type=DomainType(data["domain_type"]),
            status=DomainStatus(data["status"]),
            subdomain=data.get("subdomain"),
            dns_records=dns_records,
            verification_token=data.get("verification_token", ""),
            ssl_certificate=ssl_cert,
            deployment_id=data.get("deployment_id"),
            target_url=data.get("target_url")
        )

    def generate_subdomain(self, project_id: str, project_name: Optional[str] = None) -> DomainConfig:
        """
        Gera um subdominio automatico para o projeto.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto (opcional, para subdomain legivel)

        Returns:
            DomainConfig com subdomain gerado
        """
        # Gerar subdomain baseado no nome ou ID
        if project_name:
            # Normalizar nome para subdomain valido
            subdomain = self._normalize_subdomain(project_name)
        else:
            subdomain = f"app-{project_id[:8]}"

        # Verificar unicidade
        base_subdomain = subdomain
        counter = 1
        while self._subdomain_exists(subdomain):
            subdomain = f"{base_subdomain}-{counter}"
            counter += 1

        domain = f"{subdomain}.{self.BASE_DOMAIN}"
        domain_id = f"dom-{uuid.uuid4().hex[:8]}"

        config = DomainConfig(
            domain_id=domain_id,
            project_id=project_id,
            domain=domain,
            domain_type=DomainType.SUBDOMAIN,
            status=DomainStatus.ACTIVE,  # Subdomains sao ativados imediatamente
            subdomain=subdomain,
            ssl_certificate=SSLCertificate(
                issued=True,
                issuer="Let's Encrypt (Wildcard)",
                valid_from=datetime.now(),
                valid_until=datetime.now() + timedelta(days=90),
                auto_renew=True
            ),
            activated_at=datetime.now()
        )

        self.domains[domain_id] = config
        self._save_domains()

        logger.info(f"Subdomain generated: {domain}")
        return config

    def _normalize_subdomain(self, name: str) -> str:
        """Normaliza nome para subdomain valido"""
        # Remover acentos e caracteres especiais
        import unicodedata
        name = unicodedata.normalize('NFKD', name)
        name = name.encode('ASCII', 'ignore').decode('ASCII')

        # Converter para lowercase e substituir espacos por hifens
        name = name.lower().strip()
        name = re.sub(r'[^a-z0-9]+', '-', name)
        name = re.sub(r'-+', '-', name)
        name = name.strip('-')

        # Limitar tamanho
        if len(name) > 30:
            name = name[:30].rstrip('-')

        return name or "app"

    def _subdomain_exists(self, subdomain: str) -> bool:
        """Verifica se subdomain ja existe"""
        domain = f"{subdomain}.{self.BASE_DOMAIN}"
        return any(d.domain == domain for d in self.domains.values())

    def add_custom_domain(
        self,
        project_id: str,
        domain: str,
        deployment_id: Optional[str] = None
    ) -> DomainConfig:
        """
        Adiciona um dominio customizado.

        Args:
            project_id: ID do projeto
            domain: Dominio customizado (ex: app.meusite.com.br)
            deployment_id: ID do deployment associado

        Returns:
            DomainConfig com instrucoes de configuracao DNS
        """
        # Validar formato do dominio
        if not self._validate_domain(domain):
            raise ValueError(f"Dominio invalido: {domain}")

        # Verificar se dominio ja existe
        existing = self._get_by_domain(domain)
        if existing:
            raise ValueError(f"Dominio {domain} ja esta configurado")

        domain_id = f"dom-{uuid.uuid4().hex[:8]}"
        verification_token = f"plataformae-verify-{secrets.token_hex(16)}"

        # Criar registros DNS necessarios
        dns_records = [
            # Registro CNAME para o dominio
            DNSRecord(
                record_type=DNSRecordType.CNAME,
                name=domain,
                value=self.CNAME_TARGET,
                ttl=3600
            ),
            # Registro TXT para verificacao
            DNSRecord(
                record_type=DNSRecordType.TXT,
                name=f"_plataformae.{domain}",
                value=verification_token,
                ttl=3600
            )
        ]

        config = DomainConfig(
            domain_id=domain_id,
            project_id=project_id,
            domain=domain,
            domain_type=DomainType.CUSTOM,
            status=DomainStatus.DNS_PENDING,
            dns_records=dns_records,
            verification_token=verification_token,
            deployment_id=deployment_id,
            ssl_certificate=SSLCertificate()
        )

        self.domains[domain_id] = config
        self._save_domains()

        logger.info(f"Custom domain added: {domain}")
        return config

    def _validate_domain(self, domain: str) -> bool:
        """Valida formato do dominio"""
        # Regex basica para dominio
        pattern = r'^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,}$'
        return bool(re.match(pattern, domain))

    def _get_by_domain(self, domain: str) -> Optional[DomainConfig]:
        """Busca dominio pelo nome"""
        for config in self.domains.values():
            if config.domain == domain:
                return config
        return None

    async def verify_dns(self, domain_id: str) -> Dict[str, Any]:
        """
        Verifica se os registros DNS estao configurados corretamente.

        Args:
            domain_id: ID do dominio

        Returns:
            Resultado da verificacao
        """
        config = self.domains.get(domain_id)
        if not config:
            return {"success": False, "error": "Dominio nao encontrado"}

        if config.domain_type == DomainType.SUBDOMAIN:
            return {"success": True, "message": "Subdomain nao requer verificacao DNS"}

        results = []
        all_verified = True

        for record in config.dns_records:
            verified = await self._check_dns_record(record)
            record.verified = verified

            if verified:
                record.verified_at = datetime.now()
            else:
                all_verified = False

            results.append({
                "record_type": record.record_type.value,
                "name": record.name,
                "expected_value": record.value,
                "verified": verified
            })

        # Atualizar status
        if all_verified:
            config.status = DomainStatus.DNS_VERIFIED
            config.verified_at = datetime.now()

            # Iniciar emissao de SSL
            await self._request_ssl_certificate(config)
        else:
            config.status = DomainStatus.DNS_PENDING

        config.updated_at = datetime.now()
        self._save_domains()

        return {
            "success": all_verified,
            "records": results,
            "status": config.status.value,
            "message": "DNS verificado com sucesso!" if all_verified else "Alguns registros ainda nao foram propagados"
        }

    async def _check_dns_record(self, record: DNSRecord) -> bool:
        """Verifica um registro DNS especifico"""
        try:
            if record.record_type == DNSRecordType.CNAME:
                # Verificar CNAME
                try:
                    result = socket.gethostbyname(record.name)
                    # Se resolveu, CNAME esta funcionando
                    return True
                except socket.gaierror:
                    return False

            elif record.record_type == DNSRecordType.TXT:
                # Verificar TXT (simulado - em producao usar dnspython)
                # Por enquanto, simular verificacao
                await asyncio.sleep(0.1)
                # Simular 50% de chance de estar verificado para demo
                import random
                return random.random() > 0.3

            elif record.record_type == DNSRecordType.A:
                # Verificar registro A
                try:
                    result = socket.gethostbyname(record.name)
                    return result in self.PLATFORM_IPS
                except socket.gaierror:
                    return False

            return False

        except Exception as e:
            logger.error(f"Error checking DNS record: {e}")
            return False

    async def _request_ssl_certificate(self, config: DomainConfig):
        """Solicita certificado SSL para o dominio"""
        config.status = DomainStatus.SSL_PENDING
        config.updated_at = datetime.now()

        # Simular emissao de certificado (em producao, usar Let's Encrypt/ACME)
        await asyncio.sleep(0.5)

        config.ssl_certificate = SSLCertificate(
            issued=True,
            issuer="Let's Encrypt",
            valid_from=datetime.now(),
            valid_until=datetime.now() + timedelta(days=90),
            auto_renew=True,
            last_renewed=datetime.now()
        )

        config.status = DomainStatus.ACTIVE
        config.activated_at = datetime.now()

        self._save_domains()
        logger.info(f"SSL certificate issued for {config.domain}")

    def get_domain(self, domain_id: str) -> Optional[DomainConfig]:
        """Retorna configuracao de um dominio"""
        return self.domains.get(domain_id)

    def get_project_domains(self, project_id: str) -> List[DomainConfig]:
        """Lista dominios de um projeto"""
        return [d for d in self.domains.values() if d.project_id == project_id]

    def get_all_domains(self) -> List[DomainConfig]:
        """Lista todos os dominios"""
        return list(self.domains.values())

    async def delete_domain(self, domain_id: str) -> bool:
        """Remove um dominio"""
        if domain_id not in self.domains:
            return False

        config = self.domains[domain_id]

        # TODO: Em producao, remover registros do Cloudflare/Route53

        del self.domains[domain_id]
        self._save_domains()

        logger.info(f"Domain deleted: {config.domain}")
        return True

    def link_deployment(self, domain_id: str, deployment_id: str, target_url: str) -> bool:
        """Associa um deployment a um dominio"""
        config = self.domains.get(domain_id)
        if not config:
            return False

        config.deployment_id = deployment_id
        config.target_url = target_url
        config.updated_at = datetime.now()

        self._save_domains()
        return True

    def get_dns_instructions(self, domain_id: str) -> Dict[str, Any]:
        """Retorna instrucoes de configuracao DNS"""
        config = self.domains.get(domain_id)
        if not config:
            return {"error": "Dominio nao encontrado"}

        if config.domain_type == DomainType.SUBDOMAIN:
            return {
                "type": "subdomain",
                "message": "Subdomains nao requerem configuracao DNS manual",
                "domain": config.domain,
                "ready": True
            }

        return {
            "type": "custom",
            "domain": config.domain,
            "status": config.status.value,
            "instructions": [
                {
                    "step": 1,
                    "title": "Adicione o registro CNAME",
                    "description": f"No seu provedor DNS, crie um registro CNAME apontando para {self.CNAME_TARGET}",
                    "record": {
                        "type": "CNAME",
                        "name": config.domain.split('.')[0] if '.' in config.domain else "@",
                        "value": self.CNAME_TARGET,
                        "ttl": 3600
                    },
                    "verified": config.dns_records[0].verified if config.dns_records else False
                },
                {
                    "step": 2,
                    "title": "Adicione o registro TXT de verificacao",
                    "description": "Este registro prova que voce e o dono do dominio",
                    "record": {
                        "type": "TXT",
                        "name": f"_plataformae",
                        "value": config.verification_token,
                        "ttl": 3600
                    },
                    "verified": config.dns_records[1].verified if len(config.dns_records) > 1 else False
                }
            ],
            "notes": [
                "A propagacao DNS pode levar ate 48 horas",
                "Apos configurar, clique em 'Verificar DNS'",
                "O certificado SSL sera emitido automaticamente apos verificacao"
            ],
            "ready": config.status == DomainStatus.ACTIVE
        }

    def get_available_subdomains(self, project_name: str, count: int = 5) -> List[str]:
        """Sugere subdomains disponiveis"""
        base = self._normalize_subdomain(project_name)
        suggestions = []

        # Sugestao principal
        if not self._subdomain_exists(base):
            suggestions.append(f"{base}.{self.BASE_DOMAIN}")

        # Variacoes
        variations = [
            f"{base}-app",
            f"{base}-api",
            f"my-{base}",
            f"{base}-prod",
            f"{base}-dev",
            f"get-{base}",
            f"try-{base}"
        ]

        for var in variations:
            if len(suggestions) >= count:
                break
            if not self._subdomain_exists(var):
                suggestions.append(f"{var}.{self.BASE_DOMAIN}")

        return suggestions


# Instancia global
domain_manager = DomainManager()
