# -*- coding: utf-8 -*-
"""
Local Encryption
================
Criptografia local com Fernet para fallback em ambientes dev.

Terminal 5 - Issue #299
"""

import os
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from pathlib import Path

logger = logging.getLogger(__name__)

# Tenta importar cryptography
try:
    from cryptography.fernet import Fernet, InvalidToken
    CRYPTOGRAPHY_AVAILABLE = True
except ImportError:
    CRYPTOGRAPHY_AVAILABLE = False
    logger.warning("Cryptography nao instalado. Use: pip install cryptography")


@dataclass
class LocalEncryptionConfig:
    """
    Configuracao para criptografia local.

    Attributes:
        storage_path: Caminho para armazenar secrets criptografados
        key_file: Caminho para arquivo com chave de criptografia
        auto_generate_key: Gerar chave automaticamente se nao existir
    """
    storage_path: str = ".secrets"
    key_file: str = ".secrets/.encryption_key"
    auto_generate_key: bool = True

    def validate(self) -> List[str]:
        """Valida a configuracao"""
        errors = []
        if not self.storage_path:
            errors.append("storage_path e obrigatorio")
        return errors


class LocalEncryption:
    """
    Gerenciador de secrets local com criptografia Fernet.

    Usar apenas para desenvolvimento ou como fallback.
    Para producao, usar Azure Key Vault.

    Exemplo:
        config = LocalEncryptionConfig(
            storage_path=".secrets",
            auto_generate_key=True
        )
        encryption = LocalEncryption(config)

        # Salvar secret
        encryption.set_secret("api-key", "sk-...")

        # Recuperar secret
        value = encryption.get_secret("api-key")
    """

    def __init__(self, config: LocalEncryptionConfig):
        """
        Inicializa o gerenciador.

        Args:
            config: Configuracao de criptografia local
        """
        if not CRYPTOGRAPHY_AVAILABLE:
            raise ImportError(
                "Cryptography nao instalado. Use: pip install cryptography"
            )

        self.config = config
        self._fernet: Optional[Fernet] = None
        self._secrets_file: Path = Path(config.storage_path) / "secrets.enc"
        self._secrets: Dict[str, Dict[str, Any]] = {}
        self._audit_log: List[Dict[str, Any]] = []

        self._ensure_storage()
        self._load_key()
        self._load_secrets()

    def _ensure_storage(self):
        """Cria diretorio de armazenamento se nao existir"""
        storage_path = Path(self.config.storage_path)
        if not storage_path.exists():
            storage_path.mkdir(parents=True, mode=0o700)
            logger.info(f"Diretorio de secrets criado: {storage_path}")

    def _load_key(self):
        """Carrega ou gera chave de criptografia"""
        key_path = Path(self.config.key_file)

        if key_path.exists():
            with open(key_path, "rb") as f:
                key = f.read()
            logger.info("Chave de criptografia carregada")
        elif self.config.auto_generate_key:
            key = Fernet.generate_key()
            key_path.parent.mkdir(parents=True, exist_ok=True)
            with open(key_path, "wb") as f:
                f.write(key)
            # Restringe permissoes do arquivo
            os.chmod(key_path, 0o600)
            logger.info("Nova chave de criptografia gerada")
        else:
            raise ValueError(f"Chave de criptografia nao encontrada: {key_path}")

        self._fernet = Fernet(key)

    def _load_secrets(self):
        """Carrega secrets do arquivo"""
        if self._secrets_file.exists():
            try:
                with open(self._secrets_file, "rb") as f:
                    encrypted_data = f.read()

                if encrypted_data:
                    decrypted_data = self._fernet.decrypt(encrypted_data)
                    self._secrets = json.loads(decrypted_data.decode())
                    logger.info(f"Carregados {len(self._secrets)} secrets")
            except InvalidToken:
                logger.error("Falha ao descriptografar secrets - chave incorreta?")
                self._secrets = {}
            except Exception as e:
                logger.error(f"Erro ao carregar secrets: {e}")
                self._secrets = {}
        else:
            self._secrets = {}

    def _save_secrets(self):
        """Salva secrets no arquivo"""
        try:
            data = json.dumps(self._secrets).encode()
            encrypted_data = self._fernet.encrypt(data)

            with open(self._secrets_file, "wb") as f:
                f.write(encrypted_data)

            # Restringe permissoes do arquivo
            os.chmod(self._secrets_file, 0o600)
        except Exception as e:
            logger.error(f"Erro ao salvar secrets: {e}")
            raise

    def _log_access(
        self,
        operation: str,
        secret_name: str,
        success: bool,
        tenant_id: Optional[str] = None,
        error: Optional[str] = None
    ):
        """Registra acesso para auditoria"""
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "operation": operation,
            "secret_name": secret_name,
            "success": success,
            "tenant_id": tenant_id,
            "error": error,
            "storage": "local"
        }
        self._audit_log.append(log_entry)

        if len(self._audit_log) > 1000:
            self._audit_log = self._audit_log[-1000:]

    def _get_secret_key(self, name: str, tenant_id: Optional[str] = None) -> str:
        """Gera chave do secret com isolamento de tenant"""
        if tenant_id:
            return f"tenant:{tenant_id}:{name}"
        return name

    # =========================================================================
    # Operacoes CRUD
    # =========================================================================

    def get_secret(
        self,
        name: str,
        tenant_id: Optional[str] = None
    ) -> Optional[str]:
        """
        Recupera um secret.

        Args:
            name: Nome do secret
            tenant_id: ID do tenant para isolamento

        Returns:
            Valor do secret ou None se nao encontrado
        """
        secret_key = self._get_secret_key(name, tenant_id)

        if secret_key in self._secrets:
            secret_data = self._secrets[secret_key]
            self._log_access("GET", secret_key, True, tenant_id)
            return secret_data.get("value")

        self._log_access("GET", secret_key, False, tenant_id, "Not found")
        return None

    def set_secret(
        self,
        name: str,
        value: str,
        tenant_id: Optional[str] = None,
        content_type: Optional[str] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Salva um secret.

        Args:
            name: Nome do secret
            value: Valor do secret
            tenant_id: ID do tenant para isolamento
            content_type: Tipo de conteudo
            tags: Tags adicionais

        Returns:
            True se salvo com sucesso
        """
        secret_key = self._get_secret_key(name, tenant_id)

        try:
            self._secrets[secret_key] = {
                "value": value,
                "content_type": content_type,
                "tenant_id": tenant_id,
                "created_at": datetime.utcnow().isoformat(),
                "tags": tags or {}
            }
            self._save_secrets()
            self._log_access("SET", secret_key, True, tenant_id)
            return True
        except Exception as e:
            self._log_access("SET", secret_key, False, tenant_id, str(e))
            return False

    def delete_secret(
        self,
        name: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """
        Remove um secret.

        Args:
            name: Nome do secret
            tenant_id: ID do tenant para isolamento

        Returns:
            True se removido com sucesso
        """
        secret_key = self._get_secret_key(name, tenant_id)

        if secret_key in self._secrets:
            try:
                del self._secrets[secret_key]
                self._save_secrets()
                self._log_access("DELETE", secret_key, True, tenant_id)
                return True
            except Exception as e:
                self._log_access("DELETE", secret_key, False, tenant_id, str(e))
                return False

        self._log_access("DELETE", secret_key, False, tenant_id, "Not found")
        return False

    def list_secrets(
        self,
        tenant_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista secrets.

        Args:
            tenant_id: Filtrar por tenant

        Returns:
            Lista de secrets (apenas metadados, nao valores)
        """
        secrets = []

        for key, data in self._secrets.items():
            # Filtra por tenant se especificado
            secret_tenant = data.get("tenant_id")
            if tenant_id and secret_tenant != tenant_id:
                continue

            secrets.append({
                "name": key,
                "content_type": data.get("content_type"),
                "tenant_id": secret_tenant,
                "created_at": data.get("created_at"),
                "tags": data.get("tags", {})
            })

        return secrets

    # =========================================================================
    # Operacoes em Lote
    # =========================================================================

    def get_secrets_batch(
        self,
        names: List[str],
        tenant_id: Optional[str] = None
    ) -> Dict[str, Optional[str]]:
        """Recupera multiplos secrets"""
        results = {}
        for name in names:
            results[name] = self.get_secret(name, tenant_id)
        return results

    def set_secrets_batch(
        self,
        secrets: Dict[str, str],
        tenant_id: Optional[str] = None
    ) -> Dict[str, bool]:
        """Salva multiplos secrets"""
        results = {}
        for name, value in secrets.items():
            results[name] = self.set_secret(name, value, tenant_id)
        return results

    # =========================================================================
    # Utilitarios
    # =========================================================================

    def rotate_key(self) -> bool:
        """
        Rotaciona a chave de criptografia.

        Descriptografa todos os secrets com a chave antiga,
        gera nova chave, e re-criptografa.

        Returns:
            True se rotacionado com sucesso
        """
        try:
            # Faz backup dos secrets descriptografados
            secrets_backup = self._secrets.copy()

            # Gera nova chave
            new_key = Fernet.generate_key()
            key_path = Path(self.config.key_file)

            with open(key_path, "wb") as f:
                f.write(new_key)
            os.chmod(key_path, 0o600)

            # Atualiza Fernet com nova chave
            self._fernet = Fernet(new_key)

            # Re-salva secrets com nova criptografia
            self._secrets = secrets_backup
            self._save_secrets()

            logger.info("Chave de criptografia rotacionada com sucesso")
            return True
        except Exception as e:
            logger.error(f"Erro ao rotacionar chave: {e}")
            return False

    def export_secrets(
        self,
        tenant_id: Optional[str] = None
    ) -> Dict[str, str]:
        """
        Exporta secrets como dicionario.

        CUIDADO: Retorna valores em texto plano!

        Args:
            tenant_id: Filtrar por tenant

        Returns:
            Dict com nome -> valor
        """
        result = {}
        for key, data in self._secrets.items():
            if tenant_id:
                if data.get("tenant_id") != tenant_id:
                    continue
            result[key] = data.get("value", "")
        return result

    def get_audit_log(
        self,
        tenant_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Retorna log de auditoria"""
        logs = self._audit_log
        if tenant_id:
            logs = [l for l in logs if l.get("tenant_id") == tenant_id]
        return logs[-limit:]

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas"""
        return {
            "total_secrets": len(self._secrets),
            "storage_path": str(self._secrets_file),
            "audit_log_size": len(self._audit_log),
            "storage_type": "local_encrypted"
        }
