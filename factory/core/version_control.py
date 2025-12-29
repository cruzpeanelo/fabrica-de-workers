# -*- coding: utf-8 -*-
"""
Version Control - Fabrica de Agentes
====================================
Sistema completo de versionamento de codigo com:
- Snapshots automaticos em cada geracao
- Diff visual entre versoes
- Rollback para versoes anteriores
- Branch/merge para experimentos

Issue #58: [Feature] Versionamento e Historico de Codigo
"""
import os
import hashlib
import difflib
import shutil
from datetime import datetime
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass, field
import json


@dataclass
class FileDiff:
    """Representa o diff de um arquivo entre duas versoes"""
    file_path: str
    old_content: Optional[str] = None
    new_content: Optional[str] = None
    diff_lines: List[str] = field(default_factory=list)
    status: str = "modified"  # added, modified, deleted

    def to_dict(self) -> dict:
        return {
            "file_path": self.file_path,
            "status": self.status,
            "diff_lines": self.diff_lines,
            "additions": len([l for l in self.diff_lines if l.startswith('+') and not l.startswith('+++')]),
            "deletions": len([l for l in self.diff_lines if l.startswith('-') and not l.startswith('---')])
        }


@dataclass
class VersionSnapshot:
    """Representa um snapshot de versao do codigo"""
    version_hash: str
    story_id: str
    task_id: Optional[str] = None
    message: str = ""
    author: str = "system"
    files: Dict[str, str] = field(default_factory=dict)  # path -> content
    file_hashes: Dict[str, str] = field(default_factory=dict)  # path -> hash
    parent_hash: Optional[str] = None
    branch: str = "main"
    created_at: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict:
        return {
            "version_hash": self.version_hash,
            "story_id": self.story_id,
            "task_id": self.task_id,
            "message": self.message,
            "author": self.author,
            "files_count": len(self.files),
            "file_hashes": self.file_hashes,
            "parent_hash": self.parent_hash,
            "branch": self.branch,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "metadata": self.metadata
        }


class VersionControl:
    """
    Sistema de Versionamento de Codigo

    Funcionalidades:
    - Criar snapshots automaticos do codigo gerado
    - Calcular diff entre versoes
    - Rollback para versoes anteriores
    - Suporte a branches para experimentos
    """

    def __init__(self, db_session=None, project_path: str = None):
        """
        Inicializa o controle de versao

        Args:
            db_session: Sessao do SQLAlchemy para persistencia
            project_path: Caminho base dos projetos
        """
        self.db = db_session
        self.project_path = Path(project_path) if project_path else Path("projects")
        self._snapshots_cache: Dict[str, VersionSnapshot] = {}

    def create_snapshot(
        self,
        story_id: str,
        files: Dict[str, str],
        message: str = "",
        task_id: str = None,
        author: str = "worker",
        branch: str = "main",
        metadata: Dict[str, Any] = None
    ) -> VersionSnapshot:
        """
        Cria um novo snapshot do codigo

        Args:
            story_id: ID da story associada
            files: Dicionario de arquivos {path: content}
            message: Mensagem descritiva da versao
            task_id: ID da task (opcional)
            author: Autor do snapshot (agente/worker)
            branch: Nome da branch
            metadata: Metadados adicionais

        Returns:
            VersionSnapshot criado
        """
        # Calcular hashes de cada arquivo
        file_hashes = {}
        for file_path, content in files.items():
            file_hashes[file_path] = self._hash_content(content)

        # Calcular hash unico da versao (baseado nos hashes dos arquivos + timestamp)
        version_data = json.dumps({
            "files": file_hashes,
            "timestamp": datetime.utcnow().isoformat(),
            "story_id": story_id
        }, sort_keys=True)
        version_hash = hashlib.sha256(version_data.encode()).hexdigest()[:12]

        # Buscar snapshot anterior para definir parent
        parent_hash = self._get_latest_hash(story_id, branch)

        # Criar snapshot
        snapshot = VersionSnapshot(
            version_hash=version_hash,
            story_id=story_id,
            task_id=task_id,
            message=message or f"Snapshot automatico - {datetime.utcnow().strftime('%Y-%m-%d %H:%M')}",
            author=author,
            files=files.copy(),
            file_hashes=file_hashes,
            parent_hash=parent_hash,
            branch=branch,
            created_at=datetime.utcnow(),
            metadata=metadata or {}
        )

        # Salvar no banco de dados
        if self.db:
            self._save_to_db(snapshot)

        # Cache
        self._snapshots_cache[version_hash] = snapshot

        return snapshot

    def get_snapshot(self, version_hash: str) -> Optional[VersionSnapshot]:
        """
        Recupera um snapshot pelo hash

        Args:
            version_hash: Hash unico da versao

        Returns:
            VersionSnapshot ou None
        """
        # Verificar cache
        if version_hash in self._snapshots_cache:
            return self._snapshots_cache[version_hash]

        # Buscar no banco
        if self.db:
            return self._load_from_db(version_hash)

        return None

    def get_versions(
        self,
        story_id: str,
        branch: str = None,
        limit: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Lista versoes de uma story

        Args:
            story_id: ID da story
            branch: Filtrar por branch (opcional)
            limit: Limite de resultados

        Returns:
            Lista de versoes (sem conteudo dos arquivos)
        """
        if not self.db:
            return []

        try:
            from factory.database.models import CodeVersion

            query = self.db.query(CodeVersion).filter(
                CodeVersion.story_id == story_id
            )

            if branch:
                query = query.filter(CodeVersion.branch == branch)

            versions = query.order_by(CodeVersion.created_at.desc()).limit(limit).all()

            return [v.to_dict() for v in versions]
        except Exception as e:
            print(f"[VersionControl] Erro ao listar versoes: {e}")
            return []

    def get_diff(
        self,
        hash_from: str,
        hash_to: str,
        context_lines: int = 3
    ) -> Dict[str, Any]:
        """
        Calcula diff entre duas versoes

        Args:
            hash_from: Hash da versao anterior
            hash_to: Hash da versao atual
            context_lines: Linhas de contexto no diff

        Returns:
            Dicionario com diff de cada arquivo
        """
        snapshot_from = self.get_snapshot(hash_from)
        snapshot_to = self.get_snapshot(hash_to)

        if not snapshot_from or not snapshot_to:
            return {"error": "Versao nao encontrada", "files": []}

        diffs = []
        all_files = set(snapshot_from.files.keys()) | set(snapshot_to.files.keys())

        for file_path in all_files:
            old_content = snapshot_from.files.get(file_path, "")
            new_content = snapshot_to.files.get(file_path, "")

            if file_path not in snapshot_from.files:
                status = "added"
            elif file_path not in snapshot_to.files:
                status = "deleted"
            elif old_content != new_content:
                status = "modified"
            else:
                continue  # Sem mudancas

            # Gerar diff unificado
            diff_lines = list(difflib.unified_diff(
                old_content.splitlines(keepends=True),
                new_content.splitlines(keepends=True),
                fromfile=f"a/{file_path}",
                tofile=f"b/{file_path}",
                n=context_lines
            ))

            file_diff = FileDiff(
                file_path=file_path,
                old_content=old_content,
                new_content=new_content,
                diff_lines=[l.rstrip('\n') for l in diff_lines],
                status=status
            )

            diffs.append(file_diff.to_dict())

        return {
            "from_version": snapshot_from.to_dict(),
            "to_version": snapshot_to.to_dict(),
            "files": diffs,
            "total_additions": sum(f["additions"] for f in diffs),
            "total_deletions": sum(f["deletions"] for f in diffs),
            "files_changed": len(diffs)
        }

    def get_file_diff(
        self,
        hash_from: str,
        hash_to: str,
        file_path: str,
        context_lines: int = 5
    ) -> Dict[str, Any]:
        """
        Calcula diff de um arquivo especifico entre versoes

        Args:
            hash_from: Hash da versao anterior
            hash_to: Hash da versao atual
            file_path: Caminho do arquivo
            context_lines: Linhas de contexto

        Returns:
            Diff do arquivo com contexto
        """
        snapshot_from = self.get_snapshot(hash_from)
        snapshot_to = self.get_snapshot(hash_to)

        if not snapshot_from or not snapshot_to:
            return {"error": "Versao nao encontrada"}

        old_content = snapshot_from.files.get(file_path, "")
        new_content = snapshot_to.files.get(file_path, "")

        # Gerar diff com mais contexto
        diff_lines = list(difflib.unified_diff(
            old_content.splitlines(keepends=True),
            new_content.splitlines(keepends=True),
            fromfile=f"a/{file_path} ({hash_from})",
            tofile=f"b/{file_path} ({hash_to})",
            n=context_lines
        ))

        # Gerar side-by-side diff
        differ = difflib.Differ()
        side_by_side = list(differ.compare(
            old_content.splitlines(),
            new_content.splitlines()
        ))

        return {
            "file_path": file_path,
            "from_version": hash_from,
            "to_version": hash_to,
            "old_content": old_content,
            "new_content": new_content,
            "unified_diff": [l.rstrip('\n') for l in diff_lines],
            "side_by_side": side_by_side,
            "additions": len([l for l in diff_lines if l.startswith('+') and not l.startswith('+++')]),
            "deletions": len([l for l in diff_lines if l.startswith('-') and not l.startswith('---')])
        }

    def rollback(
        self,
        story_id: str,
        target_hash: str,
        create_backup: bool = True
    ) -> Dict[str, Any]:
        """
        Reverte o codigo para uma versao anterior

        Args:
            story_id: ID da story
            target_hash: Hash da versao alvo
            create_backup: Se True, cria snapshot antes de reverter

        Returns:
            Resultado da operacao com arquivos restaurados
        """
        target_snapshot = self.get_snapshot(target_hash)

        if not target_snapshot:
            return {
                "success": False,
                "error": "Versao alvo nao encontrada",
                "version_hash": target_hash
            }

        if target_snapshot.story_id != story_id:
            return {
                "success": False,
                "error": "Versao pertence a outra story",
                "version_hash": target_hash
            }

        # Criar backup do estado atual antes do rollback
        if create_backup:
            current_hash = self._get_latest_hash(story_id)
            if current_hash:
                current_snapshot = self.get_snapshot(current_hash)
                if current_snapshot:
                    self.create_snapshot(
                        story_id=story_id,
                        files=current_snapshot.files,
                        message=f"Backup antes de rollback para {target_hash}",
                        author="system",
                        branch=f"backup/{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}",
                        metadata={"rollback_target": target_hash}
                    )

        # Restaurar arquivos no sistema de arquivos
        restored_files = []
        project_dir = self.project_path / story_id

        for file_path, content in target_snapshot.files.items():
            full_path = project_dir / file_path
            try:
                full_path.parent.mkdir(parents=True, exist_ok=True)
                full_path.write_text(content, encoding='utf-8')
                restored_files.append(file_path)
            except Exception as e:
                print(f"[VersionControl] Erro ao restaurar {file_path}: {e}")

        # Criar novo snapshot para o rollback
        rollback_snapshot = self.create_snapshot(
            story_id=story_id,
            files=target_snapshot.files,
            message=f"Rollback para versao {target_hash}",
            author="system",
            metadata={
                "rollback_from": self._get_latest_hash(story_id),
                "rollback_to": target_hash
            }
        )

        return {
            "success": True,
            "message": f"Rollback realizado para versao {target_hash}",
            "target_version": target_snapshot.to_dict(),
            "new_version": rollback_snapshot.to_dict(),
            "restored_files": restored_files,
            "files_count": len(restored_files)
        }

    def preview_rollback(
        self,
        story_id: str,
        target_hash: str
    ) -> Dict[str, Any]:
        """
        Pre-visualiza as mudancas de um rollback sem aplica-lo

        Args:
            story_id: ID da story
            target_hash: Hash da versao alvo

        Returns:
            Preview das mudancas que serao aplicadas
        """
        target_snapshot = self.get_snapshot(target_hash)
        current_hash = self._get_latest_hash(story_id)

        if not target_snapshot:
            return {"error": "Versao alvo nao encontrada"}

        if not current_hash:
            return {"error": "Nenhuma versao atual encontrada"}

        # Calcular diff entre atual e alvo
        diff = self.get_diff(current_hash, target_hash)

        return {
            "current_version": current_hash,
            "target_version": target_hash,
            "changes": diff,
            "will_restore": list(target_snapshot.files.keys()),
            "files_affected": diff.get("files_changed", 0)
        }

    def create_branch(
        self,
        story_id: str,
        branch_name: str,
        from_hash: str = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova branch para experimentos

        Args:
            story_id: ID da story
            branch_name: Nome da nova branch
            from_hash: Hash base (default: ultima versao da main)

        Returns:
            Informacoes da branch criada
        """
        if not from_hash:
            from_hash = self._get_latest_hash(story_id, "main")

        if not from_hash:
            return {"error": "Nenhuma versao base encontrada"}

        base_snapshot = self.get_snapshot(from_hash)
        if not base_snapshot:
            return {"error": "Versao base nao encontrada"}

        # Criar snapshot inicial da branch
        branch_snapshot = self.create_snapshot(
            story_id=story_id,
            files=base_snapshot.files,
            message=f"Branch criada a partir de {from_hash}",
            author="system",
            branch=branch_name,
            metadata={"branched_from": from_hash}
        )

        # Salvar branch no banco
        if self.db:
            self._save_branch_to_db(story_id, branch_name, from_hash)

        return {
            "success": True,
            "branch_name": branch_name,
            "base_hash": from_hash,
            "branch_hash": branch_snapshot.version_hash,
            "files_count": len(branch_snapshot.files)
        }

    def list_branches(self, story_id: str) -> List[Dict[str, Any]]:
        """
        Lista todas as branches de uma story

        Args:
            story_id: ID da story

        Returns:
            Lista de branches com metadados
        """
        if not self.db:
            return []

        try:
            from factory.database.models import CodeBranch

            branches = self.db.query(CodeBranch).filter(
                CodeBranch.story_id == story_id
            ).all()

            return [b.to_dict() for b in branches]
        except Exception as e:
            print(f"[VersionControl] Erro ao listar branches: {e}")
            return [{"branch_name": "main", "is_default": True}]

    def merge_branch(
        self,
        story_id: str,
        source_branch: str,
        target_branch: str = "main"
    ) -> Dict[str, Any]:
        """
        Merge uma branch na branch alvo

        Args:
            story_id: ID da story
            source_branch: Branch de origem
            target_branch: Branch de destino

        Returns:
            Resultado do merge
        """
        source_hash = self._get_latest_hash(story_id, source_branch)
        target_hash = self._get_latest_hash(story_id, target_branch)

        if not source_hash or not target_hash:
            return {"error": "Branches nao encontradas"}

        source_snapshot = self.get_snapshot(source_hash)
        target_snapshot = self.get_snapshot(target_hash)

        if not source_snapshot or not target_snapshot:
            return {"error": "Snapshots nao encontrados"}

        # Merge simples: usa arquivos da source (substitui na target)
        # TODO: Implementar merge com resolucao de conflitos
        merged_files = target_snapshot.files.copy()
        merged_files.update(source_snapshot.files)

        # Detectar conflitos (arquivos modificados em ambas)
        conflicts = []
        for file_path in set(source_snapshot.files.keys()) & set(target_snapshot.files.keys()):
            source_content = source_snapshot.files.get(file_path, "")
            target_content = target_snapshot.files.get(file_path, "")
            if source_content != target_content:
                # Usar versao da source (pode-se implementar merge inteligente aqui)
                conflicts.append({
                    "file": file_path,
                    "resolution": "using_source"
                })

        # Criar snapshot de merge
        merge_snapshot = self.create_snapshot(
            story_id=story_id,
            files=merged_files,
            message=f"Merge de {source_branch} em {target_branch}",
            author="system",
            branch=target_branch,
            metadata={
                "merge_from": source_branch,
                "merge_to": target_branch,
                "source_hash": source_hash,
                "target_hash": target_hash,
                "conflicts": conflicts
            }
        )

        return {
            "success": True,
            "merged_hash": merge_snapshot.version_hash,
            "source_branch": source_branch,
            "target_branch": target_branch,
            "files_merged": len(merged_files),
            "conflicts_resolved": conflicts
        }

    def compare_branches(
        self,
        story_id: str,
        branch_a: str,
        branch_b: str
    ) -> Dict[str, Any]:
        """
        Compara duas branches lado a lado

        Args:
            story_id: ID da story
            branch_a: Primeira branch
            branch_b: Segunda branch

        Returns:
            Comparacao das branches
        """
        hash_a = self._get_latest_hash(story_id, branch_a)
        hash_b = self._get_latest_hash(story_id, branch_b)

        if not hash_a or not hash_b:
            return {"error": "Branches nao encontradas"}

        return {
            "branch_a": branch_a,
            "branch_b": branch_b,
            "diff": self.get_diff(hash_a, hash_b)
        }

    def get_timeline(
        self,
        story_id: str,
        include_branches: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Retorna timeline visual das versoes

        Args:
            story_id: ID da story
            include_branches: Incluir todas as branches

        Returns:
            Timeline com versoes e branches
        """
        versions = self.get_versions(story_id, limit=100)

        if not include_branches:
            versions = [v for v in versions if v.get("branch") == "main"]

        # Organizar em timeline
        timeline = []
        branches_seen = set()

        for version in versions:
            branch = version.get("branch", "main")

            if branch not in branches_seen:
                branches_seen.add(branch)

            timeline.append({
                "hash": version.get("version_hash"),
                "message": version.get("message"),
                "author": version.get("author"),
                "branch": branch,
                "created_at": version.get("created_at"),
                "parent_hash": version.get("parent_hash"),
                "is_current": version == versions[0] if versions else False
            })

        return {
            "timeline": timeline,
            "branches": list(branches_seen),
            "total_versions": len(timeline)
        }

    # ==========================================================================
    # Metodos Privados
    # ==========================================================================

    def _hash_content(self, content: str) -> str:
        """Calcula hash SHA256 do conteudo"""
        return hashlib.sha256(content.encode('utf-8')).hexdigest()[:12]

    def _get_latest_hash(self, story_id: str, branch: str = "main") -> Optional[str]:
        """Retorna hash da versao mais recente"""
        if not self.db:
            return None

        try:
            from factory.database.models import CodeVersion

            version = self.db.query(CodeVersion).filter(
                CodeVersion.story_id == story_id,
                CodeVersion.branch == branch
            ).order_by(CodeVersion.created_at.desc()).first()

            return version.version_hash if version else None
        except Exception:
            return None

    def _save_to_db(self, snapshot: VersionSnapshot) -> bool:
        """Salva snapshot no banco de dados"""
        try:
            from factory.database.models import CodeVersion

            version = CodeVersion(
                version_hash=snapshot.version_hash,
                story_id=snapshot.story_id,
                task_id=snapshot.task_id,
                message=snapshot.message,
                author=snapshot.author,
                files_content=snapshot.files,
                file_hashes=snapshot.file_hashes,
                parent_hash=snapshot.parent_hash,
                branch=snapshot.branch,
                metadata=snapshot.metadata,
                created_at=snapshot.created_at
            )

            self.db.add(version)
            self.db.commit()
            return True
        except Exception as e:
            print(f"[VersionControl] Erro ao salvar no DB: {e}")
            self.db.rollback()
            return False

    def _load_from_db(self, version_hash: str) -> Optional[VersionSnapshot]:
        """Carrega snapshot do banco de dados"""
        try:
            from factory.database.models import CodeVersion

            version = self.db.query(CodeVersion).filter(
                CodeVersion.version_hash == version_hash
            ).first()

            if not version:
                return None

            snapshot = VersionSnapshot(
                version_hash=version.version_hash,
                story_id=version.story_id,
                task_id=version.task_id,
                message=version.message,
                author=version.author,
                files=version.files_content or {},
                file_hashes=version.file_hashes or {},
                parent_hash=version.parent_hash,
                branch=version.branch,
                created_at=version.created_at,
                metadata=version.metadata or {}
            )

            # Cache
            self._snapshots_cache[version_hash] = snapshot

            return snapshot
        except Exception as e:
            print(f"[VersionControl] Erro ao carregar do DB: {e}")
            return None

    def _save_branch_to_db(self, story_id: str, branch_name: str, base_hash: str) -> bool:
        """Salva informacao de branch no banco"""
        try:
            from factory.database.models import CodeBranch

            branch = CodeBranch(
                branch_id=f"BR-{hashlib.sha256(f'{story_id}:{branch_name}'.encode()).hexdigest()[:8].upper()}",
                story_id=story_id,
                branch_name=branch_name,
                base_hash=base_hash,
                is_default=branch_name == "main"
            )

            self.db.add(branch)
            self.db.commit()
            return True
        except Exception as e:
            print(f"[VersionControl] Erro ao salvar branch: {e}")
            self.db.rollback()
            return False


# =============================================================================
# Funcao utilitaria para uso no worker
# =============================================================================

def create_auto_snapshot(
    db_session,
    story_id: str,
    task_id: str,
    files: Dict[str, str],
    message: str = None,
    author: str = "worker"
) -> Optional[VersionSnapshot]:
    """
    Cria snapshot automatico durante geracao de codigo

    Uso:
        from factory.core.version_control import create_auto_snapshot

        snapshot = create_auto_snapshot(
            db_session=db,
            story_id="STR-0001",
            task_id="STSK-0001",
            files={"main.py": "print('Hello')"},
            author="AGT-08"
        )
    """
    vc = VersionControl(db_session=db_session)

    return vc.create_snapshot(
        story_id=story_id,
        task_id=task_id,
        files=files,
        message=message or f"Auto-snapshot: {task_id}",
        author=author
    )


if __name__ == "__main__":
    # Teste basico
    vc = VersionControl()

    # Criar snapshots de teste
    snapshot1 = vc.create_snapshot(
        story_id="STR-TEST",
        files={
            "main.py": "def hello():\n    print('Hello')\n",
            "utils.py": "def add(a, b):\n    return a + b\n"
        },
        message="Versao inicial",
        author="test"
    )
    print(f"Snapshot 1: {snapshot1.version_hash}")

    snapshot2 = vc.create_snapshot(
        story_id="STR-TEST",
        files={
            "main.py": "def hello():\n    print('Hello World!')\n",
            "utils.py": "def add(a, b):\n    return a + b\n",
            "config.py": "DEBUG = True\n"
        },
        message="Adicionado config",
        author="test"
    )
    print(f"Snapshot 2: {snapshot2.version_hash}")

    # Testar diff
    diff = vc.get_diff(snapshot1.version_hash, snapshot2.version_hash)
    print(f"\nDiff entre versoes:")
    print(f"  Arquivos alterados: {diff['files_changed']}")
    print(f"  Adicoes: {diff['total_additions']}")
    print(f"  Remocoes: {diff['total_deletions']}")

    for file_diff in diff["files"]:
        print(f"\n  {file_diff['file_path']} ({file_diff['status']}):")
        for line in file_diff["diff_lines"][:10]:
            print(f"    {line}")
