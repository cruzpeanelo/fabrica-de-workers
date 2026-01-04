"""
GitHub Skill - Plataforma E
Integracao com GitHub para versionamento de projetos
"""
import os
import json
import subprocess
from pathlib import Path
from typing import Optional, Dict, List
from datetime import datetime

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))


class GitHubSkill:
    """Skill para integracao com GitHub"""

    def __init__(self, token: str = None):
        """
        Inicializa a skill do GitHub

        Args:
            token: GitHub Personal Access Token. Se nao fornecido,
                   busca em GITHUB_TOKEN env var ou .github_token
        """
        self.token = token or self._load_token()
        self.api_base = "https://api.github.com"

    def _load_token(self) -> Optional[str]:
        """Carrega token do ambiente ou arquivo"""
        # Tenta variavel de ambiente
        token = os.environ.get("GITHUB_TOKEN")
        if token:
            return token

        # Tenta arquivo local
        token_file = Path(__file__).parent.parent.parent / ".github_token"
        if token_file.exists():
            return token_file.read_text().strip()

        return None

    def _run_command(self, cmd: List[str], cwd: str = None) -> Dict:
        """Executa comando git"""
        try:
            result = subprocess.run(
                cmd,
                cwd=cwd,
                capture_output=True,
                text=True,
                timeout=60
            )
            return {
                "success": result.returncode == 0,
                "output": result.stdout,
                "error": result.stderr
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    def _api_request(self, method: str, endpoint: str, data: dict = None) -> Dict:
        """Faz requisicao para API do GitHub"""
        import urllib.request
        import urllib.error

        url = f"{self.api_base}{endpoint}"
        headers = {
            "Authorization": f"token {self.token}",
            "Accept": "application/vnd.github.v3+json",
            "Content-Type": "application/json"
        }

        try:
            req = urllib.request.Request(url, method=method, headers=headers)
            if data:
                req.data = json.dumps(data).encode()

            with urllib.request.urlopen(req, timeout=30) as response:
                return {
                    "success": True,
                    "data": json.loads(response.read().decode()),
                    "status": response.status
                }
        except urllib.error.HTTPError as e:
            return {
                "success": False,
                "error": e.reason,
                "status": e.code,
                "data": json.loads(e.read().decode()) if e.read() else {}
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    # =========================================================================
    # REPOSITORY OPERATIONS
    # =========================================================================

    def create_repo(self, name: str, description: str = "", private: bool = False) -> Dict:
        """
        Cria novo repositorio no GitHub

        Args:
            name: Nome do repositorio
            description: Descricao do repositorio
            private: Se True, cria repositorio privado
        """
        return self._api_request("POST", "/user/repos", {
            "name": name,
            "description": description,
            "private": private,
            "auto_init": False
        })

    def get_repo(self, owner: str, repo: str) -> Dict:
        """Busca informacoes do repositorio"""
        return self._api_request("GET", f"/repos/{owner}/{repo}")

    def list_repos(self, per_page: int = 30) -> Dict:
        """Lista repositorios do usuario"""
        return self._api_request("GET", f"/user/repos?per_page={per_page}&sort=updated")

    # =========================================================================
    # GIT OPERATIONS
    # =========================================================================

    def init_repo(self, path: str, remote_url: str = None) -> Dict:
        """
        Inicializa repositorio Git local

        Args:
            path: Caminho do diretorio
            remote_url: URL do repositorio remoto (opcional)
        """
        results = []

        # git init
        result = self._run_command(["git", "init"], cwd=path)
        results.append(("init", result))

        if remote_url:
            # Configura remote
            result = self._run_command(
                ["git", "remote", "add", "origin", remote_url],
                cwd=path
            )
            results.append(("remote", result))

        return {
            "success": all(r[1]["success"] for r in results),
            "steps": results
        }

    def clone_repo(self, repo_url: str, dest_path: str) -> Dict:
        """Clona repositorio"""
        # Adiciona token na URL para autenticacao
        if self.token and "github.com" in repo_url:
            repo_url = repo_url.replace(
                "https://github.com",
                f"https://{self.token}@github.com"
            )

        return self._run_command(["git", "clone", repo_url, dest_path])

    def commit_and_push(
        self,
        path: str,
        message: str,
        branch: str = "main",
        add_all: bool = True
    ) -> Dict:
        """
        Faz commit e push das alteracoes

        Args:
            path: Caminho do repositorio
            message: Mensagem do commit
            branch: Branch para push
            add_all: Se True, adiciona todos os arquivos
        """
        results = []

        # git add
        if add_all:
            result = self._run_command(["git", "add", "-A"], cwd=path)
            results.append(("add", result))

        # git commit
        result = self._run_command(
            ["git", "commit", "-m", message],
            cwd=path
        )
        results.append(("commit", result))

        # git push
        result = self._run_command(
            ["git", "push", "-u", "origin", branch],
            cwd=path
        )
        results.append(("push", result))

        return {
            "success": all(r[1]["success"] for r in results),
            "steps": results
        }

    def pull(self, path: str, branch: str = "main") -> Dict:
        """Faz pull do repositorio"""
        return self._run_command(["git", "pull", "origin", branch], cwd=path)

    def status(self, path: str) -> Dict:
        """Retorna status do repositorio"""
        return self._run_command(["git", "status", "--porcelain"], cwd=path)

    def get_remote_url(self, path: str) -> Optional[str]:
        """Retorna URL do remote origin"""
        result = self._run_command(
            ["git", "remote", "get-url", "origin"],
            cwd=path
        )
        if result["success"]:
            return result["output"].strip()
        return None

    def set_remote_url(self, path: str, url: str) -> Dict:
        """Define URL do remote origin"""
        # Verifica se remote existe
        check = self._run_command(["git", "remote", "get-url", "origin"], cwd=path)

        if check["success"]:
            # Atualiza remote existente
            return self._run_command(
                ["git", "remote", "set-url", "origin", url],
                cwd=path
            )
        else:
            # Adiciona novo remote
            return self._run_command(
                ["git", "remote", "add", "origin", url],
                cwd=path
            )

    # =========================================================================
    # PROJECT INTEGRATION
    # =========================================================================

    def setup_project_repo(
        self,
        project_path: str,
        repo_name: str,
        description: str = "",
        private: bool = False
    ) -> Dict:
        """
        Configura repositorio completo para um projeto

        Args:
            project_path: Caminho do projeto
            repo_name: Nome do repositorio
            description: Descricao
            private: Se privado
        """
        results = {}

        # 1. Cria repositorio no GitHub
        create_result = self.create_repo(repo_name, description, private)
        results["create_repo"] = create_result

        if not create_result["success"]:
            # Se falhou por ja existir, continua
            if create_result.get("status") != 422:
                return {"success": False, "error": "Falha ao criar repo", "results": results}

        # 2. Inicializa git local
        init_result = self.init_repo(project_path)
        results["init"] = init_result

        # 3. Configura remote
        # Busca username
        user_result = self._api_request("GET", "/user")
        if user_result["success"]:
            username = user_result["data"]["login"]
            remote_url = f"https://github.com/{username}/{repo_name}.git"

            set_remote = self.set_remote_url(project_path, remote_url)
            results["set_remote"] = set_remote

        return {
            "success": True,
            "results": results,
            "message": f"Projeto configurado. Use 'git push -u origin main' para enviar."
        }


# =========================================================================
# HELPER FUNCTIONS
# =========================================================================

def get_github_skill(token: str = None) -> GitHubSkill:
    """Retorna instancia da skill GitHub"""
    return GitHubSkill(token)


def save_github_token(token: str):
    """Salva token de forma segura"""
    token_file = Path(__file__).parent.parent.parent / ".github_token"
    token_file.write_text(token)
    print(f"Token salvo em: {token_file}")


if __name__ == "__main__":
    # Teste basico
    skill = GitHubSkill()
    if skill.token:
        result = skill._api_request("GET", "/user")
        if result["success"]:
            print(f"Conectado como: {result['data']['login']}")
        else:
            print(f"Erro: {result.get('error')}")
    else:
        print("Token nao configurado")
