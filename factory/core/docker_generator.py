# -*- coding: utf-8 -*-
"""
Docker Generator - Geracao de arquivos Docker para deploy
==========================================================

Este modulo gera arquivos Docker para deploy facil das aplicacoes
criadas pela Fabrica de Agentes.

Funcionalidades (Issue #77):
- Geracao de Dockerfile otimizado por tipo de projeto (Python/Node.js)
- Geracao de docker-compose.yml com banco de dados
- Suporte a PostgreSQL, MySQL, MongoDB containers
- Comandos para build e run de containers
- Logs do container em tempo real

Uso:
    from factory.core.docker_generator import DockerGenerator, deploy_docker

    # Criar gerador
    generator = DockerGenerator("meu-projeto")

    # Gerar arquivos Docker
    result = generator.generate_docker_files(database="postgresql")

    # Ou deploy completo
    result = deploy_docker("meu-projeto", database="postgresql")
"""

import os
import subprocess
import json
import logging
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime
from enum import Enum

# Configurar logging
logger = logging.getLogger(__name__)


class DatabaseType(Enum):
    """Tipos de banco de dados suportados para Docker."""
    POSTGRESQL = "postgresql"
    MYSQL = "mysql"
    MONGODB = "mongodb"
    SQLITE = "sqlite"
    NONE = "none"


class DockerGenerator:
    """Gera arquivos Docker para deploy de aplicacoes."""

    PROJECTS_DIR = Path(r"C:\Users\lcruz\Fabrica de Agentes\projects")

    def __init__(self, project_id: str):
        """
        Inicializa o gerador Docker.

        Args:
            project_id: ID do projeto
        """
        self.project_id = project_id
        self.project_path = self._find_project_path()
        self.project_type = self._detect_project_type() if self.project_path else "unknown"
        self.docker_image_name = f"fabrica-{project_id.lower().replace('-', '_')}"
        self.docker_container_name = f"fabrica-{project_id.lower().replace('-', '_')}-app"

    def _find_project_path(self) -> Optional[Path]:
        """Encontra o caminho do projeto."""
        possible_names = [
            self.project_id,
            self.project_id.lower(),
            self.project_id.lower().replace("-", "_"),
            self.project_id.replace("-", "_"),
        ]

        for name in possible_names:
            path = self.PROJECTS_DIR / name
            if path.exists():
                return path

        return None

    def _detect_project_type(self) -> str:
        """Detecta o tipo de projeto baseado nos arquivos."""
        if not self.project_path:
            return "unknown"

        # Verificar arquivos Python
        py_files = list(self.project_path.rglob("*.py"))
        if py_files:
            return "python"

        # Verificar arquivos Node.js
        if (self.project_path / "package.json").exists():
            return "nodejs"

        js_files = list(self.project_path.rglob("*.js"))
        ts_files = list(self.project_path.rglob("*.ts"))
        if js_files or ts_files:
            return "nodejs"

        return "unknown"

    def generate_docker_files(
        self,
        database: str = "postgresql",
        include_nginx: bool = False
    ) -> Dict:
        """
        Gera arquivos Docker para deploy da aplicacao.

        Args:
            database: Tipo de banco de dados (postgresql, mysql, mongodb, sqlite, none)
            include_nginx: Se deve incluir configuracao Nginx como proxy reverso

        Returns:
            Dicionario com resultado da geracao
        """
        if not self.project_path or not self.project_path.exists():
            return {
                "success": False,
                "message": "Projeto nao encontrado"
            }

        files_created = []

        try:
            # 1. Gerar Dockerfile
            dockerfile_content = self._generate_dockerfile()
            dockerfile_path = self.project_path / "Dockerfile"
            dockerfile_path.write_text(dockerfile_content, encoding="utf-8")
            files_created.append(str(dockerfile_path))

            # 2. Gerar .dockerignore
            dockerignore_content = self._generate_dockerignore()
            dockerignore_path = self.project_path / ".dockerignore"
            dockerignore_path.write_text(dockerignore_content, encoding="utf-8")
            files_created.append(str(dockerignore_path))

            # 3. Gerar docker-compose.yml
            db_type = DatabaseType(database) if database in [e.value for e in DatabaseType] else DatabaseType.POSTGRESQL
            compose_content = self._generate_docker_compose(db_type, include_nginx)
            compose_path = self.project_path / "docker-compose.yml"
            compose_path.write_text(compose_content, encoding="utf-8")
            files_created.append(str(compose_path))

            # 4. Gerar arquivo .env.docker com variaveis de ambiente
            env_content = self._generate_docker_env(db_type)
            env_path = self.project_path / ".env.docker"
            env_path.write_text(env_content, encoding="utf-8")
            files_created.append(str(env_path))

            # 5. Gerar scripts de conveniencia
            scripts = self._generate_docker_scripts()
            for script_name, script_content in scripts.items():
                script_path = self.project_path / script_name
                script_path.write_text(script_content, encoding="utf-8")
                files_created.append(str(script_path))

            # 6. Gerar nginx.conf se solicitado
            if include_nginx:
                nginx_content = self._generate_nginx_config()
                nginx_path = self.project_path / "nginx.conf"
                nginx_path.write_text(nginx_content, encoding="utf-8")
                files_created.append(str(nginx_path))

            return {
                "success": True,
                "message": "Arquivos Docker gerados com sucesso!",
                "files_created": files_created,
                "project_type": self.project_type,
                "database": database,
                "commands": {
                    "build": f"docker build -t {self.docker_image_name} .",
                    "run": "docker-compose up -d",
                    "logs": "docker-compose logs -f",
                    "stop": "docker-compose down"
                }
            }

        except Exception as e:
            logger.error(f"Erro ao gerar arquivos Docker: {e}")
            return {
                "success": False,
                "message": f"Erro ao gerar arquivos Docker: {str(e)}"
            }

    def _generate_dockerfile(self) -> str:
        """Gera Dockerfile otimizado baseado no tipo de projeto."""
        if self.project_type == "python":
            return self._generate_python_dockerfile()
        elif self.project_type == "nodejs":
            return self._generate_nodejs_dockerfile()
        else:
            return self._generate_python_dockerfile()

    def _generate_python_dockerfile(self) -> str:
        """Gera Dockerfile otimizado para projetos Python/FastAPI."""
        return f'''# Dockerfile para {self.project_id}
# Gerado automaticamente pela Fabrica de Agentes
# Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}

# ============================================================
# STAGE 1: Base image
# ============================================================
FROM python:3.11-slim as base

# Variaveis de ambiente para Python
ENV PYTHONDONTWRITEBYTECODE=1 \\
    PYTHONUNBUFFERED=1 \\
    PYTHONFAULTHANDLER=1 \\
    PIP_NO_CACHE_DIR=1 \\
    PIP_DISABLE_PIP_VERSION_CHECK=1

WORKDIR /app

# ============================================================
# STAGE 2: Builder - instala dependencias
# ============================================================
FROM base as builder

# Instalar dependencias do sistema para compilacao
RUN apt-get update && apt-get install -y --no-install-recommends \\
    build-essential \\
    libpq-dev \\
    && rm -rf /var/lib/apt/lists/*

# Criar virtualenv
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Copiar e instalar dependencias
COPY requirements.txt .
RUN pip install --upgrade pip && \\
    pip install -r requirements.txt

# ============================================================
# STAGE 3: Runtime - imagem final otimizada
# ============================================================
FROM base as runtime

# Instalar dependencias de runtime apenas
RUN apt-get update && apt-get install -y --no-install-recommends \\
    libpq5 \\
    curl \\
    && rm -rf /var/lib/apt/lists/* \\
    && apt-get clean

# Copiar virtualenv do builder
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Criar usuario nao-root para seguranca
RUN groupadd -r appgroup && useradd -r -g appgroup appuser

# Copiar codigo da aplicacao
COPY --chown=appuser:appgroup . .

# Trocar para usuario nao-root
USER appuser

# Expor porta da aplicacao
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \\
    CMD curl -f http://localhost:8000/health || exit 1

# Comando de inicializacao
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
'''

    def _generate_nodejs_dockerfile(self) -> str:
        """Gera Dockerfile otimizado para projetos Node.js/Express."""
        return f'''# Dockerfile para {self.project_id}
# Gerado automaticamente pela Fabrica de Agentes
# Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}

# ============================================================
# STAGE 1: Base image
# ============================================================
FROM node:20-alpine as base

WORKDIR /app

# ============================================================
# STAGE 2: Builder - instala dependencias
# ============================================================
FROM base as builder

# Copiar arquivos de dependencias
COPY package*.json ./

# Instalar todas as dependencias (incluindo devDependencies para build)
RUN npm ci

# Copiar codigo fonte
COPY . .

# Build (se houver script de build)
RUN npm run build --if-present

# Remover devDependencies
RUN npm prune --production

# ============================================================
# STAGE 3: Runtime - imagem final otimizada
# ============================================================
FROM base as runtime

# Instalar dumb-init para gerenciamento de processos
RUN apk add --no-cache dumb-init curl

# Criar usuario nao-root
RUN addgroup -g 1001 -S appgroup && \\
    adduser -S appuser -u 1001 -G appgroup

WORKDIR /app

# Copiar node_modules e codigo do builder
COPY --from=builder --chown=appuser:appgroup /app/node_modules ./node_modules
COPY --from=builder --chown=appuser:appgroup /app .

# Trocar para usuario nao-root
USER appuser

# Expor porta
EXPOSE 3000

# Variaveis de ambiente
ENV NODE_ENV=production \\
    PORT=3000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \\
    CMD curl -f http://localhost:3000/health || exit 1

# Comando de inicializacao com dumb-init
ENTRYPOINT ["dumb-init", "--"]
CMD ["node", "server.js"]
'''

    def _generate_dockerignore(self) -> str:
        """Gera arquivo .dockerignore."""
        return '''# .dockerignore - Gerado pela Fabrica de Agentes

# Git
.git
.gitignore

# Python
__pycache__
*.py[cod]
*$py.class
*.so
.Python
env/
venv/
.venv/
ENV/
.eggs/
*.egg-info/
*.egg

# Node.js
node_modules/
npm-debug.log*
yarn-debug.log*
yarn-error.log*

# IDE
.idea/
.vscode/
*.swp
*.swo
*~

# Docker
Dockerfile*
docker-compose*
.docker/

# Testes
.pytest_cache/
.coverage
htmlcov/
.tox/
coverage.xml
*.cover

# Docs
docs/
*.md
!README.md

# Arquivos temporarios
*.log
*.tmp
.DS_Store
Thumbs.db

# Arquivos de ambiente (seguranca)
.env
.env.local
.env.*.local
*.pem
*.key

# Outros
*.bak
*.backup
'''

    def _generate_docker_compose(
        self,
        database: DatabaseType,
        include_nginx: bool = False
    ) -> str:
        """Gera docker-compose.yml com servicos configurados."""
        app_port = "8000" if self.project_type == "python" else "3000"

        # Servico principal da aplicacao
        app_service = f'''  app:
    build:
      context: .
      dockerfile: Dockerfile
    container_name: {self.docker_container_name}
    restart: unless-stopped
    ports:
      - "{app_port}:{app_port}"
    environment:
      - NODE_ENV=production
      - DATABASE_URL=${{DATABASE_URL}}
    env_file:
      - .env.docker
    volumes:
      - ./logs:/app/logs
    networks:
      - app-network
    depends_on:'''

        # Adicionar dependencias do banco
        db_depends = ""
        db_service = ""
        db_volumes = ""

        if database == DatabaseType.POSTGRESQL:
            db_depends = '''
      db:
        condition: service_healthy'''
            db_service = f'''
  db:
    image: postgres:15-alpine
    container_name: {self.docker_container_name}-db
    restart: unless-stopped
    environment:
      POSTGRES_USER: ${{POSTGRES_USER:-appuser}}
      POSTGRES_PASSWORD: ${{POSTGRES_PASSWORD:-apppassword}}
      POSTGRES_DB: ${{POSTGRES_DB:-appdb}}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
    networks:
      - app-network
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${{POSTGRES_USER:-appuser}}"]
      interval: 10s
      timeout: 5s
      retries: 5'''
            db_volumes = '''
  postgres_data:
    driver: local'''

        elif database == DatabaseType.MYSQL:
            db_depends = '''
      db:
        condition: service_healthy'''
            db_service = f'''
  db:
    image: mysql:8.0
    container_name: {self.docker_container_name}-db
    restart: unless-stopped
    environment:
      MYSQL_ROOT_PASSWORD: ${{MYSQL_ROOT_PASSWORD:-rootpassword}}
      MYSQL_DATABASE: ${{MYSQL_DATABASE:-appdb}}
      MYSQL_USER: ${{MYSQL_USER:-appuser}}
      MYSQL_PASSWORD: ${{MYSQL_PASSWORD:-apppassword}}
    volumes:
      - mysql_data:/var/lib/mysql
    ports:
      - "3306:3306"
    networks:
      - app-network
    healthcheck:
      test: ["CMD", "mysqladmin", "ping", "-h", "localhost"]
      interval: 10s
      timeout: 5s
      retries: 5'''
            db_volumes = '''
  mysql_data:
    driver: local'''

        elif database == DatabaseType.MONGODB:
            db_depends = '''
      db:
        condition: service_started'''
            db_service = f'''
  db:
    image: mongo:7.0
    container_name: {self.docker_container_name}-db
    restart: unless-stopped
    environment:
      MONGO_INITDB_ROOT_USERNAME: ${{MONGO_USER:-appuser}}
      MONGO_INITDB_ROOT_PASSWORD: ${{MONGO_PASSWORD:-apppassword}}
      MONGO_INITDB_DATABASE: ${{MONGO_DB:-appdb}}
    volumes:
      - mongo_data:/data/db
    ports:
      - "27017:27017"
    networks:
      - app-network'''
            db_volumes = '''
  mongo_data:
    driver: local'''

        elif database == DatabaseType.NONE or database == DatabaseType.SQLITE:
            db_depends = " []"

        # Servico Nginx (opcional)
        nginx_service = ""
        if include_nginx:
            nginx_service = f'''
  nginx:
    image: nginx:alpine
    container_name: {self.docker_container_name}-nginx
    restart: unless-stopped
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/nginx/ssl:ro
    networks:
      - app-network
    depends_on:
      - app'''

        compose_content = f'''# docker-compose.yml para {self.project_id}
# Gerado automaticamente pela Fabrica de Agentes
# Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}
#
# Uso:
#   docker-compose up -d       # Iniciar em background
#   docker-compose logs -f     # Ver logs
#   docker-compose down        # Parar containers

version: "3.8"

services:
{app_service}{db_depends}
{db_service}{nginx_service}

networks:
  app-network:
    driver: bridge

volumes:{db_volumes}
  logs:
    driver: local
'''
        return compose_content

    def _generate_docker_env(self, database: DatabaseType) -> str:
        """Gera arquivo .env.docker com variaveis de ambiente."""
        base_env = f'''# Variaveis de ambiente Docker para {self.project_id}
# Gerado automaticamente pela Fabrica de Agentes
# Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}
#
# IMPORTANTE: Altere as senhas padrao antes de usar em producao!

# Aplicacao
APP_NAME={self.project_id}
APP_ENV=production
DEBUG=false
LOG_LEVEL=info

'''

        if database == DatabaseType.POSTGRESQL:
            base_env += '''# PostgreSQL
POSTGRES_USER=appuser
POSTGRES_PASSWORD=apppassword
POSTGRES_DB=appdb
DATABASE_URL=postgresql://appuser:apppassword@db:5432/appdb
'''

        elif database == DatabaseType.MYSQL:
            base_env += '''# MySQL
MYSQL_ROOT_PASSWORD=rootpassword
MYSQL_USER=appuser
MYSQL_PASSWORD=apppassword
MYSQL_DATABASE=appdb
DATABASE_URL=mysql://appuser:apppassword@db:3306/appdb
'''

        elif database == DatabaseType.MONGODB:
            base_env += '''# MongoDB
MONGO_USER=appuser
MONGO_PASSWORD=apppassword
MONGO_DB=appdb
DATABASE_URL=mongodb://appuser:apppassword@db:27017/appdb
'''

        elif database == DatabaseType.SQLITE:
            base_env += '''# SQLite
DATABASE_URL=sqlite:///./app.db
'''

        return base_env

    def _generate_docker_scripts(self) -> Dict[str, str]:
        """Gera scripts de conveniencia para Docker."""
        app_port = "8000" if self.project_type == "python" else "3000"
        scripts = {}

        # Script Windows (.bat)
        scripts["docker-build.bat"] = f'''@echo off
echo ========================================
echo   Docker Build - {self.project_id}
echo ========================================
echo.
docker build -t {self.docker_image_name} .
echo.
echo Build concluido!
pause
'''

        scripts["docker-run.bat"] = f'''@echo off
echo ========================================
echo   Docker Run - {self.project_id}
echo ========================================
echo.
docker-compose up -d
echo.
echo Containers iniciados!
echo Acesse: http://localhost:{app_port}
echo.
echo Para ver logs: docker-compose logs -f
pause
'''

        scripts["docker-stop.bat"] = f'''@echo off
echo ========================================
echo   Docker Stop - {self.project_id}
echo ========================================
echo.
docker-compose down
echo.
echo Containers parados!
pause
'''

        scripts["docker-logs.bat"] = f'''@echo off
echo ========================================
echo   Docker Logs - {self.project_id}
echo ========================================
echo.
docker-compose logs -f
'''

        # Scripts Linux/Mac (.sh)
        scripts["docker-build.sh"] = f'''#!/bin/bash
echo "========================================"
echo "  Docker Build - {self.project_id}"
echo "========================================"
echo
docker build -t {self.docker_image_name} .
echo
echo "Build concluido!"
'''

        scripts["docker-run.sh"] = f'''#!/bin/bash
echo "========================================"
echo "  Docker Run - {self.project_id}"
echo "========================================"
echo
docker-compose up -d
echo
echo "Containers iniciados!"
echo "Acesse: http://localhost:{app_port}"
echo
echo "Para ver logs: docker-compose logs -f"
'''

        scripts["docker-stop.sh"] = f'''#!/bin/bash
echo "========================================"
echo "  Docker Stop - {self.project_id}"
echo "========================================"
echo
docker-compose down
echo
echo "Containers parados!"
'''

        scripts["docker-logs.sh"] = f'''#!/bin/bash
echo "========================================"
echo "  Docker Logs - {self.project_id}"
echo "========================================"
echo
docker-compose logs -f
'''

        return scripts

    def _generate_nginx_config(self) -> str:
        """Gera configuracao Nginx como proxy reverso."""
        app_port = "8000" if self.project_type == "python" else "3000"
        return f'''# nginx.conf para {self.project_id}
# Gerado automaticamente pela Fabrica de Agentes

events {{
    worker_connections 1024;
}}

http {{
    upstream app {{
        server app:{app_port};
    }}

    server {{
        listen 80;
        server_name localhost;

        # Redirect HTTP to HTTPS (descomente em producao)
        # return 301 https://$server_name$request_uri;

        location / {{
            proxy_pass http://app;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection 'upgrade';
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_cache_bypass $http_upgrade;
        }}

        # Health check endpoint
        location /health {{
            proxy_pass http://app/health;
            proxy_http_version 1.1;
        }}
    }}

    # HTTPS server (descomente e configure certificados)
    # server {{
    #     listen 443 ssl http2;
    #     server_name localhost;
    #
    #     ssl_certificate /etc/nginx/ssl/cert.pem;
    #     ssl_certificate_key /etc/nginx/ssl/key.pem;
    #
    #     location / {{
    #         proxy_pass http://app;
    #         proxy_http_version 1.1;
    #         proxy_set_header Upgrade $http_upgrade;
    #         proxy_set_header Connection 'upgrade';
    #         proxy_set_header Host $host;
    #         proxy_set_header X-Real-IP $remote_addr;
    #         proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    #         proxy_set_header X-Forwarded-Proto $scheme;
    #         proxy_cache_bypass $http_upgrade;
    #     }}
    # }}
}}
'''

    def build_docker(self) -> Dict:
        """Executa docker build para a aplicacao."""
        if not self.project_path or not self.project_path.exists():
            return {"success": False, "message": "Projeto nao encontrado"}

        dockerfile_path = self.project_path / "Dockerfile"
        if not dockerfile_path.exists():
            result = self.generate_docker_files()
            if not result.get("success"):
                return result

        try:
            logger.info(f"Iniciando docker build para {self.project_id}")

            process = subprocess.run(
                ["docker", "build", "-t", self.docker_image_name, "."],
                cwd=str(self.project_path),
                capture_output=True,
                text=True,
                timeout=600
            )

            if process.returncode == 0:
                return {
                    "success": True,
                    "message": f"Imagem {self.docker_image_name} criada com sucesso!",
                    "image_name": self.docker_image_name,
                    "output": process.stdout
                }
            else:
                return {
                    "success": False,
                    "message": "Erro no docker build",
                    "error": process.stderr,
                    "output": process.stdout
                }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "message": "Timeout: Build demorou mais de 10 minutos"
            }
        except FileNotFoundError:
            return {
                "success": False,
                "message": "Docker nao encontrado. Verifique se o Docker esta instalado."
            }
        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao executar docker build: {str(e)}"
            }

    def run_docker(self, detached: bool = True) -> Dict:
        """Executa docker-compose up para iniciar os containers."""
        if not self.project_path or not self.project_path.exists():
            return {"success": False, "message": "Projeto nao encontrado"}

        compose_path = self.project_path / "docker-compose.yml"
        if not compose_path.exists():
            return {
                "success": False,
                "message": "docker-compose.yml nao encontrado. Execute generate_docker_files primeiro."
            }

        try:
            logger.info(f"Iniciando containers para {self.project_id}")

            cmd = ["docker-compose", "up"]
            if detached:
                cmd.append("-d")

            process = subprocess.run(
                cmd,
                cwd=str(self.project_path),
                capture_output=True,
                text=True,
                timeout=300
            )

            app_port = "8000" if self.project_type == "python" else "3000"

            if process.returncode == 0:
                return {
                    "success": True,
                    "message": "Containers iniciados com sucesso!",
                    "app_url": f"http://localhost:{app_port}",
                    "container_name": self.docker_container_name,
                    "output": process.stdout
                }
            else:
                return {
                    "success": False,
                    "message": "Erro ao iniciar containers",
                    "error": process.stderr,
                    "output": process.stdout
                }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "message": "Timeout: Containers demoraram mais de 5 minutos para iniciar"
            }
        except FileNotFoundError:
            return {
                "success": False,
                "message": "Docker Compose nao encontrado. Verifique se o Docker esta instalado."
            }
        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao executar docker-compose: {str(e)}"
            }

    def stop_docker(self) -> Dict:
        """Para os containers Docker da aplicacao."""
        if not self.project_path or not self.project_path.exists():
            return {"success": False, "message": "Projeto nao encontrado"}

        try:
            logger.info(f"Parando containers para {self.project_id}")

            process = subprocess.run(
                ["docker-compose", "down"],
                cwd=str(self.project_path),
                capture_output=True,
                text=True,
                timeout=120
            )

            if process.returncode == 0:
                return {
                    "success": True,
                    "message": "Containers parados com sucesso!",
                    "output": process.stdout
                }
            else:
                return {
                    "success": False,
                    "message": "Erro ao parar containers",
                    "error": process.stderr
                }

        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao parar containers: {str(e)}"
            }

    def get_docker_logs(self, lines: int = 100) -> Dict:
        """Obtem logs dos containers Docker."""
        if not self.project_path or not self.project_path.exists():
            return {"success": False, "message": "Projeto nao encontrado"}

        try:
            cmd = ["docker-compose", "logs", "--tail", str(lines)]

            process = subprocess.run(
                cmd,
                cwd=str(self.project_path),
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": True,
                "logs": process.stdout,
                "errors": process.stderr if process.stderr else None
            }

        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao obter logs: {str(e)}"
            }

    def get_docker_status(self) -> Dict:
        """Verifica status dos containers Docker."""
        if not self.project_path or not self.project_path.exists():
            return {"success": False, "message": "Projeto nao encontrado"}

        try:
            compose_path = self.project_path / "docker-compose.yml"
            has_docker_files = compose_path.exists()

            process = subprocess.run(
                ["docker-compose", "ps", "--format", "json"],
                cwd=str(self.project_path),
                capture_output=True,
                text=True,
                timeout=30
            )

            containers = []
            running = False

            if process.returncode == 0 and process.stdout.strip():
                try:
                    for line in process.stdout.strip().split('\n'):
                        if line:
                            container = json.loads(line)
                            containers.append(container)
                            if container.get("State") == "running":
                                running = True
                except json.JSONDecodeError:
                    containers = [{"raw": process.stdout}]
                    running = "Up" in process.stdout

            app_port = "8000" if self.project_type == "python" else "3000"

            return {
                "success": True,
                "has_docker_files": has_docker_files,
                "containers": containers,
                "running": running,
                "app_url": f"http://localhost:{app_port}" if running else None
            }

        except FileNotFoundError:
            return {
                "success": True,
                "has_docker_files": (self.project_path / "docker-compose.yml").exists(),
                "containers": [],
                "running": False,
                "docker_installed": False,
                "message": "Docker nao esta instalado ou nao esta no PATH"
            }
        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao verificar status: {str(e)}"
            }

    def deploy_docker(self, database: str = "postgresql") -> Dict:
        """Deploy completo com Docker: gera arquivos, build e run."""
        steps = []

        # 1. Gerar arquivos Docker
        logger.info(f"[Deploy] Gerando arquivos Docker para {self.project_id}")
        gen_result = self.generate_docker_files(database=database)
        steps.append({
            "step": "generate_files",
            "success": gen_result.get("success"),
            "message": gen_result.get("message")
        })

        if not gen_result.get("success"):
            return {
                "success": False,
                "message": "Falha ao gerar arquivos Docker",
                "steps": steps
            }

        # 2. Build da imagem
        logger.info(f"[Deploy] Executando docker build para {self.project_id}")
        build_result = self.build_docker()
        steps.append({
            "step": "docker_build",
            "success": build_result.get("success"),
            "message": build_result.get("message")
        })

        if not build_result.get("success"):
            return {
                "success": False,
                "message": "Falha no docker build",
                "steps": steps
            }

        # 3. Iniciar containers
        logger.info(f"[Deploy] Iniciando containers para {self.project_id}")
        run_result = self.run_docker()
        steps.append({
            "step": "docker_run",
            "success": run_result.get("success"),
            "message": run_result.get("message")
        })

        if not run_result.get("success"):
            return {
                "success": False,
                "message": "Falha ao iniciar containers",
                "steps": steps
            }

        app_port = "8000" if self.project_type == "python" else "3000"
        return {
            "success": True,
            "message": "Deploy com Docker concluido com sucesso!",
            "app_url": f"http://localhost:{app_port}",
            "container_name": self.docker_container_name,
            "image_name": self.docker_image_name,
            "steps": steps,
            "commands": {
                "logs": "docker-compose logs -f",
                "stop": "docker-compose down",
                "restart": "docker-compose restart"
            }
        }


# ============================================================
# FUNCOES UTILITARIAS PARA USO DIRETO
# ============================================================

def generate_docker(project_id: str, database: str = "postgresql") -> Dict:
    """
    Gera arquivos Docker para um projeto.

    Args:
        project_id: ID do projeto
        database: Tipo de banco (postgresql, mysql, mongodb, sqlite, none)

    Returns:
        Dicionario com resultado da geracao
    """
    generator = DockerGenerator(project_id)
    return generator.generate_docker_files(database=database)


def build_docker(project_id: str) -> Dict:
    """Executa docker build para um projeto."""
    generator = DockerGenerator(project_id)
    return generator.build_docker()


def run_docker(project_id: str) -> Dict:
    """Inicia containers Docker para um projeto."""
    generator = DockerGenerator(project_id)
    return generator.run_docker()


def stop_docker(project_id: str) -> Dict:
    """Para containers Docker de um projeto."""
    generator = DockerGenerator(project_id)
    return generator.stop_docker()


def docker_logs(project_id: str, lines: int = 100) -> Dict:
    """Obtem logs Docker de um projeto."""
    generator = DockerGenerator(project_id)
    return generator.get_docker_logs(lines=lines)


def docker_status(project_id: str) -> Dict:
    """Verifica status Docker de um projeto."""
    generator = DockerGenerator(project_id)
    return generator.get_docker_status()


def deploy_docker(project_id: str, database: str = "postgresql") -> Dict:
    """Deploy completo com Docker: gera, build e run."""
    generator = DockerGenerator(project_id)
    return generator.deploy_docker(database=database)
