#!/usr/bin/env python3
"""
Fabrica de Agentes - Setup PostgreSQL

Script para configurar e migrar para PostgreSQL automaticamente.

Uso:
    python setup_postgres.py

Passos executados:
    1. Verifica se Docker esta rodando
    2. Inicia containers PostgreSQL e Redis
    3. Aguarda PostgreSQL ficar pronto
    4. Executa migracao dos dados
    5. Verifica integridade
"""

import subprocess
import sys
import time
import os

def run_command(cmd, capture=True, shell=True):
    """Executa comando e retorna resultado"""
    try:
        result = subprocess.run(
            cmd,
            shell=shell,
            capture_output=capture,
            text=True
        )
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)

def check_docker():
    """Verifica se Docker esta instalado e rodando"""
    print("[1/5] Verificando Docker...")

    # Verificar instalacao
    success, stdout, stderr = run_command("docker --version")
    if not success:
        print("   ERRO: Docker nao encontrado!")
        print("   ")
        print("   Para instalar Docker Desktop:")
        print("   1. Acesse: https://www.docker.com/products/docker-desktop")
        print("   2. Baixe e instale o Docker Desktop para Windows")
        print("   3. Reinicie o computador se necessario")
        print("   4. Abra o Docker Desktop e aguarde inicializar")
        print("   5. Execute este script novamente")
        return False

    print(f"   Docker instalado: {stdout.strip()}")

    # Verificar se daemon esta rodando
    success, stdout, stderr = run_command("docker info")
    if not success:
        print("   ERRO: Docker daemon nao esta rodando!")
        print("   ")
        print("   Abra o Docker Desktop e aguarde inicializar.")
        return False

    print("   Docker daemon rodando!")
    return True

def start_containers():
    """Inicia containers via docker-compose"""
    print("\n[2/5] Iniciando containers PostgreSQL e Redis...")

    # Verificar se docker-compose existe
    success, _, _ = run_command("docker-compose --version")
    compose_cmd = "docker-compose" if success else "docker compose"

    # Parar containers existentes
    run_command(f"{compose_cmd} down")

    # Iniciar containers
    success, stdout, stderr = run_command(f"{compose_cmd} up -d postgres redis")
    if not success:
        print(f"   ERRO ao iniciar containers: {stderr}")
        return False

    print("   Containers iniciados!")
    return True

def wait_for_postgres(max_attempts=30):
    """Aguarda PostgreSQL ficar pronto"""
    print("\n[3/5] Aguardando PostgreSQL ficar pronto...")

    for i in range(max_attempts):
        success, _, _ = run_command(
            'docker exec fabrica-postgres pg_isready -U fabrica -d fabrica_db'
        )
        if success:
            print(f"   PostgreSQL pronto! (tentativa {i+1})")
            return True

        print(f"   Aguardando... ({i+1}/{max_attempts})")
        time.sleep(2)

    print("   ERRO: PostgreSQL nao ficou pronto a tempo!")
    return False

def run_migration():
    """Executa script de migracao"""
    print("\n[4/5] Executando migracao de dados...")

    # Importar e executar migracao
    try:
        from migrate_sqlite_to_postgres import main as migrate_main
        success = migrate_main()
        return success
    except Exception as e:
        print(f"   ERRO na migracao: {e}")
        return False

def verify_application():
    """Verifica se aplicacao funciona com PostgreSQL"""
    print("\n[5/5] Verificando aplicacao...")

    try:
        # Testar conexao via SQLAlchemy
        from factory.database.connection import check_db_health, IS_POSTGRES
        import asyncio

        if not IS_POSTGRES:
            print("   AVISO: Aplicacao ainda configurada para SQLite")
            print("   Verifique o arquivo .env")
            return False

        # Executar health check
        async def check():
            return await check_db_health()

        health = asyncio.run(check())

        db_status = health.get("database", {}).get("status", "unknown")
        redis_status = health.get("redis", {}).get("status", "unknown")

        print(f"   Database: {db_status}")
        print(f"   Redis: {redis_status}")

        return db_status == "healthy"

    except Exception as e:
        print(f"   ERRO ao verificar: {e}")
        return False

def main():
    print("=" * 60)
    print("  SETUP PostgreSQL - Fabrica de Agentes")
    print("=" * 60)

    # Step 1: Check Docker
    if not check_docker():
        return False

    # Step 2: Start containers
    if not start_containers():
        return False

    # Step 3: Wait for PostgreSQL
    if not wait_for_postgres():
        return False

    # Step 4: Run migration
    if not run_migration():
        print("\nMigracao falhou, mas containers estao rodando.")
        print("Voce pode tentar a migracao manualmente:")
        print("  python migrate_sqlite_to_postgres.py")
        return False

    # Step 5: Verify
    verify_application()

    print("\n" + "=" * 60)
    print("  SETUP CONCLUIDO!")
    print("  ")
    print("  Para iniciar a aplicacao:")
    print("    python factory/dashboard/app_v6_agile.py")
    print("  ")
    print("  Dashboard disponivel em: http://localhost:9001")
    print("  ")
    print("  Comandos uteis:")
    print("    docker-compose ps         # Ver status dos containers")
    print("    docker-compose logs -f    # Ver logs")
    print("    docker-compose down       # Parar containers")
    print("=" * 60)

    return True

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
