#!/usr/bin/env python
"""
Full Stack Launcher
Plataforma E v4.0

Inicia toda a stack: Dashboard/API + Workers

Usage:
    python factory/scripts/start_all.py [--workers N]

Prerequisites:
    - Docker (PostgreSQL + Redis): docker-compose up -d
    - Ou SQLite + Redis local como fallback
"""
import asyncio
import subprocess
import signal
import sys
import os
from pathlib import Path

# Adicionar raiz ao path
ROOT_DIR = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(ROOT_DIR))

from factory.config import (
    DEFAULT_WORKERS,
    DASHBOARD_HOST,
    DASHBOARD_PORT,
    ANTHROPIC_API_KEY
)


processes = []


def cleanup():
    """Limpa processos ao sair"""
    print("\n[Launcher] Parando todos os servicos...")
    for proc in processes:
        if proc.poll() is None:  # Ainda rodando
            proc.terminate()
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                proc.kill()
    print("[Launcher] Todos os servicos parados")


def signal_handler(signum, frame):
    """Handler para sinais"""
    cleanup()
    sys.exit(0)


def check_prerequisites():
    """Verifica pre-requisitos"""
    errors = []

    # Verificar API key
    if not ANTHROPIC_API_KEY:
        errors.append("ANTHROPIC_API_KEY nao configurada")

    # Verificar Python packages
    try:
        import fastapi
        import uvicorn
    except ImportError:
        errors.append("FastAPI/Uvicorn nao instalados (pip install fastapi uvicorn)")

    try:
        import anthropic
    except ImportError:
        errors.append("Anthropic SDK nao instalado (pip install anthropic)")

    if errors:
        print("[ERROR] Pre-requisitos nao atendidos:")
        for e in errors:
            print(f"  - {e}")
        return False

    return True


def start_dashboard():
    """Inicia o dashboard/API"""
    print(f"[Launcher] Iniciando Dashboard em http://{DASHBOARD_HOST}:{DASHBOARD_PORT}")

    dashboard_path = ROOT_DIR / "factory" / "dashboard" / "app_v4.py"

    proc = subprocess.Popen(
        [sys.executable, str(dashboard_path)],
        cwd=str(ROOT_DIR),
        env={**os.environ},
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )
    processes.append(proc)
    return proc


def start_workers(num_workers: int):
    """Inicia workers em background"""
    print(f"[Launcher] Iniciando {num_workers} workers...")

    workers_path = ROOT_DIR / "factory" / "scripts" / "start_workers.py"

    proc = subprocess.Popen(
        [sys.executable, str(workers_path), "--workers", str(num_workers)],
        cwd=str(ROOT_DIR),
        env={**os.environ},
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )
    processes.append(proc)
    return proc


async def stream_output(proc, prefix: str):
    """Stream output de um processo"""
    while True:
        if proc.poll() is not None:
            break
        line = proc.stdout.readline()
        if line:
            print(f"[{prefix}] {line.rstrip()}")
        else:
            await asyncio.sleep(0.1)


async def main(num_workers: int):
    """Main loop"""
    print("=" * 60)
    print("  FABRICA DE AGENTES v4.0 - Full Stack")
    print("=" * 60)
    print()

    # Verificar pre-requisitos
    if not check_prerequisites():
        sys.exit(1)

    # Registrar handlers
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Iniciar servicos
    dashboard_proc = start_dashboard()
    await asyncio.sleep(2)  # Aguardar dashboard iniciar

    workers_proc = start_workers(num_workers)

    print()
    print("=" * 60)
    print(f"  Dashboard: http://{DASHBOARD_HOST}:{DASHBOARD_PORT}")
    print(f"  API Docs:  http://{DASHBOARD_HOST}:{DASHBOARD_PORT}/docs")
    print(f"  Workers:   {num_workers} ativos")
    print("=" * 60)
    print("  Pressione Ctrl+C para parar")
    print("=" * 60)
    print()

    # Stream output de ambos
    try:
        await asyncio.gather(
            stream_output(dashboard_proc, "DASHBOARD"),
            stream_output(workers_proc, "WORKERS"),
        )
    except asyncio.CancelledError:
        pass

    cleanup()


def cli():
    """CLI"""
    import argparse

    parser = argparse.ArgumentParser(description="Inicia Plataforma E v4.0")
    parser.add_argument(
        "-w", "--workers",
        type=int,
        default=DEFAULT_WORKERS,
        help=f"Numero de workers (default: {DEFAULT_WORKERS})"
    )

    args = parser.parse_args()
    asyncio.run(main(args.workers))


if __name__ == "__main__":
    cli()
