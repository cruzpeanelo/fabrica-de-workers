#!/usr/bin/env python
"""
Worker Launcher Script
Plataforma E v4.0

Inicia pool de workers Claude para processar jobs da fila.

Usage:
    python factory/scripts/start_workers.py [--workers N] [--model MODEL]

Examples:
    python factory/scripts/start_workers.py
    python factory/scripts/start_workers.py --workers 3
    python factory/scripts/start_workers.py --workers 5 --model claude-sonnet-4-20250514
"""
import asyncio
import argparse
import signal
import sys
from pathlib import Path

# Adicionar raiz ao path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent))

from factory.config import (
    DEFAULT_WORKERS,
    MAX_WORKERS,
    CLAUDE_MODEL,
    ANTHROPIC_API_KEY,
    get_enabled_mcp_tools
)
from factory.core.worker import WorkerPool


# Flag global para shutdown graceful
shutdown_event = asyncio.Event()


def signal_handler(signum, frame):
    """Handler para sinais de shutdown"""
    print(f"\n[Launcher] Recebido sinal {signum}, iniciando shutdown...")
    shutdown_event.set()


async def main(num_workers: int, model: str):
    """
    Funcao principal que inicia o pool de workers

    Args:
        num_workers: Numero de workers a iniciar
        model: Modelo Claude a usar
    """
    # Verificar API key
    if not ANTHROPIC_API_KEY:
        print("[ERROR] ANTHROPIC_API_KEY nao configurada!")
        print("Configure a variavel de ambiente ou no arquivo .env")
        sys.exit(1)

    print("=" * 60)
    print("  FABRICA DE AGENTES v4.0 - Worker Launcher")
    print("=" * 60)
    print(f"  Workers: {num_workers}")
    print(f"  Modelo: {model}")
    print(f"  MCP Tools: {get_enabled_mcp_tools()}")
    print("=" * 60)
    print()

    # Criar pool de workers
    pool = WorkerPool(
        num_workers=num_workers,
        model=model,
        mcp_tools=get_enabled_mcp_tools()
    )

    # Registrar handlers de sinal
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    try:
        # Task para monitorar shutdown
        async def monitor_shutdown():
            await shutdown_event.wait()
            print("[Launcher] Parando workers...")
            await pool.stop_all()

        # Iniciar pool e monitor em paralelo
        monitor_task = asyncio.create_task(monitor_shutdown())
        pool_task = asyncio.create_task(pool.start_all())

        # Aguardar qualquer um terminar
        done, pending = await asyncio.wait(
            [monitor_task, pool_task],
            return_when=asyncio.FIRST_COMPLETED
        )

        # Cancelar tasks pendentes
        for task in pending:
            task.cancel()
            try:
                await task
            except asyncio.CancelledError:
                pass

    except Exception as e:
        print(f"[ERROR] Erro fatal: {e}")
        await pool.stop_all()
        sys.exit(1)

    print("[Launcher] Shutdown completo")


def cli():
    """Parse argumentos CLI e executa"""
    parser = argparse.ArgumentParser(
        description="Inicia pool de workers Claude",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
    %(prog)s                          # Inicia com configuracao padrao
    %(prog)s --workers 3              # Inicia 3 workers
    %(prog)s --workers 5 --model claude-sonnet-4-20250514
        """
    )

    parser.add_argument(
        "-w", "--workers",
        type=int,
        default=DEFAULT_WORKERS,
        help=f"Numero de workers (default: {DEFAULT_WORKERS}, max: {MAX_WORKERS})"
    )

    parser.add_argument(
        "-m", "--model",
        type=str,
        default=CLAUDE_MODEL,
        help=f"Modelo Claude (default: {CLAUDE_MODEL})"
    )

    args = parser.parse_args()

    # Validar numero de workers
    if args.workers < 1:
        print("[ERROR] Numero de workers deve ser >= 1")
        sys.exit(1)
    if args.workers > MAX_WORKERS:
        print(f"[WARNING] Limitando a {MAX_WORKERS} workers (maximo configurado)")
        args.workers = MAX_WORKERS

    # Executar
    asyncio.run(main(args.workers, args.model))


if __name__ == "__main__":
    cli()
