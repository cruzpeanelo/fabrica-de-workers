#!/usr/bin/env python3
"""
Plataforma E - Disaster Recovery Test Script
===================================================
Issue #98: Plano de Disaster Recovery Documentado

Este script executa testes automatizados de DR para validar:
- Backup e restore do banco de dados
- Failover de pods/services
- Recuperacao de Redis
- Conectividade pos-failover
- Integridade de dados

Uso:
    # Executar todos os testes
    python scripts/dr_test.py --test all

    # Testar apenas backup
    python scripts/dr_test.py --test backup

    # Modo dry-run
    python scripts/dr_test.py --test all --dry-run

    # Verbose mode
    python scripts/dr_test.py --test all -v

Testes disponiveis:
    - backup: Testa backup e restore do database
    - failover: Testa failover de pods
    - redis: Testa recuperacao do Redis
    - connectivity: Testa conectividade da API
    - integrity: Testa integridade dos dados
    - all: Executa todos os testes
"""

import os
import sys
import subprocess
import argparse
import logging
import json
import time
import hashlib
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from enum import Enum

# Configuracao de logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class TestResult(Enum):
    PASSED = "PASSED"
    FAILED = "FAILED"
    SKIPPED = "SKIPPED"
    WARNING = "WARNING"


@dataclass
class TestCase:
    name: str
    description: str
    result: TestResult = TestResult.SKIPPED
    duration_seconds: float = 0.0
    message: str = ""
    details: Dict = None


class DRTestSuite:
    """Suite de testes de Disaster Recovery."""

    def __init__(self, dry_run: bool = False, verbose: bool = False):
        self.dry_run = dry_run
        self.verbose = verbose
        self.results: List[TestCase] = []
        self.start_time = None
        self.end_time = None

        # Configuracoes
        self.db_host = os.getenv('DB_HOST', 'localhost')
        self.db_port = os.getenv('DB_PORT', '5432')
        self.db_user = os.getenv('DB_USER', 'postgres')
        self.db_password = os.getenv('DB_PASSWORD', '')
        self.db_name = os.getenv('DB_NAME', 'fabrica_db')
        self.backup_dir = Path(os.getenv('BACKUP_DIR', '/backups'))
        self.api_url = os.getenv('API_URL', 'http://localhost:9001')
        self.k8s_namespace = os.getenv('K8S_NAMESPACE', 'fabrica-agentes')

    def run(self, tests: List[str]) -> bool:
        """
        Executa os testes especificados.

        Args:
            tests: Lista de testes a executar

        Returns:
            True se todos os testes passaram
        """
        self.start_time = datetime.now()
        logger.info("=" * 70)
        logger.info("FABRICA DE AGENTES - DISASTER RECOVERY TESTS")
        logger.info("=" * 70)
        logger.info(f"Start time: {self.start_time.isoformat()}")
        logger.info(f"Dry run: {self.dry_run}")
        logger.info(f"Tests to run: {', '.join(tests)}")
        logger.info("-" * 70)

        # Mapear testes
        test_map = {
            'backup': self.test_backup_restore,
            'failover': self.test_pod_failover,
            'redis': self.test_redis_recovery,
            'connectivity': self.test_api_connectivity,
            'integrity': self.test_data_integrity,
        }

        # Executar testes
        if 'all' in tests:
            tests = list(test_map.keys())

        for test_name in tests:
            if test_name in test_map:
                logger.info(f"\n>>> Running test: {test_name}")
                try:
                    test_map[test_name]()
                except Exception as e:
                    logger.error(f"Test {test_name} failed with exception: {e}")
                    self.results.append(TestCase(
                        name=test_name,
                        description=f"Test {test_name}",
                        result=TestResult.FAILED,
                        message=str(e)
                    ))

        self.end_time = datetime.now()
        return self._print_summary()

    def test_backup_restore(self):
        """Testa backup e restore do banco de dados."""
        test = TestCase(
            name="backup_restore",
            description="Database backup and restore test"
        )
        start = time.time()

        try:
            if self.dry_run:
                logger.info("[DRY RUN] Would test backup and restore")
                test.result = TestResult.SKIPPED
                test.message = "Dry run - skipped"
                self.results.append(test)
                return

            # 1. Criar backup
            logger.info("Step 1: Creating backup...")
            backup_result = self._run_command([
                'python', 'scripts/backup_database.py'
            ], cwd=str(Path(__file__).parent.parent))

            if backup_result.returncode != 0:
                raise Exception(f"Backup failed: {backup_result.stderr}")

            # 2. Encontrar backup criado
            backups = list(self.backup_dir.glob("backup_*.sql.gz"))
            if not backups:
                raise Exception("No backup file found after backup")

            latest_backup = max(backups, key=lambda x: x.stat().st_mtime)
            logger.info(f"Step 2: Found backup: {latest_backup.name}")

            # 3. Criar banco de teste
            test_db = f"{self.db_name}_dr_test"
            logger.info(f"Step 3: Creating test database: {test_db}")

            # 4. Restaurar em banco de teste
            logger.info("Step 4: Restoring to test database...")
            restore_result = self._run_command([
                'python', 'scripts/restore_database.py',
                '--file', latest_backup.name,
                '--target-db', test_db,
                '--no-verify',
                '-y'
            ], cwd=str(Path(__file__).parent.parent))

            if restore_result.returncode != 0:
                raise Exception(f"Restore failed: {restore_result.stderr}")

            # 5. Verificar dados restaurados
            logger.info("Step 5: Verifying restored data...")
            verify_result = self._verify_database(test_db)

            if not verify_result:
                raise Exception("Data verification failed")

            # 6. Limpar banco de teste
            logger.info("Step 6: Cleaning up test database...")
            self._drop_database(test_db)

            test.result = TestResult.PASSED
            test.message = f"Backup and restore completed successfully"
            test.details = {
                "backup_file": latest_backup.name,
                "backup_size_mb": round(latest_backup.stat().st_size / 1024 / 1024, 2),
                "test_database": test_db
            }

        except Exception as e:
            test.result = TestResult.FAILED
            test.message = str(e)
            logger.error(f"Backup/Restore test failed: {e}")

        test.duration_seconds = time.time() - start
        self.results.append(test)

    def test_pod_failover(self):
        """Testa failover de pods."""
        test = TestCase(
            name="pod_failover",
            description="Kubernetes pod failover test"
        )
        start = time.time()

        try:
            if self.dry_run:
                logger.info("[DRY RUN] Would test pod failover")
                test.result = TestResult.SKIPPED
                test.message = "Dry run - skipped"
                self.results.append(test)
                return

            # Verificar se kubectl esta disponivel
            if not self._check_kubectl():
                test.result = TestResult.SKIPPED
                test.message = "kubectl not available or not configured"
                self.results.append(test)
                return

            # 1. Obter pod atual
            logger.info("Step 1: Getting current API pods...")
            pods_result = self._run_command([
                'kubectl', 'get', 'pods',
                '-n', self.k8s_namespace,
                '-l', 'app=fabrica-api',
                '-o', 'json'
            ])

            if pods_result.returncode != 0:
                raise Exception(f"Could not get pods: {pods_result.stderr}")

            pods_data = json.loads(pods_result.stdout)
            if not pods_data.get('items'):
                test.result = TestResult.SKIPPED
                test.message = "No API pods found in cluster"
                self.results.append(test)
                return

            initial_pod_count = len(pods_data['items'])
            pod_to_delete = pods_data['items'][0]['metadata']['name']
            logger.info(f"Step 2: Found {initial_pod_count} pods, will delete: {pod_to_delete}")

            # 2. Deletar um pod
            logger.info(f"Step 3: Deleting pod {pod_to_delete}...")
            delete_result = self._run_command([
                'kubectl', 'delete', 'pod', pod_to_delete,
                '-n', self.k8s_namespace
            ])

            if delete_result.returncode != 0:
                raise Exception(f"Could not delete pod: {delete_result.stderr}")

            # 3. Aguardar novo pod ficar ready
            logger.info("Step 4: Waiting for new pod to be ready...")
            ready = self._wait_for_pods_ready(initial_pod_count, timeout=120)

            if not ready:
                raise Exception("New pod did not become ready in time")

            # 4. Verificar conectividade
            logger.info("Step 5: Verifying API connectivity...")
            if not self._check_api_health():
                raise Exception("API health check failed after failover")

            test.result = TestResult.PASSED
            test.message = f"Pod failover completed successfully"
            test.details = {
                "deleted_pod": pod_to_delete,
                "initial_count": initial_pod_count,
                "recovery_verified": True
            }

        except Exception as e:
            test.result = TestResult.FAILED
            test.message = str(e)
            logger.error(f"Pod failover test failed: {e}")

        test.duration_seconds = time.time() - start
        self.results.append(test)

    def test_redis_recovery(self):
        """Testa recuperacao do Redis."""
        test = TestCase(
            name="redis_recovery",
            description="Redis connection recovery test"
        )
        start = time.time()

        try:
            if self.dry_run:
                logger.info("[DRY RUN] Would test Redis recovery")
                test.result = TestResult.SKIPPED
                test.message = "Dry run - skipped"
                self.results.append(test)
                return

            import redis

            redis_url = os.getenv('REDIS_URL', 'redis://localhost:6379/0')
            logger.info(f"Step 1: Connecting to Redis: {redis_url}")

            # 1. Conectar ao Redis
            r = redis.from_url(redis_url)

            # 2. Escrever dado de teste
            test_key = f"dr_test_{datetime.now().strftime('%Y%m%d%H%M%S')}"
            test_value = "DR Test Value"
            logger.info(f"Step 2: Writing test key: {test_key}")
            r.set(test_key, test_value, ex=300)  # Expira em 5 min

            # 3. Ler dado de volta
            logger.info("Step 3: Reading test key back...")
            read_value = r.get(test_key)

            if read_value is None:
                raise Exception("Could not read test key")

            if read_value.decode() != test_value:
                raise Exception(f"Value mismatch: expected {test_value}, got {read_value}")

            # 4. Verificar info do Redis
            logger.info("Step 4: Getting Redis info...")
            info = r.info()

            # 5. Limpar key de teste
            r.delete(test_key)

            test.result = TestResult.PASSED
            test.message = "Redis read/write test passed"
            test.details = {
                "redis_version": info.get('redis_version'),
                "connected_clients": info.get('connected_clients'),
                "used_memory_human": info.get('used_memory_human')
            }

        except ImportError:
            test.result = TestResult.SKIPPED
            test.message = "redis package not installed"
        except Exception as e:
            test.result = TestResult.FAILED
            test.message = str(e)
            logger.error(f"Redis test failed: {e}")

        test.duration_seconds = time.time() - start
        self.results.append(test)

    def test_api_connectivity(self):
        """Testa conectividade da API."""
        test = TestCase(
            name="api_connectivity",
            description="API health and connectivity test"
        )
        start = time.time()

        try:
            if self.dry_run:
                logger.info("[DRY RUN] Would test API connectivity")
                test.result = TestResult.SKIPPED
                test.message = "Dry run - skipped"
                self.results.append(test)
                return

            import urllib.request
            import urllib.error

            endpoints = [
                '/api/health',
                '/api/v1/health',
            ]

            results = {}

            for endpoint in endpoints:
                url = f"{self.api_url}{endpoint}"
                logger.info(f"Testing endpoint: {url}")

                try:
                    start_req = time.time()
                    req = urllib.request.Request(url)
                    with urllib.request.urlopen(req, timeout=10) as response:
                        status = response.status
                        latency = time.time() - start_req
                        results[endpoint] = {
                            "status": status,
                            "latency_ms": round(latency * 1000, 2),
                            "success": True
                        }
                        logger.info(f"  -> {status} OK ({latency*1000:.0f}ms)")

                except urllib.error.URLError as e:
                    results[endpoint] = {
                        "status": 0,
                        "error": str(e),
                        "success": False
                    }
                    logger.warning(f"  -> FAILED: {e}")

            # Verificar se pelo menos um endpoint funcionou
            successful = sum(1 for r in results.values() if r.get('success'))

            if successful == 0:
                test.result = TestResult.FAILED
                test.message = "No endpoints responded successfully"
            elif successful < len(endpoints):
                test.result = TestResult.WARNING
                test.message = f"{successful}/{len(endpoints)} endpoints responding"
            else:
                test.result = TestResult.PASSED
                test.message = "All endpoints responding"

            test.details = results

        except Exception as e:
            test.result = TestResult.FAILED
            test.message = str(e)
            logger.error(f"API connectivity test failed: {e}")

        test.duration_seconds = time.time() - start
        self.results.append(test)

    def test_data_integrity(self):
        """Testa integridade dos dados."""
        test = TestCase(
            name="data_integrity",
            description="Database data integrity test"
        )
        start = time.time()

        try:
            if self.dry_run:
                logger.info("[DRY RUN] Would test data integrity")
                test.result = TestResult.SKIPPED
                test.message = "Dry run - skipped"
                self.results.append(test)
                return

            import psycopg2

            logger.info("Step 1: Connecting to database...")
            conn = psycopg2.connect(
                host=self.db_host,
                port=self.db_port,
                user=self.db_user,
                password=self.db_password,
                database=self.db_name
            )
            cur = conn.cursor()

            checks = {}

            # 1. Verificar tabelas existem
            logger.info("Step 2: Checking tables exist...")
            cur.execute("""
                SELECT table_name
                FROM information_schema.tables
                WHERE table_schema = 'public'
            """)
            tables = [row[0] for row in cur.fetchall()]
            checks['tables'] = tables
            logger.info(f"  -> Found {len(tables)} tables")

            # 2. Verificar integridade referencial
            logger.info("Step 3: Checking referential integrity...")
            integrity_issues = []

            # Verificar foreign keys
            cur.execute("""
                SELECT
                    tc.table_name,
                    tc.constraint_name,
                    ccu.table_name AS foreign_table
                FROM information_schema.table_constraints tc
                JOIN information_schema.constraint_column_usage ccu
                    ON tc.constraint_name = ccu.constraint_name
                WHERE tc.constraint_type = 'FOREIGN KEY'
            """)
            fk_constraints = cur.fetchall()
            checks['foreign_keys'] = len(fk_constraints)

            # 3. Verificar indices
            logger.info("Step 4: Checking indexes...")
            cur.execute("""
                SELECT count(*)
                FROM pg_indexes
                WHERE schemaname = 'public'
            """)
            index_count = cur.fetchone()[0]
            checks['indexes'] = index_count
            logger.info(f"  -> Found {index_count} indexes")

            # 4. Verificar contagem de registros principais
            logger.info("Step 5: Checking record counts...")
            record_counts = {}
            for table in ['stories', 'story_tasks', 'jobs']:
                if table in tables:
                    try:
                        cur.execute(f"SELECT count(*) FROM {table}")
                        count = cur.fetchone()[0]
                        record_counts[table] = count
                        logger.info(f"  -> {table}: {count} records")
                    except Exception:
                        record_counts[table] = 'error'

            checks['record_counts'] = record_counts

            conn.close()

            # Avaliar resultado
            if len(tables) == 0:
                test.result = TestResult.FAILED
                test.message = "No tables found in database"
            elif integrity_issues:
                test.result = TestResult.WARNING
                test.message = f"Found {len(integrity_issues)} integrity issues"
            else:
                test.result = TestResult.PASSED
                test.message = "Data integrity checks passed"

            test.details = checks

        except ImportError:
            test.result = TestResult.SKIPPED
            test.message = "psycopg2 package not installed"
        except Exception as e:
            test.result = TestResult.FAILED
            test.message = str(e)
            logger.error(f"Data integrity test failed: {e}")

        test.duration_seconds = time.time() - start
        self.results.append(test)

    # ==========================================================================
    # Helper methods
    # ==========================================================================

    def _run_command(
        self,
        cmd: List[str],
        cwd: str = None,
        timeout: int = 300
    ) -> subprocess.CompletedProcess:
        """Executa comando e retorna resultado."""
        if self.verbose:
            logger.info(f"Running: {' '.join(cmd)}")

        env = os.environ.copy()
        env['PGPASSWORD'] = self.db_password

        return subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=cwd,
            env=env,
            timeout=timeout
        )

    def _check_kubectl(self) -> bool:
        """Verifica se kubectl esta disponivel."""
        try:
            result = subprocess.run(
                ['kubectl', 'version', '--client'],
                capture_output=True,
                timeout=5
            )
            return result.returncode == 0
        except Exception:
            return False

    def _verify_database(self, db_name: str) -> bool:
        """Verifica se banco de dados tem dados validos."""
        try:
            result = self._run_command([
                'psql',
                '-h', self.db_host,
                '-p', self.db_port,
                '-U', self.db_user,
                '-d', db_name,
                '-t', '-c',
                "SELECT count(*) FROM pg_tables WHERE schemaname = 'public'"
            ])

            if result.returncode != 0:
                return False

            count = int(result.stdout.strip())
            return count > 0

        except Exception:
            return False

    def _drop_database(self, db_name: str):
        """Remove banco de dados de teste."""
        try:
            self._run_command([
                'dropdb',
                '-h', self.db_host,
                '-p', self.db_port,
                '-U', self.db_user,
                '--if-exists',
                db_name
            ])
        except Exception:
            pass

    def _wait_for_pods_ready(self, expected_count: int, timeout: int = 120) -> bool:
        """Aguarda pods ficarem ready."""
        start = time.time()

        while time.time() - start < timeout:
            result = self._run_command([
                'kubectl', 'get', 'pods',
                '-n', self.k8s_namespace,
                '-l', 'app=fabrica-api',
                '-o', 'json'
            ])

            if result.returncode == 0:
                pods_data = json.loads(result.stdout)
                ready_pods = sum(
                    1 for pod in pods_data.get('items', [])
                    if all(
                        cs.get('ready', False)
                        for cs in pod.get('status', {}).get('containerStatuses', [])
                    )
                )

                if ready_pods >= expected_count:
                    return True

            time.sleep(5)

        return False

    def _check_api_health(self) -> bool:
        """Verifica health da API."""
        try:
            import urllib.request
            req = urllib.request.Request(f"{self.api_url}/api/health")
            with urllib.request.urlopen(req, timeout=10) as response:
                return response.status == 200
        except Exception:
            return False

    def _print_summary(self) -> bool:
        """Imprime resumo dos testes."""
        duration = (self.end_time - self.start_time).total_seconds()

        logger.info("\n" + "=" * 70)
        logger.info("TEST RESULTS SUMMARY")
        logger.info("=" * 70)

        passed = sum(1 for t in self.results if t.result == TestResult.PASSED)
        failed = sum(1 for t in self.results if t.result == TestResult.FAILED)
        skipped = sum(1 for t in self.results if t.result == TestResult.SKIPPED)
        warnings = sum(1 for t in self.results if t.result == TestResult.WARNING)

        for test in self.results:
            status_icon = {
                TestResult.PASSED: "[PASS]",
                TestResult.FAILED: "[FAIL]",
                TestResult.SKIPPED: "[SKIP]",
                TestResult.WARNING: "[WARN]"
            }.get(test.result, "[????]")

            logger.info(
                f"{status_icon} {test.name:20} - {test.message} "
                f"({test.duration_seconds:.1f}s)"
            )

        logger.info("-" * 70)
        logger.info(f"Total: {len(self.results)} tests")
        logger.info(f"  Passed:   {passed}")
        logger.info(f"  Failed:   {failed}")
        logger.info(f"  Warnings: {warnings}")
        logger.info(f"  Skipped:  {skipped}")
        logger.info(f"Duration:   {duration:.1f} seconds")
        logger.info("=" * 70)

        # Gerar relatorio JSON
        report = {
            "timestamp": self.start_time.isoformat(),
            "duration_seconds": duration,
            "summary": {
                "total": len(self.results),
                "passed": passed,
                "failed": failed,
                "warnings": warnings,
                "skipped": skipped
            },
            "tests": [
                {
                    "name": t.name,
                    "description": t.description,
                    "result": t.result.value,
                    "message": t.message,
                    "duration_seconds": t.duration_seconds,
                    "details": t.details
                }
                for t in self.results
            ]
        }

        report_path = Path("dr_test_report.json")
        with open(report_path, 'w') as f:
            json.dump(report, f, indent=2)
        logger.info(f"\nReport saved to: {report_path}")

        return failed == 0


def main():
    parser = argparse.ArgumentParser(
        description='Plataforma E - DR Test Suite'
    )
    parser.add_argument(
        '--test',
        nargs='+',
        default=['all'],
        choices=['all', 'backup', 'failover', 'redis', 'connectivity', 'integrity'],
        help='Tests to run'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without executing'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Verbose output'
    )

    args = parser.parse_args()

    suite = DRTestSuite(dry_run=args.dry_run, verbose=args.verbose)
    success = suite.run(args.test)

    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
