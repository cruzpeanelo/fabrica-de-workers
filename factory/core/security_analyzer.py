# -*- coding: utf-8 -*-
"""
Security Analyzer - Analise de Seguranca Automatizada (SAST/DAST) v1.0
======================================================================
Fabrica de Agentes - Issue #57

Implementa analise de seguranca estatica e dinamica:
- SAST: Analise estatica de codigo fonte
- DAST: Testes de seguranca em aplicacao rodando (futuro)
- Integracao com Bandit (Python security linter)
- Deteccao de OWASP Top 10
- Relatorio de seguranca com score

Autor: Claude AI
Data: 2024
"""
import os
import re
import sys
import json
import subprocess
import asyncio
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod

# Garantir encoding UTF-8
if sys.stdout:
    try:
        sys.stdout.reconfigure(encoding='utf-8')
    except:
        pass

# Adicionar path do projeto
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from dotenv import load_dotenv
load_dotenv()


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class VulnerabilitySeverity(str, Enum):
    """Niveis de severidade de vulnerabilidade"""
    CRITICAL = "critical"
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
    INFO = "info"


class VulnerabilityType(str, Enum):
    """Tipos de vulnerabilidade OWASP Top 10 e outros"""
    SQL_INJECTION = "sql_injection"
    XSS = "xss"
    COMMAND_INJECTION = "command_injection"
    PATH_TRAVERSAL = "path_traversal"
    HARDCODED_SECRET = "hardcoded_secret"
    UNSAFE_DESERIALIZATION = "unsafe_deserialization"
    BROKEN_AUTH = "broken_auth"
    SENSITIVE_DATA_EXPOSURE = "sensitive_data_exposure"
    XXE = "xxe"
    BROKEN_ACCESS_CONTROL = "broken_access_control"
    SECURITY_MISCONFIGURATION = "security_misconfiguration"
    INSECURE_DEPENDENCY = "insecure_dependency"
    INSUFFICIENT_LOGGING = "insufficient_logging"
    SSRF = "ssrf"
    CRYPTO_WEAKNESS = "crypto_weakness"
    OTHER = "other"


class ScanType(str, Enum):
    """Tipos de scan de seguranca"""
    SAST = "sast"  # Static Application Security Testing
    DAST = "dast"  # Dynamic Application Security Testing
    SCA = "sca"    # Software Composition Analysis (dependencies)


# Configuracoes de severidade para calculo de score
SEVERITY_WEIGHTS = {
    VulnerabilitySeverity.CRITICAL: 40,
    VulnerabilitySeverity.HIGH: 20,
    VulnerabilitySeverity.MEDIUM: 10,
    VulnerabilitySeverity.LOW: 5,
    VulnerabilitySeverity.INFO: 1
}

# Patterns regex para deteccao de vulnerabilidades
SECURITY_PATTERNS = {
    # SQL Injection
    VulnerabilityType.SQL_INJECTION: [
        (r'execute\s*\(\s*["\'].*%s.*["\']\s*%', "String formatting in SQL query"),
        (r'execute\s*\(\s*f["\']', "f-string in SQL query"),
        (r'cursor\.execute\s*\([^,]+\+', "String concatenation in SQL"),
        (r'\.raw\s*\(\s*["\'].*\+', "Django raw SQL with concatenation"),
        (r'text\s*\(\s*["\'].*%', "SQLAlchemy text with formatting"),
    ],

    # XSS (Cross-Site Scripting)
    VulnerabilityType.XSS: [
        (r'\.innerHTML\s*=', "Direct innerHTML assignment"),
        (r'document\.write\s*\(', "document.write usage"),
        (r'render_template_string\s*\(.*\+', "Flask template string concatenation"),
        (r'mark_safe\s*\(.*\+', "Django mark_safe with concatenation"),
        (r'Markup\s*\(.*\+', "Jinja2 Markup with concatenation"),
    ],

    # Command Injection
    VulnerabilityType.COMMAND_INJECTION: [
        (r'os\.system\s*\(', "os.system usage"),
        (r'os\.popen\s*\(', "os.popen usage"),
        (r'subprocess\.call\s*\([^,]*shell\s*=\s*True', "subprocess with shell=True"),
        (r'subprocess\.run\s*\([^,]*shell\s*=\s*True', "subprocess.run with shell=True"),
        (r'subprocess\.Popen\s*\([^,]*shell\s*=\s*True', "Popen with shell=True"),
        (r'eval\s*\(', "eval usage"),
        (r'exec\s*\(', "exec usage"),
    ],

    # Path Traversal
    VulnerabilityType.PATH_TRAVERSAL: [
        (r'open\s*\([^)]*\+', "open() with string concatenation"),
        (r'Path\s*\([^)]*\+', "Path with concatenation"),
        (r'os\.path\.join\s*\([^)]*request', "path.join with user input"),
        (r'send_file\s*\([^)]*request', "send_file with user input"),
    ],

    # Hardcoded Secrets
    VulnerabilityType.HARDCODED_SECRET: [
        (r'password\s*=\s*["\'][^"\']{3,}["\']', "Hardcoded password"),
        (r'secret\s*=\s*["\'][^"\']{3,}["\']', "Hardcoded secret"),
        (r'api_key\s*=\s*["\'][^"\']{3,}["\']', "Hardcoded API key"),
        (r'apikey\s*=\s*["\'][^"\']{3,}["\']', "Hardcoded API key"),
        (r'token\s*=\s*["\'][^"\']{8,}["\']', "Hardcoded token"),
        (r'(sk|pk)[-_](live|test)[-_][a-zA-Z0-9]{20,}', "Stripe API key"),
        (r'AKIA[0-9A-Z]{16}', "AWS Access Key ID"),
        (r'ghp_[a-zA-Z0-9]{36}', "GitHub Personal Access Token"),
        (r'sk-[a-zA-Z0-9]{48}', "OpenAI API Key pattern"),
        (r'Bearer\s+[a-zA-Z0-9\-_]{20,}', "Hardcoded Bearer token"),
    ],

    # Unsafe Deserialization
    VulnerabilityType.UNSAFE_DESERIALIZATION: [
        (r'pickle\.loads?\s*\(', "pickle.load usage"),
        (r'yaml\.load\s*\([^)]*Loader\s*=\s*None', "yaml.load without SafeLoader"),
        (r'yaml\.unsafe_load', "yaml.unsafe_load usage"),
        (r'marshal\.loads?\s*\(', "marshal.load usage"),
    ],

    # Cryptographic Weaknesses
    VulnerabilityType.CRYPTO_WEAKNESS: [
        (r'md5\s*\(', "MD5 hash usage (weak)"),
        (r'sha1\s*\(', "SHA1 hash usage (weak)"),
        (r'DES\s*\(', "DES encryption (weak)"),
        (r'random\.random\s*\(', "Insecure random for crypto"),
        (r'random\.randint\s*\(', "Insecure random for crypto"),
    ],

    # Sensitive Data Exposure
    VulnerabilityType.SENSITIVE_DATA_EXPOSURE: [
        (r'print\s*\([^)]*password', "Printing password"),
        (r'print\s*\([^)]*secret', "Printing secret"),
        (r'logging\.[^(]+\([^)]*password', "Logging password"),
        (r'logging\.[^(]+\([^)]*secret', "Logging secret"),
        (r'debug\s*=\s*True', "Debug mode enabled"),
    ],

    # SSRF (Server-Side Request Forgery)
    VulnerabilityType.SSRF: [
        (r'requests\.(get|post|put|delete)\s*\([^)]*request\.', "requests with user input URL"),
        (r'urllib\.request\.urlopen\s*\([^)]*request\.', "urllib with user input URL"),
        (r'httpx\.(get|post)\s*\([^)]*request\.', "httpx with user input URL"),
    ],
}


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class Vulnerability:
    """Representa uma vulnerabilidade encontrada"""
    vuln_id: str
    vuln_type: VulnerabilityType
    severity: VulnerabilitySeverity
    title: str
    description: str
    file_path: str
    line_number: int
    code_snippet: str = ""
    recommendation: str = ""
    cwe_id: Optional[str] = None
    owasp_category: Optional[str] = None
    detected_by: str = "security_analyzer"
    can_auto_fix: bool = False
    fix_suggestion: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "vuln_id": self.vuln_id,
            "vuln_type": self.vuln_type.value,
            "severity": self.severity.value,
            "title": self.title,
            "description": self.description,
            "file_path": self.file_path,
            "line_number": self.line_number,
            "code_snippet": self.code_snippet,
            "recommendation": self.recommendation,
            "cwe_id": self.cwe_id,
            "owasp_category": self.owasp_category,
            "detected_by": self.detected_by,
            "can_auto_fix": self.can_auto_fix,
            "fix_suggestion": self.fix_suggestion
        }


@dataclass
class SecurityScanConfig:
    """Configuracao do scan de seguranca"""
    scan_types: List[ScanType] = field(default_factory=lambda: [ScanType.SAST])
    use_bandit: bool = True
    include_info: bool = False  # Incluir vulnerabilidades de severidade INFO
    exclude_patterns: List[str] = field(default_factory=lambda: ["test_*", "*_test.py", "tests/*"])
    max_file_size_mb: float = 5.0
    timeout_seconds: int = 300
    verbose: bool = True


@dataclass
class SecurityReport:
    """Relatorio de seguranca completo"""
    report_id: str
    project_id: str
    scan_type: ScanType
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat())

    # Score e estatisticas
    security_score: int = 100  # 0-100
    risk_level: str = "low"    # low, medium, high, critical

    # Contadores por severidade
    critical_count: int = 0
    high_count: int = 0
    medium_count: int = 0
    low_count: int = 0
    info_count: int = 0

    # Vulnerabilidades
    vulnerabilities: List[Vulnerability] = field(default_factory=list)

    # Metricas
    files_scanned: int = 0
    lines_scanned: int = 0
    scan_duration_ms: int = 0

    # Compliance
    owasp_compliance: Dict[str, bool] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "report_id": self.report_id,
            "project_id": self.project_id,
            "scan_type": self.scan_type.value,
            "timestamp": self.timestamp,
            "security_score": self.security_score,
            "risk_level": self.risk_level,
            "summary": {
                "critical": self.critical_count,
                "high": self.high_count,
                "medium": self.medium_count,
                "low": self.low_count,
                "info": self.info_count,
                "total": len(self.vulnerabilities)
            },
            "vulnerabilities": [v.to_dict() for v in self.vulnerabilities],
            "metrics": {
                "files_scanned": self.files_scanned,
                "lines_scanned": self.lines_scanned,
                "scan_duration_ms": self.scan_duration_ms
            },
            "owasp_compliance": self.owasp_compliance
        }

    def to_ascii_report(self) -> str:
        """Gera relatorio em formato ASCII para exibicao"""
        bar_width = 15

        def make_bar(percentage: float) -> str:
            filled = int((percentage / 100) * bar_width)
            empty = bar_width - filled
            return "█" * filled + "░" * empty

        # Determinar cor/simbolo do score
        if self.security_score >= 80:
            score_status = "✓ PASS"
        elif self.security_score >= 60:
            score_status = "⚠ WARN"
        else:
            score_status = "✗ FAIL"

        lines = [
            "┌─────────────────────────────────────────────────┐",
            f"│ 🔒 Security Report - {self.project_id[:20]:<20} │",
            "├─────────────────────────────────────────────────┤",
            f"│ Score: {self.security_score}/100  {make_bar(self.security_score)} ({self.risk_level.upper()}) │",
            "├─────────────────────────────────────────────────┤",
        ]

        # Vulnerabilidades por severidade
        if self.critical_count > 0:
            lines.append(f"│ ❌ CRITICAL ({self.critical_count}){' ' * 30}│")
            for v in [x for x in self.vulnerabilities if x.severity == VulnerabilitySeverity.CRITICAL][:3]:
                lines.append(f"│    └ {v.title[:40]:<40} │")

        if self.high_count > 0:
            lines.append(f"│ ⚠️  HIGH ({self.high_count}){' ' * 33}│")
            for v in [x for x in self.vulnerabilities if x.severity == VulnerabilitySeverity.HIGH][:3]:
                lines.append(f"│    └ {v.title[:40]:<40} │")

        if self.medium_count > 0:
            lines.append(f"│ 💡 MEDIUM ({self.medium_count}){' ' * 31}│")

        if self.low_count > 0:
            lines.append(f"│ ℹ️  LOW ({self.low_count}){' ' * 34}│")

        lines.extend([
            "├─────────────────────────────────────────────────┤",
            f"│ Files: {self.files_scanned} | Lines: {self.lines_scanned} | Time: {self.scan_duration_ms}ms │",
            "└─────────────────────────────────────────────────┘"
        ])

        return "\n".join(lines)


# =============================================================================
# SCANNER BASE
# =============================================================================

class BaseSecurityScanner(ABC):
    """Classe base para scanners de seguranca"""

    def __init__(self, config: SecurityScanConfig):
        self.config = config
        self._log_buffer: List[str] = []

    @abstractmethod
    async def scan(self, project_path: Path) -> List[Vulnerability]:
        """Executa scan e retorna lista de vulnerabilidades"""
        pass

    def _log(self, message: str):
        """Log interno"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        log_msg = f"[{timestamp}] [Security] {message}"
        self._log_buffer.append(log_msg)
        if self.config.verbose:
            print(log_msg)

    def _should_skip_file(self, file_path: Path) -> bool:
        """Verifica se arquivo deve ser ignorado"""
        # Verificar tamanho
        try:
            size_mb = file_path.stat().st_size / (1024 * 1024)
            if size_mb > self.config.max_file_size_mb:
                return True
        except:
            pass

        # Verificar patterns de exclusao
        file_str = str(file_path)
        for pattern in self.config.exclude_patterns:
            if pattern.startswith("*"):
                if file_str.endswith(pattern[1:]):
                    return True
            elif pattern.endswith("*"):
                if pattern[:-1] in file_str:
                    return True
            elif pattern in file_str:
                return True

        return False


# =============================================================================
# SAST SCANNER - Analise Estatica
# =============================================================================

class SASTScanner(BaseSecurityScanner):
    """Scanner de Analise Estatica de Seguranca (SAST)"""

    def __init__(self, config: SecurityScanConfig):
        super().__init__(config)
        self._vuln_counter = 0

    async def scan(self, project_path: Path) -> List[Vulnerability]:
        """Executa analise estatica do codigo"""
        vulnerabilities = []

        self._log(f"Iniciando SAST scan em: {project_path}")

        # Scan com patterns customizados
        pattern_vulns = await self._scan_patterns(project_path)
        vulnerabilities.extend(pattern_vulns)

        # Scan com Bandit (se habilitado e disponivel)
        if self.config.use_bandit:
            bandit_vulns = await self._scan_bandit(project_path)
            vulnerabilities.extend(bandit_vulns)

        self._log(f"SAST scan finalizado. {len(vulnerabilities)} vulnerabilidades encontradas")

        return vulnerabilities

    async def _scan_patterns(self, project_path: Path) -> List[Vulnerability]:
        """Scan usando patterns regex customizados"""
        vulnerabilities = []

        # Escanear arquivos Python
        for py_file in project_path.rglob("*.py"):
            if self._should_skip_file(py_file):
                continue

            try:
                content = py_file.read_text(encoding="utf-8", errors="ignore")
                lines = content.split("\n")

                for vuln_type, patterns in SECURITY_PATTERNS.items():
                    for pattern, description in patterns:
                        for line_num, line in enumerate(lines, 1):
                            if re.search(pattern, line, re.IGNORECASE):
                                vuln = self._create_vulnerability(
                                    vuln_type=vuln_type,
                                    title=description,
                                    file_path=str(py_file.relative_to(project_path)),
                                    line_number=line_num,
                                    code_snippet=line.strip()[:100],
                                    detected_by="pattern_scanner"
                                )
                                vulnerabilities.append(vuln)

            except Exception as e:
                self._log(f"Erro ao analisar {py_file}: {e}")

        # Escanear arquivos JavaScript/TypeScript
        for js_file in list(project_path.rglob("*.js")) + list(project_path.rglob("*.ts")):
            if self._should_skip_file(js_file):
                continue

            try:
                content = js_file.read_text(encoding="utf-8", errors="ignore")
                lines = content.split("\n")

                # Patterns especificos para JS
                js_patterns = [
                    (VulnerabilityType.XSS, r'\.innerHTML\s*=', "Direct innerHTML assignment"),
                    (VulnerabilityType.XSS, r'document\.write\s*\(', "document.write usage"),
                    (VulnerabilityType.COMMAND_INJECTION, r'eval\s*\(', "eval usage"),
                    (VulnerabilityType.HARDCODED_SECRET, r'(api_?key|secret|password)\s*[:=]\s*["\'][^"\']+["\']', "Hardcoded credential"),
                ]

                for vuln_type, pattern, description in js_patterns:
                    for line_num, line in enumerate(lines, 1):
                        if re.search(pattern, line, re.IGNORECASE):
                            vuln = self._create_vulnerability(
                                vuln_type=vuln_type,
                                title=description,
                                file_path=str(js_file.relative_to(project_path)),
                                line_number=line_num,
                                code_snippet=line.strip()[:100],
                                detected_by="pattern_scanner"
                            )
                            vulnerabilities.append(vuln)

            except Exception as e:
                self._log(f"Erro ao analisar {js_file}: {e}")

        return vulnerabilities

    async def _scan_bandit(self, project_path: Path) -> List[Vulnerability]:
        """Executa Bandit para analise de seguranca Python"""
        vulnerabilities = []

        try:
            # Verificar se bandit esta instalado
            result = subprocess.run(
                ["bandit", "--version"],
                capture_output=True,
                text=True,
                timeout=10
            )

            if result.returncode != 0:
                self._log("Bandit nao encontrado. Pulando scan com Bandit.")
                return vulnerabilities

        except (FileNotFoundError, subprocess.TimeoutExpired):
            self._log("Bandit nao encontrado. Pulando scan com Bandit.")
            return vulnerabilities

        try:
            self._log("Executando Bandit...")

            # Executar Bandit com output JSON
            result = subprocess.run(
                [
                    "bandit",
                    "-r", str(project_path),
                    "-f", "json",
                    "-ll",  # Low and above severity
                    "--exclude", ".git,node_modules,venv,__pycache__"
                ],
                capture_output=True,
                text=True,
                timeout=self.config.timeout_seconds
            )

            # Bandit retorna codigo 1 quando encontra issues
            if result.stdout:
                bandit_output = json.loads(result.stdout)

                for issue in bandit_output.get("results", []):
                    severity = self._map_bandit_severity(issue.get("issue_severity", "LOW"))
                    vuln_type = self._map_bandit_type(issue.get("test_id", ""))

                    vuln = Vulnerability(
                        vuln_id=f"BANDIT-{self._next_vuln_id()}",
                        vuln_type=vuln_type,
                        severity=severity,
                        title=issue.get("issue_text", "Security issue"),
                        description=f"Bandit {issue.get('test_id')}: {issue.get('issue_text')}",
                        file_path=issue.get("filename", "").replace(str(project_path) + "/", ""),
                        line_number=issue.get("line_number", 0),
                        code_snippet=issue.get("code", "")[:200],
                        recommendation=issue.get("more_info", ""),
                        cwe_id=issue.get("issue_cwe", {}).get("id"),
                        detected_by="bandit"
                    )
                    vulnerabilities.append(vuln)

                self._log(f"Bandit encontrou {len(vulnerabilities)} issues")

        except subprocess.TimeoutExpired:
            self._log("Bandit timeout")
        except json.JSONDecodeError:
            self._log("Erro ao parsear output do Bandit")
        except Exception as e:
            self._log(f"Erro ao executar Bandit: {e}")

        return vulnerabilities

    def _create_vulnerability(
        self,
        vuln_type: VulnerabilityType,
        title: str,
        file_path: str,
        line_number: int,
        code_snippet: str,
        detected_by: str
    ) -> Vulnerability:
        """Cria objeto Vulnerability com defaults apropriados"""

        # Determinar severidade baseado no tipo
        severity_map = {
            VulnerabilityType.SQL_INJECTION: VulnerabilitySeverity.CRITICAL,
            VulnerabilityType.COMMAND_INJECTION: VulnerabilitySeverity.CRITICAL,
            VulnerabilityType.HARDCODED_SECRET: VulnerabilitySeverity.HIGH,
            VulnerabilityType.XSS: VulnerabilitySeverity.HIGH,
            VulnerabilityType.PATH_TRAVERSAL: VulnerabilitySeverity.HIGH,
            VulnerabilityType.UNSAFE_DESERIALIZATION: VulnerabilitySeverity.HIGH,
            VulnerabilityType.SSRF: VulnerabilitySeverity.HIGH,
            VulnerabilityType.CRYPTO_WEAKNESS: VulnerabilitySeverity.MEDIUM,
            VulnerabilityType.SENSITIVE_DATA_EXPOSURE: VulnerabilitySeverity.MEDIUM,
            VulnerabilityType.SECURITY_MISCONFIGURATION: VulnerabilitySeverity.MEDIUM,
            VulnerabilityType.INSUFFICIENT_LOGGING: VulnerabilitySeverity.LOW,
        }

        severity = severity_map.get(vuln_type, VulnerabilitySeverity.MEDIUM)

        # Recomendacoes por tipo
        recommendations = {
            VulnerabilityType.SQL_INJECTION: "Use parameterized queries ou ORM. Nunca concatene strings em queries SQL.",
            VulnerabilityType.COMMAND_INJECTION: "Evite shell=True. Use subprocess com lista de argumentos e valide inputs.",
            VulnerabilityType.HARDCODED_SECRET: "Mova credenciais para variaveis de ambiente ou secret manager.",
            VulnerabilityType.XSS: "Escape output HTML. Use template engines seguros.",
            VulnerabilityType.PATH_TRAVERSAL: "Valide e sanitize paths. Use os.path.normpath e whitelist.",
            VulnerabilityType.UNSAFE_DESERIALIZATION: "Use yaml.safe_load() e evite pickle com dados nao confiaveis.",
            VulnerabilityType.CRYPTO_WEAKNESS: "Use algoritmos modernos: SHA256+, AES, secrets.token_bytes().",
            VulnerabilityType.SENSITIVE_DATA_EXPOSURE: "Nunca logue ou imprima dados sensiveis. Use mascaramento.",
            VulnerabilityType.SSRF: "Valide e whitelist URLs permitidas. Bloqueie ranges de IP internos.",
        }

        # CWE IDs por tipo
        cwe_map = {
            VulnerabilityType.SQL_INJECTION: "CWE-89",
            VulnerabilityType.XSS: "CWE-79",
            VulnerabilityType.COMMAND_INJECTION: "CWE-78",
            VulnerabilityType.PATH_TRAVERSAL: "CWE-22",
            VulnerabilityType.HARDCODED_SECRET: "CWE-798",
            VulnerabilityType.UNSAFE_DESERIALIZATION: "CWE-502",
            VulnerabilityType.CRYPTO_WEAKNESS: "CWE-327",
            VulnerabilityType.SSRF: "CWE-918",
        }

        # OWASP Categories
        owasp_map = {
            VulnerabilityType.SQL_INJECTION: "A03:2021 - Injection",
            VulnerabilityType.XSS: "A03:2021 - Injection",
            VulnerabilityType.COMMAND_INJECTION: "A03:2021 - Injection",
            VulnerabilityType.BROKEN_AUTH: "A07:2021 - Identification and Authentication Failures",
            VulnerabilityType.SENSITIVE_DATA_EXPOSURE: "A02:2021 - Cryptographic Failures",
            VulnerabilityType.HARDCODED_SECRET: "A02:2021 - Cryptographic Failures",
            VulnerabilityType.BROKEN_ACCESS_CONTROL: "A01:2021 - Broken Access Control",
            VulnerabilityType.SECURITY_MISCONFIGURATION: "A05:2021 - Security Misconfiguration",
            VulnerabilityType.INSECURE_DEPENDENCY: "A06:2021 - Vulnerable and Outdated Components",
            VulnerabilityType.SSRF: "A10:2021 - Server-Side Request Forgery",
        }

        return Vulnerability(
            vuln_id=f"VULN-{self._next_vuln_id()}",
            vuln_type=vuln_type,
            severity=severity,
            title=title,
            description=f"{vuln_type.value}: {title}",
            file_path=file_path,
            line_number=line_number,
            code_snippet=code_snippet,
            recommendation=recommendations.get(vuln_type, "Review and fix the security issue."),
            cwe_id=cwe_map.get(vuln_type),
            owasp_category=owasp_map.get(vuln_type),
            detected_by=detected_by
        )

    def _next_vuln_id(self) -> str:
        """Gera proximo ID de vulnerabilidade"""
        self._vuln_counter += 1
        return f"{self._vuln_counter:04d}"

    def _map_bandit_severity(self, severity: str) -> VulnerabilitySeverity:
        """Mapeia severidade do Bandit para enum"""
        mapping = {
            "HIGH": VulnerabilitySeverity.HIGH,
            "MEDIUM": VulnerabilitySeverity.MEDIUM,
            "LOW": VulnerabilitySeverity.LOW,
        }
        return mapping.get(severity.upper(), VulnerabilitySeverity.MEDIUM)

    def _map_bandit_type(self, test_id: str) -> VulnerabilityType:
        """Mapeia test_id do Bandit para tipo de vulnerabilidade"""
        # Mapear IDs comuns do Bandit
        mappings = {
            "B101": VulnerabilityType.OTHER,  # assert_used
            "B102": VulnerabilityType.COMMAND_INJECTION,  # exec_used
            "B103": VulnerabilityType.SECURITY_MISCONFIGURATION,  # set_bad_file_permissions
            "B104": VulnerabilityType.SECURITY_MISCONFIGURATION,  # hardcoded_bind_all_interfaces
            "B105": VulnerabilityType.HARDCODED_SECRET,  # hardcoded_password_string
            "B106": VulnerabilityType.HARDCODED_SECRET,  # hardcoded_password_funcarg
            "B107": VulnerabilityType.HARDCODED_SECRET,  # hardcoded_password_default
            "B108": VulnerabilityType.PATH_TRAVERSAL,  # hardcoded_tmp_directory
            "B110": VulnerabilityType.OTHER,  # try_except_pass
            "B112": VulnerabilityType.OTHER,  # try_except_continue
            "B201": VulnerabilityType.COMMAND_INJECTION,  # flask_debug_true
            "B301": VulnerabilityType.UNSAFE_DESERIALIZATION,  # pickle
            "B302": VulnerabilityType.UNSAFE_DESERIALIZATION,  # marshal
            "B303": VulnerabilityType.CRYPTO_WEAKNESS,  # md5
            "B304": VulnerabilityType.CRYPTO_WEAKNESS,  # des
            "B305": VulnerabilityType.CRYPTO_WEAKNESS,  # cipher
            "B306": VulnerabilityType.OTHER,  # mktemp
            "B307": VulnerabilityType.COMMAND_INJECTION,  # eval
            "B308": VulnerabilityType.XSS,  # mark_safe
            "B310": VulnerabilityType.SSRF,  # urllib_urlopen
            "B311": VulnerabilityType.CRYPTO_WEAKNESS,  # random
            "B312": VulnerabilityType.OTHER,  # telnetlib
            "B313": VulnerabilityType.XXE,  # xml_bad_cElementTree
            "B314": VulnerabilityType.XXE,  # xml_bad_ElementTree
            "B320": VulnerabilityType.XXE,  # xml_bad_sax
            "B321": VulnerabilityType.OTHER,  # ftplib
            "B323": VulnerabilityType.SECURITY_MISCONFIGURATION,  # unverified_context
            "B324": VulnerabilityType.CRYPTO_WEAKNESS,  # hashlib
            "B501": VulnerabilityType.SECURITY_MISCONFIGURATION,  # request_with_no_cert_validation
            "B502": VulnerabilityType.SECURITY_MISCONFIGURATION,  # ssl_with_bad_version
            "B503": VulnerabilityType.SECURITY_MISCONFIGURATION,  # ssl_with_bad_defaults
            "B504": VulnerabilityType.SECURITY_MISCONFIGURATION,  # ssl_with_no_version
            "B505": VulnerabilityType.CRYPTO_WEAKNESS,  # weak_cryptographic_key
            "B506": VulnerabilityType.UNSAFE_DESERIALIZATION,  # yaml_load
            "B507": VulnerabilityType.SECURITY_MISCONFIGURATION,  # ssh_no_host_key_verification
            "B601": VulnerabilityType.COMMAND_INJECTION,  # paramiko_calls
            "B602": VulnerabilityType.COMMAND_INJECTION,  # subprocess_popen_with_shell_equals_true
            "B603": VulnerabilityType.COMMAND_INJECTION,  # subprocess_without_shell_equals_true
            "B604": VulnerabilityType.COMMAND_INJECTION,  # any_other_function_with_shell_equals_true
            "B605": VulnerabilityType.COMMAND_INJECTION,  # start_process_with_a_shell
            "B606": VulnerabilityType.COMMAND_INJECTION,  # start_process_with_no_shell
            "B607": VulnerabilityType.COMMAND_INJECTION,  # start_process_with_partial_path
            "B608": VulnerabilityType.SQL_INJECTION,  # hardcoded_sql_expressions
            "B609": VulnerabilityType.COMMAND_INJECTION,  # linux_commands_wildcard_injection
            "B610": VulnerabilityType.SQL_INJECTION,  # django_extra_used
            "B611": VulnerabilityType.SQL_INJECTION,  # django_rawsql_used
            "B701": VulnerabilityType.XSS,  # jinja2_autoescape_false
            "B702": VulnerabilityType.XSS,  # use_of_mako_templates
            "B703": VulnerabilityType.XSS,  # django_mark_safe
        }
        return mappings.get(test_id, VulnerabilityType.OTHER)


# =============================================================================
# SCA SCANNER - Software Composition Analysis
# =============================================================================

class SCAScanner(BaseSecurityScanner):
    """Scanner de Analise de Composicao de Software (dependencias)"""

    async def scan(self, project_path: Path) -> List[Vulnerability]:
        """Analisa dependencias em busca de vulnerabilidades conhecidas"""
        vulnerabilities = []

        self._log(f"Iniciando SCA scan em: {project_path}")

        # Verificar requirements.txt
        req_file = project_path / "requirements.txt"
        if req_file.exists():
            vulns = await self._scan_python_deps(req_file)
            vulnerabilities.extend(vulns)

        # Verificar package.json
        pkg_file = project_path / "package.json"
        if pkg_file.exists():
            vulns = await self._scan_node_deps(pkg_file)
            vulnerabilities.extend(vulns)

        return vulnerabilities

    async def _scan_python_deps(self, req_file: Path) -> List[Vulnerability]:
        """Scan dependencias Python usando pip-audit ou safety"""
        vulnerabilities = []

        try:
            # Tentar pip-audit primeiro
            result = subprocess.run(
                ["pip-audit", "-r", str(req_file), "-f", "json", "--progress-spinner=off"],
                capture_output=True,
                text=True,
                timeout=120
            )

            if result.stdout:
                try:
                    audit_data = json.loads(result.stdout)
                    for dep in audit_data.get("dependencies", []):
                        for vuln in dep.get("vulns", []):
                            vulnerabilities.append(Vulnerability(
                                vuln_id=f"DEP-{vuln.get('id', 'UNKNOWN')}",
                                vuln_type=VulnerabilityType.INSECURE_DEPENDENCY,
                                severity=self._map_cvss_severity(vuln.get("fix_versions", [])),
                                title=f"Vulnerable dependency: {dep.get('name')}",
                                description=vuln.get("description", "Known vulnerability in dependency"),
                                file_path="requirements.txt",
                                line_number=0,
                                recommendation=f"Update to version: {', '.join(vuln.get('fix_versions', ['latest']))}",
                                detected_by="pip-audit"
                            ))
                except json.JSONDecodeError:
                    pass

        except FileNotFoundError:
            self._log("pip-audit nao encontrado")
        except subprocess.TimeoutExpired:
            self._log("pip-audit timeout")
        except Exception as e:
            self._log(f"Erro no pip-audit: {e}")

        return vulnerabilities

    async def _scan_node_deps(self, pkg_file: Path) -> List[Vulnerability]:
        """Scan dependencias Node.js usando npm audit"""
        vulnerabilities = []

        try:
            result = subprocess.run(
                ["npm", "audit", "--json"],
                cwd=str(pkg_file.parent),
                capture_output=True,
                text=True,
                timeout=120
            )

            if result.stdout:
                try:
                    audit_data = json.loads(result.stdout)

                    for vuln_id, vuln in audit_data.get("vulnerabilities", {}).items():
                        severity = self._map_npm_severity(vuln.get("severity", "low"))
                        vulnerabilities.append(Vulnerability(
                            vuln_id=f"NPM-{vuln_id}",
                            vuln_type=VulnerabilityType.INSECURE_DEPENDENCY,
                            severity=severity,
                            title=f"Vulnerable dependency: {vuln.get('name', vuln_id)}",
                            description=vuln.get("title", "Known vulnerability"),
                            file_path="package.json",
                            line_number=0,
                            recommendation=vuln.get("fixAvailable", {}).get("name", "Update to latest version"),
                            detected_by="npm-audit"
                        ))
                except json.JSONDecodeError:
                    pass

        except FileNotFoundError:
            self._log("npm nao encontrado")
        except subprocess.TimeoutExpired:
            self._log("npm audit timeout")
        except Exception as e:
            self._log(f"Erro no npm audit: {e}")

        return vulnerabilities

    def _map_cvss_severity(self, fix_versions: List[str]) -> VulnerabilitySeverity:
        """Mapeia severidade baseado em fix versions disponiveis"""
        if not fix_versions:
            return VulnerabilitySeverity.HIGH
        return VulnerabilitySeverity.MEDIUM

    def _map_npm_severity(self, severity: str) -> VulnerabilitySeverity:
        """Mapeia severidade do npm para enum"""
        mapping = {
            "critical": VulnerabilitySeverity.CRITICAL,
            "high": VulnerabilitySeverity.HIGH,
            "moderate": VulnerabilitySeverity.MEDIUM,
            "low": VulnerabilitySeverity.LOW,
            "info": VulnerabilitySeverity.INFO,
        }
        return mapping.get(severity.lower(), VulnerabilitySeverity.MEDIUM)


# =============================================================================
# CLASSE PRINCIPAL - SECURITY ANALYZER
# =============================================================================

class SecurityAnalyzer:
    """
    Analisador de Seguranca Principal

    Coordena os diferentes scanners (SAST, SCA) e gera relatorio consolidado.

    Uso:
        analyzer = SecurityAnalyzer(config)
        report = await analyzer.analyze(project_path)
        print(report.to_ascii_report())
    """

    PROJECTS_DIR = Path(r"C:\Users\lcruz\Fabrica de Agentes\projects")

    def __init__(self, config: SecurityScanConfig = None):
        self.config = config or SecurityScanConfig()
        self._scanners: Dict[ScanType, BaseSecurityScanner] = {}
        self._setup_scanners()

    def _setup_scanners(self):
        """Configura os scanners disponíveis"""
        if ScanType.SAST in self.config.scan_types:
            self._scanners[ScanType.SAST] = SASTScanner(self.config)
        if ScanType.SCA in self.config.scan_types:
            self._scanners[ScanType.SCA] = SCAScanner(self.config)

    def _find_project_path(self, project_id: str) -> Optional[Path]:
        """Encontra o caminho do projeto (tenta varias convencoes de nome)."""
        possible_names = [
            project_id,
            project_id.lower(),
            project_id.lower().replace("-", "_"),
            project_id.replace("-", "_"),
        ]

        for name in possible_names:
            path = self.PROJECTS_DIR / name
            if path.exists():
                return path

        return None

    async def analyze(self, project_id_or_path: str) -> SecurityReport:
        """
        Executa analise de seguranca completa em um projeto.

        Args:
            project_id_or_path: ID do projeto ou caminho direto

        Returns:
            SecurityReport com todas as vulnerabilidades encontradas
        """
        start_time = datetime.now()

        # Determinar path do projeto
        if os.path.isabs(project_id_or_path) and os.path.exists(project_id_or_path):
            project_path = Path(project_id_or_path)
            project_id = project_path.name
        else:
            project_path = self._find_project_path(project_id_or_path)
            project_id = project_id_or_path

            if not project_path or not project_path.exists():
                # Retornar report vazio com erro
                return SecurityReport(
                    report_id=f"SEC-{datetime.now().strftime('%Y%m%d%H%M%S')}",
                    project_id=project_id,
                    scan_type=ScanType.SAST,
                    security_score=0,
                    risk_level="unknown"
                )

        # Coletar vulnerabilidades de todos os scanners
        all_vulnerabilities: List[Vulnerability] = []
        files_scanned = 0
        lines_scanned = 0

        for scan_type, scanner in self._scanners.items():
            vulns = await scanner.scan(project_path)
            all_vulnerabilities.extend(vulns)

        # Contar arquivos e linhas
        for py_file in project_path.rglob("*.py"):
            if not scanner._should_skip_file(py_file):
                files_scanned += 1
                try:
                    lines_scanned += len(py_file.read_text(encoding="utf-8", errors="ignore").split("\n"))
                except:
                    pass

        # Filtrar vulnerabilidades INFO se configurado
        if not self.config.include_info:
            all_vulnerabilities = [v for v in all_vulnerabilities if v.severity != VulnerabilitySeverity.INFO]

        # Deduplicate vulnerabilidades (mesmo arquivo, linha e tipo)
        seen = set()
        unique_vulns = []
        for v in all_vulnerabilities:
            key = (v.file_path, v.line_number, v.vuln_type)
            if key not in seen:
                seen.add(key)
                unique_vulns.append(v)

        # Calcular estatisticas
        critical_count = len([v for v in unique_vulns if v.severity == VulnerabilitySeverity.CRITICAL])
        high_count = len([v for v in unique_vulns if v.severity == VulnerabilitySeverity.HIGH])
        medium_count = len([v for v in unique_vulns if v.severity == VulnerabilitySeverity.MEDIUM])
        low_count = len([v for v in unique_vulns if v.severity == VulnerabilitySeverity.LOW])
        info_count = len([v for v in unique_vulns if v.severity == VulnerabilitySeverity.INFO])

        # Calcular score (100 - penalidades)
        penalty = (
            critical_count * SEVERITY_WEIGHTS[VulnerabilitySeverity.CRITICAL] +
            high_count * SEVERITY_WEIGHTS[VulnerabilitySeverity.HIGH] +
            medium_count * SEVERITY_WEIGHTS[VulnerabilitySeverity.MEDIUM] +
            low_count * SEVERITY_WEIGHTS[VulnerabilitySeverity.LOW] +
            info_count * SEVERITY_WEIGHTS[VulnerabilitySeverity.INFO]
        )
        security_score = max(0, 100 - penalty)

        # Determinar nivel de risco
        if critical_count > 0 or security_score < 40:
            risk_level = "critical"
        elif high_count > 0 or security_score < 60:
            risk_level = "high"
        elif medium_count > 0 or security_score < 80:
            risk_level = "medium"
        else:
            risk_level = "low"

        # Calcular OWASP compliance
        owasp_compliance = self._calculate_owasp_compliance(unique_vulns)

        # Calcular duracao
        duration_ms = int((datetime.now() - start_time).total_seconds() * 1000)

        # Criar report
        report = SecurityReport(
            report_id=f"SEC-{datetime.now().strftime('%Y%m%d%H%M%S')}",
            project_id=project_id,
            scan_type=ScanType.SAST,
            security_score=security_score,
            risk_level=risk_level,
            critical_count=critical_count,
            high_count=high_count,
            medium_count=medium_count,
            low_count=low_count,
            info_count=info_count,
            vulnerabilities=unique_vulns,
            files_scanned=files_scanned,
            lines_scanned=lines_scanned,
            scan_duration_ms=duration_ms,
            owasp_compliance=owasp_compliance
        )

        return report

    def _calculate_owasp_compliance(self, vulnerabilities: List[Vulnerability]) -> Dict[str, bool]:
        """Calcula compliance com OWASP Top 10 2021"""
        owasp_categories = {
            "A01:2021 - Broken Access Control": True,
            "A02:2021 - Cryptographic Failures": True,
            "A03:2021 - Injection": True,
            "A04:2021 - Insecure Design": True,
            "A05:2021 - Security Misconfiguration": True,
            "A06:2021 - Vulnerable Components": True,
            "A07:2021 - Auth Failures": True,
            "A08:2021 - Data Integrity": True,
            "A09:2021 - Logging Failures": True,
            "A10:2021 - SSRF": True,
        }

        # Marcar como nao-compliant se houver vulnerabilidades criticas/altas
        for vuln in vulnerabilities:
            if vuln.owasp_category and vuln.severity in [VulnerabilitySeverity.CRITICAL, VulnerabilitySeverity.HIGH]:
                for key in owasp_categories:
                    if vuln.owasp_category.startswith(key[:3]):
                        owasp_categories[key] = False

        return owasp_compliance


# =============================================================================
# FUNCOES AUXILIARES PARA INTEGRACAO
# =============================================================================

async def scan_project(project_id: str, config: SecurityScanConfig = None) -> SecurityReport:
    """
    Funcao helper para escanear um projeto.

    Args:
        project_id: ID do projeto
        config: Configuracao opcional

    Returns:
        SecurityReport
    """
    analyzer = SecurityAnalyzer(config)
    return await analyzer.analyze(project_id)


async def scan_story_code(story_id: str, code_files: List[str]) -> SecurityReport:
    """
    Escaneia codigo gerado para uma story especifica.

    Args:
        story_id: ID da story
        code_files: Lista de arquivos de codigo

    Returns:
        SecurityReport
    """
    # Criar diretorio temporario com os arquivos
    import tempfile
    import shutil

    with tempfile.TemporaryDirectory() as tmpdir:
        tmppath = Path(tmpdir)

        for file_path in code_files:
            if os.path.exists(file_path):
                shutil.copy(file_path, tmppath / os.path.basename(file_path))

        analyzer = SecurityAnalyzer()
        report = await analyzer.analyze(str(tmppath))
        report.project_id = story_id

        return report


def get_security_badge(score: int) -> Dict[str, str]:
    """
    Retorna dados para badge de seguranca.

    Args:
        score: Score de seguranca (0-100)

    Returns:
        Dict com cor, icone e label
    """
    if score >= 80:
        return {
            "color": "#10B981",  # Verde
            "icon": "shield-check",
            "label": "Secure",
            "text_color": "white"
        }
    elif score >= 60:
        return {
            "color": "#F59E0B",  # Amarelo
            "icon": "shield-exclamation",
            "label": "Warning",
            "text_color": "black"
        }
    elif score >= 40:
        return {
            "color": "#F97316",  # Laranja
            "icon": "shield-x",
            "label": "At Risk",
            "text_color": "white"
        }
    else:
        return {
            "color": "#EF4444",  # Vermelho
            "icon": "shield-x",
            "label": "Critical",
            "text_color": "white"
        }


# =============================================================================
# CLI / MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Security Analyzer - Analise de Seguranca SAST/DAST"
    )
    parser.add_argument(
        "project",
        help="ID do projeto ou caminho para o codigo"
    )
    parser.add_argument(
        "-o", "--output",
        choices=["json", "ascii", "html"],
        default="ascii",
        help="Formato de saida"
    )
    parser.add_argument(
        "--no-bandit",
        action="store_true",
        help="Desabilitar scan com Bandit"
    )
    parser.add_argument(
        "--include-info",
        action="store_true",
        help="Incluir vulnerabilidades de severidade INFO"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Modo verbose"
    )

    args = parser.parse_args()

    config = SecurityScanConfig(
        use_bandit=not args.no_bandit,
        include_info=args.include_info,
        verbose=args.verbose
    )

    async def main():
        analyzer = SecurityAnalyzer(config)

        print(f"\n[SecurityAnalyzer] Analisando: {args.project}")
        print(f"[SecurityAnalyzer] Bandit: {'ON' if config.use_bandit else 'OFF'}")
        print("-" * 50)

        report = await analyzer.analyze(args.project)

        if args.output == "json":
            print(json.dumps(report.to_dict(), indent=2))
        elif args.output == "html":
            # TODO: Gerar HTML report
            print(report.to_ascii_report())
        else:
            print(report.to_ascii_report())

        # Retornar codigo de erro se houver vulnerabilidades criticas
        if report.critical_count > 0:
            sys.exit(1)

    asyncio.run(main())
