"""
Code Validator - AST-based Security Analysis
Issue #201: Code validation and sanitization

Provides:
- AST parsing for dangerous code detection
- Import whitelist enforcement
- Dangerous function call detection
- Code sanitization
"""
import ast
import re
import logging
from typing import Optional, List, Set, Dict, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class SecuritySeverity(Enum):
    """Severity levels for security issues"""
    CRITICAL = "critical"
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
    INFO = "info"


@dataclass
class SecurityIssue:
    """Represents a security issue found in code"""
    severity: SecuritySeverity
    category: str
    message: str
    line: int
    column: int
    code_snippet: str = ""
    suggestion: str = ""


@dataclass
class ValidationResult:
    """Result of code validation"""
    is_safe: bool
    issues: List[SecurityIssue] = field(default_factory=list)
    blocked: bool = False
    block_reason: str = ""


# Import whitelist - allowed imports
ALLOWED_IMPORTS: Set[str] = {
    # Standard library - safe
    "json",
    "datetime",
    "math",
    "random",
    "collections",
    "itertools",
    "functools",
    "typing",
    "dataclasses",
    "enum",
    "re",
    "string",
    "textwrap",
    "uuid",
    "hashlib",
    "base64",
    "copy",
    "decimal",
    "fractions",
    "statistics",
    "time",  # time.sleep allowed but monitored
    "calendar",
    "zoneinfo",

    # Data processing
    "pandas",
    "numpy",
    "scipy",

    # HTTP (with restrictions)
    "requests",
    "httpx",
    "aiohttp",

    # Parsing
    "csv",
    "xml.etree.ElementTree",
    "html.parser",
    "yaml",
    "toml",

    # Logging (safe)
    "logging",

    # Testing
    "unittest",
    "pytest",
}

# Blocked imports - never allowed
BLOCKED_IMPORTS: Set[str] = {
    "os",
    "sys",
    "subprocess",
    "shutil",
    "pathlib",
    "glob",
    "fnmatch",
    "socket",
    "ssl",
    "ftplib",
    "smtplib",
    "telnetlib",
    "ctypes",
    "multiprocessing",
    "threading",
    "pickle",
    "marshal",
    "shelve",
    "dbm",
    "sqlite3",
    "importlib",
    "builtins",
    "__future__",
    "code",
    "codeop",
    "compileall",
    "py_compile",
    "dis",
    "inspect",
    "gc",
    "traceback",
    "linecache",
    "tokenize",
    "tabnanny",
    "pyclbr",
    "symtable",
}

# Dangerous function calls
DANGEROUS_CALLS: Dict[str, str] = {
    # OS commands
    "os.system": "Executes shell commands",
    "os.popen": "Opens pipe to/from command",
    "os.exec": "Replaces current process",
    "os.execl": "Replaces current process",
    "os.execle": "Replaces current process",
    "os.execlp": "Replaces current process",
    "os.execlpe": "Replaces current process",
    "os.execv": "Replaces current process",
    "os.execve": "Replaces current process",
    "os.execvp": "Replaces current process",
    "os.execvpe": "Replaces current process",
    "os.spawn": "Spawns new process",
    "os.spawnl": "Spawns new process",
    "os.spawnle": "Spawns new process",
    "os.spawnlp": "Spawns new process",
    "os.spawnlpe": "Spawns new process",
    "os.spawnv": "Spawns new process",
    "os.spawnve": "Spawns new process",
    "os.spawnvp": "Spawns new process",
    "os.spawnvpe": "Spawns new process",
    "os.fork": "Creates child process",
    "os.forkpty": "Creates child process with pty",
    "os.kill": "Sends signal to process",
    "os.killpg": "Sends signal to process group",
    "os.remove": "Deletes file",
    "os.unlink": "Deletes file",
    "os.rmdir": "Removes directory",
    "os.removedirs": "Removes directories",
    "os.rename": "Renames file",
    "os.renames": "Renames file",
    "os.replace": "Replaces file",
    "os.mkdir": "Creates directory",
    "os.makedirs": "Creates directories",
    "os.chdir": "Changes directory",
    "os.chroot": "Changes root directory",
    "os.chmod": "Changes permissions",
    "os.chown": "Changes ownership",
    "os.environ": "Access to environment variables",
    "os.getenv": "Access to environment variables",
    "os.putenv": "Modifies environment",
    "os.setenv": "Modifies environment",

    # Subprocess
    "subprocess.run": "Executes command",
    "subprocess.call": "Executes command",
    "subprocess.check_call": "Executes command",
    "subprocess.check_output": "Executes command",
    "subprocess.Popen": "Creates subprocess",
    "subprocess.getoutput": "Executes command",
    "subprocess.getstatusoutput": "Executes command",

    # Code execution
    "eval": "Executes arbitrary code",
    "exec": "Executes arbitrary code",
    "compile": "Compiles code",
    "__import__": "Dynamic import",
    "importlib.import_module": "Dynamic import",

    # File operations (unless whitelisted)
    "open": "File access",
    "file": "File access",

    # Network
    "socket.socket": "Network socket",
    "urllib.request.urlopen": "Network request",

    # Dangerous builtins
    "globals": "Access to global namespace",
    "locals": "Access to local namespace",
    "vars": "Access to namespace",
    "dir": "Object introspection",
    "getattr": "Attribute access (can be dangerous)",
    "setattr": "Attribute modification",
    "delattr": "Attribute deletion",
    "hasattr": "Attribute check",
    "type": "Type manipulation",
    "object.__subclasses__": "Class introspection",
    "object.__mro__": "Class introspection",
    "object.__bases__": "Class introspection",

    # Pickle
    "pickle.load": "Arbitrary code execution",
    "pickle.loads": "Arbitrary code execution",
    "cPickle.load": "Arbitrary code execution",
    "cPickle.loads": "Arbitrary code execution",

    # Other dangerous
    "input": "User input (can be exploited)",
    "breakpoint": "Debugger",
    "help": "Interactive help",
}

# String patterns that indicate malicious intent
DANGEROUS_PATTERNS: List[Tuple[str, str, SecuritySeverity]] = [
    (r"__.*__", "Dunder attribute access", SecuritySeverity.HIGH),
    (r"import\s+os", "OS import detected", SecuritySeverity.CRITICAL),
    (r"import\s+subprocess", "Subprocess import detected", SecuritySeverity.CRITICAL),
    (r"from\s+os\s+import", "OS import detected", SecuritySeverity.CRITICAL),
    (r"exec\s*\(", "Exec call detected", SecuritySeverity.CRITICAL),
    (r"eval\s*\(", "Eval call detected", SecuritySeverity.CRITICAL),
    (r"__import__\s*\(", "Dynamic import detected", SecuritySeverity.CRITICAL),
    (r"\.system\s*\(", "System call detected", SecuritySeverity.CRITICAL),
    (r"\.popen\s*\(", "Popen call detected", SecuritySeverity.CRITICAL),
    (r"subprocess\.", "Subprocess usage detected", SecuritySeverity.CRITICAL),
    (r"open\s*\([^)]*['\"]w", "File write detected", SecuritySeverity.HIGH),
    (r"/etc/passwd", "Sensitive file access", SecuritySeverity.CRITICAL),
    (r"/etc/shadow", "Sensitive file access", SecuritySeverity.CRITICAL),
    (r"\.env", "Environment file access", SecuritySeverity.HIGH),
    (r"rm\s+-rf", "Destructive command", SecuritySeverity.CRITICAL),
    (r"curl\s+", "Network command", SecuritySeverity.MEDIUM),
    (r"wget\s+", "Network command", SecuritySeverity.MEDIUM),
    (r"nc\s+-", "Netcat usage", SecuritySeverity.CRITICAL),
    (r"python\s+-c", "Python command execution", SecuritySeverity.HIGH),
    (r"bash\s+-c", "Bash command execution", SecuritySeverity.CRITICAL),
    (r"sh\s+-c", "Shell command execution", SecuritySeverity.CRITICAL),
]


class CodeValidator(ast.NodeVisitor):
    """
    AST-based code validator for security analysis.

    Checks for:
    - Dangerous imports
    - Dangerous function calls
    - Suspicious patterns
    - Unsafe constructs
    """

    def __init__(self, allowed_imports: Optional[Set[str]] = None):
        self.allowed_imports = allowed_imports or ALLOWED_IMPORTS
        self.issues: List[SecurityIssue] = []
        self.source_lines: List[str] = []

    def validate(self, code: str) -> ValidationResult:
        """
        Validate Python code for security issues.

        Args:
            code: Python source code

        Returns:
            ValidationResult with issues found
        """
        self.issues = []
        self.source_lines = code.split('\n')

        # First, check string patterns (fast)
        self._check_patterns(code)

        # Then, parse and analyze AST
        try:
            tree = ast.parse(code)
            self.visit(tree)
        except SyntaxError as e:
            self.issues.append(SecurityIssue(
                severity=SecuritySeverity.INFO,
                category="syntax",
                message=f"Syntax error: {e.msg}",
                line=e.lineno or 0,
                column=e.offset or 0
            ))

        # Determine if code should be blocked
        critical_issues = [i for i in self.issues if i.severity == SecuritySeverity.CRITICAL]
        high_issues = [i for i in self.issues if i.severity == SecuritySeverity.HIGH]

        is_safe = len(critical_issues) == 0 and len(high_issues) == 0
        blocked = len(critical_issues) > 0

        block_reason = ""
        if blocked:
            block_reason = critical_issues[0].message

        return ValidationResult(
            is_safe=is_safe,
            issues=self.issues,
            blocked=blocked,
            block_reason=block_reason
        )

    def _check_patterns(self, code: str):
        """Check code against dangerous patterns"""
        for pattern, description, severity in DANGEROUS_PATTERNS:
            for match in re.finditer(pattern, code, re.IGNORECASE):
                line_num = code[:match.start()].count('\n') + 1
                self.issues.append(SecurityIssue(
                    severity=severity,
                    category="pattern",
                    message=f"Dangerous pattern detected: {description}",
                    line=line_num,
                    column=match.start() - code.rfind('\n', 0, match.start()),
                    code_snippet=match.group()
                ))

    def _get_line(self, lineno: int) -> str:
        """Get source line by number"""
        if 0 < lineno <= len(self.source_lines):
            return self.source_lines[lineno - 1]
        return ""

    def visit_Import(self, node: ast.Import):
        """Check import statements"""
        for alias in node.names:
            module = alias.name.split('.')[0]
            self._check_import(module, node.lineno, node.col_offset)
        self.generic_visit(node)

    def visit_ImportFrom(self, node: ast.ImportFrom):
        """Check from ... import statements"""
        if node.module:
            module = node.module.split('.')[0]
            self._check_import(module, node.lineno, node.col_offset)

            # Check specific imports
            for alias in node.names:
                full_name = f"{node.module}.{alias.name}"
                if full_name in DANGEROUS_CALLS:
                    self.issues.append(SecurityIssue(
                        severity=SecuritySeverity.CRITICAL,
                        category="import",
                        message=f"Dangerous import: {full_name} - {DANGEROUS_CALLS[full_name]}",
                        line=node.lineno,
                        column=node.col_offset,
                        code_snippet=self._get_line(node.lineno)
                    ))
        self.generic_visit(node)

    def _check_import(self, module: str, line: int, col: int):
        """Check if import is allowed"""
        if module in BLOCKED_IMPORTS:
            self.issues.append(SecurityIssue(
                severity=SecuritySeverity.CRITICAL,
                category="import",
                message=f"Blocked import: {module}",
                line=line,
                column=col,
                code_snippet=self._get_line(line),
                suggestion="This module is not allowed for security reasons"
            ))
        elif module not in self.allowed_imports:
            self.issues.append(SecurityIssue(
                severity=SecuritySeverity.MEDIUM,
                category="import",
                message=f"Unrecognized import: {module}",
                line=line,
                column=col,
                code_snippet=self._get_line(line),
                suggestion="Contact admin to whitelist this module if needed"
            ))

    def visit_Call(self, node: ast.Call):
        """Check function calls"""
        call_name = self._get_call_name(node)

        if call_name:
            # Check against dangerous calls
            if call_name in DANGEROUS_CALLS:
                self.issues.append(SecurityIssue(
                    severity=SecuritySeverity.CRITICAL,
                    category="call",
                    message=f"Dangerous call: {call_name} - {DANGEROUS_CALLS[call_name]}",
                    line=node.lineno,
                    column=node.col_offset,
                    code_snippet=self._get_line(node.lineno)
                ))

            # Check for eval/exec even without module prefix
            if call_name in ("eval", "exec", "compile", "__import__"):
                self.issues.append(SecurityIssue(
                    severity=SecuritySeverity.CRITICAL,
                    category="call",
                    message=f"Code execution function: {call_name}",
                    line=node.lineno,
                    column=node.col_offset,
                    code_snippet=self._get_line(node.lineno)
                ))

        self.generic_visit(node)

    def _get_call_name(self, node: ast.Call) -> Optional[str]:
        """Extract full call name from AST node"""
        if isinstance(node.func, ast.Name):
            return node.func.id
        elif isinstance(node.func, ast.Attribute):
            parts = []
            current = node.func
            while isinstance(current, ast.Attribute):
                parts.append(current.attr)
                current = current.value
            if isinstance(current, ast.Name):
                parts.append(current.id)
            return '.'.join(reversed(parts))
        return None

    def visit_Attribute(self, node: ast.Attribute):
        """Check attribute access for dunder methods"""
        if node.attr.startswith('__') and node.attr.endswith('__'):
            if node.attr not in ('__init__', '__str__', '__repr__', '__len__', '__iter__', '__next__'):
                self.issues.append(SecurityIssue(
                    severity=SecuritySeverity.HIGH,
                    category="attribute",
                    message=f"Dunder attribute access: {node.attr}",
                    line=node.lineno,
                    column=node.col_offset,
                    code_snippet=self._get_line(node.lineno)
                ))
        self.generic_visit(node)

    def visit_Subscript(self, node: ast.Subscript):
        """Check subscript access for __class__ etc"""
        if isinstance(node.slice, ast.Constant):
            if isinstance(node.slice.value, str) and node.slice.value.startswith('__'):
                self.issues.append(SecurityIssue(
                    severity=SecuritySeverity.HIGH,
                    category="subscript",
                    message=f"Dunder subscript access: {node.slice.value}",
                    line=node.lineno,
                    column=node.col_offset,
                    code_snippet=self._get_line(node.lineno)
                ))
        self.generic_visit(node)


def validate_code(code: str, allowed_imports: Optional[Set[str]] = None) -> ValidationResult:
    """
    Convenience function to validate code.

    Args:
        code: Python source code
        allowed_imports: Custom set of allowed imports

    Returns:
        ValidationResult
    """
    validator = CodeValidator(allowed_imports)
    return validator.validate(code)


def sanitize_code(code: str) -> Tuple[str, List[str]]:
    """
    Attempt to sanitize code by removing dangerous constructs.

    WARNING: This is not foolproof. Always use with sandboxing.

    Args:
        code: Python source code

    Returns:
        Tuple of (sanitized_code, list_of_removals)
    """
    removals = []

    # Remove dangerous imports
    lines = code.split('\n')
    sanitized_lines = []

    for i, line in enumerate(lines):
        stripped = line.strip()
        should_remove = False

        # Check import lines
        if stripped.startswith('import ') or stripped.startswith('from '):
            for blocked in BLOCKED_IMPORTS:
                if f'import {blocked}' in stripped or f'from {blocked}' in stripped:
                    should_remove = True
                    removals.append(f"Line {i+1}: Removed blocked import '{blocked}'")
                    break

        # Check for dangerous function calls
        for pattern, _, severity in DANGEROUS_PATTERNS:
            if severity == SecuritySeverity.CRITICAL and re.search(pattern, stripped):
                should_remove = True
                removals.append(f"Line {i+1}: Removed dangerous pattern")
                break

        if should_remove:
            sanitized_lines.append(f"# REMOVED: {stripped}")
        else:
            sanitized_lines.append(line)

    return '\n'.join(sanitized_lines), removals


def is_safe_for_execution(code: str) -> Tuple[bool, str]:
    """
    Quick check if code is safe to execute.

    Args:
        code: Python source code

    Returns:
        Tuple of (is_safe, reason)
    """
    result = validate_code(code)

    if result.blocked:
        return False, result.block_reason

    if not result.is_safe:
        high_issues = [i for i in result.issues if i.severity in (SecuritySeverity.CRITICAL, SecuritySeverity.HIGH)]
        if high_issues:
            return False, high_issues[0].message

    return True, ""


def log_security_event(
    event_type: str,
    code: str,
    result: ValidationResult,
    user_id: Optional[str] = None,
    job_id: Optional[str] = None
):
    """
    Log security event for auditing.

    Args:
        event_type: Type of event (validation, block, execution)
        code: The code that was analyzed
        result: Validation result
        user_id: Optional user ID
        job_id: Optional job ID
    """
    log_data = {
        "event_type": event_type,
        "user_id": user_id,
        "job_id": job_id,
        "is_safe": result.is_safe,
        "blocked": result.blocked,
        "issue_count": len(result.issues),
        "critical_count": sum(1 for i in result.issues if i.severity == SecuritySeverity.CRITICAL),
        "code_preview": code[:200] + "..." if len(code) > 200 else code
    }

    if result.blocked:
        logger.warning(f"[SecurityAudit] Code blocked: {log_data}")
    elif not result.is_safe:
        logger.info(f"[SecurityAudit] Code with issues: {log_data}")
    else:
        logger.debug(f"[SecurityAudit] Code validated: {log_data}")
