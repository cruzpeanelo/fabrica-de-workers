"""
Import Whitelist - Configurable allowed imports
Issue #201: Code validation and sanitization

Provides configurable whitelist of allowed Python imports
for user code execution.
"""
import os
from typing import Set, Dict, Any


# =============================================================================
# DEFAULT WHITELIST
# =============================================================================

# Safe standard library modules
STDLIB_SAFE: Set[str] = {
    # Data types and structures
    "json",
    "csv",
    "collections",
    "enum",
    "dataclasses",
    "typing",
    "copy",

    # Math and numbers
    "math",
    "decimal",
    "fractions",
    "random",
    "statistics",

    # String and text
    "string",
    "textwrap",
    "re",

    # Date and time
    "datetime",
    "time",
    "calendar",
    "zoneinfo",

    # Iteration and functional
    "itertools",
    "functools",
    "operator",

    # Encoding
    "base64",
    "hashlib",
    "hmac",
    "uuid",

    # Other safe
    "abc",
    "contextlib",
    "warnings",
}

# Data science libraries (optional)
DATA_SCIENCE: Set[str] = {
    "pandas",
    "numpy",
    "scipy",
    "sklearn",
    "matplotlib",
    "seaborn",
    "plotly",
}

# Web/HTTP libraries (with caution)
HTTP_LIBS: Set[str] = {
    "requests",
    "httpx",
    "aiohttp",
    "urllib.parse",
}

# Parsing libraries
PARSING_LIBS: Set[str] = {
    "xml.etree.ElementTree",
    "html.parser",
    "yaml",
    "toml",
}

# Testing libraries
TESTING_LIBS: Set[str] = {
    "unittest",
    "pytest",
    "mock",
}

# Logging (safe)
LOGGING_LIBS: Set[str] = {
    "logging",
}


# =============================================================================
# BLOCKED IMPORTS (NEVER ALLOWED)
# =============================================================================

BLOCKED_ALWAYS: Set[str] = {
    # OS and system
    "os",
    "sys",
    "subprocess",
    "shutil",
    "pathlib",
    "glob",
    "fnmatch",

    # Network (low-level)
    "socket",
    "ssl",
    "ftplib",
    "smtplib",
    "telnetlib",
    "poplib",
    "imaplib",
    "nntplib",

    # Code execution
    "code",
    "codeop",
    "compileall",
    "py_compile",
    "dis",
    "inspect",
    "ast",  # can be used for escape

    # Memory and process
    "ctypes",
    "multiprocessing",
    "threading",
    "concurrent.futures",
    "gc",
    "traceback",
    "tracemalloc",

    # Serialization (unsafe)
    "pickle",
    "marshal",
    "shelve",
    "dbm",

    # Import system
    "importlib",
    "builtins",
    "__future__",

    # Database
    "sqlite3",
    "psycopg2",
    "pymysql",
    "sqlalchemy",

    # Low-level
    "mmap",
    "resource",
    "pty",
    "tty",
    "termios",
    "fcntl",
    "signal",

    # Debug
    "pdb",
    "bdb",
    "profile",
    "cProfile",
    "pstats",
    "timeit",
}


# =============================================================================
# WHITELIST PROFILES
# =============================================================================

class WhitelistProfile:
    """Predefined whitelist profiles for different use cases"""

    @staticmethod
    def minimal() -> Set[str]:
        """Minimal whitelist - only basic data processing"""
        return STDLIB_SAFE.copy()

    @staticmethod
    def standard() -> Set[str]:
        """Standard whitelist - safe stdlib + common libs"""
        return STDLIB_SAFE | PARSING_LIBS | LOGGING_LIBS

    @staticmethod
    def data_science() -> Set[str]:
        """Data science whitelist - includes pandas/numpy"""
        return STDLIB_SAFE | DATA_SCIENCE | PARSING_LIBS | LOGGING_LIBS

    @staticmethod
    def web_enabled() -> Set[str]:
        """Web-enabled whitelist - allows HTTP requests"""
        return STDLIB_SAFE | HTTP_LIBS | PARSING_LIBS | LOGGING_LIBS

    @staticmethod
    def full() -> Set[str]:
        """Full whitelist - all safe libraries"""
        return STDLIB_SAFE | DATA_SCIENCE | HTTP_LIBS | PARSING_LIBS | TESTING_LIBS | LOGGING_LIBS


# =============================================================================
# CONFIGURATION
# =============================================================================

def get_whitelist_from_env() -> Set[str]:
    """
    Get whitelist based on environment configuration.

    Environment variables:
        IMPORT_WHITELIST_PROFILE: minimal, standard, data_science, web_enabled, full
        IMPORT_WHITELIST_EXTRA: comma-separated list of additional imports

    Returns:
        Set of allowed imports
    """
    profile = os.getenv("IMPORT_WHITELIST_PROFILE", "standard")

    profile_map = {
        "minimal": WhitelistProfile.minimal,
        "standard": WhitelistProfile.standard,
        "data_science": WhitelistProfile.data_science,
        "web_enabled": WhitelistProfile.web_enabled,
        "full": WhitelistProfile.full,
    }

    whitelist = profile_map.get(profile, WhitelistProfile.standard)()

    # Add extra imports from env
    extra = os.getenv("IMPORT_WHITELIST_EXTRA", "")
    if extra:
        for module in extra.split(","):
            module = module.strip()
            if module and module not in BLOCKED_ALWAYS:
                whitelist.add(module)

    return whitelist


def is_import_allowed(module: str, whitelist: Set[str] = None) -> bool:
    """
    Check if an import is allowed.

    Args:
        module: Module name to check
        whitelist: Custom whitelist (uses env config if None)

    Returns:
        True if import is allowed
    """
    if whitelist is None:
        whitelist = get_whitelist_from_env()

    # Always block dangerous modules
    base_module = module.split('.')[0]
    if base_module in BLOCKED_ALWAYS:
        return False

    # Check whitelist
    return base_module in whitelist or module in whitelist


def get_whitelist_info() -> Dict[str, Any]:
    """
    Get information about current whitelist configuration.

    Returns:
        Dict with whitelist info
    """
    profile = os.getenv("IMPORT_WHITELIST_PROFILE", "standard")
    whitelist = get_whitelist_from_env()

    return {
        "profile": profile,
        "allowed_count": len(whitelist),
        "blocked_count": len(BLOCKED_ALWAYS),
        "allowed_modules": sorted(whitelist),
        "blocked_modules": sorted(BLOCKED_ALWAYS),
    }


# =============================================================================
# RUNTIME IMPORT HOOK (Optional advanced feature)
# =============================================================================

class ImportBlocker:
    """
    Import hook to block dangerous imports at runtime.

    Usage:
        blocker = ImportBlocker()
        blocker.install()
        # imports are now filtered
        blocker.uninstall()
    """

    def __init__(self, whitelist: Set[str] = None):
        self.whitelist = whitelist or get_whitelist_from_env()
        self._original_import = None

    def install(self):
        """Install the import blocker"""
        import builtins
        self._original_import = builtins.__import__

        def filtered_import(name, *args, **kwargs):
            base = name.split('.')[0]
            if base in BLOCKED_ALWAYS:
                raise ImportError(f"Import of '{name}' is blocked for security reasons")
            if base not in self.whitelist:
                raise ImportError(f"Import of '{name}' is not whitelisted")
            return self._original_import(name, *args, **kwargs)

        builtins.__import__ = filtered_import

    def uninstall(self):
        """Remove the import blocker"""
        if self._original_import:
            import builtins
            builtins.__import__ = self._original_import
            self._original_import = None

    def __enter__(self):
        self.install()
        return self

    def __exit__(self, *args):
        self.uninstall()
