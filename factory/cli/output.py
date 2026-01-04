# -*- coding: utf-8 -*-
"""
CLI Output - Plataforma E
=========================

Formatted and colored output for CLI.
"""

import sys
from enum import Enum
from typing import Optional


class Color(str, Enum):
    """ANSI color codes."""
    RESET = "\033[0m"
    BOLD = "\033[1m"
    RED = "\033[91m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    MAGENTA = "\033[95m"
    CYAN = "\033[96m"
    WHITE = "\033[97m"
    GRAY = "\033[90m"


class Output:
    """Formatted output handler."""

    def __init__(self, stream=None):
        self.stream = stream or sys.stdout
        self._color_enabled = True

    def disable_color(self):
        """Disable colored output."""
        self._color_enabled = False

    def enable_color(self):
        """Enable colored output."""
        self._color_enabled = True

    def _colorize(self, text: str, color: Optional[Color] = None) -> str:
        """Apply color to text."""
        if not self._color_enabled or color is None:
            return text
        return f"{color.value}{text}{Color.RESET.value}"

    def print(self, text: str, color: Optional[Color] = None,
              bold: bool = False, end: str = "\n"):
        """Print text with optional formatting."""
        if bold and self._color_enabled:
            text = f"{Color.BOLD.value}{text}{Color.RESET.value}"
        output = self._colorize(text, color)
        print(output, end=end, file=self.stream)

    def header(self, text: str):
        """Print a header."""
        separator = "=" * 50
        self.print(separator, color=Color.BLUE)
        self.print(f" {text}", color=Color.CYAN, bold=True)
        self.print(separator, color=Color.BLUE)

    def success(self, text: str):
        """Print success message."""
        self.print(f"[OK] {text}", color=Color.GREEN)

    def error(self, text: str):
        """Print error message."""
        self.print(f"[ERROR] {text}", color=Color.RED)

    def warning(self, text: str):
        """Print warning message."""
        self.print(f"[WARN] {text}", color=Color.YELLOW)

    def info(self, text: str):
        """Print info message."""
        self.print(f"[INFO] {text}", color=Color.BLUE)

    def table(self, headers: list, rows: list):
        """Print a simple table."""
        # Calculate column widths
        widths = [len(h) for h in headers]
        for row in rows:
            for i, cell in enumerate(row):
                widths[i] = max(widths[i], len(str(cell)))

        # Print header
        header_row = " | ".join(h.ljust(widths[i]) for i, h in enumerate(headers))
        self.print(header_row, color=Color.CYAN, bold=True)
        self.print("-" * len(header_row), color=Color.GRAY)

        # Print rows
        for row in rows:
            row_text = " | ".join(str(c).ljust(widths[i]) for i, c in enumerate(row))
            self.print(row_text)

    def progress(self, current: int, total: int, width: int = 40):
        """Print a progress bar."""
        if total == 0:
            percent = 0
        else:
            percent = current / total

        filled = int(width * percent)
        bar = "=" * filled + "-" * (width - filled)
        self.print(f"[{bar}] {percent*100:.1f}%", end="\r")

        if current >= total:
            self.print("")  # New line when complete

    def list_item(self, text: str, bullet: str = "-", indent: int = 2):
        """Print a list item."""
        self.print(f"{' ' * indent}{bullet} {text}")

    def key_value(self, key: str, value: str, key_width: int = 15):
        """Print key-value pair."""
        self.print(f"  {key.ljust(key_width)}: ", color=Color.GRAY, end="")
        self.print(value)
