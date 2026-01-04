# -*- coding: utf-8 -*-
"""
Pair Programming com IA - Plataforma E
=============================================

Modo interativo de desenvolvimento onde o usuario e a IA trabalham juntos em tempo real.

Funcionalidades:
- Editor colaborativo com sugestoes inline
- Modos de colaboracao: Guiado, Autonomo, Hibrido
- Chat contextual integrado ao codigo
- Deteccao de bugs em tempo real
- Explicacao de codigo sob demanda
- Sugestoes de proximos passos
- Code completion inteligente

Issue: #59
"""

import os
import re
import json
import asyncio
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

# Claude AI Integration
try:
    from factory.ai.claude_integration import ClaudeClient, get_claude_client, ClaudeResponse
    HAS_CLAUDE = True
except ImportError:
    HAS_CLAUDE = False
    print("[PairProgramming] Claude integration not available")


class CollaborationMode(str, Enum):
    """Modos de colaboracao no pair programming"""
    GUIDED = "guided"          # IA sugere, usuario aprova
    AUTONOMOUS = "autonomous"  # IA desenvolve, usuario observa
    HYBRID = "hybrid"          # Usuario escreve estrutura, IA completa


class SuggestionType(str, Enum):
    """Tipos de sugestao da IA"""
    COMPLETION = "completion"      # Completar codigo
    REFACTOR = "refactor"          # Refatorar codigo
    FIX_BUG = "fix_bug"            # Corrigir bug
    ADD_DOCS = "add_docs"          # Adicionar documentacao
    ADD_TESTS = "add_tests"        # Adicionar testes
    IMPROVE = "improve"            # Melhorar codigo
    EXPLAIN = "explain"            # Explicar codigo
    NEXT_STEP = "next_step"        # Sugerir proximo passo


class BugSeverity(str, Enum):
    """Severidade de bugs detectados"""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class CodeSuggestion:
    """Sugestao de codigo da IA"""
    suggestion_id: str
    suggestion_type: SuggestionType
    title: str
    description: str
    original_code: str
    suggested_code: str
    start_line: int
    end_line: int
    confidence: float  # 0.0 to 1.0
    reasoning: str
    created_at: datetime = field(default_factory=datetime.utcnow)
    accepted: Optional[bool] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "suggestion_id": self.suggestion_id,
            "suggestion_type": self.suggestion_type.value,
            "title": self.title,
            "description": self.description,
            "original_code": self.original_code,
            "suggested_code": self.suggested_code,
            "start_line": self.start_line,
            "end_line": self.end_line,
            "confidence": self.confidence,
            "reasoning": self.reasoning,
            "created_at": self.created_at.isoformat(),
            "accepted": self.accepted
        }


@dataclass
class DetectedBug:
    """Bug detectado no codigo"""
    bug_id: str
    severity: BugSeverity
    title: str
    description: str
    line_number: int
    column: int
    code_snippet: str
    fix_suggestion: Optional[str] = None
    detected_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "bug_id": self.bug_id,
            "severity": self.severity.value,
            "title": self.title,
            "description": self.description,
            "line_number": self.line_number,
            "column": self.column,
            "code_snippet": self.code_snippet,
            "fix_suggestion": self.fix_suggestion,
            "detected_at": self.detected_at.isoformat()
        }


@dataclass
class CodeExplanation:
    """Explicacao de codigo"""
    explanation_id: str
    code_snippet: str
    start_line: int
    end_line: int
    summary: str
    detailed_explanation: str
    concepts: List[str]
    complexity_rating: str  # simple, moderate, complex
    related_patterns: List[str]
    created_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "explanation_id": self.explanation_id,
            "code_snippet": self.code_snippet,
            "start_line": self.start_line,
            "end_line": self.end_line,
            "summary": self.summary,
            "detailed_explanation": self.detailed_explanation,
            "concepts": self.concepts,
            "complexity_rating": self.complexity_rating,
            "related_patterns": self.related_patterns,
            "created_at": self.created_at.isoformat()
        }


@dataclass
class PairSession:
    """Sessao de pair programming"""
    session_id: str
    project_id: str
    file_path: str
    mode: CollaborationMode
    language: str
    current_code: str
    suggestions: List[CodeSuggestion] = field(default_factory=list)
    detected_bugs: List[DetectedBug] = field(default_factory=list)
    explanations: List[CodeExplanation] = field(default_factory=list)
    chat_history: List[Dict[str, Any]] = field(default_factory=list)
    is_paused: bool = False
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "session_id": self.session_id,
            "project_id": self.project_id,
            "file_path": self.file_path,
            "mode": self.mode.value,
            "language": self.language,
            "current_code": self.current_code,
            "suggestions": [s.to_dict() for s in self.suggestions],
            "detected_bugs": [b.to_dict() for b in self.detected_bugs],
            "explanations": [e.to_dict() for e in self.explanations],
            "chat_history": self.chat_history,
            "is_paused": self.is_paused,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat()
        }


class PairProgrammer:
    """
    Classe principal para Pair Programming com IA

    Fornece assistencia em tempo real durante desenvolvimento:
    - Sugestoes de codigo inline
    - Deteccao de bugs
    - Explicacoes de codigo
    - Chat contextual
    - Code completion
    """

    def __init__(self, claude_client: ClaudeClient = None):
        """
        Inicializa o PairProgrammer

        Args:
            claude_client: Cliente Claude opcional (usa global se None)
        """
        self.claude = claude_client or (get_claude_client() if HAS_CLAUDE else None)
        self.sessions: Dict[str, PairSession] = {}
        self._suggestion_counter = 0
        self._bug_counter = 0
        self._explanation_counter = 0

    def is_available(self) -> bool:
        """Verifica se o pair programmer esta disponivel"""
        return self.claude is not None and self.claude.is_available()

    def _generate_id(self, prefix: str) -> str:
        """Gera ID unico"""
        import uuid
        return f"{prefix}-{uuid.uuid4().hex[:8]}"

    def _detect_language(self, file_path: str, code: str = "") -> str:
        """Detecta linguagem baseado na extensao ou conteudo"""
        ext_map = {
            ".py": "python",
            ".js": "javascript",
            ".ts": "typescript",
            ".tsx": "typescript",
            ".jsx": "javascript",
            ".java": "java",
            ".go": "go",
            ".rs": "rust",
            ".rb": "ruby",
            ".php": "php",
            ".cs": "csharp",
            ".cpp": "cpp",
            ".c": "c",
            ".sql": "sql",
            ".html": "html",
            ".css": "css",
            ".json": "json",
            ".yaml": "yaml",
            ".yml": "yaml",
            ".md": "markdown"
        }

        ext = Path(file_path).suffix.lower()
        return ext_map.get(ext, "python")

    # =========================================================================
    # SESSION MANAGEMENT
    # =========================================================================

    def create_session(
        self,
        project_id: str,
        file_path: str,
        initial_code: str = "",
        mode: CollaborationMode = CollaborationMode.GUIDED
    ) -> PairSession:
        """
        Cria nova sessao de pair programming

        Args:
            project_id: ID do projeto
            file_path: Caminho do arquivo sendo editado
            initial_code: Codigo inicial
            mode: Modo de colaboracao

        Returns:
            PairSession criada
        """
        session_id = self._generate_id("PAIR")
        language = self._detect_language(file_path, initial_code)

        session = PairSession(
            session_id=session_id,
            project_id=project_id,
            file_path=file_path,
            mode=mode,
            language=language,
            current_code=initial_code
        )

        self.sessions[session_id] = session
        return session

    def get_session(self, session_id: str) -> Optional[PairSession]:
        """Retorna sessao pelo ID"""
        return self.sessions.get(session_id)

    def update_code(self, session_id: str, new_code: str) -> Optional[PairSession]:
        """Atualiza codigo da sessao"""
        session = self.sessions.get(session_id)
        if session:
            session.current_code = new_code
            session.updated_at = datetime.utcnow()
        return session

    def set_mode(self, session_id: str, mode: CollaborationMode) -> Optional[PairSession]:
        """Altera modo de colaboracao"""
        session = self.sessions.get(session_id)
        if session:
            session.mode = mode
            session.updated_at = datetime.utcnow()
        return session

    def pause_session(self, session_id: str) -> Optional[PairSession]:
        """Pausa sessao (para modo autonomo)"""
        session = self.sessions.get(session_id)
        if session:
            session.is_paused = True
            session.updated_at = datetime.utcnow()
        return session

    def resume_session(self, session_id: str) -> Optional[PairSession]:
        """Retoma sessao pausada"""
        session = self.sessions.get(session_id)
        if session:
            session.is_paused = False
            session.updated_at = datetime.utcnow()
        return session

    def end_session(self, session_id: str) -> bool:
        """Encerra sessao"""
        if session_id in self.sessions:
            del self.sessions[session_id]
            return True
        return False

    # =========================================================================
    # CODE SUGGESTIONS
    # =========================================================================

    async def get_inline_suggestions(
        self,
        session_id: str,
        cursor_line: int,
        cursor_column: int,
        context_lines: int = 10
    ) -> List[CodeSuggestion]:
        """
        Obtem sugestoes inline baseado na posicao do cursor

        Args:
            session_id: ID da sessao
            cursor_line: Linha do cursor
            cursor_column: Coluna do cursor
            context_lines: Linhas de contexto

        Returns:
            Lista de sugestoes
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return []

        code_lines = session.current_code.split("\n")

        # Contexto ao redor do cursor
        start = max(0, cursor_line - context_lines)
        end = min(len(code_lines), cursor_line + context_lines)
        context_code = "\n".join(code_lines[start:end])
        current_line = code_lines[cursor_line] if cursor_line < len(code_lines) else ""

        prompt = f"""Voce e um assistente de pair programming. Analise o codigo {session.language} e sugira completions ou melhorias para a linha atual.

Contexto do codigo:
```{session.language}
{context_code}
```

Linha atual (cursor na coluna {cursor_column}):
```
{current_line}
```

Arquivo: {session.file_path}

Forneça sugestoes em JSON:
{{
    "suggestions": [
        {{
            "type": "completion|refactor|improve",
            "title": "titulo curto",
            "description": "o que a sugestao faz",
            "suggested_code": "codigo sugerido para substituir/adicionar",
            "confidence": 0.0-1.0,
            "reasoning": "por que esta sugestao"
        }}
    ]
}}

Retorne ate 3 sugestoes relevantes. Se nao houver sugestoes claras, retorne lista vazia."""

        response = self.claude.chat(prompt, max_tokens=2000)

        if not response.success:
            return []

        suggestions = self._parse_suggestions_response(
            response.content,
            current_line,
            cursor_line,
            session
        )

        session.suggestions.extend(suggestions)
        session.updated_at = datetime.utcnow()

        return suggestions

    async def get_code_completion(
        self,
        session_id: str,
        prefix: str,
        cursor_line: int
    ) -> Optional[str]:
        """
        Obtem code completion para o prefixo atual

        Args:
            session_id: ID da sessao
            prefix: Texto antes do cursor
            cursor_line: Linha do cursor

        Returns:
            Texto de completion ou None
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return None

        code_lines = session.current_code.split("\n")
        context = "\n".join(code_lines[max(0, cursor_line - 20):cursor_line])

        prompt = f"""Complete o codigo {session.language} a seguir. Retorne APENAS o texto que deve vir apos o cursor, sem explicacoes.

Contexto anterior:
```{session.language}
{context}
```

Linha atual (incompleta):
```
{prefix}
```

Complete esta linha de forma natural e util. Retorne apenas o texto de completion."""

        response = self.claude.chat(prompt, max_tokens=500)

        if response.success:
            completion = response.content.strip()
            # Remove markdown code blocks se presentes
            completion = re.sub(r'^```\w*\n?', '', completion)
            completion = re.sub(r'\n?```$', '', completion)
            return completion.strip()

        return None

    async def suggest_refactoring(
        self,
        session_id: str,
        start_line: int,
        end_line: int
    ) -> List[CodeSuggestion]:
        """
        Sugere refatoracoes para um bloco de codigo

        Args:
            session_id: ID da sessao
            start_line: Linha inicial
            end_line: Linha final

        Returns:
            Lista de sugestoes de refatoracao
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return []

        code_lines = session.current_code.split("\n")
        selected_code = "\n".join(code_lines[start_line:end_line + 1])

        # Contexto maior para entender o codigo
        context_start = max(0, start_line - 10)
        context_end = min(len(code_lines), end_line + 10)
        full_context = "\n".join(code_lines[context_start:context_end])

        prompt = f"""Analise o codigo {session.language} selecionado e sugira refatoracoes para melhorar:
- Legibilidade
- Performance
- Manutencao
- Boas praticas

Codigo selecionado (linhas {start_line + 1} a {end_line + 1}):
```{session.language}
{selected_code}
```

Contexto completo:
```{session.language}
{full_context}
```

Responda em JSON:
{{
    "refactorings": [
        {{
            "title": "titulo da refatoracao",
            "description": "descricao detalhada",
            "original_code": "codigo original",
            "refactored_code": "codigo refatorado",
            "benefits": ["beneficio1", "beneficio2"],
            "confidence": 0.0-1.0
        }}
    ]
}}

Sugira ate 3 refatoracoes valiosas."""

        response = self.claude.chat(prompt, max_tokens=3000)

        if not response.success:
            return []

        suggestions = []
        try:
            # Tenta extrair JSON da resposta
            json_match = re.search(r'\{[\s\S]*\}', response.content)
            if json_match:
                data = json.loads(json_match.group())
                for ref in data.get("refactorings", []):
                    suggestion = CodeSuggestion(
                        suggestion_id=self._generate_id("SUG"),
                        suggestion_type=SuggestionType.REFACTOR,
                        title=ref.get("title", "Refatoracao"),
                        description=ref.get("description", ""),
                        original_code=ref.get("original_code", selected_code),
                        suggested_code=ref.get("refactored_code", ""),
                        start_line=start_line,
                        end_line=end_line,
                        confidence=float(ref.get("confidence", 0.7)),
                        reasoning=", ".join(ref.get("benefits", []))
                    )
                    suggestions.append(suggestion)
                    session.suggestions.append(suggestion)
        except (json.JSONDecodeError, KeyError):
            pass

        session.updated_at = datetime.utcnow()
        return suggestions

    def accept_suggestion(self, session_id: str, suggestion_id: str) -> Optional[str]:
        """
        Aceita uma sugestao e retorna o codigo atualizado

        Args:
            session_id: ID da sessao
            suggestion_id: ID da sugestao

        Returns:
            Codigo atualizado ou None
        """
        session = self.sessions.get(session_id)
        if not session:
            return None

        for suggestion in session.suggestions:
            if suggestion.suggestion_id == suggestion_id:
                suggestion.accepted = True

                # Aplica sugestao ao codigo
                code_lines = session.current_code.split("\n")
                new_lines = suggestion.suggested_code.split("\n")

                # Substitui linhas
                code_lines[suggestion.start_line:suggestion.end_line + 1] = new_lines
                session.current_code = "\n".join(code_lines)
                session.updated_at = datetime.utcnow()

                return session.current_code

        return None

    def reject_suggestion(self, session_id: str, suggestion_id: str) -> bool:
        """Rejeita uma sugestao"""
        session = self.sessions.get(session_id)
        if not session:
            return False

        for suggestion in session.suggestions:
            if suggestion.suggestion_id == suggestion_id:
                suggestion.accepted = False
                return True

        return False

    # =========================================================================
    # BUG DETECTION
    # =========================================================================

    async def detect_bugs(self, session_id: str) -> List[DetectedBug]:
        """
        Detecta bugs no codigo atual

        Args:
            session_id: ID da sessao

        Returns:
            Lista de bugs detectados
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return []

        prompt = f"""Analise o codigo {session.language} e detecte problemas potenciais:
- Erros de sintaxe
- Bugs logicos
- Vulnerabilidades de seguranca
- Problemas de performance
- Code smells

Codigo:
```{session.language}
{session.current_code}
```

Arquivo: {session.file_path}

Responda em JSON:
{{
    "bugs": [
        {{
            "severity": "info|warning|error|critical",
            "title": "titulo curto do problema",
            "description": "descricao detalhada",
            "line_number": 1,
            "column": 1,
            "code_snippet": "linha de codigo com problema",
            "fix_suggestion": "como corrigir"
        }}
    ]
}}

Liste todos os problemas encontrados, ordenados por severidade."""

        response = self.claude.chat(prompt, max_tokens=3000)

        if not response.success:
            return []

        bugs = []
        try:
            json_match = re.search(r'\{[\s\S]*\}', response.content)
            if json_match:
                data = json.loads(json_match.group())
                for bug_data in data.get("bugs", []):
                    severity_map = {
                        "info": BugSeverity.INFO,
                        "warning": BugSeverity.WARNING,
                        "error": BugSeverity.ERROR,
                        "critical": BugSeverity.CRITICAL
                    }

                    bug = DetectedBug(
                        bug_id=self._generate_id("BUG"),
                        severity=severity_map.get(bug_data.get("severity", "warning"), BugSeverity.WARNING),
                        title=bug_data.get("title", "Problema detectado"),
                        description=bug_data.get("description", ""),
                        line_number=int(bug_data.get("line_number", 1)),
                        column=int(bug_data.get("column", 1)),
                        code_snippet=bug_data.get("code_snippet", ""),
                        fix_suggestion=bug_data.get("fix_suggestion")
                    )
                    bugs.append(bug)
                    session.detected_bugs.append(bug)
        except (json.JSONDecodeError, KeyError, ValueError):
            pass

        session.updated_at = datetime.utcnow()
        return bugs

    async def fix_bug(self, session_id: str, bug_id: str) -> Optional[str]:
        """
        Aplica correcao automatica para um bug

        Args:
            session_id: ID da sessao
            bug_id: ID do bug

        Returns:
            Codigo corrigido ou None
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return None

        bug = None
        for b in session.detected_bugs:
            if b.bug_id == bug_id:
                bug = b
                break

        if not bug:
            return None

        prompt = f"""Corrija o seguinte problema no codigo {session.language}:

Problema: {bug.title}
Descricao: {bug.description}
Linha: {bug.line_number}
Codigo problematico: {bug.code_snippet}

Codigo completo:
```{session.language}
{session.current_code}
```

Retorne o codigo completo corrigido, mantendo a estrutura original exceto pela correcao necessaria.
Retorne APENAS o codigo, sem explicacoes."""

        response = self.claude.chat(prompt, max_tokens=4000)

        if response.success:
            fixed_code = response.content.strip()
            # Remove markdown code blocks
            fixed_code = re.sub(r'^```\w*\n?', '', fixed_code)
            fixed_code = re.sub(r'\n?```$', '', fixed_code)

            session.current_code = fixed_code.strip()
            session.updated_at = datetime.utcnow()
            return session.current_code

        return None

    # =========================================================================
    # CODE EXPLANATION
    # =========================================================================

    async def explain_code(
        self,
        session_id: str,
        start_line: int,
        end_line: int
    ) -> Optional[CodeExplanation]:
        """
        Explica um trecho de codigo

        Args:
            session_id: ID da sessao
            start_line: Linha inicial
            end_line: Linha final

        Returns:
            Explicacao do codigo
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return None

        code_lines = session.current_code.split("\n")
        selected_code = "\n".join(code_lines[start_line:end_line + 1])

        prompt = f"""Explique o seguinte codigo {session.language} de forma clara e educativa:

```{session.language}
{selected_code}
```

Arquivo: {session.file_path}
Linhas: {start_line + 1} a {end_line + 1}

Responda em JSON:
{{
    "summary": "resumo em uma frase",
    "detailed_explanation": "explicacao detalhada passo a passo",
    "concepts": ["conceito1", "conceito2"],
    "complexity_rating": "simple|moderate|complex",
    "related_patterns": ["padrao1", "padrao2"],
    "learning_tips": ["dica1", "dica2"]
}}"""

        response = self.claude.chat(prompt, max_tokens=2000)

        if not response.success:
            return None

        try:
            json_match = re.search(r'\{[\s\S]*\}', response.content)
            if json_match:
                data = json.loads(json_match.group())

                explanation = CodeExplanation(
                    explanation_id=self._generate_id("EXP"),
                    code_snippet=selected_code,
                    start_line=start_line,
                    end_line=end_line,
                    summary=data.get("summary", ""),
                    detailed_explanation=data.get("detailed_explanation", ""),
                    concepts=data.get("concepts", []),
                    complexity_rating=data.get("complexity_rating", "moderate"),
                    related_patterns=data.get("related_patterns", [])
                )

                session.explanations.append(explanation)
                session.updated_at = datetime.utcnow()
                return explanation
        except (json.JSONDecodeError, KeyError):
            pass

        return None

    # =========================================================================
    # CONTEXTUAL CHAT
    # =========================================================================

    async def chat(
        self,
        session_id: str,
        message: str,
        selected_code: Optional[str] = None,
        selected_lines: Optional[Tuple[int, int]] = None
    ) -> Dict[str, Any]:
        """
        Chat contextual com a IA sobre o codigo

        Args:
            session_id: ID da sessao
            message: Mensagem do usuario
            selected_code: Codigo selecionado (opcional)
            selected_lines: Linhas selecionadas (start, end)

        Returns:
            Resposta do chat com possiveis acoes
        """
        session = self.sessions.get(session_id)
        if not session:
            return {"error": "Session not found"}

        if not self.is_available():
            return {"error": "AI not available"}

        # Monta contexto
        context_parts = [
            f"Arquivo: {session.file_path}",
            f"Linguagem: {session.language}",
            f"Modo: {session.mode.value}"
        ]

        if selected_code:
            context_parts.append(f"\nCodigo selecionado (linhas {selected_lines[0] + 1}-{selected_lines[1] + 1}):\n```{session.language}\n{selected_code}\n```")

        context_parts.append(f"\nCodigo completo:\n```{session.language}\n{session.current_code}\n```")

        system_prompt = """Voce e um par de programacao experiente ajudando em tempo real.
Voce pode:
- Explicar codigo
- Sugerir melhorias
- Corrigir bugs
- Refatorar
- Adicionar documentacao
- Escrever testes

Seja conciso mas util. Se precisar modificar codigo, mostre o codigo completo da mudanca.
Para acoes especificas, inclua um bloco JSON com a acao:
```json
{"action": "tipo_acao", "code": "codigo", "start_line": 1, "end_line": 5}
```
Tipos de acao: replace_code, add_code, delete_code, run_tests, add_docs"""

        full_message = "\n".join(context_parts) + f"\n\nUsuario: {message}"

        response = self.claude.chat(full_message, system_prompt, max_tokens=3000)

        # Registra no historico
        chat_entry = {
            "role": "user",
            "content": message,
            "selected_code": selected_code,
            "timestamp": datetime.utcnow().isoformat()
        }
        session.chat_history.append(chat_entry)

        if response.success:
            # Extrai possiveis acoes do response
            actions = []
            action_match = re.search(r'```json\s*(\{[^}]+\})\s*```', response.content)
            if action_match:
                try:
                    action = json.loads(action_match.group(1))
                    actions.append(action)
                except json.JSONDecodeError:
                    pass

            assistant_entry = {
                "role": "assistant",
                "content": response.content,
                "actions": actions,
                "timestamp": datetime.utcnow().isoformat()
            }
            session.chat_history.append(assistant_entry)
            session.updated_at = datetime.utcnow()

            return {
                "response": response.content,
                "actions": actions,
                "tokens_used": response.tokens_used
            }
        else:
            return {"error": response.error}

    # =========================================================================
    # NEXT STEPS SUGGESTIONS
    # =========================================================================

    async def suggest_next_steps(self, session_id: str) -> List[Dict[str, Any]]:
        """
        Sugere proximos passos de desenvolvimento

        Args:
            session_id: ID da sessao

        Returns:
            Lista de sugestoes de proximos passos
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return []

        prompt = f"""Analise o codigo {session.language} atual e sugira os proximos passos de desenvolvimento:

```{session.language}
{session.current_code}
```

Arquivo: {session.file_path}

Considere:
- O que esta faltando
- Melhorias possiveis
- Testes a adicionar
- Documentacao necessaria
- Refatoracoes recomendadas

Responda em JSON:
{{
    "next_steps": [
        {{
            "priority": 1-5,
            "title": "titulo do passo",
            "description": "descricao detalhada",
            "type": "feature|bugfix|refactor|test|docs|security",
            "estimated_effort": "small|medium|large",
            "code_hint": "codigo ou estrutura sugerida"
        }}
    ]
}}

Sugira ate 5 proximos passos mais importantes."""

        response = self.claude.chat(prompt, max_tokens=2000)

        if not response.success:
            return []

        try:
            json_match = re.search(r'\{[\s\S]*\}', response.content)
            if json_match:
                data = json.loads(json_match.group())
                return data.get("next_steps", [])
        except (json.JSONDecodeError, KeyError):
            pass

        return []

    # =========================================================================
    # AUTONOMOUS MODE
    # =========================================================================

    async def autonomous_develop(
        self,
        session_id: str,
        task_description: str,
        max_iterations: int = 5
    ) -> Dict[str, Any]:
        """
        Modo autonomo: IA desenvolve enquanto usuario observa

        Args:
            session_id: ID da sessao
            task_description: O que deve ser desenvolvido
            max_iterations: Maximo de iteracoes

        Returns:
            Resultado do desenvolvimento
        """
        session = self.sessions.get(session_id)
        if not session or not self.is_available():
            return {"error": "Session or AI not available"}

        if session.mode != CollaborationMode.AUTONOMOUS:
            return {"error": "Session not in autonomous mode"}

        iterations = []

        for i in range(max_iterations):
            if session.is_paused:
                return {
                    "status": "paused",
                    "iterations": iterations,
                    "current_code": session.current_code
                }

            prompt = f"""Voce esta desenvolvendo de forma autonoma.

Tarefa: {task_description}

Codigo atual:
```{session.language}
{session.current_code}
```

Iteracao: {i + 1}/{max_iterations}

Analise o estado atual e execute o proximo passo de desenvolvimento.
Retorne o codigo atualizado e uma descricao do que foi feito.

Responda em JSON:
{{
    "action_taken": "descricao da acao",
    "code": "codigo completo atualizado",
    "is_complete": true|false,
    "next_action": "proxima acao se nao completo"
}}"""

            response = self.claude.chat(prompt, max_tokens=4000)

            if not response.success:
                iterations.append({
                    "iteration": i + 1,
                    "error": response.error
                })
                break

            try:
                json_match = re.search(r'\{[\s\S]*\}', response.content, re.DOTALL)
                if json_match:
                    data = json.loads(json_match.group())

                    new_code = data.get("code", "")
                    if new_code:
                        # Remove markdown code blocks
                        new_code = re.sub(r'^```\w*\n?', '', new_code)
                        new_code = re.sub(r'\n?```$', '', new_code)
                        session.current_code = new_code.strip()

                    iteration_result = {
                        "iteration": i + 1,
                        "action": data.get("action_taken", ""),
                        "is_complete": data.get("is_complete", False)
                    }
                    iterations.append(iteration_result)

                    if data.get("is_complete"):
                        break
            except (json.JSONDecodeError, KeyError):
                iterations.append({
                    "iteration": i + 1,
                    "error": "Failed to parse response"
                })

        session.updated_at = datetime.utcnow()

        return {
            "status": "completed",
            "iterations": iterations,
            "current_code": session.current_code,
            "total_iterations": len(iterations)
        }

    # =========================================================================
    # HELPER METHODS
    # =========================================================================

    def _parse_suggestions_response(
        self,
        content: str,
        current_line: str,
        cursor_line: int,
        session: PairSession
    ) -> List[CodeSuggestion]:
        """Parse resposta de sugestoes em objetos CodeSuggestion"""
        suggestions = []

        try:
            json_match = re.search(r'\{[\s\S]*\}', content)
            if json_match:
                data = json.loads(json_match.group())

                type_map = {
                    "completion": SuggestionType.COMPLETION,
                    "refactor": SuggestionType.REFACTOR,
                    "improve": SuggestionType.IMPROVE
                }

                for sug in data.get("suggestions", []):
                    suggestion = CodeSuggestion(
                        suggestion_id=self._generate_id("SUG"),
                        suggestion_type=type_map.get(sug.get("type", "completion"), SuggestionType.COMPLETION),
                        title=sug.get("title", "Sugestao"),
                        description=sug.get("description", ""),
                        original_code=current_line,
                        suggested_code=sug.get("suggested_code", ""),
                        start_line=cursor_line,
                        end_line=cursor_line,
                        confidence=float(sug.get("confidence", 0.7)),
                        reasoning=sug.get("reasoning", "")
                    )
                    suggestions.append(suggestion)
        except (json.JSONDecodeError, KeyError, ValueError):
            pass

        return suggestions


# =============================================================================
# SINGLETON
# =============================================================================

_pair_programmer: Optional[PairProgrammer] = None


def get_pair_programmer() -> PairProgrammer:
    """Retorna instancia singleton do PairProgrammer"""
    global _pair_programmer
    if _pair_programmer is None:
        _pair_programmer = PairProgrammer()
    return _pair_programmer


# =============================================================================
# PYDANTIC SCHEMAS FOR API
# =============================================================================

from pydantic import BaseModel


class PairSessionCreate(BaseModel):
    """Schema para criar sessao de pair programming"""
    project_id: str
    file_path: str
    initial_code: Optional[str] = ""
    mode: Optional[str] = "guided"


class PairSessionUpdate(BaseModel):
    """Schema para atualizar sessao"""
    code: Optional[str] = None
    mode: Optional[str] = None


class PairChatRequest(BaseModel):
    """Schema para chat no pair programming"""
    message: str
    selected_code: Optional[str] = None
    start_line: Optional[int] = None
    end_line: Optional[int] = None


class InlineSuggestionRequest(BaseModel):
    """Schema para solicitar sugestoes inline"""
    cursor_line: int
    cursor_column: int
    context_lines: Optional[int] = 10


class CodeCompletionRequest(BaseModel):
    """Schema para code completion"""
    prefix: str
    cursor_line: int


class RefactorRequest(BaseModel):
    """Schema para solicitar refatoracao"""
    start_line: int
    end_line: int


class ExplainRequest(BaseModel):
    """Schema para explicar codigo"""
    start_line: int
    end_line: int


class AutonomousRequest(BaseModel):
    """Schema para desenvolvimento autonomo"""
    task_description: str
    max_iterations: Optional[int] = 5


# =============================================================================
# CLI TEST
# =============================================================================

if __name__ == "__main__":
    import asyncio

    async def test():
        print("[PairProgramming] Testando modulo...")

        pp = get_pair_programmer()
        print(f"[PairProgramming] Disponivel: {pp.is_available()}")

        if pp.is_available():
            # Criar sessao de teste
            session = pp.create_session(
                project_id="test-project",
                file_path="test.py",
                initial_code="""def hello():
    print("Hello World")

def add(a, b):
    return a + b
""",
                mode=CollaborationMode.GUIDED
            )
            print(f"[PairProgramming] Sessao criada: {session.session_id}")

            # Testar deteccao de bugs
            bugs = await pp.detect_bugs(session.session_id)
            print(f"[PairProgramming] Bugs detectados: {len(bugs)}")

            # Testar explicacao
            explanation = await pp.explain_code(session.session_id, 0, 1)
            if explanation:
                print(f"[PairProgramming] Explicacao: {explanation.summary}")

            # Testar sugestoes
            suggestions = await pp.suggest_next_steps(session.session_id)
            print(f"[PairProgramming] Proximos passos: {len(suggestions)}")

            # Encerrar sessao
            pp.end_session(session.session_id)
            print("[PairProgramming] Sessao encerrada")
        else:
            print("[PairProgramming] Claude nao disponivel para testes")

    asyncio.run(test())
