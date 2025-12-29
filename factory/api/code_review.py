# -*- coding: utf-8 -*-
"""
Code Review Assistido por IA
=============================
Endpoint e funcoes para realizar code review de tasks usando Claude AI.
"""
import json
from datetime import datetime
from typing import Optional
from pydantic import BaseModel


class CodeReviewRequest(BaseModel):
    code: Optional[str] = None  # Se nao fornecido, usa code_output da task


def perform_code_review(code: str, task_title: str, task_type: str, claude_client=None) -> dict:
    """
    Executa code review usando Claude AI ou analise basica.

    Args:
        code: Codigo a ser analisado
        task_title: Titulo da task
        task_type: Tipo da task
        claude_client: Cliente Claude (opcional)

    Returns:
        dict com score, summary, quality, security, performance, suggestions
    """
    if claude_client and claude_client.is_available():
        try:
            return code_review_with_claude(code, task_title, task_type, claude_client)
        except Exception as e:
            print(f"[CodeReview] Erro ao usar Claude: {e}")

    return code_review_basic(code, task_title)


def code_review_with_claude(code: str, task_title: str, task_type: str, claude) -> dict:
    """Executa code review usando Claude AI"""

    system_prompt = """Voce e um especialista em code review com experiencia em:
- Qualidade de codigo e boas praticas
- Seguranca de software e vulnerabilidades
- Performance e otimizacao
- Clean code e padroes de design

Analise o codigo fornecido e retorne APENAS um JSON valido no seguinte formato:
{
    "score": 85,
    "summary": "Resumo geral da analise em 2-3 frases",
    "quality": {
        "score": 80,
        "issues": ["Problema 1", "Problema 2"],
        "positives": ["Ponto positivo 1", "Ponto positivo 2"]
    },
    "security": {
        "score": 90,
        "vulnerabilities": ["Vulnerabilidade (se houver)"],
        "recommendations": ["Recomendacao de seguranca"]
    },
    "performance": {
        "score": 85,
        "issues": ["Problema de performance (se houver)"],
        "suggestions": ["Sugestao de otimizacao"]
    },
    "suggestions": [
        {
            "type": "improvement",
            "priority": "high",
            "title": "Titulo da sugestao",
            "description": "Descricao detalhada"
        }
    ],
    "best_practices": {
        "followed": ["Boa pratica seguida"],
        "missing": ["Boa pratica nao seguida"]
    }
}

IMPORTANTE:
- score geral de 0-100
- Seja especifico e construtivo
- Aponte tanto problemas quanto pontos positivos
- Se o codigo estiver bom, reconheca isso"""

    prompt = f"""Faca um code review completo do seguinte codigo:

CONTEXTO DA TASK: {task_title}
TIPO: {task_type}

CODIGO:
```
{code[:8000]}
```

Analise aspectos de:
1. QUALIDADE: Legibilidade, organizacao, naming, duplicacao
2. SEGURANCA: Vulnerabilidades, injection, exposicao de dados
3. PERFORMANCE: Complexidade, loops, queries, memoria
4. BOAS PRATICAS: SOLID, DRY, patterns

Retorne o JSON com a analise completa."""

    response = claude.chat(prompt, system_prompt, max_tokens=4096)

    if response.success:
        try:
            content = response.content.strip()

            # Remover markdown code blocks se presentes
            if content.startswith("```"):
                lines = content.split("\n")
                content = "\n".join(lines[1:-1] if lines[-1] == "```" else lines[1:])

            result = json.loads(content)
            result["reviewed_at"] = datetime.utcnow().isoformat()
            result["reviewed_by"] = "claude-ai"
            result["code_length"] = len(code)
            return result

        except json.JSONDecodeError as e:
            print(f"[CodeReview] JSON parse error: {e}")
            return {
                "score": 70,
                "summary": "Analise realizada mas formato de resposta invalido",
                "raw_response": response.content[:2000],
                "quality": {"score": 70, "issues": [], "positives": []},
                "security": {"score": 70, "vulnerabilities": [], "recommendations": []},
                "performance": {"score": 70, "issues": [], "suggestions": []},
                "suggestions": [],
                "reviewed_at": datetime.utcnow().isoformat(),
                "reviewed_by": "claude-ai-raw"
            }

    # Fallback se Claude falhar
    return code_review_basic(code, task_title)


def code_review_basic(code: str, task_title: str) -> dict:
    """
    Code review basico baseado em regras (fallback quando Claude nao disponivel)
    """
    issues = []
    positives = []
    security_issues = []
    performance_issues = []
    suggestions = []
    score = 70  # Score base

    code_lower = code.lower()
    lines = code.split('\n')
    line_count = len(lines)

    # === ANALISE DE QUALIDADE ===

    # Verificar tamanho do codigo
    if line_count > 500:
        issues.append(f"Arquivo muito extenso ({line_count} linhas). Considere dividir em modulos menores.")
        score -= 5
    elif line_count < 200:
        positives.append("Codigo com tamanho adequado e focado")
        score += 2

    # Verificar linhas muito longas
    long_lines = sum(1 for line in lines if len(line) > 120)
    if long_lines > 10:
        issues.append(f"{long_lines} linhas excedem 120 caracteres. Considere quebrar linhas longas.")
        score -= 3

    # Verificar comentarios
    comment_lines = sum(1 for line in lines if line.strip().startswith(('#', '//', '/*', '*')))
    comment_ratio = comment_lines / max(line_count, 1)
    if comment_ratio < 0.05:
        issues.append("Codigo com poucos comentarios. Adicione documentacao.")
        score -= 3
    elif comment_ratio > 0.15:
        positives.append("Boa documentacao com comentarios")
        score += 3

    # Verificar funcoes/metodos muito longos (heuristica simples)
    if 'def ' in code:
        func_count = code.count('def ')
        if func_count > 0:
            avg_lines_per_func = line_count / func_count
            if avg_lines_per_func > 50:
                issues.append("Funcoes muito extensas. Considere dividir em funcoes menores.")
                score -= 5
            elif avg_lines_per_func < 30:
                positives.append("Funcoes com tamanho adequado")
                score += 2

    # === ANALISE DE SEGURANCA ===

    # SQL Injection
    if any(pattern in code_lower for pattern in ['execute(', 'raw(', 'cursor.execute']):
        if 'format(' in code_lower or '%s' in code or 'f"' in code or "f'" in code:
            security_issues.append("Possivel vulnerabilidade de SQL Injection. Use parametros preparados.")
            score -= 10
            suggestions.append({
                "type": "security",
                "priority": "critical",
                "title": "Possivel SQL Injection",
                "description": "Evite concatenar strings em queries SQL. Use parametros preparados."
            })

    # Credenciais hardcoded
    if any(pattern in code_lower for pattern in ['password =', 'api_key =', 'secret =', 'token =']):
        if any(char in code for char in ['"', "'"]):
            security_issues.append("Possivel credencial hardcoded. Use variaveis de ambiente.")
            score -= 8
            suggestions.append({
                "type": "security",
                "priority": "critical",
                "title": "Credenciais Hardcoded",
                "description": "Mova credenciais para variaveis de ambiente ou secrets manager."
            })

    # eval() ou exec()
    if 'eval(' in code_lower or 'exec(' in code_lower:
        security_issues.append("Uso de eval/exec pode ser perigoso. Evite executar codigo dinamico.")
        score -= 10

    # === ANALISE DE PERFORMANCE ===

    # Loops aninhados
    if code.count('for ') > 2 or code.count('while ') > 2:
        performance_issues.append("Multiplos loops detectados. Verifique complexidade.")
        score -= 3

    # Imports desnecessarios
    if 'import *' in code:
        performance_issues.append("Evite 'import *'. Importe apenas o necessario.")
        score -= 2

    # Print em producao
    if 'print(' in code and line_count > 50:
        performance_issues.append("Muitos print() no codigo. Considere usar logging em producao.")
        score -= 2

    # === BOAS PRATICAS ===
    best_practices_followed = []
    best_practices_missing = []

    # Type hints (Python)
    if 'def ' in code:
        if '->' in code or ': ' in code:
            best_practices_followed.append("Uso de type hints")
            score += 2
        else:
            best_practices_missing.append("Adicionar type hints para melhor documentacao")

    # Docstrings
    if '"""' in code or "'''" in code:
        best_practices_followed.append("Uso de docstrings")
        score += 2
    elif 'def ' in code:
        best_practices_missing.append("Adicionar docstrings nas funcoes")

    # Error handling
    if 'try:' in code or 'except' in code or 'catch' in code:
        best_practices_followed.append("Tratamento de erros implementado")
        score += 3
    else:
        best_practices_missing.append("Adicionar tratamento de excecoes")

    # Ajustar score para ficar entre 0-100
    score = max(0, min(100, score))

    # Calcular scores por categoria
    quality_score = min(100, max(0, 70 + len(positives) * 5 - len(issues) * 5))
    security_score = max(0, 100 - len(security_issues) * 20)
    performance_score = max(0, 100 - len(performance_issues) * 10)

    return {
        "score": score,
        "summary": f"Analise automatica do codigo ({line_count} linhas). "
                   f"{'Alguns problemas encontrados.' if issues or security_issues else 'Codigo com boa qualidade geral.'}",
        "quality": {
            "score": quality_score,
            "issues": issues,
            "positives": positives
        },
        "security": {
            "score": security_score,
            "vulnerabilities": security_issues,
            "recommendations": [
                "Use parametros preparados para queries SQL",
                "Armazene credenciais em variaveis de ambiente",
                "Valide todas as entradas do usuario"
            ] if security_issues else []
        },
        "performance": {
            "score": performance_score,
            "issues": performance_issues,
            "suggestions": [
                "Considere caching para operacoes repetidas",
                "Use generators para grandes colecoes de dados"
            ] if performance_issues else []
        },
        "suggestions": suggestions,
        "best_practices": {
            "followed": best_practices_followed,
            "missing": best_practices_missing
        },
        "reviewed_at": datetime.utcnow().isoformat(),
        "reviewed_by": "auto-analyzer",
        "code_length": len(code),
        "line_count": line_count
    }


def get_score_class(score: int) -> str:
    """Retorna classe CSS baseada no score"""
    if score >= 80:
        return "text-green-600"
    elif score >= 60:
        return "text-yellow-600"
    else:
        return "text-red-600"
