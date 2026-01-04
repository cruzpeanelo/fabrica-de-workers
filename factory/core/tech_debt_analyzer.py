# -*- coding: utf-8 -*-
"""
Tech Debt Analyzer - Plataforma E
========================================
Modulo para analise de debito tecnico e refatoracao automatica.
"""

import os
import json
import fnmatch
from datetime import datetime
from collections import defaultdict
from typing import Optional, List, Dict, Any
from pathlib import Path

# Storage para historico de tech debt (em memoria)
tech_debt_history = defaultdict(list)


def analyze_file_complexity(file_path: str, content: str) -> dict:
    """Analisa complexidade de um arquivo"""
    lines = content.split('\n')
    total_lines = len(lines)
    code_lines = len([l for l in lines if l.strip() and not l.strip().startswith('#') and not l.strip().startswith('//')])
    code_smells = []

    for i, line in enumerate(lines):
        # Detectar TODO/FIXME
        if 'TODO' in line.upper() or 'FIXME' in line.upper() or 'HACK' in line.upper():
            code_smells.append({
                "type": "todo_fixme",
                "line": i + 1,
                "message": f"Marcador encontrado: {line.strip()[:100]}",
                "severity": "low"
            })

        # Linhas muito longas (mais de 120 caracteres)
        if len(line) > 120:
            code_smells.append({
                "type": "long_line",
                "line": i + 1,
                "message": f"Linha com {len(line)} caracteres (max recomendado: 120)",
                "severity": "low"
            })

        # Muitos parametros em funcao
        if 'def ' in line or 'function ' in line:
            params = line.count(',')
            if params >= 5:
                code_smells.append({
                    "type": "too_many_params",
                    "line": i + 1,
                    "message": f"Funcao com muitos parametros ({params + 1})",
                    "severity": "medium"
                })

        # Aninhamento profundo (mais de 4 niveis)
        indent = len(line) - len(line.lstrip())
        if indent >= 16:  # 4 tabs ou 16 espacos
            code_smells.append({
                "type": "deep_nesting",
                "line": i + 1,
                "message": "Aninhamento muito profundo (mais de 4 niveis)",
                "severity": "medium"
            })

    # Calcular complexidade ciclomatica simplificada
    complexity_keywords = ['if ', 'elif ', 'else:', 'for ', 'while ', 'try:', 'except', 'case ', 'switch ']
    cyclomatic = 1
    for line in lines:
        for keyword in complexity_keywords:
            if keyword in line:
                cyclomatic += 1

    # Score de complexidade (1-10)
    complexity_score = min(10, max(1,
        (cyclomatic / 10) * 3 +
        (total_lines / 500) * 3 +
        (len(code_smells) / 10) * 4
    ))

    return {
        "file_path": file_path,
        "total_lines": total_lines,
        "code_lines": code_lines,
        "cyclomatic_complexity": cyclomatic,
        "complexity_score": round(complexity_score, 1),
        "code_smells": code_smells,
        "todos_count": len([s for s in code_smells if s["type"] == "todo_fixme"])
    }


def detect_duplications(files_content: dict) -> list:
    """Detecta codigo duplicado entre arquivos"""
    duplications = []
    chunks = {}

    for file_path, content in files_content.items():
        lines = content.split('\n')
        for i in range(len(lines) - 4):
            chunk = '\n'.join(lines[i:i+5]).strip()
            if len(chunk) > 50:  # Ignorar chunks muito pequenos
                if chunk in chunks:
                    chunks[chunk].append((file_path, i + 1))
                else:
                    chunks[chunk] = [(file_path, i + 1)]

    for chunk, locations in chunks.items():
        if len(locations) > 1:
            duplications.append({
                "code_sample": chunk[:200] + "..." if len(chunk) > 200 else chunk,
                "locations": [{"file": loc[0], "line": loc[1]} for loc in locations],
                "severity": "high" if len(locations) > 3 else "medium"
            })

    return duplications[:20]  # Limitar a 20 duplicacoes


def generate_refactoring_suggestions(analysis_results: dict, claude_available: bool = False) -> list:
    """Gera sugestoes de refatoracao"""
    suggestions = []
    suggestion_id = 0

    for file_analysis in analysis_results.get("files", []):
        file_path = file_analysis.get("file_path", "")

        for smell in file_analysis.get("code_smells", []):
            suggestion_id += 1

            if smell["type"] == "too_many_params":
                suggestions.append({
                    "id": f"SUG-{suggestion_id:04d}",
                    "file_path": file_path,
                    "line": smell["line"],
                    "type": "extract_parameter_object",
                    "title": "Extrair objeto de parametros",
                    "description": "Considere criar uma classe/dict para agrupar os parametros relacionados",
                    "impact": "medium",
                    "effort": "low",
                    "priority": 2
                })

            elif smell["type"] == "deep_nesting":
                suggestions.append({
                    "id": f"SUG-{suggestion_id:04d}",
                    "file_path": file_path,
                    "line": smell["line"],
                    "type": "extract_method",
                    "title": "Extrair metodo",
                    "description": "Extraia o codigo aninhado para um metodo separado para melhorar legibilidade",
                    "impact": "high",
                    "effort": "medium",
                    "priority": 1
                })

            elif smell["type"] == "long_line":
                suggestions.append({
                    "id": f"SUG-{suggestion_id:04d}",
                    "file_path": file_path,
                    "line": smell["line"],
                    "type": "split_line",
                    "title": "Dividir linha longa",
                    "description": "Divida a linha em multiplas linhas para melhorar legibilidade",
                    "impact": "low",
                    "effort": "low",
                    "priority": 3
                })

        # Sugestao para arquivos muito complexos
        if file_analysis.get("complexity_score", 0) >= 7:
            suggestion_id += 1
            suggestions.append({
                "id": f"SUG-{suggestion_id:04d}",
                "file_path": file_path,
                "line": 1,
                "type": "split_module",
                "title": "Dividir modulo",
                "description": f"Arquivo com complexidade alta ({file_analysis.get('complexity_score')}). Considere dividir em modulos menores",
                "impact": "high",
                "effort": "high",
                "priority": 1
            })

    # Sugestoes para duplicacoes
    for dup in analysis_results.get("duplications", []):
        suggestion_id += 1
        suggestions.append({
            "id": f"SUG-{suggestion_id:04d}",
            "file_path": dup["locations"][0]["file"] if dup["locations"] else "",
            "line": dup["locations"][0]["line"] if dup["locations"] else 1,
            "type": "extract_common_code",
            "title": "Extrair codigo comum",
            "description": f"Codigo duplicado encontrado em {len(dup['locations'])} locais. Extraia para uma funcao reutilizavel",
            "impact": "high",
            "effort": "medium",
            "priority": 1,
            "duplicated_locations": dup["locations"]
        })

    # Ordenar por prioridade
    suggestions.sort(key=lambda x: x["priority"])
    return suggestions


def get_debt_recommendations(metrics: dict) -> list:
    """Gera recomendacoes baseadas nas metricas"""
    recommendations = []
    score = metrics.get("complexity_score", 0)
    dup = metrics.get("duplication_percentage", 0)
    todos = metrics.get("todos_count", 0)

    if score >= 7:
        recommendations.append({
            "priority": "high",
            "area": "complexity",
            "message": "Complexidade alta detectada. Priorize refatoracao de modulos complexos."
        })
    elif score >= 5:
        recommendations.append({
            "priority": "medium",
            "area": "complexity",
            "message": "Complexidade moderada. Considere simplificar funcoes longas."
        })

    if dup >= 20:
        recommendations.append({
            "priority": "high",
            "area": "duplication",
            "message": f"Alta duplicacao de codigo ({dup}%). Extraia codigo comum para funcoes reutilizaveis."
        })
    elif dup >= 10:
        recommendations.append({
            "priority": "medium",
            "area": "duplication",
            "message": f"Duplicacao moderada ({dup}%). Revise padroes de codigo repetitivo."
        })

    if todos >= 20:
        recommendations.append({
            "priority": "high",
            "area": "todos",
            "message": f"Muitos TODOs/FIXMEs ({todos}). Crie stories para resolver pendencias."
        })
    elif todos >= 5:
        recommendations.append({
            "priority": "low",
            "area": "todos",
            "message": f"{todos} TODOs/FIXMEs encontrados. Considere priorizar os mais importantes."
        })

    if not recommendations:
        recommendations.append({
            "priority": "low",
            "area": "general",
            "message": "Codigo em bom estado! Continue aplicando boas praticas."
        })

    return recommendations


def analyze_project_tech_debt(project_path: str, file_patterns: List[str] = None, exclude_patterns: List[str] = None, include_suggestions: bool = True) -> dict:
    """
    Analisa debito tecnico de um projeto

    Args:
        project_path: Caminho absoluto do projeto
        file_patterns: Padroes de arquivos para analisar (ex: ["*.py", "*.js"])
        exclude_patterns: Padroes para excluir (ex: ["node_modules", "__pycache__"])
        include_suggestions: Se deve gerar sugestoes de refatoracao

    Returns:
        dict com metricas, arquivos analisados e sugestoes
    """
    import glob as glob_module

    if file_patterns is None:
        file_patterns = ["*.py", "*.js", "*.ts", "*.tsx", "*.jsx"]
    if exclude_patterns is None:
        exclude_patterns = ["node_modules", "__pycache__", ".git", "venv", "dist", "build"]

    if not os.path.exists(project_path):
        return {
            "status": "no_files",
            "message": f"Pasta do projeto nao encontrada: {project_path}",
            "metrics": {
                "complexity_score": 0,
                "duplication_percentage": 0,
                "todos_count": 0,
                "total_files": 0,
                "total_lines": 0
            },
            "files": [],
            "suggestions": []
        }

    # Coletar arquivos
    files_to_analyze = []
    files_content = {}

    for pattern in file_patterns:
        search_pattern = os.path.join(project_path, "**", pattern)
        for file_path in glob_module.glob(search_pattern, recursive=True):
            # Verificar exclusoes
            should_exclude = any(exclude in file_path for exclude in exclude_patterns)

            if not should_exclude:
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        files_to_analyze.append(file_path)
                        files_content[file_path] = content
                except Exception as e:
                    print(f"[TechDebt] Erro ao ler {file_path}: {e}")

    if not files_to_analyze:
        return {
            "status": "no_files",
            "message": "Nenhum arquivo encontrado para analise",
            "metrics": {
                "complexity_score": 0,
                "duplication_percentage": 0,
                "todos_count": 0,
                "total_files": 0,
                "total_lines": 0
            },
            "files": [],
            "suggestions": []
        }

    # Analisar cada arquivo
    file_analyses = []
    total_lines = 0
    total_complexity = 0
    total_todos = 0
    all_smells = []

    for file_path in files_to_analyze:
        content = files_content[file_path]
        analysis = analyze_file_complexity(file_path, content)

        # Converter path para relativo
        rel_path = os.path.relpath(file_path, project_path)
        analysis["file_path"] = rel_path

        file_analyses.append(analysis)
        total_lines += analysis["total_lines"]
        total_complexity += analysis["complexity_score"]
        total_todos += analysis["todos_count"]
        all_smells.extend(analysis["code_smells"])

    # Detectar duplicacoes
    duplications = detect_duplications(files_content)

    # Calcular porcentagem de duplicacao
    duplicated_lines = sum(len(d["locations"]) * 5 for d in duplications)
    duplication_percentage = min(100, (duplicated_lines / max(1, total_lines)) * 100)

    # Calcular score geral
    avg_complexity = total_complexity / max(1, len(file_analyses))
    overall_score = round(avg_complexity, 1)

    # Preparar resultado
    analysis_result = {
        "files": file_analyses,
        "duplications": duplications
    }

    # Gerar sugestoes
    suggestions = []
    if include_suggestions:
        suggestions = generate_refactoring_suggestions(analysis_result, False)

    return {
        "status": "completed",
        "analyzed_at": datetime.utcnow().isoformat(),
        "metrics": {
            "complexity_score": overall_score,
            "duplication_percentage": round(duplication_percentage, 1),
            "todos_count": total_todos,
            "total_files": len(file_analyses),
            "total_lines": total_lines,
            "total_smells": len(all_smells),
            "smells_by_severity": {
                "high": len([s for s in all_smells if s.get("severity") == "high"]),
                "medium": len([s for s in all_smells if s.get("severity") == "medium"]),
                "low": len([s for s in all_smells if s.get("severity") == "low"])
            }
        },
        "files": sorted(file_analyses, key=lambda x: x["complexity_score"], reverse=True)[:20],
        "duplications": duplications,
        "suggestions": suggestions[:30]
    }


def add_to_history(project_id: str, metrics: dict):
    """Adiciona metricas ao historico"""
    history_entry = {
        "timestamp": datetime.utcnow().isoformat(),
        "complexity_score": metrics.get("complexity_score", 0),
        "duplication_percentage": metrics.get("duplication_percentage", 0),
        "todos_count": metrics.get("todos_count", 0),
        "total_files": metrics.get("total_files", 0),
        "total_smells": metrics.get("total_smells", 0)
    }
    tech_debt_history[project_id].append(history_entry)

    # Manter apenas ultimos 30 registros
    if len(tech_debt_history[project_id]) > 30:
        tech_debt_history[project_id] = tech_debt_history[project_id][-30:]


def get_history(project_id: str) -> list:
    """Retorna historico de analises de um projeto"""
    return tech_debt_history.get(project_id, [])


def get_trend(project_id: str) -> str:
    """Calcula tendencia do debito tecnico"""
    history = get_history(project_id)
    if len(history) < 2:
        return "stable"

    prev_score = history[-2].get("complexity_score", 0)
    curr_score = history[-1].get("complexity_score", 0)

    if curr_score < prev_score - 0.5:
        return "improving"
    elif curr_score > prev_score + 0.5:
        return "worsening"
    return "stable"
