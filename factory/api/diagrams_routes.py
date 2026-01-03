"""
Diagrams Routes - API REST para gerenciamento de diagramas Draw.io.

Endpoints:
- GET  /api/diagrams/             - Listar diagramas
- POST /api/diagrams/             - Criar diagrama
- GET  /api/diagrams/templates    - Listar templates
- GET  /api/diagrams/{name}       - Detalhes de um diagrama
- DELETE /api/diagrams/{name}     - Deletar diagrama
- POST /api/diagrams/export       - Exportar diagrama
- POST /api/diagrams/open         - Abrir no VS Code
- POST /api/diagrams/generate     - Gerar diagrama do codigo
"""

from flask import Blueprint, request, jsonify, send_file
from typing import Optional
import sys
import os
from pathlib import Path

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.diagrams.drawio_manager import (
    get_drawio_manager, DiagramType
)

diagrams_bp = Blueprint('diagrams', __name__, url_prefix='/api/diagrams')


@diagrams_bp.route('/', methods=['GET'])
def list_diagrams():
    """Lista todos os diagramas disponiveis."""
    manager = get_drawio_manager()
    diagrams = manager.list_diagrams()

    return jsonify({
        "success": True,
        "data": [
            {
                "name": d.name,
                "type": d.type.value,
                "path": d.path,
                "created_at": d.created_at,
                "updated_at": d.updated_at,
                "description": d.description
            }
            for d in diagrams
        ]
    })


@diagrams_bp.route('/', methods=['POST'])
def create_diagram():
    """Cria um novo diagrama."""
    data = request.get_json() or {}

    name = data.get("name")
    diagram_type = data.get("type", "architecture")
    description = data.get("description", "")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    # Converter tipo
    try:
        dt = DiagramType(diagram_type)
    except ValueError:
        dt = DiagramType.CUSTOM

    manager = get_drawio_manager()
    diagram = manager.create_diagram(name, dt, description)

    return jsonify({
        "success": True,
        "message": f"Diagrama '{name}' criado",
        "data": {
            "name": diagram.name,
            "type": diagram.type.value,
            "path": diagram.path,
            "created_at": diagram.created_at
        }
    })


@diagrams_bp.route('/templates', methods=['GET'])
def list_templates():
    """Lista templates disponiveis."""
    templates = [
        {"type": dt.value, "name": _get_type_name(dt), "description": _get_type_description(dt)}
        for dt in DiagramType
    ]

    return jsonify({
        "success": True,
        "data": templates
    })


def _get_type_name(dt: DiagramType) -> str:
    """Retorna nome do tipo de diagrama."""
    names = {
        DiagramType.ARCHITECTURE: "Arquitetura",
        DiagramType.FLOWCHART: "Fluxograma",
        DiagramType.SEQUENCE: "Diagrama de Sequencia",
        DiagramType.ER_DIAGRAM: "Diagrama ER",
        DiagramType.CLASS_DIAGRAM: "Diagrama de Classes",
        DiagramType.COMPONENT: "Diagrama de Componentes",
        DiagramType.DEPLOYMENT: "Diagrama de Deploy",
        DiagramType.CUSTOM: "Customizado",
    }
    return names.get(dt, dt.value)


def _get_type_description(dt: DiagramType) -> str:
    """Retorna descricao do tipo de diagrama."""
    descriptions = {
        DiagramType.ARCHITECTURE: "Visao geral da arquitetura do sistema",
        DiagramType.FLOWCHART: "Fluxo de processos e decisoes",
        DiagramType.SEQUENCE: "Interacoes entre componentes ao longo do tempo",
        DiagramType.ER_DIAGRAM: "Modelo de entidade-relacionamento do banco",
        DiagramType.CLASS_DIAGRAM: "Classes e seus relacionamentos",
        DiagramType.COMPONENT: "Componentes do sistema e suas conexoes",
        DiagramType.DEPLOYMENT: "Infraestrutura e deploy",
        DiagramType.CUSTOM: "Diagrama livre sem template",
    }
    return descriptions.get(dt, "")


@diagrams_bp.route('/<name>', methods=['GET'])
def get_diagram(name: str):
    """Retorna detalhes de um diagrama."""
    manager = get_drawio_manager()
    diagram_path = manager.exports_path / f"{name}.drawio"

    if not diagram_path.exists():
        return jsonify({
            "success": False,
            "error": f"Diagrama '{name}' nao encontrado"
        }), 404

    stat = diagram_path.stat()
    from datetime import datetime

    return jsonify({
        "success": True,
        "data": {
            "name": name,
            "path": str(diagram_path),
            "size": stat.st_size,
            "created_at": datetime.fromtimestamp(stat.st_ctime).isoformat(),
            "updated_at": datetime.fromtimestamp(stat.st_mtime).isoformat()
        }
    })


@diagrams_bp.route('/<name>', methods=['DELETE'])
def delete_diagram(name: str):
    """Deleta um diagrama."""
    manager = get_drawio_manager()
    diagram_path = manager.exports_path / f"{name}.drawio"

    if not diagram_path.exists():
        return jsonify({
            "success": False,
            "error": f"Diagrama '{name}' nao encontrado"
        }), 404

    try:
        diagram_path.unlink()
        return jsonify({
            "success": True,
            "message": f"Diagrama '{name}' deletado"
        })
    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@diagrams_bp.route('/export', methods=['POST'])
def export_diagram():
    """Exporta diagrama para outro formato."""
    data = request.get_json() or {}

    name = data.get("name")
    output_format = data.get("format", "png")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    if output_format not in ["png", "svg", "pdf"]:
        return jsonify({
            "success": False,
            "error": "Formato invalido. Use: png, svg, pdf"
        }), 400

    manager = get_drawio_manager()
    diagram_path = manager.exports_path / f"{name}.drawio"

    if not diagram_path.exists():
        return jsonify({
            "success": False,
            "error": f"Diagrama '{name}' nao encontrado"
        }), 404

    output_path = manager.export_diagram(str(diagram_path), output_format)

    if output_path:
        return jsonify({
            "success": True,
            "message": f"Diagrama exportado para {output_format}",
            "data": {"path": output_path}
        })
    else:
        return jsonify({
            "success": False,
            "error": "Falha ao exportar. Verifique se draw.io esta instalado."
        }), 500


@diagrams_bp.route('/open', methods=['POST'])
def open_diagram():
    """Abre diagrama no VS Code."""
    data = request.get_json() or {}
    name = data.get("name")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    manager = get_drawio_manager()
    diagram_path = manager.exports_path / f"{name}.drawio"

    if not diagram_path.exists():
        return jsonify({
            "success": False,
            "error": f"Diagrama '{name}' nao encontrado"
        }), 404

    success = manager.open_in_vscode(str(diagram_path))

    if success:
        return jsonify({
            "success": True,
            "message": f"Diagrama '{name}' aberto no VS Code"
        })
    else:
        return jsonify({
            "success": False,
            "error": "Falha ao abrir VS Code. Verifique se esta instalado."
        }), 500


@diagrams_bp.route('/generate', methods=['POST'])
def generate_from_code():
    """Gera diagrama de arquitetura a partir do codigo."""
    data = request.get_json() or {}

    source_path = data.get("source_path")
    output_name = data.get("output_name", "architecture_auto")

    if not source_path:
        return jsonify({"success": False, "error": "source_path e obrigatorio"}), 400

    manager = get_drawio_manager()
    diagram = manager.generate_architecture_from_code(source_path, output_name)

    if diagram:
        return jsonify({
            "success": True,
            "message": f"Diagrama gerado a partir de {source_path}",
            "data": {
                "name": diagram.name,
                "path": diagram.path
            }
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Caminho nao encontrado: {source_path}"
        }), 404


@diagrams_bp.route('/download/<name>', methods=['GET'])
def download_diagram(name: str):
    """Faz download do arquivo do diagrama."""
    manager = get_drawio_manager()
    diagram_path = manager.exports_path / f"{name}.drawio"

    if not diagram_path.exists():
        return jsonify({
            "success": False,
            "error": f"Diagrama '{name}' nao encontrado"
        }), 404

    return send_file(
        diagram_path,
        as_attachment=True,
        download_name=f"{name}.drawio"
    )


def register_diagrams_routes(app):
    """Registra as rotas de diagramas no app Flask."""
    app.register_blueprint(diagrams_bp)
