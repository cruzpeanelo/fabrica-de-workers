"""
Draw.io Manager - Gerenciador de diagramas de arquitetura.

Funcionalidades:
- Criar diagramas a partir de templates
- Exportar para PNG/SVG/PDF
- Abrir no VS Code com extensao Draw.io
- Gerar diagramas a partir de codigo
"""

import os
import json
import subprocess
import shutil
from pathlib import Path
from typing import Optional, Dict, List, Any
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum


class DiagramType(Enum):
    """Tipos de diagramas disponiveis."""
    ARCHITECTURE = "architecture"
    FLOWCHART = "flowchart"
    SEQUENCE = "sequence"
    ER_DIAGRAM = "er_diagram"
    CLASS_DIAGRAM = "class_diagram"
    COMPONENT = "component"
    DEPLOYMENT = "deployment"
    CUSTOM = "custom"


@dataclass
class DiagramInfo:
    """Informacoes de um diagrama."""
    name: str
    type: DiagramType
    path: str
    created_at: str
    updated_at: str
    description: str = ""
    tags: List[str] = field(default_factory=list)


class DrawioManager:
    """Gerenciador de diagramas Draw.io."""

    def __init__(self, base_path: Optional[str] = None):
        """
        Inicializa o gerenciador.

        Args:
            base_path: Caminho base do projeto
        """
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.diagrams_path = self.base_path / "factory" / "diagrams"
        self.templates_path = self.diagrams_path / "templates"
        self.exports_path = self.diagrams_path / "exports"

        # Garantir diretorios existem
        self.templates_path.mkdir(parents=True, exist_ok=True)
        self.exports_path.mkdir(parents=True, exist_ok=True)

        # Criar templates padrao se nao existirem
        self._ensure_templates()

    def _ensure_templates(self):
        """Cria templates padrao se nao existirem."""
        templates = {
            "architecture": self._get_architecture_template(),
            "flowchart": self._get_flowchart_template(),
            "sequence": self._get_sequence_template(),
            "er_diagram": self._get_er_template(),
            "class_diagram": self._get_class_template(),
        }

        for name, content in templates.items():
            template_file = self.templates_path / f"{name}.drawio"
            if not template_file.exists():
                with open(template_file, 'w', encoding='utf-8') as f:
                    f.write(content)

    def _get_base_template(self, content: str = "") -> str:
        """Retorna estrutura base de um arquivo Draw.io."""
        return f'''<?xml version="1.0" encoding="UTF-8"?>
<mxfile host="app.diagrams.net" modified="{datetime.now().isoformat()}" version="21.0.0">
  <diagram name="Page-1" id="page-1">
    <mxGraphModel dx="1422" dy="794" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169" math="0" shadow="0">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />
{content}
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>'''

    def _get_architecture_template(self) -> str:
        """Template para diagrama de arquitetura."""
        content = '''        <!-- Container Principal -->
        <mxCell id="2" value="Sistema" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#dae8fc;strokeColor=#6c8ebf;fontSize=14;fontStyle=1;" vertex="1" parent="1">
          <mxGeometry x="340" y="40" width="160" height="60" as="geometry" />
        </mxCell>

        <!-- Componente Frontend -->
        <mxCell id="3" value="Frontend" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#d5e8d4;strokeColor=#82b366;" vertex="1" parent="1">
          <mxGeometry x="120" y="160" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- Componente Backend -->
        <mxCell id="4" value="Backend API" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#fff2cc;strokeColor=#d6b656;" vertex="1" parent="1">
          <mxGeometry x="360" y="160" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- Componente Database -->
        <mxCell id="5" value="Database" style="shape=cylinder3;whiteSpace=wrap;html=1;boundedLbl=1;backgroundOutline=1;size=15;fillColor=#f8cecc;strokeColor=#b85450;" vertex="1" parent="1">
          <mxGeometry x="600" y="145" width="80" height="90" as="geometry" />
        </mxCell>

        <!-- Conexoes -->
        <mxCell id="6" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" edge="1" parent="1" source="3" target="4">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="7" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" edge="1" parent="1" source="4" target="5">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>'''
        return self._get_base_template(content)

    def _get_flowchart_template(self) -> str:
        """Template para fluxograma."""
        content = '''        <!-- Inicio -->
        <mxCell id="2" value="Inicio" style="ellipse;whiteSpace=wrap;html=1;fillColor=#d5e8d4;strokeColor=#82b366;" vertex="1" parent="1">
          <mxGeometry x="370" y="40" width="100" height="50" as="geometry" />
        </mxCell>

        <!-- Processo -->
        <mxCell id="3" value="Processo" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
          <mxGeometry x="360" y="140" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- Decisao -->
        <mxCell id="4" value="Condicao?" style="rhombus;whiteSpace=wrap;html=1;fillColor=#fff2cc;strokeColor=#d6b656;" vertex="1" parent="1">
          <mxGeometry x="360" y="250" width="120" height="80" as="geometry" />
        </mxCell>

        <!-- Fim -->
        <mxCell id="5" value="Fim" style="ellipse;whiteSpace=wrap;html=1;fillColor=#f8cecc;strokeColor=#b85450;" vertex="1" parent="1">
          <mxGeometry x="370" y="380" width="100" height="50" as="geometry" />
        </mxCell>

        <!-- Conexoes -->
        <mxCell id="6" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" edge="1" parent="1" source="2" target="3">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="7" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" edge="1" parent="1" source="3" target="4">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="8" value="Sim" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" edge="1" parent="1" source="4" target="5">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>'''
        return self._get_base_template(content)

    def _get_sequence_template(self) -> str:
        """Template para diagrama de sequencia."""
        content = '''        <!-- Ator 1 -->
        <mxCell id="2" value="Usuario" style="shape=umlActor;verticalLabelPosition=bottom;verticalAlign=top;html=1;" vertex="1" parent="1">
          <mxGeometry x="120" y="80" width="30" height="60" as="geometry" />
        </mxCell>

        <!-- Sistema -->
        <mxCell id="3" value="Sistema" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
          <mxGeometry x="300" y="80" width="100" height="40" as="geometry" />
        </mxCell>

        <!-- Database -->
        <mxCell id="4" value="Database" style="shape=cylinder3;whiteSpace=wrap;html=1;boundedLbl=1;backgroundOutline=1;size=15;fillColor=#f8cecc;strokeColor=#b85450;" vertex="1" parent="1">
          <mxGeometry x="500" y="70" width="60" height="60" as="geometry" />
        </mxCell>

        <!-- Linha de vida Usuario -->
        <mxCell id="5" value="" style="endArrow=none;dashed=1;html=1;dashPattern=1 3;strokeWidth=2;" edge="1" parent="1">
          <mxGeometry width="50" height="50" relative="1" as="geometry">
            <mxPoint x="135" y="140" as="sourcePoint" />
            <mxPoint x="135" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>

        <!-- Linha de vida Sistema -->
        <mxCell id="6" value="" style="endArrow=none;dashed=1;html=1;dashPattern=1 3;strokeWidth=2;" edge="1" parent="1">
          <mxGeometry width="50" height="50" relative="1" as="geometry">
            <mxPoint x="350" y="120" as="sourcePoint" />
            <mxPoint x="350" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>

        <!-- Mensagem -->
        <mxCell id="7" value="Request" style="html=1;verticalAlign=bottom;endArrow=block;rounded=0;" edge="1" parent="1">
          <mxGeometry width="80" relative="1" as="geometry">
            <mxPoint x="140" y="180" as="sourcePoint" />
            <mxPoint x="345" y="180" as="targetPoint" />
          </mxGeometry>
        </mxCell>

        <!-- Response -->
        <mxCell id="8" value="Response" style="html=1;verticalAlign=bottom;endArrow=open;dashed=1;endSize=8;rounded=0;" edge="1" parent="1">
          <mxGeometry x="-1" relative="1" as="geometry">
            <mxPoint x="345" y="240" as="sourcePoint" />
            <mxPoint x="140" y="240" as="targetPoint" />
          </mxGeometry>
        </mxCell>'''
        return self._get_base_template(content)

    def _get_er_template(self) -> str:
        """Template para diagrama ER."""
        content = '''        <!-- Entidade Usuario -->
        <mxCell id="2" value="Usuario" style="swimlane;fontStyle=1;childLayout=stackLayout;horizontal=1;startSize=26;horizontalStack=0;resizeParent=1;resizeParentMax=0;resizeLast=0;collapsible=1;marginBottom=0;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
          <mxGeometry x="120" y="120" width="160" height="130" as="geometry" />
        </mxCell>
        <mxCell id="3" value="PK id: int" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;fontStyle=4;" vertex="1" parent="2">
          <mxGeometry y="26" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="4" value="nome: varchar" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="52" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="5" value="email: varchar" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="78" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="6" value="created_at: datetime" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="104" width="160" height="26" as="geometry" />
        </mxCell>

        <!-- Entidade Projeto -->
        <mxCell id="7" value="Projeto" style="swimlane;fontStyle=1;childLayout=stackLayout;horizontal=1;startSize=26;horizontalStack=0;resizeParent=1;resizeParentMax=0;resizeLast=0;collapsible=1;marginBottom=0;fillColor=#d5e8d4;strokeColor=#82b366;" vertex="1" parent="1">
          <mxGeometry x="440" y="120" width="160" height="130" as="geometry" />
        </mxCell>
        <mxCell id="8" value="PK id: int" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;fontStyle=4;" vertex="1" parent="7">
          <mxGeometry y="26" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="9" value="FK user_id: int" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;fontStyle=2;" vertex="1" parent="7">
          <mxGeometry y="52" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="10" value="nome: varchar" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="78" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="11" value="status: varchar" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="104" width="160" height="26" as="geometry" />
        </mxCell>

        <!-- Relacionamento -->
        <mxCell id="12" value="1" style="text;html=1;align=center;verticalAlign=middle;resizable=0;points=[];autosize=1;strokeColor=none;fillColor=none;" vertex="1" parent="1">
          <mxGeometry x="290" y="150" width="20" height="30" as="geometry" />
        </mxCell>
        <mxCell id="13" value="N" style="text;html=1;align=center;verticalAlign=middle;resizable=0;points=[];autosize=1;strokeColor=none;fillColor=none;" vertex="1" parent="1">
          <mxGeometry x="410" y="150" width="20" height="30" as="geometry" />
        </mxCell>
        <mxCell id="14" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;endArrow=none;endFill=0;" edge="1" parent="1" source="5" target="9">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>'''
        return self._get_base_template(content)

    def _get_class_template(self) -> str:
        """Template para diagrama de classes."""
        content = '''        <!-- Classe Base -->
        <mxCell id="2" value="BaseModel" style="swimlane;fontStyle=3;childLayout=stackLayout;horizontal=1;startSize=26;horizontalStack=0;resizeParent=1;resizeParentMax=0;resizeLast=0;collapsible=1;marginBottom=0;fillColor=#f5f5f5;strokeColor=#666666;fontColor=#333333;" vertex="1" parent="1">
          <mxGeometry x="280" y="40" width="160" height="104" as="geometry" />
        </mxCell>
        <mxCell id="3" value="# id: int" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="26" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="4" value="" style="line;strokeWidth=1;fillColor=none;align=left;verticalAlign=middle;spacingTop=-1;spacingLeft=3;spacingRight=3;rotatable=0;labelPosition=right;points=[];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="52" width="160" height="8" as="geometry" />
        </mxCell>
        <mxCell id="5" value="+ save(): void" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="60" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="6" value="+ delete(): void" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="2">
          <mxGeometry y="86" width="160" height="26" as="geometry" />
        </mxCell>

        <!-- Classe Usuario -->
        <mxCell id="7" value="Usuario" style="swimlane;fontStyle=1;childLayout=stackLayout;horizontal=1;startSize=26;horizontalStack=0;resizeParent=1;resizeParentMax=0;resizeLast=0;collapsible=1;marginBottom=0;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
          <mxGeometry x="120" y="220" width="160" height="130" as="geometry" />
        </mxCell>
        <mxCell id="8" value="- nome: string" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="26" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="9" value="- email: string" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="52" width="160" height="26" as="geometry" />
        </mxCell>
        <mxCell id="10" value="" style="line;strokeWidth=1;fillColor=none;align=left;verticalAlign=middle;spacingTop=-1;spacingLeft=3;spacingRight=3;rotatable=0;labelPosition=right;points=[];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="78" width="160" height="8" as="geometry" />
        </mxCell>
        <mxCell id="11" value="+ login(): bool" style="text;strokeColor=none;fillColor=none;align=left;verticalAlign=middle;spacingLeft=4;spacingRight=4;overflow=hidden;rotatable=0;points=[[0,0.5],[1,0.5]];portConstraint=eastwest;" vertex="1" parent="7">
          <mxGeometry y="86" width="160" height="26" as="geometry" />
        </mxCell>

        <!-- Heranca -->
        <mxCell id="12" style="edgeStyle=orthogonalEdgeStyle;rounded=0;orthogonalLoop=1;jettySize=auto;html=1;endArrow=block;endFill=0;endSize=16;" edge="1" parent="1" source="7" target="2">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>'''
        return self._get_base_template(content)

    def create_diagram(
        self,
        name: str,
        diagram_type: DiagramType = DiagramType.ARCHITECTURE,
        description: str = ""
    ) -> DiagramInfo:
        """
        Cria um novo diagrama a partir de template.

        Args:
            name: Nome do diagrama
            diagram_type: Tipo do diagrama
            description: Descricao do diagrama

        Returns:
            Informacoes do diagrama criado
        """
        # Carregar template
        template_file = self.templates_path / f"{diagram_type.value}.drawio"

        if template_file.exists():
            with open(template_file, 'r', encoding='utf-8') as f:
                content = f.read()
        else:
            content = self._get_base_template()

        # Salvar diagrama
        diagram_file = self.exports_path / f"{name}.drawio"
        with open(diagram_file, 'w', encoding='utf-8') as f:
            f.write(content)

        now = datetime.now().isoformat()

        return DiagramInfo(
            name=name,
            type=diagram_type,
            path=str(diagram_file),
            created_at=now,
            updated_at=now,
            description=description
        )

    def list_diagrams(self) -> List[DiagramInfo]:
        """Lista todos os diagramas disponiveis."""
        diagrams = []

        for file in self.exports_path.glob("*.drawio"):
            stat = file.stat()
            diagrams.append(DiagramInfo(
                name=file.stem,
                type=DiagramType.CUSTOM,
                path=str(file),
                created_at=datetime.fromtimestamp(stat.st_ctime).isoformat(),
                updated_at=datetime.fromtimestamp(stat.st_mtime).isoformat()
            ))

        return diagrams

    def open_in_vscode(self, diagram_path: str) -> bool:
        """
        Abre o diagrama no VS Code.

        Args:
            diagram_path: Caminho do diagrama

        Returns:
            True se abriu com sucesso
        """
        try:
            # Tentar abrir com VS Code
            subprocess.run(["code", diagram_path], check=True)
            return True
        except Exception as e:
            print(f"[DrawioManager] Erro ao abrir VS Code: {e}")
            return False

    def export_diagram(
        self,
        diagram_path: str,
        output_format: str = "png",
        output_path: Optional[str] = None
    ) -> Optional[str]:
        """
        Exporta diagrama para outro formato.

        Requer draw.io-export ou drawio CLI instalado.

        Args:
            diagram_path: Caminho do diagrama
            output_format: Formato de saida (png, svg, pdf)
            output_path: Caminho de saida (opcional)

        Returns:
            Caminho do arquivo exportado ou None
        """
        diagram_file = Path(diagram_path)
        if not diagram_file.exists():
            print(f"[DrawioManager] Diagrama nao encontrado: {diagram_path}")
            return None

        if not output_path:
            output_path = str(diagram_file.with_suffix(f".{output_format}"))

        # Tentar exportar com drawio CLI
        try:
            # Draw.io Desktop export
            drawio_paths = [
                "/Applications/draw.io.app/Contents/MacOS/draw.io",
                "C:/Program Files/draw.io/draw.io.exe",
                "drawio",
            ]

            drawio_cmd = None
            for path in drawio_paths:
                if shutil.which(path) or Path(path).exists():
                    drawio_cmd = path
                    break

            if drawio_cmd:
                subprocess.run([
                    drawio_cmd, "-x", "-f", output_format,
                    "-o", output_path, diagram_path
                ], check=True)
                return output_path
            else:
                print("[DrawioManager] draw.io CLI nao encontrado. Instale o draw.io Desktop.")
                return None

        except Exception as e:
            print(f"[DrawioManager] Erro ao exportar: {e}")
            return None

    def generate_architecture_from_code(
        self,
        source_path: str,
        output_name: str = "architecture_auto"
    ) -> Optional[DiagramInfo]:
        """
        Gera diagrama de arquitetura a partir do codigo fonte.

        Analisa imports e estrutura de diretorios.

        Args:
            source_path: Caminho do codigo fonte
            output_name: Nome do diagrama de saida

        Returns:
            Informacoes do diagrama gerado
        """
        source = Path(source_path)
        if not source.exists():
            return None

        # Analisar estrutura
        modules = []
        connections = []

        for py_file in source.rglob("*.py"):
            if "__pycache__" in str(py_file):
                continue

            rel_path = py_file.relative_to(source)
            module_name = str(rel_path.with_suffix("")).replace("/", ".").replace("\\", ".")
            modules.append(module_name)

            # Analisar imports
            try:
                with open(py_file, 'r', encoding='utf-8') as f:
                    content = f.read()

                for line in content.split('\n'):
                    if line.startswith('from ') or line.startswith('import '):
                        # Extrair modulo importado
                        parts = line.split()
                        if len(parts) >= 2:
                            imported = parts[1].split('.')[0]
                            if imported in modules:
                                connections.append((module_name, imported))
            except:
                continue

        # Criar diagrama simplificado
        diagram = self.create_diagram(output_name, DiagramType.ARCHITECTURE,
                                      f"Auto-generated from {source_path}")

        return diagram


# Instancia global
_drawio_manager: Optional[DrawioManager] = None


def get_drawio_manager(base_path: Optional[str] = None) -> DrawioManager:
    """Retorna instancia global do DrawioManager."""
    global _drawio_manager
    if _drawio_manager is None:
        _drawio_manager = DrawioManager(base_path)
    return _drawio_manager
