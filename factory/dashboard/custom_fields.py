# -*- coding: utf-8 -*-
"""
Custom Fields Module (Issue #251)
=================================
Campos customizados por projeto para stories.

Funcionalidades:
1. Definir campos customizados por projeto
2. Tipos: text, number, date, select, multiselect, checkbox
3. Validacoes configuraveis
4. Exibicao no card da story

Endpoints:
- GET /api/projects/{id}/custom-fields - Lista campos
- POST /api/projects/{id}/custom-fields - Cria campo
- PUT /api/custom-fields/{id} - Atualiza campo
- DELETE /api/custom-fields/{id} - Remove campo
- GET /api/stories/{id}/custom-values - Valores da story
- PUT /api/stories/{id}/custom-values - Atualiza valores
"""

from fastapi import FastAPI, HTTPException, Query
from pydantic import BaseModel, Field, validator
from typing import Optional, List, Dict, Any, Union
from datetime import datetime
from enum import Enum
import uuid
import re

# =============================================================================
# ENUMS
# =============================================================================

class FieldType(str, Enum):
    """Tipos de campos customizados"""
    TEXT = "text"
    NUMBER = "number"
    DATE = "date"
    SELECT = "select"
    MULTISELECT = "multiselect"
    CHECKBOX = "checkbox"


# =============================================================================
# MODELS (In-memory storage - in production use database)
# =============================================================================

# Storage for custom fields (project_id -> list of fields)
custom_fields_store: Dict[str, List[dict]] = {}

# Storage for field values (story_id -> field_id -> value)
custom_values_store: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class FieldOption(BaseModel):
    """Opcao para campos select/multiselect"""
    id: str = Field(default_factory=lambda: f"opt-{uuid.uuid4().hex[:8]}")
    label: str
    value: str
    color: Optional[str] = "#6B7280"
    icon: Optional[str] = None


class FieldValidation(BaseModel):
    """Regras de validacao do campo"""
    required: bool = False
    min_length: Optional[int] = None  # Para text
    max_length: Optional[int] = None  # Para text
    min_value: Optional[float] = None  # Para number
    max_value: Optional[float] = None  # Para number
    pattern: Optional[str] = None  # Regex para text
    min_date: Optional[str] = None  # ISO date string
    max_date: Optional[str] = None  # ISO date string
    min_selections: Optional[int] = None  # Para multiselect
    max_selections: Optional[int] = None  # Para multiselect


class CustomFieldCreate(BaseModel):
    """Schema para criar campo customizado"""
    name: str = Field(..., min_length=1, max_length=100)
    field_type: FieldType
    description: Optional[str] = None
    placeholder: Optional[str] = None
    default_value: Optional[Any] = None
    options: Optional[List[FieldOption]] = None  # Para select/multiselect
    validation: Optional[FieldValidation] = None
    show_in_card: bool = True
    position: Optional[int] = None
    is_active: bool = True

    @validator('options')
    def validate_options(cls, v, values):
        field_type = values.get('field_type')
        if field_type in [FieldType.SELECT, FieldType.MULTISELECT]:
            if not v or len(v) == 0:
                raise ValueError(f"Campo {field_type} requer pelo menos uma opcao")
        return v


class CustomFieldUpdate(BaseModel):
    """Schema para atualizar campo customizado"""
    name: Optional[str] = Field(None, min_length=1, max_length=100)
    description: Optional[str] = None
    placeholder: Optional[str] = None
    default_value: Optional[Any] = None
    options: Optional[List[FieldOption]] = None
    validation: Optional[FieldValidation] = None
    show_in_card: Optional[bool] = None
    position: Optional[int] = None
    is_active: Optional[bool] = None


class CustomFieldValue(BaseModel):
    """Schema para valor de campo"""
    field_id: str
    value: Any


class CustomValuesUpdate(BaseModel):
    """Schema para atualizar valores de campos"""
    values: List[CustomFieldValue]


# =============================================================================
# VALIDATORS
# =============================================================================

class FieldValidator:
    """Validador de valores por tipo de campo"""

    @staticmethod
    def validate(field: dict, value: Any) -> tuple[bool, str]:
        """
        Valida um valor para um campo customizado

        Returns:
            tuple: (is_valid, error_message)
        """
        field_type = field.get("field_type")
        validation = field.get("validation", {}) or {}

        # Check required
        if validation.get("required") and (value is None or value == "" or value == []):
            return False, f"Campo '{field.get('name')}' e obrigatorio"

        # Skip further validation if empty and not required
        if value is None or value == "" or value == []:
            return True, ""

        # Type-specific validation
        validators = {
            FieldType.TEXT.value: FieldValidator._validate_text,
            FieldType.NUMBER.value: FieldValidator._validate_number,
            FieldType.DATE.value: FieldValidator._validate_date,
            FieldType.SELECT.value: FieldValidator._validate_select,
            FieldType.MULTISELECT.value: FieldValidator._validate_multiselect,
            FieldType.CHECKBOX.value: FieldValidator._validate_checkbox,
        }

        validator_func = validators.get(field_type)
        if validator_func:
            return validator_func(field, value, validation)

        return True, ""

    @staticmethod
    def _validate_text(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo de texto"""
        if not isinstance(value, str):
            return False, f"Campo '{field.get('name')}' deve ser texto"

        if validation.get("min_length") and len(value) < validation["min_length"]:
            return False, f"Campo '{field.get('name')}' deve ter no minimo {validation['min_length']} caracteres"

        if validation.get("max_length") and len(value) > validation["max_length"]:
            return False, f"Campo '{field.get('name')}' deve ter no maximo {validation['max_length']} caracteres"

        if validation.get("pattern"):
            try:
                if not re.match(validation["pattern"], value):
                    return False, f"Campo '{field.get('name')}' nao corresponde ao formato esperado"
            except re.error:
                pass  # Invalid regex, skip validation

        return True, ""

    @staticmethod
    def _validate_number(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo numerico"""
        try:
            num_value = float(value)
        except (ValueError, TypeError):
            return False, f"Campo '{field.get('name')}' deve ser um numero"

        if validation.get("min_value") is not None and num_value < validation["min_value"]:
            return False, f"Campo '{field.get('name')}' deve ser no minimo {validation['min_value']}"

        if validation.get("max_value") is not None and num_value > validation["max_value"]:
            return False, f"Campo '{field.get('name')}' deve ser no maximo {validation['max_value']}"

        return True, ""

    @staticmethod
    def _validate_date(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo de data"""
        try:
            date_value = datetime.fromisoformat(value.replace('Z', '+00:00'))
        except (ValueError, AttributeError):
            return False, f"Campo '{field.get('name')}' deve ser uma data valida (ISO format)"

        if validation.get("min_date"):
            try:
                min_date = datetime.fromisoformat(validation["min_date"].replace('Z', '+00:00'))
                if date_value < min_date:
                    return False, f"Campo '{field.get('name')}' deve ser posterior a {validation['min_date']}"
            except ValueError:
                pass

        if validation.get("max_date"):
            try:
                max_date = datetime.fromisoformat(validation["max_date"].replace('Z', '+00:00'))
                if date_value > max_date:
                    return False, f"Campo '{field.get('name')}' deve ser anterior a {validation['max_date']}"
            except ValueError:
                pass

        return True, ""

    @staticmethod
    def _validate_select(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo select"""
        options = field.get("options", [])
        valid_values = [opt.get("value") for opt in options]

        if value not in valid_values:
            return False, f"Campo '{field.get('name')}' deve ser uma das opcoes validas"

        return True, ""

    @staticmethod
    def _validate_multiselect(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo multiselect"""
        if not isinstance(value, list):
            return False, f"Campo '{field.get('name')}' deve ser uma lista de opcoes"

        options = field.get("options", [])
        valid_values = [opt.get("value") for opt in options]

        for v in value:
            if v not in valid_values:
                return False, f"Campo '{field.get('name')}' contem opcao invalida: {v}"

        if validation.get("min_selections") and len(value) < validation["min_selections"]:
            return False, f"Campo '{field.get('name')}' requer no minimo {validation['min_selections']} selecoes"

        if validation.get("max_selections") and len(value) > validation["max_selections"]:
            return False, f"Campo '{field.get('name')}' permite no maximo {validation['max_selections']} selecoes"

        return True, ""

    @staticmethod
    def _validate_checkbox(field: dict, value: Any, validation: dict) -> tuple[bool, str]:
        """Valida campo checkbox"""
        if not isinstance(value, bool):
            return False, f"Campo '{field.get('name')}' deve ser verdadeiro ou falso"

        return True, ""


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_project_fields(project_id: str) -> List[dict]:
    """Retorna todos os campos customizados de um projeto"""
    return custom_fields_store.get(project_id, [])


def get_field_by_id(field_id: str) -> tuple[Optional[dict], Optional[str]]:
    """Retorna um campo pelo ID e o project_id associado"""
    for project_id, fields in custom_fields_store.items():
        for field in fields:
            if field["id"] == field_id:
                return field, project_id
    return None, None


def get_story_values(story_id: str) -> Dict[str, Any]:
    """Retorna os valores de campos customizados de uma story"""
    return custom_values_store.get(story_id, {})


# =============================================================================
# REGISTER ENDPOINTS
# =============================================================================

def register_custom_fields(app: FastAPI):
    """Registra endpoints de campos customizados"""

    # -------------------------------------------------------------------------
    # Custom Fields Endpoints
    # -------------------------------------------------------------------------

    @app.get("/api/projects/{project_id}/custom-fields")
    def list_custom_fields(
        project_id: str,
        active_only: bool = Query(False, description="Retornar apenas campos ativos")
    ):
        """Lista campos customizados de um projeto"""
        fields = get_project_fields(project_id)

        if active_only:
            fields = [f for f in fields if f.get("is_active", True)]

        # Sort by position
        fields = sorted(fields, key=lambda x: x.get("position", 0))

        return {
            "project_id": project_id,
            "fields": fields,
            "total": len(fields)
        }

    @app.post("/api/projects/{project_id}/custom-fields")
    def create_custom_field(project_id: str, field_data: CustomFieldCreate):
        """Cria um novo campo customizado para o projeto"""
        # Initialize project fields if not exists
        if project_id not in custom_fields_store:
            custom_fields_store[project_id] = []

        fields = custom_fields_store[project_id]

        # Check for duplicate name
        existing_names = [f["name"].lower() for f in fields]
        if field_data.name.lower() in existing_names:
            raise HTTPException(400, f"Campo com nome '{field_data.name}' ja existe no projeto")

        # Calculate position if not provided
        if field_data.position is None:
            field_data.position = len(fields)

        # Create field
        field_id = f"cf-{uuid.uuid4().hex[:12]}"
        now = datetime.utcnow().isoformat()

        new_field = {
            "id": field_id,
            "project_id": project_id,
            "name": field_data.name,
            "field_type": field_data.field_type.value,
            "description": field_data.description,
            "placeholder": field_data.placeholder,
            "default_value": field_data.default_value,
            "options": [opt.dict() for opt in field_data.options] if field_data.options else None,
            "validation": field_data.validation.dict() if field_data.validation else None,
            "show_in_card": field_data.show_in_card,
            "position": field_data.position,
            "is_active": field_data.is_active,
            "created_at": now,
            "updated_at": now
        }

        fields.append(new_field)

        return {
            "success": True,
            "message": f"Campo '{field_data.name}' criado com sucesso",
            "field": new_field
        }

    @app.put("/api/custom-fields/{field_id}")
    def update_custom_field(field_id: str, field_data: CustomFieldUpdate):
        """Atualiza um campo customizado"""
        field, project_id = get_field_by_id(field_id)

        if not field:
            raise HTTPException(404, f"Campo {field_id} nao encontrado")

        fields = custom_fields_store[project_id]

        # Check for duplicate name if changing
        if field_data.name and field_data.name.lower() != field["name"].lower():
            existing_names = [f["name"].lower() for f in fields if f["id"] != field_id]
            if field_data.name.lower() in existing_names:
                raise HTTPException(400, f"Campo com nome '{field_data.name}' ja existe no projeto")

        # Update fields
        update_data = field_data.dict(exclude_unset=True)
        if "options" in update_data and update_data["options"]:
            update_data["options"] = [opt.dict() if hasattr(opt, 'dict') else opt for opt in update_data["options"]]
        if "validation" in update_data and update_data["validation"]:
            update_data["validation"] = update_data["validation"].dict() if hasattr(update_data["validation"], 'dict') else update_data["validation"]

        for key, value in update_data.items():
            field[key] = value

        field["updated_at"] = datetime.utcnow().isoformat()

        return {
            "success": True,
            "message": f"Campo atualizado com sucesso",
            "field": field
        }

    @app.delete("/api/custom-fields/{field_id}")
    def delete_custom_field(field_id: str, hard_delete: bool = Query(False)):
        """Remove um campo customizado"""
        field, project_id = get_field_by_id(field_id)

        if not field:
            raise HTTPException(404, f"Campo {field_id} nao encontrado")

        if hard_delete:
            # Remove field completely
            custom_fields_store[project_id] = [
                f for f in custom_fields_store[project_id] if f["id"] != field_id
            ]

            # Remove all values for this field
            for story_id in custom_values_store:
                if field_id in custom_values_store[story_id]:
                    del custom_values_store[story_id][field_id]

            return {
                "success": True,
                "message": f"Campo removido permanentemente"
            }
        else:
            # Soft delete - just deactivate
            field["is_active"] = False
            field["updated_at"] = datetime.utcnow().isoformat()

            return {
                "success": True,
                "message": f"Campo desativado com sucesso"
            }

    @app.post("/api/projects/{project_id}/custom-fields/reorder")
    def reorder_custom_fields(project_id: str, field_ids: List[str]):
        """Reordena campos customizados"""
        fields = get_project_fields(project_id)

        if not fields:
            raise HTTPException(404, f"Projeto {project_id} nao possui campos customizados")

        # Validate all field_ids exist
        existing_ids = {f["id"] for f in fields}
        for fid in field_ids:
            if fid not in existing_ids:
                raise HTTPException(400, f"Campo {fid} nao encontrado no projeto")

        # Update positions
        for position, field_id in enumerate(field_ids):
            for field in fields:
                if field["id"] == field_id:
                    field["position"] = position
                    field["updated_at"] = datetime.utcnow().isoformat()
                    break

        return {
            "success": True,
            "message": "Campos reordenados com sucesso"
        }

    # -------------------------------------------------------------------------
    # Custom Field Values Endpoints
    # -------------------------------------------------------------------------

    @app.get("/api/stories/{story_id}/custom-values")
    def get_story_custom_values(story_id: str, project_id: Optional[str] = None):
        """Retorna valores de campos customizados de uma story"""
        values = get_story_values(story_id)

        # If project_id provided, include field metadata
        if project_id:
            fields = get_project_fields(project_id)
            field_map = {f["id"]: f for f in fields}

            enriched_values = []
            for field_id, value in values.items():
                field_info = field_map.get(field_id, {})
                enriched_values.append({
                    "field_id": field_id,
                    "field_name": field_info.get("name"),
                    "field_type": field_info.get("field_type"),
                    "value": value,
                    "show_in_card": field_info.get("show_in_card", True)
                })

            return {
                "story_id": story_id,
                "values": enriched_values,
                "raw_values": values
            }

        return {
            "story_id": story_id,
            "values": values
        }

    @app.put("/api/stories/{story_id}/custom-values")
    def update_story_custom_values(
        story_id: str,
        data: CustomValuesUpdate,
        project_id: Optional[str] = None,
        validate: bool = Query(True, description="Validar valores antes de salvar")
    ):
        """Atualiza valores de campos customizados de uma story"""
        # Initialize story values if not exists
        if story_id not in custom_values_store:
            custom_values_store[story_id] = {}

        errors = []

        # Get fields for validation
        fields_map = {}
        if project_id:
            fields = get_project_fields(project_id)
            fields_map = {f["id"]: f for f in fields}

        # Process each value
        for item in data.values:
            field = fields_map.get(item.field_id)

            # Validate if requested and field found
            if validate and field:
                is_valid, error = FieldValidator.validate(field, item.value)
                if not is_valid:
                    errors.append(error)
                    continue

            # Save value
            custom_values_store[story_id][item.field_id] = item.value

        if errors:
            return {
                "success": False,
                "message": "Alguns valores nao passaram na validacao",
                "errors": errors,
                "values": custom_values_store[story_id]
            }

        return {
            "success": True,
            "message": "Valores atualizados com sucesso",
            "values": custom_values_store[story_id]
        }

    @app.delete("/api/stories/{story_id}/custom-values/{field_id}")
    def delete_story_custom_value(story_id: str, field_id: str):
        """Remove valor de um campo customizado de uma story"""
        if story_id not in custom_values_store:
            raise HTTPException(404, f"Story {story_id} nao possui valores customizados")

        if field_id not in custom_values_store[story_id]:
            raise HTTPException(404, f"Valor para campo {field_id} nao encontrado")

        del custom_values_store[story_id][field_id]

        return {
            "success": True,
            "message": "Valor removido com sucesso"
        }

    # -------------------------------------------------------------------------
    # Bulk Operations
    # -------------------------------------------------------------------------

    @app.post("/api/projects/{project_id}/custom-fields/bulk-update-values")
    def bulk_update_custom_values(
        project_id: str,
        updates: List[Dict[str, Any]]
    ):
        """
        Atualiza valores customizados em massa para multiplas stories

        Body format:
        [
            {"story_id": "STR-001", "values": {"field_id": "value"}},
            {"story_id": "STR-002", "values": {"field_id": "value"}}
        ]
        """
        fields = get_project_fields(project_id)
        fields_map = {f["id"]: f for f in fields}

        results = []
        for update in updates:
            story_id = update.get("story_id")
            values = update.get("values", {})

            if not story_id:
                results.append({"story_id": None, "success": False, "error": "story_id obrigatorio"})
                continue

            if story_id not in custom_values_store:
                custom_values_store[story_id] = {}

            errors = []
            for field_id, value in values.items():
                field = fields_map.get(field_id)
                if field:
                    is_valid, error = FieldValidator.validate(field, value)
                    if not is_valid:
                        errors.append(error)
                        continue

                custom_values_store[story_id][field_id] = value

            results.append({
                "story_id": story_id,
                "success": len(errors) == 0,
                "errors": errors if errors else None
            })

        return {
            "success": all(r["success"] for r in results),
            "results": results
        }

    # -------------------------------------------------------------------------
    # Field Templates
    # -------------------------------------------------------------------------

    @app.get("/api/custom-fields/templates")
    def get_field_templates():
        """Retorna templates de campos customizados pre-definidos"""
        return {
            "templates": [
                {
                    "id": "priority_custom",
                    "name": "Prioridade Customizada",
                    "description": "Campo select com niveis de prioridade",
                    "field_type": "select",
                    "options": [
                        {"label": "Critica", "value": "critical", "color": "#EF4444"},
                        {"label": "Alta", "value": "high", "color": "#F97316"},
                        {"label": "Media", "value": "medium", "color": "#EAB308"},
                        {"label": "Baixa", "value": "low", "color": "#22C55E"}
                    ],
                    "show_in_card": True
                },
                {
                    "id": "effort_estimate",
                    "name": "Estimativa de Esforco",
                    "description": "Campo numerico para horas estimadas",
                    "field_type": "number",
                    "placeholder": "Horas estimadas",
                    "validation": {"min_value": 0, "max_value": 999},
                    "show_in_card": True
                },
                {
                    "id": "due_date_custom",
                    "name": "Data de Entrega",
                    "description": "Campo de data para deadline",
                    "field_type": "date",
                    "validation": {"required": False},
                    "show_in_card": True
                },
                {
                    "id": "environment",
                    "name": "Ambiente",
                    "description": "Ambiente(s) afetado(s)",
                    "field_type": "multiselect",
                    "options": [
                        {"label": "Desenvolvimento", "value": "dev", "color": "#3B82F6"},
                        {"label": "Homologacao", "value": "staging", "color": "#8B5CF6"},
                        {"label": "Producao", "value": "prod", "color": "#EF4444"}
                    ],
                    "show_in_card": False
                },
                {
                    "id": "is_blocker",
                    "name": "E Bloqueador?",
                    "description": "Indica se a story bloqueia outras",
                    "field_type": "checkbox",
                    "default_value": False,
                    "show_in_card": True
                },
                {
                    "id": "customer_request",
                    "name": "Solicitacao do Cliente",
                    "description": "Descricao da solicitacao do cliente",
                    "field_type": "text",
                    "placeholder": "Descreva a solicitacao...",
                    "validation": {"max_length": 500},
                    "show_in_card": False
                },
                {
                    "id": "business_value",
                    "name": "Valor de Negocio",
                    "description": "Pontuacao de valor para o negocio",
                    "field_type": "select",
                    "options": [
                        {"label": "1 - Minimo", "value": "1", "color": "#6B7280"},
                        {"label": "2 - Baixo", "value": "2", "color": "#22C55E"},
                        {"label": "3 - Medio", "value": "3", "color": "#EAB308"},
                        {"label": "5 - Alto", "value": "5", "color": "#F97316"},
                        {"label": "8 - Muito Alto", "value": "8", "color": "#EF4444"}
                    ],
                    "show_in_card": True
                },
                {
                    "id": "team_tags",
                    "name": "Times Responsaveis",
                    "description": "Times envolvidos na story",
                    "field_type": "multiselect",
                    "options": [
                        {"label": "Backend", "value": "backend", "color": "#3B82F6"},
                        {"label": "Frontend", "value": "frontend", "color": "#10B981"},
                        {"label": "QA", "value": "qa", "color": "#8B5CF6"},
                        {"label": "DevOps", "value": "devops", "color": "#F97316"},
                        {"label": "UX/UI", "value": "ux", "color": "#EC4899"}
                    ],
                    "show_in_card": True
                }
            ]
        }

    @app.post("/api/projects/{project_id}/custom-fields/from-template/{template_id}")
    def create_field_from_template(project_id: str, template_id: str):
        """Cria um campo customizado a partir de um template"""
        templates = get_field_templates()["templates"]
        template = next((t for t in templates if t["id"] == template_id), None)

        if not template:
            raise HTTPException(404, f"Template {template_id} nao encontrado")

        # Convert template to create schema
        options = None
        if template.get("options"):
            options = [FieldOption(**opt) for opt in template["options"]]

        validation = None
        if template.get("validation"):
            validation = FieldValidation(**template["validation"])

        field_data = CustomFieldCreate(
            name=template["name"],
            field_type=FieldType(template["field_type"]),
            description=template.get("description"),
            placeholder=template.get("placeholder"),
            default_value=template.get("default_value"),
            options=options,
            validation=validation,
            show_in_card=template.get("show_in_card", True)
        )

        return create_custom_field(project_id, field_data)

    # -------------------------------------------------------------------------
    # Custom Fields Page
    # -------------------------------------------------------------------------

    @app.get("/custom-fields", response_class="HTMLResponse")
    def custom_fields_page():
        """Pagina de gerenciamento de campos customizados"""
        from fastapi.responses import HTMLResponse
        return HTMLResponse(content=CUSTOM_FIELDS_HTML)

    print("[Dashboard] Custom Fields endpoints loaded (Issue #251)")


# =============================================================================
# VUE.JS COMPONENT HTML
# =============================================================================

CUSTOM_FIELDS_HTML = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Campos Customizados - Plataforma E</title>
    <script src="https://cdn.jsdelivr.net/npm/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <link href="https://cdn.jsdelivr.net/npm/@heroicons/vue@2.0.18/dist/heroicons.min.css" rel="stylesheet">
    <style>
        [v-cloak] { display: none; }
        .fade-enter-active, .fade-leave-active { transition: opacity 0.3s; }
        .fade-enter-from, .fade-leave-to { opacity: 0; }
        .belgo-blue { color: #003B4A; }
        .belgo-orange { color: #FF6C00; }
        .bg-belgo-blue { background-color: #003B4A; }
        .bg-belgo-orange { background-color: #FF6C00; }
    </style>
</head>
<body class="bg-gray-100">
<div id="app" v-cloak>
    <!-- Header -->
    <header class="bg-belgo-blue text-white py-4 px-6 shadow-lg">
        <div class="max-w-7xl mx-auto flex justify-between items-center">
            <h1 class="text-2xl font-bold">Campos Customizados</h1>
            <div class="flex items-center gap-4">
                <select v-model="selectedProject" @change="loadFields" class="bg-white/10 border border-white/20 rounded px-3 py-2">
                    <option value="">Selecione um projeto</option>
                    <option v-for="p in projects" :key="p.project_id" :value="p.project_id">
                        {{ p.name }}
                    </option>
                </select>
                <button @click="showCreateModal = true" :disabled="!selectedProject"
                    class="bg-belgo-orange hover:bg-orange-600 disabled:bg-gray-400 text-white px-4 py-2 rounded font-medium transition">
                    + Novo Campo
                </button>
            </div>
        </div>
    </header>

    <!-- Main Content -->
    <main class="max-w-7xl mx-auto py-6 px-4">
        <!-- Templates Section -->
        <div class="mb-8">
            <h2 class="text-lg font-semibold text-gray-800 mb-4">Templates Disponiveis</h2>
            <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                <div v-for="template in templates" :key="template.id"
                    class="bg-white rounded-lg shadow p-4 hover:shadow-md transition cursor-pointer"
                    @click="createFromTemplate(template.id)">
                    <div class="flex items-center gap-2 mb-2">
                        <span class="px-2 py-1 bg-gray-100 rounded text-xs font-medium text-gray-600">
                            {{ template.field_type }}
                        </span>
                        <span v-if="template.show_in_card" class="px-2 py-1 bg-blue-100 text-blue-700 rounded text-xs">
                            No card
                        </span>
                    </div>
                    <h3 class="font-medium text-gray-900">{{ template.name }}</h3>
                    <p class="text-sm text-gray-500 mt-1">{{ template.description }}</p>
                </div>
            </div>
        </div>

        <!-- Fields List -->
        <div v-if="selectedProject">
            <div class="flex justify-between items-center mb-4">
                <h2 class="text-lg font-semibold text-gray-800">Campos do Projeto</h2>
                <span class="text-sm text-gray-500">{{ fields.length }} campo(s)</span>
            </div>

            <div v-if="fields.length === 0" class="bg-white rounded-lg shadow p-8 text-center">
                <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                        d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"/>
                </svg>
                <h3 class="mt-4 text-lg font-medium text-gray-900">Nenhum campo customizado</h3>
                <p class="mt-2 text-gray-500">Crie campos customizados ou use um template acima.</p>
            </div>

            <div v-else class="bg-white rounded-lg shadow overflow-hidden">
                <table class="min-w-full divide-y divide-gray-200">
                    <thead class="bg-gray-50">
                        <tr>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Nome</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Tipo</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">No Card</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Status</th>
                            <th class="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase">Acoes</th>
                        </tr>
                    </thead>
                    <tbody class="bg-white divide-y divide-gray-200">
                        <tr v-for="field in fields" :key="field.id" class="hover:bg-gray-50">
                            <td class="px-6 py-4">
                                <div class="font-medium text-gray-900">{{ field.name }}</div>
                                <div v-if="field.description" class="text-sm text-gray-500">{{ field.description }}</div>
                            </td>
                            <td class="px-6 py-4">
                                <span :class="getTypeBadgeClass(field.field_type)" class="px-2 py-1 rounded text-xs font-medium">
                                    {{ getTypeLabel(field.field_type) }}
                                </span>
                            </td>
                            <td class="px-6 py-4">
                                <span v-if="field.show_in_card" class="text-green-600">Sim</span>
                                <span v-else class="text-gray-400">Nao</span>
                            </td>
                            <td class="px-6 py-4">
                                <span v-if="field.is_active" class="px-2 py-1 bg-green-100 text-green-800 rounded-full text-xs">Ativo</span>
                                <span v-else class="px-2 py-1 bg-gray-100 text-gray-600 rounded-full text-xs">Inativo</span>
                            </td>
                            <td class="px-6 py-4 text-right space-x-2">
                                <button @click="editField(field)" class="text-blue-600 hover:text-blue-800">Editar</button>
                                <button @click="deleteField(field)" class="text-red-600 hover:text-red-800">Excluir</button>
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>

        <div v-else class="bg-white rounded-lg shadow p-8 text-center">
            <p class="text-gray-500">Selecione um projeto para gerenciar seus campos customizados.</p>
        </div>
    </main>

    <!-- Create/Edit Modal -->
    <div v-if="showCreateModal" class="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
        <div class="bg-white rounded-lg shadow-xl w-full max-w-2xl max-h-[90vh] overflow-y-auto">
            <div class="px-6 py-4 border-b flex justify-between items-center">
                <h2 class="text-xl font-semibold">{{ editingField ? 'Editar Campo' : 'Novo Campo Customizado' }}</h2>
                <button @click="closeModal" class="text-gray-500 hover:text-gray-700">&times;</button>
            </div>
            <form @submit.prevent="saveField" class="p-6 space-y-4">
                <div>
                    <label class="block text-sm font-medium text-gray-700 mb-1">Nome do Campo *</label>
                    <input v-model="formData.name" type="text" required
                        class="w-full border rounded-lg px-3 py-2 focus:ring-2 focus:ring-blue-500 focus:border-blue-500">
                </div>

                <div>
                    <label class="block text-sm font-medium text-gray-700 mb-1">Tipo *</label>
                    <select v-model="formData.field_type" required :disabled="editingField"
                        class="w-full border rounded-lg px-3 py-2 focus:ring-2 focus:ring-blue-500">
                        <option value="text">Texto</option>
                        <option value="number">Numero</option>
                        <option value="date">Data</option>
                        <option value="select">Select (uma opcao)</option>
                        <option value="multiselect">Multiselect (varias opcoes)</option>
                        <option value="checkbox">Checkbox (sim/nao)</option>
                    </select>
                </div>

                <div>
                    <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                    <textarea v-model="formData.description" rows="2"
                        class="w-full border rounded-lg px-3 py-2 focus:ring-2 focus:ring-blue-500"></textarea>
                </div>

                <div v-if="formData.field_type === 'text'" class="grid grid-cols-2 gap-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Placeholder</label>
                        <input v-model="formData.placeholder" type="text"
                            class="w-full border rounded-lg px-3 py-2">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Tamanho maximo</label>
                        <input v-model.number="formData.validation.max_length" type="number" min="0"
                            class="w-full border rounded-lg px-3 py-2">
                    </div>
                </div>

                <div v-if="formData.field_type === 'number'" class="grid grid-cols-2 gap-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Valor minimo</label>
                        <input v-model.number="formData.validation.min_value" type="number"
                            class="w-full border rounded-lg px-3 py-2">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Valor maximo</label>
                        <input v-model.number="formData.validation.max_value" type="number"
                            class="w-full border rounded-lg px-3 py-2">
                    </div>
                </div>

                <!-- Options for select/multiselect -->
                <div v-if="['select', 'multiselect'].includes(formData.field_type)">
                    <label class="block text-sm font-medium text-gray-700 mb-2">Opcoes *</label>
                    <div class="space-y-2">
                        <div v-for="(opt, index) in formData.options" :key="index"
                            class="flex items-center gap-2">
                            <input v-model="opt.label" type="text" placeholder="Label" required
                                class="flex-1 border rounded px-3 py-2">
                            <input v-model="opt.value" type="text" placeholder="Valor" required
                                class="flex-1 border rounded px-3 py-2">
                            <input v-model="opt.color" type="color" class="w-10 h-10 rounded cursor-pointer">
                            <button type="button" @click="removeOption(index)" class="text-red-500 hover:text-red-700">
                                &times;
                            </button>
                        </div>
                    </div>
                    <button type="button" @click="addOption"
                        class="mt-2 text-sm text-blue-600 hover:text-blue-800">
                        + Adicionar opcao
                    </button>
                </div>

                <div class="flex items-center gap-4">
                    <label class="flex items-center gap-2">
                        <input v-model="formData.validation.required" type="checkbox" class="rounded">
                        <span class="text-sm text-gray-700">Campo obrigatorio</span>
                    </label>
                    <label class="flex items-center gap-2">
                        <input v-model="formData.show_in_card" type="checkbox" class="rounded">
                        <span class="text-sm text-gray-700">Exibir no card</span>
                    </label>
                </div>

                <div class="flex justify-end gap-3 pt-4 border-t">
                    <button type="button" @click="closeModal"
                        class="px-4 py-2 border rounded-lg hover:bg-gray-50">
                        Cancelar
                    </button>
                    <button type="submit"
                        class="px-4 py-2 bg-belgo-orange text-white rounded-lg hover:bg-orange-600">
                        {{ editingField ? 'Salvar Alteracoes' : 'Criar Campo' }}
                    </button>
                </div>
            </form>
        </div>
    </div>

    <!-- Toast Notifications -->
    <div class="fixed bottom-4 right-4 space-y-2">
        <transition-group name="fade">
            <div v-for="toast in toasts" :key="toast.id"
                :class="toast.type === 'success' ? 'bg-green-500' : 'bg-red-500'"
                class="text-white px-4 py-3 rounded-lg shadow-lg">
                {{ toast.message }}
            </div>
        </transition-group>
    </div>
</div>

<script>
const { createApp, ref, reactive, onMounted, computed } = Vue;

createApp({
    setup() {
        const selectedProject = ref('');
        const projects = ref([]);
        const fields = ref([]);
        const templates = ref([]);
        const showCreateModal = ref(false);
        const editingField = ref(null);
        const toasts = ref([]);

        const defaultFormData = () => ({
            name: '',
            field_type: 'text',
            description: '',
            placeholder: '',
            default_value: null,
            options: [{ label: '', value: '', color: '#6B7280' }],
            validation: { required: false },
            show_in_card: true
        });

        const formData = reactive(defaultFormData());

        const loadProjects = async () => {
            try {
                const res = await fetch('/api/projects');
                const data = await res.json();
                projects.value = data.projects || [];
            } catch (e) {
                showToast('Erro ao carregar projetos', 'error');
            }
        };

        const loadFields = async () => {
            if (!selectedProject.value) {
                fields.value = [];
                return;
            }
            try {
                const res = await fetch(`/api/projects/${selectedProject.value}/custom-fields`);
                const data = await res.json();
                fields.value = data.fields || [];
            } catch (e) {
                showToast('Erro ao carregar campos', 'error');
            }
        };

        const loadTemplates = async () => {
            try {
                const res = await fetch('/api/custom-fields/templates');
                const data = await res.json();
                templates.value = data.templates || [];
            } catch (e) {
                console.error('Erro ao carregar templates:', e);
            }
        };

        const saveField = async () => {
            if (!selectedProject.value) return;

            const payload = { ...formData };
            if (!['select', 'multiselect'].includes(payload.field_type)) {
                delete payload.options;
            }

            try {
                let res;
                if (editingField.value) {
                    res = await fetch(`/api/custom-fields/${editingField.value.id}`, {
                        method: 'PUT',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(payload)
                    });
                } else {
                    res = await fetch(`/api/projects/${selectedProject.value}/custom-fields`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(payload)
                    });
                }

                const data = await res.json();
                if (data.success) {
                    showToast(data.message, 'success');
                    closeModal();
                    loadFields();
                } else {
                    showToast(data.message || 'Erro ao salvar', 'error');
                }
            } catch (e) {
                showToast('Erro ao salvar campo', 'error');
            }
        };

        const editField = (field) => {
            editingField.value = field;
            Object.assign(formData, {
                name: field.name,
                field_type: field.field_type,
                description: field.description || '',
                placeholder: field.placeholder || '',
                default_value: field.default_value,
                options: field.options || [{ label: '', value: '', color: '#6B7280' }],
                validation: field.validation || { required: false },
                show_in_card: field.show_in_card
            });
            showCreateModal.value = true;
        };

        const deleteField = async (field) => {
            if (!confirm(`Deseja realmente excluir o campo "${field.name}"?`)) return;

            try {
                const res = await fetch(`/api/custom-fields/${field.id}?hard_delete=true`, {
                    method: 'DELETE'
                });
                const data = await res.json();
                showToast(data.message, data.success ? 'success' : 'error');
                if (data.success) loadFields();
            } catch (e) {
                showToast('Erro ao excluir campo', 'error');
            }
        };

        const createFromTemplate = async (templateId) => {
            if (!selectedProject.value) {
                showToast('Selecione um projeto primeiro', 'error');
                return;
            }

            try {
                const res = await fetch(`/api/projects/${selectedProject.value}/custom-fields/from-template/${templateId}`, {
                    method: 'POST'
                });
                const data = await res.json();
                showToast(data.message, data.success ? 'success' : 'error');
                if (data.success) loadFields();
            } catch (e) {
                showToast('Erro ao criar campo', 'error');
            }
        };

        const closeModal = () => {
            showCreateModal.value = false;
            editingField.value = null;
            Object.assign(formData, defaultFormData());
        };

        const addOption = () => {
            formData.options.push({ label: '', value: '', color: '#6B7280' });
        };

        const removeOption = (index) => {
            if (formData.options.length > 1) {
                formData.options.splice(index, 1);
            }
        };

        const getTypeLabel = (type) => {
            const labels = {
                text: 'Texto',
                number: 'Numero',
                date: 'Data',
                select: 'Select',
                multiselect: 'Multiselect',
                checkbox: 'Checkbox'
            };
            return labels[type] || type;
        };

        const getTypeBadgeClass = (type) => {
            const classes = {
                text: 'bg-blue-100 text-blue-800',
                number: 'bg-green-100 text-green-800',
                date: 'bg-purple-100 text-purple-800',
                select: 'bg-yellow-100 text-yellow-800',
                multiselect: 'bg-orange-100 text-orange-800',
                checkbox: 'bg-gray-100 text-gray-800'
            };
            return classes[type] || 'bg-gray-100 text-gray-800';
        };

        const showToast = (message, type = 'success') => {
            const id = Date.now();
            toasts.value.push({ id, message, type });
            setTimeout(() => {
                toasts.value = toasts.value.filter(t => t.id !== id);
            }, 3000);
        };

        onMounted(() => {
            loadProjects();
            loadTemplates();
        });

        return {
            selectedProject,
            projects,
            fields,
            templates,
            showCreateModal,
            editingField,
            formData,
            toasts,
            loadFields,
            saveField,
            editField,
            deleteField,
            createFromTemplate,
            closeModal,
            addOption,
            removeOption,
            getTypeLabel,
            getTypeBadgeClass
        };
    }
}).mount('#app');
</script>
</body>
</html>
"""
