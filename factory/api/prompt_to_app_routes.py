"""
Prompt-to-App API Routes

Endpoints para geracao de UI a partir de prompts em linguagem natural.
Inspirado no Base44.app.
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field
from typing import Dict, List, Optional, Any

from factory.core.ai_design_generator import ai_design_generator, DesignResult
from factory.core.visual_builder import visual_builder


router = APIRouter(prefix="/api/prompt-to-app", tags=["Prompt-to-App"])


# === Request/Response Models ===

class GenerateFromPromptRequest(BaseModel):
    prompt: str = Field(..., min_length=3, max_length=1000, description="Descricao do que criar")
    use_ai: bool = Field(True, description="Usar IA para interpretacao avancada")
    canvas_id: Optional[str] = Field(None, description="Canvas para adicionar componentes (opcional)")


class GenerateFromTemplateRequest(BaseModel):
    template_id: str = Field(..., description="ID do template")
    canvas_id: Optional[str] = Field(None, description="Canvas para adicionar componentes")


class ApplyToCanvasRequest(BaseModel):
    canvas_id: str = Field(..., description="ID do canvas")
    components: List[Dict] = Field(..., description="Componentes a adicionar")
    clear_existing: bool = Field(False, description="Limpar componentes existentes")


# === Endpoints ===

@router.post("/generate", response_model=Dict)
async def generate_from_prompt(request: GenerateFromPromptRequest):
    """
    Gera design a partir de um prompt em linguagem natural.

    Exemplos de prompts:
    - "Crie um formulario de login com email e senha"
    - "Preciso de uma tabela de usuarios"
    - "Faca um dashboard com cards de metricas"
    """
    try:
        result = ai_design_generator.generate_from_prompt(
            request.prompt,
            use_ai=request.use_ai
        )

        response = result.to_dict()

        # Se canvas_id fornecido, adiciona componentes ao canvas
        if request.canvas_id:
            canvas = visual_builder.get_canvas(request.canvas_id)
            if canvas:
                for comp_data in result.components:
                    visual_builder.add_component(
                        request.canvas_id,
                        comp_data.component_id,
                        comp_data.props,
                        comp_data.position
                    )
                response['applied_to_canvas'] = True
                response['canvas_id'] = request.canvas_id
            else:
                response['applied_to_canvas'] = False
                response['canvas_error'] = "Canvas nao encontrado"

        return response

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Erro na geracao: {str(e)}")


@router.get("/templates", response_model=List[Dict])
async def list_templates():
    """Lista templates predefinidos disponiveis"""
    return ai_design_generator.list_templates()


@router.get("/templates/{template_id}", response_model=Dict)
async def get_template(template_id: str):
    """Retorna um template predefinido"""
    result = ai_design_generator.get_template(template_id)
    if not result:
        raise HTTPException(status_code=404, detail=f"Template '{template_id}' nao encontrado")
    return result.to_dict()


@router.post("/templates/{template_id}/apply", response_model=Dict)
async def apply_template(template_id: str, request: GenerateFromTemplateRequest):
    """Aplica um template a um canvas"""
    result = ai_design_generator.get_template(template_id)
    if not result:
        raise HTTPException(status_code=404, detail=f"Template '{template_id}' nao encontrado")

    response = result.to_dict()

    if request.canvas_id:
        canvas = visual_builder.get_canvas(request.canvas_id)
        if canvas:
            for comp_data in result.components:
                visual_builder.add_component(
                    request.canvas_id,
                    comp_data.component_id,
                    comp_data.props,
                    comp_data.position
                )
            response['applied_to_canvas'] = True
            response['canvas_id'] = request.canvas_id
        else:
            raise HTTPException(status_code=404, detail="Canvas nao encontrado")

    return response


@router.post("/apply-to-canvas", response_model=Dict)
async def apply_to_canvas(request: ApplyToCanvasRequest):
    """Aplica componentes gerados a um canvas"""
    canvas = visual_builder.get_canvas(request.canvas_id)
    if not canvas:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")

    # Limpa canvas se solicitado
    if request.clear_existing:
        for comp_id in list(canvas.components.keys()):
            visual_builder.remove_component(request.canvas_id, comp_id)

    # Adiciona novos componentes
    added = []
    for comp_data in request.components:
        comp = visual_builder.add_component(
            request.canvas_id,
            comp_data['component_id'],
            comp_data.get('props', {}),
            comp_data.get('position', {'x': 0, 'y': 0, 'width': 200, 'height': 50})
        )
        if comp:
            added.append(comp.to_dict())

    return {
        'canvas_id': request.canvas_id,
        'components_added': len(added),
        'components': added
    }


@router.post("/suggest", response_model=Dict)
async def suggest_components(prompt: str):
    """
    Sugere componentes baseado em um prompt (sem aplicar).
    Util para preview antes de gerar.
    """
    result = ai_design_generator.generate_from_prompt(prompt, use_ai=False)

    return {
        'intent': result.intent.value,
        'suggestions': [
            {
                'component_id': c.component_id,
                'reason': f"Detectado baseado em keywords no prompt"
            }
            for c in result.components
        ],
        'layout_suggestion': result.layout_suggestion,
        'confidence': result.confidence
    }


@router.get("/intents", response_model=List[Dict])
async def list_intents():
    """Lista intencoes de design suportadas"""
    from factory.core.ai_design_generator import DesignIntent

    return [
        {'id': 'form', 'name': 'Formulario', 'description': 'Formularios de entrada de dados'},
        {'id': 'table', 'name': 'Tabela', 'description': 'Tabelas e listagens de dados'},
        {'id': 'dashboard', 'name': 'Dashboard', 'description': 'Paineis com metricas e graficos'},
        {'id': 'list', 'name': 'Lista', 'description': 'Listas de items'},
        {'id': 'navigation', 'name': 'Navegacao', 'description': 'Menus e barras de navegacao'},
        {'id': 'landing', 'name': 'Landing Page', 'description': 'Paginas de destino'},
        {'id': 'modal', 'name': 'Modal', 'description': 'Janelas modais e dialogos'},
        {'id': 'card', 'name': 'Cards', 'description': 'Cartoes e blocos de conteudo'},
        {'id': 'crud', 'name': 'CRUD', 'description': 'Interfaces de criar/editar/deletar'},
        {'id': 'auth', 'name': 'Autenticacao', 'description': 'Login, registro, senha'},
    ]


@router.get("/examples", response_model=List[Dict])
async def get_prompt_examples():
    """Retorna exemplos de prompts para inspiracao"""
    return [
        {
            'category': 'Formularios',
            'prompts': [
                'Crie um formulario de login com email e senha',
                'Preciso de um formulario de cadastro de usuario',
                'Faca um formulario de contato com nome, email e mensagem',
                'Crie um formulario de checkout com endereco e pagamento',
            ]
        },
        {
            'category': 'Dados',
            'prompts': [
                'Crie uma tabela de usuarios com nome, email e status',
                'Preciso de uma lista de produtos com imagem e preco',
                'Faca um CRUD de clientes',
                'Crie uma listagem de pedidos com filtros',
            ]
        },
        {
            'category': 'Dashboards',
            'prompts': [
                'Crie um dashboard com metricas de vendas',
                'Preciso de um painel com KPIs e graficos',
                'Faca um dashboard de analytics',
                'Crie um painel administrativo',
            ]
        },
        {
            'category': 'Navegacao',
            'prompts': [
                'Crie um menu de navegacao horizontal',
                'Preciso de uma sidebar com items de menu',
                'Faca abas para organizar conteudo',
                'Crie um header com logo e menu',
            ]
        },
        {
            'category': 'Outros',
            'prompts': [
                'Crie uma pagina de perfil de usuario',
                'Preciso de um modal de confirmacao',
                'Faca cards de produtos',
                'Crie uma landing page com hero section',
            ]
        }
    ]
