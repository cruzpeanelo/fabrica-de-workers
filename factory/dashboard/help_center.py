# -*- coding: utf-8 -*-
"""
Help Center Module (Issue #273)
===============================
Central de ajuda e documentacao integrada na plataforma.

Funcionalidades:
- Artigos categorizados
- Busca em documentacao
- Videos tutoriais
- FAQ com busca
- Links contextuais por pagina
"""

from fastapi import APIRouter, Query
from fastapi.responses import HTMLResponse
from typing import Optional, List
from datetime import datetime

router = APIRouter(prefix="/api/help", tags=["Help Center"])

# Help articles database
HELP_ARTICLES = [
    {
        "id": "getting-started",
        "title": "Primeiros Passos",
        "category": "inicio",
        "content": """
# Bem-vindo a Plataforma E

A Plataforma E e uma plataforma de desenvolvimento autonomo que combina:

- **Dashboard Agile**: Gestao de User Stories com Kanban
- **Workers Claude**: Processamento autonomo de tarefas
- **App Generator**: Geracao automatica de aplicacoes

## Como comecar

1. Crie seu primeiro projeto no menu lateral
2. Adicione stories no backlog
3. Mova stories para "Ready" para processamento
4. Acompanhe o progresso no dashboard
        """,
        "tags": ["inicio", "tutorial", "basico"],
        "video_url": None,
        "views": 1250
    },
    {
        "id": "creating-stories",
        "title": "Criando User Stories",
        "category": "stories",
        "content": """
# Como Criar User Stories Efetivas

## Formato Padrao

Use o formato: **Como [persona], eu quero [acao] para que [beneficio]**

## Campos Importantes

- **Titulo**: Resumo claro da funcionalidade
- **Persona**: Quem vai usar a funcionalidade
- **Acao**: O que o usuario quer fazer
- **Beneficio**: Por que isso e importante
- **Criterios de Aceite**: Lista de requisitos para considerar pronto
- **Story Points**: Complexidade (Fibonacci: 1, 2, 3, 5, 8, 13, 21)

## Dicas

- Mantenha stories pequenas (max 8 pontos)
- Seja especifico nos criterios de aceite
- Use labels para organizar
        """,
        "tags": ["stories", "agile", "tutorial"],
        "video_url": "https://example.com/videos/stories",
        "views": 980
    },
    {
        "id": "kanban-workflow",
        "title": "Fluxo do Kanban",
        "category": "kanban",
        "content": """
# Entendendo o Fluxo Kanban

## Colunas Padrao

1. **Backlog**: Stories aguardando priorizacao
2. **Ready**: Stories prontas para desenvolvimento
3. **In Progress**: Em desenvolvimento
4. **Review**: Aguardando revisao
5. **Testing**: Em teste
6. **Done**: Concluidas

## Como Usar

- Arraste cards entre colunas
- Use filtros para encontrar stories
- Clique no card para ver detalhes
- Use acoes em lote para mover multiplas stories
        """,
        "tags": ["kanban", "workflow", "tutorial"],
        "video_url": None,
        "views": 750
    },
    {
        "id": "keyboard-shortcuts",
        "title": "Atalhos de Teclado",
        "category": "produtividade",
        "content": """
# Atalhos de Teclado

Aumente sua produtividade com esses atalhos:

## Navegacao
- `Ctrl+K` / `Cmd+K`: Busca global
- `?`: Mostrar todos os atalhos
- `G + H`: Ir para Home
- `G + B`: Ir para Board

## Acoes
- `N`: Nova Story
- `T`: Nova Task
- `E`: Editar item selecionado
- `Del`: Excluir item

## Visualizacao
- `V + K`: Vista Kanban
- `V + C`: Vista Calendario
- `F`: Modo Foco
        """,
        "tags": ["atalhos", "produtividade", "teclado"],
        "video_url": None,
        "views": 620
    },
    {
        "id": "integrations",
        "title": "Integracoes Externas",
        "category": "integracoes",
        "content": """
# Integracoes Disponiveis

## GitHub
Conecte seu repositorio para sincronizar issues e PRs.

## Jira
Importe stories do Jira para a plataforma.

## Slack
Receba notificacoes e crie stories pelo Slack.

## Webhooks
Configure webhooks para integrar com qualquer sistema.

## API
Use nossa API REST para integracoes customizadas.
        """,
        "tags": ["integracoes", "github", "jira", "slack"],
        "video_url": None,
        "views": 450
    }
]

# FAQ database
FAQ_ITEMS = [
    {
        "question": "Como criar minha primeira story?",
        "answer": "Clique no botao 'Nova Story' no canto superior direito ou use o atalho 'N'. Preencha os campos obrigatorios e clique em Salvar.",
        "category": "basico"
    },
    {
        "question": "Como mover stories entre colunas?",
        "answer": "Arraste e solte o card da story para a coluna desejada, ou use o menu de contexto (clique direito) para mover rapidamente.",
        "category": "kanban"
    },
    {
        "question": "O que sao Story Points?",
        "answer": "Story Points sao uma medida de complexidade relativa. Usamos a sequencia Fibonacci (1, 2, 3, 5, 8, 13, 21) para estimar o esforco.",
        "category": "agile"
    },
    {
        "question": "Como exportar meus dados?",
        "answer": "Va em Configuracoes > Exportar e escolha o formato (CSV, Excel ou PDF). Voce pode filtrar por projeto, sprint ou periodo.",
        "category": "dados"
    },
    {
        "question": "Como configurar notificacoes?",
        "answer": "Acesse Configuracoes > Notificacoes para definir quais eventos geram alertas e por qual canal (email, push, Slack).",
        "category": "configuracao"
    },
    {
        "question": "O que e o Modo Foco?",
        "answer": "O Modo Foco esconde distrações e ativa um timer Pomodoro para ajudar na concentração. Acesse pelo icone de olho ou atalho 'F'.",
        "category": "produtividade"
    },
    {
        "question": "Como usar filtros no Kanban?",
        "answer": "Use a barra de filtros acima do board para filtrar por status, prioridade, assignee ou sprint. Filtros podem ser combinados.",
        "category": "kanban"
    },
    {
        "question": "Como funciona o versionamento?",
        "answer": "Cada alteracao em stories e automaticamente versionada. Veja o historico clicando em 'Historico' nos detalhes da story.",
        "category": "dados"
    }
]


@router.get("/articles")
async def list_articles(
    category: Optional[str] = Query(None),
    search: Optional[str] = Query(None),
    limit: int = Query(20)
):
    """Lista artigos de ajuda."""
    articles = HELP_ARTICLES.copy()

    if category:
        articles = [a for a in articles if a["category"] == category]

    if search:
        search_lower = search.lower()
        articles = [a for a in articles
                   if search_lower in a["title"].lower()
                   or search_lower in a["content"].lower()
                   or any(search_lower in tag for tag in a["tags"])]

    return {
        "articles": articles[:limit],
        "total": len(articles),
        "categories": list(set(a["category"] for a in HELP_ARTICLES))
    }


@router.get("/articles/{article_id}")
async def get_article(article_id: str):
    """Retorna um artigo especifico."""
    article = next((a for a in HELP_ARTICLES if a["id"] == article_id), None)
    if not article:
        return {"error": "Artigo nao encontrado"}

    # Increment views
    article["views"] += 1

    # Get related articles
    related = [a for a in HELP_ARTICLES
               if a["id"] != article_id
               and (a["category"] == article["category"]
                    or any(tag in article["tags"] for tag in a["tags"]))][:3]

    return {
        "article": article,
        "related": related
    }


@router.get("/faq")
async def get_faq(
    category: Optional[str] = Query(None),
    search: Optional[str] = Query(None)
):
    """Retorna perguntas frequentes."""
    faqs = FAQ_ITEMS.copy()

    if category:
        faqs = [f for f in faqs if f["category"] == category]

    if search:
        search_lower = search.lower()
        faqs = [f for f in faqs
               if search_lower in f["question"].lower()
               or search_lower in f["answer"].lower()]

    return {
        "faqs": faqs,
        "categories": list(set(f["category"] for f in FAQ_ITEMS))
    }


@router.get("/search")
async def search_help(q: str = Query(..., min_length=2)):
    """Busca em artigos e FAQ."""
    q_lower = q.lower()

    # Search articles
    matching_articles = [
        {"type": "article", "id": a["id"], "title": a["title"], "category": a["category"]}
        for a in HELP_ARTICLES
        if q_lower in a["title"].lower() or q_lower in a["content"].lower()
    ]

    # Search FAQ
    matching_faq = [
        {"type": "faq", "question": f["question"], "answer": f["answer"][:100] + "..."}
        for f in FAQ_ITEMS
        if q_lower in f["question"].lower() or q_lower in f["answer"].lower()
    ]

    return {
        "query": q,
        "results": matching_articles + matching_faq,
        "total": len(matching_articles) + len(matching_faq)
    }


@router.get("/contextual/{page}")
async def get_contextual_help(page: str):
    """Retorna ajuda contextual para uma pagina."""
    context_map = {
        "board": ["kanban-workflow", "keyboard-shortcuts"],
        "stories": ["creating-stories", "kanban-workflow"],
        "settings": ["integrations"],
        "analytics": ["getting-started"],
        "home": ["getting-started"]
    }

    article_ids = context_map.get(page, ["getting-started"])
    articles = [a for a in HELP_ARTICLES if a["id"] in article_ids]

    related_faq = [f for f in FAQ_ITEMS if page in f["category"] or page == "kanban"][:3]

    return {
        "page": page,
        "articles": articles,
        "faq": related_faq
    }


def get_help_center_html():
    """Retorna o HTML da central de ajuda."""
    return '''
    <!-- Help Center (Issue #273) -->
    <div v-if="showHelpCenter"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showHelpCenter = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-4xl mx-4 max-h-[90vh] overflow-hidden flex flex-col">
            <!-- Header -->
            <div class="bg-gradient-to-r from-blue-600 to-indigo-600 px-6 py-4 text-white">
                <div class="flex items-center justify-between">
                    <h2 class="text-xl font-bold">Central de Ajuda</h2>
                    <button @click="showHelpCenter = false" class="text-white/70 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
                <!-- Search -->
                <div class="mt-4">
                    <input type="text"
                           v-model="helpSearchQuery"
                           @input="searchHelp"
                           placeholder="Buscar na documentacao..."
                           class="w-full px-4 py-2 rounded-lg text-gray-800 placeholder-gray-400">
                </div>
            </div>

            <div class="flex-1 overflow-y-auto">
                <!-- Search Results -->
                <div v-if="helpSearchResults.length > 0" class="p-6">
                    <h3 class="font-semibold mb-4">Resultados da Busca</h3>
                    <div class="space-y-3">
                        <div v-for="result in helpSearchResults" :key="result.id || result.question"
                             @click="openHelpArticle(result)"
                             class="p-3 bg-gray-50 hover:bg-gray-100 rounded-lg cursor-pointer">
                            <div class="font-medium">{{ result.title || result.question }}</div>
                            <div class="text-sm text-gray-500">{{ result.category || result.answer }}</div>
                        </div>
                    </div>
                </div>

                <!-- Default View -->
                <div v-else class="p-6">
                    <!-- Categories -->
                    <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
                        <div v-for="cat in helpCategories" :key="cat.id"
                             @click="filterHelpByCategory(cat.id)"
                             class="p-4 bg-gray-50 hover:bg-gray-100 rounded-xl cursor-pointer text-center">
                            <div class="text-2xl mb-2">{{ cat.icon }}</div>
                            <div class="font-medium text-sm">{{ cat.name }}</div>
                        </div>
                    </div>

                    <!-- Popular Articles -->
                    <div class="mb-8">
                        <h3 class="font-semibold mb-4">Artigos Populares</h3>
                        <div class="space-y-2">
                            <div v-for="article in helpArticles.slice(0, 5)" :key="article.id"
                                 @click="openHelpArticle(article)"
                                 class="flex items-center justify-between p-3 hover:bg-gray-50 rounded-lg cursor-pointer">
                                <span class="font-medium">{{ article.title }}</span>
                                <span class="text-sm text-gray-400">{{ article.views }} views</span>
                            </div>
                        </div>
                    </div>

                    <!-- FAQ -->
                    <div>
                        <h3 class="font-semibold mb-4">Perguntas Frequentes</h3>
                        <div class="space-y-3">
                            <div v-for="(faq, idx) in helpFaqs.slice(0, 5)" :key="idx"
                                 class="border border-gray-200 rounded-lg">
                                <button @click="toggleFaq(idx)"
                                        class="w-full px-4 py-3 text-left flex items-center justify-between">
                                    <span class="font-medium">{{ faq.question }}</span>
                                    <svg class="w-5 h-5 transform transition-transform"
                                         :class="{'rotate-180': expandedFaq === idx}"
                                         fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                    </svg>
                                </button>
                                <div v-if="expandedFaq === idx" class="px-4 pb-3 text-gray-600">
                                    {{ faq.answer }}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-3 bg-gray-50 border-t text-center text-sm text-gray-500">
                Nao encontrou o que procurava? <a href="#" class="text-blue-600 hover:underline">Entre em contato</a>
            </div>
        </div>
    </div>
    '''


def register_help_center(app):
    """Registra os endpoints da central de ajuda no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Help Center endpoints loaded: /api/help/*")
