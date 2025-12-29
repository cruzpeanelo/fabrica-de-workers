# -*- coding: utf-8 -*-
"""
Exemplos de Uso do SDK - Fabrica de Agentes
===========================================

Este arquivo contem exemplos de como usar o SDK Python da Fabrica de Agentes.
Execute os exemplos individuais ou use como referencia para sua integracao.

Requisitos:
    - API Key valida (obtenha em http://localhost:9001/settings)
    - Servidor da Fabrica rodando (python factory/dashboard/app_v6_agile.py)
"""

import os
import time


def exemplo_basico():
    """
    Exemplo basico de uso do SDK.

    Demonstra:
    - Inicializacao do cliente
    - Verificacao de saude da API
    - Informacoes da API Key
    """
    from fabrica import FabricaClient

    # Inicializar cliente (API Key via variavel de ambiente)
    # export FABRICA_API_KEY=sk-fab_xxx_yyy
    client = FabricaClient()

    # Ou passar diretamente:
    # client = FabricaClient(api_key='sk-fab_xxx_yyy')

    # Verificar saude da API
    health = client.health_check()
    print(f"Status da API: {health['status']}")

    # Informacoes da API
    info = client.get_api_info()
    print(f"Versao: {info['version']}")

    # Informacoes da sua API Key
    key_info = client.api_keys.get_current()
    print(f"Tier: {key_info.tier}")
    print(f"Scopes: {key_info.scopes}")
    print(f"Rate Limits: {key_info.rate_limits}")


def exemplo_projetos():
    """
    Exemplo de gerenciamento de projetos.

    Demonstra:
    - Listar projetos
    - Criar projeto
    - Buscar projeto por ID
    """
    from fabrica import FabricaClient

    client = FabricaClient()

    # Listar projetos (paginado)
    projetos = client.projects.list(page=1, per_page=10)
    print(f"Total de projetos: {projetos.total}")

    for projeto in projetos:
        print(f"  - {projeto.id}: {projeto.name} [{projeto.status}]")

    # Criar novo projeto
    novo_projeto = client.projects.create(
        name="API de Pagamentos",
        project_type="api-service",
        description="Microservico para processamento de pagamentos com PIX"
    )
    print(f"\nProjeto criado: {novo_projeto.id}")
    print(f"  Nome: {novo_projeto.name}")
    print(f"  Tipo: {novo_projeto.project_type}")
    print(f"  Status: {novo_projeto.status}")

    # Buscar projeto
    projeto = client.projects.get(novo_projeto.id)
    print(f"\nProjeto recuperado: {projeto.name}")


def exemplo_stories():
    """
    Exemplo de gerenciamento de User Stories.

    Demonstra:
    - Criar stories com narrativa Agile
    - Listar stories de um projeto
    - Executar desenvolvimento autonomo
    """
    from fabrica import FabricaClient

    client = FabricaClient()

    # Criar projeto primeiro
    projeto = client.projects.create(
        name="App de Delivery",
        project_type="web-app"
    )

    # Criar story com narrativa completa
    story = client.stories.create(
        project_id=projeto.id,
        title="Rastreamento de Pedido em Tempo Real",
        persona="cliente do delivery",
        action="acompanhar meu pedido em tempo real no mapa",
        benefit="saiba exatamente quando minha comida vai chegar",
        acceptance_criteria=[
            "Mapa exibe localizacao atual do entregador",
            "Atualizacao a cada 10 segundos",
            "Notificacao quando entregador estiver proximo",
            "Tempo estimado de chegada exibido"
        ],
        story_points=8,
        priority="high"
    )

    print(f"Story criada: {story.id}")
    print(f"  Titulo: {story.title}")
    print(f"  Narrativa: {story.narrative}")
    print(f"  Story Points: {story.story_points}")

    # Listar stories do projeto
    stories = client.stories.list(project_id=projeto.id)
    print(f"\nTotal de stories no projeto: {len(stories)}")

    # Criar mais algumas stories
    stories_data = [
        {
            "title": "Login com Telefone",
            "persona": "novo usuario",
            "action": "fazer login usando apenas meu numero de telefone",
            "benefit": "nao precise lembrar de senha",
            "story_points": 5
        },
        {
            "title": "Avaliacao do Pedido",
            "persona": "cliente que recebeu o pedido",
            "action": "avaliar o pedido com estrelas e comentario",
            "benefit": "ajudar outros clientes a escolher restaurantes",
            "story_points": 3
        }
    ]

    for data in stories_data:
        s = client.stories.create(project_id=projeto.id, **data)
        print(f"Story criada: {s.title}")


def exemplo_execucao_autonoma():
    """
    Exemplo de execucao autonoma de desenvolvimento.

    Demonstra:
    - Executar story
    - Monitorar progresso do job
    - Aguardar conclusao
    - Verificar arquivos gerados
    """
    from fabrica import FabricaClient
    from fabrica.exceptions import FabricaError

    client = FabricaClient()

    # Criar projeto e story
    projeto = client.projects.create(
        name="API REST Simples",
        project_type="api-service"
    )

    story = client.stories.create(
        project_id=projeto.id,
        title="Endpoint de Health Check",
        persona="desenvolvedor que integra com a API",
        action="verificar se a API esta funcionando",
        benefit="monitorar a disponibilidade do servico",
        acceptance_criteria=[
            "GET /health retorna status 200",
            "Resposta inclui timestamp",
            "Resposta inclui versao da API"
        ],
        story_points=1
    )

    print(f"Story criada: {story.id}")

    # Executar desenvolvimento (requer tier basic+)
    try:
        job = client.stories.execute(story.id)
        print(f"\nJob iniciado: {job.id}")
        print(f"Status: {job.status}")
        print(f"Passo atual: {job.current_step}")

        # Monitorar progresso com callback
        def progress_callback(job):
            print(f"  Progresso: {job.progress:.0f}% - {job.current_step}")

        # Aguardar conclusao (max 10 minutos)
        job = job.wait_for_completion(
            timeout=600,
            poll_interval=5,
            callback=progress_callback
        )

        print(f"\nJob concluido!")
        print(f"Status final: {job.status}")
        print(f"Arquivos gerados:")
        for f in job.files_created:
            print(f"  - {f}")

        if job.output_path:
            print(f"Saida em: {job.output_path}")

    except FabricaError as e:
        print(f"Erro: {e}")


def exemplo_webhooks():
    """
    Exemplo de configuracao de webhooks.

    Demonstra:
    - Listar webhooks
    - Criar webhook
    - Eventos disponiveis
    - Deletar webhook
    """
    from fabrica import FabricaClient

    client = FabricaClient()

    # Listar webhooks existentes
    webhooks = client.webhooks.list()
    print(f"Webhooks configurados: {len(webhooks)}")

    for wh in webhooks:
        print(f"  - {wh.name}: {wh.url}")
        print(f"    Eventos: {', '.join(wh.events)}")
        print(f"    Status: {wh.status}")

    # Criar novo webhook
    webhook = client.webhooks.create(
        name="Notificacoes do Meu Sistema",
        url="https://meuapp.com/webhooks/fabrica",
        events=[
            "story.completed",
            "job.completed",
            "job.failed"
        ]
    )

    print(f"\nWebhook criado: {webhook.id}")
    print(f"URL: {webhook.url}")
    print(f"Secret: {webhook.secret}")  # Guarde este secret!
    print(f"Eventos: {webhook.events}")

    # Deletar webhook
    # webhook.delete()


def exemplo_tratamento_erros():
    """
    Exemplo de tratamento de erros.

    Demonstra:
    - Tipos de excecoes
    - Tratamento de rate limit
    - Tratamento de erros de autenticacao
    """
    from fabrica import FabricaClient
    from fabrica.exceptions import (
        FabricaError,
        AuthenticationError,
        RateLimitError,
        NotFoundError,
        ValidationError
    )

    try:
        # Tentar com API Key invalida
        client = FabricaClient(api_key="sk-fab_invalido")
        client.projects.list()

    except AuthenticationError as e:
        print(f"Erro de autenticacao: {e.message}")

    # Cliente valido para os proximos testes
    client = FabricaClient()

    # Projeto nao encontrado
    try:
        client.projects.get("PRJ-INEXISTENTE")
    except NotFoundError as e:
        print(f"Nao encontrado: {e.message}")

    # Rate limit (simulado)
    try:
        # Fazer muitas requisicoes rapidamente pode causar rate limit
        for i in range(200):
            client.projects.list()
    except RateLimitError as e:
        print(f"Rate limit: {e.message}")
        print(f"Aguarde {e.retry_after} segundos")

    # Dados invalidos
    try:
        client.projects.create(name="")  # Nome vazio
    except ValidationError as e:
        print(f"Validacao falhou: {e.message}")
        for error in e.errors:
            print(f"  - {error}")


def exemplo_configuracao_avancada():
    """
    Exemplo de configuracao avancada do cliente.

    Demonstra:
    - Timeout customizado
    - URL base customizada
    - Retry automatico
    """
    from fabrica import FabricaClient

    # Configuracao para ambiente de producao
    client = FabricaClient(
        api_key=os.getenv("FABRICA_API_KEY"),
        base_url="https://api.fabricadeagentes.com",  # URL de producao
        timeout=60,  # Timeout maior para operacoes lentas
        max_retries=5,  # Mais retries para rate limit
    )

    # Configuracao para desenvolvimento local
    client_dev = FabricaClient(
        api_key="sk-fab_dev_key",
        base_url="http://localhost:9001",
        timeout=10,
        max_retries=1,
    )


# =============================================================================
# EXEMPLO COMPLETO: FLUXO DE DESENVOLVIMENTO
# =============================================================================

def fluxo_completo():
    """
    Fluxo completo de desenvolvimento de uma feature.

    Este exemplo demonstra o fluxo tipico:
    1. Criar projeto
    2. Definir stories
    3. Executar desenvolvimento
    4. Monitorar progresso
    5. Receber notificacoes via webhook
    """
    from fabrica import FabricaClient

    client = FabricaClient()

    print("=" * 60)
    print("FLUXO COMPLETO DE DESENVOLVIMENTO")
    print("=" * 60)

    # 1. Criar projeto
    print("\n1. Criando projeto...")
    projeto = client.projects.create(
        name="Sistema de Gestao de Estoque",
        project_type="web-app",
        description="Sistema completo para controle de estoque com dashboard"
    )
    print(f"   Projeto: {projeto.id} - {projeto.name}")

    # 2. Definir stories
    print("\n2. Criando stories...")

    stories_definicoes = [
        {
            "title": "Cadastro de Produtos",
            "persona": "gerente de estoque",
            "action": "cadastrar novos produtos no sistema",
            "benefit": "manter o catalogo atualizado",
            "story_points": 5,
            "priority": "high",
            "acceptance_criteria": [
                "Formulario com campos: nome, SKU, categoria, preco",
                "Validacao de SKU unico",
                "Upload de imagem do produto"
            ]
        },
        {
            "title": "Controle de Entrada de Estoque",
            "persona": "operador do estoque",
            "action": "registrar entradas de mercadoria",
            "benefit": "manter o estoque atualizado em tempo real",
            "story_points": 8,
            "priority": "high",
            "acceptance_criteria": [
                "Registro com quantidade e data",
                "Associacao com fornecedor",
                "Atualizacao automatica do saldo"
            ]
        },
        {
            "title": "Dashboard de Metricas",
            "persona": "gerente da loja",
            "action": "visualizar metricas do estoque em dashboard",
            "benefit": "tomar decisoes baseadas em dados",
            "story_points": 13,
            "priority": "medium",
            "acceptance_criteria": [
                "Graficos de entrada/saida por periodo",
                "Top 10 produtos mais vendidos",
                "Alertas de estoque baixo"
            ]
        }
    ]

    stories = []
    for definicao in stories_definicoes:
        story = client.stories.create(project_id=projeto.id, **definicao)
        stories.append(story)
        print(f"   Story: {story.id} - {story.title} ({story.story_points} pts)")

    # 3. Configurar webhook para notificacoes
    print("\n3. Configurando webhook...")
    try:
        webhook = client.webhooks.create(
            name="Notificacoes do Projeto",
            url="https://seu-sistema.com/webhooks",
            events=["story.completed", "job.completed", "job.failed"]
        )
        print(f"   Webhook: {webhook.id}")
        print(f"   URL: {webhook.url}")
        print(f"   Secret (guarde!): {webhook.secret}")
    except Exception as e:
        print(f"   Webhook nao configurado: {e}")

    # 4. Executar desenvolvimento da primeira story
    print("\n4. Iniciando desenvolvimento autonomo...")
    print(f"   Executando: {stories[0].title}")

    try:
        job = client.stories.execute(stories[0].id)
        print(f"   Job: {job.id}")
        print(f"   Status: {job.status}")

        # Monitorar por alguns segundos (em producao, use wait_for_completion)
        for _ in range(3):
            time.sleep(2)
            job.refresh()
            print(f"   Progresso: {job.progress:.0f}% - {job.current_step}")

            if job.is_completed or job.is_failed:
                break

        if job.is_completed:
            print("\n5. Desenvolvimento concluido!")
            print(f"   Arquivos gerados: {len(job.files_created)}")
            for f in job.files_created[:5]:
                print(f"   - {f}")
        elif job.is_failed:
            print(f"\n5. Desenvolvimento falhou: {job.error_message}")
        else:
            print(f"\n5. Desenvolvimento ainda em andamento: {job.status}")

    except Exception as e:
        print(f"   Erro na execucao: {e}")
        print("   (Requer tier 'basic' ou superior)")

    # Resumo
    print("\n" + "=" * 60)
    print("RESUMO")
    print("=" * 60)
    print(f"Projeto: {projeto.id}")
    print(f"Stories criadas: {len(stories)}")
    print(f"Total de story points: {sum(s.story_points for s in stories)}")
    print("=" * 60)


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import sys

    exemplos = {
        "basico": exemplo_basico,
        "projetos": exemplo_projetos,
        "stories": exemplo_stories,
        "execucao": exemplo_execucao_autonoma,
        "webhooks": exemplo_webhooks,
        "erros": exemplo_tratamento_erros,
        "config": exemplo_configuracao_avancada,
        "fluxo": fluxo_completo,
    }

    if len(sys.argv) < 2:
        print("Exemplos disponiveis:")
        for nome, func in exemplos.items():
            print(f"  {nome}: {func.__doc__.split(chr(10))[1].strip()}")
        print("\nUso: python examples.py <nome_do_exemplo>")
        print("Exemplo: python examples.py basico")
    else:
        nome = sys.argv[1]
        if nome in exemplos:
            print(f"\n{'='*60}")
            print(f"Executando exemplo: {nome}")
            print(f"{'='*60}\n")
            exemplos[nome]()
        else:
            print(f"Exemplo '{nome}' nao encontrado.")
            print(f"Exemplos disponiveis: {', '.join(exemplos.keys())}")
