"""
Script de Populacao de Dados de Teste
Cria 6 projetos (3 nivel baixo + 3 nivel medio) com stories e tasks
"""
import sys
import os
from datetime import datetime, timedelta
import random

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from factory.database.connection import SessionLocal
from factory.database.models import Project, Story, StoryTask, Epic, Sprint

# Configuracao dos projetos de teste
TEST_PROJECTS = [
    # Nivel Baixo
    {
        "project_id": "PROJ-LOW-001",
        "name": "Landing Page Corporativa",
        "description": "Site estatico simples com HTML/CSS/JS",
        "project_type": "web-app",
        "level": "baixo",
        "stories": [
            {"title": "Hero Section com CTA", "points": 2, "persona": "visitante", "action": "ver proposta de valor clara", "benefit": "entender o produto rapidamente"},
            {"title": "Secao de Features", "points": 3, "persona": "visitante", "action": "conhecer as funcionalidades", "benefit": "avaliar se atende minhas necessidades"},
            {"title": "Formulario de Contato", "points": 3, "persona": "lead", "action": "enviar mensagem de contato", "benefit": "receber mais informacoes"},
            {"title": "Footer com Links", "points": 1, "persona": "visitante", "action": "acessar redes sociais e termos", "benefit": "conhecer mais sobre a empresa"},
            {"title": "Responsividade Mobile", "points": 2, "persona": "usuario mobile", "action": "navegar pelo celular", "benefit": "ter boa experiencia em qualquer dispositivo"},
        ]
    },
    {
        "project_id": "PROJ-LOW-002",
        "name": "API REST de Tarefas",
        "description": "CRUD basico de tarefas com autenticacao",
        "project_type": "api-service",
        "level": "baixo",
        "stories": [
            {"title": "Endpoint POST /tasks", "points": 2, "persona": "desenvolvedor", "action": "criar uma nova tarefa", "benefit": "organizar meu trabalho"},
            {"title": "Endpoint GET /tasks", "points": 2, "persona": "desenvolvedor", "action": "listar todas as tarefas", "benefit": "visualizar o que preciso fazer"},
            {"title": "Endpoint GET /tasks/{id}", "points": 1, "persona": "desenvolvedor", "action": "buscar tarefa especifica", "benefit": "ver detalhes de uma tarefa"},
            {"title": "Endpoint PUT /tasks/{id}", "points": 2, "persona": "desenvolvedor", "action": "atualizar uma tarefa", "benefit": "modificar informacoes"},
            {"title": "Endpoint DELETE /tasks/{id}", "points": 1, "persona": "desenvolvedor", "action": "remover uma tarefa", "benefit": "limpar tarefas concluidas"},
            {"title": "Autenticacao JWT", "points": 5, "persona": "desenvolvedor", "action": "autenticar via token", "benefit": "ter acesso seguro"},
            {"title": "Validacao de Inputs", "points": 3, "persona": "desenvolvedor", "action": "receber erros claros", "benefit": "corrigir problemas rapidamente"},
            {"title": "Documentacao Swagger", "points": 2, "persona": "desenvolvedor", "action": "consultar documentacao da API", "benefit": "integrar facilmente"},
        ]
    },
    {
        "project_id": "PROJ-LOW-003",
        "name": "CLI de Backup",
        "description": "Ferramenta de linha de comando para backup de arquivos",
        "project_type": "cli-tool",
        "level": "baixo",
        "stories": [
            {"title": "Comando backup create", "points": 3, "persona": "sysadmin", "action": "criar backup de diretorio", "benefit": "proteger meus arquivos"},
            {"title": "Comando backup restore", "points": 3, "persona": "sysadmin", "action": "restaurar backup", "benefit": "recuperar arquivos perdidos"},
            {"title": "Comando backup list", "points": 2, "persona": "sysadmin", "action": "listar backups existentes", "benefit": "saber quais backups tenho"},
            {"title": "Compressao de Arquivos", "points": 2, "persona": "sysadmin", "action": "comprimir backup", "benefit": "economizar espaco em disco"},
        ]
    },
    # Nivel Medio
    {
        "project_id": "PROJ-MED-001",
        "name": "E-commerce Simplificado",
        "description": "Loja virtual completa com carrinho e checkout",
        "project_type": "web-app",
        "level": "medio",
        "stories": [
            {"title": "Catalogo de Produtos", "points": 5, "persona": "cliente", "action": "visualizar produtos disponiveis", "benefit": "encontrar o que procuro"},
            {"title": "Busca de Produtos", "points": 3, "persona": "cliente", "action": "buscar por nome ou categoria", "benefit": "encontrar produtos rapidamente"},
            {"title": "Pagina de Produto", "points": 5, "persona": "cliente", "action": "ver detalhes do produto", "benefit": "tomar decisao de compra"},
            {"title": "Carrinho de Compras", "points": 8, "persona": "cliente", "action": "adicionar/remover itens", "benefit": "gerenciar minha compra"},
            {"title": "Checkout Simples", "points": 8, "persona": "cliente", "action": "finalizar compra", "benefit": "receber meus produtos"},
            {"title": "Cadastro de Usuario", "points": 5, "persona": "cliente", "action": "criar conta", "benefit": "acompanhar pedidos"},
            {"title": "Login/Logout", "points": 3, "persona": "cliente", "action": "acessar minha conta", "benefit": "ver historico"},
            {"title": "Historico de Pedidos", "points": 5, "persona": "cliente", "action": "ver pedidos anteriores", "benefit": "acompanhar entregas"},
            {"title": "Painel Admin Produtos", "points": 8, "persona": "admin", "action": "gerenciar produtos", "benefit": "manter catalogo atualizado"},
            {"title": "Painel Admin Pedidos", "points": 5, "persona": "admin", "action": "gerenciar pedidos", "benefit": "processar vendas"},
            {"title": "Notificacao de Pedido", "points": 3, "persona": "cliente", "action": "receber confirmacao por email", "benefit": "ter certeza da compra"},
            {"title": "Calculo de Frete", "points": 5, "persona": "cliente", "action": "calcular frete por CEP", "benefit": "saber custo total"},
            {"title": "Cupom de Desconto", "points": 3, "persona": "cliente", "action": "aplicar cupom", "benefit": "economizar na compra"},
            {"title": "Wishlist", "points": 3, "persona": "cliente", "action": "salvar produtos favoritos", "benefit": "comprar depois"},
            {"title": "Avaliacao de Produtos", "points": 5, "persona": "cliente", "action": "avaliar produto comprado", "benefit": "ajudar outros clientes"},
        ]
    },
    {
        "project_id": "PROJ-MED-002",
        "name": "Dashboard Analitico",
        "description": "Painel de metricas e KPIs com graficos interativos",
        "project_type": "web-app",
        "level": "medio",
        "stories": [
            {"title": "Dashboard Overview", "points": 5, "persona": "gerente", "action": "visualizar resumo de metricas", "benefit": "tomar decisoes rapidas"},
            {"title": "Grafico de Vendas", "points": 5, "persona": "gerente", "action": "ver evolucao de vendas", "benefit": "analisar tendencias"},
            {"title": "Grafico de Usuarios", "points": 5, "persona": "gerente", "action": "ver crescimento de usuarios", "benefit": "medir adocao"},
            {"title": "Filtros de Periodo", "points": 3, "persona": "analista", "action": "filtrar por datas", "benefit": "analisar periodos especificos"},
            {"title": "Exportar para CSV", "points": 3, "persona": "analista", "action": "exportar dados", "benefit": "analisar em outras ferramentas"},
            {"title": "Exportar para PDF", "points": 3, "persona": "gerente", "action": "gerar relatorio PDF", "benefit": "compartilhar com diretoria"},
            {"title": "Alerta de KPI", "points": 5, "persona": "gerente", "action": "receber alertas de metas", "benefit": "agir quando necessario"},
            {"title": "Comparativo de Periodos", "points": 5, "persona": "analista", "action": "comparar periodos", "benefit": "identificar variacoes"},
            {"title": "Drill-down de Dados", "points": 8, "persona": "analista", "action": "detalhar metricas", "benefit": "entender causas"},
            {"title": "Dashboard Customizavel", "points": 8, "persona": "usuario", "action": "personalizar widgets", "benefit": "ver o que importa para mim"},
            {"title": "Compartilhar Dashboard", "points": 3, "persona": "gerente", "action": "compartilhar link", "benefit": "mostrar para equipe"},
            {"title": "Cache de Dados", "points": 5, "persona": "usuario", "action": "carregar rapidamente", "benefit": "nao esperar muito"},
        ]
    },
    {
        "project_id": "PROJ-MED-003",
        "name": "SaaS Multi-tenant",
        "description": "Aplicacao SaaS com isolamento de dados por tenant",
        "project_type": "saas-app",
        "level": "medio",
        "stories": [
            {"title": "Registro de Tenant", "points": 5, "persona": "empresa", "action": "criar workspace", "benefit": "usar a plataforma"},
            {"title": "Convite de Usuarios", "points": 5, "persona": "admin", "action": "convidar membros", "benefit": "colaborar com equipe"},
            {"title": "Roles e Permissoes", "points": 8, "persona": "admin", "action": "definir niveis de acesso", "benefit": "controlar o que cada um ve"},
            {"title": "Isolamento de Dados", "points": 8, "persona": "admin", "action": "garantir privacidade", "benefit": "dados seguros"},
            {"title": "Branding Customizado", "points": 5, "persona": "admin", "action": "personalizar cores e logo", "benefit": "ter identidade propria"},
            {"title": "Subdominio Personalizado", "points": 5, "persona": "admin", "action": "usar dominio proprio", "benefit": "parecer profissional"},
            {"title": "Planos e Billing", "points": 8, "persona": "admin", "action": "gerenciar assinatura", "benefit": "controlar custos"},
            {"title": "Upgrade/Downgrade", "points": 5, "persona": "admin", "action": "mudar de plano", "benefit": "escalar conforme necessidade"},
            {"title": "Limites por Plano", "points": 5, "persona": "sistema", "action": "enforcar limites", "benefit": "garantir sustentabilidade"},
            {"title": "Audit Trail", "points": 5, "persona": "compliance", "action": "ver historico de acoes", "benefit": "atender regulamentacoes"},
            {"title": "SSO Integration", "points": 8, "persona": "enterprise", "action": "login via empresa", "benefit": "simplificar acesso"},
            {"title": "API por Tenant", "points": 8, "persona": "desenvolvedor", "action": "acessar API", "benefit": "integrar sistemas"},
            {"title": "Backup por Tenant", "points": 5, "persona": "admin", "action": "exportar dados", "benefit": "ter copia de seguranca"},
            {"title": "Onboarding Wizard", "points": 5, "persona": "usuario novo", "action": "aprender a usar", "benefit": "comecar rapidamente"},
            {"title": "Dashboard do Tenant", "points": 8, "persona": "admin", "action": "ver metricas do workspace", "benefit": "monitorar uso"},
            {"title": "Notificacoes", "points": 3, "persona": "usuario", "action": "receber alertas", "benefit": "ficar atualizado"},
            {"title": "Configuracoes", "points": 3, "persona": "usuario", "action": "personalizar preferencias", "benefit": "ter experiencia personalizada"},
            {"title": "Help Center", "points": 3, "persona": "usuario", "action": "acessar documentacao", "benefit": "resolver duvidas sozinho"},
        ]
    }
]

def create_test_data():
    """Cria todos os projetos de teste com stories e tasks"""
    db = SessionLocal()

    try:
        tenant_id = "TEST-QA-001"  # Tenant de testes
        created_count = {"projects": 0, "stories": 0, "tasks": 0}

        print("=" * 60)
        print("INICIANDO POPULACAO DE DADOS DE TESTE")
        print("=" * 60)

        for proj_data in TEST_PROJECTS:
            print(f"\n[{proj_data['level'].upper()}] Criando projeto: {proj_data['name']}")

            # Verificar se projeto ja existe
            existing = db.query(Project).filter(Project.project_id == proj_data["project_id"]).first()
            if existing:
                print(f"  -> Projeto {proj_data['project_id']} ja existe, pulando...")
                continue

            # Criar projeto
            project = Project(
                project_id=proj_data["project_id"],
                tenant_id=tenant_id,
                name=proj_data["name"],
                description=proj_data["description"],
                project_type=proj_data["project_type"],
                status="PLANNING",
                config={"level": proj_data["level"], "auto_generated": True}
            )
            db.add(project)
            db.flush()
            created_count["projects"] += 1

            # Criar Epic - usar project_id completo para evitar colisao
            epic_id = f"EPIC-{proj_data['project_id']}"
            epic = Epic(
                epic_id=epic_id,
                tenant_id=tenant_id,
                project_id=proj_data["project_id"],
                title=f"Epic Principal - {proj_data['name']}",
                description=f"Agrupamento de todas as features do {proj_data['name']}",
                status="active"
            )
            db.add(epic)

            # Criar Sprint - usar project_id completo
            sprint_id = f"SPR-{proj_data['project_id']}-01"
            sprint = Sprint(
                sprint_id=sprint_id,
                tenant_id=tenant_id,
                project_id=proj_data["project_id"],
                name=f"Sprint 1 - {proj_data['name']}",
                start_date=datetime.now(),
                end_date=datetime.now() + timedelta(days=14),
                capacity=sum(s["points"] for s in proj_data["stories"]),
                status="planned"
            )
            db.add(sprint)
            db.flush()

            # Criar Stories
            for idx, story_data in enumerate(proj_data["stories"], 1):
                story_id = f"STR-{proj_data['project_id']}-{idx:03d}"

                story = Story(
                    story_id=story_id,
                    tenant_id=tenant_id,
                    project_id=proj_data["project_id"],
                    title=story_data["title"],
                    persona=story_data["persona"],
                    action=story_data["action"],
                    benefit=story_data["benefit"],
                    story_points=story_data["points"],
                    status=random.choice(["backlog", "ready", "in_progress"]),
                    priority=random.choice(["low", "medium", "high"]),
                    complexity=random.choice(["low", "medium", "high"]),
                    epic_id=epic_id,
                    sprint_id=sprint_id,
                    acceptance_criteria=[
                        f"Criterio 1 para {story_data['title']}",
                        f"Criterio 2 para {story_data['title']}",
                        f"Criterio 3 para {story_data['title']}"
                    ],
                    definition_of_done=[
                        "Codigo revisado",
                        "Testes unitarios passando",
                        "Documentacao atualizada"
                    ]
                )
                db.add(story)
                db.flush()
                created_count["stories"] += 1

                # Criar 2-3 Tasks por Story
                task_types = ["development", "test", "documentation"]
                num_tasks = random.randint(2, 3)

                for t_idx in range(num_tasks):
                    task_id = f"STSK-{story_id}-{t_idx+1:02d}"
                    task = StoryTask(
                        task_id=task_id,
                        tenant_id=tenant_id,
                        story_id=story_id,
                        title=f"{task_types[t_idx % len(task_types)].title()} - {story_data['title'][:30]}",
                        task_type=task_types[t_idx % len(task_types)],
                        status=random.choice(["pending", "in_progress"]),
                        progress=random.randint(0, 50)
                    )
                    db.add(task)
                    created_count["tasks"] += 1

                print(f"    + Story: {story_id} ({story_data['points']} pts)")

        db.commit()

        print("\n" + "=" * 60)
        print("POPULACAO CONCLUIDA!")
        print("=" * 60)
        print(f"Projetos criados: {created_count['projects']}")
        print(f"Stories criadas: {created_count['stories']}")
        print(f"Tasks criadas: {created_count['tasks']}")
        print("=" * 60)

        return created_count

    except Exception as e:
        db.rollback()
        print(f"ERRO: {e}")
        raise
    finally:
        db.close()


if __name__ == "__main__":
    create_test_data()
