# -*- coding: utf-8 -*-
"""
Test Data Factory - Fabrica de dados de teste.

Gera dados realistas para testes automatizados:
- Usuarios por persona
- Projetos com diferentes configuracoes
- Stories com criterios de aceite
- Tenants com configuracoes white-label
- Dados para integracao corporativa
"""

import random
import string
import uuid
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from pathlib import Path
import json

# Dados realistas para geracao
COMPANY_NAMES = [
    "Belgo Arames", "ArcelorMittal", "Gerdau", "Usiminas", "CSN",
    "Vale", "Petrobras", "Embraer", "WEG", "Natura",
    "Ambev", "JBS", "BRF", "Suzano", "Klabin"
]

DEPARTMENT_NAMES = [
    "TI", "RH", "Financeiro", "Comercial", "Producao",
    "Logistica", "Qualidade", "Manutencao", "P&D", "Marketing"
]

PROJECT_TYPES = [
    "web_app", "api_rest", "mobile_app", "data_pipeline",
    "integration", "automation", "analytics", "crm", "erp_module"
]

TECH_STACKS = {
    "web_app": ["React", "Vue", "Angular", "Next.js", "FastAPI"],
    "api_rest": ["FastAPI", "Flask", "Express", "Spring Boot", "NestJS"],
    "mobile_app": ["React Native", "Flutter", "Swift", "Kotlin"],
    "data_pipeline": ["Python", "Spark", "Airflow", "dbt", "Kafka"],
    "integration": ["Python", "Node.js", "MuleSoft", "Dell Boomi"],
    "analytics": ["Python", "Power BI", "Tableau", "Metabase"],
}

PERSONAS_DATA = {
    "admin": {"name": "Admin User", "role": "ADMIN", "department": "TI"},
    "developer": {"name": "Dev User", "role": "DEVELOPER", "department": "TI"},
    "project_manager": {"name": "PM User", "role": "PROJECT_MANAGER", "department": "TI"},
    "tech_lead": {"name": "Tech Lead", "role": "TECH_LEAD", "department": "TI"},
    "qa_engineer": {"name": "QA User", "role": "QA_ENGINEER", "department": "Qualidade"},
    "stakeholder": {"name": "Stakeholder User", "role": "STAKEHOLDER", "department": "Comercial"},
    "viewer": {"name": "Viewer User", "role": "VIEWER", "department": "RH"},
}

STORY_TEMPLATES = [
    {
        "persona": "usuario",
        "action": "fazer login com email e senha",
        "benefit": "acessar o sistema de forma segura"
    },
    {
        "persona": "administrador",
        "action": "gerenciar usuarios do sistema",
        "benefit": "controlar quem tem acesso"
    },
    {
        "persona": "gerente de projeto",
        "action": "visualizar o progresso das tarefas",
        "benefit": "tomar decisoes informadas"
    },
    {
        "persona": "desenvolvedor",
        "action": "criar novas funcionalidades",
        "benefit": "entregar valor ao cliente"
    },
    {
        "persona": "analista de qualidade",
        "action": "executar testes automatizados",
        "benefit": "garantir a qualidade do software"
    },
]

ACCEPTANCE_CRITERIA_TEMPLATES = [
    "Campo {field} deve aceitar no maximo {max} caracteres",
    "Sistema deve validar formato de email",
    "Mensagem de erro deve ser exibida quando {condition}",
    "Usuario deve ser redirecionado para {page} apos {action}",
    "Dados devem ser salvos no banco de dados",
    "Log de auditoria deve ser registrado",
    "Notificacao deve ser enviada ao usuario",
]

INTEGRATION_SCENARIOS = {
    "sap": {
        "name": "SAP S/4HANA",
        "type": "erp",
        "endpoints": ["/api/sap/materials", "/api/sap/orders", "/api/sap/invoices"],
        "auth": "oauth2"
    },
    "salesforce": {
        "name": "Salesforce",
        "type": "crm",
        "endpoints": ["/api/sf/accounts", "/api/sf/opportunities", "/api/sf/contacts"],
        "auth": "oauth2"
    },
    "jira": {
        "name": "Jira",
        "type": "project_management",
        "endpoints": ["/api/jira/issues", "/api/jira/projects", "/api/jira/boards"],
        "auth": "api_key"
    },
    "azure_devops": {
        "name": "Azure DevOps",
        "type": "devops",
        "endpoints": ["/api/azdo/workitems", "/api/azdo/pipelines", "/api/azdo/repos"],
        "auth": "pat"
    },
    "teams": {
        "name": "Microsoft Teams",
        "type": "collaboration",
        "endpoints": ["/api/teams/messages", "/api/teams/channels", "/api/teams/files"],
        "auth": "oauth2"
    },
    "outlook": {
        "name": "Outlook/Exchange",
        "type": "email",
        "endpoints": ["/api/outlook/messages", "/api/outlook/calendar", "/api/outlook/contacts"],
        "auth": "oauth2"
    },
}


@dataclass
class TestScenario:
    """Cenario de teste pre-definido."""
    name: str
    description: str
    users_count: int = 1
    projects_count: int = 1
    stories_per_project: int = 0
    tenants_count: int = 1
    with_integrations: List[str] = field(default_factory=list)
    with_whitelabel: bool = False


# Cenarios pre-definidos
SCENARIOS = {
    "empty": TestScenario(
        name="empty",
        description="Banco limpo - apenas estrutura basica",
        users_count=1,
        projects_count=0,
        stories_per_project=0,
        tenants_count=1
    ),
    "minimal": TestScenario(
        name="minimal",
        description="1 projeto, 1 story, 1 user",
        users_count=1,
        projects_count=1,
        stories_per_project=1,
        tenants_count=1
    ),
    "standard": TestScenario(
        name="standard",
        description="3 projetos, 10 stories, 5 users",
        users_count=5,
        projects_count=3,
        stories_per_project=10,
        tenants_count=1
    ),
    "enterprise": TestScenario(
        name="enterprise",
        description="Multi-tenant, white-label, 100+ stories",
        users_count=20,
        projects_count=5,
        stories_per_project=25,
        tenants_count=3,
        with_integrations=["sap", "salesforce", "jira"],
        with_whitelabel=True
    ),
    "regression": TestScenario(
        name="regression",
        description="Dados especificos para testes regressivos",
        users_count=10,
        projects_count=5,
        stories_per_project=15,
        tenants_count=2,
        with_integrations=["sap", "jira", "teams"],
        with_whitelabel=True
    ),
    "integration_test": TestScenario(
        name="integration_test",
        description="Foco em integracao corporativa",
        users_count=5,
        projects_count=2,
        stories_per_project=5,
        tenants_count=1,
        with_integrations=["sap", "salesforce", "jira", "azure_devops", "teams", "outlook"]
    ),
}


class TestDataFactory:
    """Fabrica de dados de teste realistas."""

    def __init__(self, base_path: Optional[str] = None):
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self._id_counter = {}

    def _generate_id(self, prefix: str) -> str:
        """Gera ID unico com prefixo."""
        if prefix not in self._id_counter:
            self._id_counter[prefix] = 0
        self._id_counter[prefix] += 1
        return f"{prefix}-{self._id_counter[prefix]:04d}"

    def _random_string(self, length: int = 8) -> str:
        """Gera string aleatoria."""
        return ''.join(random.choices(string.ascii_lowercase + string.digits, k=length))

    def create_user(self, persona: str = "developer", tenant_id: str = None) -> dict:
        """
        Cria um usuario de teste por persona.

        Args:
            persona: Tipo de persona (admin, developer, etc)
            tenant_id: ID do tenant

        Returns:
            Dict com dados do usuario
        """
        persona_data = PERSONAS_DATA.get(persona, PERSONAS_DATA["viewer"])
        user_id = self._generate_id("USR")

        return {
            "user_id": user_id,
            "username": f"{persona}_{self._random_string(4)}",
            "email": f"{persona}_{self._random_string(4)}@teste.com",
            "password_hash": "hashed_test_password_123",
            "role": persona_data["role"],
            "name": persona_data["name"],
            "department": persona_data["department"],
            "tenant_id": tenant_id,
            "active": True,
            "force_password_change": False,
            "quotas": {
                "max_jobs_per_day": 100,
                "max_concurrent_jobs": 10,
                "max_projects": 50,
                "api_tier": "enterprise"
            },
            "created_at": datetime.utcnow().isoformat()
        }

    def create_project(
        self,
        tenant_id: str = None,
        project_type: str = None,
        with_stories: int = 0
    ) -> dict:
        """
        Cria um projeto de teste.

        Args:
            tenant_id: ID do tenant
            project_type: Tipo do projeto
            with_stories: Quantidade de stories a criar

        Returns:
            Dict com dados do projeto e stories
        """
        project_type = project_type or random.choice(PROJECT_TYPES)
        company = random.choice(COMPANY_NAMES)
        department = random.choice(DEPARTMENT_NAMES)

        project_id = self._generate_id("PRJ")
        tech_stack = random.choice(TECH_STACKS.get(project_type, ["Python"]))

        project = {
            "project_id": project_id,
            "name": f"Sistema {department} - {company}",
            "description": f"Projeto de {project_type} para o departamento de {department}",
            "project_type": project_type,
            "tenant_id": tenant_id,
            "status": "IN_PROGRESS",
            "progress": random.uniform(0, 100),
            "tech_stack": tech_stack,
            "config": {
                "language": "pt-BR",
                "timezone": "America/Sao_Paulo",
                "features": ["auth", "api", "dashboard"]
            },
            "created_at": datetime.utcnow().isoformat(),
            "created_by": "test_factory"
        }

        stories = []
        if with_stories > 0:
            for _ in range(with_stories):
                story = self.create_story(project_id=project_id, tenant_id=tenant_id)
                stories.append(story)

        return {
            "project": project,
            "stories": stories
        }

    def create_story(
        self,
        project_id: str = None,
        tenant_id: str = None,
        status: str = "backlog"
    ) -> dict:
        """
        Cria uma story de teste.

        Args:
            project_id: ID do projeto
            tenant_id: ID do tenant
            status: Status da story

        Returns:
            Dict com dados da story
        """
        template = random.choice(STORY_TEMPLATES)
        story_id = self._generate_id("STR")

        # Gerar criterios de aceite
        criteria_count = random.randint(2, 5)
        acceptance_criteria = []
        for _ in range(criteria_count):
            template_criteria = random.choice(ACCEPTANCE_CRITERIA_TEMPLATES)
            criteria = template_criteria.format(
                field=random.choice(["nome", "email", "telefone", "cpf"]),
                max=random.choice([50, 100, 200, 255]),
                condition=random.choice(["campo vazio", "valor invalido", "erro de servidor"]),
                page=random.choice(["dashboard", "perfil", "home"]),
                action=random.choice(["login", "cadastro", "atualizacao"])
            )
            acceptance_criteria.append({"description": criteria, "completed": False})

        return {
            "story_id": story_id,
            "project_id": project_id or self._generate_id("PRJ"),
            "tenant_id": tenant_id,
            "title": f"Como {template['persona']}, quero {template['action']}",
            "persona": f"Como um(a) {template['persona']}",
            "action": f"Eu quero {template['action']}",
            "benefit": f"Para que eu possa {template['benefit']}",
            "acceptance_criteria": acceptance_criteria,
            "definition_of_done": [
                {"description": "Codigo implementado e revisado", "completed": False},
                {"description": "Testes unitarios passando", "completed": False},
                {"description": "Documentacao atualizada", "completed": False},
            ],
            "story_points": random.choice([1, 2, 3, 5, 8, 13]),
            "complexity": random.choice(["low", "medium", "high"]),
            "status": status,
            "priority": random.choice(["low", "medium", "high", "urgent"]),
            "category": random.choice(["feature", "bug", "tech_debt", "improvement"]),
            "created_at": datetime.utcnow().isoformat(),
            "created_by": "test_factory"
        }

    def create_tenant(
        self,
        branding: dict = None,
        with_users: int = 0,
        with_projects: int = 0
    ) -> dict:
        """
        Cria um tenant de teste.

        Args:
            branding: Configuracoes de white-label
            with_users: Quantidade de usuarios a criar
            with_projects: Quantidade de projetos a criar

        Returns:
            Dict com dados do tenant
        """
        company = random.choice(COMPANY_NAMES)
        tenant_id = self._generate_id("TNT")

        default_branding = {
            "logo_url": f"/static/logos/{tenant_id}.png",
            "primary_color": "#003B4A",
            "secondary_color": "#FF6C00",
            "company_name": company,
            "custom_domain": f"{company.lower().replace(' ', '')}.fabrica.com.br",
            "favicon_url": f"/static/favicons/{tenant_id}.ico",
            "email_template": "corporate",
            "pdf_template": "corporate_branded"
        }

        tenant = {
            "tenant_id": tenant_id,
            "name": company,
            "slug": company.lower().replace(" ", "-"),
            "domain": f"{company.lower().replace(' ', '')}.com.br",
            "settings": {
                "language": "pt-BR",
                "timezone": "America/Sao_Paulo",
                "date_format": "DD/MM/YYYY",
                "currency": "BRL"
            },
            "branding": branding or default_branding,
            "features": {
                "white_label": branding is not None,
                "multi_project": True,
                "advanced_reports": True,
                "integrations": True
            },
            "quotas": {
                "max_users": 100,
                "max_projects": 50,
                "max_storage_gb": 100,
                "api_rate_limit": 10000
            },
            "active": True,
            "created_at": datetime.utcnow().isoformat()
        }

        users = []
        if with_users > 0:
            # Criar pelo menos um admin
            users.append(self.create_user("admin", tenant_id))
            for i in range(with_users - 1):
                persona = random.choice(list(PERSONAS_DATA.keys()))
                users.append(self.create_user(persona, tenant_id))

        projects = []
        if with_projects > 0:
            for _ in range(with_projects):
                project_data = self.create_project(tenant_id=tenant_id)
                projects.append(project_data["project"])

        return {
            "tenant": tenant,
            "users": users,
            "projects": projects
        }

    def create_integration_config(self, integration_type: str) -> dict:
        """
        Cria configuracao de integracao corporativa.

        Args:
            integration_type: Tipo de integracao (sap, salesforce, jira, etc)

        Returns:
            Dict com configuracao da integracao
        """
        config = INTEGRATION_SCENARIOS.get(integration_type)
        if not config:
            return {}

        return {
            "integration_id": self._generate_id("INT"),
            "type": integration_type,
            "name": config["name"],
            "category": config["type"],
            "endpoints": config["endpoints"],
            "auth_type": config["auth"],
            "credentials": {
                "client_id": f"test_client_{self._random_string(8)}",
                "client_secret": f"test_secret_{self._random_string(16)}",
                "token_url": f"https://{integration_type}.example.com/oauth/token"
            },
            "settings": {
                "sync_interval_minutes": 30,
                "retry_count": 3,
                "timeout_seconds": 30
            },
            "enabled": True,
            "last_sync": datetime.utcnow().isoformat()
        }

    def create_document(
        self,
        doc_type: str = "office",
        file_format: str = None
    ) -> dict:
        """
        Cria documento de teste para analise.

        Args:
            doc_type: Tipo do documento (office, pdf, teams, outlook, whatsapp)
            file_format: Formato especifico

        Returns:
            Dict com dados do documento
        """
        formats = {
            "office": ["docx", "xlsx", "pptx"],
            "pdf": ["pdf"],
            "teams": ["teams_chat", "teams_file"],
            "outlook": ["msg", "eml"],
            "whatsapp": ["txt", "json"]
        }

        fmt = file_format or random.choice(formats.get(doc_type, ["txt"]))
        doc_id = self._generate_id("DOC")

        return {
            "document_id": doc_id,
            "type": doc_type,
            "format": fmt,
            "filename": f"documento_teste_{doc_id}.{fmt}",
            "size_bytes": random.randint(1024, 10485760),
            "content_preview": "Conteudo de teste para analise...",
            "metadata": {
                "author": "Test Factory",
                "created": datetime.utcnow().isoformat(),
                "modified": datetime.utcnow().isoformat(),
                "pages": random.randint(1, 50) if fmt == "pdf" else None,
                "sheets": random.randint(1, 10) if fmt == "xlsx" else None
            },
            "analysis_result": None,
            "uploaded_at": datetime.utcnow().isoformat()
        }

    def bulk_create(self, model: str, count: int, **kwargs) -> List[dict]:
        """
        Cria multiplos registros de um modelo.

        Args:
            model: Nome do modelo (user, project, story, tenant)
            count: Quantidade a criar
            **kwargs: Argumentos extras para o metodo de criacao

        Returns:
            Lista de dicts criados
        """
        creators = {
            "user": self.create_user,
            "project": lambda **kw: self.create_project(**kw)["project"],
            "story": self.create_story,
            "tenant": lambda **kw: self.create_tenant(**kw)["tenant"],
            "integration": self.create_integration_config,
            "document": self.create_document
        }

        creator = creators.get(model)
        if not creator:
            raise ValueError(f"Modelo desconhecido: {model}")

        return [creator(**kwargs) for _ in range(count)]

    def seed_database(self, scenario: str = "standard") -> dict:
        """
        Popula banco com cenario pre-definido.

        Args:
            scenario: Nome do cenario (empty, minimal, standard, enterprise, regression)

        Returns:
            Dict com todos os dados criados
        """
        config = SCENARIOS.get(scenario)
        if not config:
            raise ValueError(f"Cenario desconhecido: {scenario}")

        result = {
            "scenario": config.name,
            "description": config.description,
            "tenants": [],
            "users": [],
            "projects": [],
            "stories": [],
            "integrations": []
        }

        # Criar tenants
        for _ in range(config.tenants_count):
            tenant_data = self.create_tenant(
                branding={"enabled": True} if config.with_whitelabel else None,
                with_users=config.users_count // config.tenants_count,
                with_projects=config.projects_count // config.tenants_count
            )

            result["tenants"].append(tenant_data["tenant"])
            result["users"].extend(tenant_data["users"])
            result["projects"].extend(tenant_data["projects"])

            # Criar stories para cada projeto
            for project in tenant_data["projects"]:
                for _ in range(config.stories_per_project):
                    story = self.create_story(
                        project_id=project["project_id"],
                        tenant_id=tenant_data["tenant"]["tenant_id"]
                    )
                    result["stories"].append(story)

        # Criar integracoes
        for integration_type in config.with_integrations:
            integration = self.create_integration_config(integration_type)
            result["integrations"].append(integration)

        return result

    def export_to_json(self, data: dict, filename: str) -> Path:
        """
        Exporta dados para arquivo JSON.

        Args:
            data: Dados a exportar
            filename: Nome do arquivo

        Returns:
            Path do arquivo criado
        """
        output_path = self.base_path / "factory" / "testing" / "fixtures" / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)

        return output_path


# Instancia global
_data_factory: Optional[TestDataFactory] = None


def get_data_factory(base_path: Optional[str] = None) -> TestDataFactory:
    """Retorna instancia global do TestDataFactory."""
    global _data_factory
    if _data_factory is None:
        _data_factory = TestDataFactory(base_path)
    return _data_factory


if __name__ == "__main__":
    # Demo: Criar dados de teste
    factory = TestDataFactory()

    print("\n=== DEMO: Test Data Factory ===\n")

    # Criar cenario enterprise
    data = factory.seed_database("enterprise")

    print(f"Cenario: {data['scenario']}")
    print(f"Descricao: {data['description']}")
    print(f"Tenants criados: {len(data['tenants'])}")
    print(f"Usuarios criados: {len(data['users'])}")
    print(f"Projetos criados: {len(data['projects'])}")
    print(f"Stories criadas: {len(data['stories'])}")
    print(f"Integracoes: {len(data['integrations'])}")

    # Mostrar exemplo
    if data['tenants']:
        print(f"\nTenant exemplo: {data['tenants'][0]['name']}")
    if data['users']:
        print(f"Usuario exemplo: {data['users'][0]['username']} ({data['users'][0]['role']})")
    if data['stories']:
        print(f"Story exemplo: {data['stories'][0]['title'][:50]}...")
