# =============================================================================
# Plataforma E - Makefile
# =============================================================================
# Issue #382: Comandos padronizados para desenvolvimento
#
# Uso: make <target>
#      make help        Mostra todos os comandos disponíveis
# =============================================================================

.PHONY: help dev stop logs shell test lint build push deploy-dev deploy-staging db-migrate db-reset db-shell clean

# Variáveis
COMPOSE_FILE := docker-compose.yml
DOCKER_IMAGE := fabrica-agentes
DOCKER_TAG := latest
DASHBOARD_PORT := 9001

# Detectar comando do compose
COMPOSE_CMD := $(shell docker compose version > /dev/null 2>&1 && echo "docker compose" || echo "docker-compose")

# Cores para output
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m

# =============================================================================
# HELP
# =============================================================================

help: ## Mostra esta ajuda
	@echo ""
	@echo "$(BLUE)╔═══════════════════════════════════════════════════════════════╗$(NC)"
	@echo "$(BLUE)║           FABRICA DE AGENTES - Comandos Make                  ║$(NC)"
	@echo "$(BLUE)╚═══════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo "$(GREEN)Desenvolvimento:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -E "^(dev|stop|logs|shell|restart)" | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(GREEN)Banco de Dados:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -E "^db-" | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(GREEN)Testes e Qualidade:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -E "^(test|lint|coverage)" | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(GREEN)Build e Deploy:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -E "^(build|push|deploy)" | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(GREEN)Limpeza:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -E "^clean" | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""

# =============================================================================
# DESENVOLVIMENTO
# =============================================================================

dev: ## Inicia ambiente de desenvolvimento
	@echo "$(BLUE)[INFO]$(NC) Iniciando ambiente de desenvolvimento..."
	@./scripts/start.sh --no-browser || $(COMPOSE_CMD) up -d
	@echo "$(GREEN)[OK]$(NC) Dashboard: http://localhost:$(DASHBOARD_PORT)"

stop: ## Para todos os serviços
	@echo "$(BLUE)[INFO]$(NC) Parando serviços..."
	@$(COMPOSE_CMD) down
	@echo "$(GREEN)[OK]$(NC) Serviços parados"

restart: ## Reinicia todos os serviços
	@echo "$(BLUE)[INFO]$(NC) Reiniciando serviços..."
	@$(COMPOSE_CMD) restart
	@echo "$(GREEN)[OK]$(NC) Serviços reiniciados"

logs: ## Mostra logs da aplicação (use: make logs service=app)
	@$(COMPOSE_CMD) logs -f $(if $(service),$(service),app)

shell: ## Abre shell no container da app
	@docker exec -it fabrica-app /bin/bash

status: ## Mostra status dos containers
	@$(COMPOSE_CMD) ps

# =============================================================================
# BANCO DE DADOS
# =============================================================================

db-migrate: ## Executa migrations do banco
	@echo "$(BLUE)[INFO]$(NC) Executando migrations..."
	@docker exec -it fabrica-app python -c "from factory.database.connection import init_db; init_db()"
	@echo "$(GREEN)[OK]$(NC) Migrations executadas"

db-reset: ## Reseta banco de dados (PERDE DADOS!)
	@echo "$(YELLOW)[AVISO]$(NC) Isso vai apagar todos os dados!"
	@read -p "Confirma? (y/N) " confirm && [ "$$confirm" = "y" ] || exit 1
	@docker exec -it fabrica-app python -c "from factory.database.connection import reset_db; reset_db()"
	@echo "$(GREEN)[OK]$(NC) Banco resetado"

db-shell: ## Abre psql no banco PostgreSQL
	@docker exec -it fabrica-postgres psql -U fabrica -d fabrica_db

db-seed: ## Popula banco com dados de exemplo
	@echo "$(BLUE)[INFO]$(NC) Populando banco com dados de exemplo..."
	@docker exec -it fabrica-app python scripts/seed_data.py
	@echo "$(GREEN)[OK]$(NC) Dados inseridos"

db-backup: ## Faz backup do banco
	@echo "$(BLUE)[INFO]$(NC) Criando backup..."
	@mkdir -p backups
	@docker exec fabrica-postgres pg_dump -U fabrica fabrica_db > backups/backup_$$(date +%Y%m%d_%H%M%S).sql
	@echo "$(GREEN)[OK]$(NC) Backup salvo em backups/"

# =============================================================================
# TESTES E QUALIDADE
# =============================================================================

test: ## Executa todos os testes
	@echo "$(BLUE)[INFO]$(NC) Executando testes..."
	@python -m pytest tests/ -v --tb=short
	@echo "$(GREEN)[OK]$(NC) Testes concluídos"

test-fast: ## Executa testes rápidos (sem integração)
	@python -m pytest tests/ -v --tb=short -m "not integration"

coverage: ## Executa testes com cobertura
	@echo "$(BLUE)[INFO]$(NC) Executando testes com cobertura..."
	@python -m pytest tests/ --cov=factory --cov-report=term-missing --cov-report=html
	@echo "$(GREEN)[OK]$(NC) Relatório em htmlcov/index.html"

lint: ## Executa linter (ruff)
	@echo "$(BLUE)[INFO]$(NC) Executando linter..."
	@python -m ruff check factory/ --fix
	@echo "$(GREEN)[OK]$(NC) Lint concluído"

lint-check: ## Verifica linting sem corrigir
	@python -m ruff check factory/

format: ## Formata código com ruff
	@python -m ruff format factory/

type-check: ## Verifica tipos com mypy
	@python -m mypy factory/ --ignore-missing-imports

# =============================================================================
# BUILD E DEPLOY
# =============================================================================

build: ## Build da imagem Docker
	@echo "$(BLUE)[INFO]$(NC) Construindo imagem Docker..."
	@docker build -t $(DOCKER_IMAGE):$(DOCKER_TAG) .
	@echo "$(GREEN)[OK]$(NC) Imagem construída: $(DOCKER_IMAGE):$(DOCKER_TAG)"

build-prod: ## Build da imagem de produção
	@echo "$(BLUE)[INFO]$(NC) Construindo imagem de produção..."
	@docker build -f Dockerfile.prod -t $(DOCKER_IMAGE):$(DOCKER_TAG) .
	@echo "$(GREEN)[OK]$(NC) Imagem de produção construída"

push: ## Push da imagem para registry
	@echo "$(BLUE)[INFO]$(NC) Enviando imagem para registry..."
	@docker push $(DOCKER_IMAGE):$(DOCKER_TAG)
	@echo "$(GREEN)[OK]$(NC) Imagem enviada"

deploy-dev: ## Deploy para ambiente dev (Kubernetes)
	@echo "$(BLUE)[INFO]$(NC) Deploying para dev..."
	@kubectl apply -k kubernetes/overlays/development/
	@echo "$(GREEN)[OK]$(NC) Deploy concluído"

deploy-staging: ## Deploy para ambiente staging (Kubernetes)
	@echo "$(BLUE)[INFO]$(NC) Deploying para staging..."
	@kubectl apply -k kubernetes/overlays/staging/
	@echo "$(GREEN)[OK]$(NC) Deploy concluído"

deploy-prod: ## Deploy para produção (requer confirmação)
	@echo "$(YELLOW)[AVISO]$(NC) Deploy para PRODUÇÃO!"
	@read -p "Confirma? (y/N) " confirm && [ "$$confirm" = "y" ] || exit 1
	@kubectl apply -k kubernetes/overlays/production/
	@echo "$(GREEN)[OK]$(NC) Deploy de produção concluído"

# =============================================================================
# TERRAFORM
# =============================================================================

tf-init: ## Inicializa Terraform
	@cd terraform && terraform init

tf-plan: ## Plano de execução Terraform
	@cd terraform && terraform plan -var-file=environments/dev.tfvars

tf-apply: ## Aplica infraestrutura com Terraform
	@cd terraform && terraform apply -var-file=environments/dev.tfvars

# =============================================================================
# LIMPEZA
# =============================================================================

clean: ## Remove arquivos temporários
	@echo "$(BLUE)[INFO]$(NC) Limpando arquivos temporários..."
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".ruff_cache" -exec rm -rf {} + 2>/dev/null || true
	@rm -rf htmlcov/ .coverage 2>/dev/null || true
	@echo "$(GREEN)[OK]$(NC) Limpeza concluída"

clean-docker: ## Remove containers e imagens órfãs
	@echo "$(BLUE)[INFO]$(NC) Limpando Docker..."
	@$(COMPOSE_CMD) down --rmi local -v 2>/dev/null || true
	@docker system prune -f
	@echo "$(GREEN)[OK]$(NC) Docker limpo"

clean-all: clean clean-docker ## Limpeza completa (arquivos + docker)
	@echo "$(GREEN)[OK]$(NC) Limpeza completa concluída"

# =============================================================================
# UTILITÁRIOS
# =============================================================================

env-setup: ## Cria arquivo .env a partir do exemplo
	@if [ ! -f .env ]; then \
		cp .env.example .env; \
		echo "$(GREEN)[OK]$(NC) Arquivo .env criado"; \
	else \
		echo "$(YELLOW)[INFO]$(NC) Arquivo .env já existe"; \
	fi

redis-cli: ## Abre Redis CLI
	@docker exec -it fabrica-redis redis-cli -a fabrica_redis_secret

minio-console: ## Abre console do MinIO no browser
	@echo "$(BLUE)[INFO]$(NC) Abrindo MinIO Console..."
	@python -c "import webbrowser; webbrowser.open('http://localhost:9090')" 2>/dev/null || \
		echo "Acesse: http://localhost:9090"

health: ## Verifica saúde dos serviços
	@echo "$(BLUE)[INFO]$(NC) Verificando saúde dos serviços..."
	@curl -sf http://localhost:$(DASHBOARD_PORT)/health && echo " Dashboard: $(GREEN)OK$(NC)" || echo " Dashboard: $(YELLOW)OFFLINE$(NC)"
	@docker exec fabrica-postgres pg_isready -U fabrica -d fabrica_db > /dev/null 2>&1 && echo " PostgreSQL: $(GREEN)OK$(NC)" || echo " PostgreSQL: $(YELLOW)OFFLINE$(NC)"
	@docker exec fabrica-redis redis-cli -a fabrica_redis_secret ping > /dev/null 2>&1 && echo " Redis: $(GREEN)OK$(NC)" || echo " Redis: $(YELLOW)OFFLINE$(NC)"

install: ## Instala dependências Python
	@pip install -r requirements.txt
	@pip install -r requirements-dev.txt 2>/dev/null || true
	@echo "$(GREEN)[OK]$(NC) Dependências instaladas"
