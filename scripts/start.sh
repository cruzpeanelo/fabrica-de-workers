#!/bin/bash
# =============================================================================
# Fabrica de Agentes - Script de Inicializacao Local
# =============================================================================
# Issue #375: Script para iniciar ambiente de desenvolvimento com 1 comando
#
# Uso: ./scripts/start.sh [opcoes]
#   --no-browser    Nao abrir browser automaticamente
#   --reset         Resetar volumes antes de iniciar
#   --build         Forcar rebuild das imagens
#   -h, --help      Mostrar ajuda
# =============================================================================

set -e

# Cores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuracoes
COMPOSE_FILE="docker-compose.yml"
DASHBOARD_PORT=9001
DASHBOARD_URL="http://localhost:${DASHBOARD_PORT}"
MAX_WAIT_SECONDS=120

# Flags
OPEN_BROWSER=true
RESET_VOLUMES=false
FORCE_BUILD=false

# =============================================================================
# Funcoes
# =============================================================================

print_banner() {
    echo -e "${BLUE}"
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║           FABRICA DE AGENTES - Ambiente Local                 ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

print_step() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[!]${NC} $1"
}

print_error() {
    echo -e "${RED}[✗]${NC} $1"
}

show_help() {
    echo "Uso: $0 [opcoes]"
    echo ""
    echo "Opcoes:"
    echo "  --no-browser    Nao abrir browser automaticamente"
    echo "  --reset         Resetar volumes antes de iniciar (PERDE DADOS!)"
    echo "  --build         Forcar rebuild das imagens"
    echo "  -h, --help      Mostrar esta ajuda"
    echo ""
    echo "Exemplos:"
    echo "  $0              Inicia ambiente normalmente"
    echo "  $0 --build      Inicia com rebuild das imagens"
    echo "  $0 --reset      Reseta tudo e inicia do zero"
}

check_prerequisites() {
    print_step "Verificando pre-requisitos..."

    # Docker
    if ! command -v docker &> /dev/null; then
        print_error "Docker nao encontrado. Instale em: https://docs.docker.com/get-docker/"
        exit 1
    fi
    print_success "Docker instalado: $(docker --version | head -1)"

    # Docker Compose
    if ! command -v docker-compose &> /dev/null && ! docker compose version &> /dev/null; then
        print_error "Docker Compose nao encontrado."
        exit 1
    fi

    if docker compose version &> /dev/null; then
        COMPOSE_CMD="docker compose"
    else
        COMPOSE_CMD="docker-compose"
    fi
    print_success "Docker Compose disponivel"

    # Docker daemon
    if ! docker info &> /dev/null; then
        print_error "Docker daemon nao esta rodando. Inicie o Docker Desktop."
        exit 1
    fi
    print_success "Docker daemon rodando"
}

setup_env() {
    print_step "Configurando variaveis de ambiente..."

    if [ ! -f ".env" ]; then
        if [ -f ".env.example" ]; then
            cp .env.example .env
            print_success "Arquivo .env criado a partir de .env.example"
            print_warning "Edite .env para configurar sua ANTHROPIC_API_KEY"
        else
            print_warning "Arquivo .env.example nao encontrado, usando defaults"
        fi
    else
        print_success "Arquivo .env encontrado"
    fi
}

reset_volumes() {
    print_step "Resetando volumes (isso vai apagar todos os dados!)..."

    read -p "Tem certeza que deseja apagar todos os dados? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        $COMPOSE_CMD down -v 2>/dev/null || true
        print_success "Volumes removidos"
    else
        print_warning "Reset cancelado"
        RESET_VOLUMES=false
    fi
}

start_services() {
    print_step "Iniciando servicos..."

    BUILD_FLAG=""
    if [ "$FORCE_BUILD" = true ]; then
        BUILD_FLAG="--build"
    fi

    $COMPOSE_CMD up -d $BUILD_FLAG
    print_success "Containers iniciados"
}

wait_for_health() {
    print_step "Aguardando servicos ficarem saudaveis..."

    local elapsed=0
    local interval=5

    while [ $elapsed -lt $MAX_WAIT_SECONDS ]; do
        # Verificar PostgreSQL
        if docker exec fabrica-postgres pg_isready -U fabrica -d fabrica_db &> /dev/null; then
            print_success "PostgreSQL pronto"
            break
        fi

        echo -n "."
        sleep $interval
        elapsed=$((elapsed + interval))
    done

    if [ $elapsed -ge $MAX_WAIT_SECONDS ]; then
        print_warning "Timeout aguardando PostgreSQL, continuando..."
    fi

    # Aguardar Dashboard
    elapsed=0
    print_step "Aguardando Dashboard..."

    while [ $elapsed -lt $MAX_WAIT_SECONDS ]; do
        if curl -sf "${DASHBOARD_URL}/health" &> /dev/null; then
            print_success "Dashboard pronto!"
            return 0
        fi

        echo -n "."
        sleep $interval
        elapsed=$((elapsed + interval))
    done

    print_error "Timeout aguardando Dashboard"
    return 1
}

show_status() {
    echo ""
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}                   AMBIENTE INICIADO COM SUCESSO               ${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "  Dashboard:      ${BLUE}${DASHBOARD_URL}${NC}"
    echo -e "  Swagger API:    ${BLUE}${DASHBOARD_URL}/docs${NC}"
    echo -e "  PostgreSQL:     ${BLUE}localhost:5433${NC}"
    echo -e "  Redis:          ${BLUE}localhost:6379${NC}"
    echo -e "  MinIO Console:  ${BLUE}http://localhost:9090${NC}"
    echo ""
    echo -e "  Comandos uteis:"
    echo -e "    ${YELLOW}docker-compose logs -f app${NC}     Ver logs"
    echo -e "    ${YELLOW}./scripts/stop.sh${NC}              Parar servicos"
    echo -e "    ${YELLOW}docker-compose ps${NC}              Ver status"
    echo ""
}

open_browser() {
    if [ "$OPEN_BROWSER" = true ]; then
        print_step "Abrindo browser..."

        # Detectar SO e abrir browser
        if [[ "$OSTYPE" == "darwin"* ]]; then
            open "$DASHBOARD_URL"
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            xdg-open "$DASHBOARD_URL" 2>/dev/null || print_warning "Nao foi possivel abrir browser"
        elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
            start "$DASHBOARD_URL"
        fi
    fi
}

# =============================================================================
# Main
# =============================================================================

# Parse argumentos
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-browser)
            OPEN_BROWSER=false
            shift
            ;;
        --reset)
            RESET_VOLUMES=true
            shift
            ;;
        --build)
            FORCE_BUILD=true
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            print_error "Opcao desconhecida: $1"
            show_help
            exit 1
            ;;
    esac
done

# Mudar para diretorio do projeto
cd "$(dirname "$0")/.."

# Executar
print_banner
check_prerequisites
setup_env

if [ "$RESET_VOLUMES" = true ]; then
    reset_volumes
fi

start_services

if wait_for_health; then
    show_status
    open_browser
    exit 0
else
    print_error "Falha ao iniciar ambiente. Verifique os logs:"
    echo "  docker-compose logs"
    exit 1
fi
