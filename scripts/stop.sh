#!/bin/bash
# =============================================================================
# Fabrica de Agentes - Script para Parar Servicos
# =============================================================================
# Uso: ./scripts/stop.sh [opcoes]
#   --clean    Remove volumes (PERDE DADOS!)
#   -h, --help Mostrar ajuda
# =============================================================================

set -e

# Cores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

CLEAN_VOLUMES=false

print_step() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[!]${NC} $1"
}

show_help() {
    echo "Uso: $0 [opcoes]"
    echo ""
    echo "Opcoes:"
    echo "  --clean    Remove volumes (PERDE DADOS!)"
    echo "  -h, --help Mostrar esta ajuda"
}

# Parse argumentos
while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN_VOLUMES=true
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Opcao desconhecida: $1"
            show_help
            exit 1
            ;;
    esac
done

# Mudar para diretorio do projeto
cd "$(dirname "$0")/.."

# Detectar compose command
if docker compose version &> /dev/null; then
    COMPOSE_CMD="docker compose"
else
    COMPOSE_CMD="docker-compose"
fi

print_step "Parando servicos da Fabrica de Agentes..."

if [ "$CLEAN_VOLUMES" = true ]; then
    print_warning "Removendo volumes (dados serao perdidos)..."
    read -p "Confirma? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        $COMPOSE_CMD down -v
        print_success "Servicos parados e volumes removidos"
    else
        print_warning "Cancelado. Parando sem remover volumes..."
        $COMPOSE_CMD down
        print_success "Servicos parados (volumes mantidos)"
    fi
else
    $COMPOSE_CMD down
    print_success "Servicos parados (volumes mantidos)"
fi

echo ""
echo "Para reiniciar: ./scripts/start.sh"
