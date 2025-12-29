#!/bin/bash
# =============================================================================
# Fabrica de Agentes - Script de Deploy Kubernetes
# =============================================================================
# Uso: ./scripts/deploy-k8s.sh [environment]
# Exemplo: ./scripts/deploy-k8s.sh production
# =============================================================================

set -e

# Cores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Funcoes auxiliares
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Configuracoes
NAMESPACE="fabrica-agentes"
ENVIRONMENT="${1:-production}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
K8S_DIR="$PROJECT_DIR/k8s"
HELM_DIR="$PROJECT_DIR/helm/fabrica-agentes"

# Banner
echo "============================================================================="
echo "  Fabrica de Agentes - Kubernetes Deploy"
echo "  Ambiente: $ENVIRONMENT"
echo "  Namespace: $NAMESPACE"
echo "============================================================================="
echo ""

# Verificar pre-requisitos
log_info "Verificando pre-requisitos..."

if ! command -v kubectl &> /dev/null; then
    log_error "kubectl nao encontrado. Por favor, instale o kubectl."
    exit 1
fi

if ! kubectl cluster-info &> /dev/null; then
    log_error "Nao foi possivel conectar ao cluster Kubernetes."
    exit 1
fi

log_success "kubectl configurado corretamente"

# Verificar se arquivos existem
if [ ! -d "$K8S_DIR" ]; then
    log_error "Diretorio k8s/ nao encontrado em $K8S_DIR"
    exit 1
fi

# Opcao de usar Helm ou Kustomize
echo ""
echo "Selecione o metodo de deploy:"
echo "1) Kustomize (kubectl apply -k)"
echo "2) Helm Chart"
echo "3) Manifests individuais (kubectl apply -f)"
read -p "Opcao [1-3]: " DEPLOY_METHOD

case $DEPLOY_METHOD in
    1)
        # Deploy com Kustomize
        log_info "Iniciando deploy com Kustomize..."

        # Aplicar todos os recursos
        kubectl apply -k "$K8S_DIR"

        log_success "Deploy com Kustomize concluido!"
        ;;

    2)
        # Deploy com Helm
        log_info "Iniciando deploy com Helm..."

        if ! command -v helm &> /dev/null; then
            log_error "helm nao encontrado. Por favor, instale o Helm."
            exit 1
        fi

        # Verificar se values customizado existe
        CUSTOM_VALUES=""
        if [ -f "$PROJECT_DIR/values-$ENVIRONMENT.yaml" ]; then
            CUSTOM_VALUES="-f $PROJECT_DIR/values-$ENVIRONMENT.yaml"
            log_info "Usando values customizado: values-$ENVIRONMENT.yaml"
        fi

        # Atualizar dependencias do Helm
        log_info "Atualizando dependencias do Helm..."
        helm dependency update "$HELM_DIR" 2>/dev/null || true

        # Verificar se release existe
        if helm list -n "$NAMESPACE" | grep -q "fabrica-agentes"; then
            log_info "Release existente encontrada. Executando upgrade..."
            helm upgrade fabrica-agentes "$HELM_DIR" \
                -n "$NAMESPACE" \
                $CUSTOM_VALUES \
                --wait \
                --timeout 10m
        else
            log_info "Instalando nova release..."
            helm install fabrica-agentes "$HELM_DIR" \
                -n "$NAMESPACE" \
                --create-namespace \
                $CUSTOM_VALUES \
                --wait \
                --timeout 10m
        fi

        log_success "Deploy com Helm concluido!"
        ;;

    3)
        # Deploy com manifests individuais
        log_info "Iniciando deploy com manifests individuais..."

        # 1. Namespace
        log_info "1/7 - Criando namespace..."
        kubectl apply -f "$K8S_DIR/namespace.yaml"

        # 2. Secrets e ConfigMaps
        log_info "2/7 - Aplicando configuracoes..."
        kubectl apply -f "$K8S_DIR/secrets.yaml"
        kubectl apply -f "$K8S_DIR/configmap.yaml"

        # 3. Storage
        log_info "3/7 - Criando volumes persistentes..."
        kubectl apply -f "$K8S_DIR/storage/pvc.yaml"

        # 4. Aguardar PVCs
        log_info "4/7 - Aguardando PVCs ficarem bound..."
        kubectl wait --for=condition=Bound pvc --all -n "$NAMESPACE" --timeout=120s || true

        # 5. API
        log_info "5/7 - Deployando API..."
        kubectl apply -f "$K8S_DIR/api/"

        # 6. Workers
        log_info "6/7 - Deployando Workers..."
        kubectl apply -f "$K8S_DIR/workers/"

        # 7. Ingress
        log_info "7/7 - Configurando Ingress..."
        kubectl apply -f "$K8S_DIR/ingress.yaml"

        log_success "Deploy com manifests concluido!"
        ;;

    *)
        log_error "Opcao invalida"
        exit 1
        ;;
esac

# Aguardar pods ficarem prontos
echo ""
log_info "Aguardando pods ficarem prontos..."
kubectl wait --for=condition=Ready pod \
    -l app.kubernetes.io/name=fabrica-agentes \
    -n "$NAMESPACE" \
    --timeout=300s || log_warn "Timeout aguardando pods. Verifique o status manualmente."

# Mostrar status final
echo ""
echo "============================================================================="
echo "  Status do Deploy"
echo "============================================================================="
echo ""

log_info "Pods:"
kubectl get pods -n "$NAMESPACE" -o wide

echo ""
log_info "Services:"
kubectl get svc -n "$NAMESPACE"

echo ""
log_info "Ingress:"
kubectl get ingress -n "$NAMESPACE"

echo ""
log_info "HPA:"
kubectl get hpa -n "$NAMESPACE"

echo ""
log_info "PVC:"
kubectl get pvc -n "$NAMESPACE"

# Verificar health
echo ""
log_info "Verificando health da API..."
API_POD=$(kubectl get pod -l app.kubernetes.io/component=api -n "$NAMESPACE" -o jsonpath='{.items[0].metadata.name}' 2>/dev/null)
if [ -n "$API_POD" ]; then
    kubectl exec "$API_POD" -n "$NAMESPACE" -- curl -s http://localhost:9001/health 2>/dev/null && log_success "API saudavel!" || log_warn "Health check falhou"
fi

echo ""
echo "============================================================================="
log_success "Deploy concluido com sucesso!"
echo "============================================================================="
echo ""
echo "Proximos passos:"
echo "  1. Configure o DNS para apontar para o Ingress"
echo "  2. Verifique os certificados TLS"
echo "  3. Acesse o dashboard: kubectl port-forward svc/fabrica-api-service 9001:9001 -n $NAMESPACE"
echo ""
