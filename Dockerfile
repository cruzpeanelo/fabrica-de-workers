# =============================================================================
# Fabrica de Agentes - Dockerfile
# =============================================================================
# Multi-stage build para imagem de desenvolvimento/producao
#
# Build:   docker build -t fabrica-agentes:latest .
# Run:     docker run -p 9001:9001 fabrica-agentes:latest
#
# Portas:
#   9001 - Dashboard Agile (API + Frontend)
#
# Para builds de producao com multiplos targets, use Dockerfile.prod
# =============================================================================

# Build stage
FROM python:3.12-slim AS builder

WORKDIR /app

# Install build dependencies (incluindo libpq-dev para PostgreSQL)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements and install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir --user -r requirements.txt

# =============================================================================
# Production stage
# =============================================================================
FROM python:3.12-slim

LABEL maintainer="Fabrica de Agentes <fabrica@belgo.com.br>"
LABEL version="6.5"
LABEL description="Dashboard Agile - Sistema de desenvolvimento autonomo com IA"

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 \
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security
RUN groupadd --gid 1000 appgroup \
    && useradd --uid 1000 --gid appgroup --create-home --shell /bin/bash appuser

# Copy installed packages from builder
COPY --from=builder /root/.local /home/appuser/.local

# Copy application code
COPY --chown=appuser:appgroup factory/ ./factory/
COPY --chown=appuser:appgroup requirements.txt .

# Set environment variables
ENV PATH=/home/appuser/.local/bin:$PATH \
    PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONPATH=/app \
    DASHBOARD_PORT=9001 \
    DASHBOARD_HOST=0.0.0.0

# Create directories for data persistence
RUN mkdir -p /app/factory/database /app/projects /app/uploads /app/logs \
    && chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Expose Dashboard Agile port
EXPOSE 9001

# Health check - verifica endpoint /health do Dashboard
HEALTHCHECK --interval=30s --timeout=10s --start-period=15s --retries=3 \
    CMD curl -f http://localhost:9001/health || exit 1

# Run the Dashboard Agile application
CMD ["python", "-m", "uvicorn", "factory.dashboard.app_v6_agile:app", "--host", "0.0.0.0", "--port", "9001"]
