@echo off
REM =============================================================================
REM Fabrica de Agentes - Script de Inicializacao Local (Windows)
REM =============================================================================
REM Issue #375: Script para iniciar ambiente de desenvolvimento com 1 comando
REM
REM Uso: scripts\start.bat [opcoes]
REM   /nobrowser    Nao abrir browser automaticamente
REM   /build        Forcar rebuild das imagens
REM   /?            Mostrar ajuda
REM =============================================================================

setlocal enabledelayedexpansion

REM Configuracoes
set DASHBOARD_PORT=9001
set DASHBOARD_URL=http://localhost:%DASHBOARD_PORT%
set MAX_WAIT_SECONDS=120
set OPEN_BROWSER=1
set FORCE_BUILD=0

REM Parse argumentos
:parse_args
if "%~1"=="" goto :start_main
if /i "%~1"=="/nobrowser" set OPEN_BROWSER=0
if /i "%~1"=="/build" set FORCE_BUILD=1
if /i "%~1"=="/?" goto :show_help
if /i "%~1"=="-h" goto :show_help
if /i "%~1"=="--help" goto :show_help
shift
goto :parse_args

:show_help
echo Uso: start.bat [opcoes]
echo.
echo Opcoes:
echo   /nobrowser    Nao abrir browser automaticamente
echo   /build        Forcar rebuild das imagens
echo   /?            Mostrar esta ajuda
echo.
goto :eof

:start_main
REM Mudar para diretorio do projeto
cd /d "%~dp0\.."

echo.
echo ╔═══════════════════════════════════════════════════════════════╗
echo ║           FABRICA DE AGENTES - Ambiente Local                 ║
echo ╚═══════════════════════════════════════════════════════════════╝
echo.

REM Verificar Docker
echo [INFO] Verificando pre-requisitos...
docker --version >nul 2>&1
if errorlevel 1 (
    echo [ERRO] Docker nao encontrado. Instale em: https://docs.docker.com/get-docker/
    exit /b 1
)
echo [OK] Docker instalado

REM Verificar Docker daemon
docker info >nul 2>&1
if errorlevel 1 (
    echo [ERRO] Docker daemon nao esta rodando. Inicie o Docker Desktop.
    exit /b 1
)
echo [OK] Docker daemon rodando

REM Configurar .env
echo [INFO] Configurando variaveis de ambiente...
if not exist ".env" (
    if exist ".env.example" (
        copy .env.example .env >nul
        echo [OK] Arquivo .env criado a partir de .env.example
        echo [AVISO] Edite .env para configurar sua ANTHROPIC_API_KEY
    ) else (
        echo [AVISO] Arquivo .env.example nao encontrado, usando defaults
    )
) else (
    echo [OK] Arquivo .env encontrado
)

REM Iniciar servicos
echo [INFO] Iniciando servicos...
if %FORCE_BUILD%==1 (
    docker-compose up -d --build
) else (
    docker-compose up -d
)
if errorlevel 1 (
    echo [ERRO] Falha ao iniciar containers
    exit /b 1
)
echo [OK] Containers iniciados

REM Aguardar Dashboard
echo [INFO] Aguardando Dashboard ficar pronto...
set /a elapsed=0
set /a interval=5

:wait_loop
if %elapsed% geq %MAX_WAIT_SECONDS% (
    echo [ERRO] Timeout aguardando Dashboard
    goto :show_status
)

curl -sf %DASHBOARD_URL%/health >nul 2>&1
if not errorlevel 1 (
    echo [OK] Dashboard pronto!
    goto :success
)

echo|set /p="."
timeout /t %interval% /nobreak >nul
set /a elapsed+=interval
goto :wait_loop

:success
echo.
echo.
echo ═══════════════════════════════════════════════════════════════
echo                    AMBIENTE INICIADO COM SUCESSO
echo ═══════════════════════════════════════════════════════════════
echo.
echo   Dashboard:      %DASHBOARD_URL%
echo   Swagger API:    %DASHBOARD_URL%/docs
echo   PostgreSQL:     localhost:5433
echo   Redis:          localhost:6379
echo   MinIO Console:  http://localhost:9090
echo.
echo   Comandos uteis:
echo     docker-compose logs -f app     Ver logs
echo     scripts\stop.bat               Parar servicos
echo     docker-compose ps              Ver status
echo.

REM Abrir browser
if %OPEN_BROWSER%==1 (
    echo [INFO] Abrindo browser...
    start "" "%DASHBOARD_URL%"
)

goto :eof

:show_status
echo.
echo [AVISO] Dashboard pode nao estar totalmente pronto.
echo Verifique os logs com: docker-compose logs
echo.
goto :eof
