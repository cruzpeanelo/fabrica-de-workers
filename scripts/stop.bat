@echo off
REM =============================================================================
REM Fabrica de Agentes - Script para Parar Servicos (Windows)
REM =============================================================================
REM Uso: scripts\stop.bat [opcoes]
REM   /clean    Remove volumes (PERDE DADOS!)
REM   /?        Mostrar ajuda
REM =============================================================================

setlocal

set CLEAN_VOLUMES=0

REM Parse argumentos
:parse_args
if "%~1"=="" goto :start_main
if /i "%~1"=="/clean" set CLEAN_VOLUMES=1
if /i "%~1"=="/?" goto :show_help
if /i "%~1"=="-h" goto :show_help
shift
goto :parse_args

:show_help
echo Uso: stop.bat [opcoes]
echo.
echo Opcoes:
echo   /clean    Remove volumes (PERDE DADOS!)
echo   /?        Mostrar esta ajuda
echo.
goto :eof

:start_main
REM Mudar para diretorio do projeto
cd /d "%~dp0\.."

echo [INFO] Parando servicos da Fabrica de Agentes...

if %CLEAN_VOLUMES%==1 (
    echo [AVISO] Removendo volumes (dados serao perdidos)...
    set /p confirm="Confirma? (S/N): "
    if /i "!confirm!"=="S" (
        docker-compose down -v
        echo [OK] Servicos parados e volumes removidos
    ) else (
        echo [INFO] Cancelado. Parando sem remover volumes...
        docker-compose down
        echo [OK] Servicos parados (volumes mantidos)
    )
) else (
    docker-compose down
    echo [OK] Servicos parados (volumes mantidos)
)

echo.
echo Para reiniciar: scripts\start.bat
