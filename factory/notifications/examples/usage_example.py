# -*- coding: utf-8 -*-
"""
Exemplos de Uso do Sistema de Notificacoes
Plataforma E v6.0

Este arquivo demonstra como usar o sistema de notificacoes
em diferentes cenarios.
"""

import asyncio
import os


# =============================================================================
# EXEMPLO 1: Configuracao Basica
# =============================================================================

def exemplo_configuracao_basica():
    """
    Demonstra como configurar os canais de notificacao.
    """
    from factory.notifications import notification_manager

    # Configurar canal de Email (SMTP)
    notification_manager.register_channel("email", {
        "smtp_host": "smtp.gmail.com",
        "smtp_port": 587,
        "username": "seu_email@gmail.com",
        "password": "sua_app_password",  # Use App Password do Google
        "from_email": "seu_email@gmail.com",
        "from_name": "Plataforma E",
        "use_tls": True
    })

    # Configurar canal de Slack
    notification_manager.register_channel("slack", {
        "webhook_url": "https://hooks.slack.com/services/XXX/YYY/ZZZ",
        "channel": "#desenvolvimento",
        "username": "Fabrica Bot",
        "icon_emoji": ":factory:"
    })

    # Configurar canal de Teams
    notification_manager.register_channel("teams", {
        "webhook_url": "https://outlook.office.com/webhook/XXX/IncomingWebhook/YYY/ZZZ"
    })

    print("Canais configurados:", notification_manager.list_channels())


# =============================================================================
# EXEMPLO 2: Envio Simples
# =============================================================================

async def exemplo_envio_simples():
    """
    Demonstra como enviar uma notificacao simples.
    """
    from factory.notifications import notification_manager

    # Enviar notificacao
    resultado = await notification_manager.notify(
        event_type="project_completed",
        data={
            "project_name": "Sistema de Vendas",
            "duration": "2 horas",
            "files_count": 45,
            "summary": "Projeto finalizado com sucesso!"
        },
        recipients=["equipe@empresa.com"],
        channels=["email", "teams"]  # Opcional: especificar canais
    )

    print(f"Notificacao enviada: {resultado.success}")
    print(f"Canais usados: {resultado.channels_sent}")
    print(f"Canais falharam: {resultado.channels_failed}")


# =============================================================================
# EXEMPLO 3: Uso com Event Handlers
# =============================================================================

async def exemplo_event_handlers():
    """
    Demonstra como usar os event handlers pre-definidos.
    """
    from factory.notifications import event_handler

    # Notificar projeto criado
    await event_handler.on_project_created(
        project_id="PRJ-001",
        project_name="Novo Sistema",
        project_type="web_application",
        created_by="joao.silva",
        description="Sistema de gestao de vendas"
    )

    # Notificar story completada
    await event_handler.on_story_completed(
        story_id="STR-0042",
        title="Implementar login com email",
        assignee="maria.santos",
        duration="4 horas",
        tasks_completed=5,
        tasks_total=5,
        tasks_summary="- Endpoint de autenticacao\n- Validacao de senha\n- Testes unitarios"
    )

    # Notificar erro
    await event_handler.on_error(
        error_type="DatabaseConnectionError",
        error_message="Falha ao conectar ao banco de dados PostgreSQL",
        severity="critical",
        component="database.connection"
    )


# =============================================================================
# EXEMPLO 4: Configuracao via Variaveis de Ambiente
# =============================================================================

def exemplo_config_env():
    """
    Demonstra como configurar via variaveis de ambiente.
    """
    # Definir variaveis de ambiente (normalmente feito no .env ou sistema)
    os.environ["NOTIFICATION_EMAIL_ENABLED"] = "true"
    os.environ["NOTIFICATION_EMAIL_SMTP_HOST"] = "smtp.gmail.com"
    os.environ["NOTIFICATION_EMAIL_SMTP_PORT"] = "587"
    os.environ["NOTIFICATION_EMAIL_USERNAME"] = "seu_email@gmail.com"
    os.environ["NOTIFICATION_EMAIL_PASSWORD"] = "sua_app_password"
    os.environ["NOTIFICATION_EMAIL_FROM"] = "seu_email@gmail.com"
    os.environ["NOTIFICATION_EMAIL_USE_TLS"] = "true"

    os.environ["NOTIFICATION_SLACK_ENABLED"] = "true"
    os.environ["NOTIFICATION_SLACK_WEBHOOK_URL"] = "https://hooks.slack.com/services/XXX"

    os.environ["NOTIFICATION_TEAMS_ENABLED"] = "true"
    os.environ["NOTIFICATION_TEAMS_WEBHOOK_URL"] = "https://outlook.office.com/webhook/XXX"

    # Carregar e aplicar configuracao
    from factory.notifications import load_config, setup_notification_manager

    config = load_config()
    print("Configuracao carregada:", config.to_dict())

    setup_notification_manager(config)
    print("NotificationManager configurado!")


# =============================================================================
# EXEMPLO 5: Regras Customizadas
# =============================================================================

def exemplo_regras_customizadas():
    """
    Demonstra como adicionar regras customizadas.
    """
    from factory.notifications import notification_manager

    # Adicionar regra customizada
    notification_manager.add_rule({
        "rule_id": "CUSTOM-001",
        "name": "Projetos do Time Backend",
        "description": "Notifica o time de backend sobre projetos especificos",
        "event_type": "project_created",
        "conditions": {
            "operator": "AND",
            "conditions": [
                {"field": "project_type", "op": "in", "value": ["api", "backend", "microservice"]},
                {"field": "priority", "op": "eq", "value": "high"}
            ]
        },
        "channels": ["slack", "email"],
        "recipients": ["backend-team@empresa.com"],
        "template": "project_created",
        "priority": "high",
        "is_active": True
    })

    print("Regra customizada adicionada!")


# =============================================================================
# EXEMPLO 6: Decoradores
# =============================================================================

async def exemplo_decoradores():
    """
    Demonstra como usar decoradores para notificacoes automaticas.
    """
    from factory.notifications import notify_on_complete, notify_on_error

    @notify_on_complete("job_completed", lambda r: {"job_id": r, "status": "success"})
    async def processar_job(job_id: str):
        """Funcao que notifica automaticamente ao completar"""
        # ... processamento ...
        return job_id

    @notify_on_error()
    async def funcao_arriscada():
        """Funcao que notifica automaticamente em caso de erro"""
        # ... codigo que pode falhar ...
        pass

    # Executar
    await processar_job("JOB-001")


# =============================================================================
# EXEMPLO 7: Quiet Hours
# =============================================================================

def exemplo_quiet_hours():
    """
    Demonstra como configurar horario de nao perturbe.
    """
    from factory.notifications import notification_manager

    # Configurar quiet hours (22:00 - 07:00)
    notification_manager.set_quiet_hours(
        start="22:00",
        end="07:00",
        timezone="America/Sao_Paulo",
        exceptions=["error", "approval_required"]  # Estes eventos ignoram quiet hours
    )

    print("Quiet hours configurado!")


# =============================================================================
# EXEMPLO 8: Mensagem com Acoes/Botoes
# =============================================================================

async def exemplo_com_acoes():
    """
    Demonstra como enviar notificacao com botoes de acao.
    """
    from factory.notifications import notification_manager

    resultado = await notification_manager.notify(
        event_type="approval_required",
        data={
            "action_type": "Deploy para Producao",
            "requester": "Carlos Silva",
            "project_name": "Sistema de Vendas",
            "description": "Solicitacao de deploy da versao 2.1.0",
            "approval_url": "https://sistema.empresa.com/approvals/123"
        },
        context={
            "actions": [
                {
                    "text": "Aprovar",
                    "url": "https://sistema.empresa.com/approvals/123/approve",
                    "style": "primary"
                },
                {
                    "text": "Rejeitar",
                    "url": "https://sistema.empresa.com/approvals/123/reject",
                    "style": "danger"
                },
                {
                    "text": "Ver Detalhes",
                    "url": "https://sistema.empresa.com/approvals/123"
                }
            ],
            "approvers": ["gerente@empresa.com", "tech-lead@empresa.com"]
        }
    )

    print(f"Solicitacao de aprovacao enviada: {resultado.success}")


# =============================================================================
# EXEMPLO 9: Estatisticas
# =============================================================================

def exemplo_estatisticas():
    """
    Demonstra como obter estatisticas do sistema.
    """
    from factory.notifications import notification_manager

    stats = notification_manager.get_stats()
    print("Estatisticas do NotificationManager:")
    print(f"  - Habilitado: {stats['enabled']}")
    print(f"  - Canais: {stats['channels_count']}")
    print(f"  - Regras: {stats['rules_count']}")
    print(f"  - Templates: {stats['templates_count']}")


# =============================================================================
# EXEMPLO 10: Callback de Eventos
# =============================================================================

def exemplo_callbacks():
    """
    Demonstra como registrar callbacks para eventos.
    """
    from factory.notifications import notification_manager

    def on_notification_sent(result):
        """Callback executado apos cada envio"""
        print(f"Notificacao {result.notification_id} enviada!")
        print(f"  Canais: {result.channels_sent}")
        print(f"  Destinatarios: {result.recipients_count}")

    def on_notification_error(notification_id, event_type, error):
        """Callback executado em caso de erro"""
        print(f"ERRO na notificacao {notification_id}: {error}")

    # Registrar callbacks
    notification_manager.on_send(on_notification_sent)
    notification_manager.on_error(on_notification_error)

    print("Callbacks registrados!")


# =============================================================================
# MAIN - Executar exemplos
# =============================================================================

if __name__ == "__main__":
    print("=" * 60)
    print("Exemplos de Uso do Sistema de Notificacoes")
    print("=" * 60)

    # Executar exemplos sincronos
    print("\n1. Configuracao basica:")
    # exemplo_configuracao_basica()  # Descomentar para testar

    print("\n2. Configuracao via ambiente:")
    # exemplo_config_env()  # Descomentar para testar

    print("\n3. Regras customizadas:")
    # exemplo_regras_customizadas()  # Descomentar para testar

    print("\n4. Quiet hours:")
    # exemplo_quiet_hours()  # Descomentar para testar

    print("\n5. Estatisticas:")
    # exemplo_estatisticas()  # Descomentar para testar

    print("\n6. Callbacks:")
    # exemplo_callbacks()  # Descomentar para testar

    # Executar exemplos async
    print("\n7. Envio simples (async):")
    # asyncio.run(exemplo_envio_simples())  # Descomentar para testar

    print("\n8. Event handlers (async):")
    # asyncio.run(exemplo_event_handlers())  # Descomentar para testar

    print("\n9. Com acoes (async):")
    # asyncio.run(exemplo_com_acoes())  # Descomentar para testar

    print("\nExemplos prontos! Descomente as linhas para executar.")
