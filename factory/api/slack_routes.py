# -*- coding: utf-8 -*-
"""
Slack Integration API Routes
============================
Endpoints para integracao bidirecional com Slack.

Endpoints:
- GET  /api/v1/slack/status - Status da integracao
- POST /api/v1/slack/oauth/callback - OAuth callback
- POST /api/v1/slack/commands - Handler de slash commands
- POST /api/v1/slack/events - Handler de eventos/webhooks
- POST /api/v1/slack/interactivity - Handler de interatividade (botoes, modais)
- POST /api/v1/slack/notify - Enviar notificacao manual
- GET  /api/v1/slack/channels - Lista canais disponiveis
- POST /api/v1/slack/test - Testar conexao

Issue #263 - Integracao com Slack bidirecional
"""

import os
import json
import logging
import time
from typing import Optional, Dict, Any
from datetime import datetime

from fastapi import APIRouter, HTTPException, Request, Response, BackgroundTasks
from fastapi.responses import JSONResponse, RedirectResponse
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1/slack", tags=["Slack Integration"])


# =============================================================================
# MODELS
# =============================================================================

class SlackNotifyRequest(BaseModel):
    """Request para enviar notificacao"""
    channel: Optional[str] = Field(None, description="Canal de destino (usa default se nao especificado)")
    message: str = Field(..., description="Mensagem a enviar")
    blocks: Optional[list] = Field(None, description="Blocos Block Kit opcionais")
    thread_ts: Optional[str] = Field(None, description="Thread para responder")


class SlackStoryNotifyRequest(BaseModel):
    """Request para notificar sobre story"""
    story_id: str = Field(..., description="ID da story")
    action: str = Field("created", description="Acao: created, updated, status_changed")
    channel: Optional[str] = Field(None, description="Canal de destino")
    changes: Optional[Dict[str, Any]] = Field(None, description="Mudancas (para updates)")


class SlackConfigRequest(BaseModel):
    """Request para configurar Slack"""
    bot_token: Optional[str] = Field(None, description="Token do bot")
    default_channel: Optional[str] = Field(None, description="Canal padrao")
    notify_on_story_create: bool = Field(True)
    notify_on_status_change: bool = Field(True)
    notify_on_task_complete: bool = Field(True)


# =============================================================================
# HELPERS
# =============================================================================

def get_tenant_id(request: Request) -> str:
    """Extrai tenant_id do request"""
    tenant_id = request.headers.get("X-Tenant-ID")
    if tenant_id:
        return tenant_id
    return "default"


async def verify_slack_signature(request: Request) -> bool:
    """
    Verifica assinatura do Slack.

    Valida que o request veio do Slack usando o signing secret.
    """
    from factory.integrations.slack_integration import SlackIntegrationConfig

    config = SlackIntegrationConfig.from_env()
    if not config.signing_secret:
        logger.warning("Signing secret nao configurado, pulando verificacao")
        return True

    timestamp = request.headers.get("X-Slack-Request-Timestamp", "")
    signature = request.headers.get("X-Slack-Signature", "")

    if not timestamp or not signature:
        return False

    # Anti-replay: verificar timestamp
    current_time = int(time.time())
    try:
        request_time = int(timestamp)
    except ValueError:
        return False

    if abs(current_time - request_time) > 300:
        logger.warning("Timestamp do Slack muito antigo")
        return False

    # Calcular assinatura
    import hmac
    import hashlib

    body = await request.body()
    sig_basestring = f"v0:{timestamp}:{body.decode()}"
    expected_sig = "v0=" + hmac.new(
        config.signing_secret.encode(),
        sig_basestring.encode(),
        hashlib.sha256
    ).hexdigest()

    return hmac.compare_digest(expected_sig, signature)


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_slack_status(request: Request):
    """
    Retorna status da integracao com Slack.

    Inclui:
    - Se esta conectado
    - Configuracoes ativas
    - Info do workspace
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.slack_integration import get_slack_integration

        slack = get_slack_integration(tenant_id)
        status = slack.get_status()

        return {
            "tenant_id": tenant_id,
            **status
        }

    except Exception as e:
        logger.error(f"Erro ao buscar status Slack: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/test")
async def test_slack_connection(request: Request):
    """
    Testa conexao com Slack.

    Verifica se as credenciais sao validas e o bot esta ativo.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.slack_integration import get_slack_integration

        slack = get_slack_integration(tenant_id)
        connected = await slack.test_connection()

        if connected:
            return {
                "success": True,
                "message": "Conexao com Slack OK",
                "status": slack.get_status()
            }
        else:
            return {
                "success": False,
                "message": "Falha ao conectar",
                "error": slack.last_error
            }

    except Exception as e:
        logger.error(f"Erro ao testar Slack: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/oauth/authorize")
async def slack_oauth_authorize(request: Request):
    """
    Inicia fluxo OAuth com Slack.

    Redireciona para pagina de autorizacao do Slack.
    """
    from factory.integrations.slack_integration import SlackIntegrationConfig

    config = SlackIntegrationConfig.from_env()

    if not config.client_id:
        raise HTTPException(
            status_code=400,
            detail="SLACK_CLIENT_ID nao configurado"
        )

    # Construir URL de autorizacao
    scopes = [
        "chat:write",
        "chat:write.public",
        "channels:read",
        "channels:join",
        "users:read",
        "reactions:read",
        "reactions:write",
        "commands",
        "app_mentions:read"
    ]

    # URL base do app para callback
    base_url = os.getenv("APP_BASE_URL", "http://localhost:9001")
    redirect_uri = f"{base_url}/api/v1/slack/oauth/callback"

    state = f"tenant_{get_tenant_id(request)}_{int(time.time())}"

    auth_url = (
        f"https://slack.com/oauth/v2/authorize"
        f"?client_id={config.client_id}"
        f"&scope={','.join(scopes)}"
        f"&redirect_uri={redirect_uri}"
        f"&state={state}"
    )

    return RedirectResponse(url=auth_url)


@router.get("/oauth/callback")
async def slack_oauth_callback(
    request: Request,
    code: Optional[str] = None,
    state: Optional[str] = None,
    error: Optional[str] = None
):
    """
    Callback do OAuth Slack.

    Troca o code por access token e salva configuracao.
    """
    if error:
        logger.error(f"Erro no OAuth Slack: {error}")
        raise HTTPException(status_code=400, detail=f"OAuth error: {error}")

    if not code:
        raise HTTPException(status_code=400, detail="Codigo de autorizacao nao fornecido")

    from factory.integrations.slack_integration import SlackIntegrationConfig

    config = SlackIntegrationConfig.from_env()

    if not config.client_id or not config.client_secret:
        raise HTTPException(
            status_code=400,
            detail="SLACK_CLIENT_ID ou SLACK_CLIENT_SECRET nao configurado"
        )

    # Extrair tenant do state
    tenant_id = "default"
    if state and state.startswith("tenant_"):
        parts = state.split("_")
        if len(parts) >= 2:
            tenant_id = parts[1]

    try:
        import aiohttp

        # URL de callback
        base_url = os.getenv("APP_BASE_URL", "http://localhost:9001")
        redirect_uri = f"{base_url}/api/v1/slack/oauth/callback"

        # Trocar code por token
        async with aiohttp.ClientSession() as session:
            async with session.post(
                "https://slack.com/api/oauth.v2.access",
                data={
                    "client_id": config.client_id,
                    "client_secret": config.client_secret,
                    "code": code,
                    "redirect_uri": redirect_uri
                }
            ) as response:
                result = await response.json()

        if not result.get("ok"):
            error_msg = result.get("error", "Erro desconhecido")
            logger.error(f"Erro ao trocar code por token: {error_msg}")
            raise HTTPException(status_code=400, detail=f"OAuth failed: {error_msg}")

        # Extrair tokens e info
        access_token = result.get("access_token")
        team_info = result.get("team", {})

        logger.info(
            f"OAuth Slack concluido para tenant {tenant_id}, "
            f"team: {team_info.get('name')}"
        )

        # TODO: Salvar token no banco de dados para o tenant

        return {
            "success": True,
            "message": "Integracao Slack configurada com sucesso",
            "team": {
                "id": team_info.get("id"),
                "name": team_info.get("name")
            },
            "tenant_id": tenant_id
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no OAuth callback: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/commands")
async def handle_slash_command(request: Request):
    """
    Handler para slash commands do Slack.

    Comandos suportados:
    - /story [titulo] - Cria nova story
    - /status [story_id] - Verifica status de story
    - /help - Mostra ajuda
    """
    # Verificar assinatura
    if not await verify_slack_signature(request):
        raise HTTPException(status_code=401, detail="Assinatura invalida")

    # Parse form data
    form = await request.form()
    command = form.get("command", "")
    text = form.get("text", "")
    user_id = form.get("user_id", "")
    user_name = form.get("user_name", "")
    channel_id = form.get("channel_id", "")
    team_id = form.get("team_id", "")
    response_url = form.get("response_url", "")

    logger.info(f"Slash command recebido: {command} {text} de {user_name}")

    # Processar comandos
    if command == "/story":
        return await handle_story_command(text, user_id, channel_id, team_id)

    elif command == "/status":
        return await handle_status_command(text, user_id, channel_id)

    elif command == "/help" or command == "/fabrica":
        return {
            "response_type": "ephemeral",
            "blocks": [
                {
                    "type": "header",
                    "text": {"type": "plain_text", "text": "Fabrica de Agentes - Ajuda", "emoji": True}
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": (
                            "*Comandos disponiveis:*\n\n"
                            "`/story [titulo]` - Cria uma nova story\n"
                            "`/story [EPIC-01] [titulo]` - Cria story em um epic\n"
                            "`/status [story_id]` - Verifica status de uma story\n"
                            "`/help` - Mostra esta ajuda\n\n"
                            "*Reacoes para atualizar status:*\n"
                            ":eyes: - Em progresso\n"
                            ":rocket: - Pronto\n"
                            ":white_check_mark: - Concluido\n"
                            ":hourglass: - Em revisao\n"
                            ":bug: - Em teste"
                        )
                    }
                }
            ]
        }

    else:
        return {
            "response_type": "ephemeral",
            "text": f"Comando {command} nao reconhecido. Use /help para ver comandos disponiveis."
        }


async def handle_story_command(
    text: str,
    user_id: str,
    channel_id: str,
    team_id: str
) -> Dict:
    """Processa comando /story"""
    if not text.strip():
        return {
            "response_type": "ephemeral",
            "text": "Por favor, forneca um titulo para a story. Exemplo: `/story Implementar login com OAuth`"
        }

    # Extrair epic se presente
    epic_id = None
    title = text.strip()

    if title.startswith("["):
        end = title.find("]")
        if end > 0:
            epic_id = title[1:end]
            title = title[end + 1:].strip()

    # Extrair prioridade
    priority = "medium"
    if title.endswith("!!!!") or title.endswith("!!!"):
        priority = "urgent"
        title = title.rstrip("!")
    elif title.endswith("!!"):
        priority = "high"
        title = title.rstrip("!")
    elif title.endswith("!"):
        priority = "medium"
        title = title.rstrip("!")

    try:
        # Criar story
        from factory.database.repositories import StoryRepository
        from factory.database.connection import get_session

        with get_session() as session:
            repo = StoryRepository(session)

            story_data = {
                "title": title,
                "status": "backlog",
                "priority": priority,
                "created_via": "slack",
                "slack_channel": channel_id,
                "slack_user": user_id,
                "slack_team": team_id
            }

            if epic_id:
                story_data["epic_id"] = epic_id

            story = repo.create(story_data)
            story_id = story.story_id

        # Responder
        priority_emoji = {
            "urgent": ":rotating_light:",
            "high": ":fire:",
            "medium": ":large_blue_circle:",
            "low": ":white_circle:"
        }.get(priority, ":large_blue_circle:")

        return {
            "response_type": "in_channel",
            "blocks": [
                {
                    "type": "header",
                    "text": {"type": "plain_text", "text": ":sparkles: Nova Story Criada", "emoji": True}
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": f"*{story_id}* - {title}"
                    }
                },
                {
                    "type": "context",
                    "elements": [
                        {"type": "mrkdwn", "text": f":bookmark: Status: backlog"},
                        {"type": "mrkdwn", "text": f"{priority_emoji} Prioridade: {priority}"}
                    ]
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": "_Adicione reacoes para atualizar o status da story!_"
                    }
                }
            ]
        }

    except Exception as e:
        logger.error(f"Erro ao criar story: {e}")
        return {
            "response_type": "ephemeral",
            "text": f"Erro ao criar story: {str(e)}"
        }


async def handle_status_command(text: str, user_id: str, channel_id: str) -> Dict:
    """Processa comando /status"""
    story_id = text.strip().upper()

    if not story_id:
        return {
            "response_type": "ephemeral",
            "text": "Por favor, forneca o ID da story. Exemplo: `/status STR-0001`"
        }

    try:
        from factory.database.repositories import StoryRepository
        from factory.database.connection import get_session

        with get_session() as session:
            repo = StoryRepository(session)
            story = repo.get_by_story_id(story_id)

            if not story:
                return {
                    "response_type": "ephemeral",
                    "text": f"Story {story_id} nao encontrada."
                }

            # Formatar resposta
            status = story.status
            priority = story.priority or "medium"

            status_emoji = {
                "backlog": ":bookmark:",
                "ready": ":rocket:",
                "in_progress": ":eyes:",
                "review": ":hourglass:",
                "testing": ":bug:",
                "done": ":white_check_mark:"
            }.get(status, ":grey_question:")

            priority_emoji = {
                "urgent": ":rotating_light:",
                "high": ":fire:",
                "medium": ":large_blue_circle:",
                "low": ":white_circle:"
            }.get(priority, ":large_blue_circle:")

            # Calcular progresso
            progress = getattr(story, "progress", 0) or 0
            progress_bar = "=" * int(progress / 10) + "-" * (10 - int(progress / 10))

            blocks = [
                {
                    "type": "header",
                    "text": {"type": "plain_text", "text": f"{status_emoji} {story_id}", "emoji": True}
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": f"*{story.title}*"
                    }
                },
                {
                    "type": "section",
                    "fields": [
                        {"type": "mrkdwn", "text": f"*Status:*\n{status_emoji} {status}"},
                        {"type": "mrkdwn", "text": f"*Prioridade:*\n{priority_emoji} {priority}"}
                    ]
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": f"*Progresso:* [{progress_bar}] {progress}%"
                    }
                }
            ]

            # Adicionar pontos se disponivel
            if hasattr(story, "story_points") and story.story_points:
                blocks.append({
                    "type": "context",
                    "elements": [
                        {"type": "mrkdwn", "text": f":1234: *Story Points:* {story.story_points}"}
                    ]
                })

            return {
                "response_type": "in_channel",
                "blocks": blocks
            }

    except Exception as e:
        logger.error(f"Erro ao buscar status: {e}")
        return {
            "response_type": "ephemeral",
            "text": f"Erro ao buscar status: {str(e)}"
        }


@router.post("/events")
async def handle_slack_events(request: Request, background_tasks: BackgroundTasks):
    """
    Handler para eventos do Slack (Event API).

    Processa:
    - URL verification
    - Mensagens
    - Reacoes
    - Mencoes do app
    """
    # Verificar assinatura
    if not await verify_slack_signature(request):
        raise HTTPException(status_code=401, detail="Assinatura invalida")

    body = await request.json()

    # URL Verification challenge
    if body.get("type") == "url_verification":
        return {"challenge": body.get("challenge")}

    # Verificar se e retry (Slack reenvia eventos nao confirmados)
    retry_num = request.headers.get("X-Slack-Retry-Num")
    if retry_num:
        logger.info(f"Ignorando retry #{retry_num} do Slack")
        return Response(status_code=200)

    # Processar evento em background
    event = body.get("event", {})
    event_type = event.get("type", "")

    tenant_id = get_tenant_id(request)

    logger.info(f"Evento Slack recebido: {event_type}")

    # Adicionar processamento em background
    background_tasks.add_task(process_slack_event, event, tenant_id)

    # Responder imediatamente (Slack requer resposta em 3s)
    return Response(status_code=200)


async def process_slack_event(event: Dict, tenant_id: str):
    """Processa evento Slack em background"""
    event_type = event.get("type", "")

    try:
        from factory.integrations.slack_integration import get_slack_integration

        slack = get_slack_integration(tenant_id)

        if event_type == "reaction_added":
            await handle_reaction_event(event, slack, tenant_id)

        elif event_type == "message":
            await handle_message_event(event, slack, tenant_id)

        elif event_type == "app_mention":
            await handle_mention_event(event, slack, tenant_id)

    except Exception as e:
        logger.error(f"Erro ao processar evento {event_type}: {e}")


async def handle_reaction_event(event: Dict, slack, tenant_id: str):
    """Processa evento de reacao"""
    reaction = event.get("reaction", "")
    user = event.get("user", "")
    item = event.get("item", {})
    channel = item.get("channel", "")
    ts = item.get("ts", "")

    # Verificar se e reacao que mapeia para status
    from factory.integrations.slack_integration import REACTION_TO_STATUS

    new_status = REACTION_TO_STATUS.get(reaction)
    if not new_status:
        return

    logger.info(f"Reacao {reaction} -> status {new_status} no canal {channel}")

    # TODO: Buscar story associada a mensagem e atualizar status
    # Isso requer armazenar o mapeamento message_ts -> story_id


async def handle_message_event(event: Dict, slack, tenant_id: str):
    """Processa evento de mensagem"""
    # Ignorar mensagens de bots
    if event.get("bot_id"):
        return

    text = event.get("text", "")
    channel = event.get("channel", "")
    user = event.get("user", "")
    ts = event.get("ts", "")

    # Verificar se e mensagem para criar story
    if text.startswith(":story:") or text.startswith(":bookmark:"):
        logger.info(f"Mensagem para criar story detectada: {text}")

        from factory.integrations.slack_integration import SlackMessage

        message = SlackMessage(
            channel=channel,
            user=user,
            text=text,
            ts=ts
        )

        story_data = slack.client.parse_story_from_message(message)
        if story_data:
            # TODO: Criar story e notificar
            pass


async def handle_mention_event(event: Dict, slack, tenant_id: str):
    """Processa mencao do app"""
    channel = event.get("channel", "")
    ts = event.get("ts", "")

    # Responder a mencao
    await slack.client.send_message(
        channel=channel,
        text="Ola! Use `/story` para criar uma nova story ou `/status` para verificar o status.",
        thread_ts=ts
    )


@router.post("/interactivity")
async def handle_slack_interactivity(request: Request):
    """
    Handler para interatividade do Slack.

    Processa:
    - Cliques em botoes
    - Selecoes em menus
    - Submissoes de modais
    """
    if not await verify_slack_signature(request):
        raise HTTPException(status_code=401, detail="Assinatura invalida")

    form = await request.form()
    payload_str = form.get("payload", "{}")
    payload = json.loads(payload_str)

    action_type = payload.get("type", "")
    logger.info(f"Interatividade Slack: {action_type}")

    if action_type == "block_actions":
        actions = payload.get("actions", [])
        for action in actions:
            action_id = action.get("action_id", "")

            if action_id == "view_story":
                story_id = action.get("value", "")
                # TODO: Abrir modal com detalhes da story
                return {
                    "response_type": "ephemeral",
                    "text": f"Visualizando story {story_id}..."
                }

    elif action_type == "view_submission":
        # Processar submissao de modal
        view = payload.get("view", {})
        callback_id = view.get("callback_id", "")

        if callback_id == "create_story_modal":
            # TODO: Processar criacao de story via modal
            pass

    return Response(status_code=200)


@router.post("/notify")
async def send_notification(request: Request, notify_req: SlackNotifyRequest):
    """
    Envia notificacao manual para Slack.

    Permite enviar mensagens customizadas para canais.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.slack_integration import get_slack_integration

        slack = get_slack_integration(tenant_id)

        if not slack.is_connected:
            await slack.connect()

        result = await slack.client.send_message(
            channel=notify_req.channel or slack.config.default_channel,
            text=notify_req.message,
            blocks=notify_req.blocks,
            thread_ts=notify_req.thread_ts
        )

        if result:
            return {
                "success": True,
                "message": "Notificacao enviada",
                "channel": result.get("channel"),
                "ts": result.get("ts")
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao enviar notificacao"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao enviar notificacao: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/notify/story")
async def notify_story(request: Request, notify_req: SlackStoryNotifyRequest):
    """
    Envia notificacao sobre uma story.

    Busca a story e envia notificacao formatada.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.slack_integration import get_slack_integration
        from factory.database.repositories import StoryRepository
        from factory.database.connection import get_session

        # Buscar story
        with get_session() as session:
            repo = StoryRepository(session)
            story = repo.get_by_story_id(notify_req.story_id)

            if not story:
                raise HTTPException(
                    status_code=404,
                    detail=f"Story {notify_req.story_id} nao encontrada"
                )

            story_dict = story.to_dict() if hasattr(story, "to_dict") else {
                "story_id": story.story_id,
                "title": story.title,
                "status": story.status,
                "priority": getattr(story, "priority", "medium"),
                "description": getattr(story, "description", ""),
                "story_points": getattr(story, "story_points", None)
            }

        # Enviar notificacao
        slack = get_slack_integration(tenant_id)

        if not slack.is_connected:
            await slack.connect()

        if notify_req.action == "created":
            result = await slack.client.notify_story_created(
                story_dict,
                channel=notify_req.channel
            )
        else:
            result = await slack.client.notify_story_updated(
                story_dict,
                changes=notify_req.changes or {},
                channel=notify_req.channel
            )

        if result:
            return {
                "success": True,
                "message": f"Notificacao de {notify_req.action} enviada",
                "story_id": notify_req.story_id,
                "channel": result.get("channel"),
                "ts": result.get("ts")
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao enviar notificacao"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao notificar story: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/channels")
async def list_channels(request: Request):
    """
    Lista canais disponiveis no workspace.

    Retorna canais publicos e privados que o bot pode acessar.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.slack_integration import get_slack_integration

        slack = get_slack_integration(tenant_id)

        if not slack.is_connected:
            await slack.connect()

        channels = await slack.client.list_channels()

        return {
            "success": True,
            "channels": [
                {
                    "id": ch.get("id"),
                    "name": ch.get("name"),
                    "is_private": ch.get("is_private", False),
                    "is_member": ch.get("is_member", False),
                    "num_members": ch.get("num_members", 0)
                }
                for ch in channels
            ],
            "count": len(channels)
        }

    except Exception as e:
        logger.error(f"Erro ao listar canais: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/configure")
async def configure_slack(request: Request, config: SlackConfigRequest):
    """
    Configura integracao Slack para o tenant.

    Permite atualizar configuracoes sem necessidade de variaveis de ambiente.
    """
    tenant_id = get_tenant_id(request)

    try:
        # TODO: Salvar configuracao no banco de dados para o tenant

        return {
            "success": True,
            "message": "Configuracao atualizada",
            "tenant_id": tenant_id
        }

    except Exception as e:
        logger.error(f"Erro ao configurar Slack: {e}")
        raise HTTPException(status_code=500, detail=str(e))
