# -*- coding: utf-8 -*-
"""
Webhook Module
==============
Sistema completo de webhooks para integracoes.

Terminal 5 - Issue #303: Signature verification
Terminal A - Issue #363: Event processor e inbound routes

Supported Platforms:
- GitHub (HMAC-SHA256)
- Azure DevOps (Basic Auth / Custom Headers)
- Jira (JWT / HMAC)
- Salesforce (HMAC / Org ID / IP)

Usage Example:
    from factory.integrations.webhooks import (
        GitHubWebhookVerifier,
        AzureDevOpsWebhookVerifier,
        JiraWebhookVerifier,
        SalesforceWebhookVerifier,
        get_webhook_registry,
        WebhookPlatform
    )

    # Register verifiers
    registry = get_webhook_registry()

    # GitHub
    github_verifier = GitHubWebhookVerifier(secret="github_webhook_secret")
    registry.register(WebhookPlatform.GITHUB, "tenant-123", github_verifier)

    # Azure DevOps
    azure_verifier = AzureDevOpsWebhookVerifier(
        username="webhook",
        password="secret"
    )
    registry.register(WebhookPlatform.AZURE_DEVOPS, "tenant-123", azure_verifier)

    # In FastAPI endpoints
    @app.post("/webhook/github")
    async def github_webhook(request: Request):
        verifier = registry.get(WebhookPlatform.GITHUB, "tenant-123")
        if not await verifier.verify(request):
            raise HTTPException(status_code=401)
        # Process webhook...

    # Or use the dependency
    from factory.integrations.webhooks import require_webhook_signature

    @app.post("/webhook/github")
    async def github_webhook(
        request: Request,
        _: None = Depends(require_webhook_signature(WebhookPlatform.GITHUB))
    ):
        # Only reaches here if signature is valid
        payload = await request.json()
        ...
"""

# Base classes and utilities
from .signature_verifier import (
    WebhookConfig,
    WebhookPlatform,
    WebhookSignatureVerifier,
    WebhookVerifierRegistry,
    WebhookVerificationLog,
    VerificationResult,
    get_webhook_registry,
    verify_webhook_signature,
    require_webhook_signature,
)

# Platform-specific verifiers
from .github_webhook import (
    GitHubWebhookVerifier,
    GitHubEvents,
    create_github_verifier,
    parse_github_webhook,
)

from .azure_webhook import (
    AzureDevOpsWebhookVerifier,
    AzureDevOpsEvents,
    create_azure_devops_verifier,
    parse_azure_devops_webhook,
)

from .jira_webhook import (
    JiraWebhookVerifier,
    JiraEvents,
    create_jira_cloud_verifier,
    create_jira_server_verifier,
    parse_jira_webhook,
)

from .salesforce_webhook import (
    SalesforceWebhookVerifier,
    SalesforceEvents,
    create_salesforce_verifier,
    parse_salesforce_webhook,
    normalize_org_id,
)

# Event processor (Issue #363)
from .processor import (
    WebhookEvent,
    WebhookEventProcessor,
    WebhookSource,
    EventStatus,
    EVENT_MAPPINGS,
    get_processor,
    process_webhook_event,
)

# Inbound routes (Issue #363)
from .routes import router as inbound_webhook_router

__all__ = [
    # Base classes
    "WebhookConfig",
    "WebhookPlatform",
    "WebhookSignatureVerifier",
    "WebhookVerifierRegistry",
    "WebhookVerificationLog",
    "VerificationResult",

    # Registry and dependencies
    "get_webhook_registry",
    "verify_webhook_signature",
    "require_webhook_signature",

    # GitHub
    "GitHubWebhookVerifier",
    "GitHubEvents",
    "create_github_verifier",
    "parse_github_webhook",

    # Azure DevOps
    "AzureDevOpsWebhookVerifier",
    "AzureDevOpsEvents",
    "create_azure_devops_verifier",
    "parse_azure_devops_webhook",

    # Jira
    "JiraWebhookVerifier",
    "JiraEvents",
    "create_jira_cloud_verifier",
    "create_jira_server_verifier",
    "parse_jira_webhook",

    # Salesforce
    "SalesforceWebhookVerifier",
    "SalesforceEvents",
    "create_salesforce_verifier",
    "parse_salesforce_webhook",
    "normalize_org_id",

    # Event processor (Issue #363)
    "WebhookEvent",
    "WebhookEventProcessor",
    "WebhookSource",
    "EventStatus",
    "EVENT_MAPPINGS",
    "get_processor",
    "process_webhook_event",

    # Inbound routes (Issue #363)
    "inbound_webhook_router",
]
