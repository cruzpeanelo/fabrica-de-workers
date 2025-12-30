# -*- coding: utf-8 -*-
"""
Fixtures compartilhadas para testes de integrações
==================================================
Issue #326 - Terminal A
"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime


# ==================== CONFIGURAÇÕES DE TESTE ====================

@pytest.fixture
def tenant_id():
    """ID do tenant para testes"""
    return "test-tenant-001"


@pytest.fixture
def project_id():
    """ID do projeto para testes"""
    return "test-project-001"


# ==================== JIRA FIXTURES ====================

@pytest.fixture
def jira_config():
    """Configuração Jira para testes"""
    return {
        "url": "https://test.atlassian.net",
        "username": "test@example.com",
        "api_token": "test-token-123",
        "project_key": "TEST",
        "default_issue_type": "Story"
    }


@pytest.fixture
def mock_jira_issue():
    """Issue Jira mockada"""
    return {
        "key": "TEST-123",
        "id": "10001",
        "self": "https://test.atlassian.net/rest/api/3/issue/10001",
        "fields": {
            "summary": "Test Story Title",
            "description": "As a user, I want to test, so that I can verify",
            "status": {"name": "To Do", "id": "1"},
            "priority": {"name": "Medium", "id": "3"},
            "issuetype": {"name": "Story", "id": "10001"},
            "project": {"key": "TEST", "name": "Test Project"},
            "assignee": {"displayName": "Test User", "accountId": "123"},
            "reporter": {"displayName": "Reporter User", "accountId": "456"},
            "created": "2024-01-15T10:00:00.000+0000",
            "updated": "2024-01-15T12:00:00.000+0000",
            "labels": ["backend", "urgent"],
            "components": [],
            "fixVersions": []
        }
    }


@pytest.fixture
def mock_jira_board():
    """Board Jira mockado"""
    return {
        "id": 1,
        "name": "Test Board",
        "type": "scrum",
        "location": {
            "projectId": 10000,
            "projectKey": "TEST"
        }
    }


@pytest.fixture
def mock_jira_sprint():
    """Sprint Jira mockado"""
    return {
        "id": 1,
        "name": "Sprint 1",
        "state": "active",
        "startDate": "2024-01-01T00:00:00.000Z",
        "endDate": "2024-01-14T00:00:00.000Z",
        "originBoardId": 1
    }


# ==================== AZURE DEVOPS FIXTURES ====================

@pytest.fixture
def azure_devops_config():
    """Configuração Azure DevOps para testes"""
    return {
        "organization": "test-org",
        "project": "test-project",
        "pat": "test-pat-token",
        "api_version": "7.0"
    }


@pytest.fixture
def mock_azure_work_item():
    """Work Item Azure DevOps mockado"""
    return {
        "id": 123,
        "rev": 1,
        "fields": {
            "System.Title": "Test Work Item",
            "System.Description": "Test description",
            "System.State": "New",
            "System.WorkItemType": "User Story",
            "System.AssignedTo": {"displayName": "Test User"},
            "System.CreatedDate": "2024-01-15T10:00:00Z",
            "System.ChangedDate": "2024-01-15T12:00:00Z",
            "Microsoft.VSTS.Scheduling.StoryPoints": 5
        },
        "url": "https://dev.azure.com/test-org/test-project/_apis/wit/workItems/123"
    }


@pytest.fixture
def mock_azure_repo():
    """Repositório Azure DevOps mockado"""
    return {
        "id": "repo-123",
        "name": "test-repo",
        "url": "https://dev.azure.com/test-org/test-project/_git/test-repo",
        "defaultBranch": "refs/heads/main",
        "project": {"id": "proj-123", "name": "test-project"}
    }


@pytest.fixture
def mock_azure_pipeline():
    """Pipeline Azure DevOps mockado"""
    return {
        "id": 1,
        "name": "Build Pipeline",
        "folder": "\\",
        "revision": 1,
        "url": "https://dev.azure.com/test-org/test-project/_apis/pipelines/1"
    }


# ==================== SALESFORCE FIXTURES ====================

@pytest.fixture
def salesforce_config():
    """Configuração Salesforce para testes"""
    return {
        "instance_url": "https://test.salesforce.com",
        "client_id": "test-client-id",
        "client_secret": "test-client-secret",
        "username": "test@example.com",
        "password": "test-password",
        "security_token": "test-token"
    }


@pytest.fixture
def mock_salesforce_account():
    """Account Salesforce mockado"""
    return {
        "Id": "001000000000001",
        "Name": "Test Account",
        "Type": "Customer",
        "Industry": "Technology",
        "Website": "https://test.com",
        "CreatedDate": "2024-01-15T10:00:00.000+0000"
    }


@pytest.fixture
def mock_salesforce_opportunity():
    """Opportunity Salesforce mockada"""
    return {
        "Id": "006000000000001",
        "Name": "Test Opportunity",
        "StageName": "Prospecting",
        "Amount": 50000.00,
        "CloseDate": "2024-06-30",
        "AccountId": "001000000000001"
    }


# ==================== WEBHOOK FIXTURES ====================

@pytest.fixture
def webhook_secret():
    """Secret para verificação de webhooks"""
    return "test-webhook-secret-key"


@pytest.fixture
def jira_webhook_payload():
    """Payload de webhook Jira"""
    return {
        "webhookEvent": "jira:issue_updated",
        "timestamp": 1705320000000,
        "issue": {
            "key": "TEST-123",
            "fields": {
                "summary": "Updated Story",
                "status": {"name": "In Progress"}
            }
        },
        "changelog": {
            "items": [
                {
                    "field": "status",
                    "fromString": "To Do",
                    "toString": "In Progress"
                }
            ]
        }
    }


@pytest.fixture
def github_webhook_payload():
    """Payload de webhook GitHub"""
    return {
        "action": "opened",
        "number": 1,
        "pull_request": {
            "id": 123,
            "title": "Test PR",
            "body": "Test description",
            "state": "open",
            "head": {"ref": "feature-branch"},
            "base": {"ref": "main"}
        },
        "repository": {
            "id": 456,
            "name": "test-repo",
            "full_name": "test-org/test-repo"
        }
    }


# ==================== HTTP MOCK FIXTURES ====================

@pytest.fixture
def mock_http_response():
    """Factory para respostas HTTP mockadas"""
    def _create_response(status=200, json_data=None, text=""):
        response = AsyncMock()
        response.status = status
        response.status_code = status
        response.json = AsyncMock(return_value=json_data or {})
        response.text = AsyncMock(return_value=text)
        return response
    return _create_response


@pytest.fixture
def mock_aiohttp_session(mock_http_response):
    """Sessão aiohttp mockada"""
    session = AsyncMock()
    session.get = AsyncMock(return_value=mock_http_response())
    session.post = AsyncMock(return_value=mock_http_response())
    session.put = AsyncMock(return_value=mock_http_response())
    session.delete = AsyncMock(return_value=mock_http_response())
    session.patch = AsyncMock(return_value=mock_http_response())
    session.__aenter__ = AsyncMock(return_value=session)
    session.__aexit__ = AsyncMock(return_value=None)
    return session


# ==================== STORY FIXTURES ====================

@pytest.fixture
def mock_story():
    """Story local mockada"""
    story = MagicMock()
    story.story_id = "STR-001"
    story.tenant_id = "test-tenant-001"
    story.project_id = "test-project-001"
    story.title = "Test Story"
    story.description = "Test description"
    story.persona = "user"
    story.action = "test action"
    story.benefit = "test benefit"
    story.status = "backlog"
    story.priority = "medium"
    story.story_points = 5
    story.assignee = "test-user"
    story.tags = ["test"]
    story.acceptance_criteria = ["AC1", "AC2"]
    story.external_references = {}
    story.created_at = datetime(2024, 1, 15, 10, 0, 0)
    story.updated_at = datetime(2024, 1, 15, 12, 0, 0)
    return story


@pytest.fixture
def mock_story_repository():
    """Repository de stories mockado"""
    repo = AsyncMock()
    repo.get_by_id = AsyncMock(return_value=None)
    repo.get_by_project = AsyncMock(return_value=[])
    repo.create = AsyncMock()
    repo.update = AsyncMock()
    return repo
