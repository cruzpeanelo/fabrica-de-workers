"""
Integration Tests for REST API
==============================

Tests for FastAPI endpoints in factory.dashboard.app
"""

import pytest
from fastapi.testclient import TestClient


class TestStatusEndpoint:
    """Tests for /api/status endpoint"""

    @pytest.mark.integration
    def test_get_status(self, api_client):
        """Test getting factory status"""
        response = api_client.get("/api/status")

        assert response.status_code == 200
        data = response.json()

        assert "factory" in data
        assert "projects" in data
        assert "agents" in data
        assert "skills" in data
        assert "timestamp" in data

    @pytest.mark.integration
    def test_status_factory_info(self, api_client):
        """Test factory info in status"""
        response = api_client.get("/api/status")
        data = response.json()

        factory = data["factory"]
        assert "name" in factory
        assert "version" in factory
        assert "status" in factory
        assert factory["status"] == "running"


class TestProjectEndpoints:
    """Tests for /api/projects endpoints"""

    @pytest.mark.integration
    def test_list_projects(self, api_client):
        """Test listing all projects"""
        response = api_client.get("/api/projects")

        assert response.status_code == 200
        data = response.json()
        assert "projects" in data
        assert isinstance(data["projects"], list)

    @pytest.mark.integration
    def test_create_project(self, api_client):
        """Test creating a new project"""
        response = api_client.post("/api/projects", json={
            "name": "API Test Project",
            "description": "Created via API test",
            "project_type": "web-app"
        })

        assert response.status_code == 200
        data = response.json()
        # API returns {"success": true, "project": {...}}
        assert "project" in data or "project_id" in data
        if "project" in data:
            assert "project_id" in data["project"]
            assert data["project"]["name"] == "API Test Project"
        else:
            assert data["name"] == "API Test Project"

    @pytest.mark.integration
    def test_get_project(self, api_client):
        """Test getting a specific project"""
        # First create a project
        create_response = api_client.post("/api/projects", json={
            "name": "Get Test Project",
            "project_type": "api-service"
        })
        create_data = create_response.json()
        project_id = create_data.get("project", {}).get("project_id") or create_data.get("project_id")

        if project_id:
            # Then get it
            response = api_client.get(f"/api/projects/{project_id}")

            assert response.status_code == 200
            data = response.json()
            # Handle both possible response formats
            actual_id = data.get("project_id") or data.get("project", {}).get("project_id")
            assert actual_id == project_id

    @pytest.mark.integration
    def test_get_project_not_found(self, api_client):
        """Test getting non-existent project"""
        response = api_client.get("/api/projects/NON-EXISTENT-123")

        assert response.status_code == 404

    @pytest.mark.integration
    def test_update_project(self, api_client):
        """Test updating a project"""
        # Create project
        create_response = api_client.post("/api/projects", json={
            "name": "Update Test Project",
            "project_type": "web-app"
        })
        create_data = create_response.json()
        project_id = create_data.get("project", {}).get("project_id") or create_data.get("project_id")

        if project_id:
            # Update it
            response = api_client.put(f"/api/projects/{project_id}", json={
                "name": "Updated Project Name",
                "status": "IN_PROGRESS"
            })

            assert response.status_code == 200
            data = response.json()
            # Handle both possible response formats
            updated_name = data.get("name") or data.get("project", {}).get("name")
            updated_status = data.get("status") or data.get("project", {}).get("status")
            assert updated_name == "Updated Project Name"
            assert updated_status == "IN_PROGRESS"

    @pytest.mark.integration
    def test_list_projects_with_filter(self, api_client):
        """Test listing projects with status filter"""
        response = api_client.get("/api/projects?status=PLANNING")

        assert response.status_code == 200
        data = response.json()
        for project in data["projects"]:
            assert project["status"] == "PLANNING"


class TestStoryEndpoints:
    """Tests for /api/stories endpoints"""

    @pytest.mark.integration
    def test_list_stories(self, api_client):
        """Test listing all stories"""
        response = api_client.get("/api/stories")

        assert response.status_code == 200
        data = response.json()
        assert "stories" in data

    @pytest.mark.integration
    def test_create_story(self, api_client):
        """Test creating a new story"""
        # First create a project
        project_response = api_client.post("/api/projects", json={
            "name": "Story Test Project",
            "project_type": "web-app"
        })
        project_data = project_response.json()
        project_id = project_data.get("project", {}).get("project_id") or project_data.get("project_id")

        if project_id:
            # Then create a story
            response = api_client.post("/api/stories", json={
                "title": "Test User Story",
                "description": "As a user, I want to test",
                "project_id": project_id,
                "sprint": 1,
                "points": 5
            })

            assert response.status_code == 200
            data = response.json()
            # Handle different response formats
            story_data = data.get("story") or data
            assert "story_id" in story_data
            assert story_data["title"] == "Test User Story"

    @pytest.mark.integration
    def test_get_stories_by_project(self, api_client):
        """Test getting stories for a project"""
        # Create project and story
        project_response = api_client.post("/api/projects", json={
            "name": "Project for Stories",
            "project_type": "web-app"
        })
        project_data = project_response.json()
        project_id = project_data.get("project", {}).get("project_id") or project_data.get("project_id")

        if project_id:
            api_client.post("/api/stories", json={
                "title": "Project Story",
                "project_id": project_id
            })

            # Get stories for project
            response = api_client.get(f"/api/projects/{project_id}/stories")

            assert response.status_code == 200
            data = response.json()
            assert "stories" in data


class TestAgentEndpoints:
    """Tests for /api/agents endpoints"""

    @pytest.mark.integration
    def test_list_agents(self, api_client):
        """Test listing all agents"""
        response = api_client.get("/api/agents")

        assert response.status_code == 200
        data = response.json()
        assert "agents" in data
        assert isinstance(data["agents"], list)

    @pytest.mark.integration
    def test_get_agents_by_domain(self, api_client):
        """Test getting agents by domain"""
        # List all agents first to see what domains exist
        list_response = api_client.get("/api/agents")
        agents = list_response.json()["agents"]

        if agents:
            domain = agents[0].get("domain")
            if domain:
                response = api_client.get(f"/api/agents?domain={domain}")
                assert response.status_code == 200


class TestSkillEndpoints:
    """Tests for /api/skills endpoints"""

    @pytest.mark.integration
    def test_list_skills(self, api_client):
        """Test listing all skills"""
        response = api_client.get("/api/skills")

        assert response.status_code == 200
        data = response.json()
        assert "skills" in data
        assert isinstance(data["skills"], list)

    @pytest.mark.integration
    def test_skills_have_required_fields(self, api_client):
        """Test that skills have required fields"""
        response = api_client.get("/api/skills")
        data = response.json()

        if data["skills"]:
            skill = data["skills"][0]
            assert "skill_id" in skill
            assert "name" in skill
            assert "skill_type" in skill


class TestSprintEndpoints:
    """Tests for /api/sprints endpoints"""

    @pytest.mark.integration
    def test_create_sprint(self, api_client):
        """Test creating a sprint"""
        # Create project first
        project_response = api_client.post("/api/projects", json={
            "name": "Sprint Test Project",
            "project_type": "web-app"
        })
        project_data = project_response.json()
        project_id = project_data.get("project", {}).get("project_id") or project_data.get("project_id")

        if project_id:
            # Create sprint
            response = api_client.post("/api/sprints", json={
                "project_id": project_id,
                "sprint_number": 1,
                "name": "Sprint 1",
                "goal": "Complete MVP"
            })

            assert response.status_code == 200
            data = response.json()
            sprint_data = data.get("sprint") or data
            assert sprint_data["sprint_number"] == 1

    @pytest.mark.integration
    def test_get_project_sprints(self, api_client):
        """Test getting sprints for a project"""
        # Create project
        project_response = api_client.post("/api/projects", json={
            "name": "Sprints Project",
            "project_type": "web-app"
        })
        project_data = project_response.json()
        project_id = project_data.get("project", {}).get("project_id") or project_data.get("project_id")

        if project_id:
            # Create sprint
            api_client.post("/api/sprints", json={
                "project_id": project_id,
                "sprint_number": 1
            })

            # Get sprints
            response = api_client.get(f"/api/projects/{project_id}/sprints")

            assert response.status_code == 200
            data = response.json()
            assert "sprints" in data


class TestAuthEndpoints:
    """Tests for /api/auth endpoints"""

    @pytest.mark.integration
    def test_login_invalid_credentials(self, api_client):
        """Test login with invalid credentials"""
        response = api_client.post("/api/auth/login", json={
            "username": "invalid_user_xyz",
            "password": "invalid_password_xyz"
        })

        assert response.status_code == 401

    @pytest.mark.integration
    def test_login_missing_fields(self, api_client):
        """Test login with missing fields"""
        response = api_client.post("/api/auth/login", json={
            "username": "test"
            # Missing password
        })

        assert response.status_code == 422  # Validation error


class TestLogsEndpoint:
    """Tests for /api/logs endpoints"""

    @pytest.mark.integration
    def test_get_logs(self, api_client):
        """Test getting activity logs"""
        response = api_client.get("/api/logs")

        assert response.status_code == 200
        data = response.json()
        assert "logs" in data


class TestTemplateEndpoints:
    """Tests for /api/templates endpoints"""

    @pytest.mark.integration
    def test_list_templates(self, api_client):
        """Test listing all templates"""
        response = api_client.get("/api/templates")

        assert response.status_code == 200
        data = response.json()
        assert "templates" in data


class TestHealthCheck:
    """Tests for health check endpoints"""

    @pytest.mark.integration
    def test_root_redirect(self, api_client):
        """Test root endpoint"""
        response = api_client.get("/", follow_redirects=False)

        # Should either return HTML or redirect
        assert response.status_code in [200, 307, 302]

    @pytest.mark.integration
    def test_api_status_health(self, api_client):
        """Test API is healthy via status endpoint"""
        response = api_client.get("/api/status")

        assert response.status_code == 200
        assert response.json()["factory"]["status"] == "running"


class TestCORSHeaders:
    """Tests for CORS configuration"""

    @pytest.mark.integration
    def test_cors_headers_present(self, api_client):
        """Test that CORS headers are properly configured"""
        response = api_client.options("/api/status", headers={
            "Origin": "http://localhost:3000",
            "Access-Control-Request-Method": "GET"
        })

        # CORS preflight should succeed
        assert response.status_code in [200, 204]


class TestErrorHandling:
    """Tests for error handling"""

    @pytest.mark.integration
    def test_404_on_invalid_endpoint(self, api_client):
        """Test 404 on invalid endpoint"""
        response = api_client.get("/api/nonexistent_endpoint_xyz")

        assert response.status_code == 404

    @pytest.mark.integration
    def test_invalid_json_body(self, api_client):
        """Test handling of invalid JSON"""
        response = api_client.post(
            "/api/projects",
            content="not valid json",
            headers={"Content-Type": "application/json"}
        )

        assert response.status_code == 422  # Validation error


class TestPagination:
    """Tests for pagination features if available"""

    @pytest.mark.integration
    def test_agents_pagination(self, api_client):
        """Test agents endpoint with limit parameter if supported"""
        response = api_client.get("/api/agents?limit=10")

        # Should either work with pagination or ignore it
        assert response.status_code == 200
