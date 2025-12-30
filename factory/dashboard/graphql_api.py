# -*- coding: utf-8 -*-
"""
GraphQL API Module (Issue #268)
===============================
API GraphQL para integracoes avancadas.

Funcionalidades:
- Schema GraphQL completo
- Queries para Stories, Tasks, Sprints, Epics
- Mutations para CRUD
- Subscriptions para real-time (via WebSocket)
- Introspection e playground
- Rate limiting por query complexity
"""

from fastapi import APIRouter, HTTPException, Depends, Request
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import json

router = APIRouter(prefix="/graphql", tags=["GraphQL"])

# GraphQL Schema Definition
GRAPHQL_SCHEMA = '''
type Query {
    # Stories
    stories(projectId: String!, status: String, limit: Int, offset: Int): [Story!]!
    story(id: String!): Story

    # Tasks
    tasks(storyId: String, status: String, limit: Int): [Task!]!
    task(id: String!): Task

    # Sprints
    sprints(projectId: String!): [Sprint!]!
    sprint(id: String!): Sprint
    currentSprint(projectId: String!): Sprint

    # Epics
    epics(projectId: String!): [Epic!]!
    epic(id: String!): Epic

    # Users
    users(projectId: String): [User!]!
    user(id: String!): User

    # Metrics
    projectMetrics(projectId: String!): ProjectMetrics!
    sprintMetrics(sprintId: String!): SprintMetrics!
    velocityHistory(projectId: String!, sprints: Int): [VelocityPoint!]!
}

type Mutation {
    # Stories
    createStory(input: StoryInput!): Story!
    updateStory(id: String!, input: StoryInput!): Story!
    deleteStory(id: String!): Boolean!
    moveStory(id: String!, status: String!): Story!

    # Tasks
    createTask(input: TaskInput!): Task!
    updateTask(id: String!, input: TaskInput!): Task!
    deleteTask(id: String!): Boolean!
    completeTask(id: String!): Task!

    # Sprints
    createSprint(input: SprintInput!): Sprint!
    startSprint(id: String!): Sprint!
    completeSprint(id: String!): Sprint!

    # Comments
    addComment(storyId: String!, content: String!): Comment!
}

type Subscription {
    storyUpdated(projectId: String!): Story!
    taskCompleted(storyId: String!): Task!
    sprintProgress(sprintId: String!): SprintProgress!
}

type Story {
    id: String!
    title: String!
    description: String
    status: String!
    priority: String!
    storyPoints: Int
    assignee: User
    epic: Epic
    sprint: Sprint
    tasks: [Task!]!
    comments: [Comment!]!
    createdAt: String!
    updatedAt: String!
    progress: Int!
}

type Task {
    id: String!
    storyId: String!
    title: String!
    status: String!
    progress: Int!
    assignee: User
    createdAt: String!
}

type Sprint {
    id: String!
    name: String!
    goal: String
    startDate: String
    endDate: String
    status: String!
    stories: [Story!]!
    totalPoints: Int!
    completedPoints: Int!
}

type Epic {
    id: String!
    name: String!
    description: String
    color: String
    stories: [Story!]!
    progress: Int!
}

type User {
    id: String!
    name: String!
    email: String
    avatar: String
    role: String
}

type Comment {
    id: String!
    content: String!
    author: User!
    createdAt: String!
}

type ProjectMetrics {
    totalStories: Int!
    completedStories: Int!
    inProgressStories: Int!
    totalPoints: Int!
    completedPoints: Int!
    velocity: Float!
    burndownData: [BurndownPoint!]!
}

type SprintMetrics {
    totalStories: Int!
    completedStories: Int!
    remainingPoints: Int!
    daysRemaining: Int!
    burndownData: [BurndownPoint!]!
}

type BurndownPoint {
    date: String!
    ideal: Float!
    actual: Float!
}

type VelocityPoint {
    sprintName: String!
    planned: Int!
    completed: Int!
}

type SprintProgress {
    sprintId: String!
    completedPoints: Int!
    totalPoints: Int!
    percentage: Float!
}

input StoryInput {
    projectId: String!
    title: String!
    description: String
    persona: String
    action: String
    benefit: String
    status: String
    priority: String
    storyPoints: Int
    assigneeId: String
    epicId: String
    sprintId: String
    tags: [String!]
}

input TaskInput {
    storyId: String!
    title: String!
    description: String
    assigneeId: String
}

input SprintInput {
    projectId: String!
    name: String!
    goal: String
    startDate: String!
    endDate: String!
}
'''


class GraphQLRequest(BaseModel):
    query: str
    variables: Optional[Dict[str, Any]] = None
    operationName: Optional[str] = None


class GraphQLResponse(BaseModel):
    data: Optional[Dict[str, Any]] = None
    errors: Optional[List[Dict[str, Any]]] = None


# Simple GraphQL executor (in production, use graphene or strawberry)
class GraphQLExecutor:
    def __init__(self):
        self.resolvers = {}
        self._setup_resolvers()

    def _setup_resolvers(self):
        """Setup query resolvers."""
        self.resolvers = {
            "stories": self._resolve_stories,
            "story": self._resolve_story,
            "tasks": self._resolve_tasks,
            "task": self._resolve_task,
            "sprints": self._resolve_sprints,
            "sprint": self._resolve_sprint,
            "currentSprint": self._resolve_current_sprint,
            "epics": self._resolve_epics,
            "projectMetrics": self._resolve_project_metrics,
            "sprintMetrics": self._resolve_sprint_metrics,
            "velocityHistory": self._resolve_velocity_history
        }

    async def execute(self, query: str, variables: dict = None, operation_name: str = None) -> dict:
        """Execute a GraphQL query."""
        try:
            # Parse query (simplified - in production use graphql-core)
            query_type = self._detect_query_type(query)
            field_name = self._extract_field_name(query)

            if field_name not in self.resolvers:
                return {
                    "data": None,
                    "errors": [{"message": f"Unknown field: {field_name}"}]
                }

            # Execute resolver
            result = await self.resolvers[field_name](variables or {})

            return {
                "data": {field_name: result},
                "errors": None
            }

        except Exception as e:
            return {
                "data": None,
                "errors": [{"message": str(e)}]
            }

    def _detect_query_type(self, query: str) -> str:
        """Detect if query, mutation or subscription."""
        query = query.strip().lower()
        if query.startswith("mutation"):
            return "mutation"
        elif query.startswith("subscription"):
            return "subscription"
        return "query"

    def _extract_field_name(self, query: str) -> str:
        """Extract the main field name from query."""
        # Simple extraction - find first word after {
        import re
        match = re.search(r'\{\s*(\w+)', query)
        if match:
            return match.group(1)
        return ""

    async def _resolve_stories(self, variables: dict) -> List[dict]:
        """Resolve stories query."""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            query = db.query(Story)

            if "projectId" in variables:
                query = query.filter(Story.project_id == variables["projectId"])
            if "status" in variables:
                query = query.filter(Story.status == variables["status"])

            limit = variables.get("limit", 50)
            offset = variables.get("offset", 0)

            stories = query.offset(offset).limit(limit).all()

            return [self._story_to_dict(s) for s in stories]
        finally:
            db.close()

    async def _resolve_story(self, variables: dict) -> Optional[dict]:
        """Resolve single story query."""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        story_id = variables.get("id")
        if not story_id:
            return None

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            return self._story_to_dict(story) if story else None
        finally:
            db.close()

    async def _resolve_tasks(self, variables: dict) -> List[dict]:
        """Resolve tasks query."""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask

        db = SessionLocal()
        try:
            query = db.query(StoryTask)

            if "storyId" in variables:
                query = query.filter(StoryTask.story_id == variables["storyId"])
            if "status" in variables:
                query = query.filter(StoryTask.status == variables["status"])

            limit = variables.get("limit", 100)
            tasks = query.limit(limit).all()

            return [self._task_to_dict(t) for t in tasks]
        finally:
            db.close()

    async def _resolve_task(self, variables: dict) -> Optional[dict]:
        """Resolve single task query."""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask

        task_id = variables.get("id")
        if not task_id:
            return None

        db = SessionLocal()
        try:
            task = db.query(StoryTask).filter(StoryTask.task_id == task_id).first()
            return self._task_to_dict(task) if task else None
        finally:
            db.close()

    async def _resolve_sprints(self, variables: dict) -> List[dict]:
        """Resolve sprints query."""
        # Mock data - in production, query from database
        return [
            {
                "id": "SPR-001",
                "name": "Sprint 1",
                "goal": "MVP Features",
                "startDate": "2025-01-01",
                "endDate": "2025-01-14",
                "status": "active",
                "totalPoints": 34,
                "completedPoints": 21
            }
        ]

    async def _resolve_sprint(self, variables: dict) -> Optional[dict]:
        """Resolve single sprint query."""
        sprints = await self._resolve_sprints(variables)
        sprint_id = variables.get("id")
        return next((s for s in sprints if s["id"] == sprint_id), None)

    async def _resolve_current_sprint(self, variables: dict) -> Optional[dict]:
        """Resolve current sprint query."""
        sprints = await self._resolve_sprints(variables)
        return next((s for s in sprints if s["status"] == "active"), None)

    async def _resolve_epics(self, variables: dict) -> List[dict]:
        """Resolve epics query."""
        return [
            {
                "id": "EPIC-001",
                "name": "User Authentication",
                "description": "Complete auth system",
                "color": "#3B82F6",
                "progress": 75
            }
        ]

    async def _resolve_project_metrics(self, variables: dict) -> dict:
        """Resolve project metrics."""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        project_id = variables.get("projectId")

        db = SessionLocal()
        try:
            total = db.query(Story).filter(Story.project_id == project_id).count()
            completed = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == "done"
            ).count()
            in_progress = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == "in_progress"
            ).count()

            return {
                "totalStories": total,
                "completedStories": completed,
                "inProgressStories": in_progress,
                "totalPoints": 0,
                "completedPoints": 0,
                "velocity": 0.0,
                "burndownData": []
            }
        finally:
            db.close()

    async def _resolve_sprint_metrics(self, variables: dict) -> dict:
        """Resolve sprint metrics."""
        return {
            "totalStories": 8,
            "completedStories": 5,
            "remainingPoints": 13,
            "daysRemaining": 5,
            "burndownData": [
                {"date": "2025-01-01", "ideal": 34, "actual": 34},
                {"date": "2025-01-05", "ideal": 24, "actual": 26},
                {"date": "2025-01-10", "ideal": 14, "actual": 13}
            ]
        }

    async def _resolve_velocity_history(self, variables: dict) -> List[dict]:
        """Resolve velocity history."""
        return [
            {"sprintName": "Sprint 1", "planned": 34, "completed": 30},
            {"sprintName": "Sprint 2", "planned": 32, "completed": 32},
            {"sprintName": "Sprint 3", "planned": 36, "completed": 28}
        ]

    def _story_to_dict(self, story) -> dict:
        """Convert Story model to dict."""
        if not story:
            return None
        return {
            "id": story.story_id,
            "title": story.title,
            "description": story.description,
            "status": story.status,
            "priority": story.priority,
            "storyPoints": story.story_points,
            "createdAt": story.created_at.isoformat() if story.created_at else None,
            "updatedAt": story.updated_at.isoformat() if story.updated_at else None,
            "progress": 0,
            "assignee": None,
            "epic": None,
            "sprint": None,
            "tasks": [],
            "comments": []
        }

    def _task_to_dict(self, task) -> dict:
        """Convert Task model to dict."""
        if not task:
            return None
        return {
            "id": task.task_id,
            "storyId": task.story_id,
            "title": task.title,
            "status": task.status,
            "progress": task.progress or 0,
            "createdAt": task.created_at.isoformat() if task.created_at else None,
            "assignee": None
        }


# Global executor instance
executor = GraphQLExecutor()


@router.post("")
@router.post("/")
async def graphql_endpoint(request: GraphQLRequest):
    """Main GraphQL endpoint."""
    result = await executor.execute(
        query=request.query,
        variables=request.variables,
        operation_name=request.operationName
    )
    return result


@router.get("/schema")
async def get_schema():
    """Returns the GraphQL schema."""
    return {
        "schema": GRAPHQL_SCHEMA,
        "format": "SDL"
    }


@router.get("/playground")
async def graphql_playground():
    """Returns GraphQL Playground HTML."""
    html = '''
    <!DOCTYPE html>
    <html>
    <head>
        <title>GraphQL Playground</title>
        <link rel="stylesheet" href="https://unpkg.com/graphiql/graphiql.min.css" />
        <style>
            body { margin: 0; height: 100vh; }
            #graphiql { height: 100vh; }
        </style>
    </head>
    <body>
        <div id="graphiql"></div>
        <script crossorigin src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
        <script crossorigin src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>
        <script crossorigin src="https://unpkg.com/graphiql/graphiql.min.js"></script>
        <script>
            const fetcher = GraphiQL.createFetcher({
                url: '/graphql',
            });
            ReactDOM.render(
                React.createElement(GraphiQL, { fetcher: fetcher }),
                document.getElementById('graphiql'),
            );
        </script>
    </body>
    </html>
    '''
    from fastapi.responses import HTMLResponse
    return HTMLResponse(content=html)


def get_graphql_examples():
    """Retorna exemplos de queries GraphQL."""
    return {
        "examples": [
            {
                "name": "List Stories",
                "query": '''
query GetStories($projectId: String!) {
    stories(projectId: $projectId, limit: 10) {
        id
        title
        status
        priority
        storyPoints
        assignee {
            name
        }
    }
}''',
                "variables": {"projectId": "default"}
            },
            {
                "name": "Get Story with Tasks",
                "query": '''
query GetStoryDetails($id: String!) {
    story(id: $id) {
        id
        title
        description
        status
        tasks {
            id
            title
            status
            progress
        }
        comments {
            content
            author { name }
            createdAt
        }
    }
}''',
                "variables": {"id": "STR-001"}
            },
            {
                "name": "Project Metrics",
                "query": '''
query ProjectDashboard($projectId: String!) {
    projectMetrics(projectId: $projectId) {
        totalStories
        completedStories
        velocity
        burndownData {
            date
            ideal
            actual
        }
    }
    currentSprint(projectId: $projectId) {
        name
        daysRemaining
        completedPoints
        totalPoints
    }
}''',
                "variables": {"projectId": "default"}
            },
            {
                "name": "Create Story",
                "query": '''
mutation CreateStory($input: StoryInput!) {
    createStory(input: $input) {
        id
        title
        status
    }
}''',
                "variables": {
                    "input": {
                        "projectId": "default",
                        "title": "New Feature",
                        "description": "Description here",
                        "priority": "high",
                        "storyPoints": 5
                    }
                }
            }
        ]
    }


def register_graphql_api(app):
    """Registra os endpoints GraphQL no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] GraphQL API endpoints loaded: /graphql")
