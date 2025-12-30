# -*- coding: utf-8 -*-
"""
Jira Agile Integration Module
=============================
Integracao com Jira Agile REST API (/rest/agile/1.0/).

Funcionalidades:
- Boards: Listar e buscar boards
- Sprints: CRUD completo de sprints
- Epics: Buscar epics e issues de epics
- Backlog: Gerenciar backlog e mover issues
- Velocity: Metricas de velocidade do time

Configuracao via variaveis de ambiente (mesmas do jira.py):
- JIRA_URL: URL base do Jira (ex: https://empresa.atlassian.net)
- JIRA_EMAIL: Email do usuario
- JIRA_API_TOKEN: Token de API
- JIRA_PROJECT_KEY: Chave do projeto padrao (ex: PROJ)
- JIRA_TENANT_ID: ID do tenant

Issue #311 - Implement Jira Agile REST API methods.
"""

import logging
from typing import Any, Dict, List, Optional

from .jira_integration import JiraIntegration, get_jira_integration
from .jira.config import JiraConfig

logger = logging.getLogger(__name__)


class JiraAgileIntegration(JiraIntegration):
    """
    Extensao do JiraIntegration com suporte a Jira Agile REST API.

    A API Agile usa o endpoint base /rest/agile/1.0/ em vez de /rest/api/3/.

    Exemplo de uso:
    ```python
    config = JiraConfig.from_env()
    jira = JiraAgileIntegration(config)

    if await jira.connect():
        # Listar boards do projeto
        boards = await jira.get_boards("PROJ")

        # Listar sprints de um board
        sprints = await jira.get_sprints(board_id=123, state="active")

        # Buscar issues de um sprint
        issues = await jira.get_sprint_issues(sprint_id=456)

        # Mover issues para sprint
        await jira.move_to_sprint(["PROJ-1", "PROJ-2"], sprint_id=456)
    ```
    """

    AGILE_API_VERSION = "1.0"

    @property
    def agile_base_url(self) -> str:
        """URL base da API Agile"""
        url = self.config.base_url.rstrip("/")
        return f"{url}/rest/agile/{self.AGILE_API_VERSION}"

    # =========================================================================
    # Boards
    # =========================================================================

    async def get_boards(
        self,
        project_key: Optional[str] = None,
        board_type: Optional[str] = None,
        name: Optional[str] = None,
        start_at: int = 0,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista boards disponiveis no Jira.

        Args:
            project_key: Filtrar por chave do projeto (ex: PROJ)
            board_type: Filtrar por tipo (scrum, kanban, simple)
            name: Filtrar por nome (contém)
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina

        Returns:
            List[Dict]: Lista de boards com id, name, type, etc.

        Example:
            ```python
            # Listar todos os boards do projeto
            boards = await jira.get_boards("PROJ")

            # Listar apenas boards Scrum
            scrum_boards = await jira.get_boards(board_type="scrum")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board"

            params = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if project_key:
                params["projectKeyOrId"] = project_key
            if board_type:
                params["type"] = board_type
            if name:
                params["name"] = name

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    boards = data.get("values", [])
                    logger.debug(f"Encontrados {len(boards)} boards")
                    return boards
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar boards: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar boards: {e}")

        return []

    async def get_board(self, board_id: int) -> Optional[Dict]:
        """
        Busca um board especifico pelo ID.

        Args:
            board_id: ID do board

        Returns:
            Dict com informacoes do board ou None se nao encontrado

        Example:
            ```python
            board = await jira.get_board(123)
            print(f"Board: {board['name']} ({board['type']})")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    board = await response.json()
                    logger.debug(f"Board encontrado: {board.get('name')}")
                    return board
                elif response.status == 404:
                    logger.warning(f"Board nao encontrado: {board_id}")
                else:
                    error = await response.text()
                    logger.error(f"Erro ao buscar board {board_id}: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao buscar board {board_id}: {e}")

        return None

    # =========================================================================
    # Sprints
    # =========================================================================

    async def get_sprints(
        self,
        board_id: int,
        state: Optional[str] = None,
        start_at: int = 0,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista sprints de um board.

        Args:
            board_id: ID do board
            state: Filtrar por estado (future, active, closed)
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina

        Returns:
            List[Dict]: Lista de sprints com id, name, state, startDate, endDate, etc.

        Example:
            ```python
            # Listar todos os sprints
            all_sprints = await jira.get_sprints(board_id=123)

            # Listar apenas sprint ativo
            active = await jira.get_sprints(board_id=123, state="active")

            # Listar sprints fechados
            closed = await jira.get_sprints(board_id=123, state="closed")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/sprint"

            params = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if state:
                params["state"] = state

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    sprints = data.get("values", [])
                    logger.debug(f"Encontrados {len(sprints)} sprints no board {board_id}")
                    return sprints
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar sprints: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar sprints do board {board_id}: {e}")

        return []

    async def get_sprint(self, sprint_id: int) -> Optional[Dict]:
        """
        Busca um sprint especifico pelo ID.

        Args:
            sprint_id: ID do sprint

        Returns:
            Dict com informacoes do sprint ou None se nao encontrado

        Example:
            ```python
            sprint = await jira.get_sprint(456)
            print(f"Sprint: {sprint['name']} ({sprint['state']})")
            print(f"Inicio: {sprint.get('startDate')}")
            print(f"Fim: {sprint.get('endDate')}")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    sprint = await response.json()
                    logger.debug(f"Sprint encontrado: {sprint.get('name')}")
                    return sprint
                elif response.status == 404:
                    logger.warning(f"Sprint nao encontrado: {sprint_id}")
                else:
                    error = await response.text()
                    logger.error(f"Erro ao buscar sprint {sprint_id}: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao buscar sprint {sprint_id}: {e}")

        return None

    async def create_sprint(
        self,
        board_id: int,
        name: str,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None,
        goal: Optional[str] = None
    ) -> Optional[Dict]:
        """
        Cria um novo sprint em um board.

        Args:
            board_id: ID do board onde criar o sprint
            name: Nome do sprint
            start_date: Data de inicio (formato ISO: 2024-01-15T09:00:00.000Z)
            end_date: Data de fim (formato ISO: 2024-01-29T17:00:00.000Z)
            goal: Objetivo do sprint (opcional)

        Returns:
            Dict com o sprint criado ou None em caso de erro

        Example:
            ```python
            # Criar sprint simples
            sprint = await jira.create_sprint(
                board_id=123,
                name="Sprint 10"
            )

            # Criar sprint com datas e goal
            sprint = await jira.create_sprint(
                board_id=123,
                name="Sprint 10",
                start_date="2024-01-15T09:00:00.000Z",
                end_date="2024-01-29T17:00:00.000Z",
                goal="Implementar autenticacao OAuth"
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint"

            data: Dict[str, Any] = {
                "originBoardId": board_id,
                "name": name
            }

            if start_date:
                data["startDate"] = start_date
            if end_date:
                data["endDate"] = end_date
            if goal:
                data["goal"] = goal

            async with session.post(url, json=data) as response:
                if response.status == 201:
                    sprint = await response.json()
                    logger.info(f"Sprint criado: {sprint.get('name')} (ID: {sprint.get('id')})")
                    return sprint
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar sprint: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao criar sprint: {e}")

        return None

    async def update_sprint(
        self,
        sprint_id: int,
        data: Dict[str, Any]
    ) -> Optional[Dict]:
        """
        Atualiza um sprint existente.

        Args:
            sprint_id: ID do sprint
            data: Dados para atualizar. Campos suportados:
                - name: Nome do sprint
                - state: Estado (future, active, closed)
                - startDate: Data de inicio
                - endDate: Data de fim
                - goal: Objetivo do sprint
                - completeDate: Data de conclusao (apenas para closed)

        Returns:
            Dict com o sprint atualizado ou None em caso de erro

        Example:
            ```python
            # Iniciar sprint
            sprint = await jira.update_sprint(
                sprint_id=456,
                data={
                    "state": "active",
                    "startDate": "2024-01-15T09:00:00.000Z"
                }
            )

            # Fechar sprint
            sprint = await jira.update_sprint(
                sprint_id=456,
                data={
                    "state": "closed",
                    "completeDate": "2024-01-29T17:00:00.000Z"
                }
            )

            # Renomear sprint
            sprint = await jira.update_sprint(
                sprint_id=456,
                data={"name": "Sprint 10 - Corrigido"}
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            async with session.post(url, json=data) as response:
                if response.status == 200:
                    sprint = await response.json()
                    logger.info(f"Sprint atualizado: {sprint.get('name')}")
                    return sprint
                else:
                    error = await response.text()
                    logger.error(f"Erro ao atualizar sprint {sprint_id}: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao atualizar sprint {sprint_id}: {e}")

        return None

    async def get_sprint_issues(
        self,
        sprint_id: int,
        jql: Optional[str] = None,
        start_at: int = 0,
        max_results: int = 50,
        fields: Optional[List[str]] = None
    ) -> List[Dict]:
        """
        Lista issues de um sprint.

        Args:
            sprint_id: ID do sprint
            jql: JQL adicional para filtrar issues
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina
            fields: Lista de campos a retornar (ex: ["summary", "status"])

        Returns:
            List[Dict]: Lista de issues do sprint

        Example:
            ```python
            # Buscar todas as issues do sprint
            issues = await jira.get_sprint_issues(sprint_id=456)

            # Filtrar apenas bugs
            bugs = await jira.get_sprint_issues(
                sprint_id=456,
                jql="issuetype = Bug"
            )

            # Buscar com campos especificos
            issues = await jira.get_sprint_issues(
                sprint_id=456,
                fields=["summary", "status", "assignee"]
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"

            params: Dict[str, Any] = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if jql:
                params["jql"] = jql
            if fields:
                params["fields"] = ",".join(fields)

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    issues = data.get("issues", [])
                    logger.debug(f"Encontradas {len(issues)} issues no sprint {sprint_id}")
                    return issues
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar issues do sprint: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar issues do sprint {sprint_id}: {e}")

        return []

    # =========================================================================
    # Epics
    # =========================================================================

    async def get_epics(
        self,
        board_id: int,
        done: Optional[bool] = None,
        start_at: int = 0,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista epics de um board.

        Args:
            board_id: ID do board
            done: Filtrar por epics concluidos (True) ou nao concluidos (False)
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina

        Returns:
            List[Dict]: Lista de epics com id, key, name, summary, done

        Example:
            ```python
            # Listar todos os epics
            epics = await jira.get_epics(board_id=123)

            # Listar apenas epics em andamento
            in_progress = await jira.get_epics(board_id=123, done=False)

            # Listar epics concluidos
            completed = await jira.get_epics(board_id=123, done=True)
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/epic"

            params: Dict[str, Any] = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if done is not None:
                params["done"] = str(done).lower()

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    epics = data.get("values", [])
                    logger.debug(f"Encontrados {len(epics)} epics no board {board_id}")
                    return epics
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar epics: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar epics do board {board_id}: {e}")

        return []

    async def get_epic(self, epic_id: str) -> Optional[Dict]:
        """
        Busca um epic especifico pelo ID ou key.

        Args:
            epic_id: ID numerico ou key do epic (ex: "PROJ-100" ou "12345")

        Returns:
            Dict com informacoes do epic ou None se nao encontrado

        Example:
            ```python
            # Buscar por key
            epic = await jira.get_epic("PROJ-100")

            # Buscar por ID
            epic = await jira.get_epic("12345")

            print(f"Epic: {epic['name']}")
            print(f"Done: {epic['done']}")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/epic/{epic_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    epic = await response.json()
                    logger.debug(f"Epic encontrado: {epic.get('name')}")
                    return epic
                elif response.status == 404:
                    logger.warning(f"Epic nao encontrado: {epic_id}")
                else:
                    error = await response.text()
                    logger.error(f"Erro ao buscar epic {epic_id}: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao buscar epic {epic_id}: {e}")

        return None

    async def get_epic_issues(
        self,
        epic_id: str,
        jql: Optional[str] = None,
        start_at: int = 0,
        max_results: int = 50,
        fields: Optional[List[str]] = None
    ) -> List[Dict]:
        """
        Lista issues de um epic.

        Args:
            epic_id: ID numerico ou key do epic (ex: "PROJ-100")
            jql: JQL adicional para filtrar issues
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina
            fields: Lista de campos a retornar

        Returns:
            List[Dict]: Lista de issues do epic

        Example:
            ```python
            # Buscar todas as issues do epic
            issues = await jira.get_epic_issues("PROJ-100")

            # Filtrar por status
            in_progress = await jira.get_epic_issues(
                "PROJ-100",
                jql="status = 'In Progress'"
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/epic/{epic_id}/issue"

            params: Dict[str, Any] = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if jql:
                params["jql"] = jql
            if fields:
                params["fields"] = ",".join(fields)

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    issues = data.get("issues", [])
                    logger.debug(f"Encontradas {len(issues)} issues no epic {epic_id}")
                    return issues
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar issues do epic: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar issues do epic {epic_id}: {e}")

        return []

    # =========================================================================
    # Backlog
    # =========================================================================

    async def get_backlog(
        self,
        board_id: int,
        jql: Optional[str] = None,
        start_at: int = 0,
        max_results: int = 50,
        fields: Optional[List[str]] = None
    ) -> List[Dict]:
        """
        Lista issues do backlog de um board (issues sem sprint).

        Args:
            board_id: ID do board
            jql: JQL adicional para filtrar issues
            start_at: Offset para paginacao
            max_results: Maximo de resultados por pagina
            fields: Lista de campos a retornar

        Returns:
            List[Dict]: Lista de issues no backlog

        Example:
            ```python
            # Buscar todo o backlog
            backlog = await jira.get_backlog(board_id=123)

            # Filtrar bugs no backlog
            bugs = await jira.get_backlog(
                board_id=123,
                jql="issuetype = Bug"
            )

            # Ordenar por prioridade
            prioritized = await jira.get_backlog(
                board_id=123,
                jql="ORDER BY priority DESC"
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/backlog"

            params: Dict[str, Any] = {
                "startAt": start_at,
                "maxResults": max_results
            }

            if jql:
                params["jql"] = jql
            if fields:
                params["fields"] = ",".join(fields)

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    issues = data.get("issues", [])
                    logger.debug(f"Encontradas {len(issues)} issues no backlog do board {board_id}")
                    return issues
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar backlog: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao listar backlog do board {board_id}: {e}")

        return []

    async def move_to_sprint(
        self,
        issue_keys: List[str],
        sprint_id: int,
        rank_before: Optional[str] = None,
        rank_after: Optional[str] = None
    ) -> bool:
        """
        Move issues para um sprint.

        Args:
            issue_keys: Lista de chaves de issues (ex: ["PROJ-1", "PROJ-2"])
            sprint_id: ID do sprint destino
            rank_before: Issue key antes da qual ranquear (opcional)
            rank_after: Issue key depois da qual ranquear (opcional)

        Returns:
            bool: True se movidas com sucesso

        Example:
            ```python
            # Mover issues para sprint
            success = await jira.move_to_sprint(
                issue_keys=["PROJ-1", "PROJ-2", "PROJ-3"],
                sprint_id=456
            )

            # Mover e posicionar no topo
            success = await jira.move_to_sprint(
                issue_keys=["PROJ-5"],
                sprint_id=456,
                rank_before="PROJ-1"  # Colocar antes de PROJ-1
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return False

        if not issue_keys:
            logger.warning("Lista de issues vazia")
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"

            data: Dict[str, Any] = {
                "issues": issue_keys
            }

            if rank_before:
                data["rankBeforeIssue"] = rank_before
            if rank_after:
                data["rankAfterIssue"] = rank_after

            async with session.post(url, json=data) as response:
                if response.status == 204:
                    logger.info(f"Issues movidas para sprint {sprint_id}: {issue_keys}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao mover issues para sprint: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao mover issues para sprint {sprint_id}: {e}")

        return False

    async def move_to_backlog(self, issue_keys: List[str]) -> bool:
        """
        Move issues para o backlog (remove do sprint atual).

        Args:
            issue_keys: Lista de chaves de issues (ex: ["PROJ-1", "PROJ-2"])

        Returns:
            bool: True se movidas com sucesso

        Example:
            ```python
            # Mover issues de volta para backlog
            success = await jira.move_to_backlog(
                issue_keys=["PROJ-1", "PROJ-2"]
            )
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return False

        if not issue_keys:
            logger.warning("Lista de issues vazia")
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/backlog/issue"

            data = {
                "issues": issue_keys
            }

            async with session.post(url, json=data) as response:
                if response.status == 204:
                    logger.info(f"Issues movidas para backlog: {issue_keys}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao mover issues para backlog: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao mover issues para backlog: {e}")

        return False

    # =========================================================================
    # Velocity
    # =========================================================================

    async def get_velocity(
        self,
        board_id: int
    ) -> Optional[Dict]:
        """
        Obtem dados de velocidade de um board Scrum.

        A velocidade e calculada com base nos story points completados
        nos sprints anteriores.

        Args:
            board_id: ID do board (deve ser Scrum)

        Returns:
            Dict com dados de velocidade ou None em caso de erro.
            Formato retornado:
            {
                "sprints": [
                    {
                        "id": 1,
                        "name": "Sprint 1",
                        "state": "closed",
                        "estimated": {"value": 20.0, "text": "20"},
                        "completed": {"value": 18.0, "text": "18"}
                    },
                    ...
                ],
                "velocityStatEntries": {
                    "1": {
                        "estimated": {"value": 20.0, "text": "20"},
                        "completed": {"value": 18.0, "text": "18"}
                    }
                }
            }

        Example:
            ```python
            velocity = await jira.get_velocity(board_id=123)

            if velocity:
                sprints = velocity.get("sprints", [])
                for sprint in sprints:
                    name = sprint["name"]
                    completed = sprint.get("completed", {}).get("value", 0)
                    print(f"{name}: {completed} pontos completados")

                # Calcular media de velocidade
                total = sum(
                    s.get("completed", {}).get("value", 0)
                    for s in sprints
                )
                avg = total / len(sprints) if sprints else 0
                print(f"Velocidade media: {avg:.1f} pontos/sprint")
            ```

        Note:
            Este endpoint requer que o board seja do tipo Scrum.
            Boards Kanban nao suportam velocity.
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            # Velocity usa o endpoint do Greenhopper (legado mas ainda suportado)
            # /rest/greenhopper/1.0/rapid/charts/velocity
            url = f"{self.config.base_url.rstrip('/')}/rest/greenhopper/1.0/rapid/charts/velocity"

            params = {
                "rapidViewId": board_id
            }

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    logger.debug(f"Velocity obtida para board {board_id}")
                    return data
                elif response.status == 404:
                    logger.warning(f"Board {board_id} nao encontrado ou nao e Scrum")
                else:
                    error = await response.text()
                    logger.error(f"Erro ao obter velocity: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao obter velocity do board {board_id}: {e}")

        return None

    # =========================================================================
    # Helpers
    # =========================================================================

    async def get_board_configuration(self, board_id: int) -> Optional[Dict]:
        """
        Obtem configuracao de um board.

        Args:
            board_id: ID do board

        Returns:
            Dict com configuracao do board (colunas, filtro, etc.)

        Example:
            ```python
            config = await jira.get_board_configuration(123)
            columns = config.get("columnConfig", {}).get("columns", [])
            for col in columns:
                print(f"Coluna: {col['name']}")
            ```
        """
        if not self.is_connected:
            logger.warning("Nao conectado ao Jira")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/configuration"

            async with session.get(url) as response:
                if response.status == 200:
                    config = await response.json()
                    logger.debug(f"Configuracao obtida para board {board_id}")
                    return config
                else:
                    error = await response.text()
                    logger.error(f"Erro ao obter configuracao: {response.status} - {error}")

        except Exception as e:
            logger.error(f"Erro ao obter configuracao do board {board_id}: {e}")

        return None

    async def get_all_sprints_paginated(
        self,
        board_id: int,
        state: Optional[str] = None
    ) -> List[Dict]:
        """
        Lista todos os sprints com paginacao automatica.

        Args:
            board_id: ID do board
            state: Filtrar por estado (future, active, closed)

        Returns:
            List[Dict]: Lista completa de sprints

        Example:
            ```python
            # Obter todos os sprints fechados (para calculo de velocity)
            all_closed = await jira.get_all_sprints_paginated(
                board_id=123,
                state="closed"
            )
            ```
        """
        all_sprints = []
        start_at = 0
        page_size = 50

        while True:
            sprints = await self.get_sprints(
                board_id=board_id,
                state=state,
                start_at=start_at,
                max_results=page_size
            )

            if not sprints:
                break

            all_sprints.extend(sprints)

            if len(sprints) < page_size:
                break

            start_at += page_size

        return all_sprints


# =============================================================================
# Instancia Global e Factory Functions
# =============================================================================

_jira_agile_instance: Optional[JiraAgileIntegration] = None


def get_jira_agile_integration() -> JiraAgileIntegration:
    """
    Retorna instancia global da integracao Jira Agile.

    Returns:
        JiraAgileIntegration: Instancia singleton

    Example:
        ```python
        jira = get_jira_agile_integration()
        if await jira.connect():
            boards = await jira.get_boards()
        ```
    """
    global _jira_agile_instance
    if _jira_agile_instance is None:
        config = JiraConfig.from_env()
        _jira_agile_instance = JiraAgileIntegration(config)
    return _jira_agile_instance


async def init_jira_agile_integration() -> Optional[JiraAgileIntegration]:
    """
    Inicializa e conecta a integracao Jira Agile se configurada.

    Returns:
        JiraAgileIntegration se conectado com sucesso, None caso contrario

    Example:
        ```python
        jira = await init_jira_agile_integration()
        if jira:
            sprints = await jira.get_sprints(board_id=123)
        ```
    """
    jira = get_jira_agile_integration()
    if jira.config.is_valid() and jira.config.enabled:
        if await jira.connect():
            return jira
    return None
