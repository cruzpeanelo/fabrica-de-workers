"""
Cursor-Based Pagination - Issue #107
====================================

Implementacao de paginacao baseada em cursor para grandes datasets.

Vantagens sobre offset-based:
- Performance consistente independente da pagina
- Evita resultados duplicados/perdidos em dados que mudam
- Mais eficiente para datasets grandes

Uso:
    from factory.api.pagination import CursorPagination

    paginator = CursorPagination()
    result = await paginator.paginate(
        query=db.query(Story),
        cursor=request_cursor,
        limit=20
    )
"""

import base64
import json
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple, TypeVar, Generic
from pydantic import BaseModel

from sqlalchemy.orm import Query
from sqlalchemy import asc, desc

T = TypeVar("T")


class CursorData(BaseModel):
    """Dados codificados no cursor"""
    last_id: str
    last_value: Optional[Any] = None
    direction: str = "next"
    sort_field: str = "id"


class PaginatedResult(BaseModel, Generic[T]):
    """Resultado paginado"""
    items: List[Any]
    cursor: Optional[str] = None
    has_more: bool = False
    total_count: Optional[int] = None


class CursorPagination:
    """
    Implementacao de paginacao baseada em cursor.

    Suporta:
    - Paginacao para frente e para tras
    - Ordenacao customizada
    - Contagem total opcional (para performance)
    """

    def __init__(
        self,
        default_limit: int = 20,
        max_limit: int = 100,
        id_field: str = "id",
        default_sort_field: str = "created_at",
        default_sort_order: str = "desc"
    ):
        """
        Inicializa o paginador.

        Args:
            default_limit: Limite padrao de itens
            max_limit: Limite maximo permitido
            id_field: Nome do campo ID do modelo
            default_sort_field: Campo padrao para ordenacao
            default_sort_order: Ordem padrao (asc/desc)
        """
        self.default_limit = default_limit
        self.max_limit = max_limit
        self.id_field = id_field
        self.default_sort_field = default_sort_field
        self.default_sort_order = default_sort_order

    def encode_cursor(
        self,
        item: Any,
        sort_field: str = None,
        direction: str = "next"
    ) -> str:
        """
        Codifica um cursor a partir de um item.

        Args:
            item: Item do qual extrair o cursor
            sort_field: Campo de ordenacao
            direction: Direcao da paginacao

        Returns:
            Cursor codificado em base64
        """
        sort_field = sort_field or self.default_sort_field

        # Extrair ID
        if hasattr(item, self.id_field):
            last_id = getattr(item, self.id_field)
        elif isinstance(item, dict):
            last_id = item.get(self.id_field) or item.get(f"{self.id_field[:-3]}_id")
        else:
            last_id = str(item)

        # Extrair valor do campo de ordenacao
        last_value = None
        if hasattr(item, sort_field):
            last_value = getattr(item, sort_field)
            if isinstance(last_value, datetime):
                last_value = last_value.isoformat()
        elif isinstance(item, dict) and sort_field in item:
            last_value = item[sort_field]
            if isinstance(last_value, datetime):
                last_value = last_value.isoformat()

        cursor_data = {
            "last_id": str(last_id),
            "last_value": last_value,
            "direction": direction,
            "sort_field": sort_field
        }

        # Codificar em base64
        json_str = json.dumps(cursor_data, default=str)
        return base64.urlsafe_b64encode(json_str.encode()).decode()

    def decode_cursor(self, cursor: str) -> Optional[CursorData]:
        """
        Decodifica um cursor.

        Args:
            cursor: Cursor codificado em base64

        Returns:
            CursorData ou None se invalido
        """
        try:
            json_str = base64.urlsafe_b64decode(cursor.encode()).decode()
            data = json.loads(json_str)
            return CursorData(**data)
        except Exception:
            return None

    def paginate(
        self,
        query: Query,
        model: Any,
        cursor: Optional[str] = None,
        limit: int = None,
        sort_field: str = None,
        sort_order: str = None,
        include_total: bool = False
    ) -> PaginatedResult:
        """
        Pagina uma query SQLAlchemy.

        Args:
            query: Query SQLAlchemy base
            model: Modelo SQLAlchemy
            cursor: Cursor da pagina anterior
            limit: Numero de itens a retornar
            sort_field: Campo para ordenacao
            sort_order: Ordem (asc/desc)
            include_total: Incluir contagem total

        Returns:
            PaginatedResult com items, cursor e metadados
        """
        # Validar e ajustar limite
        limit = min(limit or self.default_limit, self.max_limit)
        sort_field = sort_field or self.default_sort_field
        sort_order = sort_order or self.default_sort_order

        # Obter contagem total se solicitado
        total_count = None
        if include_total:
            total_count = query.count()

        # Aplicar ordenacao
        sort_column = getattr(model, sort_field, None)
        id_column = getattr(model, self.id_field, None)

        if sort_column is None:
            sort_column = getattr(model, "id", None)
        if id_column is None:
            id_column = getattr(model, "id", None)

        if sort_order == "desc":
            query = query.order_by(desc(sort_column), desc(id_column))
        else:
            query = query.order_by(asc(sort_column), asc(id_column))

        # Aplicar cursor se fornecido
        if cursor:
            cursor_data = self.decode_cursor(cursor)
            if cursor_data:
                # Filtrar baseado no cursor
                if cursor_data.direction == "next":
                    if sort_order == "desc":
                        if cursor_data.last_value:
                            query = query.filter(
                                (sort_column < cursor_data.last_value) |
                                ((sort_column == cursor_data.last_value) &
                                 (id_column < cursor_data.last_id))
                            )
                        else:
                            query = query.filter(id_column < cursor_data.last_id)
                    else:
                        if cursor_data.last_value:
                            query = query.filter(
                                (sort_column > cursor_data.last_value) |
                                ((sort_column == cursor_data.last_value) &
                                 (id_column > cursor_data.last_id))
                            )
                        else:
                            query = query.filter(id_column > cursor_data.last_id)

        # Buscar itens + 1 para verificar se ha mais
        items = query.limit(limit + 1).all()

        # Verificar se ha mais resultados
        has_more = len(items) > limit
        if has_more:
            items = items[:limit]

        # Gerar cursor para proxima pagina
        next_cursor = None
        if has_more and items:
            next_cursor = self.encode_cursor(
                items[-1],
                sort_field=sort_field,
                direction="next"
            )

        return PaginatedResult(
            items=items,
            cursor=next_cursor,
            has_more=has_more,
            total_count=total_count
        )

    def paginate_list(
        self,
        items: List[Any],
        cursor: Optional[str] = None,
        limit: int = None,
        id_field: str = None,
        sort_field: str = None
    ) -> PaginatedResult:
        """
        Pagina uma lista em memoria.

        Util para dados ja carregados ou APIs externas.

        Args:
            items: Lista de itens
            cursor: Cursor da pagina anterior
            limit: Numero de itens
            id_field: Campo ID
            sort_field: Campo de ordenacao

        Returns:
            PaginatedResult
        """
        limit = min(limit or self.default_limit, self.max_limit)
        id_field = id_field or self.id_field
        total_count = len(items)

        # Aplicar cursor
        start_index = 0
        if cursor:
            cursor_data = self.decode_cursor(cursor)
            if cursor_data:
                # Encontrar indice do item apos o cursor
                for i, item in enumerate(items):
                    item_id = None
                    if hasattr(item, id_field):
                        item_id = str(getattr(item, id_field))
                    elif isinstance(item, dict):
                        item_id = str(item.get(id_field))

                    if item_id == cursor_data.last_id:
                        start_index = i + 1
                        break

        # Fatiar lista
        end_index = start_index + limit
        page_items = items[start_index:end_index]
        has_more = end_index < total_count

        # Gerar cursor
        next_cursor = None
        if has_more and page_items:
            next_cursor = self.encode_cursor(
                page_items[-1],
                sort_field=sort_field,
                direction="next"
            )

        return PaginatedResult(
            items=page_items,
            cursor=next_cursor,
            has_more=has_more,
            total_count=total_count
        )


# Instancia global para uso simplificado
default_paginator = CursorPagination()


async def paginate_query(
    query: Query,
    model: Any,
    cursor: Optional[str] = None,
    limit: int = 20,
    include_total: bool = False
) -> PaginatedResult:
    """
    Funcao de conveniencia para paginar queries.

    Args:
        query: Query SQLAlchemy
        model: Modelo SQLAlchemy
        cursor: Cursor opcional
        limit: Limite de itens
        include_total: Incluir contagem

    Returns:
        PaginatedResult
    """
    return default_paginator.paginate(
        query=query,
        model=model,
        cursor=cursor,
        limit=limit,
        include_total=include_total
    )
