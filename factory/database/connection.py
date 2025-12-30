"""
Conexao com o Banco de Dados - Fabrica de Agentes v4.0
Suporta PostgreSQL (producao) e SQLite (fallback local)
"""
import os
from pathlib import Path
from contextlib import asynccontextmanager
from typing import AsyncGenerator, Generator

from sqlalchemy import create_engine, event
from sqlalchemy.orm import sessionmaker, declarative_base, Session
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from dotenv import load_dotenv

# Carregar variaveis de ambiente
load_dotenv()

# ============================================
# Configuracao do Banco de Dados
# ============================================

# Diretorio local (fallback SQLite)
DATABASE_DIR = Path(__file__).parent
SQLITE_FILE = DATABASE_DIR / "factory.db"

# URLs de conexao
DATABASE_URL = os.getenv("DATABASE_URL", f"sqlite+aiosqlite:///{SQLITE_FILE}")
DATABASE_URL_SYNC = os.getenv("DATABASE_URL_SYNC", f"sqlite:///{SQLITE_FILE}")

# Detectar tipo de banco
IS_POSTGRES = "postgresql" in DATABASE_URL

# ============================================
# Engine Async (para API FastAPI)
# ============================================

if IS_POSTGRES:
    async_engine = create_async_engine(
        DATABASE_URL,
        pool_size=20,
        max_overflow=10,
        pool_pre_ping=True,
        echo=False
    )
else:
    # SQLite async
    async_engine = create_async_engine(
        DATABASE_URL,
        echo=False
    )

AsyncSessionLocal = async_sessionmaker(
    async_engine,
    class_=AsyncSession,
    expire_on_commit=False
)

# ============================================
# Engine Sync (para scripts e migrations)
# ============================================

if IS_POSTGRES:
    sync_engine = create_engine(
        DATABASE_URL_SYNC,
        pool_size=10,
        max_overflow=5,
        pool_pre_ping=True,
        echo=False
    )
else:
    sync_engine = create_engine(
        DATABASE_URL_SYNC,
        connect_args={"check_same_thread": False},
        echo=False
    )

    # Habilitar foreign keys no SQLite
    @event.listens_for(sync_engine, "connect")
    def set_sqlite_pragma(dbapi_connection, connection_record):
        cursor = dbapi_connection.cursor()
        cursor.execute("PRAGMA foreign_keys=ON")
        cursor.close()

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=sync_engine)

# ============================================
# Base para os modelos
# ============================================

Base = declarative_base()

# ============================================
# Redis Connection
# ============================================

REDIS_URL = os.getenv("REDIS_URL", "redis://localhost:6379")

_redis_client = None

async def get_redis():
    """Obtem cliente Redis (singleton async)"""
    global _redis_client
    if _redis_client is None:
        try:
            import redis.asyncio as aioredis
            _redis_client = aioredis.from_url(
                REDIS_URL,
                encoding="utf-8",
                decode_responses=True
            )
        except ImportError:
            print("[Factory DB] Redis nao disponivel - instale com: pip install redis")
            return None
    return _redis_client

async def close_redis():
    """Fecha conexao Redis"""
    global _redis_client
    if _redis_client:
        await _redis_client.close()
        _redis_client = None

# ============================================
# Dependency Injection para FastAPI
# ============================================

async def get_async_db() -> AsyncGenerator[AsyncSession, None]:
    """Dependency async para FastAPI"""
    async with AsyncSessionLocal() as session:
        try:
            yield session
        finally:
            await session.close()

def get_db() -> Generator[Session, None, None]:
    """Dependency sync para scripts"""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


# Issue #182: Context manager síncrono com transaction boundaries
from contextlib import contextmanager

@contextmanager
def transaction_context() -> Generator[Session, None, None]:
    """
    Context manager síncrono para transações (Issue #182)

    Uso:
        with transaction_context() as db:
            repo = ProjectRepository(db)
            repo.create({...})
            repo.update(...)
            # commit automático no fim do bloco
            # rollback automático em caso de exceção
    """
    db = SessionLocal()
    try:
        yield db
        db.commit()
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


@asynccontextmanager
async def get_db_context() -> AsyncGenerator[AsyncSession, None]:
    """Context manager para uso fora de FastAPI"""
    async with AsyncSessionLocal() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise
        finally:
            await session.close()

# ============================================
# Inicializacao e Reset
# ============================================

def init_db():
    """Inicializa o banco de dados criando todas as tabelas"""
    from . import models  # noqa: F401
    Base.metadata.create_all(bind=sync_engine)
    db_type = "PostgreSQL" if IS_POSTGRES else "SQLite"
    print(f"[Factory DB] Banco de dados {db_type} inicializado")
    return True

async def init_db_async():
    """Inicializa o banco de dados de forma assincrona"""
    from . import models  # noqa: F401
    async with async_engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    db_type = "PostgreSQL" if IS_POSTGRES else "SQLite"
    print(f"[Factory DB] Banco de dados {db_type} inicializado (async)")
    return True

def reset_db():
    """Remove e recria todas as tabelas (CUIDADO: perde todos os dados)"""
    from . import models  # noqa: F401
    Base.metadata.drop_all(bind=sync_engine)
    Base.metadata.create_all(bind=sync_engine)
    db_type = "PostgreSQL" if IS_POSTGRES else "SQLite"
    print(f"[Factory DB] Banco de dados {db_type} resetado")
    return True

async def reset_db_async():
    """Remove e recria todas as tabelas de forma assincrona"""
    from . import models  # noqa: F401
    async with async_engine.begin() as conn:
        await conn.run_sync(Base.metadata.drop_all)
        await conn.run_sync(Base.metadata.create_all)
    db_type = "PostgreSQL" if IS_POSTGRES else "SQLite"
    print(f"[Factory DB] Banco de dados {db_type} resetado (async)")
    return True

# ============================================
# Health Check
# ============================================

async def check_db_health() -> dict:
    """Verifica saude das conexoes"""
    health = {
        "database": {"status": "unknown", "type": "PostgreSQL" if IS_POSTGRES else "SQLite"},
        "redis": {"status": "unknown"}
    }

    # Check database
    try:
        async with AsyncSessionLocal() as session:
            await session.execute("SELECT 1")
            health["database"]["status"] = "healthy"
    except Exception as e:
        health["database"]["status"] = "unhealthy"
        health["database"]["error"] = str(e)

    # Check Redis
    try:
        redis = await get_redis()
        if redis:
            await redis.ping()
            health["redis"]["status"] = "healthy"
        else:
            health["redis"]["status"] = "not_configured"
    except Exception as e:
        health["redis"]["status"] = "unhealthy"
        health["redis"]["error"] = str(e)

    return health

# ============================================
# Aliases para compatibilidade
# ============================================

# Manter compatibilidade com codigo existente
engine = sync_engine
DATABASE_FILE = SQLITE_FILE

if __name__ == "__main__":
    init_db()
    print(f"Database type: {'PostgreSQL' if IS_POSTGRES else 'SQLite'}")
    print(f"Redis URL: {REDIS_URL}")
