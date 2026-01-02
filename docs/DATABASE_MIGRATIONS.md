# Database Migrations - Fabrica de Agentes

## Decisão Arquitetural

O projeto utiliza **migrações customizadas** em vez de Alembic pelos seguintes motivos:

1. **Simplicidade**: Scripts Python puros sem dependências extras
2. **Controle**: Execução automática no startup da aplicação
3. **Flexibilidade**: Suporte a PostgreSQL e SQLite sem configuração adicional
4. **Idempotência**: Migrações verificam se já foram aplicadas antes de executar

## Estrutura

```
factory/database/
├── connection.py          # Conexão e execução de migrações
├── models.py              # Modelos SQLAlchemy
├── repositories.py        # Data access layer
└── migrations/
    ├── add_tenant_id_columns.py      # Issue #241
    ├── add_external_references.py    # Issue #401
    └── add_external_integration_fields.py
```

## Como Funciona

### Execução Automática

As migrações são executadas automaticamente quando:

```python
# Em factory/database/connection.py
def init_db():
    Base.metadata.create_all(bind=sync_engine)
    run_migrations()  # Executa todas as migrações pendentes
```

### Ordem de Execução

```python
def run_migrations():
    # 1. Add tenant_id columns (Issue #241)
    from .migrations.add_tenant_id_columns import upgrade
    upgrade()

    # 2. Add external_references (Issue #401)
    from .migrations.add_external_references import upgrade
    upgrade()
```

## Criando Nova Migração

### 1. Criar arquivo de migração

```python
# factory/database/migrations/add_new_column.py

def get_database_type():
    """Detecta PostgreSQL ou SQLite"""
    db_url = os.getenv("DATABASE_URL", "")
    return "postgresql" if "postgresql" in db_url else "sqlite"

def upgrade():
    """Adiciona nova coluna à tabela"""
    from sqlalchemy import text
    from factory.database.connection import sync_engine

    db_type = get_database_type()

    with sync_engine.connect() as conn:
        # Verificar se coluna já existe
        if db_type == "postgresql":
            result = conn.execute(text("""
                SELECT column_name FROM information_schema.columns
                WHERE table_name = 'my_table' AND column_name = 'new_column'
            """))
            exists = result.fetchone() is not None
        else:
            result = conn.execute(text("PRAGMA table_info(my_table)"))
            columns = {row[1] for row in result.fetchall()}
            exists = 'new_column' in columns

        if exists:
            print("Coluna já existe - nada a fazer")
            return

        # Adicionar coluna
        if db_type == "postgresql":
            conn.execute(text("""
                ALTER TABLE my_table ADD COLUMN new_column VARCHAR(100)
            """))
        else:
            conn.execute(text("""
                ALTER TABLE my_table ADD COLUMN new_column TEXT
            """))

        conn.commit()
        print("Migração concluída!")
```

### 2. Registrar em connection.py

```python
def run_migrations():
    # ... migrações existentes ...

    # Nova migração
    try:
        from .migrations.add_new_column import upgrade
        upgrade()
    except Exception as e:
        errors.append(f"add_new_column: {e}")
```

### 3. Adicionar fallback em init-db.sql (opcional)

```sql
-- scripts/init-db.sql
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'my_table' AND column_name = 'new_column'
    ) THEN
        ALTER TABLE my_table ADD COLUMN new_column VARCHAR(100);
    END IF;
END $$;
```

## Executando Migrações Manualmente

```bash
# Via Python
python -m factory.database.migrations.add_external_references

# Via init_db
python -c "from factory.database.connection import init_db; init_db()"

# Via Docker
docker-compose exec app python -c "from factory.database.connection import init_db; init_db()"
```

## Rollback

Migrações de rollback não são implementadas automaticamente. Para reverter:

```sql
-- Manualmente via SQL
ALTER TABLE my_table DROP COLUMN new_column;
```

## Por que não Alembic?

| Aspecto | Custom Migrations | Alembic |
|---------|-------------------|---------|
| Setup | Zero config | Requer alembic.ini |
| Dependências | Nenhuma extra | alembic package |
| Complexidade | Baixa | Média |
| Versionamento | Por arquivo | Por revision |
| Auto-generate | Não | Sim |
| Rollback | Manual | Automático |

Para projetos maiores ou com muitas migrações frequentes, considerar migrar para Alembic.

## Referências

- Issue #175: No Alembic migration system
- Issue #241: Add tenant_id columns
- Issue #401: Add external_references column
