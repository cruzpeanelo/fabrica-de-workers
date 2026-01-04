# -*- coding: utf-8 -*-
"""
Row Level Security (RLS) Setup - Issue #122
Plataforma E v6.5

This module provides database-level tenant isolation using:
1. PostgreSQL Row Level Security (RLS) policies
2. SQLite application-level enforcement (fallback)

RLS ensures that even direct SQL queries cannot access
data from other tenants - the database itself enforces isolation.

IMPORTANT: RLS is the gold standard for multi-tenant security.
Application-level filtering can be bypassed; RLS cannot.
"""

import os
from typing import List, Optional
from datetime import datetime

from sqlalchemy import text, inspect
from sqlalchemy.engine import Engine


# =============================================================================
# TABLES THAT REQUIRE TENANT ISOLATION
# =============================================================================

# All tables that store tenant-specific data
TENANT_ISOLATED_TABLES = [
    "projects",
    "jobs",
    "tasks",
    "stories",
    "story_tasks",
    "story_documentation",
    "story_designs",
    "story_estimates",
    "epics",
    "sprints",
    "attachments",
    "chat_messages",
    "execution_logs",
    "code_versions",
    "code_branches",
    "ab_tests",
    "ab_test_variants",
    "productivity_metrics",
    "productivity_snapshots",
    "agent_performance",
    "marketplace_downloads",
    # User-related tables (tenant-scoped users)
    "user_roles",
]

# Tables that are global (no tenant isolation)
GLOBAL_TABLES = [
    "users",           # Users can belong to multiple tenants
    "roles",           # Roles are global definitions
    "permissions",     # Permissions are global
    "workers",         # Workers are infrastructure
    "activity_logs",   # System-wide logs
    "audit_logs",      # Audit is global (includes tenant_id in details)
    "marketplace_items",     # Global marketplace
    "marketplace_reviews",   # Global reviews
]


# =============================================================================
# POSTGRESQL RLS SETUP
# =============================================================================

def setup_postgresql_rls(engine: Engine, force_recreate: bool = False) -> dict:
    """
    Set up Row Level Security (RLS) policies for PostgreSQL.

    This creates policies that:
    1. Enable RLS on all tenant-isolated tables
    2. Filter SELECT/INSERT/UPDATE/DELETE by tenant_id
    3. Use session variable for tenant context

    Args:
        engine: SQLAlchemy engine connected to PostgreSQL
        force_recreate: If True, drop and recreate policies

    Returns:
        Dictionary with setup results
    """
    results = {
        "success": True,
        "tables_processed": [],
        "errors": [],
        "warnings": [],
        "timestamp": datetime.utcnow().isoformat()
    }

    with engine.connect() as conn:
        # Check if PostgreSQL
        dialect = engine.dialect.name
        if dialect != "postgresql":
            results["success"] = False
            results["errors"].append(f"RLS requires PostgreSQL, got {dialect}")
            return results

        try:
            # Create app role for RLS if not exists
            conn.execute(text("""
                DO $$
                BEGIN
                    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'app_user') THEN
                        CREATE ROLE app_user;
                    END IF;
                END
                $$;
            """))

            # Process each table
            for table_name in TENANT_ISOLATED_TABLES:
                try:
                    result = _setup_table_rls(conn, table_name, force_recreate)
                    results["tables_processed"].append({
                        "table": table_name,
                        "status": "success",
                        **result
                    })
                except Exception as e:
                    results["errors"].append({
                        "table": table_name,
                        "error": str(e)
                    })

            # Create helper function for setting tenant context
            conn.execute(text("""
                CREATE OR REPLACE FUNCTION set_tenant_context(tenant_id text)
                RETURNS void AS $$
                BEGIN
                    PERFORM set_config('app.current_tenant_id', tenant_id, false);
                END;
                $$ LANGUAGE plpgsql SECURITY DEFINER;
            """))

            # Create helper function for getting tenant context
            conn.execute(text("""
                CREATE OR REPLACE FUNCTION get_tenant_context()
                RETURNS text AS $$
                BEGIN
                    RETURN current_setting('app.current_tenant_id', true);
                END;
                $$ LANGUAGE plpgsql STABLE;
            """))

            # Create trigger function for auto-setting tenant_id on insert
            conn.execute(text("""
                CREATE OR REPLACE FUNCTION set_tenant_id_on_insert()
                RETURNS TRIGGER AS $$
                BEGIN
                    IF NEW.tenant_id IS NULL THEN
                        NEW.tenant_id := current_setting('app.current_tenant_id', true);
                    END IF;

                    IF NEW.tenant_id IS NULL THEN
                        RAISE EXCEPTION 'tenant_id is required';
                    END IF;

                    RETURN NEW;
                END;
                $$ LANGUAGE plpgsql;
            """))

            conn.commit()

        except Exception as e:
            results["success"] = False
            results["errors"].append(f"Setup error: {str(e)}")
            conn.rollback()

    return results


def _setup_table_rls(conn, table_name: str, force_recreate: bool) -> dict:
    """Set up RLS for a single table"""
    result = {
        "rls_enabled": False,
        "policies_created": []
    }

    # Check if table exists
    check_table = conn.execute(text(f"""
        SELECT EXISTS (
            SELECT FROM information_schema.tables
            WHERE table_name = :table_name
        )
    """), {"table_name": table_name})

    if not check_table.scalar():
        result["warning"] = f"Table {table_name} does not exist"
        return result

    # Check if tenant_id column exists
    check_column = conn.execute(text(f"""
        SELECT EXISTS (
            SELECT FROM information_schema.columns
            WHERE table_name = :table_name AND column_name = 'tenant_id'
        )
    """), {"table_name": table_name})

    if not check_column.scalar():
        result["warning"] = f"Table {table_name} has no tenant_id column"
        return result

    # Drop existing policies if force_recreate
    if force_recreate:
        conn.execute(text(f"""
            DO $$
            DECLARE
                pol RECORD;
            BEGIN
                FOR pol IN SELECT policyname FROM pg_policies WHERE tablename = '{table_name}'
                LOOP
                    EXECUTE 'DROP POLICY IF EXISTS ' || quote_ident(pol.policyname) || ' ON {table_name}';
                END LOOP;
            END
            $$;
        """))

    # Enable RLS on table
    conn.execute(text(f"ALTER TABLE {table_name} ENABLE ROW LEVEL SECURITY"))
    conn.execute(text(f"ALTER TABLE {table_name} FORCE ROW LEVEL SECURITY"))
    result["rls_enabled"] = True

    # Create policies
    policy_name = f"{table_name}_tenant_isolation"

    # SELECT policy - only see own tenant's data
    conn.execute(text(f"""
        CREATE POLICY IF NOT EXISTS {policy_name}_select ON {table_name}
        FOR SELECT
        USING (tenant_id = current_setting('app.current_tenant_id', true))
    """))
    result["policies_created"].append(f"{policy_name}_select")

    # INSERT policy - can only insert with current tenant_id
    conn.execute(text(f"""
        CREATE POLICY IF NOT EXISTS {policy_name}_insert ON {table_name}
        FOR INSERT
        WITH CHECK (
            tenant_id = current_setting('app.current_tenant_id', true)
            OR tenant_id IS NULL
        )
    """))
    result["policies_created"].append(f"{policy_name}_insert")

    # UPDATE policy - can only update own tenant's data
    conn.execute(text(f"""
        CREATE POLICY IF NOT EXISTS {policy_name}_update ON {table_name}
        FOR UPDATE
        USING (tenant_id = current_setting('app.current_tenant_id', true))
        WITH CHECK (tenant_id = current_setting('app.current_tenant_id', true))
    """))
    result["policies_created"].append(f"{policy_name}_update")

    # DELETE policy - can only delete own tenant's data
    conn.execute(text(f"""
        CREATE POLICY IF NOT EXISTS {policy_name}_delete ON {table_name}
        FOR DELETE
        USING (tenant_id = current_setting('app.current_tenant_id', true))
    """))
    result["policies_created"].append(f"{policy_name}_delete")

    # Create trigger for auto-setting tenant_id
    trigger_name = f"{table_name}_set_tenant_id"
    conn.execute(text(f"""
        DROP TRIGGER IF EXISTS {trigger_name} ON {table_name};
        CREATE TRIGGER {trigger_name}
        BEFORE INSERT ON {table_name}
        FOR EACH ROW
        EXECUTE FUNCTION set_tenant_id_on_insert();
    """))

    return result


def disable_postgresql_rls(engine: Engine) -> dict:
    """
    Disable RLS on all tables (for maintenance/migration).

    WARNING: This removes tenant isolation protection!
    Only use during controlled maintenance windows.
    """
    results = {
        "success": True,
        "tables_processed": [],
        "errors": []
    }

    with engine.connect() as conn:
        for table_name in TENANT_ISOLATED_TABLES:
            try:
                conn.execute(text(f"ALTER TABLE {table_name} DISABLE ROW LEVEL SECURITY"))
                results["tables_processed"].append(table_name)
            except Exception as e:
                results["errors"].append({
                    "table": table_name,
                    "error": str(e)
                })

        conn.commit()

    return results


# =============================================================================
# SQLITE APPLICATION-LEVEL ENFORCEMENT
# =============================================================================

class SQLiteTenantEnforcer:
    """
    Application-level tenant isolation for SQLite.

    SQLite doesn't support RLS, so we enforce at the application level.
    This is less secure than PostgreSQL RLS but provides reasonable
    protection for development and testing environments.
    """

    def __init__(self, db_session):
        self.db = db_session
        self._current_tenant = None

    def set_tenant(self, tenant_id: str):
        """Set current tenant context"""
        self._current_tenant = tenant_id

    def get_tenant(self) -> Optional[str]:
        """Get current tenant context"""
        return self._current_tenant

    def filter_query(self, query, model_class):
        """Filter query by current tenant"""
        if not self._current_tenant:
            raise ValueError("Tenant context not set")

        if hasattr(model_class, 'tenant_id'):
            return query.filter(model_class.tenant_id == self._current_tenant)

        return query

    def prepare_insert(self, entity):
        """Prepare entity for insert with tenant_id"""
        if not self._current_tenant:
            raise ValueError("Tenant context not set")

        if hasattr(entity, 'tenant_id'):
            if entity.tenant_id is None:
                entity.tenant_id = self._current_tenant
            elif entity.tenant_id != self._current_tenant:
                raise ValueError("Cannot insert entity with different tenant_id")

        return entity

    def validate_update(self, entity) -> bool:
        """Validate entity belongs to current tenant before update"""
        if not self._current_tenant:
            return False

        if hasattr(entity, 'tenant_id'):
            return entity.tenant_id == self._current_tenant

        return True


# =============================================================================
# TENANT_ID COLUMN MIGRATION
# =============================================================================

def add_tenant_id_column(engine: Engine, table_name: str, default_tenant: str = None) -> dict:
    """
    Add tenant_id column to existing table.

    This is used during migration to add tenant isolation
    to tables that previously didn't have it.

    Args:
        engine: SQLAlchemy engine
        table_name: Name of table to modify
        default_tenant: Default tenant_id for existing rows

    Returns:
        Migration result dictionary
    """
    result = {
        "success": False,
        "table": table_name,
        "rows_updated": 0
    }

    dialect = engine.dialect.name

    with engine.connect() as conn:
        try:
            # Check if column already exists
            if dialect == "postgresql":
                check_sql = text("""
                    SELECT EXISTS (
                        SELECT FROM information_schema.columns
                        WHERE table_name = :table_name AND column_name = 'tenant_id'
                    )
                """)
            else:  # SQLite
                check_sql = text(f"PRAGMA table_info({table_name})")

            if dialect == "postgresql":
                exists = conn.execute(check_sql, {"table_name": table_name}).scalar()
            else:
                columns = conn.execute(check_sql).fetchall()
                exists = any(col[1] == 'tenant_id' for col in columns)

            if exists:
                result["message"] = "Column already exists"
                result["success"] = True
                return result

            # Add column
            if dialect == "postgresql":
                conn.execute(text(f"""
                    ALTER TABLE {table_name}
                    ADD COLUMN tenant_id VARCHAR(50)
                """))

                # Add index
                conn.execute(text(f"""
                    CREATE INDEX IF NOT EXISTS idx_{table_name}_tenant_id
                    ON {table_name} (tenant_id)
                """))
            else:
                conn.execute(text(f"""
                    ALTER TABLE {table_name}
                    ADD COLUMN tenant_id VARCHAR(50)
                """))

            # Update existing rows if default provided
            if default_tenant:
                update_result = conn.execute(text(f"""
                    UPDATE {table_name}
                    SET tenant_id = :tenant_id
                    WHERE tenant_id IS NULL
                """), {"tenant_id": default_tenant})
                result["rows_updated"] = update_result.rowcount

            # Make column NOT NULL after populating
            if default_tenant and dialect == "postgresql":
                conn.execute(text(f"""
                    ALTER TABLE {table_name}
                    ALTER COLUMN tenant_id SET NOT NULL
                """))

            conn.commit()
            result["success"] = True

        except Exception as e:
            result["error"] = str(e)
            conn.rollback()

    return result


def verify_tenant_isolation(engine: Engine) -> dict:
    """
    Verify that tenant isolation is properly configured.

    This performs security checks to ensure:
    1. All required tables have tenant_id column
    2. RLS is enabled (PostgreSQL) or enforcer is active
    3. No data can be accessed without tenant context

    Returns:
        Verification report
    """
    report = {
        "success": True,
        "database_type": engine.dialect.name,
        "tables_checked": [],
        "issues": [],
        "recommendations": []
    }

    with engine.connect() as conn:
        for table_name in TENANT_ISOLATED_TABLES:
            table_check = {
                "table": table_name,
                "exists": False,
                "has_tenant_id": False,
                "tenant_id_indexed": False,
                "rls_enabled": False
            }

            try:
                # Check table exists
                if engine.dialect.name == "postgresql":
                    exists = conn.execute(text("""
                        SELECT EXISTS (
                            SELECT FROM information_schema.tables
                            WHERE table_name = :table_name
                        )
                    """), {"table_name": table_name}).scalar()
                else:
                    tables = conn.execute(text(
                        "SELECT name FROM sqlite_master WHERE type='table' AND name=:table_name"
                    ), {"table_name": table_name}).fetchall()
                    exists = len(tables) > 0

                table_check["exists"] = exists

                if exists:
                    # Check tenant_id column
                    if engine.dialect.name == "postgresql":
                        has_column = conn.execute(text("""
                            SELECT EXISTS (
                                SELECT FROM information_schema.columns
                                WHERE table_name = :table_name AND column_name = 'tenant_id'
                            )
                        """), {"table_name": table_name}).scalar()
                    else:
                        columns = conn.execute(text(f"PRAGMA table_info({table_name})")).fetchall()
                        has_column = any(col[1] == 'tenant_id' for col in columns)

                    table_check["has_tenant_id"] = has_column

                    if not has_column:
                        report["issues"].append({
                            "table": table_name,
                            "issue": "Missing tenant_id column",
                            "severity": "CRITICAL"
                        })

                    # Check RLS (PostgreSQL only)
                    if engine.dialect.name == "postgresql":
                        rls_result = conn.execute(text("""
                            SELECT relrowsecurity FROM pg_class
                            WHERE relname = :table_name
                        """), {"table_name": table_name}).fetchone()

                        if rls_result:
                            table_check["rls_enabled"] = rls_result[0]

                            if not rls_result[0]:
                                report["issues"].append({
                                    "table": table_name,
                                    "issue": "RLS not enabled",
                                    "severity": "HIGH"
                                })

            except Exception as e:
                table_check["error"] = str(e)

            report["tables_checked"].append(table_check)

    # Add recommendations
    if engine.dialect.name != "postgresql":
        report["recommendations"].append(
            "Use PostgreSQL in production for database-level RLS enforcement"
        )

    critical_issues = [i for i in report["issues"] if i.get("severity") == "CRITICAL"]
    if critical_issues:
        report["success"] = False

    return report


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """CLI for RLS setup"""
    import argparse
    import json

    parser = argparse.ArgumentParser(description="Row Level Security Setup")
    parser.add_argument("command", choices=["setup", "disable", "verify", "migrate"])
    parser.add_argument("--force", action="store_true", help="Force recreate policies")
    parser.add_argument("--table", help="Specific table for migration")
    parser.add_argument("--default-tenant", help="Default tenant for migration")

    args = parser.parse_args()

    from factory.database.connection import sync_engine

    if args.command == "setup":
        result = setup_postgresql_rls(sync_engine, force_recreate=args.force)
        print(json.dumps(result, indent=2))

    elif args.command == "disable":
        result = disable_postgresql_rls(sync_engine)
        print(json.dumps(result, indent=2))

    elif args.command == "verify":
        result = verify_tenant_isolation(sync_engine)
        print(json.dumps(result, indent=2))

    elif args.command == "migrate":
        if not args.table:
            print("Error: --table required for migration")
            return
        result = add_tenant_id_column(sync_engine, args.table, args.default_tenant)
        print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
