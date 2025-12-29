-- Fabrica de Agentes - Database Initialization Script
-- This script runs on first PostgreSQL container startup
-- Issue #123 - Setup Local Completo com Docker Compose

-- Enable extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";

-- Create schemas
CREATE SCHEMA IF NOT EXISTS fabrica;
CREATE SCHEMA IF NOT EXISTS audit;

-- Grant permissions
GRANT ALL ON SCHEMA fabrica TO fabrica;
GRANT ALL ON SCHEMA audit TO fabrica;

-- Set default schema
ALTER USER fabrica SET search_path TO fabrica, public;

-- Create audit log function for tracking changes
CREATE OR REPLACE FUNCTION audit.log_changes()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO audit.change_log (table_name, operation, new_data, changed_at)
        VALUES (TG_TABLE_NAME, 'INSERT', row_to_json(NEW), NOW());
        RETURN NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO audit.change_log (table_name, operation, old_data, new_data, changed_at)
        VALUES (TG_TABLE_NAME, 'UPDATE', row_to_json(OLD), row_to_json(NEW), NOW());
        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO audit.change_log (table_name, operation, old_data, changed_at)
        VALUES (TG_TABLE_NAME, 'DELETE', row_to_json(OLD), NOW());
        RETURN OLD;
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Create audit change log table
CREATE TABLE IF NOT EXISTS audit.change_log (
    id SERIAL PRIMARY KEY,
    table_name VARCHAR(100) NOT NULL,
    operation VARCHAR(10) NOT NULL,
    old_data JSONB,
    new_data JSONB,
    changed_at TIMESTAMP DEFAULT NOW()
);

-- Create index for faster audit queries
CREATE INDEX IF NOT EXISTS idx_change_log_table ON audit.change_log(table_name);
CREATE INDEX IF NOT EXISTS idx_change_log_timestamp ON audit.change_log(changed_at);

-- Success message
DO $$
BEGIN
    RAISE NOTICE 'Fabrica de Agentes database initialized successfully!';
END $$;
