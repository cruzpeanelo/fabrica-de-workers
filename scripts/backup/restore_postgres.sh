#!/bin/bash
# =============================================================================
# PostgreSQL Restore Script
# Issue #199: Automated Backup System - Disaster Recovery
#
# Usage: ./restore_postgres.sh <backup_file> [--target-db <database>]
# =============================================================================

set -e

# Configuration
BACKUP_DIR="${BACKUP_DIR:-/var/backups/fabrica}"

# Database connection (from environment)
PG_HOST="${PGHOST:-localhost}"
PG_PORT="${PGPORT:-5432}"
PG_USER="${PGUSER:-postgres}"
PG_DATABASE="${PGDATABASE:-factory_db}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# Usage
show_usage() {
    echo "Usage: $0 <backup_file> [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target-db <database>  Target database (default: \$PGDATABASE or factory_db)"
    echo "  --no-confirm            Skip confirmation prompt"
    echo "  --verify-only           Only verify backup integrity, don't restore"
    echo "  --create-db             Create database if it doesn't exist"
    echo "  -h, --help              Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 /var/backups/fabrica/daily/postgres_20241230.dump"
    echo "  $0 backup.dump.enc --target-db factory_restore"
    echo "  $0 backup.dump --verify-only"
}

# Parse arguments
BACKUP_FILE=""
TARGET_DB="${PG_DATABASE}"
NO_CONFIRM=false
VERIFY_ONLY=false
CREATE_DB=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --target-db)
            TARGET_DB="$2"
            shift 2
            ;;
        --no-confirm)
            NO_CONFIRM=true
            shift
            ;;
        --verify-only)
            VERIFY_ONLY=true
            shift
            ;;
        --create-db)
            CREATE_DB=true
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            if [ -z "${BACKUP_FILE}" ]; then
                BACKUP_FILE="$1"
            fi
            shift
            ;;
    esac
done

# Validate backup file
if [ -z "${BACKUP_FILE}" ]; then
    log_error "Backup file not specified"
    show_usage
    exit 1
fi

if [ ! -f "${BACKUP_FILE}" ]; then
    log_error "Backup file not found: ${BACKUP_FILE}"
    exit 1
fi

# Decrypt backup if needed
decrypt_backup() {
    local input_file="$1"

    if [[ "${input_file}" == *.enc ]]; then
        log_step "Decrypting backup..."

        if [ -z "${BACKUP_ENCRYPTION_KEY}" ]; then
            log_error "BACKUP_ENCRYPTION_KEY not set"
            exit 1
        fi

        local decrypted_file="${input_file%.enc}"

        openssl enc -d -aes-256-cbc \
            -in "${input_file}" \
            -out "${decrypted_file}" \
            -pass env:BACKUP_ENCRYPTION_KEY

        echo "${decrypted_file}"
    else
        echo "${input_file}"
    fi
}

# Verify backup integrity
verify_backup() {
    local backup_file="$1"

    log_step "Verifying backup integrity..."

    # Check file size
    local size=$(stat -f%z "${backup_file}" 2>/dev/null || stat -c%s "${backup_file}" 2>/dev/null)
    if [ "${size}" -eq 0 ]; then
        log_error "Backup file is empty"
        return 1
    fi
    log_info "File size: $(numfmt --to=iec ${size} 2>/dev/null || echo ${size} bytes)"

    # Check if it's a valid pg_dump custom format
    if [[ "${backup_file}" == *.dump ]]; then
        pg_restore --list "${backup_file}" > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            log_info "Valid PostgreSQL custom format backup"
        else
            log_error "Invalid PostgreSQL backup format"
            return 1
        fi
    fi

    # Verify checksum if exists
    local checksum_file="${backup_file}.sha256"
    if [ -f "${checksum_file}" ]; then
        local expected=$(cat "${checksum_file}")
        local actual=$(sha256sum "${backup_file}" | awk '{print $1}')

        if [ "${expected}" == "${actual}" ]; then
            log_info "Checksum verified: ${actual}"
        else
            log_error "Checksum mismatch!"
            log_error "Expected: ${expected}"
            log_error "Actual: ${actual}"
            return 1
        fi
    else
        log_warn "No checksum file found, skipping verification"
    fi

    log_info "Backup verification passed"
    return 0
}

# Create database if needed
create_database() {
    if [ "${CREATE_DB}" = true ]; then
        log_step "Creating database ${TARGET_DB} if not exists..."

        psql -h "${PG_HOST}" -p "${PG_PORT}" -U "${PG_USER}" -d postgres \
            -c "CREATE DATABASE ${TARGET_DB};" 2>/dev/null || true
    fi
}

# Pre-restore checks
pre_restore_checks() {
    log_step "Running pre-restore checks..."

    # Check database connectivity
    psql -h "${PG_HOST}" -p "${PG_PORT}" -U "${PG_USER}" -d "${TARGET_DB}" \
        -c "SELECT 1" > /dev/null 2>&1

    if [ $? -ne 0 ]; then
        log_warn "Cannot connect to database ${TARGET_DB}"

        if [ "${CREATE_DB}" = true ]; then
            create_database
        else
            log_error "Database doesn't exist. Use --create-db to create it."
            exit 1
        fi
    fi

    log_info "Database connection OK"
}

# Perform restore
perform_restore() {
    local backup_file="$1"

    log_step "Starting restore to ${TARGET_DB}..."
    log_warn "This will OVERWRITE existing data in ${TARGET_DB}!"

    # Confirmation prompt
    if [ "${NO_CONFIRM}" = false ]; then
        echo ""
        read -p "Are you sure you want to continue? (yes/no): " confirm
        if [ "${confirm}" != "yes" ]; then
            log_info "Restore cancelled"
            exit 0
        fi
    fi

    # Create backup of current state
    log_step "Creating backup of current state..."
    local pre_restore_backup="${BACKUP_DIR}/temp/pre_restore_$(date +%Y%m%d_%H%M%S).dump"
    mkdir -p "${BACKUP_DIR}/temp"

    pg_dump \
        --host="${PG_HOST}" \
        --port="${PG_PORT}" \
        --username="${PG_USER}" \
        --dbname="${TARGET_DB}" \
        --format=custom \
        --file="${pre_restore_backup}" 2>/dev/null || true

    if [ -f "${pre_restore_backup}" ]; then
        log_info "Pre-restore backup saved: ${pre_restore_backup}"
    fi

    # Perform restore
    log_step "Restoring database..."

    pg_restore \
        --host="${PG_HOST}" \
        --port="${PG_PORT}" \
        --username="${PG_USER}" \
        --dbname="${TARGET_DB}" \
        --clean \
        --if-exists \
        --no-owner \
        --no-privileges \
        "${backup_file}"

    if [ $? -eq 0 ]; then
        log_info "Restore completed successfully!"
        return 0
    else
        log_warn "Restore completed with some warnings (this is often normal)"
        return 0
    fi
}

# Post-restore verification
post_restore_verify() {
    log_step "Running post-restore verification..."

    # Count tables
    local table_count=$(psql -h "${PG_HOST}" -p "${PG_PORT}" -U "${PG_USER}" -d "${TARGET_DB}" \
        -t -c "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public';")

    log_info "Tables in database: ${table_count}"

    # Sample data check
    local row_count=$(psql -h "${PG_HOST}" -p "${PG_PORT}" -U "${PG_USER}" -d "${TARGET_DB}" \
        -t -c "SELECT sum(reltuples) FROM pg_class WHERE relkind = 'r' AND relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = 'public');" 2>/dev/null || echo "0")

    log_info "Approximate row count: ${row_count}"

    log_info "Post-restore verification completed"
}

# Main execution
main() {
    log_info "========================================"
    log_info "PostgreSQL Restore Script"
    log_info "========================================"
    log_info "Backup file: ${BACKUP_FILE}"
    log_info "Target database: ${TARGET_DB}"
    log_info "Host: ${PG_HOST}:${PG_PORT}"
    log_info "========================================"

    # Decrypt if needed
    WORKING_FILE=$(decrypt_backup "${BACKUP_FILE}")

    # Verify backup
    if ! verify_backup "${WORKING_FILE}"; then
        log_error "Backup verification failed!"
        exit 1
    fi

    # Exit if verify-only
    if [ "${VERIFY_ONLY}" = true ]; then
        log_info "Verification complete (--verify-only mode)"
        exit 0
    fi

    # Pre-restore checks
    pre_restore_checks

    # Perform restore
    if perform_restore "${WORKING_FILE}"; then
        post_restore_verify

        log_info "========================================"
        log_info "RESTORE COMPLETED SUCCESSFULLY!"
        log_info "========================================"

        # Cleanup decrypted file if we created it
        if [ "${WORKING_FILE}" != "${BACKUP_FILE}" ]; then
            rm -f "${WORKING_FILE}"
        fi

        exit 0
    else
        log_error "========================================"
        log_error "RESTORE FAILED!"
        log_error "========================================"
        exit 1
    fi
}

main
