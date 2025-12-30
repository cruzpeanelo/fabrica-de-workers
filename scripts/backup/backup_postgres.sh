#!/bin/bash
# =============================================================================
# PostgreSQL Backup Script
# Issue #199: Automated Backup System
#
# Usage: ./backup_postgres.sh [daily|weekly|monthly|yearly]
# =============================================================================

set -e

# Configuration
BACKUP_DIR="${BACKUP_DIR:-/var/backups/fabrica}"
RETENTION_DAILY=7
RETENTION_WEEKLY=28
RETENTION_MONTHLY=365
RETENTION_YEARLY=2555

# Database connection (from environment)
PG_HOST="${PGHOST:-localhost}"
PG_PORT="${PGPORT:-5432}"
PG_USER="${PGUSER:-postgres}"
PG_DATABASE="${PGDATABASE:-factory_db}"

# Backup category (daily, weekly, monthly, yearly)
CATEGORY="${1:-daily}"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_NAME="postgres_${TIMESTAMP}.dump"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
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

# Create backup directories
create_directories() {
    log_info "Creating backup directories..."
    mkdir -p "${BACKUP_DIR}/daily"
    mkdir -p "${BACKUP_DIR}/weekly"
    mkdir -p "${BACKUP_DIR}/monthly"
    mkdir -p "${BACKUP_DIR}/yearly"
    mkdir -p "${BACKUP_DIR}/temp"
}

# Perform backup
perform_backup() {
    local output_file="${BACKUP_DIR}/${CATEGORY}/${BACKUP_NAME}"

    log_info "Starting PostgreSQL backup..."
    log_info "Database: ${PG_DATABASE}@${PG_HOST}:${PG_PORT}"
    log_info "Category: ${CATEGORY}"
    log_info "Output: ${output_file}"

    # Run pg_dump
    pg_dump \
        --host="${PG_HOST}" \
        --port="${PG_PORT}" \
        --username="${PG_USER}" \
        --dbname="${PG_DATABASE}" \
        --format=custom \
        --compress=9 \
        --file="${output_file}"

    if [ $? -eq 0 ]; then
        log_info "Backup created successfully"

        # Compress further with gzip (for non-custom format)
        # gzip -9 "${output_file}"

        # Calculate checksum
        local checksum=$(sha256sum "${output_file}" | awk '{print $1}')
        echo "${checksum}" > "${output_file}.sha256"

        log_info "Checksum: ${checksum}"

        # Report size
        local size=$(du -h "${output_file}" | awk '{print $1}')
        log_info "Backup size: ${size}"

        return 0
    else
        log_error "Backup failed!"
        return 1
    fi
}

# Encrypt backup (optional)
encrypt_backup() {
    local input_file="${BACKUP_DIR}/${CATEGORY}/${BACKUP_NAME}"
    local output_file="${input_file}.enc"

    if [ -n "${BACKUP_ENCRYPTION_KEY}" ]; then
        log_info "Encrypting backup..."

        openssl enc -aes-256-cbc -salt \
            -in "${input_file}" \
            -out "${output_file}" \
            -pass env:BACKUP_ENCRYPTION_KEY

        if [ $? -eq 0 ]; then
            rm -f "${input_file}"
            log_info "Backup encrypted successfully"
            BACKUP_NAME="${BACKUP_NAME}.enc"
        else
            log_warn "Encryption failed, keeping unencrypted backup"
        fi
    else
        log_warn "BACKUP_ENCRYPTION_KEY not set, skipping encryption"
    fi
}

# Upload to S3 (optional)
upload_to_s3() {
    local backup_file="${BACKUP_DIR}/${CATEGORY}/${BACKUP_NAME}"

    if [ -n "${S3_BUCKET}" ]; then
        log_info "Uploading to S3..."

        aws s3 cp "${backup_file}" \
            "s3://${S3_BUCKET}/backups/${CATEGORY}/${BACKUP_NAME}" \
            --sse AES256

        if [ $? -eq 0 ]; then
            log_info "Upload completed: s3://${S3_BUCKET}/backups/${CATEGORY}/${BACKUP_NAME}"
        else
            log_warn "S3 upload failed"
        fi
    else
        log_info "S3_BUCKET not set, skipping upload"
    fi
}

# Cleanup old backups based on retention
cleanup_old_backups() {
    log_info "Cleaning up old backups..."

    # Daily retention
    find "${BACKUP_DIR}/daily" -type f -mtime +${RETENTION_DAILY} -delete 2>/dev/null || true

    # Weekly retention
    find "${BACKUP_DIR}/weekly" -type f -mtime +${RETENTION_WEEKLY} -delete 2>/dev/null || true

    # Monthly retention
    find "${BACKUP_DIR}/monthly" -type f -mtime +${RETENTION_MONTHLY} -delete 2>/dev/null || true

    # Yearly retention
    find "${BACKUP_DIR}/yearly" -type f -mtime +${RETENTION_YEARLY} -delete 2>/dev/null || true

    log_info "Cleanup completed"
}

# Main execution
main() {
    log_info "========================================"
    log_info "PostgreSQL Backup Script"
    log_info "Timestamp: ${TIMESTAMP}"
    log_info "========================================"

    create_directories

    if perform_backup; then
        encrypt_backup
        upload_to_s3
        cleanup_old_backups

        log_info "========================================"
        log_info "Backup completed successfully!"
        log_info "========================================"
        exit 0
    else
        log_error "========================================"
        log_error "Backup FAILED!"
        log_error "========================================"

        # Send alert if webhook configured
        if [ -n "${ALERT_WEBHOOK}" ]; then
            curl -X POST "${ALERT_WEBHOOK}" \
                -H "Content-Type: application/json" \
                -d '{"text":"[CRITICAL] PostgreSQL backup failed for '"${PG_DATABASE}"'"}'
        fi

        exit 1
    fi
}

main
