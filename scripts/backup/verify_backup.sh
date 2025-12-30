#!/bin/bash
# =============================================================================
# Backup Verification Script
# Issue #199: Automated Backup System
#
# Usage: ./verify_backup.sh [backup_file|--all]
# =============================================================================

set -e

# Configuration
BACKUP_DIR="${BACKUP_DIR:-/var/backups/fabrica}"

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

log_ok() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

# Verify single backup file
verify_single_backup() {
    local backup_file="$1"
    local errors=0

    echo ""
    log_info "Verifying: ${backup_file}"
    echo "----------------------------------------"

    # Check file exists
    if [ ! -f "${backup_file}" ]; then
        log_fail "File not found"
        return 1
    fi
    log_ok "File exists"

    # Check file size
    local size=$(stat -f%z "${backup_file}" 2>/dev/null || stat -c%s "${backup_file}" 2>/dev/null)
    if [ "${size}" -eq 0 ]; then
        log_fail "File is empty"
        return 1
    fi
    log_ok "File size: $(numfmt --to=iec ${size} 2>/dev/null || echo ${size} bytes)"

    # Check file permissions
    if [ ! -r "${backup_file}" ]; then
        log_fail "File not readable"
        return 1
    fi
    log_ok "File readable"

    # Verify checksum if exists
    local checksum_file="${backup_file}.sha256"
    if [ -f "${checksum_file}" ]; then
        local expected=$(cat "${checksum_file}")
        local actual=$(sha256sum "${backup_file}" | awk '{print $1}')

        if [ "${expected}" == "${actual}" ]; then
            log_ok "Checksum verified"
        else
            log_fail "Checksum mismatch!"
            log_error "  Expected: ${expected}"
            log_error "  Actual: ${actual}"
            errors=$((errors + 1))
        fi
    else
        log_warn "No checksum file (${checksum_file})"
    fi

    # Verify format based on extension
    if [[ "${backup_file}" == *.dump ]]; then
        # PostgreSQL custom format
        pg_restore --list "${backup_file}" > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            log_ok "Valid PostgreSQL custom format"

            # Count objects in backup
            local object_count=$(pg_restore --list "${backup_file}" 2>/dev/null | wc -l)
            log_info "  Objects in backup: ${object_count}"
        else
            log_fail "Invalid PostgreSQL backup format"
            errors=$((errors + 1))
        fi
    elif [[ "${backup_file}" == *.sql ]] || [[ "${backup_file}" == *.sql.gz ]]; then
        # SQL format
        if [[ "${backup_file}" == *.gz ]]; then
            gzip -t "${backup_file}" 2>/dev/null
            if [ $? -eq 0 ]; then
                log_ok "Valid gzip format"
            else
                log_fail "Invalid gzip format"
                errors=$((errors + 1))
            fi
        fi
        log_ok "SQL backup format"
    elif [[ "${backup_file}" == *.tar.gz ]]; then
        # Tar archive
        tar -tzf "${backup_file}" > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            log_ok "Valid tar.gz archive"

            # Count files
            local file_count=$(tar -tzf "${backup_file}" | wc -l)
            log_info "  Files in archive: ${file_count}"
        else
            log_fail "Invalid tar.gz archive"
            errors=$((errors + 1))
        fi
    elif [[ "${backup_file}" == *.enc ]]; then
        # Encrypted backup
        log_info "Encrypted backup detected"

        if [ -z "${BACKUP_ENCRYPTION_KEY}" ]; then
            log_warn "Cannot verify encrypted content (BACKUP_ENCRYPTION_KEY not set)"
        else
            # Try to decrypt header
            openssl enc -d -aes-256-cbc \
                -in "${backup_file}" \
                -pass env:BACKUP_ENCRYPTION_KEY \
                2>/dev/null | head -c 1024 > /dev/null

            if [ $? -eq 0 ]; then
                log_ok "Encryption key valid"
            else
                log_fail "Cannot decrypt (wrong key?)"
                errors=$((errors + 1))
            fi
        fi
    elif [[ "${backup_file}" == *.db ]]; then
        # SQLite backup
        sqlite3 "${backup_file}" "PRAGMA integrity_check;" 2>/dev/null | grep -q "ok"
        if [ $? -eq 0 ]; then
            log_ok "Valid SQLite database"

            # Count tables
            local table_count=$(sqlite3 "${backup_file}" "SELECT count(*) FROM sqlite_master WHERE type='table';" 2>/dev/null)
            log_info "  Tables in database: ${table_count}"
        else
            log_fail "SQLite integrity check failed"
            errors=$((errors + 1))
        fi
    else
        log_warn "Unknown backup format"
    fi

    # Check file age
    local mtime=$(stat -f%m "${backup_file}" 2>/dev/null || stat -c%Y "${backup_file}" 2>/dev/null)
    local now=$(date +%s)
    local age_hours=$(( (now - mtime) / 3600 ))
    local age_days=$(( age_hours / 24 ))

    if [ ${age_days} -gt 0 ]; then
        log_info "Backup age: ${age_days} days"
    else
        log_info "Backup age: ${age_hours} hours"
    fi

    if [ ${errors} -eq 0 ]; then
        echo "----------------------------------------"
        log_ok "Backup verification PASSED"
        return 0
    else
        echo "----------------------------------------"
        log_fail "Backup verification FAILED (${errors} errors)"
        return 1
    fi
}

# Verify all backups in directory
verify_all_backups() {
    local dir="$1"
    local total=0
    local passed=0
    local failed=0

    log_info "Scanning backups in: ${dir}"

    # Find all backup files
    for backup_file in $(find "${dir}" -type f \( -name "*.dump" -o -name "*.sql*" -o -name "*.tar.gz" -o -name "*.db" -o -name "*.enc" \) 2>/dev/null | sort); do
        # Skip checksum files
        [[ "${backup_file}" == *.sha256 ]] && continue

        total=$((total + 1))

        if verify_single_backup "${backup_file}"; then
            passed=$((passed + 1))
        else
            failed=$((failed + 1))
        fi
    done

    echo ""
    echo "========================================"
    log_info "Verification Summary"
    echo "========================================"
    echo "Total backups: ${total}"
    echo "Passed: ${passed}"
    echo "Failed: ${failed}"
    echo "========================================"

    if [ ${failed} -gt 0 ]; then
        return 1
    fi
    return 0
}

# Generate backup report
generate_report() {
    local report_file="${BACKUP_DIR}/backup_report_$(date +%Y%m%d).txt"

    {
        echo "========================================"
        echo "Backup Verification Report"
        echo "Generated: $(date)"
        echo "========================================"
        echo ""

        for category in daily weekly monthly yearly; do
            local dir="${BACKUP_DIR}/${category}"
            if [ -d "${dir}" ]; then
                echo "## ${category^} Backups"
                echo "----------------------------------------"

                for backup_file in $(find "${dir}" -type f -name "*.dump" -o -name "*.tar.gz" 2>/dev/null | sort -r | head -5); do
                    echo "- $(basename ${backup_file})"
                    echo "  Size: $(du -h ${backup_file} | awk '{print $1}')"
                    echo "  Date: $(stat -f%Sm -t '%Y-%m-%d %H:%M' ${backup_file} 2>/dev/null || stat -c'%y' ${backup_file} 2>/dev/null | cut -d. -f1)"
                    echo ""
                done
                echo ""
            fi
        done

        echo "========================================"
        echo "Disk Usage"
        echo "========================================"
        du -sh ${BACKUP_DIR}/* 2>/dev/null || echo "N/A"

    } | tee "${report_file}"

    log_info "Report saved to: ${report_file}"
}

# Show usage
show_usage() {
    echo "Usage: $0 [OPTIONS] [backup_file]"
    echo ""
    echo "Options:"
    echo "  --all           Verify all backups in backup directory"
    echo "  --report        Generate backup status report"
    echo "  --dir <path>    Specify backup directory (default: \$BACKUP_DIR)"
    echo "  -h, --help      Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 /path/to/backup.dump"
    echo "  $0 --all"
    echo "  $0 --report"
}

# Main
main() {
    local mode="single"
    local backup_file=""

    while [[ $# -gt 0 ]]; do
        case $1 in
            --all)
                mode="all"
                shift
                ;;
            --report)
                mode="report"
                shift
                ;;
            --dir)
                BACKUP_DIR="$2"
                shift 2
                ;;
            -h|--help)
                show_usage
                exit 0
                ;;
            *)
                backup_file="$1"
                shift
                ;;
        esac
    done

    echo "========================================"
    echo "Backup Verification Script"
    echo "========================================"

    case ${mode} in
        single)
            if [ -z "${backup_file}" ]; then
                log_error "No backup file specified"
                show_usage
                exit 1
            fi
            verify_single_backup "${backup_file}"
            ;;
        all)
            verify_all_backups "${BACKUP_DIR}"
            ;;
        report)
            generate_report
            ;;
    esac
}

main "$@"
