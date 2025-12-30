#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Apply SafeErrorHandler to all route files - Issue #172

This script updates all API route files to use SafeErrorHandler
instead of exposing raw exception messages via str(e).

Run: python apply_safe_error_handler.py
"""

import os
import re

def update_route_file(file_path):
    """Update a route file with SafeErrorHandler"""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    original_content = content

    # Check if already has SafeErrorHandler import
    has_import = 'from factory.api.error_handler import SafeErrorHandler' in content

    if not has_import:
        # Add import before router definition
        if 'router = APIRouter' in content:
            content = content.replace(
                'router = APIRouter',
                '# Safe error handling (Issue #172)\nfrom factory.api.error_handler import SafeErrorHandler\n\nrouter = APIRouter'
            )

    # Replace various patterns of exception handling that expose str(e)

    # Pattern 1: raise HTTPException(500, f"...: {str(e)}")
    content = re.sub(
        r'raise HTTPException\(500, f"[^"]*\{str\(e\)\}"\)',
        'raise HTTPException(500, SafeErrorHandler.sanitize_error(e))',
        content
    )

    # Pattern 2: raise HTTPException(500, str(e))
    content = re.sub(
        r'raise HTTPException\(500, str\(e\)\)',
        'raise HTTPException(500, SafeErrorHandler.sanitize_error(e))',
        content
    )

    # Pattern 3: raise HTTPException(status_code=500, detail=str(e))
    content = re.sub(
        r'raise HTTPException\(status_code=500, detail=str\(e\)\)',
        'raise HTTPException(status_code=500, detail=SafeErrorHandler.sanitize_error(e))',
        content
    )

    # Pattern 4: raise HTTPException(status_code=400, detail=str(e))
    content = re.sub(
        r'raise HTTPException\(status_code=400, detail=str\(e\)\)',
        'raise HTTPException(status_code=400, detail=SafeErrorHandler.sanitize_error(e))',
        content
    )

    # Pattern 5: raise HTTPException(500, f"...: {e}")
    content = re.sub(
        r'raise HTTPException\(500, f"[^"]*\{e\}"\)',
        'raise HTTPException(500, SafeErrorHandler.sanitize_error(e))',
        content
    )

    # Pattern 6: raise HTTPException(status_code=500, detail=f"...: {e}")
    content = re.sub(
        r'raise HTTPException\(status_code=500, detail=f"[^"]*\{e\}"\)',
        'raise HTTPException(status_code=500, detail=SafeErrorHandler.sanitize_error(e))',
        content
    )

    if content != original_content:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        print(f'Updated: {file_path}')
        return True
    else:
        print(f'No changes: {file_path}')
        return False


def main():
    # List of route files to update
    route_files = [
        'factory/api/admin_routes.py',
        'factory/api/analytics_routes.py',
        'factory/api/ab_testing_routes.py',
        'factory/api/code_review_routes.py',
        'factory/api/docker_routes.py',
        'factory/api/integration_routes.py',
        'factory/api/mfa_routes.py',
        'factory/api/input_routes.py',
        'factory/api/preview_routes.py',
        'factory/api/public_api_v1.py',
        'factory/api/project_preview.py',
        'factory/api/security_routes.py',
        'factory/api/tenant_routes.py',
        'factory/api/test_routes.py',
        'factory/api/tech_debt_routes.py',
        'factory/api/webhook_routes.py',
        'factory/api/portal_routes.py',
    ]

    updated_count = 0
    for file_path in route_files:
        if os.path.exists(file_path):
            if update_route_file(file_path):
                updated_count += 1
        else:
            print(f'Not found: {file_path}')

    print(f'\nTotal files updated: {updated_count}')
    print('\nDone! All route files have been updated with SafeErrorHandler.')


if __name__ == '__main__':
    main()
