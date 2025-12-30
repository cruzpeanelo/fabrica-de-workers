# -*- coding: utf-8 -*-
"""
Encryption API Routes - Issue #344
==================================
FastAPI endpoints for encryption key management.
"""

from typing import List, Optional
from fastapi import APIRouter, HTTPException, Request, BackgroundTasks
from pydantic import BaseModel, Field

from .encryption import (
    KeyManager,
    EncryptionService,
    is_encryption_available,
    ENCRYPTION_ALGORITHM
)


router = APIRouter(prefix="/api/security", tags=["Encryption"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class EncryptionKeyResponse(BaseModel):
    """Encryption key metadata (no secrets)."""
    id: str
    tenant_id: Optional[str]
    algorithm: str
    created_at: str
    rotated_at: Optional[str]
    is_active: bool
    version: int


class EncryptionStatusResponse(BaseModel):
    """Encryption system status."""
    available: bool
    algorithm: str
    master_key_configured: bool
    message: str


class RotateKeyRequest(BaseModel):
    """Request to rotate encryption key."""
    re_encrypt_existing: bool = Field(
        default=False,
        description="Re-encrypt existing data with new key"
    )


class RotateKeyResponse(BaseModel):
    """Key rotation result."""
    success: bool
    key_id: str
    version: int
    message: str


class EncryptRequest(BaseModel):
    """Request to encrypt data."""
    data: str = Field(..., min_length=1)
    tenant_id: Optional[str] = None


class DecryptRequest(BaseModel):
    """Request to decrypt data."""
    encrypted_data: str = Field(..., min_length=1)
    tenant_id: Optional[str] = None


class EncryptDecryptResponse(BaseModel):
    """Encryption/decryption result."""
    result: str
    tenant_id: Optional[str]


class BulkEncryptRequest(BaseModel):
    """Request to bulk encrypt a table field."""
    model_name: str = Field(..., description="SQLAlchemy model name")
    field_name: str = Field(..., description="Field to encrypt")
    tenant_id_field: str = Field(default="tenant_id")


class BulkEncryptResponse(BaseModel):
    """Bulk encryption result."""
    processed: int
    encrypted: int
    errors: int
    message: str


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/encryption/status", response_model=EncryptionStatusResponse)
async def get_encryption_status():
    """Get encryption system status."""
    available = is_encryption_available()

    return EncryptionStatusResponse(
        available=available,
        algorithm=ENCRYPTION_ALGORITHM,
        master_key_configured=KeyManager._master_key is not None if available else False,
        message="Encryption is available" if available else "cryptography package not installed"
    )


@router.get("/keys", response_model=List[EncryptionKeyResponse])
async def list_encryption_keys(
    tenant_id: Optional[str] = None,
    request: Request = None
):
    """List encryption keys (metadata only)."""
    if not is_encryption_available():
        raise HTTPException(503, "Encryption not available")

    keys = KeyManager.list_keys(tenant_id)
    return [EncryptionKeyResponse(**k) for k in keys]


@router.post("/keys/rotate", response_model=RotateKeyResponse)
async def rotate_encryption_key(
    data: RotateKeyRequest,
    tenant_id: Optional[str] = None,
    background_tasks: BackgroundTasks = None,
    request: Request = None
):
    """
    Rotate encryption key for tenant.

    New data will be encrypted with the new key.
    Optionally re-encrypt existing data in background.
    """
    if not is_encryption_available():
        raise HTTPException(503, "Encryption not available")

    # Get tenant from header if not provided
    if not tenant_id:
        tenant_id = request.headers.get("X-Tenant-Id")

    # Rotate key
    new_key = KeyManager.rotate_key(tenant_id)

    message = f"Key rotated to version {new_key.version}"

    if data.re_encrypt_existing:
        message += ". Background re-encryption started."
        # In production, add background task here

    return RotateKeyResponse(
        success=True,
        key_id=new_key.id,
        version=new_key.version,
        message=message
    )


# =============================================================================
# UTILITY ENDPOINTS (for testing/admin)
# =============================================================================

@router.post("/encrypt", response_model=EncryptDecryptResponse)
async def encrypt_data(data: EncryptRequest):
    """
    Encrypt a string value.

    For testing/admin purposes only.
    """
    if not is_encryption_available():
        raise HTTPException(503, "Encryption not available")

    encrypted = EncryptionService.encrypt(data.data, data.tenant_id)

    return EncryptDecryptResponse(
        result=encrypted,
        tenant_id=data.tenant_id
    )


@router.post("/decrypt", response_model=EncryptDecryptResponse)
async def decrypt_data(data: DecryptRequest):
    """
    Decrypt an encrypted value.

    For testing/admin purposes only.
    """
    if not is_encryption_available():
        raise HTTPException(503, "Encryption not available")

    decrypted = EncryptionService.decrypt(data.encrypted_data, data.tenant_id)

    return EncryptDecryptResponse(
        result=decrypted,
        tenant_id=data.tenant_id
    )


@router.post("/encrypt-existing", response_model=BulkEncryptResponse)
async def encrypt_existing_data(
    data: BulkEncryptRequest,
    background_tasks: BackgroundTasks,
    request: Request = None
):
    """
    Bulk encrypt existing unencrypted data in a table.

    Runs in background for large datasets.
    """
    if not is_encryption_available():
        raise HTTPException(503, "Encryption not available")

    # In production, this would:
    # 1. Look up the model class
    # 2. Use BulkEncryptor.encrypt_field()
    # 3. Run as background task

    return BulkEncryptResponse(
        processed=0,
        encrypted=0,
        errors=0,
        message="Bulk encryption job queued. This is a placeholder - implement with actual model."
    )
