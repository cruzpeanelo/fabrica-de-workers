# -*- coding: utf-8 -*-
"""Painel de Administracao de Usuarios (#87) - Plataforma E v6.5"""
from datetime import datetime
from typing import Optional, List
from enum import Enum
import hashlib, secrets
from fastapi import APIRouter, Depends, HTTPException, Query
from pydantic import BaseModel, EmailStr
from sqlalchemy.orm import Session
from factory.database.connection import get_db

class UserRole(str, Enum):
    ADMIN = "admin"
    MANAGER = "manager"
    DEVELOPER = "developer"
    VIEWER = "viewer"
    BILLING = "billing"

class UserStatus(str, Enum):
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"
    PENDING = "pending"

class UserBase(BaseModel):
    email: EmailStr
    name: str
    role: UserRole = UserRole.VIEWER
    tenant_id: Optional[str] = None

class UserCreate(UserBase):
    password: str

class UserUpdate(BaseModel):
    name: Optional[str] = None
    email: Optional[EmailStr] = None
    role: Optional[UserRole] = None
    status: Optional[UserStatus] = None

class UserResponse(UserBase):
    id: str
    status: UserStatus
    created_at: datetime
    last_login: Optional[datetime] = None
    class Config:
        from_attributes = True

class UserListResponse(BaseModel):
    users: List[UserResponse]
    total: int
    page: int
    page_size: int

class PasswordResetRequest(BaseModel):
    new_password: str

class BulkActionRequest(BaseModel):
    user_ids: List[str]
    action: str

class UserStatsResponse(BaseModel):
    total_users: int
    active_users: int
    inactive_users: int
    suspended_users: int
    users_by_role: dict
    new_users_today: int
    new_users_week: int

class UserAdminService:
    def __init__(self, db: Session):
        self.db = db

    def _hash_password(self, password: str):
        salt = secrets.token_hex(16)
        hash_obj = hashlib.pbkdf2_hmac("sha256", password.encode(), salt.encode(), 100000)
        return hash_obj.hex(), salt

    def list_users(self, page=1, page_size=20, role=None, status=None, search=None, tenant_id=None):
        from factory.database.models import User
        query = self.db.query(User)
        if role: query = query.filter(User.role == role.value)
        if status: query = query.filter(User.status == status.value)
        if tenant_id: query = query.filter(User.tenant_id == tenant_id)
        if search:
            sf = "%" + search + "%"
            query = query.filter((User.name.ilike(sf)) | (User.email.ilike(sf)))
        total = query.count()
        users = query.offset((page - 1) * page_size).limit(page_size).all()
        return {"users": users, "total": total, "page": page, "page_size": page_size}

    def get_user(self, user_id: str):
        from factory.database.models import User
        return self.db.query(User).filter(User.id == user_id).first()

    def create_user(self, user_data: UserCreate):
        from factory.database.models import User
        existing = self.db.query(User).filter(User.email == user_data.email).first()
        if existing:
            raise HTTPException(status_code=400, detail="Email ja cadastrado")
        password_hash, salt = self._hash_password(user_data.password)
        user = User(email=user_data.email, name=user_data.name, role=user_data.role.value,
                    status=UserStatus.ACTIVE.value, password_hash=password_hash,
                    password_salt=salt, tenant_id=user_data.tenant_id, created_at=datetime.utcnow())
        self.db.add(user)
        self.db.commit()
        self.db.refresh(user)
        return user

    def update_user(self, user_id: str, user_data: UserUpdate):
        from factory.database.models import User
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            raise HTTPException(status_code=404, detail="Usuario nao encontrado")
        for field, value in user_data.model_dump(exclude_unset=True).items():
            if value is not None:
                if isinstance(value, Enum): value = value.value
                setattr(user, field, value)
        user.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(user)
        return user

    def delete_user(self, user_id: str):
        from factory.database.models import User
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            raise HTTPException(status_code=404, detail="Usuario nao encontrado")
        self.db.delete(user)
        self.db.commit()
        return True

    def reset_password(self, user_id: str, new_password: str):
        from factory.database.models import User
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            raise HTTPException(status_code=404, detail="Usuario nao encontrado")
        password_hash, salt = self._hash_password(new_password)
        user.password_hash = password_hash
        user.password_salt = salt
        user.updated_at = datetime.utcnow()
        self.db.commit()
        return True

    def change_role(self, user_id: str, new_role: UserRole):
        from factory.database.models import User
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            raise HTTPException(status_code=404, detail="Usuario nao encontrado")
        user.role = new_role.value
        user.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(user)
        return user

    def bulk_action(self, user_ids: List[str], action: str):
        from factory.database.models import User
        affected = 0
        for uid in user_ids:
            user = self.db.query(User).filter(User.id == uid).first()
            if not user: continue
            if action == "activate": user.status = UserStatus.ACTIVE.value
            elif action == "deactivate": user.status = UserStatus.INACTIVE.value
            elif action == "suspend": user.status = UserStatus.SUSPENDED.value
            elif action == "delete": self.db.delete(user)
            affected += 1
        self.db.commit()
        return {"action": action, "requested": len(user_ids), "affected": affected}

    def get_statistics(self, tenant_id=None):
        from factory.database.models import User
        from datetime import timedelta
        query = self.db.query(User)
        if tenant_id: query = query.filter(User.tenant_id == tenant_id)
        total = query.count()
        active = query.filter(User.status == UserStatus.ACTIVE.value).count()
        inactive = query.filter(User.status == UserStatus.INACTIVE.value).count()
        suspended = query.filter(User.status == UserStatus.SUSPENDED.value).count()
        users_by_role = {r.value: query.filter(User.role == r.value).count() for r in UserRole}
        today = datetime.utcnow().replace(hour=0, minute=0, second=0, microsecond=0)
        return {"total_users": total, "active_users": active, "inactive_users": inactive,
                "suspended_users": suspended, "users_by_role": users_by_role,
                "new_users_today": query.filter(User.created_at >= today).count(),
                "new_users_week": query.filter(User.created_at >= today - timedelta(days=7)).count()}

user_admin_router = APIRouter(prefix="/admin/users", tags=["User Administration"])

@user_admin_router.get("", response_model=UserListResponse)
async def list_users(page: int = Query(1, ge=1), page_size: int = Query(20, ge=1, le=100),
                     role: Optional[UserRole] = None, status: Optional[UserStatus] = None,
                     search: Optional[str] = None, tenant_id: Optional[str] = None,
                     db: Session = Depends(get_db)):
    return UserAdminService(db).list_users(page, page_size, role, status, search, tenant_id)

@user_admin_router.get("/stats", response_model=UserStatsResponse)
async def get_user_statistics(tenant_id: Optional[str] = None, db: Session = Depends(get_db)):
    return UserAdminService(db).get_statistics(tenant_id)

@user_admin_router.get("/{user_id}", response_model=UserResponse)
async def get_user(user_id: str, db: Session = Depends(get_db)):
    user = UserAdminService(db).get_user(user_id)
    if not user: raise HTTPException(status_code=404, detail="Usuario nao encontrado")
    return user

@user_admin_router.post("", response_model=UserResponse)
async def create_user(user_data: UserCreate, db: Session = Depends(get_db)):
    return UserAdminService(db).create_user(user_data)

@user_admin_router.put("/{user_id}", response_model=UserResponse)
async def update_user(user_id: str, user_data: UserUpdate, db: Session = Depends(get_db)):
    return UserAdminService(db).update_user(user_id, user_data)

@user_admin_router.delete("/{user_id}")
async def delete_user(user_id: str, db: Session = Depends(get_db)):
    UserAdminService(db).delete_user(user_id)
    return {"message": "Usuario deletado com sucesso"}

@user_admin_router.post("/{user_id}/reset-password")
async def reset_password(user_id: str, request: PasswordResetRequest, db: Session = Depends(get_db)):
    UserAdminService(db).reset_password(user_id, request.new_password)
    return {"message": "Senha resetada com sucesso"}

@user_admin_router.patch("/{user_id}/role")
async def change_user_role(user_id: str, role: UserRole, db: Session = Depends(get_db)):
    return UserAdminService(db).change_role(user_id, role)

@user_admin_router.post("/bulk-action")
async def bulk_action(request: BulkActionRequest, db: Session = Depends(get_db)):
    return UserAdminService(db).bulk_action(request.user_ids, request.action)
