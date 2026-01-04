# -*- coding: utf-8 -*-
"""
RBAC Integration for Dashboard
Plataforma E v6.1

Provides endpoints and UI for RBAC management
Can be imported and registered with the main app
"""

from fastapi import APIRouter, HTTPException, Depends, Request
from fastapi.responses import HTMLResponse
from typing import Optional, List
from datetime import datetime

# Import RBAC
from factory.auth.rbac import (
    RBACManager, rbac_router, require_permission, require_role,
    get_current_user, check_permission, DEFAULT_ROLES, RESOURCES, ACTIONS,
    UserContext
)
from factory.database.connection import SessionLocal


def register_rbac(app):
    """Register RBAC router and admin page with the app"""

    # Include the RBAC API router
    app.include_router(rbac_router)
    print("[Dashboard] RBAC router included")

    # Add admin roles page
    @app.get("/admin/roles", response_class=HTMLResponse)
    async def admin_roles_page():
        """Admin page for managing roles and permissions"""
        return get_admin_roles_html()

    # Initialize default roles endpoint
    @app.on_event("startup")
    async def init_rbac():
        """Initialize default roles on startup"""
        db = SessionLocal()
        try:
            rbac = RBACManager(db)
            rbac.init_default_roles()
        except Exception as e:
            print(f"[RBAC] Error initializing roles: {e}")
        finally:
            db.close()


def get_admin_roles_html():
    """Returns the admin roles management page HTML"""
    return '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Gerenciador de Acessos e Perfis - RBAC</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <style>
        body { font-family: 'Inter', sans-serif; }
        .permission-cell { transition: all 0.2s; }
        .permission-cell:hover { transform: scale(1.1); }
        .fade-enter-active, .fade-leave-active { transition: opacity 0.3s; }
        .fade-enter-from, .fade-leave-to { opacity: 0; }
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <div id="app">
        <!-- Header -->
        <header class="bg-[#003B4A] text-white shadow-lg">
            <div class="max-w-7xl mx-auto px-4 py-4 flex items-center justify-between">
                <div class="flex items-center space-x-4">
                    <a href="/" class="text-white hover:text-orange-300 transition">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"></path>
                        </svg>
                    </a>
                    <h1 class="text-2xl font-bold">Gerenciador de Acessos e Perfis</h1>
                </div>
                <div class="flex items-center space-x-4">
                    <span class="text-sm opacity-75">RBAC v1.0</span>
                </div>
            </div>
        </header>

        <!-- Main Content -->
        <main class="max-w-7xl mx-auto px-4 py-8">
            <!-- Tabs -->
            <div class="bg-white rounded-lg shadow-md mb-6">
                <div class="border-b border-gray-200">
                    <nav class="flex -mb-px">
                        <button
                            v-for="tab in tabs"
                            :key="tab.id"
                            @click="activeTab = tab.id"
                            :class="[
                                'px-6 py-4 text-sm font-medium transition',
                                activeTab === tab.id
                                    ? 'border-b-2 border-[#FF6C00] text-[#FF6C00]'
                                    : 'text-gray-500 hover:text-gray-700 hover:border-gray-300'
                            ]"
                        >
                            {{ tab.name }}
                        </button>
                    </nav>
                </div>
            </div>

            <!-- Roles Tab -->
            <div v-if="activeTab === 'roles'" class="space-y-6">
                <!-- Actions Bar -->
                <div class="flex justify-between items-center">
                    <h2 class="text-xl font-semibold text-gray-800">Roles (Perfis de Acesso)</h2>
                    <button
                        @click="showNewRoleModal = true"
                        class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600 transition flex items-center space-x-2"
                    >
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path>
                        </svg>
                        <span>Nova Role</span>
                    </button>
                </div>

                <!-- Roles Grid -->
                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                    <div
                        v-for="role in roles"
                        :key="role.role_id"
                        class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition cursor-pointer"
                        @click="selectRole(role)"
                    >
                        <div class="flex items-start justify-between">
                            <div>
                                <h3 class="text-lg font-semibold text-gray-800">{{ role.name }}</h3>
                                <p class="text-sm text-gray-500 mt-1">{{ role.description }}</p>
                            </div>
                            <span
                                v-if="role.is_system"
                                class="px-2 py-1 text-xs bg-blue-100 text-blue-700 rounded-full"
                            >
                                Sistema
                            </span>
                        </div>
                        <div class="mt-4 flex items-center justify-between">
                            <span class="text-sm text-gray-500">
                                Level: {{ role.level }}
                            </span>
                            <span class="text-sm text-gray-500">
                                {{ role.permissions?.length || 0 }} permissoes
                            </span>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Permission Matrix Tab -->
            <div v-if="activeTab === 'matrix'" class="space-y-6">
                <h2 class="text-xl font-semibold text-gray-800">Matriz de Permissoes</h2>

                <div class="bg-white rounded-lg shadow-md overflow-hidden">
                    <div class="overflow-x-auto">
                        <table class="min-w-full divide-y divide-gray-200">
                            <thead class="bg-gray-50">
                                <tr>
                                    <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider sticky left-0 bg-gray-50 z-10">
                                        Recurso
                                    </th>
                                    <th
                                        v-for="role in roles"
                                        :key="role.role_id"
                                        class="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider min-w-[120px]"
                                    >
                                        {{ role.name }}
                                    </th>
                                </tr>
                            </thead>
                            <tbody class="bg-white divide-y divide-gray-200">
                                <tr v-for="resource in resources" :key="resource" class="hover:bg-gray-50">
                                    <td class="px-4 py-3 whitespace-nowrap text-sm font-medium text-gray-900 sticky left-0 bg-white">
                                        {{ resource }}
                                    </td>
                                    <td
                                        v-for="role in roles"
                                        :key="role.role_id"
                                        class="px-4 py-3 whitespace-nowrap text-center"
                                    >
                                        <div class="flex justify-center space-x-1">
                                            <span
                                                v-for="action in ['C', 'R', 'U', 'D']"
                                                :key="action"
                                                :class="[
                                                    'w-6 h-6 rounded text-xs flex items-center justify-center permission-cell cursor-pointer',
                                                    hasPermission(role, resource, action)
                                                        ? 'bg-green-500 text-white'
                                                        : 'bg-gray-200 text-gray-400'
                                                ]"
                                                :title="getActionName(action)"
                                                @click="togglePermission(role, resource, action)"
                                            >
                                                {{ action }}
                                            </span>
                                        </div>
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <div class="flex items-center space-x-4 text-sm text-gray-500">
                    <span class="flex items-center"><span class="w-4 h-4 bg-green-500 rounded mr-2"></span> Permitido</span>
                    <span class="flex items-center"><span class="w-4 h-4 bg-gray-200 rounded mr-2"></span> Negado</span>
                    <span>C=Create, R=Read, U=Update, D=Delete</span>
                </div>
            </div>

            <!-- Users Tab -->
            <div v-if="activeTab === 'users'" class="space-y-6">
                <div class="flex justify-between items-center">
                    <h2 class="text-xl font-semibold text-gray-800">Usuarios e Atribuicoes</h2>
                    <button
                        @click="showAssignRoleModal = true"
                        class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600 transition flex items-center space-x-2"
                    >
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z"></path>
                        </svg>
                        <span>Atribuir Role</span>
                    </button>
                </div>

                <div class="bg-white rounded-lg shadow-md overflow-hidden">
                    <table class="min-w-full divide-y divide-gray-200">
                        <thead class="bg-gray-50">
                            <tr>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Usuario</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Role</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Projeto</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Atribuido Em</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Acoes</th>
                            </tr>
                        </thead>
                        <tbody class="divide-y divide-gray-200">
                            <tr v-for="ur in userRoles" :key="ur.id" class="hover:bg-gray-50">
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span class="text-sm text-gray-900">User #{{ ur.user_id }}</span>
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span class="px-2 py-1 text-xs bg-blue-100 text-blue-700 rounded-full">
                                        {{ ur.role?.name || ur.role_id }}
                                    </span>
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span class="text-sm text-gray-500">{{ ur.project_id || 'Global' }}</span>
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span class="text-sm text-gray-500">{{ formatDate(ur.assigned_at) }}</span>
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <button
                                        @click="revokeUserRole(ur)"
                                        class="text-red-600 hover:text-red-800"
                                    >
                                        Revogar
                                    </button>
                                </td>
                            </tr>
                            <tr v-if="userRoles.length === 0">
                                <td colspan="5" class="px-6 py-8 text-center text-gray-500">
                                    Nenhuma atribuicao encontrada
                                </td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            </div>

            <!-- Audit Tab -->
            <div v-if="activeTab === 'audit'" class="space-y-6">
                <h2 class="text-xl font-semibold text-gray-800">Log de Auditoria</h2>

                <div class="bg-white rounded-lg shadow-md overflow-hidden">
                    <table class="min-w-full divide-y divide-gray-200">
                        <thead class="bg-gray-50">
                            <tr>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Data/Hora</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Usuario</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Acao</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Recurso</th>
                                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Status</th>
                            </tr>
                        </thead>
                        <tbody class="divide-y divide-gray-200">
                            <tr v-for="log in auditLogs" :key="log.id" class="hover:bg-gray-50">
                                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                    {{ formatDate(log.timestamp) }}
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                                    {{ log.username || 'Sistema' }}
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span class="px-2 py-1 text-xs bg-gray-100 text-gray-700 rounded">
                                        {{ log.action }}
                                    </span>
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                    {{ log.resource }} {{ log.resource_id ? '#' + log.resource_id : '' }}
                                </td>
                                <td class="px-6 py-4 whitespace-nowrap">
                                    <span :class="[
                                        'px-2 py-1 text-xs rounded-full',
                                        log.success ? 'bg-green-100 text-green-700' : 'bg-red-100 text-red-700'
                                    ]">
                                        {{ log.success ? 'Sucesso' : 'Falha' }}
                                    </span>
                                </td>
                            </tr>
                            <tr v-if="auditLogs.length === 0">
                                <td colspan="5" class="px-6 py-8 text-center text-gray-500">
                                    Nenhum log encontrado
                                </td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            </div>
        </main>

        <!-- New Role Modal -->
        <div v-if="showNewRoleModal" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div class="bg-white rounded-lg shadow-xl w-full max-w-md p-6">
                <h3 class="text-lg font-semibold mb-4">Nova Role</h3>
                <form @submit.prevent="createRole">
                    <div class="space-y-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Nome</label>
                            <input
                                v-model="newRole.name"
                                type="text"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                placeholder="Ex: SUPERVISOR"
                                required
                            >
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                            <textarea
                                v-model="newRole.description"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                rows="2"
                                placeholder="Descricao da role..."
                            ></textarea>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Nivel</label>
                            <input
                                v-model.number="newRole.level"
                                type="number"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                min="0"
                                max="100"
                            >
                        </div>
                    </div>
                    <div class="mt-6 flex justify-end space-x-3">
                        <button
                            type="button"
                            @click="showNewRoleModal = false"
                            class="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50"
                        >
                            Cancelar
                        </button>
                        <button
                            type="submit"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600"
                        >
                            Criar Role
                        </button>
                    </div>
                </form>
            </div>
        </div>

        <!-- Assign Role Modal -->
        <div v-if="showAssignRoleModal" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div class="bg-white rounded-lg shadow-xl w-full max-w-md p-6">
                <h3 class="text-lg font-semibold mb-4">Atribuir Role</h3>
                <form @submit.prevent="assignRole">
                    <div class="space-y-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">ID do Usuario</label>
                            <input
                                v-model.number="assignData.user_id"
                                type="number"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                placeholder="ID do usuario"
                                required
                            >
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Role</label>
                            <select
                                v-model="assignData.role_id"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                required
                            >
                                <option value="">Selecione uma role</option>
                                <option v-for="role in roles" :key="role.role_id" :value="role.role_id">
                                    {{ role.name }}
                                </option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Projeto (opcional)</label>
                            <input
                                v-model="assignData.project_id"
                                type="text"
                                class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-orange-500 focus:border-orange-500"
                                placeholder="ID do projeto (deixe vazio para global)"
                            >
                        </div>
                    </div>
                    <div class="mt-6 flex justify-end space-x-3">
                        <button
                            type="button"
                            @click="showAssignRoleModal = false"
                            class="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50"
                        >
                            Cancelar
                        </button>
                        <button
                            type="submit"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600"
                        >
                            Atribuir
                        </button>
                    </div>
                </form>
            </div>
        </div>

        <!-- Role Detail Modal -->
        <div v-if="selectedRole" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div class="bg-white rounded-lg shadow-xl w-full max-w-2xl p-6 max-h-[90vh] overflow-y-auto">
                <div class="flex justify-between items-start mb-4">
                    <div>
                        <h3 class="text-lg font-semibold">{{ selectedRole.name }}</h3>
                        <p class="text-sm text-gray-500">{{ selectedRole.description }}</p>
                    </div>
                    <button @click="selectedRole = null" class="text-gray-400 hover:text-gray-600">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
                        </svg>
                    </button>
                </div>

                <div class="space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-2">Permissoes</label>
                        <div class="flex flex-wrap gap-2">
                            <span
                                v-for="perm in selectedRole.permissions"
                                :key="perm"
                                class="px-3 py-1 bg-blue-100 text-blue-700 rounded-full text-sm"
                            >
                                {{ perm }}
                            </span>
                        </div>
                    </div>

                    <div v-if="!selectedRole.is_system" class="pt-4 border-t">
                        <h4 class="text-sm font-medium text-gray-700 mb-2">Editar Permissoes</h4>
                        <div class="grid grid-cols-2 gap-2 max-h-60 overflow-y-auto">
                            <label
                                v-for="resource in resources"
                                :key="resource"
                                class="flex items-center space-x-2 text-sm"
                            >
                                <input
                                    type="checkbox"
                                    :checked="selectedRole.permissions?.includes(resource + ':*')"
                                    @change="toggleResourcePermission(resource)"
                                    class="rounded text-orange-500 focus:ring-orange-500"
                                >
                                <span>{{ resource }}:*</span>
                            </label>
                        </div>
                    </div>
                </div>

                <div class="mt-6 flex justify-between">
                    <button
                        v-if="!selectedRole.is_system"
                        @click="deleteRole(selectedRole)"
                        class="px-4 py-2 text-red-600 hover:text-red-800"
                    >
                        Excluir Role
                    </button>
                    <div class="flex space-x-3">
                        <button
                            @click="selectedRole = null"
                            class="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50"
                        >
                            Fechar
                        </button>
                        <button
                            v-if="!selectedRole.is_system"
                            @click="saveRole"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600"
                        >
                            Salvar
                        </button>
                    </div>
                </div>
            </div>
        </div>

        <!-- Toast Notifications -->
        <div class="fixed bottom-4 right-4 space-y-2 z-50">
            <transition-group name="fade">
                <div
                    v-for="toast in toasts"
                    :key="toast.id"
                    :class="[
                        'px-4 py-3 rounded-lg shadow-lg text-white flex items-center space-x-2',
                        toast.type === 'success' ? 'bg-green-500' :
                        toast.type === 'error' ? 'bg-red-500' : 'bg-blue-500'
                    ]"
                >
                    <span>{{ toast.message }}</span>
                </div>
            </transition-group>
        </div>
    </div>

    <script>
        const { createApp, ref, onMounted, computed } = Vue;

        createApp({
            setup() {
                // State
                const activeTab = ref('roles');
                const tabs = [
                    { id: 'roles', name: 'Roles' },
                    { id: 'matrix', name: 'Matriz de Permissoes' },
                    { id: 'users', name: 'Usuarios' },
                    { id: 'audit', name: 'Auditoria' }
                ];

                const roles = ref([]);
                const userRoles = ref([]);
                const auditLogs = ref([]);
                const resources = ref([]);
                const actions = ref([]);
                const permissionMatrix = ref({});

                const selectedRole = ref(null);
                const showNewRoleModal = ref(false);
                const showAssignRoleModal = ref(false);

                const newRole = ref({ name: '', description: '', level: 25, permissions: [] });
                const assignData = ref({ user_id: null, role_id: '', project_id: '' });

                const toasts = ref([]);
                let toastId = 0;

                // Methods
                const addToast = (type, message) => {
                    const id = ++toastId;
                    toasts.value.push({ id, type, message });
                    setTimeout(() => {
                        toasts.value = toasts.value.filter(t => t.id !== id);
                    }, 3000);
                };

                const loadRoles = async () => {
                    try {
                        const res = await fetch('/api/rbac/roles');
                        roles.value = await res.json();
                    } catch (e) {
                        console.error('Error loading roles:', e);
                    }
                };

                const loadPermissions = async () => {
                    try {
                        const res = await fetch('/api/rbac/permissions');
                        const data = await res.json();
                        resources.value = data.resources;
                        actions.value = data.actions;
                    } catch (e) {
                        console.error('Error loading permissions:', e);
                    }
                };

                const loadMatrix = async () => {
                    try {
                        const res = await fetch('/api/rbac/permissions/matrix');
                        permissionMatrix.value = await res.json();
                    } catch (e) {
                        console.error('Error loading matrix:', e);
                    }
                };

                const loadAuditLogs = async () => {
                    try {
                        const res = await fetch('/api/rbac/audit?limit=50');
                        if (res.ok) {
                            auditLogs.value = await res.json();
                        }
                    } catch (e) {
                        console.error('Error loading audit logs:', e);
                    }
                };

                const selectRole = (role) => {
                    selectedRole.value = { ...role };
                };

                const createRole = async () => {
                    try {
                        const res = await fetch('/api/rbac/roles', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(newRole.value)
                        });
                        if (res.ok) {
                            addToast('success', 'Role criada com sucesso');
                            showNewRoleModal.value = false;
                            newRole.value = { name: '', description: '', level: 25, permissions: [] };
                            loadRoles();
                        } else {
                            const err = await res.json();
                            addToast('error', err.detail || 'Erro ao criar role');
                        }
                    } catch (e) {
                        addToast('error', 'Erro ao criar role');
                    }
                };

                const saveRole = async () => {
                    if (!selectedRole.value) return;
                    try {
                        const res = await fetch(`/api/rbac/roles/${selectedRole.value.role_id}`, {
                            method: 'PUT',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({
                                permissions: selectedRole.value.permissions
                            })
                        });
                        if (res.ok) {
                            addToast('success', 'Role atualizada');
                            selectedRole.value = null;
                            loadRoles();
                            loadMatrix();
                        } else {
                            const err = await res.json();
                            addToast('error', err.detail || 'Erro ao atualizar role');
                        }
                    } catch (e) {
                        addToast('error', 'Erro ao atualizar role');
                    }
                };

                const deleteRole = async (role) => {
                    if (!confirm(`Excluir role ${role.name}?`)) return;
                    try {
                        const res = await fetch(`/api/rbac/roles/${role.role_id}`, {
                            method: 'DELETE'
                        });
                        if (res.ok) {
                            addToast('success', 'Role excluida');
                            selectedRole.value = null;
                            loadRoles();
                        } else {
                            const err = await res.json();
                            addToast('error', err.detail || 'Erro ao excluir role');
                        }
                    } catch (e) {
                        addToast('error', 'Erro ao excluir role');
                    }
                };

                const assignRole = async () => {
                    try {
                        const data = { ...assignData.value };
                        if (!data.project_id) delete data.project_id;

                        const res = await fetch(`/api/rbac/users/${data.user_id}/roles`, {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(data)
                        });
                        if (res.ok) {
                            addToast('success', 'Role atribuida');
                            showAssignRoleModal.value = false;
                            assignData.value = { user_id: null, role_id: '', project_id: '' };
                            loadUserRoles();
                        } else {
                            const err = await res.json();
                            addToast('error', err.detail || 'Erro ao atribuir role');
                        }
                    } catch (e) {
                        addToast('error', 'Erro ao atribuir role');
                    }
                };

                const loadUserRoles = async () => {
                    // This would need an endpoint to list all user roles
                    // For now, we'll leave it empty
                    userRoles.value = [];
                };

                const revokeUserRole = async (ur) => {
                    if (!confirm('Revogar esta atribuicao?')) return;
                    try {
                        const url = `/api/rbac/users/${ur.user_id}/roles/${ur.role_id}` +
                            (ur.project_id ? `?project_id=${ur.project_id}` : '');
                        const res = await fetch(url, { method: 'DELETE' });
                        if (res.ok) {
                            addToast('success', 'Role revogada');
                            loadUserRoles();
                        }
                    } catch (e) {
                        addToast('error', 'Erro ao revogar role');
                    }
                };

                const hasPermission = (role, resource, actionLetter) => {
                    const actionMap = { 'C': 'create', 'R': 'read', 'U': 'update', 'D': 'delete' };
                    const action = actionMap[actionLetter];
                    const perms = role.permissions || [];

                    return perms.includes('*:*') ||
                           perms.includes(`${resource}:*`) ||
                           perms.includes(`${resource}:${action}`) ||
                           perms.includes(`${resource}:manage`);
                };

                const getActionName = (letter) => {
                    const names = { 'C': 'Create', 'R': 'Read', 'U': 'Update', 'D': 'Delete' };
                    return names[letter];
                };

                const togglePermission = (role, resource, actionLetter) => {
                    if (role.is_system) {
                        addToast('error', 'Nao e possivel editar roles do sistema');
                        return;
                    }
                    // Would need to implement toggle logic and save
                    addToast('info', 'Use o modal de detalhes para editar permissoes');
                };

                const toggleResourcePermission = (resource) => {
                    if (!selectedRole.value) return;
                    const perm = `${resource}:*`;
                    const perms = selectedRole.value.permissions || [];
                    const idx = perms.indexOf(perm);
                    if (idx >= 0) {
                        perms.splice(idx, 1);
                    } else {
                        perms.push(perm);
                    }
                    selectedRole.value.permissions = [...perms];
                };

                const formatDate = (isoString) => {
                    if (!isoString) return '-';
                    return new Date(isoString).toLocaleString('pt-BR');
                };

                // Initialize
                onMounted(async () => {
                    await loadRoles();
                    await loadPermissions();
                    await loadMatrix();
                    await loadAuditLogs();
                });

                return {
                    activeTab,
                    tabs,
                    roles,
                    userRoles,
                    auditLogs,
                    resources,
                    actions,
                    permissionMatrix,
                    selectedRole,
                    showNewRoleModal,
                    showAssignRoleModal,
                    newRole,
                    assignData,
                    toasts,
                    selectRole,
                    createRole,
                    saveRole,
                    deleteRole,
                    assignRole,
                    revokeUserRole,
                    hasPermission,
                    getActionName,
                    togglePermission,
                    toggleResourcePermission,
                    formatDate
                };
            }
        }).mount('#app');
    </script>
</body>
</html>'''


# =============================================================================
# Issue #187 - Embedded RBAC Components for Main Dashboard
# =============================================================================

def get_rbac_sidebar_section() -> str:
    """Returns HTML for the Admin section in sidebar. Only visible for ADMIN users."""
    return '''
                    <!-- Admin Section (Issue #187 - RBAC Integration) -->
                    <div class="mt-6 pt-4 border-t border-gray-200" v-if="currentUserIsAdmin">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">
                            <svg class="w-4 h-4 inline mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/>
                            </svg>
                            Administracao
                        </h3>
                        <button @click="showAdminRolesModal = true; loadRbacData()" class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"/></svg>
                            Gerenciar Roles
                        </button>
                        <button @click="showAdminUsersModal = true; loadRbacUsers()" class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"/></svg>
                            Gerenciar Usuarios
                        </button>
                        <button @click="showAuditLogModal = true; loadAuditLogs()" class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"/></svg>
                            Log de Auditoria
                        </button>
                    </div>
    '''


def get_rbac_vue_state() -> str:
    """Returns Vue.js state declarations for RBAC admin."""
    return '''
            // ========== ISSUE #187 - RBAC ADMIN STATE ==========
            const currentUserIsAdmin = ref(true); // TODO: Get from auth context
            const showAdminRolesModal = ref(false);
            const showAdminUsersModal = ref(false);
            const showAuditLogModal = ref(false);
            const showNewRoleForm = ref(false);
            const showAssignRoleForm = ref(false);
            const selectedRoleForEdit = ref(null);
            const rbacActiveTab = ref('roles');
            const rbacLoading = ref(false);
            const rbacRoles = ref([]);
            const rbacResources = ref([]);
            const rbacUserRoles = ref([]);
            const rbacAuditLogs = ref([]);
            const roleForm = ref({ name: '', description: '', level: 25, permissions: [], is_system: false });
            const assignRoleForm = ref({ user_id: null, role_id: '', project_id: '' });
    '''


def get_rbac_vue_methods() -> str:
    """Returns Vue.js methods for RBAC admin functionality."""
    return '''
            // ========== ISSUE #187 - RBAC ADMIN METHODS ==========
            const loadRbacData = async () => { rbacLoading.value = true; try { const [rolesRes, permsRes] = await Promise.all([fetch('/api/rbac/roles'), fetch('/api/rbac/permissions')]); if (rolesRes.ok) rbacRoles.value = await rolesRes.json(); if (permsRes.ok) { const permsData = await permsRes.json(); rbacResources.value = permsData.resources || []; } } catch (e) { console.error('RBAC error:', e); } finally { rbacLoading.value = false; } };
            const loadRbacUsers = async () => { rbacLoading.value = true; try { const res = await fetch('/api/admin/rbac/users'); if (res.ok) rbacUserRoles.value = await res.json(); } catch (e) { rbacUserRoles.value = []; } finally { rbacLoading.value = false; } };
            const loadAuditLogs = async () => { rbacLoading.value = true; try { const res = await fetch('/api/rbac/audit?limit=50'); if (res.ok) rbacAuditLogs.value = await res.json(); } catch (e) { rbacAuditLogs.value = []; } finally { rbacLoading.value = false; } };
            const selectRoleForEdit = (role) => { selectedRoleForEdit.value = role; roleForm.value = { name: role.name, description: role.description || '', level: role.level || 25, permissions: [...(role.permissions || [])], is_system: role.is_system || false }; };
            const closeRoleForm = () => { showNewRoleForm.value = false; selectedRoleForEdit.value = null; roleForm.value = { name: '', description: '', level: 25, permissions: [], is_system: false }; };
            const saveRbacRole = async () => { try { const isEdit = selectedRoleForEdit.value !== null; const url = isEdit ? `/api/rbac/roles/${selectedRoleForEdit.value.role_id}` : '/api/rbac/roles'; const res = await fetch(url, { method: isEdit ? 'PUT' : 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(roleForm.value) }); if (res.ok) { addToast('success', isEdit ? 'Role atualizada' : 'Role criada'); closeRoleForm(); loadRbacData(); } } catch (e) { addToast('error', 'Erro', e.message); } };
            const deleteRbacRole = async (role) => { if (!confirm('Excluir role ' + role.name + '?')) return; try { const res = await fetch('/api/rbac/roles/' + role.role_id, { method: 'DELETE' }); if (res.ok) { addToast('success', 'Role excluida'); closeRoleForm(); loadRbacData(); } } catch (e) { addToast('error', 'Erro', e.message); } };
            const assignRoleToUser = async () => { try { const data = { ...assignRoleForm.value }; if (!data.project_id) delete data.project_id; const res = await fetch('/api/rbac/users/' + data.user_id + '/roles', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(data) }); if (res.ok) { addToast('success', 'Role atribuida'); showAssignRoleForm.value = false; assignRoleForm.value = { user_id: null, role_id: '', project_id: '' }; loadRbacUsers(); } } catch (e) { addToast('error', 'Erro', e.message); } };
            const revokeUserRole = async (ur) => { if (!confirm('Revogar role?')) return; try { const res = await fetch('/api/rbac/users/' + ur.user_id + '/roles/' + ur.role_id, { method: 'DELETE' }); if (res.ok) { addToast('success', 'Role revogada'); loadRbacUsers(); } } catch (e) { addToast('error', 'Erro', e.message); } };
            const hasRolePermission = (role, resource, actionLetter) => { const actionMap = { 'C': 'create', 'R': 'read', 'U': 'update', 'D': 'delete' }; const action = actionMap[actionLetter]; const perms = role.permissions || []; return perms.includes('*:*') || perms.includes(resource + ':*') || perms.includes(resource + ':' + action); };
            const getActionLabel = (letter) => { return { 'C': 'Create', 'R': 'Read', 'U': 'Update', 'D': 'Delete' }[letter]; };
            const formatRbacDateTime = (isoString) => { if (!isoString) return '-'; return new Date(isoString).toLocaleString('pt-BR'); };
    '''


def get_rbac_vue_return() -> str:
    """Returns Vue.js return statement additions for RBAC admin."""
    return '''
                // Issue #187 - RBAC Admin
                currentUserIsAdmin, showAdminRolesModal, showAdminUsersModal, showAuditLogModal, showNewRoleForm, showAssignRoleForm, selectedRoleForEdit, rbacActiveTab, rbacLoading, rbacRoles, rbacResources, rbacUserRoles, rbacAuditLogs, roleForm, assignRoleForm, loadRbacData, loadRbacUsers, loadAuditLogs, selectRoleForEdit, closeRoleForm, saveRbacRole, deleteRbacRole, assignRoleToUser, revokeUserRole, hasRolePermission, getActionLabel, formatRbacDateTime,'''
