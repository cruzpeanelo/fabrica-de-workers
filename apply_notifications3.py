# -*- coding: utf-8 -*-
"""Script to add notification badge and panel to the header"""

# Read the file
with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Pattern to find - notification sound toggle section
old_pattern = '''                        <!-- Notification Sound Toggle -->
                        <button @click="toggleNotificationSound" class="text-white/70 hover:text-white p-1" :title="notificationSoundEnabled ? 'Desativar som' : 'Ativar som'">
                            <span v-if="notificationSoundEnabled">🔔</span>
                            <span v-else>🔕</span>
                        </button>

                        <!-- Dark Mode Toggle -->'''

new_text = '''                        <!-- Notifications Panel -->
                        <div class="relative">
                            <button @click="toggleNotificationsPanel" class="relative text-white/70 hover:text-white p-2" title="Notificacoes">
                                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
                                </svg>
                                <span v-if="unreadNotificationCount > 0" class="absolute -top-1 -right-1 bg-red-500 text-white text-xs rounded-full w-5 h-5 flex items-center justify-center font-bold">
                                    {{ unreadNotificationCount > 9 ? '9+' : unreadNotificationCount }}
                                </span>
                            </button>
                            <!-- Notifications Dropdown -->
                            <div v-if="showNotificationsPanel" class="absolute right-0 mt-2 w-80 bg-white rounded-lg shadow-xl z-50 max-h-96 overflow-hidden" @click.stop>
                                <div class="p-3 border-b flex justify-between items-center bg-gray-50">
                                    <span class="font-semibold text-gray-800">Notificacoes</span>
                                    <button v-if="unreadNotificationCount > 0" @click="markAllNotificationsRead" class="text-xs text-blue-600 hover:text-blue-800">
                                        Marcar todas como lidas
                                    </button>
                                </div>
                                <div class="overflow-y-auto max-h-72">
                                    <div v-if="notifications.length === 0" class="p-4 text-center text-gray-500">
                                        Nenhuma notificacao
                                    </div>
                                    <div v-for="n in notifications" :key="n.id"
                                         :class="['p-3 border-b hover:bg-gray-50 cursor-pointer transition', !n.read ? 'bg-blue-50' : '']"
                                         @click="markNotificationRead(n.id)">
                                        <div class="flex items-start gap-2">
                                            <span class="text-lg">{{ getNotificationIcon(n.type) }}</span>
                                            <div class="flex-1 min-w-0">
                                                <p class="text-sm font-medium text-gray-900 truncate">{{ getNotificationTitle(n.type) }}</p>
                                                <p class="text-xs text-gray-600 truncate">{{ getNotificationText(n) }}</p>
                                                <p class="text-xs text-gray-400 mt-1">{{ formatNotificationTime(n.timestamp) }}</p>
                                            </div>
                                            <span v-if="!n.read" class="w-2 h-2 bg-blue-500 rounded-full flex-shrink-0"></span>
                                        </div>
                                    </div>
                                </div>
                                <div class="p-2 border-t bg-gray-50">
                                    <button @click="showNotificationPreferences = true; showNotificationsPanel = false" class="w-full text-xs text-gray-600 hover:text-gray-800 py-1">
                                        Configurar notificacoes
                                    </button>
                                </div>
                            </div>
                        </div>

                        <!-- Notification Sound Toggle -->
                        <button @click="toggleNotificationSound" class="text-white/70 hover:text-white p-1" :title="notificationSoundEnabled ? 'Desativar som' : 'Ativar som'">
                            <span v-if="notificationSoundEnabled">🔔</span>
                            <span v-else>🔕</span>
                        </button>

                        <!-- Dark Mode Toggle -->'''

if old_pattern in content:
    content = content.replace(old_pattern, new_text)
    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)
    print('SUCCESS: Notification badge and panel added to header')
else:
    print('WARNING: Pattern not found exactly')
    # Check for variations
    if 'Notification Sound Toggle' in content:
        print('Found Notification Sound Toggle but pattern does not match')
        # Show actual content
        idx = content.find('<!-- Notification Sound Toggle -->')
        print(f'Content around marker: {content[idx-50:idx+200]}')
