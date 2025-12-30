# -*- coding: utf-8 -*-
"""
Issue #155 - Voice Input Feature
Apply all changes to add voice input button for story creation
"""
import re

def apply_changes():
    # Read file
    with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
        content = f.read()

    changes_made = []

    # 1. Add CSS styles for voice input
    css_marker = '.view-mode-btn.active {'
    voice_css = '''
        /* ===================== ISSUE #155 - VOICE INPUT ===================== */
        .voice-input-btn {
            display: inline-flex;
            align-items: center;
            justify-content: center;
            width: 40px;
            height: 40px;
            border: none;
            border-radius: 8px;
            background: #F3F4F6;
            color: #6B7280;
            cursor: pointer;
            transition: all 0.2s ease;
            flex-shrink: 0;
        }
        .voice-input-btn:hover {
            background: #E5E7EB;
            color: #003B4A;
        }
        .voice-input-btn.recording {
            background: #FEE2E2;
            color: #EF4444;
            animation: pulse-recording 1.5s ease-in-out infinite;
        }
        .voice-input-btn:disabled {
            opacity: 0.5;
            cursor: not-allowed;
        }
        @keyframes pulse-recording {
            0%, 100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(239, 68, 68, 0.4); }
            50% { transform: scale(1.05); box-shadow: 0 0 0 8px rgba(239, 68, 68, 0); }
        }
        .recording-indicator {
            display: inline-flex;
            align-items: center;
            gap: 8px;
            padding: 6px 12px;
            background: #FEE2E2;
            border-radius: 20px;
            font-size: 0.75rem;
            color: #B91C1C;
            font-weight: 500;
        }
        .recording-dot {
            width: 8px;
            height: 8px;
            background: #EF4444;
            border-radius: 50%;
            animation: blink-recording 1s ease-in-out infinite;
        }
        @keyframes blink-recording {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.3; }
        }
        .voice-status-text {
            font-size: 0.75rem;
            color: #6B7280;
            margin-top: 4px;
        }
        .voice-status-text.error { color: #EF4444; }
        .voice-status-text.success { color: #10B981; }
        .input-with-voice {
            display: flex;
            gap: 8px;
            align-items: flex-start;
        }
        .input-with-voice textarea { flex: 1; }
        .voice-processing {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            font-size: 0.75rem;
            color: #6B7280;
        }'''

    if '.voice-input-btn' not in content:
        # Find the closing of view-mode-btn.active style
        pattern = r'(\.view-mode-btn\.active \{[^}]+\})\s*\n\s*</style>'
        match = re.search(pattern, content)
        if match:
            content = content.replace(match.group(0), match.group(1) + voice_css + '\n    </style>')
            changes_made.append('CSS styles added')

    # 2. Add voice recording state
    state_marker = 'const newDesign = ref({ title:'
    voice_state = '''
            // Issue #155 - Voice Input State
            const voiceRecording = ref({
                isSupported: false,
                isRecording: false,
                isProcessing: false,
                targetField: null,
                duration: 0,
                statusMessage: '',
                statusType: '',
                recognition: null,
                mediaRecorder: null,
                audioChunks: [],
                durationInterval: null
            });
'''
    if 'voiceRecording = ref({' not in content:
        idx = content.find(state_marker)
        if idx != -1:
            # Find end of newDesign line
            end_idx = content.find('\n', idx)
            content = content[:end_idx+1] + voice_state + content[end_idx+1:]
            changes_made.append('Voice state added')

    # 3. Add voice input HTML to description field
    old_description_html = '''                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newStory.description" rows="3"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                  placeholder="Detalhes adicionais..."></textarea>
                    </div>

                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Criterios de Aceite (um por linha)</label>
                        <textarea v-model="newStoryCriteria" rows="3"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                  placeholder="Usuario pode fazer login com email&#10;Senha deve ter minimo 8 caracteres"></textarea>
                    </div>'''

    new_description_html = '''                    <div>
                        <div class="flex items-center justify-between mb-1">
                            <label class="block text-sm font-medium text-gray-700">Descricao</label>
                            <div v-if="voiceRecording.isRecording" class="recording-indicator">
                                <span class="recording-dot"></span>
                                <span>Gravando... {{ voiceRecording.duration }}s</span>
                            </div>
                            <div v-else-if="voiceRecording.isProcessing" class="voice-processing">
                                <div class="spinner spinner-sm"></div>
                                <span>Processando audio...</span>
                            </div>
                        </div>
                        <div class="input-with-voice">
                            <textarea v-model="newStory.description" rows="3"
                                      class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                      placeholder="Detalhes adicionais... (ou use o microfone para ditar)"></textarea>
                            <button type="button"
                                    @click="toggleVoiceRecording('description')"
                                    :class="['voice-input-btn', { 'recording': voiceRecording.isRecording && voiceRecording.targetField === 'description' }]"
                                    :disabled="!voiceRecording.isSupported || voiceRecording.isProcessing"
                                    :title="voiceRecording.isSupported ? (voiceRecording.isRecording ? 'Parar gravacao' : 'Iniciar gravacao por voz') : 'Navegador nao suporta reconhecimento de voz'">
                                <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <path d="M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z"></path>
                                    <path d="M19 10v2a7 7 0 0 1-14 0v-2"></path>
                                    <line x1="12" x2="12" y1="19" y2="22"></line>
                                </svg>
                            </button>
                        </div>
                        <p v-if="voiceRecording.statusMessage"
                           :class="['voice-status-text', voiceRecording.statusType]">
                            {{ voiceRecording.statusMessage }}
                        </p>
                    </div>

                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Criterios de Aceite (um por linha)</label>
                        <div class="input-with-voice">
                            <textarea v-model="newStoryCriteria" rows="3"
                                      class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                      placeholder="Usuario pode fazer login com email&#10;Senha deve ter minimo 8 caracteres"></textarea>
                            <button type="button"
                                    @click="toggleVoiceRecording('criteria')"
                                    :class="['voice-input-btn', { 'recording': voiceRecording.isRecording && voiceRecording.targetField === 'criteria' }]"
                                    :disabled="!voiceRecording.isSupported || voiceRecording.isProcessing"
                                    :title="voiceRecording.isSupported ? (voiceRecording.isRecording ? 'Parar gravacao' : 'Ditar criterios por voz') : 'Navegador nao suporta reconhecimento de voz'">
                                <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <path d="M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z"></path>
                                    <path d="M19 10v2a7 7 0 0 1-14 0v-2"></path>
                                    <line x1="12" x2="12" y1="19" y2="22"></line>
                                </svg>
                            </button>
                        </div>
                    </div>'''

    if 'toggleVoiceRecording' not in content:
        content = content.replace(old_description_html, new_description_html)
        changes_made.append('HTML voice buttons added')

    # 4. Add voice functions after applyTemplate
    voice_functions = '''
            // ===================== Issue #155 - Voice Input Functions =====================
            const initVoiceRecording = () => {
                const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
                voiceRecording.value.isSupported = !!SpeechRecognition && !!navigator.mediaDevices?.getUserMedia;
                if (voiceRecording.value.isSupported) {
                    voiceRecording.value.recognition = new SpeechRecognition();
                    voiceRecording.value.recognition.continuous = true;
                    voiceRecording.value.recognition.interimResults = true;
                    voiceRecording.value.recognition.lang = 'pt-BR';
                    voiceRecording.value.recognition.onresult = (event) => {
                        let finalTranscript = '';
                        for (let i = event.resultIndex; i < event.results.length; i++) {
                            if (event.results[i].isFinal) finalTranscript += event.results[i][0].transcript + ' ';
                        }
                        if (finalTranscript && voiceRecording.value.targetField) {
                            if (voiceRecording.value.targetField === 'description') newStory.value.description += finalTranscript;
                            else if (voiceRecording.value.targetField === 'criteria') newStoryCriteria.value += finalTranscript;
                        }
                    };
                    voiceRecording.value.recognition.onerror = (event) => {
                        voiceRecording.value.statusMessage = 'Erro: ' + event.error;
                        voiceRecording.value.statusType = 'error';
                        stopVoiceRecording();
                    };
                    voiceRecording.value.recognition.onend = () => { if (voiceRecording.value.isRecording) voiceRecording.value.recognition.start(); };
                }
            };
            const toggleVoiceRecording = async (targetField) => {
                if (!voiceRecording.value.isSupported) { voiceRecording.value.statusMessage = 'Navegador nao suporta voz'; voiceRecording.value.statusType = 'error'; return; }
                voiceRecording.value.isRecording ? stopVoiceRecording() : await startVoiceRecording(targetField);
            };
            const startVoiceRecording = async (targetField) => {
                try {
                    await navigator.mediaDevices.getUserMedia({ audio: true });
                    voiceRecording.value.targetField = targetField;
                    voiceRecording.value.isRecording = true;
                    voiceRecording.value.duration = 0;
                    voiceRecording.value.statusMessage = 'Fale agora...';
                    voiceRecording.value.statusType = '';
                    voiceRecording.value.audioChunks = [];
                    voiceRecording.value.recognition.start();
                    voiceRecording.value.durationInterval = setInterval(() => { voiceRecording.value.duration++; }, 1000);
                    const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
                    voiceRecording.value.mediaRecorder = new MediaRecorder(stream);
                    voiceRecording.value.mediaRecorder.ondataavailable = (e) => { if (e.data.size > 0) voiceRecording.value.audioChunks.push(e.data); };
                    voiceRecording.value.mediaRecorder.onstop = async () => { stream.getTracks().forEach(t => t.stop()); if (voiceRecording.value.audioChunks.length > 0) await sendAudioToBackend(); };
                    voiceRecording.value.mediaRecorder.start(1000);
                    addToast('info', 'Gravacao iniciada', 'Fale para ditar');
                } catch (e) { voiceRecording.value.statusMessage = 'Erro microfone: ' + e.message; voiceRecording.value.statusType = 'error'; voiceRecording.value.isRecording = false; }
            };
            const stopVoiceRecording = () => {
                voiceRecording.value.isRecording = false;
                if (voiceRecording.value.durationInterval) { clearInterval(voiceRecording.value.durationInterval); voiceRecording.value.durationInterval = null; }
                if (voiceRecording.value.recognition) voiceRecording.value.recognition.stop();
                if (voiceRecording.value.mediaRecorder?.state !== 'inactive') voiceRecording.value.mediaRecorder?.stop();
                voiceRecording.value.statusMessage = 'Gravacao finalizada';
                voiceRecording.value.statusType = 'success';
                addToast('success', 'Gravacao concluida', 'Texto transcrito');
                setTimeout(() => { voiceRecording.value.statusMessage = ''; voiceRecording.value.statusType = ''; }, 3000);
            };
            const sendAudioToBackend = async () => {
                try {
                    voiceRecording.value.isProcessing = true;
                    voiceRecording.value.statusMessage = 'Processando com IA...';
                    const audioBlob = new Blob(voiceRecording.value.audioChunks, { type: 'audio/webm' });
                    const formData = new FormData();
                    formData.append('audio', audioBlob, 'voice_input.webm');
                    if (newStory.value.title) formData.append('context', 'Story: ' + newStory.value.title);
                    const response = await fetch('/api/v1/inputs/voice', { method: 'POST', body: formData });
                    if (response.ok) {
                        const result = await response.json();
                        if (result.stories?.length > 0) {
                            const s = result.stories[0];
                            if (s.title && !newStory.value.title) newStory.value.title = s.title;
                            if (s.persona && !newStory.value.persona) newStory.value.persona = s.persona;
                            if (s.action && !newStory.value.action) newStory.value.action = s.action;
                            if (s.benefit && !newStory.value.benefit) newStory.value.benefit = s.benefit;
                            if (s.acceptance_criteria?.length) newStoryCriteria.value = s.acceptance_criteria.join('\\n');
                            voiceRecording.value.statusMessage = 'IA preencheu campos!';
                            addToast('success', 'IA processou', 'Campos preenchidos');
                        }
                    }
                    voiceRecording.value.statusType = 'success';
                } catch (e) { voiceRecording.value.statusMessage = 'Transcricao OK'; voiceRecording.value.statusType = 'success'; }
                finally { voiceRecording.value.isProcessing = false; voiceRecording.value.audioChunks = []; setTimeout(() => { voiceRecording.value.statusMessage = ''; }, 5000); }
            };
'''

    if 'initVoiceRecording' not in content:
        # Insert before Dark Mode
        old_marker = '            };\n\n            // Dark Mode'
        new_marker = '            };' + voice_functions + '\n            // Dark Mode'
        content = content.replace(old_marker, new_marker, 1)
        changes_made.append('Voice functions added')

    # 5. Add initVoiceRecording to onMounted
    if 'initVoiceRecording();' not in content:
        content = content.replace(
            'initTerminal();\n                loadUserTenants();',
            'initTerminal();\n                initVoiceRecording(); // Issue #155 - Voice Input\n                loadUserTenants();'
        )
        changes_made.append('onMounted init added')

    # 6. Add to return statement
    if 'voiceRecording, toggleVoiceRecording' not in content:
        old_return = '''                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh
            };
        }
    }).mount('#app');'''
        new_return = '''                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh,
                // Issue #155 - Voice Input
                voiceRecording, toggleVoiceRecording
            };
        }
    }).mount('#app');'''
        content = content.replace(old_return, new_return, 1)
        changes_made.append('Return statement updated')

    # Write file
    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)

    if changes_made:
        print(f'Changes applied: {", ".join(changes_made)}')
    else:
        print('All changes already present')

if __name__ == '__main__':
    apply_changes()
