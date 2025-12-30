# -*- coding: utf-8 -*-
"""Script to add voice input functions to app_v6_agile.py"""
import re

# Read file
with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
    content = f.read()

voice_code = '''
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

# Check if voice code already exists
if 'initVoiceRecording' not in content:
    # Insert before Dark Mode
    old_text = '            };\n\n            // Dark Mode'
    new_text = '            };\n' + voice_code + '\n            // Dark Mode'
    content = content.replace(old_text, new_text, 1)

    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)
    print('Voice functions added successfully')
else:
    print('Voice functions already exist')
