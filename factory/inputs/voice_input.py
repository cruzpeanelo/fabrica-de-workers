"""
Voice Input - Enhanced Speech-to-Text with Browser Recording
Plataforma E v6.5 - Issue #127

Features:
- OpenAI Whisper API integration for cloud transcription
- Local Whisper model support for offline processing
- Browser MediaRecorder integration support
- Real-time transcription streaming
- Multi-language support with auto-detection
- Automatic user story conversion with Claude
"""

import os
import io
import json
import base64
import tempfile
import logging
import asyncio
from typing import Optional, List, Dict, Any, Union, AsyncGenerator
from datetime import datetime
from pathlib import Path
from dataclasses import dataclass, field
from enum import Enum

# OpenAI Whisper API
try:
    import openai
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False
    openai = None

# Local Whisper model
try:
    import whisper as local_whisper
    LOCAL_WHISPER_AVAILABLE = True
except ImportError:
    LOCAL_WHISPER_AVAILABLE = False
    local_whisper = None

# Anthropic Claude
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    Anthropic = None

# Audio processing
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    np = None

try:
    from pydub import AudioSegment
    PYDUB_AVAILABLE = True
except ImportError:
    PYDUB_AVAILABLE = False
    AudioSegment = None

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class TranscriptionProvider(str, Enum):
    """Available transcription providers"""
    OPENAI_API = "openai_api"      # OpenAI Whisper API (cloud)
    LOCAL_WHISPER = "local_whisper"  # Local Whisper model
    AUTO = "auto"                   # Auto-select best available


class AudioFormat(str, Enum):
    """Supported audio formats"""
    WEBM = "webm"
    MP3 = "mp3"
    WAV = "wav"
    M4A = "m4a"
    OGG = "ogg"
    FLAC = "flac"
    MP4 = "mp4"


@dataclass
class TranscriptionResult:
    """Result of audio transcription"""
    text: str
    language: str
    duration: float
    confidence: float = 0.0
    segments: List[Dict[str, Any]] = field(default_factory=list)
    words: List[Dict[str, Any]] = field(default_factory=list)
    provider: str = "unknown"
    processing_time: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "text": self.text,
            "language": self.language,
            "duration": self.duration,
            "confidence": self.confidence,
            "segments": self.segments,
            "words": self.words,
            "provider": self.provider,
            "processing_time": self.processing_time
        }


@dataclass
class BrowserRecordingConfig:
    """Configuration for browser-based recording"""
    sample_rate: int = 44100
    channels: int = 1
    bits_per_sample: int = 16
    mime_type: str = "audio/webm;codecs=opus"
    max_duration_seconds: int = 600  # 10 minutes
    chunk_duration_ms: int = 250     # For streaming
    noise_reduction: bool = True
    echo_cancellation: bool = True
    auto_gain_control: bool = True


class VoiceInputError(Exception):
    """Custom exception for voice input errors"""
    pass


class VoiceInput:
    """
    Enhanced Voice Input system with multiple transcription providers
    and browser recording support.

    Features:
    - OpenAI Whisper API (cloud) for high accuracy
    - Local Whisper model for offline/privacy
    - Browser MediaRecorder integration
    - Real-time streaming transcription
    - Multi-language auto-detection
    - Automatic conversion to User Stories

    Example:
        voice = VoiceInput()
        result = await voice.transcribe_audio(audio_bytes)
        stories = await voice.extract_requirements(result.text)
    """

    # Supported audio formats
    SUPPORTED_FORMATS = [".webm", ".mp3", ".wav", ".m4a", ".ogg", ".flac", ".mp4"]

    # Maximum file size (25MB for API, 100MB for local)
    MAX_FILE_SIZE_API = 25 * 1024 * 1024
    MAX_FILE_SIZE_LOCAL = 100 * 1024 * 1024

    # Default Whisper models
    WHISPER_MODELS = {
        "tiny": {"size": "tiny", "params": "39M", "speed": "~32x"},
        "base": {"size": "base", "params": "74M", "speed": "~16x"},
        "small": {"size": "small", "params": "244M", "speed": "~6x"},
        "medium": {"size": "medium", "params": "769M", "speed": "~2x"},
        "large": {"size": "large-v3", "params": "1.5B", "speed": "~1x"}
    }

    def __init__(
        self,
        provider: TranscriptionProvider = TranscriptionProvider.AUTO,
        openai_api_key: Optional[str] = None,
        local_model: str = "base",
        claude_model: Optional[str] = None,
        language: Optional[str] = None
    ):
        """
        Initialize VoiceInput.

        Args:
            provider: Transcription provider to use
            openai_api_key: OpenAI API key (or from env OPENAI_API_KEY)
            local_model: Local Whisper model size
            claude_model: Claude model for story extraction
            language: Force language (None for auto-detect)
        """
        self.provider = provider
        self.openai_api_key = openai_api_key or os.getenv("OPENAI_API_KEY")
        self.local_model_name = local_model
        self.claude_model = claude_model or CLAUDE_MODEL
        self.language = language

        # Lazy initialization
        self._openai_client = None
        self._local_model = None
        self._claude = None
        self._initialized = False

        # Recording configuration
        self.browser_config = BrowserRecordingConfig()

        # Stats
        self._stats = {
            "total_transcriptions": 0,
            "total_duration_seconds": 0,
            "total_stories_extracted": 0,
            "by_provider": {"openai_api": 0, "local_whisper": 0}
        }

    def _ensure_initialized(self, force_provider: Optional[TranscriptionProvider] = None) -> None:
        """Lazily initialize providers."""
        provider = force_provider or self.provider

        # Auto-select provider
        if provider == TranscriptionProvider.AUTO:
            if self.openai_api_key and OPENAI_AVAILABLE:
                provider = TranscriptionProvider.OPENAI_API
            elif LOCAL_WHISPER_AVAILABLE:
                provider = TranscriptionProvider.LOCAL_WHISPER
            else:
                raise VoiceInputError(
                    "No transcription provider available. Install openai or openai-whisper."
                )

        # Initialize OpenAI
        if provider == TranscriptionProvider.OPENAI_API:
            if not OPENAI_AVAILABLE:
                raise VoiceInputError(
                    "OpenAI SDK not installed. Install with: pip install openai"
                )
            if not self.openai_api_key:
                raise VoiceInputError(
                    "OPENAI_API_KEY not configured."
                )
            if not self._openai_client:
                self._openai_client = openai.OpenAI(api_key=self.openai_api_key)
                logger.info("OpenAI client initialized")

        # Initialize local Whisper
        if provider == TranscriptionProvider.LOCAL_WHISPER:
            if not LOCAL_WHISPER_AVAILABLE:
                raise VoiceInputError(
                    "Local Whisper not installed. Install with: pip install openai-whisper"
                )
            if not self._local_model:
                logger.info(f"Loading local Whisper model: {self.local_model_name}")
                self._local_model = local_whisper.load_model(self.local_model_name)
                logger.info("Local Whisper model loaded")

        # Initialize Claude
        if not self._claude and ANTHROPIC_AVAILABLE and ANTHROPIC_API_KEY:
            self._claude = Anthropic(api_key=ANTHROPIC_API_KEY)

        self._initialized = True

    def validate_audio(
        self,
        audio_data: bytes,
        filename: Optional[str] = None,
        provider: Optional[TranscriptionProvider] = None
    ) -> None:
        """Validate audio file before processing."""
        max_size = (
            self.MAX_FILE_SIZE_API
            if provider == TranscriptionProvider.OPENAI_API
            else self.MAX_FILE_SIZE_LOCAL
        )

        if len(audio_data) > max_size:
            raise VoiceInputError(
                f"Audio file too large. Maximum: {max_size / (1024*1024):.1f}MB"
            )

        if filename:
            ext = Path(filename).suffix.lower()
            if ext not in self.SUPPORTED_FORMATS:
                raise VoiceInputError(
                    f"Unsupported format: {ext}. Supported: {', '.join(self.SUPPORTED_FORMATS)}"
                )

        if len(audio_data) < 100:
            raise VoiceInputError("Audio file too small or corrupted")

    def _convert_webm_to_wav(self, audio_data: bytes) -> bytes:
        """Convert WebM audio to WAV for better compatibility."""
        if not PYDUB_AVAILABLE:
            return audio_data

        try:
            audio = AudioSegment.from_file(io.BytesIO(audio_data), format="webm")
            wav_buffer = io.BytesIO()
            audio.export(wav_buffer, format="wav")
            return wav_buffer.getvalue()
        except Exception as e:
            logger.warning(f"WebM conversion failed: {e}, using original")
            return audio_data

    async def transcribe_with_openai(
        self,
        audio_data: bytes,
        filename: str = "audio.webm"
    ) -> TranscriptionResult:
        """
        Transcribe using OpenAI Whisper API.

        Args:
            audio_data: Raw audio bytes
            filename: Filename for format detection

        Returns:
            TranscriptionResult with transcription details
        """
        self._ensure_initialized(TranscriptionProvider.OPENAI_API)

        import time
        start_time = time.time()

        try:
            # Create file-like object
            audio_file = io.BytesIO(audio_data)
            audio_file.name = filename

            # Call OpenAI API
            response = self._openai_client.audio.transcriptions.create(
                model="whisper-1",
                file=audio_file,
                language=self.language,
                response_format="verbose_json",
                timestamp_granularities=["word", "segment"]
            )

            processing_time = time.time() - start_time

            # Extract segments and words
            segments = []
            if hasattr(response, 'segments') and response.segments:
                segments = [
                    {
                        "start": seg.get("start", 0),
                        "end": seg.get("end", 0),
                        "text": seg.get("text", "").strip()
                    }
                    for seg in response.segments
                ]

            words = []
            if hasattr(response, 'words') and response.words:
                words = [
                    {
                        "start": w.get("start", 0),
                        "end": w.get("end", 0),
                        "word": w.get("word", "")
                    }
                    for w in response.words
                ]

            duration = response.duration if hasattr(response, 'duration') else 0

            result = TranscriptionResult(
                text=response.text.strip(),
                language=response.language if hasattr(response, 'language') else "unknown",
                duration=duration,
                confidence=0.95,  # OpenAI doesn't return confidence
                segments=segments,
                words=words,
                provider="openai_api",
                processing_time=processing_time
            )

            self._stats["total_transcriptions"] += 1
            self._stats["total_duration_seconds"] += duration
            self._stats["by_provider"]["openai_api"] += 1

            logger.info(
                f"OpenAI transcription: {len(result.text)} chars, "
                f"{duration:.1f}s audio, {processing_time:.2f}s processing"
            )

            return result

        except Exception as e:
            logger.error(f"OpenAI transcription failed: {e}")
            raise VoiceInputError(f"OpenAI transcription failed: {e}")

    async def transcribe_with_local(
        self,
        audio_data: bytes,
        filename: str = "audio.webm"
    ) -> TranscriptionResult:
        """
        Transcribe using local Whisper model.

        Args:
            audio_data: Raw audio bytes
            filename: Filename for format detection

        Returns:
            TranscriptionResult with transcription details
        """
        self._ensure_initialized(TranscriptionProvider.LOCAL_WHISPER)

        import time
        start_time = time.time()

        # Convert WebM if needed
        ext = Path(filename).suffix.lower()
        if ext == ".webm":
            audio_data = self._convert_webm_to_wav(audio_data)
            filename = "audio.wav"

        try:
            # Write to temp file
            with tempfile.NamedTemporaryFile(
                suffix=Path(filename).suffix,
                delete=False
            ) as temp_file:
                temp_file.write(audio_data)
                temp_path = temp_file.name

            try:
                # Transcribe
                result = self._local_model.transcribe(
                    temp_path,
                    language=self.language,
                    task="transcribe"
                )

                processing_time = time.time() - start_time

                # Extract segments
                segments = [
                    {
                        "start": seg["start"],
                        "end": seg["end"],
                        "text": seg["text"].strip()
                    }
                    for seg in result.get("segments", [])
                ]

                duration = segments[-1]["end"] if segments else 0

                transcription = TranscriptionResult(
                    text=result["text"].strip(),
                    language=result.get("language", "unknown"),
                    duration=duration,
                    confidence=0.9,
                    segments=segments,
                    words=[],
                    provider="local_whisper",
                    processing_time=processing_time
                )

                self._stats["total_transcriptions"] += 1
                self._stats["total_duration_seconds"] += duration
                self._stats["by_provider"]["local_whisper"] += 1

                logger.info(
                    f"Local transcription: {len(transcription.text)} chars, "
                    f"{duration:.1f}s audio, {processing_time:.2f}s processing"
                )

                return transcription

            finally:
                os.unlink(temp_path)

        except Exception as e:
            logger.error(f"Local transcription failed: {e}")
            raise VoiceInputError(f"Local transcription failed: {e}")

    async def transcribe_audio(
        self,
        audio_data: Union[bytes, str],
        filename: Optional[str] = None,
        provider: Optional[TranscriptionProvider] = None
    ) -> TranscriptionResult:
        """
        Transcribe audio using the configured provider.

        Args:
            audio_data: Raw audio bytes or base64-encoded string
            filename: Optional filename for format detection
            provider: Override default provider

        Returns:
            TranscriptionResult with transcription details
        """
        # Handle base64 input (from browser)
        if isinstance(audio_data, str):
            if audio_data.startswith("data:"):
                # Remove data URL prefix
                audio_data = audio_data.split(",", 1)[1]
            audio_data = base64.b64decode(audio_data)

        filename = filename or "audio.webm"
        provider = provider or self.provider

        self.validate_audio(audio_data, filename, provider)

        # Select provider
        if provider == TranscriptionProvider.AUTO:
            if self.openai_api_key and OPENAI_AVAILABLE:
                provider = TranscriptionProvider.OPENAI_API
            elif LOCAL_WHISPER_AVAILABLE:
                provider = TranscriptionProvider.LOCAL_WHISPER
            else:
                raise VoiceInputError("No transcription provider available")

        # Transcribe
        if provider == TranscriptionProvider.OPENAI_API:
            return await self.transcribe_with_openai(audio_data, filename)
        else:
            return await self.transcribe_with_local(audio_data, filename)

    async def stream_transcribe(
        self,
        audio_chunks: AsyncGenerator[bytes, None],
        chunk_duration_ms: int = 250
    ) -> AsyncGenerator[Dict[str, Any], None]:
        """
        Stream transcription for real-time processing.

        Args:
            audio_chunks: Async generator of audio chunks
            chunk_duration_ms: Duration of each chunk in ms

        Yields:
            Partial transcription results
        """
        buffer = io.BytesIO()
        chunk_count = 0

        async for chunk in audio_chunks:
            buffer.write(chunk)
            chunk_count += 1

            # Process every N chunks (e.g., every 2 seconds)
            if chunk_count >= (2000 // chunk_duration_ms):
                buffer.seek(0)
                audio_data = buffer.read()

                try:
                    result = await self.transcribe_audio(audio_data)
                    yield {
                        "type": "partial",
                        "text": result.text,
                        "confidence": result.confidence
                    }
                except Exception as e:
                    logger.warning(f"Partial transcription failed: {e}")

                # Reset buffer but keep some overlap
                buffer = io.BytesIO()
                chunk_count = 0

        # Final transcription
        buffer.seek(0)
        final_audio = buffer.read()
        if final_audio:
            try:
                result = await self.transcribe_audio(final_audio)
                yield {
                    "type": "final",
                    "text": result.text,
                    "language": result.language,
                    "duration": result.duration,
                    "confidence": result.confidence
                }
            except Exception as e:
                yield {"type": "error", "message": str(e)}

    async def extract_requirements(
        self,
        transcript: str,
        context: Optional[str] = None,
        project_name: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Extract User Stories from transcribed text using Claude.

        Args:
            transcript: Transcribed text
            context: Optional project context
            project_name: Optional project name

        Returns:
            List of extracted User Stories
        """
        if not self._claude:
            if not ANTHROPIC_AVAILABLE or not ANTHROPIC_API_KEY:
                raise VoiceInputError("Claude not available for story extraction")
            self._claude = Anthropic(api_key=ANTHROPIC_API_KEY)

        if not transcript or not transcript.strip():
            return []

        prompt = f"""Analise a seguinte transcricao de voz de um usuario descrevendo requisitos
de software e extraia User Stories no formato JSON.

{'Projeto: ' + project_name if project_name else ''}
{'Contexto: ' + context if context else ''}

Para cada requisito identificado, crie uma User Story com:
- title: Titulo curto e descritivo
- persona: Quem e o usuario (ex: "gerente", "administrador", "usuario final")
- action: O que ele quer fazer
- benefit: O beneficio esperado
- acceptance_criteria: Lista de criterios de aceite (minimo 3)
- story_points: Estimativa fibonacci (1, 2, 3, 5, 8, 13, 21)
- priority: Prioridade (low, medium, high, urgent)
- confidence: Nivel de confianca na extracao (0.0 a 1.0)

Transcricao:
---
{transcript}
---

Responda APENAS com JSON valido:
{{
    "stories": [
        {{
            "title": "...",
            "persona": "...",
            "action": "...",
            "benefit": "...",
            "acceptance_criteria": ["...", "...", "..."],
            "story_points": 5,
            "priority": "medium",
            "confidence": 0.85,
            "original_text": "trecho relevante da transcricao"
        }}
    ],
    "summary": "resumo dos requisitos identificados",
    "clarifications_needed": ["perguntas para esclarecer requisitos vagos"]
}}

Se nao identificar requisitos claros, retorne stories vazio com clarifications_needed."""

        try:
            response = self._claude.messages.create(
                model=self.claude_model,
                max_tokens=4096,
                messages=[{"role": "user", "content": prompt}]
            )

            response_text = response.content[0].text.strip()

            # Extract JSON
            if "```json" in response_text:
                json_start = response_text.find("```json") + 7
                json_end = response_text.find("```", json_start)
                response_text = response_text[json_start:json_end].strip()
            elif "```" in response_text:
                json_start = response_text.find("```") + 3
                json_end = response_text.find("```", json_start)
                response_text = response_text[json_start:json_end].strip()

            result = json.loads(response_text)
            stories = result.get("stories", [])

            # Add metadata
            for story in stories:
                story["source"] = "voice"
                story["extracted_at"] = datetime.utcnow().isoformat()

            self._stats["total_stories_extracted"] += len(stories)

            logger.info(f"Extracted {len(stories)} stories from voice transcript")
            return stories

        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse Claude response: {e}")
            return [{
                "title": "Requisitos de voz a revisar",
                "persona": "usuario",
                "action": "revisar requisitos capturados por voz",
                "benefit": "processar corretamente os requisitos",
                "acceptance_criteria": ["Revisar transcricao original"],
                "story_points": 1,
                "priority": "medium",
                "source": "voice",
                "needs_review": True,
                "original_transcript": transcript[:500],
                "extracted_at": datetime.utcnow().isoformat()
            }]
        except Exception as e:
            logger.error(f"Story extraction failed: {e}")
            raise VoiceInputError(f"Failed to extract stories: {e}")

    async def process_voice_input(
        self,
        audio_data: Union[bytes, str],
        filename: Optional[str] = None,
        context: Optional[str] = None,
        extract_stories: bool = True
    ) -> Dict[str, Any]:
        """
        Complete pipeline: audio -> transcription -> user stories.

        Args:
            audio_data: Raw audio bytes or base64 string
            filename: Optional filename
            context: Optional project context
            extract_stories: Whether to extract stories

        Returns:
            Dictionary with transcription and stories
        """
        import time
        start_time = time.time()

        # Transcribe
        transcription = await self.transcribe_audio(audio_data, filename)

        # Extract stories if requested
        stories = []
        if extract_stories and transcription.text:
            stories = await self.extract_requirements(transcription.text, context)

        processing_time = time.time() - start_time

        return {
            "transcription": transcription.to_dict(),
            "stories": stories,
            "stories_count": len(stories),
            "processing_time": round(processing_time, 2),
            "source": "voice",
            "processed_at": datetime.utcnow().isoformat()
        }

    def get_browser_recording_config(self) -> Dict[str, Any]:
        """
        Get configuration for browser-based recording.

        Returns:
            Configuration dictionary for browser MediaRecorder
        """
        return {
            "audio": {
                "sampleRate": self.browser_config.sample_rate,
                "channelCount": self.browser_config.channels,
                "echoCancellation": self.browser_config.echo_cancellation,
                "noiseSuppression": self.browser_config.noise_reduction,
                "autoGainControl": self.browser_config.auto_gain_control
            },
            "mimeType": self.browser_config.mime_type,
            "maxDurationSeconds": self.browser_config.max_duration_seconds,
            "chunkDurationMs": self.browser_config.chunk_duration_ms
        }

    def get_status(self) -> Dict[str, Any]:
        """Get current status of VoiceInput system."""
        return {
            "initialized": self._initialized,
            "provider": self.provider.value,
            "openai_available": OPENAI_AVAILABLE and bool(self.openai_api_key),
            "local_whisper_available": LOCAL_WHISPER_AVAILABLE,
            "claude_available": ANTHROPIC_AVAILABLE and bool(ANTHROPIC_API_KEY),
            "local_model": self.local_model_name,
            "language": self.language or "auto-detect",
            "supported_formats": self.SUPPORTED_FORMATS,
            "browser_config": self.get_browser_recording_config(),
            "stats": self._stats
        }

    def get_supported_formats(self) -> List[str]:
        """Return list of supported audio formats."""
        return self.SUPPORTED_FORMATS.copy()


# Browser JavaScript for recording (to be served with the dashboard)
BROWSER_RECORDING_JS = """
class VoiceRecorder {
    constructor(options = {}) {
        this.mediaRecorder = null;
        this.audioChunks = [];
        this.stream = null;
        this.isRecording = false;
        this.onDataAvailable = options.onDataAvailable || null;
        this.onStop = options.onStop || null;
        this.onError = options.onError || null;
        this.config = options.config || {
            audio: {
                sampleRate: 44100,
                channelCount: 1,
                echoCancellation: true,
                noiseSuppression: true,
                autoGainControl: true
            },
            mimeType: 'audio/webm;codecs=opus'
        };
    }

    async start() {
        try {
            this.stream = await navigator.mediaDevices.getUserMedia({
                audio: this.config.audio
            });

            this.mediaRecorder = new MediaRecorder(this.stream, {
                mimeType: this.config.mimeType
            });

            this.audioChunks = [];

            this.mediaRecorder.ondataavailable = (event) => {
                if (event.data.size > 0) {
                    this.audioChunks.push(event.data);
                    if (this.onDataAvailable) {
                        this.onDataAvailable(event.data);
                    }
                }
            };

            this.mediaRecorder.onstop = () => {
                const audioBlob = new Blob(this.audioChunks, { type: this.config.mimeType });
                if (this.onStop) {
                    this.onStop(audioBlob);
                }
            };

            this.mediaRecorder.onerror = (event) => {
                if (this.onError) {
                    this.onError(event.error);
                }
            };

            this.mediaRecorder.start(250); // 250ms chunks
            this.isRecording = true;
            return true;

        } catch (error) {
            if (this.onError) {
                this.onError(error);
            }
            return false;
        }
    }

    stop() {
        if (this.mediaRecorder && this.isRecording) {
            this.mediaRecorder.stop();
            this.stream.getTracks().forEach(track => track.stop());
            this.isRecording = false;
        }
    }

    getAudioBlob() {
        return new Blob(this.audioChunks, { type: this.config.mimeType });
    }

    async blobToBase64(blob) {
        return new Promise((resolve, reject) => {
            const reader = new FileReader();
            reader.onloadend = () => resolve(reader.result);
            reader.onerror = reject;
            reader.readAsDataURL(blob);
        });
    }
}

// Usage:
// const recorder = new VoiceRecorder({
//     onStop: async (blob) => {
//         const base64 = await recorder.blobToBase64(blob);
//         // Send to server
//         fetch('/api/voice/transcribe', {
//             method: 'POST',
//             headers: { 'Content-Type': 'application/json' },
//             body: JSON.stringify({ audio: base64 })
//         });
//     }
// });
// recorder.start();
// setTimeout(() => recorder.stop(), 5000);
"""
