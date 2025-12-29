"""
Voice Capture - Speech-to-Text for Requirements Capture
Fabrica de Agentes v6.5 - Issue #127

Uses OpenAI Whisper for transcription and Claude for requirements extraction.
"""

import os
import io
import json
import tempfile
import logging
from typing import Optional, List, Dict, Any
from datetime import datetime
from pathlib import Path

try:
    import whisper
    WHISPER_AVAILABLE = True
except ImportError:
    WHISPER_AVAILABLE = False
    whisper = None

try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    Anthropic = None

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class VoiceCaptureError(Exception):
    """Custom exception for voice capture errors"""
    pass


class VoiceCapture:
    """
    Voice-to-Requirements capture system.

    Uses OpenAI Whisper for speech-to-text transcription and
    Claude for intelligent requirements extraction into User Stories.

    Example:
        voice = VoiceCapture()
        stories = await voice.process_voice_input(audio_bytes)
    """

    # Supported audio formats
    SUPPORTED_FORMATS = [".mp3", ".wav", ".m4a", ".webm", ".ogg", ".flac", ".mp4"]

    # Maximum audio duration in seconds (10 minutes)
    MAX_DURATION_SECONDS = 600

    # Maximum file size in bytes (25MB)
    MAX_FILE_SIZE = 25 * 1024 * 1024

    def __init__(
        self,
        whisper_model: str = "base",
        claude_model: Optional[str] = None,
        language: Optional[str] = None
    ):
        """
        Initialize VoiceCapture.

        Args:
            whisper_model: Whisper model size (tiny, base, small, medium, large)
            claude_model: Claude model to use for extraction (optional)
            language: Force specific language for transcription (auto-detect if None)
        """
        self.whisper_model_name = whisper_model
        self.whisper_model = None
        self.claude_model = claude_model or CLAUDE_MODEL
        self.language = language
        self.claude = None

        # Lazy initialization
        self._initialized = False

    def _ensure_initialized(self) -> None:
        """Lazily initialize Whisper and Claude clients."""
        if self._initialized:
            return

        # Initialize Whisper
        if not WHISPER_AVAILABLE:
            raise VoiceCaptureError(
                "Whisper is not installed. Install with: pip install openai-whisper"
            )

        try:
            logger.info(f"Loading Whisper model: {self.whisper_model_name}")
            self.whisper_model = whisper.load_model(self.whisper_model_name)
        except Exception as e:
            raise VoiceCaptureError(f"Failed to load Whisper model: {e}")

        # Initialize Claude
        if not ANTHROPIC_AVAILABLE:
            raise VoiceCaptureError(
                "Anthropic SDK is not installed. Install with: pip install anthropic"
            )

        if not ANTHROPIC_API_KEY:
            raise VoiceCaptureError(
                "ANTHROPIC_API_KEY is not configured. Set it in environment variables."
            )

        try:
            self.claude = Anthropic(api_key=ANTHROPIC_API_KEY)
        except Exception as e:
            raise VoiceCaptureError(f"Failed to initialize Claude client: {e}")

        self._initialized = True
        logger.info("VoiceCapture initialized successfully")

    def validate_audio(self, audio_data: bytes, filename: Optional[str] = None) -> None:
        """
        Validate audio file before processing.

        Args:
            audio_data: Raw audio bytes
            filename: Optional filename to check extension

        Raises:
            VoiceCaptureError: If validation fails
        """
        # Check file size
        if len(audio_data) > self.MAX_FILE_SIZE:
            raise VoiceCaptureError(
                f"Audio file too large. Maximum size is {self.MAX_FILE_SIZE / (1024*1024):.1f}MB"
            )

        # Check file format if filename provided
        if filename:
            ext = Path(filename).suffix.lower()
            if ext not in self.SUPPORTED_FORMATS:
                raise VoiceCaptureError(
                    f"Unsupported audio format: {ext}. "
                    f"Supported formats: {', '.join(self.SUPPORTED_FORMATS)}"
                )

        # Basic audio validation (check for valid audio header)
        if len(audio_data) < 100:
            raise VoiceCaptureError("Audio file too small or corrupted")

    async def transcribe(
        self,
        audio_data: bytes,
        filename: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Transcribe audio to text using Whisper.

        Args:
            audio_data: Raw audio bytes
            filename: Optional filename for format detection

        Returns:
            Dictionary with transcription results:
            {
                "text": str,           # Full transcription
                "language": str,       # Detected language
                "segments": list,      # Time-stamped segments
                "duration": float      # Audio duration in seconds
            }
        """
        self._ensure_initialized()
        self.validate_audio(audio_data, filename)

        # Write audio to temp file (Whisper needs file path)
        ext = Path(filename).suffix if filename else ".mp3"

        try:
            with tempfile.NamedTemporaryFile(suffix=ext, delete=False) as temp_file:
                temp_file.write(audio_data)
                temp_path = temp_file.name

            # Transcribe with Whisper
            logger.info(f"Transcribing audio file: {len(audio_data)} bytes")

            result = self.whisper_model.transcribe(
                temp_path,
                language=self.language,
                task="transcribe"
            )

            transcription = {
                "text": result["text"].strip(),
                "language": result.get("language", "unknown"),
                "segments": [
                    {
                        "start": seg["start"],
                        "end": seg["end"],
                        "text": seg["text"].strip()
                    }
                    for seg in result.get("segments", [])
                ],
                "duration": result.get("segments", [{}])[-1].get("end", 0) if result.get("segments") else 0
            }

            logger.info(
                f"Transcription complete: {len(transcription['text'])} chars, "
                f"language={transcription['language']}"
            )

            return transcription

        except Exception as e:
            logger.error(f"Transcription failed: {e}")
            raise VoiceCaptureError(f"Transcription failed: {e}")
        finally:
            # Clean up temp file
            try:
                os.unlink(temp_path)
            except Exception:
                pass

    async def extract_requirements(
        self,
        transcript: str,
        context: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Extract User Stories from transcribed text using Claude.

        Args:
            transcript: Text transcription from audio
            context: Optional project context or additional instructions

        Returns:
            List of extracted User Stories as dictionaries
        """
        self._ensure_initialized()

        if not transcript or not transcript.strip():
            logger.warning("Empty transcript provided")
            return []

        prompt = f"""Analise a seguinte transcrição de um usuário descrevendo o que ele precisa
e extraia User Stories no formato JSON.

Para cada requisito identificado, crie uma User Story com:
- title: Título curto e descritivo
- persona: Quem é o usuário (ex: "gerente de vendas", "administrador")
- action: O que ele quer fazer (ex: "gerar relatórios de vendas")
- benefit: O benefício esperado (ex: "tomar decisões baseadas em dados")
- acceptance_criteria: Lista de critérios de aceite (pelo menos 3)
- story_points: Estimativa de complexidade (1, 2, 3, 5, 8, 13 ou 21)
- priority: Prioridade (low, medium, high, urgent)

{f'Contexto do projeto: {context}' if context else ''}

Transcrição:
---
{transcript}
---

Responda APENAS com um JSON válido no formato:
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
            "original_text": "trecho relevante da transcrição"
        }}
    ],
    "summary": "resumo geral dos requisitos identificados",
    "suggestions": ["sugestão 1", "sugestão 2"]
}}

Se não conseguir identificar requisitos claros, retorne um array vazio de stories
com uma mensagem no summary explicando o que mais precisa de informações."""

        try:
            logger.info("Extracting requirements with Claude")

            response = self.claude.messages.create(
                model=self.claude_model,
                max_tokens=4096,
                messages=[
                    {
                        "role": "user",
                        "content": prompt
                    }
                ]
            )

            # Parse response
            response_text = response.content[0].text.strip()

            # Try to extract JSON from response
            try:
                # Handle cases where JSON is wrapped in markdown code blocks
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

                # Add metadata to each story
                for story in stories:
                    story["source"] = "voice"
                    story["extracted_at"] = datetime.utcnow().isoformat()

                logger.info(f"Extracted {len(stories)} stories from transcript")
                return stories

            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse Claude response as JSON: {e}")
                # Return a single story with the raw response for manual review
                return [{
                    "title": "Requisitos a revisar",
                    "persona": "usuario",
                    "action": "revisar requisitos capturados por voz",
                    "benefit": "processar corretamente os requisitos",
                    "acceptance_criteria": ["Revisar transcrição original"],
                    "story_points": 1,
                    "priority": "medium",
                    "source": "voice",
                    "raw_response": response_text,
                    "original_transcript": transcript[:500],
                    "extracted_at": datetime.utcnow().isoformat(),
                    "needs_review": True
                }]

        except Exception as e:
            logger.error(f"Requirements extraction failed: {e}")
            raise VoiceCaptureError(f"Failed to extract requirements: {e}")

    async def process_voice_input(
        self,
        audio_data: bytes,
        filename: Optional[str] = None,
        context: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Complete pipeline: audio -> transcription -> user stories.

        Args:
            audio_data: Raw audio bytes
            filename: Optional filename for format detection
            context: Optional project context

        Returns:
            Dictionary with:
            {
                "stories": List of extracted User Stories,
                "transcript": Full transcription text,
                "language": Detected language,
                "duration": Audio duration in seconds,
                "processing_time": Processing time in seconds
            }
        """
        import time
        start_time = time.time()

        try:
            # Step 1: Transcribe
            transcription = await self.transcribe(audio_data, filename)

            # Step 2: Extract requirements
            stories = await self.extract_requirements(
                transcription["text"],
                context=context
            )

            processing_time = time.time() - start_time

            return {
                "stories": stories,
                "transcript": transcription["text"],
                "language": transcription["language"],
                "duration": transcription["duration"],
                "segments": transcription["segments"],
                "processing_time": round(processing_time, 2),
                "source": "voice",
                "processed_at": datetime.utcnow().isoformat()
            }

        except VoiceCaptureError:
            raise
        except Exception as e:
            logger.error(f"Voice processing pipeline failed: {e}")
            raise VoiceCaptureError(f"Voice processing failed: {e}")

    def get_supported_formats(self) -> List[str]:
        """Return list of supported audio formats."""
        return self.SUPPORTED_FORMATS.copy()

    def get_status(self) -> Dict[str, Any]:
        """Get current status of the VoiceCapture system."""
        return {
            "initialized": self._initialized,
            "whisper_available": WHISPER_AVAILABLE,
            "anthropic_available": ANTHROPIC_AVAILABLE,
            "whisper_model": self.whisper_model_name,
            "claude_model": self.claude_model,
            "language": self.language or "auto-detect",
            "max_file_size_mb": self.MAX_FILE_SIZE / (1024 * 1024),
            "supported_formats": self.SUPPORTED_FORMATS
        }
