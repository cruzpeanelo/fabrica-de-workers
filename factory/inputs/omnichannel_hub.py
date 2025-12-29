"""
Omnichannel Hub - Unified Input Processing Center
Fabrica de Agentes v6.5 - Issue #131

Central hub that unifies all input methods (voice, document, whatsapp, email, api)
for consistent requirements capture and story creation.
"""

import os
import json
import logging
from typing import Optional, List, Dict, Any, Union, Callable
from datetime import datetime
from enum import Enum
from dataclasses import dataclass, field

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class InputSource(str, Enum):
    """Supported input sources"""
    VOICE = "voice"
    DOCUMENT = "document"
    WHATSAPP = "whatsapp"
    EMAIL = "email"
    API = "api"
    FORM = "form"
    CHAT = "chat"


class ProcessingStatus(str, Enum):
    """Status of input processing"""
    PENDING = "pending"
    PROCESSING = "processing"
    COMPLETED = "completed"
    FAILED = "failed"
    NEEDS_REVIEW = "needs_review"


@dataclass
class InputRecord:
    """Record of a processed input"""
    input_id: str
    source: InputSource
    tenant_id: str
    status: ProcessingStatus
    raw_data: Dict[str, Any]
    stories: List[Dict[str, Any]] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    error_message: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    processed_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "input_id": self.input_id,
            "source": self.source.value,
            "tenant_id": self.tenant_id,
            "status": self.status.value,
            "stories_count": len(self.stories),
            "stories": self.stories,
            "metadata": self.metadata,
            "error_message": self.error_message,
            "created_at": self.created_at.isoformat(),
            "processed_at": self.processed_at.isoformat() if self.processed_at else None
        }


class OmnichannelHubError(Exception):
    """Custom exception for Omnichannel Hub errors"""
    pass


class EmailProcessor:
    """
    Simple email processor for requirements extraction.

    In production, this would integrate with email services like
    SendGrid, Mailgun, or direct IMAP/SMTP.
    """

    def __init__(self):
        self.claude = None
        self._initialized = False

    def _ensure_initialized(self):
        if self._initialized:
            return

        try:
            from anthropic import Anthropic
            self.claude = Anthropic(api_key=ANTHROPIC_API_KEY)
            self._initialized = True
        except Exception as e:
            logger.error(f"Failed to initialize EmailProcessor: {e}")
            raise OmnichannelHubError(f"EmailProcessor init failed: {e}")

    async def process(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Process email data and extract requirements.

        Args:
            data: Dictionary with email fields:
                - subject: Email subject
                - body: Email body text
                - from_email: Sender email
                - attachments: Optional list of attachment data

        Returns:
            List of extracted User Stories
        """
        self._ensure_initialized()

        subject = data.get("subject", "")
        body = data.get("body", "")
        from_email = data.get("from_email", "unknown")

        if not body and not subject:
            return []

        prompt = f"""Analise o seguinte email e extraia requisitos de software
em formato de User Stories JSON.

De: {from_email}
Assunto: {subject}

Corpo:
{body}

Responda APENAS com um JSON valido:
{{
    "stories": [
        {{
            "title": "...",
            "persona": "...",
            "action": "...",
            "benefit": "...",
            "acceptance_criteria": ["...", "...", "..."],
            "story_points": 5,
            "priority": "medium"
        }}
    ]
}}"""

        try:
            response = self.claude.messages.create(
                model=CLAUDE_MODEL,
                max_tokens=2048,
                messages=[{"role": "user", "content": prompt}]
            )

            response_text = response.content[0].text.strip()

            # Handle JSON in markdown
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

            for story in stories:
                story["source"] = "email"
                story["from_email"] = from_email
                story["extracted_at"] = datetime.utcnow().isoformat()

            return stories

        except Exception as e:
            logger.error(f"Email processing failed: {e}")
            return []


class DirectAPIInput:
    """
    Direct API input for programmatic story creation.

    Validates and normalizes story data from external systems.
    """

    REQUIRED_FIELDS = ["title"]
    OPTIONAL_FIELDS = [
        "persona", "action", "benefit", "acceptance_criteria",
        "story_points", "priority", "category", "tags"
    ]

    async def process(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Process API input and create normalized stories.

        Args:
            data: Dictionary with story data or list of stories

        Returns:
            List of normalized User Stories
        """
        # Handle both single story and list of stories
        if isinstance(data, list):
            stories_input = data
        elif "stories" in data:
            stories_input = data["stories"]
        else:
            stories_input = [data]

        stories = []
        for story_data in stories_input:
            story = self._normalize_story(story_data)
            if story:
                stories.append(story)

        return stories

    def _normalize_story(self, data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Normalize and validate a single story."""
        # Check required fields
        if not data.get("title"):
            return None

        story = {
            "title": data["title"],
            "persona": data.get("persona", "usuario"),
            "action": data.get("action", ""),
            "benefit": data.get("benefit", ""),
            "acceptance_criteria": data.get("acceptance_criteria", []),
            "story_points": data.get("story_points", 3),
            "priority": data.get("priority", "medium"),
            "category": data.get("category", "feature"),
            "tags": data.get("tags", []),
            "source": "api",
            "extracted_at": datetime.utcnow().isoformat()
        }

        # Validate story_points
        valid_points = [1, 2, 3, 5, 8, 13, 21]
        if story["story_points"] not in valid_points:
            story["story_points"] = 3

        # Validate priority
        valid_priorities = ["low", "medium", "high", "urgent"]
        if story["priority"] not in valid_priorities:
            story["priority"] = "medium"

        return story


class OmnichannelHub:
    """
    Unified hub for processing inputs from all channels.

    Provides a single interface for:
    - Voice capture (speech-to-text)
    - Document processing (DOCX, XLSX, PPTX, PDF)
    - WhatsApp integration
    - Email processing
    - Direct API input

    Example:
        hub = OmnichannelHub(tenant_id="tenant-123")
        result = await hub.process_input("voice", audio_bytes)
        stories = result["stories"]
    """

    def __init__(
        self,
        tenant_id: str,
        auto_save: bool = True,
        story_callback: Optional[Callable] = None
    ):
        """
        Initialize OmnichannelHub.

        Args:
            tenant_id: Tenant identifier
            auto_save: Whether to auto-save stories to database
            story_callback: Optional callback for each extracted story
        """
        self.tenant_id = tenant_id
        self.auto_save = auto_save
        self.story_callback = story_callback

        # Initialize processors lazily
        self._processors: Dict[InputSource, Any] = {}
        self._voice_capture = None
        self._document_processor = None
        self._whatsapp_integration = None
        self._email_processor = None
        self._api_input = None

        # Input history (in-memory, use database in production)
        self._input_history: List[InputRecord] = []

        # Counters
        self._stats = {
            "total_inputs": 0,
            "total_stories": 0,
            "by_source": {s.value: 0 for s in InputSource}
        }

    def _get_processor(self, source: InputSource) -> Any:
        """Get or create processor for source."""
        if source in self._processors:
            return self._processors[source]

        if source == InputSource.VOICE:
            from factory.inputs.voice_capture import VoiceCapture
            self._processors[source] = VoiceCapture()

        elif source == InputSource.DOCUMENT:
            from factory.inputs.document_processor import DocumentProcessor
            self._processors[source] = DocumentProcessor()

        elif source == InputSource.WHATSAPP:
            from factory.inputs.whatsapp_integration import WhatsAppIntegration
            self._processors[source] = WhatsAppIntegration()

        elif source == InputSource.EMAIL:
            self._processors[source] = EmailProcessor()

        elif source == InputSource.API:
            self._processors[source] = DirectAPIInput()

        else:
            raise OmnichannelHubError(f"Unknown source: {source}")

        return self._processors[source]

    def _generate_input_id(self) -> str:
        """Generate unique input ID."""
        import uuid
        return f"INP-{uuid.uuid4().hex[:8].upper()}"

    async def process_input(
        self,
        source: Union[str, InputSource],
        data: Any,
        metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Process input from any source.

        Args:
            source: Input source (voice, document, whatsapp, email, api)
            data: Input data (format depends on source)
            metadata: Optional metadata (filename, context, etc.)

        Returns:
            Dictionary with:
            {
                "input_id": str,
                "source": str,
                "stories": List[Dict],
                "status": str,
                "processing_time": float
            }
        """
        import time
        start_time = time.time()

        # Normalize source
        if isinstance(source, str):
            try:
                source = InputSource(source.lower())
            except ValueError:
                raise OmnichannelHubError(f"Unknown source: {source}")

        metadata = metadata or {}
        input_id = self._generate_input_id()

        # Create input record
        record = InputRecord(
            input_id=input_id,
            source=source,
            tenant_id=self.tenant_id,
            status=ProcessingStatus.PROCESSING,
            raw_data={"size": len(data) if isinstance(data, bytes) else "n/a"},
            metadata=metadata
        )

        try:
            # Get appropriate processor
            processor = self._get_processor(source)

            # Process based on source type
            if source == InputSource.VOICE:
                result = await processor.process_voice_input(
                    data,
                    filename=metadata.get("filename"),
                    context=metadata.get("context")
                )
                stories = result.get("stories", [])
                record.metadata["transcript"] = result.get("transcript", "")[:500]
                record.metadata["language"] = result.get("language")
                record.metadata["duration"] = result.get("duration")

            elif source == InputSource.DOCUMENT:
                result = await processor.process_document(
                    data,
                    filename=metadata.get("filename", "document.txt"),
                    context=metadata.get("context")
                )
                stories = result.get("stories", [])
                record.metadata["document_type"] = result.get("document_type")
                record.metadata["file_size"] = result.get("file_size")

            elif source == InputSource.WHATSAPP:
                result = await processor.handle_message(
                    from_number=metadata.get("from_number", "unknown"),
                    message=data if isinstance(data, str) else str(data),
                    tenant_id=self.tenant_id
                )
                stories = result.get("stories", [])
                record.metadata["response"] = result.get("response", "")

            elif source == InputSource.EMAIL:
                stories = await processor.process(data if isinstance(data, dict) else {"body": str(data)})

            elif source == InputSource.API:
                stories = await processor.process(data if isinstance(data, dict) else {"title": str(data)})

            else:
                stories = []

            # Add tenant_id and source to all stories
            for story in stories:
                story["tenant_id"] = self.tenant_id
                story["source"] = source.value
                story["input_id"] = input_id

            # Update record
            record.stories = stories
            record.status = ProcessingStatus.COMPLETED
            record.processed_at = datetime.utcnow()

            # Update stats
            self._stats["total_inputs"] += 1
            self._stats["total_stories"] += len(stories)
            self._stats["by_source"][source.value] += 1

            # Call callback for each story
            if self.story_callback and stories:
                for story in stories:
                    try:
                        await self.story_callback(story)
                    except Exception as e:
                        logger.error(f"Story callback failed: {e}")

            # Auto-save stories
            if self.auto_save and stories:
                await self._save_stories(stories)

        except Exception as e:
            logger.error(f"Input processing failed: {e}")
            record.status = ProcessingStatus.FAILED
            record.error_message = str(e)
            stories = []

        # Save to history
        self._input_history.append(record)

        processing_time = time.time() - start_time

        return {
            "input_id": input_id,
            "source": source.value,
            "tenant_id": self.tenant_id,
            "stories": stories,
            "stories_count": len(stories),
            "status": record.status.value,
            "processing_time": round(processing_time, 2),
            "metadata": record.metadata,
            "error_message": record.error_message
        }

    async def _save_stories(self, stories: List[Dict[str, Any]]) -> List[str]:
        """
        Save stories to database.

        Args:
            stories: List of story dictionaries

        Returns:
            List of created story IDs
        """
        story_ids = []

        try:
            # Import story creation function
            from factory.database.repositories import create_story

            for story in stories:
                try:
                    story_id = await create_story(story)
                    story_ids.append(story_id)
                    logger.info(f"Story saved: {story_id}")
                except Exception as e:
                    logger.warning(f"Failed to save story: {e}")

        except ImportError:
            logger.warning("Story repository not available, skipping save")

        return story_ids

    def get_input_history(
        self,
        filters: Optional[Dict[str, Any]] = None,
        limit: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Get input history with optional filters.

        Args:
            filters: Optional filters (source, status, date_from, date_to)
            limit: Maximum records to return

        Returns:
            List of input records
        """
        records = self._input_history.copy()

        if filters:
            if "source" in filters:
                source = filters["source"]
                if isinstance(source, str):
                    source = InputSource(source.lower())
                records = [r for r in records if r.source == source]

            if "status" in filters:
                status = filters["status"]
                if isinstance(status, str):
                    status = ProcessingStatus(status.lower())
                records = [r for r in records if r.status == status]

            if "date_from" in filters:
                date_from = filters["date_from"]
                if isinstance(date_from, str):
                    date_from = datetime.fromisoformat(date_from)
                records = [r for r in records if r.created_at >= date_from]

            if "date_to" in filters:
                date_to = filters["date_to"]
                if isinstance(date_to, str):
                    date_to = datetime.fromisoformat(date_to)
                records = [r for r in records if r.created_at <= date_to]

        # Sort by created_at descending
        records.sort(key=lambda r: r.created_at, reverse=True)

        # Limit results
        records = records[:limit]

        return [r.to_dict() for r in records]

    def get_input_by_id(self, input_id: str) -> Optional[Dict[str, Any]]:
        """Get specific input record by ID."""
        for record in self._input_history:
            if record.input_id == input_id:
                return record.to_dict()
        return None

    def get_stats(self) -> Dict[str, Any]:
        """Get processing statistics."""
        return {
            **self._stats,
            "tenant_id": self.tenant_id,
            "history_size": len(self._input_history),
            "auto_save_enabled": self.auto_save
        }

    def get_available_sources(self) -> Dict[str, Dict[str, Any]]:
        """Get information about available input sources."""
        sources = {}

        # Voice
        try:
            from factory.inputs.voice_capture import VoiceCapture, WHISPER_AVAILABLE
            vc = VoiceCapture()
            sources["voice"] = {
                "available": WHISPER_AVAILABLE,
                "description": "Speech-to-text using Whisper + Claude",
                "formats": vc.SUPPORTED_FORMATS
            }
        except ImportError:
            sources["voice"] = {"available": False}

        # Document
        try:
            from factory.inputs.document_processor import (
                DocumentProcessor,
                DOCX_AVAILABLE,
                XLSX_AVAILABLE,
                PPTX_AVAILABLE,
                PDF_AVAILABLE
            )
            sources["document"] = {
                "available": True,
                "description": "Office documents and PDF processing",
                "formats": {
                    "docx": DOCX_AVAILABLE,
                    "xlsx": XLSX_AVAILABLE,
                    "pptx": PPTX_AVAILABLE,
                    "pdf": PDF_AVAILABLE,
                    "txt": True
                }
            }
        except ImportError:
            sources["document"] = {"available": False}

        # WhatsApp
        try:
            from factory.inputs.whatsapp_integration import (
                WhatsAppIntegration,
                TWILIO_AVAILABLE
            )
            sources["whatsapp"] = {
                "available": True,
                "description": "WhatsApp via Twilio",
                "twilio_available": TWILIO_AVAILABLE
            }
        except ImportError:
            sources["whatsapp"] = {"available": False}

        # Email
        sources["email"] = {
            "available": True,
            "description": "Email-based requirements capture"
        }

        # API
        sources["api"] = {
            "available": True,
            "description": "Direct API for programmatic input"
        }

        return sources

    def get_status(self) -> Dict[str, Any]:
        """Get current status of the Omnichannel Hub."""
        return {
            "tenant_id": self.tenant_id,
            "auto_save": self.auto_save,
            "stats": self.get_stats(),
            "sources": self.get_available_sources(),
            "active_processors": list(self._processors.keys())
        }
