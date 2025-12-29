"""
Omnichannel Hub - Unified Input Processing Center
Fabrica de Agentes v6.5 - Issue #131

Central hub that unifies all input methods (voice, document, whatsapp, email, api, video)
for consistent requirements capture and story creation.

Features:
- Unified interface for all input channels
- Input dashboard with real-time status
- Intelligent routing based on content type
- Story deduplication using similarity matching
- Priority queue management
- Analytics and reporting
"""

import os
import re
import json
import hashlib
import logging
import asyncio
from typing import Optional, List, Dict, Any, Union, Callable, Tuple
from datetime import datetime, timedelta
from enum import Enum
from dataclasses import dataclass, field
from collections import defaultdict
import uuid

try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    Anthropic = None

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class InputSource(str, Enum):
    """Supported input sources"""
    VOICE = "voice"
    VOICE_INPUT = "voice_input"
    DOCUMENT = "document"
    OFFICE = "office"
    WHATSAPP = "whatsapp"
    EMAIL = "email"
    API = "api"
    FORM = "form"
    CHAT = "chat"
    VIDEO = "video"


class ProcessingStatus(str, Enum):
    """Status of input processing"""
    PENDING = "pending"
    QUEUED = "queued"
    PROCESSING = "processing"
    COMPLETED = "completed"
    FAILED = "failed"
    NEEDS_REVIEW = "needs_review"
    DUPLICATE = "duplicate"


class Priority(str, Enum):
    """Input processing priority"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    URGENT = "urgent"


class RoutingRule(str, Enum):
    """Routing rules for inputs"""
    AUTO = "auto"
    VOICE_FIRST = "voice_first"
    DOCUMENT_FIRST = "document_first"
    FIFO = "fifo"
    PRIORITY = "priority"


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
    priority: Priority = Priority.MEDIUM
    content_hash: Optional[str] = None
    duplicate_of: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    processed_at: Optional[datetime] = None
    processing_time: float = 0.0

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
            "priority": self.priority.value,
            "content_hash": self.content_hash,
            "duplicate_of": self.duplicate_of,
            "created_at": self.created_at.isoformat(),
            "processed_at": self.processed_at.isoformat() if self.processed_at else None,
            "processing_time": self.processing_time
        }


@dataclass
class DashboardMetrics:
    """Metrics for the input dashboard"""
    total_inputs_today: int = 0
    total_stories_today: int = 0
    pending_count: int = 0
    processing_count: int = 0
    failed_count: int = 0
    duplicate_count: int = 0
    avg_processing_time: float = 0.0
    by_source: Dict[str, int] = field(default_factory=dict)
    by_priority: Dict[str, int] = field(default_factory=dict)
    hourly_distribution: Dict[int, int] = field(default_factory=dict)
    top_sources: List[Dict[str, Any]] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "total_inputs_today": self.total_inputs_today,
            "total_stories_today": self.total_stories_today,
            "pending_count": self.pending_count,
            "processing_count": self.processing_count,
            "failed_count": self.failed_count,
            "duplicate_count": self.duplicate_count,
            "avg_processing_time": round(self.avg_processing_time, 2),
            "by_source": self.by_source,
            "by_priority": self.by_priority,
            "hourly_distribution": self.hourly_distribution,
            "top_sources": self.top_sources
        }


class OmnichannelHubError(Exception):
    """Custom exception for Omnichannel Hub errors"""
    pass


class StoryDeduplicator:
    """Detects and manages duplicate stories using content similarity."""
    SIMILARITY_THRESHOLD = 0.85

    def __init__(self, use_semantic: bool = False):
        self.use_semantic = use_semantic
        self._hash_index: Dict[str, str] = {}
        self._title_index: Dict[str, List[str]] = defaultdict(list)

    def _compute_hash(self, story: Dict[str, Any]) -> str:
        content = json.dumps({
            "title": story.get("title", "").lower().strip(),
            "persona": story.get("persona", "").lower().strip(),
            "action": story.get("action", "").lower().strip(),
            "benefit": story.get("benefit", "").lower().strip()
        }, sort_keys=True)
        return hashlib.md5(content.encode()).hexdigest()

    def _normalize_title(self, title: str) -> str:
        normalized = re.sub(r'[^\w\s]', '', title.lower())
        return ' '.join(normalized.split())

    def _levenshtein_similarity(self, s1: str, s2: str) -> float:
        if not s1 or not s2: return 0.0
        if s1 == s2: return 1.0
        len1, len2 = len(s1), len(s2)
        if len1 < len2: s1, s2 = s2, s1; len1, len2 = len2, len1
        current_row = range(len2 + 1)
        for i in range(1, len1 + 1):
            previous_row, current_row = current_row, [i] + [0] * len2
            for j in range(1, len2 + 1):
                add, delete, change = previous_row[j] + 1, current_row[j-1] + 1, previous_row[j-1]
                if s1[i-1] != s2[j-1]: change += 1
                current_row[j] = min(add, delete, change)
        return 1 - (current_row[len2] / max(len1, len2))

    def check_duplicate(self, story: Dict[str, Any], input_id: str) -> Tuple[bool, Optional[str], float]:
        story_hash = self._compute_hash(story)
        if story_hash in self._hash_index:
            return True, self._hash_index[story_hash], 1.0
        title = story.get("title", "")
        normalized_title = self._normalize_title(title)
        for indexed_title, input_ids in self._title_index.items():
            similarity = self._levenshtein_similarity(normalized_title, indexed_title)
            if similarity >= self.SIMILARITY_THRESHOLD:
                return True, input_ids[0], similarity
        self._hash_index[story_hash] = input_id
        self._title_index[normalized_title].append(input_id)
        return False, None, 0.0

    def clear_index(self):
        self._hash_index.clear()
        self._title_index.clear()


class InputRouter:
    """Routes inputs to appropriate processors based on content and rules."""
    def __init__(self, routing_rule: RoutingRule = RoutingRule.AUTO):
        self.routing_rule = routing_rule
        self._processor_load: Dict[str, int] = defaultdict(int)

    def determine_source(self, data: Any, metadata: Dict[str, Any]) -> InputSource:
        if "source" in metadata:
            try: return InputSource(metadata["source"].lower())
            except ValueError: pass
        filename = metadata.get("filename", "")
        if filename:
            ext = filename.lower().split(".")[-1] if "." in filename else ""
            if ext in ["mp3", "wav", "m4a", "webm", "ogg", "flac"]: return InputSource.VOICE_INPUT
            if ext in ["docx", "doc", "xlsx", "xls", "pptx", "ppt"]: return InputSource.OFFICE
            if ext in ["pdf", "txt"]: return InputSource.DOCUMENT
            if ext in ["mp4", "mkv"]: return InputSource.VIDEO
        if isinstance(data, bytes):
            if data[:4] == b'RIFF' or data[:3] == b'ID3': return InputSource.VOICE_INPUT
            if data[:4] == b'%PDF': return InputSource.DOCUMENT
            if data[:4] == b'PK\x03\x04': return InputSource.OFFICE
            return InputSource.DOCUMENT
        if isinstance(data, dict):
            if "subject" in data and "body" in data: return InputSource.EMAIL
            if "from_number" in data or "phone" in data: return InputSource.WHATSAPP
            if "form_id" in data or "fields" in data: return InputSource.FORM
            return InputSource.API
        if isinstance(data, str) and metadata.get("channel") == "chat": return InputSource.CHAT
        return InputSource.API

    def get_priority(self, source: InputSource, metadata: Dict[str, Any]) -> Priority:
        if "priority" in metadata:
            try: return Priority(metadata["priority"].lower())
            except ValueError: pass
        priority_map = {
            InputSource.VOICE: Priority.HIGH, InputSource.VOICE_INPUT: Priority.HIGH,
            InputSource.VIDEO: Priority.HIGH, InputSource.WHATSAPP: Priority.HIGH,
            InputSource.CHAT: Priority.HIGH, InputSource.EMAIL: Priority.MEDIUM,
            InputSource.FORM: Priority.MEDIUM, InputSource.DOCUMENT: Priority.MEDIUM,
            InputSource.OFFICE: Priority.MEDIUM, InputSource.API: Priority.LOW
        }
        return priority_map.get(source, Priority.MEDIUM)

    def route(self, data: Any, metadata: Dict[str, Any]) -> Tuple[InputSource, Priority, str]:
        source = self.determine_source(data, metadata)
        priority = self.get_priority(source, metadata)
        processor_id = f"{source.value}_processor"
        self._processor_load[processor_id] += 1
        return source, priority, processor_id


class EmailProcessor:
    """Simple email processor for requirements extraction."""
    def __init__(self):
        self.claude = None
        self._initialized = False

    def _ensure_initialized(self):
        if self._initialized: return
        try:
            from anthropic import Anthropic
            self.claude = Anthropic(api_key=ANTHROPIC_API_KEY)
            self._initialized = True
        except Exception as e:
            logger.error(f"Failed to initialize EmailProcessor: {e}")
            raise OmnichannelHubError(f"EmailProcessor init failed: {e}")

    async def process(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        self._ensure_initialized()
        subject = data.get("subject", "")
        body = data.get("body", "")
        from_email = data.get("from_email", "unknown")
        if not body and not subject: return []
        prompt = f"""Analise o seguinte email e extraia requisitos de software em formato de User Stories JSON.
De: {from_email}
Assunto: {subject}
Corpo: {body}
Responda APENAS com um JSON valido: {{"stories": [{{"title": "...", "persona": "...", "action": "...", "benefit": "...", "acceptance_criteria": ["...", "...", "..."], "story_points": 5, "priority": "medium"}}]}}"""
        try:
            response = self.claude.messages.create(model=CLAUDE_MODEL, max_tokens=2048, messages=[{"role": "user", "content": prompt}])
            response_text = response.content[0].text.strip()
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
    """Direct API input for programmatic story creation."""
    async def process(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        if isinstance(data, list): stories_input = data
        elif "stories" in data: stories_input = data["stories"]
        else: stories_input = [data]
        stories = []
        for story_data in stories_input:
            story = self._normalize_story(story_data)
            if story: stories.append(story)
        return stories

    def _normalize_story(self, data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        if not data.get("title"): return None
        story = {"title": data["title"], "persona": data.get("persona", "usuario"), "action": data.get("action", ""), "benefit": data.get("benefit", ""), "acceptance_criteria": data.get("acceptance_criteria", []), "story_points": data.get("story_points", 3), "priority": data.get("priority", "medium"), "category": data.get("category", "feature"), "tags": data.get("tags", []), "source": "api", "extracted_at": datetime.utcnow().isoformat()}
        if story["story_points"] not in [1, 2, 3, 5, 8, 13, 21]: story["story_points"] = 3
        if story["priority"] not in ["low", "medium", "high", "urgent"]: story["priority"] = "medium"
        return story


class OmnichannelHub:
    """Unified hub for processing inputs from all channels with dashboard, routing, and deduplication."""

    def __init__(self, tenant_id: str, auto_save: bool = True, story_callback: Optional[Callable] = None, routing_rule: RoutingRule = RoutingRule.AUTO, enable_deduplication: bool = True, semantic_deduplication: bool = False):
        self.tenant_id = tenant_id
        self.auto_save = auto_save
        self.story_callback = story_callback
        self.enable_deduplication = enable_deduplication
        self._processors: Dict[InputSource, Any] = {}
        self._input_history: List[InputRecord] = []
        self._queue: List[Tuple[InputRecord, Any, Dict]] = []
        self._processing_lock = asyncio.Lock()
        self.router = InputRouter(routing_rule)
        self.deduplicator = StoryDeduplicator(use_semantic=semantic_deduplication)
        self._all_stories: List[Dict[str, Any]] = []
        self._stats = {"total_inputs": 0, "total_stories": 0, "duplicates_detected": 0, "by_source": {s.value: 0 for s in InputSource}, "by_priority": {p.value: 0 for p in Priority}, "processing_times": []}

    def _get_processor(self, source: InputSource) -> Any:
        if source in self._processors: return self._processors[source]
        if source == InputSource.VOICE:
            from factory.inputs.voice_capture import VoiceCapture
            self._processors[source] = VoiceCapture()
        elif source == InputSource.VOICE_INPUT:
            from factory.inputs.voice_input import VoiceInput
            self._processors[source] = VoiceInput()
        elif source == InputSource.DOCUMENT:
            from factory.inputs.document_processor import DocumentProcessor
            self._processors[source] = DocumentProcessor()
        elif source == InputSource.OFFICE:
            from factory.inputs.office_processor import OfficeProcessor
            self._processors[source] = OfficeProcessor()
        elif source == InputSource.WHATSAPP:
            from factory.inputs.whatsapp_integration import WhatsAppIntegration
            self._processors[source] = WhatsAppIntegration()
        elif source == InputSource.VIDEO:
            from factory.inputs.video_assistant import VideoAssistant
            self._processors[source] = VideoAssistant(tenant_id=self.tenant_id)
        elif source == InputSource.EMAIL:
            self._processors[source] = EmailProcessor()
        elif source in [InputSource.API, InputSource.FORM, InputSource.CHAT]:
            self._processors[source] = DirectAPIInput()
        else: raise OmnichannelHubError(f"Unknown source: {source}")
        return self._processors[source]

    def _generate_input_id(self) -> str:
        return f"INP-{uuid.uuid4().hex[:8].upper()}"

    async def process_input(self, source: Union[str, InputSource], data: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        import time
        start_time = time.time()
        metadata = metadata or {}
        if isinstance(source, str):
            try: source = InputSource(source.lower())
            except ValueError: source, priority, _ = self.router.route(data, metadata)
        else: priority = self.router.get_priority(source, metadata)
        input_id = self._generate_input_id()
        record = InputRecord(input_id=input_id, source=source, tenant_id=self.tenant_id, status=ProcessingStatus.PROCESSING, raw_data={"size": len(data) if isinstance(data, bytes) else "n/a"}, metadata=metadata, priority=priority)
        duplicates_found = []
        stories = []
        try:
            processor = self._get_processor(source)
            if source in [InputSource.VOICE, InputSource.VOICE_INPUT]:
                if hasattr(processor, 'process_voice_input'): result = await processor.process_voice_input(data, filename=metadata.get("filename"), context=metadata.get("context"))
                else: result = await processor.transcribe_audio(data, metadata.get("filename")); result = {"stories": [], "transcript": result.text if hasattr(result, 'text') else str(result)}
                stories = result.get("stories", [])
                record.metadata["transcript"] = result.get("transcript", "")[:500]
                record.metadata["language"] = result.get("language")
                record.metadata["duration"] = result.get("duration")
            elif source in [InputSource.DOCUMENT, InputSource.OFFICE]:
                result = await processor.process_document(data, filename=metadata.get("filename", "document.txt"), context=metadata.get("context"))
                stories = result.get("stories", [])
                record.metadata["document_type"] = result.get("document_type")
                record.metadata["file_size"] = result.get("file_size")
            elif source == InputSource.WHATSAPP:
                result = await processor.handle_message(from_number=metadata.get("from_number", "unknown"), message=data if isinstance(data, str) else str(data), tenant_id=self.tenant_id)
                stories = result.get("stories", [])
                record.metadata["response"] = result.get("response", "")
            elif source == InputSource.VIDEO:
                session_id = metadata.get("session_id")
                if session_id: result = await processor.end_session(session_id); stories = result.get("stories", [])
                else: stories = []
            elif source == InputSource.EMAIL: stories = await processor.process(data if isinstance(data, dict) else {"body": str(data)})
            elif source in [InputSource.API, InputSource.FORM, InputSource.CHAT]: stories = await processor.process(data if isinstance(data, dict) else {"title": str(data)})
            else: stories = []
            if self.enable_deduplication:
                deduplicated_stories = []
                for story in stories:
                    is_dup, dup_of, score = self.deduplicator.check_duplicate(story, input_id)
                    if is_dup: duplicates_found.append({"story_title": story.get("title"), "duplicate_of": dup_of, "similarity": score}); self._stats["duplicates_detected"] += 1
                    else: deduplicated_stories.append(story)
                stories = deduplicated_stories
            for story in stories: story["tenant_id"] = self.tenant_id; story["source"] = source.value; story["input_id"] = input_id
            record.stories = stories
            record.status = ProcessingStatus.COMPLETED
            record.processed_at = datetime.utcnow()
            self._stats["total_inputs"] += 1
            self._stats["total_stories"] += len(stories)
            self._stats["by_source"][source.value] = self._stats["by_source"].get(source.value, 0) + 1
            self._stats["by_priority"][priority.value] = self._stats["by_priority"].get(priority.value, 0) + 1
            self._all_stories.extend(stories)
            if self.story_callback and stories:
                for story in stories:
                    try: await self.story_callback(story)
                    except Exception as e: logger.error(f"Story callback failed: {e}")
            if self.auto_save and stories: await self._save_stories(stories)
        except Exception as e:
            logger.error(f"Input processing failed: {e}")
            record.status = ProcessingStatus.FAILED
            record.error_message = str(e)
            stories = []
        processing_time = time.time() - start_time
        record.processing_time = processing_time
        self._stats["processing_times"].append(processing_time)
        if len(self._stats["processing_times"]) > 100: self._stats["processing_times"] = self._stats["processing_times"][-100:]
        self._input_history.append(record)
        return {"input_id": input_id, "source": source.value, "tenant_id": self.tenant_id, "stories": stories, "stories_count": len(stories), "status": record.status.value, "priority": priority.value, "processing_time": round(processing_time, 2), "metadata": record.metadata, "error_message": record.error_message, "duplicates": duplicates_found}

    async def _save_stories(self, stories: List[Dict[str, Any]]) -> List[str]:
        story_ids = []
        try:
            from factory.database.repositories import create_story
            for story in stories:
                try: story_id = await create_story(story); story_ids.append(story_id); logger.info(f"Story saved: {story_id}")
                except Exception as e: logger.warning(f"Failed to save story: {e}")
        except ImportError: logger.warning("Story repository not available, skipping save")
        return story_ids

    def get_dashboard_metrics(self) -> DashboardMetrics:
        today = datetime.utcnow().date()
        today_records = [r for r in self._input_history if r.created_at.date() == today]
        metrics = DashboardMetrics(total_inputs_today=len(today_records), total_stories_today=sum(len(r.stories) for r in today_records), pending_count=len([r for r in self._input_history if r.status == ProcessingStatus.PENDING]), processing_count=len([r for r in self._input_history if r.status == ProcessingStatus.PROCESSING]), failed_count=len([r for r in self._input_history if r.status == ProcessingStatus.FAILED]), duplicate_count=self._stats["duplicates_detected"])
        if self._stats["processing_times"]: metrics.avg_processing_time = sum(self._stats["processing_times"]) / len(self._stats["processing_times"])
        metrics.by_source = dict(self._stats["by_source"])
        metrics.by_priority = dict(self._stats["by_priority"])
        for record in today_records: hour = record.created_at.hour; metrics.hourly_distribution[hour] = metrics.hourly_distribution.get(hour, 0) + 1
        source_counts = defaultdict(int)
        for record in self._input_history: source_counts[record.source.value] += 1
        metrics.top_sources = [{"source": source, "count": count} for source, count in sorted(source_counts.items(), key=lambda x: -x[1])[:5]]
        return metrics

    def get_input_history(self, filters: Optional[Dict[str, Any]] = None, limit: int = 50) -> List[Dict[str, Any]]:
        records = self._input_history.copy()
        if filters:
            if "source" in filters: source = filters["source"]; source = InputSource(source.lower()) if isinstance(source, str) else source; records = [r for r in records if r.source == source]
            if "status" in filters: status = filters["status"]; status = ProcessingStatus(status.lower()) if isinstance(status, str) else status; records = [r for r in records if r.status == status]
            if "priority" in filters: priority = filters["priority"]; priority = Priority(priority.lower()) if isinstance(priority, str) else priority; records = [r for r in records if r.priority == priority]
            if "date_from" in filters: date_from = filters["date_from"]; date_from = datetime.fromisoformat(date_from) if isinstance(date_from, str) else date_from; records = [r for r in records if r.created_at >= date_from]
            if "date_to" in filters: date_to = filters["date_to"]; date_to = datetime.fromisoformat(date_to) if isinstance(date_to, str) else date_to; records = [r for r in records if r.created_at <= date_to]
        records.sort(key=lambda r: r.created_at, reverse=True)
        return [r.to_dict() for r in records[:limit]]

    def get_input_by_id(self, input_id: str) -> Optional[Dict[str, Any]]:
        for record in self._input_history:
            if record.input_id == input_id: return record.to_dict()
        return None

    def get_stats(self) -> Dict[str, Any]:
        avg_time = sum(self._stats["processing_times"]) / len(self._stats["processing_times"]) if self._stats["processing_times"] else 0.0
        return {**self._stats, "tenant_id": self.tenant_id, "history_size": len(self._input_history), "auto_save_enabled": self.auto_save, "avg_processing_time": round(avg_time, 2), "deduplication_enabled": self.enable_deduplication}

    def get_available_sources(self) -> Dict[str, Dict[str, Any]]:
        sources = {}
        try:
            from factory.inputs.voice_capture import VoiceCapture, WHISPER_AVAILABLE
            vc = VoiceCapture(); sources["voice"] = {"available": WHISPER_AVAILABLE, "description": "Speech-to-text using Whisper + Claude", "formats": vc.SUPPORTED_FORMATS}
        except ImportError: sources["voice"] = {"available": False}
        try:
            from factory.inputs.voice_input import VoiceInput, OPENAI_AVAILABLE, LOCAL_WHISPER_AVAILABLE
            sources["voice_input"] = {"available": OPENAI_AVAILABLE or LOCAL_WHISPER_AVAILABLE, "description": "Enhanced voice with browser recording", "providers": {"openai_api": OPENAI_AVAILABLE, "local_whisper": LOCAL_WHISPER_AVAILABLE}}
        except ImportError: sources["voice_input"] = {"available": False}
        try:
            from factory.inputs.document_processor import DocumentProcessor, DOCX_AVAILABLE, XLSX_AVAILABLE, PPTX_AVAILABLE, PDF_AVAILABLE
            sources["document"] = {"available": True, "description": "Office documents and PDF processing", "formats": {"docx": DOCX_AVAILABLE, "xlsx": XLSX_AVAILABLE, "pptx": PPTX_AVAILABLE, "pdf": PDF_AVAILABLE, "txt": True}}
        except ImportError: sources["document"] = {"available": False}
        try:
            from factory.inputs.office_processor import OfficeProcessor
            op = OfficeProcessor(); sources["office"] = {"available": True, "description": "Enhanced Office document processing", "formats": op.get_supported_formats()}
        except ImportError: sources["office"] = {"available": False}
        try:
            from factory.inputs.whatsapp_integration import WhatsAppIntegration, TWILIO_AVAILABLE
            sources["whatsapp"] = {"available": True, "description": "WhatsApp via Twilio", "twilio_available": TWILIO_AVAILABLE}
        except ImportError: sources["whatsapp"] = {"available": False}
        try:
            from factory.inputs.video_assistant import VideoAssistant
            sources["video"] = {"available": True, "description": "WebRTC video assistant for requirements capture"}
        except ImportError: sources["video"] = {"available": False}
        sources["email"] = {"available": True, "description": "Email-based requirements capture"}
        sources["api"] = {"available": True, "description": "Direct API for programmatic input"}
        sources["form"] = {"available": True, "description": "Web form submissions"}
        sources["chat"] = {"available": True, "description": "Chat-based requirements capture"}
        return sources

    def get_status(self) -> Dict[str, Any]:
        return {"tenant_id": self.tenant_id, "auto_save": self.auto_save, "deduplication_enabled": self.enable_deduplication, "routing_rule": self.router.routing_rule.value, "stats": self.get_stats(), "sources": self.get_available_sources(), "active_processors": [p.value for p in self._processors.keys()], "dashboard_metrics": self.get_dashboard_metrics().to_dict()}

    def clear_history(self, older_than_days: int = 30) -> int:
        cutoff = datetime.utcnow() - timedelta(days=older_than_days)
        original_count = len(self._input_history)
        self._input_history = [r for r in self._input_history if r.created_at >= cutoff]
        cleared = original_count - len(self._input_history)
        logger.info(f"Cleared {cleared} old input records")
        return cleared

    def reset_deduplication_index(self):
        self.deduplicator.clear_index()
        logger.info("Deduplication index cleared")
