"""
Video Assistant - WebRTC Video-based Requirements Capture
Fabrica de Agentes v6.5 - Issue #130

Features:
- WebRTC integration for video calls
- Session recording and storage
- Real-time audio transcription
- Video frame analysis for diagrams/mockups
- Conversation analysis and story extraction
- Meeting summary generation
"""

import os
import io
import json
import base64
import asyncio
import logging
import uuid
from typing import Optional, List, Dict, Any, Callable, AsyncGenerator
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

# WebRTC and media processing
try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    aiohttp = None

try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False
    Image = None

try:
    import cv2
    import numpy as np
    CV2_AVAILABLE = True
except ImportError:
    CV2_AVAILABLE = False
    cv2 = None
    np = None

# Anthropic Claude
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    Anthropic = None

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class SessionState(str, Enum):
    """Video session states"""
    CREATED = "created"
    WAITING = "waiting"
    CONNECTED = "connected"
    RECORDING = "recording"
    PAUSED = "paused"
    ENDED = "ended"
    FAILED = "failed"


class ParticipantRole(str, Enum):
    """Participant roles in session"""
    HOST = "host"           # Product owner/analyst
    STAKEHOLDER = "stakeholder"  # Business user
    TECHNICAL = "technical"  # Developer/architect
    OBSERVER = "observer"    # Silent observer


@dataclass
class Participant:
    """Represents a session participant"""
    participant_id: str
    name: str
    role: ParticipantRole
    joined_at: datetime = field(default_factory=datetime.utcnow)
    video_enabled: bool = True
    audio_enabled: bool = True
    connection_id: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "participant_id": self.participant_id,
            "name": self.name,
            "role": self.role.value,
            "joined_at": self.joined_at.isoformat(),
            "video_enabled": self.video_enabled,
            "audio_enabled": self.audio_enabled
        }


@dataclass
class TranscriptSegment:
    """A segment of the session transcript"""
    speaker: str
    text: str
    start_time: float
    end_time: float
    confidence: float = 0.0
    is_requirement: bool = False

    def to_dict(self) -> Dict[str, Any]:
        return {
            "speaker": self.speaker,
            "text": self.text,
            "start_time": self.start_time,
            "end_time": self.end_time,
            "confidence": self.confidence,
            "is_requirement": self.is_requirement
        }


@dataclass
class CapturedFrame:
    """A captured video frame with analysis"""
    frame_id: str
    timestamp: float
    image_data: bytes
    description: Optional[str] = None
    contains_diagram: bool = False
    contains_mockup: bool = False
    ocr_text: Optional[str] = None
    relevance_score: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "frame_id": self.frame_id,
            "timestamp": self.timestamp,
            "description": self.description,
            "contains_diagram": self.contains_diagram,
            "contains_mockup": self.contains_mockup,
            "ocr_text": self.ocr_text,
            "relevance_score": self.relevance_score
        }


@dataclass
class VideoSession:
    """Represents a video recording session"""
    session_id: str
    tenant_id: str
    title: str
    state: SessionState = SessionState.CREATED
    participants: List[Participant] = field(default_factory=list)
    transcript: List[TranscriptSegment] = field(default_factory=list)
    captured_frames: List[CapturedFrame] = field(default_factory=list)
    extracted_stories: List[Dict[str, Any]] = field(default_factory=list)
    recording_path: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    started_at: Optional[datetime] = None
    ended_at: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def duration_seconds(self) -> float:
        if self.started_at and self.ended_at:
            return (self.ended_at - self.started_at).total_seconds()
        return 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "session_id": self.session_id,
            "tenant_id": self.tenant_id,
            "title": self.title,
            "state": self.state.value,
            "participants": [p.to_dict() for p in self.participants],
            "transcript_segments": len(self.transcript),
            "captured_frames": len(self.captured_frames),
            "stories_extracted": len(self.extracted_stories),
            "duration_seconds": self.duration_seconds(),
            "created_at": self.created_at.isoformat(),
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "ended_at": self.ended_at.isoformat() if self.ended_at else None,
            "metadata": self.metadata
        }


class VideoAssistantError(Exception):
    """Custom exception for Video Assistant errors"""
    pass


class VideoAssistant:
    """
    WebRTC Video Assistant for requirements capture through video calls.

    Features:
    - WebRTC-based video calling integration
    - Real-time session recording
    - Audio transcription during calls
    - Frame capture for diagrams/mockups
    - AI-powered conversation analysis
    - Automatic story extraction

    Example:
        assistant = VideoAssistant(tenant_id="tenant-123")
        session = await assistant.create_session("Requirements Review")
        await assistant.start_recording(session.session_id)
        # ... video call happens ...
        stories = await assistant.end_session(session.session_id)
    """

    # Default settings
    DEFAULT_RECORDING_DIR = "uploads/recordings"
    FRAME_CAPTURE_INTERVAL = 30  # seconds
    MAX_SESSION_DURATION = 7200  # 2 hours

    # WebRTC configuration
    ICE_SERVERS = [
        {"urls": "stun:stun.l.google.com:19302"},
        {"urls": "stun:stun1.l.google.com:19302"}
    ]

    def __init__(
        self,
        tenant_id: str,
        recording_dir: Optional[str] = None,
        signaling_url: Optional[str] = None,
        claude_model: Optional[str] = None,
        auto_transcribe: bool = True,
        auto_capture_frames: bool = True
    ):
        """
        Initialize VideoAssistant.

        Args:
            tenant_id: Tenant identifier
            recording_dir: Directory for recordings
            signaling_url: WebRTC signaling server URL
            claude_model: Claude model for analysis
            auto_transcribe: Enable automatic transcription
            auto_capture_frames: Enable automatic frame capture
        """
        self.tenant_id = tenant_id
        self.recording_dir = Path(recording_dir or self.DEFAULT_RECORDING_DIR)
        self.signaling_url = signaling_url or os.getenv("WEBRTC_SIGNALING_URL")
        self.claude_model = claude_model or CLAUDE_MODEL
        self.auto_transcribe = auto_transcribe
        self.auto_capture_frames = auto_capture_frames

        # Sessions storage
        self.sessions: Dict[str, VideoSession] = {}

        # Claude client
        self.claude = None
        self._initialized = False

        # Callbacks
        self.on_participant_joined: Optional[Callable] = None
        self.on_transcript_segment: Optional[Callable] = None
        self.on_frame_captured: Optional[Callable] = None
        self.on_story_extracted: Optional[Callable] = None

        # Stats
        self._stats = {
            "total_sessions": 0,
            "total_duration_minutes": 0,
            "total_stories_extracted": 0,
            "total_frames_captured": 0
        }

    def _ensure_initialized(self) -> None:
        """Initialize Claude and directories."""
        if self._initialized:
            return

        # Create recording directory
        self.recording_dir.mkdir(parents=True, exist_ok=True)

        # Initialize Claude
        if ANTHROPIC_AVAILABLE and ANTHROPIC_API_KEY:
            self.claude = Anthropic(api_key=ANTHROPIC_API_KEY)
            logger.info("VideoAssistant Claude initialized")

        self._initialized = True

    def _generate_session_id(self) -> str:
        """Generate unique session ID."""
        return f"VID-{uuid.uuid4().hex[:8].upper()}"

    async def create_session(
        self,
        title: str,
        scheduled_at: Optional[datetime] = None,
        participants: Optional[List[Dict[str, str]]] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> VideoSession:
        """
        Create a new video session.

        Args:
            title: Session title
            scheduled_at: Optional scheduled start time
            participants: Optional list of participant info
            metadata: Optional metadata

        Returns:
            Created VideoSession
        """
        self._ensure_initialized()

        session_id = self._generate_session_id()

        session = VideoSession(
            session_id=session_id,
            tenant_id=self.tenant_id,
            title=title,
            state=SessionState.CREATED,
            metadata=metadata or {}
        )

        if scheduled_at:
            session.metadata["scheduled_at"] = scheduled_at.isoformat()

        # Add initial participants
        if participants:
            for p in participants:
                participant = Participant(
                    participant_id=f"P-{uuid.uuid4().hex[:6].upper()}",
                    name=p.get("name", "Unknown"),
                    role=ParticipantRole(p.get("role", "stakeholder"))
                )
                session.participants.append(participant)

        self.sessions[session_id] = session
        self._stats["total_sessions"] += 1

        logger.info(f"Created video session: {session_id} - {title}")
        return session

    async def join_session(
        self,
        session_id: str,
        name: str,
        role: str = "stakeholder"
    ) -> Dict[str, Any]:
        """
        Join an existing session.

        Args:
            session_id: Session ID
            name: Participant name
            role: Participant role

        Returns:
            Join information with WebRTC configuration
        """
        session = self.sessions.get(session_id)
        if not session:
            raise VideoAssistantError(f"Session not found: {session_id}")

        if session.state == SessionState.ENDED:
            raise VideoAssistantError("Session has ended")

        # Create participant
        participant = Participant(
            participant_id=f"P-{uuid.uuid4().hex[:6].upper()}",
            name=name,
            role=ParticipantRole(role)
        )
        session.participants.append(participant)

        if session.state == SessionState.CREATED:
            session.state = SessionState.WAITING

        # Notify callback
        if self.on_participant_joined:
            try:
                await self.on_participant_joined(session, participant)
            except Exception as e:
                logger.error(f"Participant callback failed: {e}")

        logger.info(f"Participant {name} joined session {session_id}")

        return {
            "session_id": session_id,
            "participant_id": participant.participant_id,
            "ice_servers": self.ICE_SERVERS,
            "signaling_url": self.signaling_url,
            "session_state": session.state.value,
            "participants": [p.to_dict() for p in session.participants]
        }

    async def start_recording(
        self,
        session_id: str
    ) -> Dict[str, Any]:
        """
        Start recording a session.

        Args:
            session_id: Session ID

        Returns:
            Recording information
        """
        session = self.sessions.get(session_id)
        if not session:
            raise VideoAssistantError(f"Session not found: {session_id}")

        if session.state not in [SessionState.CREATED, SessionState.WAITING, SessionState.CONNECTED]:
            raise VideoAssistantError(f"Cannot start recording in state: {session.state}")

        # Set up recording path
        timestamp = datetime.utcnow().strftime("%Y%m%d_%H%M%S")
        recording_filename = f"{session_id}_{timestamp}.webm"
        session.recording_path = str(self.recording_dir / recording_filename)

        session.state = SessionState.RECORDING
        session.started_at = datetime.utcnow()

        logger.info(f"Started recording session {session_id}")

        return {
            "session_id": session_id,
            "state": session.state.value,
            "recording_path": session.recording_path,
            "started_at": session.started_at.isoformat()
        }

    async def add_transcript_segment(
        self,
        session_id: str,
        speaker: str,
        text: str,
        start_time: float,
        end_time: float,
        confidence: float = 0.0
    ) -> TranscriptSegment:
        """
        Add a transcript segment to the session.

        Args:
            session_id: Session ID
            speaker: Speaker name
            text: Transcribed text
            start_time: Start time in seconds
            end_time: End time in seconds
            confidence: Transcription confidence

        Returns:
            Created TranscriptSegment
        """
        session = self.sessions.get(session_id)
        if not session:
            raise VideoAssistantError(f"Session not found: {session_id}")

        # Check if text might contain a requirement
        is_requirement = self._detect_requirement_in_text(text)

        segment = TranscriptSegment(
            speaker=speaker,
            text=text,
            start_time=start_time,
            end_time=end_time,
            confidence=confidence,
            is_requirement=is_requirement
        )
        session.transcript.append(segment)

        # Notify callback
        if self.on_transcript_segment:
            try:
                await self.on_transcript_segment(session, segment)
            except Exception as e:
                logger.error(f"Transcript callback failed: {e}")

        return segment

    def _detect_requirement_in_text(self, text: str) -> bool:
        """Detect if text might contain a requirement."""
        patterns = [
            r"(?i)precis[ao]",
            r"(?i)quer[eo]",
            r"(?i)deve",
            r"(?i)necessit[ao]",
            r"(?i)seria\s+bom",
            r"(?i)gostaria",
            r"(?i)importante",
            r"(?i)essencial",
            r"(?i)funcionalidade",
            r"(?i)sistema",
            r"(?i)relatorio",
            r"(?i)dashboard"
        ]

        import re
        for pattern in patterns:
            if re.search(pattern, text):
                return True
        return False

    async def capture_frame(
        self,
        session_id: str,
        image_data: bytes,
        timestamp: float,
        analyze: bool = True
    ) -> CapturedFrame:
        """
        Capture a video frame for analysis.

        Args:
            session_id: Session ID
            image_data: Raw image bytes (PNG/JPEG)
            timestamp: Frame timestamp in seconds
            analyze: Whether to analyze frame with AI

        Returns:
            CapturedFrame with analysis
        """
        session = self.sessions.get(session_id)
        if not session:
            raise VideoAssistantError(f"Session not found: {session_id}")

        frame_id = f"FRM-{uuid.uuid4().hex[:6].upper()}"

        frame = CapturedFrame(
            frame_id=frame_id,
            timestamp=timestamp,
            image_data=image_data
        )

        # Analyze frame if requested
        if analyze and self.claude:
            analysis = await self._analyze_frame(image_data)
            frame.description = analysis.get("description")
            frame.contains_diagram = analysis.get("contains_diagram", False)
            frame.contains_mockup = analysis.get("contains_mockup", False)
            frame.ocr_text = analysis.get("ocr_text")
            frame.relevance_score = analysis.get("relevance_score", 0.0)

        session.captured_frames.append(frame)
        self._stats["total_frames_captured"] += 1

        # Notify callback
        if self.on_frame_captured:
            try:
                await self.on_frame_captured(session, frame)
            except Exception as e:
                logger.error(f"Frame callback failed: {e}")

        logger.info(f"Captured frame {frame_id} at {timestamp:.1f}s")
        return frame

    async def _analyze_frame(self, image_data: bytes) -> Dict[str, Any]:
        """Analyze a frame using Claude Vision."""
        if not self.claude:
            return {}

        try:
            # Convert to base64
            image_base64 = base64.b64encode(image_data).decode("utf-8")

            # Detect image type
            if image_data[:8] == b'\x89PNG\r\n\x1a\n':
                media_type = "image/png"
            elif image_data[:2] == b'\xff\xd8':
                media_type = "image/jpeg"
            else:
                media_type = "image/png"

            response = self.claude.messages.create(
                model=self.claude_model,
                max_tokens=1024,
                messages=[
                    {
                        "role": "user",
                        "content": [
                            {
                                "type": "image",
                                "source": {
                                    "type": "base64",
                                    "media_type": media_type,
                                    "data": image_base64
                                }
                            },
                            {
                                "type": "text",
                                "text": """Analise esta imagem de uma sessao de video e responda em JSON:
{
    "description": "descricao breve do que aparece",
    "contains_diagram": true/false,
    "contains_mockup": true/false,
    "contains_whiteboard": true/false,
    "ocr_text": "texto visivel na imagem ou null",
    "relevance_score": 0.0-1.0 (relevancia para requisitos de software),
    "elements": ["lista de elementos identificados"]
}"""
                            }
                        ]
                    }
                ]
            )

            response_text = response.content[0].text
            if "```json" in response_text:
                json_start = response_text.find("```json") + 7
                json_end = response_text.find("```", json_start)
                response_text = response_text[json_start:json_end]

            return json.loads(response_text)

        except Exception as e:
            logger.error(f"Frame analysis failed: {e}")
            return {}

    async def end_session(
        self,
        session_id: str,
        extract_stories: bool = True
    ) -> Dict[str, Any]:
        """
        End a session and process results.

        Args:
            session_id: Session ID
            extract_stories: Whether to extract stories

        Returns:
            Session summary with extracted stories
        """
        session = self.sessions.get(session_id)
        if not session:
            raise VideoAssistantError(f"Session not found: {session_id}")

        session.state = SessionState.ENDED
        session.ended_at = datetime.utcnow()

        duration_minutes = session.duration_seconds() / 60
        self._stats["total_duration_minutes"] += duration_minutes

        # Extract stories from transcript
        if extract_stories and session.transcript:
            stories = await self._extract_stories_from_session(session)
            session.extracted_stories = stories
            self._stats["total_stories_extracted"] += len(stories)

        logger.info(
            f"Ended session {session_id}: {duration_minutes:.1f} min, "
            f"{len(session.transcript)} segments, {len(session.extracted_stories)} stories"
        )

        return {
            "session": session.to_dict(),
            "stories": session.extracted_stories,
            "summary": await self._generate_session_summary(session),
            "transcript": [s.to_dict() for s in session.transcript],
            "key_frames": [
                f.to_dict() for f in session.captured_frames
                if f.relevance_score > 0.5
            ]
        }

    async def _extract_stories_from_session(
        self,
        session: VideoSession
    ) -> List[Dict[str, Any]]:
        """Extract user stories from session transcript."""
        if not self.claude:
            return []

        # Build transcript text
        transcript_text = "\n".join([
            f"[{s.start_time:.0f}s] {s.speaker}: {s.text}"
            for s in session.transcript
        ])

        # Include frame descriptions
        frame_descriptions = "\n".join([
            f"[{f.timestamp:.0f}s] Frame: {f.description}"
            for f in session.captured_frames
            if f.description
        ])

        prompt = f"""Analise a transcricao desta sessao de video sobre requisitos de software
e extraia User Stories em formato JSON.

Sessao: {session.title}
Participantes: {', '.join([p.name + ' (' + p.role.value + ')' for p in session.participants])}
Duracao: {session.duration_seconds() / 60:.1f} minutos

Transcricao:
---
{transcript_text[:15000]}
---

{f'Elementos visuais capturados:{frame_descriptions}' if frame_descriptions else ''}

Para cada requisito identificado, crie uma User Story com:
- title: Titulo descritivo
- persona: Quem mencionou ou precisa
- action: O que quer fazer
- benefit: Beneficio esperado
- acceptance_criteria: Criterios de aceite (minimo 3)
- story_points: Estimativa (1, 2, 3, 5, 8, 13, 21)
- priority: Prioridade baseada na enfase da conversa
- mentioned_by: Quem mencionou
- timestamp: Momento aproximado na sessao

Responda APENAS com JSON valido:
{{
    "stories": [...],
    "key_decisions": ["decisoes importantes tomadas"],
    "open_questions": ["perguntas sem resposta"],
    "action_items": ["acoes acordadas"]
}}"""

        try:
            response = self.claude.messages.create(
                model=self.claude_model,
                max_tokens=4096,
                messages=[{"role": "user", "content": prompt}]
            )

            response_text = response.content[0].text.strip()

            if "```json" in response_text:
                json_start = response_text.find("```json") + 7
                json_end = response_text.find("```", json_start)
                response_text = response_text[json_start:json_end]

            result = json.loads(response_text)
            stories = result.get("stories", [])

            # Add metadata
            for story in stories:
                story["source"] = "video_session"
                story["session_id"] = session.session_id
                story["extracted_at"] = datetime.utcnow().isoformat()

            return stories

        except Exception as e:
            logger.error(f"Story extraction failed: {e}")
            return []

    async def _generate_session_summary(
        self,
        session: VideoSession
    ) -> str:
        """Generate a session summary."""
        if not self.claude or not session.transcript:
            return ""

        transcript_text = "\n".join([
            f"{s.speaker}: {s.text}" for s in session.transcript[:50]
        ])

        try:
            response = self.claude.messages.create(
                model=self.claude_model,
                max_tokens=500,
                messages=[{
                    "role": "user",
                    "content": f"""Gere um resumo executivo de 3-5 paragrafos desta sessao:

Titulo: {session.title}
Participantes: {len(session.participants)}
Duracao: {session.duration_seconds() / 60:.1f} minutos

Transcricao (parcial):
{transcript_text}

Foque nos principais requisitos discutidos e decisoes tomadas."""
                }]
            )

            return response.content[0].text.strip()

        except Exception as e:
            logger.error(f"Summary generation failed: {e}")
            return ""

    def get_session(self, session_id: str) -> Optional[VideoSession]:
        """Get session by ID."""
        return self.sessions.get(session_id)

    def list_sessions(
        self,
        state: Optional[SessionState] = None,
        limit: int = 50
    ) -> List[Dict[str, Any]]:
        """List sessions with optional state filter."""
        sessions = list(self.sessions.values())

        if state:
            sessions = [s for s in sessions if s.state == state]

        sessions.sort(key=lambda s: s.created_at, reverse=True)
        return [s.to_dict() for s in sessions[:limit]]

    def get_webrtc_config(self) -> Dict[str, Any]:
        """
        Get WebRTC configuration for client.

        Returns:
            Configuration for browser WebRTC setup
        """
        return {
            "ice_servers": self.ICE_SERVERS,
            "signaling_url": self.signaling_url,
            "constraints": {
                "video": {
                    "width": {"ideal": 1280},
                    "height": {"ideal": 720},
                    "frameRate": {"ideal": 30}
                },
                "audio": {
                    "echoCancellation": True,
                    "noiseSuppression": True,
                    "autoGainControl": True
                }
            },
            "recording": {
                "mimeType": "video/webm;codecs=vp9,opus",
                "videoBitsPerSecond": 2500000
            }
        }

    def get_status(self) -> Dict[str, Any]:
        """Get current status."""
        return {
            "initialized": self._initialized,
            "tenant_id": self.tenant_id,
            "claude_available": bool(self.claude),
            "signaling_configured": bool(self.signaling_url),
            "recording_dir": str(self.recording_dir),
            "active_sessions": len([
                s for s in self.sessions.values()
                if s.state in [SessionState.WAITING, SessionState.CONNECTED, SessionState.RECORDING]
            ]),
            "total_sessions": len(self.sessions),
            "stats": self._stats,
            "features": {
                "auto_transcribe": self.auto_transcribe,
                "auto_capture_frames": self.auto_capture_frames,
                "cv2_available": CV2_AVAILABLE,
                "pil_available": PIL_AVAILABLE
            }
        }


# Browser JavaScript for WebRTC (to be served with dashboard)
BROWSER_WEBRTC_JS = """
class VideoSessionClient {
    constructor(options = {}) {
        this.sessionId = options.sessionId;
        this.participantId = null;
        this.localStream = null;
        this.peerConnection = null;
        this.dataChannel = null;
        this.mediaRecorder = null;
        this.recordedChunks = [];
        this.signalingSocket = null;

        // Callbacks
        this.onRemoteStream = options.onRemoteStream || null;
        this.onParticipantJoined = options.onParticipantJoined || null;
        this.onMessage = options.onMessage || null;
        this.onError = options.onError || null;

        // Configuration
        this.config = options.config || {
            iceServers: [
                { urls: 'stun:stun.l.google.com:19302' }
            ]
        };
    }

    async initialize(signalingUrl) {
        // Connect to signaling server
        this.signalingSocket = new WebSocket(signalingUrl);

        this.signalingSocket.onmessage = (event) => {
            const message = JSON.parse(event.data);
            this.handleSignalingMessage(message);
        };

        this.signalingSocket.onclose = () => {
            console.log('Signaling connection closed');
        };

        // Wait for connection
        await new Promise((resolve, reject) => {
            this.signalingSocket.onopen = resolve;
            this.signalingSocket.onerror = reject;
        });
    }

    async startLocalStream(constraints = { video: true, audio: true }) {
        try {
            this.localStream = await navigator.mediaDevices.getUserMedia(constraints);
            return this.localStream;
        } catch (error) {
            if (this.onError) this.onError(error);
            throw error;
        }
    }

    async createPeerConnection() {
        this.peerConnection = new RTCPeerConnection(this.config);

        // Add local tracks
        if (this.localStream) {
            this.localStream.getTracks().forEach(track => {
                this.peerConnection.addTrack(track, this.localStream);
            });
        }

        // Handle remote stream
        this.peerConnection.ontrack = (event) => {
            if (this.onRemoteStream) {
                this.onRemoteStream(event.streams[0]);
            }
        };

        // Handle ICE candidates
        this.peerConnection.onicecandidate = (event) => {
            if (event.candidate) {
                this.sendSignalingMessage({
                    type: 'ice-candidate',
                    candidate: event.candidate
                });
            }
        };

        // Create data channel
        this.dataChannel = this.peerConnection.createDataChannel('messages');
        this.dataChannel.onmessage = (event) => {
            if (this.onMessage) {
                this.onMessage(JSON.parse(event.data));
            }
        };

        return this.peerConnection;
    }

    async createOffer() {
        const offer = await this.peerConnection.createOffer();
        await this.peerConnection.setLocalDescription(offer);
        this.sendSignalingMessage({
            type: 'offer',
            sdp: offer
        });
    }

    async handleSignalingMessage(message) {
        switch (message.type) {
            case 'offer':
                await this.peerConnection.setRemoteDescription(message.sdp);
                const answer = await this.peerConnection.createAnswer();
                await this.peerConnection.setLocalDescription(answer);
                this.sendSignalingMessage({
                    type: 'answer',
                    sdp: answer
                });
                break;

            case 'answer':
                await this.peerConnection.setRemoteDescription(message.sdp);
                break;

            case 'ice-candidate':
                await this.peerConnection.addIceCandidate(message.candidate);
                break;

            case 'participant-joined':
                if (this.onParticipantJoined) {
                    this.onParticipantJoined(message.participant);
                }
                break;
        }
    }

    sendSignalingMessage(message) {
        if (this.signalingSocket && this.signalingSocket.readyState === WebSocket.OPEN) {
            this.signalingSocket.send(JSON.stringify({
                ...message,
                sessionId: this.sessionId,
                participantId: this.participantId
            }));
        }
    }

    startRecording() {
        if (!this.localStream) return;

        this.recordedChunks = [];
        this.mediaRecorder = new MediaRecorder(this.localStream, {
            mimeType: 'video/webm;codecs=vp9,opus'
        });

        this.mediaRecorder.ondataavailable = (event) => {
            if (event.data.size > 0) {
                this.recordedChunks.push(event.data);
            }
        };

        this.mediaRecorder.start(1000);
    }

    stopRecording() {
        if (this.mediaRecorder) {
            this.mediaRecorder.stop();
            return new Blob(this.recordedChunks, { type: 'video/webm' });
        }
        return null;
    }

    captureFrame() {
        if (!this.localStream) return null;

        const video = document.createElement('video');
        video.srcObject = this.localStream;
        video.play();

        const canvas = document.createElement('canvas');
        canvas.width = video.videoWidth || 640;
        canvas.height = video.videoHeight || 480;

        const ctx = canvas.getContext('2d');
        ctx.drawImage(video, 0, 0);

        return canvas.toDataURL('image/png');
    }

    disconnect() {
        if (this.localStream) {
            this.localStream.getTracks().forEach(track => track.stop());
        }
        if (this.peerConnection) {
            this.peerConnection.close();
        }
        if (this.signalingSocket) {
            this.signalingSocket.close();
        }
    }
}

// Usage:
// const client = new VideoSessionClient({
//     sessionId: 'VID-ABC123',
//     onRemoteStream: (stream) => {
//         document.getElementById('remoteVideo').srcObject = stream;
//     }
// });
// await client.initialize('wss://signaling.example.com');
// const localStream = await client.startLocalStream();
// document.getElementById('localVideo').srcObject = localStream;
// await client.createPeerConnection();
// await client.createOffer();
"""
