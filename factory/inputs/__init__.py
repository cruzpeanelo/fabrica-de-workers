"""
Factory Inputs - Omnichannel Input Methods for Requirements Capture
Fabrica de Agentes v6.5

This module provides multiple input methods for capturing requirements:
- Voice Capture (Speech-to-Text with Whisper + Claude)
- Voice Input (Enhanced browser recording with OpenAI Whisper API)
- Document Processing (DOCX, XLSX, PPTX, PDF)
- Office Processor (Enhanced Microsoft Office document processing)
- WhatsApp Integration (via Twilio)
- Video Assistant (WebRTC video-based requirements capture)
- Email Processing
- Direct API Input
- Omnichannel Hub (unified interface with dashboard, routing, deduplication)

Issues: #127, #128, #129, #130, #131
"""

from .voice_capture import VoiceCapture
from .voice_input import VoiceInput
from .document_processor import DocumentProcessor
from .office_processor import OfficeProcessor
from .whatsapp_integration import WhatsAppIntegration
from .video_assistant import VideoAssistant
from .omnichannel_hub import OmnichannelHub

__all__ = [
    "VoiceCapture",
    "VoiceInput",
    "DocumentProcessor",
    "OfficeProcessor",
    "WhatsAppIntegration",
    "VideoAssistant",
    "OmnichannelHub",
]

__version__ = "2.0.0"
