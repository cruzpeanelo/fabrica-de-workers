"""
Factory Inputs - Omnichannel Input Methods for Requirements Capture
Fabrica de Agentes v6.5

This module provides multiple input methods for capturing requirements:
- Voice Capture (Speech-to-Text with Whisper + Claude)
- Document Processing (DOCX, XLSX, PPTX, PDF)
- WhatsApp Integration (via Twilio)
- Email Processing
- Direct API Input
- Omnichannel Hub (unified interface)

Issues: #127, #128, #129, #131
"""

from .voice_capture import VoiceCapture
from .document_processor import DocumentProcessor
from .whatsapp_integration import WhatsAppIntegration
from .omnichannel_hub import OmnichannelHub

__all__ = [
    "VoiceCapture",
    "DocumentProcessor",
    "WhatsAppIntegration",
    "OmnichannelHub",
]

__version__ = "1.0.0"
