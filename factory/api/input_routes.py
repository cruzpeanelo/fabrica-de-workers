"""
Input Routes - API Endpoints for Omnichannel Input Methods
Fabrica de Agentes v6.5 - Issues #127, #128, #129, #131

Provides REST API endpoints for:
- Voice capture (POST /api/v1/inputs/voice)
- Document processing (POST /api/v1/inputs/document)
- WhatsApp webhook (POST /api/v1/webhooks/whatsapp)
- Omnichannel hub (POST /api/v1/inputs/hub)
"""

import logging
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Depends, Query, UploadFile, File, Form, Request, Response
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1", tags=["Inputs"])


# =============================================================================
# MODELS
# =============================================================================

class StoryResponse(BaseModel):
    """User Story response model"""
    title: str
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: List[str] = []
    story_points: Optional[int] = None
    priority: Optional[str] = None
    category: Optional[str] = None
    source: str
    extracted_at: Optional[str] = None


class VoiceInputResponse(BaseModel):
    """Response from voice input processing"""
    stories: List[StoryResponse]
    transcript: str
    language: Optional[str] = None
    duration: Optional[float] = None
    processing_time: float
    source: str = "voice"


class DocumentInputResponse(BaseModel):
    """Response from document input processing"""
    stories: List[StoryResponse]
    document_type: str
    filename: str
    file_size: int
    processing_time: float
    source: str = "document"


class WhatsAppMessageRequest(BaseModel):
    """WhatsApp message request model"""
    from_number: str = Field(..., description="Sender phone number")
    message: str = Field(..., description="Message content")
    tenant_id: Optional[str] = Field(None, description="Tenant ID")


class WhatsAppMessageResponse(BaseModel):
    """Response from WhatsApp message processing"""
    response: str
    stories: List[StoryResponse]
    conversation_state: Optional[str] = None


class OmnichannelInputRequest(BaseModel):
    """Request for omnichannel hub processing"""
    source: str = Field(..., description="Input source (voice, document, whatsapp, email, api)")
    data: Any = Field(..., description="Input data")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Additional metadata")


class OmnichannelInputResponse(BaseModel):
    """Response from omnichannel hub processing"""
    input_id: str
    source: str
    tenant_id: str
    stories: List[StoryResponse]
    stories_count: int
    status: str
    processing_time: float
    metadata: Optional[Dict[str, Any]] = None
    error_message: Optional[str] = None


class InputHistoryFilters(BaseModel):
    """Filters for input history"""
    source: Optional[str] = None
    status: Optional[str] = None
    date_from: Optional[str] = None
    date_to: Optional[str] = None


class InputStatusResponse(BaseModel):
    """Status of input processing systems"""
    voice: Dict[str, Any]
    document: Dict[str, Any]
    whatsapp: Dict[str, Any]
    email: Dict[str, Any]
    api: Dict[str, Any]


class EmailInputRequest(BaseModel):
    """Email input request model"""
    subject: Optional[str] = Field(None, description="Email subject")
    body: str = Field(..., description="Email body")
    from_email: Optional[str] = Field(None, description="Sender email")


class APIInputRequest(BaseModel):
    """Direct API input request model"""
    stories: List[Dict[str, Any]] = Field(..., description="List of stories to create")


# =============================================================================
# DEPENDENCIES
# =============================================================================

async def get_tenant_id(request: Request) -> str:
    """
    Extract tenant ID from request.

    In production, this would extract tenant from JWT or API key.
    """
    # Try to get from header
    tenant_id = request.headers.get("X-Tenant-ID")
    if tenant_id:
        return tenant_id

    # Try to get from query param
    tenant_id = request.query_params.get("tenant_id")
    if tenant_id:
        return tenant_id

    # Default tenant for development
    return "default-tenant"


# =============================================================================
# VOICE INPUT ROUTES
# =============================================================================

@router.post("/inputs/voice", response_model=VoiceInputResponse)
async def voice_to_story(
    audio: UploadFile = File(..., description="Audio file to transcribe"),
    context: Optional[str] = Form(None, description="Optional project context"),
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Convert voice recording to User Stories.

    Accepts audio files (MP3, WAV, M4A, WebM, OGG, FLAC) and:
    1. Transcribes using OpenAI Whisper
    2. Extracts requirements using Claude
    3. Returns User Stories in JSON format

    **Supported formats:** MP3, WAV, M4A, WebM, OGG, FLAC, MP4
    **Maximum file size:** 25MB
    """
    try:
        from factory.inputs.voice_capture import VoiceCapture, VoiceCaptureError

        voice = VoiceCapture()

        # Read audio data
        audio_data = await audio.read()

        # Process voice input
        result = await voice.process_voice_input(
            audio_data,
            filename=audio.filename,
            context=context
        )

        return VoiceInputResponse(
            stories=result.get("stories", []),
            transcript=result.get("transcript", ""),
            language=result.get("language"),
            duration=result.get("duration"),
            processing_time=result.get("processing_time", 0),
            source="voice"
        )

    except VoiceCaptureError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except ImportError as e:
        raise HTTPException(
            status_code=503,
            detail=f"Voice capture not available: {e}. Install whisper: pip install openai-whisper"
        )
    except Exception as e:
        logger.error(f"Voice processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"Voice processing failed: {e}")


@router.get("/inputs/voice/status")
async def voice_capture_status():
    """Get status of voice capture system."""
    try:
        from factory.inputs.voice_capture import VoiceCapture

        voice = VoiceCapture()
        return voice.get_status()
    except ImportError as e:
        return {
            "available": False,
            "error": str(e)
        }


# =============================================================================
# DOCUMENT INPUT ROUTES
# =============================================================================

@router.post("/inputs/document", response_model=DocumentInputResponse)
async def process_document(
    file: UploadFile = File(..., description="Document to process"),
    context: Optional[str] = Form(None, description="Optional project context"),
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Extract User Stories from Office documents.

    Supports:
    - **DOCX:** Microsoft Word documents
    - **XLSX:** Microsoft Excel spreadsheets
    - **PPTX:** Microsoft PowerPoint presentations
    - **PDF:** Adobe PDF documents
    - **TXT:** Plain text files

    **Maximum file size:** 50MB
    """
    try:
        from factory.inputs.document_processor import DocumentProcessor, DocumentProcessorError

        processor = DocumentProcessor()

        # Read file data
        file_data = await file.read()

        # Process document
        result = await processor.process_document(
            file_data,
            filename=file.filename or "document",
            context=context
        )

        return DocumentInputResponse(
            stories=result.get("stories", []),
            document_type=result.get("document_type", "unknown"),
            filename=result.get("filename", file.filename),
            file_size=result.get("file_size", len(file_data)),
            processing_time=result.get("processing_time", 0),
            source="document"
        )

    except DocumentProcessorError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except ImportError as e:
        raise HTTPException(
            status_code=503,
            detail=f"Document processor not available: {e}"
        )
    except Exception as e:
        logger.error(f"Document processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"Document processing failed: {e}")


@router.get("/inputs/document/status")
async def document_processor_status():
    """Get status of document processor."""
    try:
        from factory.inputs.document_processor import DocumentProcessor

        processor = DocumentProcessor()
        return processor.get_status()
    except ImportError as e:
        return {
            "available": False,
            "error": str(e)
        }


# =============================================================================
# WHATSAPP ROUTES
# =============================================================================

@router.post("/inputs/whatsapp", response_model=WhatsAppMessageResponse)
async def handle_whatsapp_message(
    request: WhatsAppMessageRequest,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Process WhatsApp message for requirements capture.

    This endpoint is for direct API calls. For Twilio webhooks,
    use /webhooks/whatsapp.

    **Commands:**
    - `/reset` - Reset conversation
    - `/status` - Get conversation status
    """
    try:
        from factory.inputs.whatsapp_integration import WhatsAppIntegration, WhatsAppError

        whatsapp = WhatsAppIntegration()

        result = await whatsapp.handle_message(
            from_number=request.from_number,
            message=request.message,
            tenant_id=request.tenant_id or tenant_id
        )

        return WhatsAppMessageResponse(
            response=result.get("response", ""),
            stories=result.get("stories", []),
            conversation_state=result.get("conversation_state")
        )

    except WhatsAppError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except ImportError as e:
        raise HTTPException(
            status_code=503,
            detail=f"WhatsApp integration not available: {e}"
        )
    except Exception as e:
        logger.error(f"WhatsApp message processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"WhatsApp processing failed: {e}")


@router.post("/webhooks/whatsapp")
async def whatsapp_webhook(request: Request):
    """
    Twilio WhatsApp Webhook endpoint.

    Configure this URL in your Twilio WhatsApp sandbox or
    WhatsApp Business API settings.

    Returns TwiML response.
    """
    try:
        from factory.inputs.whatsapp_integration import WhatsAppIntegration, WhatsAppError

        # Parse Twilio form data
        form = await request.form()
        from_number = form.get("From", "")
        message = form.get("Body", "")
        media_url = form.get("MediaUrl0")  # Optional voice/image

        if not from_number or not message:
            return Response(
                content='<?xml version="1.0" encoding="UTF-8"?><Response><Message>Mensagem invalida</Message></Response>',
                media_type="application/xml"
            )

        whatsapp = WhatsAppIntegration()

        # Extract tenant from phone number
        tenant_id = whatsapp.extract_tenant_from_number(from_number)

        # Process message
        result = await whatsapp.handle_message(
            from_number=from_number,
            message=message,
            tenant_id=tenant_id,
            media_url=media_url
        )

        # Create TwiML response
        twiml = whatsapp.create_webhook_response(result.get("response", "Erro ao processar mensagem"))

        return Response(content=twiml, media_type="application/xml")

    except Exception as e:
        logger.error(f"WhatsApp webhook failed: {e}")
        return Response(
            content=f'<?xml version="1.0" encoding="UTF-8"?><Response><Message>Erro interno: {str(e)[:100]}</Message></Response>',
            media_type="application/xml"
        )


@router.get("/inputs/whatsapp/status")
async def whatsapp_status():
    """Get status of WhatsApp integration."""
    try:
        from factory.inputs.whatsapp_integration import WhatsAppIntegration

        whatsapp = WhatsAppIntegration()
        return whatsapp.get_status()
    except ImportError as e:
        return {
            "available": False,
            "error": str(e)
        }


@router.get("/inputs/whatsapp/conversations")
async def list_whatsapp_conversations(
    tenant_id: str = Depends(get_tenant_id)
):
    """List active WhatsApp conversations."""
    try:
        from factory.inputs.whatsapp_integration import WhatsAppIntegration

        whatsapp = WhatsAppIntegration()
        return {
            "conversations": whatsapp.get_all_conversations(tenant_id=tenant_id)
        }
    except ImportError as e:
        raise HTTPException(status_code=503, detail=str(e))


# =============================================================================
# EMAIL INPUT ROUTES
# =============================================================================

@router.post("/inputs/email")
async def process_email(
    request: EmailInputRequest,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Extract User Stories from email content.

    Send email subject and body to extract requirements.
    """
    try:
        from factory.inputs.omnichannel_hub import EmailProcessor

        processor = EmailProcessor()

        stories = await processor.process({
            "subject": request.subject,
            "body": request.body,
            "from_email": request.from_email
        })

        return {
            "stories": stories,
            "stories_count": len(stories),
            "source": "email"
        }

    except Exception as e:
        logger.error(f"Email processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"Email processing failed: {e}")


# =============================================================================
# DIRECT API INPUT ROUTES
# =============================================================================

@router.post("/inputs/api")
async def process_api_input(
    request: APIInputRequest,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Direct API input for programmatic story creation.

    Accepts a list of stories and normalizes them.
    """
    try:
        from factory.inputs.omnichannel_hub import DirectAPIInput

        processor = DirectAPIInput()

        stories = await processor.process({"stories": request.stories})

        # Add tenant_id to all stories
        for story in stories:
            story["tenant_id"] = tenant_id

        return {
            "stories": stories,
            "stories_count": len(stories),
            "source": "api"
        }

    except Exception as e:
        logger.error(f"API input processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"API input processing failed: {e}")


# =============================================================================
# OMNICHANNEL HUB ROUTES
# =============================================================================

@router.post("/inputs/hub", response_model=OmnichannelInputResponse)
async def omnichannel_input(
    source: str = Form(..., description="Input source (voice, document, whatsapp, email, api)"),
    data: Optional[str] = Form(None, description="Data for text-based sources"),
    file: Optional[UploadFile] = File(None, description="File for voice/document sources"),
    context: Optional[str] = Form(None, description="Optional project context"),
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Unified omnichannel input endpoint.

    Process input from any source through a single endpoint.

    **Sources:**
    - `voice` - Audio file upload
    - `document` - Document file upload
    - `whatsapp` - WhatsApp message (JSON in data field)
    - `email` - Email content (JSON in data field)
    - `api` - Direct story data (JSON in data field)
    """
    try:
        from factory.inputs.omnichannel_hub import OmnichannelHub, OmnichannelHubError
        import json

        hub = OmnichannelHub(tenant_id=tenant_id)

        # Prepare input data based on source
        if source in ["voice", "document"]:
            if not file:
                raise HTTPException(
                    status_code=400,
                    detail=f"File is required for {source} source"
                )
            input_data = await file.read()
            metadata = {
                "filename": file.filename,
                "context": context
            }
        else:
            if not data:
                raise HTTPException(
                    status_code=400,
                    detail=f"Data is required for {source} source"
                )
            try:
                input_data = json.loads(data)
            except json.JSONDecodeError:
                input_data = data
            metadata = {"context": context}

        # Process through hub
        result = await hub.process_input(
            source=source,
            data=input_data,
            metadata=metadata
        )

        return OmnichannelInputResponse(**result)

    except OmnichannelHubError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Omnichannel hub processing failed: {e}")
        raise HTTPException(status_code=500, detail=f"Hub processing failed: {e}")


@router.get("/inputs/hub/history")
async def get_input_history(
    source: Optional[str] = Query(None, description="Filter by source"),
    status: Optional[str] = Query(None, description="Filter by status"),
    limit: int = Query(50, ge=1, le=100),
    tenant_id: str = Depends(get_tenant_id)
):
    """Get input processing history."""
    try:
        from factory.inputs.omnichannel_hub import OmnichannelHub

        hub = OmnichannelHub(tenant_id=tenant_id)

        filters = {}
        if source:
            filters["source"] = source
        if status:
            filters["status"] = status

        history = hub.get_input_history(filters=filters, limit=limit)

        return {
            "history": history,
            "count": len(history)
        }

    except Exception as e:
        logger.error(f"Failed to get input history: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/inputs/hub/stats")
async def get_input_stats(tenant_id: str = Depends(get_tenant_id)):
    """Get input processing statistics."""
    try:
        from factory.inputs.omnichannel_hub import OmnichannelHub

        hub = OmnichannelHub(tenant_id=tenant_id)
        return hub.get_stats()

    except Exception as e:
        logger.error(f"Failed to get stats: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/inputs/status", response_model=InputStatusResponse)
async def get_inputs_status():
    """Get status of all input processing systems."""
    try:
        from factory.inputs.omnichannel_hub import OmnichannelHub

        hub = OmnichannelHub(tenant_id="status-check")
        sources = hub.get_available_sources()

        return InputStatusResponse(
            voice=sources.get("voice", {"available": False}),
            document=sources.get("document", {"available": False}),
            whatsapp=sources.get("whatsapp", {"available": False}),
            email=sources.get("email", {"available": False}),
            api=sources.get("api", {"available": False})
        )

    except Exception as e:
        logger.error(f"Failed to get status: {e}")
        return InputStatusResponse(
            voice={"available": False, "error": str(e)},
            document={"available": False, "error": str(e)},
            whatsapp={"available": False, "error": str(e)},
            email={"available": False, "error": str(e)},
            api={"available": False, "error": str(e)}
        )
