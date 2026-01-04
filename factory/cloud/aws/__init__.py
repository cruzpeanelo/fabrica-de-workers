# -*- coding: utf-8 -*-
"""
AWS Cloud Provider Module
=========================
Implementacao do provider AWS para a Plataforma E.

Recursos suportados:
- EC2 (Maquinas virtuais)
- S3 (Object Storage)
- Lambda (Serverless functions)
- RDS (Banco de dados gerenciado)
- VPC e Security Groups
"""

from .provider import AWSProvider
from .ec2 import EC2Manager
from .s3 import S3Manager
from .lambda_manager import LambdaManager
from .rds import RDSManager

__all__ = [
    "AWSProvider",
    "EC2Manager",
    "S3Manager",
    "LambdaManager",
    "RDSManager",
]
