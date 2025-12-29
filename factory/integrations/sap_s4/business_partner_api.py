# -*- coding: utf-8 -*-
"""SAP S/4HANA Business Partner API - Issue #101"""
import logging
from dataclasses import dataclass
from typing import Dict, List, Optional
from enum import Enum
from .odata_v4_client import ODataV4Client, ODataQueryBuilder, ODataConfig, FilterOperator
from .sap_auth import SAPAuthenticator

logger = logging.getLogger(__name__)

class BusinessPartnerCategory(str, Enum):
    ORGANIZATION = "1"
    PERSON = "2"
    GROUP = "3"

class BusinessPartnerRole(str, Enum):
    CUSTOMER = "FLCU00"
    SUPPLIER = "FLVN00"
    CONTACT = "BUP001"
    EMPLOYEE = "BUP003"

@dataclass
class BusinessPartnerAddress:
    country: str
    region: str = ""
    city: str = ""
    postal_code: str = ""
    street: str = ""
    house_number: str = ""
    def to_dict(self) -> Dict:
        return {"Country": self.country, "Region": self.region, "CityName": self.city, "PostalCode": self.postal_code, "StreetName": self.street, "HouseNumber": self.house_number}

class BusinessPartnerAPI:
    ENTITY_SET = "A_BusinessPartner"
    def __init__(self, config: ODataConfig, authenticator: SAPAuthenticator):
        self.client = ODataV4Client(config, authenticator)

    async def list_business_partners(self, category=None, search_term=None, limit=100) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("BusinessPartner", "BusinessPartnerFullName", "BusinessPartnerCategory", "SearchTerm1")
        if category: query.filter("BusinessPartnerCategory", FilterOperator.EQ, category.value)
        if search_term: query.filter("SearchTerm1", FilterOperator.CONTAINS, search_term.upper())
        query.top(limit).orderby("BusinessPartnerFullName")
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items

    async def get_business_partner(self, bp_id: str) -> Optional[Dict]:
        response = await self.client.get(self.ENTITY_SET, key=f"'{bp_id}'")
        return response.first

    async def create_business_partner(self, category, name, search_term) -> Optional[Dict]:
        payload = {"BusinessPartnerCategory": category.value, "SearchTerm1": search_term[:20]}
        if category == BusinessPartnerCategory.ORGANIZATION: payload["OrganizationBPName1"] = name[:40]
        response = await self.client.create(self.ENTITY_SET, payload)
        return response.first

    async def search(self, term, limit=50) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("BusinessPartner", "BusinessPartnerFullName").search(term).top(limit)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items
