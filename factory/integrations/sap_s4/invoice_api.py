# -*- coding: utf-8 -*-
"""SAP S/4HANA Invoice/Billing Document API - Issue #101"""
import logging
from dataclasses import dataclass
from datetime import datetime, date
from typing import Dict, List, Optional
from enum import Enum
from .odata_v4_client import ODataV4Client, ODataQueryBuilder, ODataConfig, FilterOperator
from .sap_auth import SAPAuthenticator

logger = logging.getLogger(__name__)

class BillingDocumentType(str, Enum):
    INVOICE = "F2"
    CREDIT_MEMO = "G2"
    DEBIT_MEMO = "L2"
    PRO_FORMA = "F5"
    INTERCOMPANY = "IV"
    INVOICE_CANCELLATION = "S1"
    CASH_SALES = "BV"

class BillingDocumentStatus(str, Enum):
    NOT_PROCESSED = ""
    PARTIALLY_PROCESSED = "A"
    COMPLETELY_PROCESSED = "C"
    CANCELLED = "X"

@dataclass
class BillingDocumentItem:
    material: str
    billing_quantity: float
    billing_quantity_unit: str = "PC"
    net_amount: float = 0.0
    tax_amount: float = 0.0
    plant: str = ""
    def to_dict(self) -> Dict:
        return {"Material": self.material, "BillingQuantity": str(self.billing_quantity), "BillingQuantityUnit": self.billing_quantity_unit, "NetAmount": str(self.net_amount) if self.net_amount else None, "Plant": self.plant if self.plant else None}

@dataclass
class BillingDocumentPartner:
    partner_function: str
    customer: str
    def to_dict(self) -> Dict:
        return {"PartnerFunction": self.partner_function, "Customer": self.customer}

class InvoiceAPI:
    ENTITY_SET = "A_BillingDocument"
    ITEM_ENTITY = "to_Item"
    PARTNER_ENTITY = "to_Partner"

    def __init__(self, config: ODataConfig, authenticator: SAPAuthenticator):
        self.client = ODataV4Client(config, authenticator)

    async def list_invoices(self, customer=None, sales_org=None, document_type=None, date_from=None, date_to=None, limit=100, offset=0) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("BillingDocument", "BillingDocumentType", "SalesOrganization", "SoldToParty", "BillingDocumentDate", "TotalNetAmount", "TaxAmount", "TransactionCurrency", "BillingDocumentIsCancelled", "AccountingPostingStatus")
        if customer:
            query.filter("SoldToParty", FilterOperator.EQ, customer)
        if sales_org:
            query.filter("SalesOrganization", FilterOperator.EQ, sales_org)
        if document_type:
            query.filter("BillingDocumentType", FilterOperator.EQ, document_type.value if isinstance(document_type, BillingDocumentType) else document_type)
        if date_from:
            query.filter("BillingDocumentDate", FilterOperator.GE, date_from.strftime("%Y-%m-%d") if isinstance(date_from, (datetime, date)) else date_from)
        if date_to:
            query.filter("BillingDocumentDate", FilterOperator.LE, date_to.strftime("%Y-%m-%d") if isinstance(date_to, (datetime, date)) else date_to)
        query.top(limit).skip(offset).orderby("BillingDocument", desc=True)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items

    async def get_invoice(self, document_number: str, include_all=True) -> Optional[Dict]:
        expand = [self.ITEM_ENTITY, self.PARTNER_ENTITY] if include_all else [self.ITEM_ENTITY]
        response = await self.client.get(self.ENTITY_SET, key=f"'{document_number}'", expand=expand)
        return response.first

    async def create_invoice(self, document_type, sales_org, distribution_channel, division, sold_to_party, items, billing_date=None, payer=None, partners=None) -> Optional[Dict]:
        payload = {"BillingDocumentType": document_type.value if isinstance(document_type, BillingDocumentType) else document_type, "SalesOrganization": sales_org, "DistributionChannel": distribution_channel, "Division": division, "SoldToParty": sold_to_party}
        if billing_date:
            payload["BillingDocumentDate"] = billing_date.strftime("%Y-%m-%d") if isinstance(billing_date, (datetime, date)) else billing_date
        if payer:
            payload["PayerParty"] = payer
        payload[self.ITEM_ENTITY] = [item.to_dict() if hasattr(item, "to_dict") else item for item in items]
        if partners:
            payload[self.PARTNER_ENTITY] = [p.to_dict() if hasattr(p, "to_dict") else p for p in partners]
        response = await self.client.create(self.ENTITY_SET, payload)
        return response.first

    async def create_invoice_from_sales_order(self, sales_order: str, billing_date=None) -> Optional[Dict]:
        parameters = {"SalesDocument": sales_order}
        if billing_date:
            parameters["BillingDocumentDate"] = billing_date.strftime("%Y-%m-%d") if isinstance(billing_date, (datetime, date)) else billing_date
        response = await self.client.call_action("CreateBillingDocumentFromSalesOrder", parameters=parameters)
        return response.first

    async def cancel_invoice(self, document_number: str, reason: str = "") -> Optional[Dict]:
        return await self.client.call_action("CancelBillingDocument", parameters={"CancellationReason": reason}, bound_entity=f"{self.ENTITY_SET}('{document_number}')")

    async def release_to_accounting(self, document_number: str) -> bool:
        response = await self.client.call_action("ReleaseBillingDocumentToAccounting", bound_entity=f"{self.ENTITY_SET}('{document_number}')")
        return response is not None

    async def get_invoice_status(self, document_number: str) -> Dict:
        invoice = await self.get_invoice(document_number, include_all=False)
        if not invoice:
            return {}
        return {"document_number": invoice.get("BillingDocument"), "is_cancelled": invoice.get("BillingDocumentIsCancelled"), "accounting_status": invoice.get("AccountingPostingStatus"), "total_amount": invoice.get("TotalNetAmount"), "currency": invoice.get("TransactionCurrency")}

    async def search(self, term: str, limit=50) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("BillingDocument", "SoldToParty", "TotalNetAmount", "TransactionCurrency", "BillingDocumentDate")
        query.search(term).top(limit).orderby("BillingDocument", desc=True)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items

