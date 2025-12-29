# -*- coding: utf-8 -*-
"""SAP S/4HANA Sales Order API - Issue #101"""
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional
from enum import Enum
from .odata_v4_client import ODataV4Client, ODataQueryBuilder, ODataConfig, FilterOperator
from .sap_auth import SAPAuthenticator

logger = logging.getLogger(__name__)

class SalesOrderType(str, Enum):
    STANDARD = "OR"
    RUSH = "RO"
    CASH = "BV"
    FREE_OF_CHARGE = "FD"
    CONSIGNMENT = "KB"
    RETURNS = "RE"

class SalesOrderStatus(str, Enum):
    OPEN = "A"
    IN_PROCESS = "B"
    COMPLETED = "C"

@dataclass
class SalesOrderItem:
    material: str
    requested_quantity: float
    sales_unit: str = "PC"
    net_price: float = 0
    plant: str = ""
    batch: str = ""
    def to_dict(self) -> Dict:
        return {
            "Material": self.material,
            "RequestedQuantity": str(self.requested_quantity),
            "RequestedQuantityUnit": self.sales_unit,
            "NetPriceAmount": str(self.net_price) if self.net_price else None,
            "ProductionPlant": self.plant if self.plant else None,
            "Batch": self.batch if self.batch else None
        }

@dataclass
class SalesOrderPartner:
    partner_function: str
    customer: str
    def to_dict(self) -> Dict:
        return {"PartnerFunction": self.partner_function, "Customer": self.customer}

class SalesOrderAPI:
    ENTITY_SET = "A_SalesOrder"
    ITEM_ENTITY = "to_Item"
    PARTNER_ENTITY = "to_Partner"
    SCHEDULE_ENTITY = "to_ScheduleLine"

    def __init__(self, config: ODataConfig, authenticator: SAPAuthenticator):
        self.client = ODataV4Client(config, authenticator)

    async def list_sales_orders(self, customer=None, sales_org=None, order_type=None, date_from=None, date_to=None, limit=100, offset=0) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("SalesOrder", "SalesOrderType", "SalesOrganization", "SoldToParty", "TotalNetAmount", "TransactionCurrency", "SalesOrderDate", "RequestedDeliveryDate", "OverallSDProcessStatus")
        if customer:
            query.filter("SoldToParty", FilterOperator.EQ, customer)
        if sales_org:
            query.filter("SalesOrganization", FilterOperator.EQ, sales_org)
        if order_type:
            query.filter("SalesOrderType", FilterOperator.EQ, order_type.value if isinstance(order_type, SalesOrderType) else order_type)
        if date_from:
            query.filter("SalesOrderDate", FilterOperator.GE, date_from.strftime("%Y-%m-%d") if isinstance(date_from, datetime) else date_from)
        if date_to:
            query.filter("SalesOrderDate", FilterOperator.LE, date_to.strftime("%Y-%m-%d") if isinstance(date_to, datetime) else date_to)
        query.top(limit).skip(offset).orderby("SalesOrder", desc=True)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items

    async def get_sales_order(self, order_number: str, include_all=True) -> Optional[Dict]:
        expand = [self.ITEM_ENTITY, self.PARTNER_ENTITY, self.SCHEDULE_ENTITY] if include_all else [self.ITEM_ENTITY]
        response = await self.client.get(self.ENTITY_SET, key=f"'{order_number}'", expand=expand)
        return response.first

    async def create_sales_order(self, order_type, sales_org, distribution_channel, division, sold_to_party, items, requested_delivery_date=None, purchase_order_by_customer="", partners=None) -> Optional[Dict]:
        payload = {
            "SalesOrderType": order_type.value if isinstance(order_type, SalesOrderType) else order_type,
            "SalesOrganization": sales_org,
            "DistributionChannel": distribution_channel,
            "OrganizationDivision": division,
            "SoldToParty": sold_to_party,
            "PurchaseOrderByCustomer": purchase_order_by_customer
        }
        if requested_delivery_date:
            payload["RequestedDeliveryDate"] = requested_delivery_date.strftime("%Y-%m-%d") if isinstance(requested_delivery_date, datetime) else requested_delivery_date
        payload[self.ITEM_ENTITY] = [item.to_dict() if hasattr(item, "to_dict") else item for item in items]
        if partners:
            payload[self.PARTNER_ENTITY] = [p.to_dict() if hasattr(p, "to_dict") else p for p in partners]
        response = await self.client.create(self.ENTITY_SET, payload)
        return response.first

    async def update_sales_order(self, order_number, requested_delivery_date=None, purchase_order="") -> bool:
        payload = {}
        if requested_delivery_date:
            payload["RequestedDeliveryDate"] = requested_delivery_date.strftime("%Y-%m-%d") if isinstance(requested_delivery_date, datetime) else requested_delivery_date
        if purchase_order:
            payload["PurchaseOrderByCustomer"] = purchase_order
        if payload:
            await self.client.update(f"{self.ENTITY_SET}('{order_number}')", payload)
        return True

    async def reject_sales_order(self, order_number: str, reason: str = "") -> bool:
        return await self.client.call_action("Reject", parameters={"Reason": reason}, bound_entity=f"{self.ENTITY_SET}('{order_number}')")

    async def add_item(self, order_number: str, item: SalesOrderItem) -> Optional[Dict]:
        entity_path = f"{self.ENTITY_SET}('{order_number}')/{self.ITEM_ENTITY}"
        response = await self.client.create(entity_path, item.to_dict())
        return response.first

    async def update_item(self, order_number, item_number, quantity=None, price=None) -> bool:
        payload = {}
        if quantity is not None:
            payload["RequestedQuantity"] = str(quantity)
        if price is not None:
            payload["NetPriceAmount"] = str(price)
        if payload:
            await self.client.update(f"{self.ENTITY_SET}('{order_number}')/{self.ITEM_ENTITY}('{item_number}')", payload)
        return True

    async def get_order_status(self, order_number: str) -> Dict:
        order = await self.get_sales_order(order_number, include_all=False)
        if not order:
            return {}
        return {
            "order_number": order.get("SalesOrder"),
            "status": order.get("OverallSDProcessStatus"),
            "delivery_status": order.get("OverallDeliveryStatus"),
            "billing_status": order.get("OverallBillingStatus"),
            "total_amount": order.get("TotalNetAmount"),
            "currency": order.get("TransactionCurrency")
        }

    async def search(self, term, limit=50) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("SalesOrder", "SoldToParty", "TotalNetAmount", "TransactionCurrency", "SalesOrderDate")
        query.search(term).top(limit).orderby("SalesOrder", desc=True)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items
