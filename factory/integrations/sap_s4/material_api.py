# -*- coding: utf-8 -*-
"""SAP S/4HANA Material API - Issue #101"""
import logging
from dataclasses import dataclass
from typing import Dict, List, Optional
from enum import Enum
from .odata_v4_client import ODataV4Client, ODataQueryBuilder, ODataConfig, FilterOperator
from .sap_auth import SAPAuthenticator

logger = logging.getLogger(__name__)

class MaterialType(str, Enum):
    FINISHED = "FERT"
    RAW = "ROH"
    SEMIFINISHED = "HALB"
    TRADING = "HAWA"
    SERVICE = "DIEN"
    PACKAGING = "VERP"

class MaterialStatus(str, Enum):
    ACTIVE = ""
    BLOCKED = "01"
    DISCONTINUED = "02"

@dataclass
class MaterialPlant:
    plant: str
    storage_location: str = ""
    mrp_type: str = "PD"
    reorder_point: float = 0
    safety_stock: float = 0
    def to_dict(self) -> Dict:
        return {"Plant": self.plant, "StorageLocation": self.storage_location, "MRPType": self.mrp_type, "ReorderThresholdQuantity": self.reorder_point, "SafetyStock": self.safety_stock}

@dataclass
class MaterialSales:
    sales_org: str
    distribution_channel: str
    division: str = "00"
    sales_unit: str = "PC"
    def to_dict(self) -> Dict:
        return {"SalesOrganization": self.sales_org, "DistributionChannel": self.distribution_channel, "Division": self.division, "SalesUnit": self.sales_unit}

class MaterialAPI:
    ENTITY_SET = "A_Product"
    PLANT_ENTITY = "to_Plant"
    SALES_ENTITY = "to_ProductSalesDelivery"
    DESCRIPTION_ENTITY = "to_Description"

    def __init__(self, config: ODataConfig, authenticator: SAPAuthenticator):
        self.client = ODataV4Client(config, authenticator)

    async def list_materials(self, material_type=None, search_term=None, plant=None, limit=100, offset=0) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("Product", "ProductType", "BaseUnit", "ProductGroup", "CreationDate", "LastChangeDate", "IsMarkedForDeletion")
        if material_type:
            query.filter("ProductType", FilterOperator.EQ, material_type.value)
        if search_term:
            query.filter("Product", FilterOperator.CONTAINS, search_term.upper())
        query.expand(self.DESCRIPTION_ENTITY)
        query.top(limit).skip(offset).orderby("Product")
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items

    async def get_material(self, material_number: str, include_all=True) -> Optional[Dict]:
        expand = [self.PLANT_ENTITY, self.SALES_ENTITY, self.DESCRIPTION_ENTITY] if include_all else [self.DESCRIPTION_ENTITY]
        response = await self.client.get(self.ENTITY_SET, key=f"'{material_number}'", expand=expand)
        return response.first

    async def create_material(self, material_number, material_type, base_unit="PC", product_group="", descriptions=None, plant_data=None, sales_data=None) -> Optional[Dict]:
        payload = {
            "Product": material_number.upper()[:40],
            "ProductType": material_type.value if isinstance(material_type, MaterialType) else material_type,
            "BaseUnit": base_unit,
            "ProductGroup": product_group
        }
        if descriptions:
            payload[self.DESCRIPTION_ENTITY] = [{"Language": lang, "ProductDescription": desc} for lang, desc in descriptions.items()]
        if plant_data:
            payload[self.PLANT_ENTITY] = [p.to_dict() if hasattr(p, "to_dict") else p for p in plant_data]
        if sales_data:
            payload[self.SALES_ENTITY] = [s.to_dict() if hasattr(s, "to_dict") else s for s in sales_data]
        response = await self.client.create(self.ENTITY_SET, payload)
        return response.first

    async def update_material(self, material_number, product_group=None, is_deleted=None) -> bool:
        payload = {}
        if product_group:
            payload["ProductGroup"] = product_group
        if is_deleted is not None:
            payload["IsMarkedForDeletion"] = is_deleted
        if payload:
            await self.client.update(f"{self.ENTITY_SET}('{material_number}')", payload)
        return True

    async def delete_material(self, material_number: str) -> bool:
        return await self.client.delete(f"{self.ENTITY_SET}('{material_number}')")

    async def get_material_stock(self, material_number: str, plant: str = None) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("Material", "Plant", "StorageLocation", "MatlWrhsStkQtyInMatlBaseUnit", "MaterialBaseUnit")
        query.filter("Material", FilterOperator.EQ, material_number)
        if plant:
            query.filter("Plant", FilterOperator.EQ, plant)
        response = await self.client.query("A_MatlStkInAcctMod", query)
        return response.items

    async def search(self, term, limit=50) -> List[Dict]:
        query = ODataQueryBuilder()
        query.select("Product", "ProductType", "BaseUnit")
        query.search(term).top(limit)
        query.expand(self.DESCRIPTION_ENTITY)
        response = await self.client.query(self.ENTITY_SET, query)
        return response.items
