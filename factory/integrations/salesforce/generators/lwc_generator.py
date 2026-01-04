# -*- coding: utf-8 -*-
"""
Salesforce LWC Generator
========================
Gerador de Lightning Web Components.

Funcionalidades:
- Geracao de componentes LWC completos
- Templates para diferentes casos de uso
- Geracao de testes Jest
- Boas praticas automaticas

Exemplo de uso:
    from factory.integrations.salesforce.generators import LWCGenerator

    generator = LWCGenerator()

    # Gerar componente basico
    files = generator.generate_component("myComponent", "Meu Componente")

    # Gerar componente de formulario
    files = generator.generate_form_component("accountForm", "Account", ["Name", "Phone"])

    # Gerar componente de lista
    files = generator.generate_list_component("accountList", "Account")
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)


class LWCComponentType(str, Enum):
    """Tipos de componentes LWC"""
    BASIC = "Basic"
    FORM = "Form"
    LIST = "List"
    DATATABLE = "DataTable"
    CARD = "Card"
    MODAL = "Modal"
    NAVIGATION = "Navigation"
    FLOW_SCREEN = "FlowScreen"


@dataclass
class LWCField:
    """Definicao de um campo para formularios"""
    api_name: str
    label: str
    type: str = "text"
    required: bool = False
    placeholder: str = ""
    max_length: Optional[int] = None
    options: List[Dict[str, str]] = field(default_factory=list)


@dataclass
class LWCColumn:
    """Definicao de uma coluna para datatables"""
    field_name: str
    label: str
    type: str = "text"
    sortable: bool = False
    type_attributes: Dict[str, Any] = field(default_factory=dict)


class LWCGenerator:
    """
    Gerador de Lightning Web Components

    Gera componentes LWC completos seguindo
    boas praticas do Salesforce.
    """

    def __init__(self, api_version: str = "59.0"):
        """
        Inicializa o gerador

        Args:
            api_version: Versao da API Salesforce
        """
        self.api_version = api_version
        self._indent = "    "

    def generate_component(
        self,
        name: str,
        label: str,
        description: str = "",
        targets: Optional[List[str]] = None,
        is_exposed: bool = True
    ) -> Dict[str, str]:
        """
        Gera componente LWC basico

        Args:
            name: Nome do componente (camelCase)
            label: Label do componente
            description: Descricao
            targets: Alvos onde pode ser usado
            is_exposed: Se e exposto para uso

        Returns:
            Dict com arquivos {nome: conteudo}
        """
        targets = targets or ["lightning__RecordPage", "lightning__AppPage", "lightning__HomePage"]

        files = {
            f"{name}.js": self._generate_basic_js(name),
            f"{name}.html": self._generate_basic_html(name),
            f"{name}.css": self._generate_basic_css(),
            f"{name}.js-meta.xml": self._generate_meta_xml(name, label, description, targets, is_exposed)
        }

        return files

    def generate_form_component(
        self,
        name: str,
        sobject: str,
        fields: List[str],
        label: str = "",
        include_validation: bool = True
    ) -> Dict[str, str]:
        """
        Gera componente de formulario

        Args:
            name: Nome do componente
            sobject: Objeto Salesforce
            fields: Lista de campos
            label: Label do componente
            include_validation: Incluir validacao

        Returns:
            Dict com arquivos
        """
        label = label or f"Formulario de {sobject}"

        files = {
            f"{name}.js": self._generate_form_js(name, sobject, fields, include_validation),
            f"{name}.html": self._generate_form_html(name, sobject, fields),
            f"{name}.css": self._generate_form_css(),
            f"{name}.js-meta.xml": self._generate_meta_xml(
                name, label, f"Formulario para {sobject}",
                ["lightning__RecordPage"], True
            )
        }

        return files

    def generate_list_component(
        self,
        name: str,
        sobject: str,
        columns: Optional[List[LWCColumn]] = None,
        label: str = "",
        include_search: bool = True,
        include_pagination: bool = True
    ) -> Dict[str, str]:
        """
        Gera componente de lista/datatable

        Args:
            name: Nome do componente
            sobject: Objeto Salesforce
            columns: Colunas da tabela
            label: Label do componente
            include_search: Incluir busca
            include_pagination: Incluir paginacao

        Returns:
            Dict com arquivos
        """
        label = label or f"Lista de {sobject}"

        if not columns:
            columns = [
                LWCColumn("Name", "Nome", sortable=True),
                LWCColumn("CreatedDate", "Data de Criacao", type="date", sortable=True)
            ]

        files = {
            f"{name}.js": self._generate_list_js(name, sobject, columns, include_search, include_pagination),
            f"{name}.html": self._generate_list_html(name, columns, include_search, include_pagination),
            f"{name}.css": self._generate_list_css(),
            f"{name}.js-meta.xml": self._generate_meta_xml(
                name, label, f"Lista de {sobject}",
                ["lightning__AppPage", "lightning__HomePage"], True
            )
        }

        return files

    def generate_modal_component(
        self,
        name: str,
        label: str,
        content_type: str = "custom"
    ) -> Dict[str, str]:
        """
        Gera componente modal

        Args:
            name: Nome do componente
            label: Label/titulo do modal
            content_type: Tipo de conteudo

        Returns:
            Dict com arquivos
        """
        files = {
            f"{name}.js": self._generate_modal_js(name, label),
            f"{name}.html": self._generate_modal_html(name, label),
            f"{name}.css": self._generate_modal_css(),
            f"{name}.js-meta.xml": self._generate_meta_xml(
                name, label, "Componente Modal",
                [], False
            )
        }

        return files

    def generate_apex_wrapper_component(
        self,
        name: str,
        apex_class: str,
        apex_method: str,
        label: str = ""
    ) -> Dict[str, str]:
        """
        Gera componente que chama metodo Apex

        Args:
            name: Nome do componente
            apex_class: Classe Apex
            apex_method: Metodo Apex
            label: Label do componente

        Returns:
            Dict com arquivos
        """
        label = label or f"{apex_class} - {apex_method}"

        files = {
            f"{name}.js": self._generate_apex_wrapper_js(name, apex_class, apex_method),
            f"{name}.html": self._generate_apex_wrapper_html(name),
            f"{name}.css": self._generate_basic_css(),
            f"{name}.js-meta.xml": self._generate_meta_xml(
                name, label, f"Wrapper para {apex_class}.{apex_method}",
                ["lightning__AppPage"], True
            )
        }

        return files

    # ==================== JAVASCRIPT GENERATORS ====================

    def _generate_basic_js(self, name: str) -> str:
        """Gera JavaScript basico"""
        class_name = self._to_pascal_case(name)

        return f"""import {{ LightningElement, api }} from 'lwc';

/**
 * {class_name}
 * Componente Lightning Web Component
 *
 * @author Plataforma E
 * @date {datetime.now().strftime('%Y-%m-%d')}
 */
export default class {class_name} extends LightningElement {{
    @api recordId;

    connectedCallback() {{
        // Inicializacao quando componente e conectado ao DOM
        console.log('{name} carregado');
    }}

    handleClick(event) {{
        // Handler de exemplo
        const detail = {{ message: 'Clicado!' }};
        this.dispatchEvent(new CustomEvent('action', {{ detail }}));
    }}
}}
"""

    def _generate_form_js(
        self,
        name: str,
        sobject: str,
        fields: List[str],
        include_validation: bool
    ) -> str:
        """Gera JavaScript de formulario"""
        class_name = self._to_pascal_case(name)
        fields_import = ", ".join([f.upper() + "_FIELD" for f in fields[:3]])

        validation_code = ""
        if include_validation:
            validation_code = """
    validateFields() {
        const inputFields = this.template.querySelectorAll('lightning-input-field');
        let isValid = true;

        inputFields.forEach(field => {
            if (!field.reportValidity()) {
                isValid = false;
            }
        });

        return isValid;
    }
"""

        return f"""import {{ LightningElement, api, track, wire }} from 'lwc';
import {{ ShowToastEvent }} from 'lightning/platformShowToastEvent';
import {{ getRecord }} from 'lightning/uiRecordApi';

// Campos do objeto
// import {fields_import} from '@salesforce/schema/{sobject}.Id';

/**
 * {class_name}
 * Formulario para {sobject}
 */
export default class {class_name} extends LightningElement {{
    @api recordId;
    @api objectApiName = '{sobject}';

    @track isLoading = false;
    @track error;

    // Campos do formulario
    fields = {fields};

    handleSubmit(event) {{
        event.preventDefault();

        if (!this.validateFields()) {{
            return;
        }}

        this.isLoading = true;
        const fields = event.detail.fields;

        this.template.querySelector('lightning-record-edit-form').submit(fields);
    }}

    handleSuccess(event) {{
        this.isLoading = false;
        const recordId = event.detail.id;

        this.dispatchEvent(new ShowToastEvent({{
            title: 'Sucesso',
            message: 'Registro salvo com sucesso',
            variant: 'success'
        }}));

        // Disparar evento de sucesso
        this.dispatchEvent(new CustomEvent('success', {{
            detail: {{ recordId }}
        }}));
    }}

    handleError(event) {{
        this.isLoading = false;
        this.error = event.detail.message;

        this.dispatchEvent(new ShowToastEvent({{
            title: 'Erro',
            message: this.error,
            variant: 'error'
        }}));
    }}

    handleCancel() {{
        this.dispatchEvent(new CustomEvent('cancel'));
    }}
{validation_code}
    validateFields() {{
        const inputFields = this.template.querySelectorAll('lightning-input-field');
        let isValid = true;

        inputFields.forEach(field => {{
            if (!field.reportValidity()) {{
                isValid = false;
            }}
        }});

        return isValid;
    }}
}}
"""

    def _generate_list_js(
        self,
        name: str,
        sobject: str,
        columns: List[LWCColumn],
        include_search: bool,
        include_pagination: bool
    ) -> str:
        """Gera JavaScript de lista"""
        class_name = self._to_pascal_case(name)

        # Gerar definicao de colunas
        columns_def = []
        for col in columns:
            col_def = f"{{ label: '{col.label}', fieldName: '{col.field_name}', type: '{col.type}'"
            if col.sortable:
                col_def += ", sortable: true"
            col_def += " }"
            columns_def.append(col_def)

        columns_str = ",\n        ".join(columns_def)

        search_code = ""
        if include_search:
            search_code = """
    @track searchTerm = '';

    handleSearch(event) {
        this.searchTerm = event.target.value.toLowerCase();
        this.filterData();
    }

    filterData() {
        if (!this.searchTerm) {
            this.filteredData = this.data;
            return;
        }

        this.filteredData = this.data.filter(record => {
            return Object.values(record).some(value =>
                String(value).toLowerCase().includes(this.searchTerm)
            );
        });
    }
"""

        pagination_code = ""
        if include_pagination:
            pagination_code = """
    @track currentPage = 1;
    @track pageSize = 10;

    get totalPages() {
        return Math.ceil(this.filteredData.length / this.pageSize);
    }

    get paginatedData() {
        const start = (this.currentPage - 1) * this.pageSize;
        const end = start + this.pageSize;
        return this.filteredData.slice(start, end);
    }

    get disablePrevious() {
        return this.currentPage <= 1;
    }

    get disableNext() {
        return this.currentPage >= this.totalPages;
    }

    handlePrevious() {
        if (this.currentPage > 1) {
            this.currentPage--;
        }
    }

    handleNext() {
        if (this.currentPage < this.totalPages) {
            this.currentPage++;
        }
    }
"""

        return f"""import {{ LightningElement, api, track, wire }} from 'lwc';
import {{ ShowToastEvent }} from 'lightning/platformShowToastEvent';
import {{ refreshApex }} from '@salesforce/apex';
// import getRecords from '@salesforce/apex/{sobject}Controller.getRecords';

/**
 * {class_name}
 * Lista de {sobject}
 */
export default class {class_name} extends LightningElement {{
    @api recordId;

    @track data = [];
    @track filteredData = [];
    @track isLoading = true;
    @track error;
{search_code}
{pagination_code}
    columns = [
        {columns_str}
    ];

    // Wire para buscar dados
    // @wire(getRecords)
    // wiredRecords({{ error, data }}) {{
    //     this.isLoading = false;
    //     if (data) {{
    //         this.data = data;
    //         this.filteredData = data;
    //     }} else if (error) {{
    //         this.error = error;
    //     }}
    // }}

    connectedCallback() {{
        // Carregar dados iniciais
        this.loadData();
    }}

    async loadData() {{
        this.isLoading = true;
        try {{
            // Simular dados - substituir por chamada Apex
            this.data = [
                {{ Id: '001xx000003DGb1', Name: 'Exemplo 1', CreatedDate: new Date().toISOString() }},
                {{ Id: '001xx000003DGb2', Name: 'Exemplo 2', CreatedDate: new Date().toISOString() }}
            ];
            this.filteredData = this.data;
        }} catch (error) {{
            this.error = error;
            this.dispatchEvent(new ShowToastEvent({{
                title: 'Erro',
                message: error.body?.message || 'Erro ao carregar dados',
                variant: 'error'
            }}));
        }} finally {{
            this.isLoading = false;
        }}
    }}

    handleRowAction(event) {{
        const action = event.detail.action;
        const row = event.detail.row;

        this.dispatchEvent(new CustomEvent('rowaction', {{
            detail: {{ action: action.name, record: row }}
        }}));
    }}

    handleRefresh() {{
        this.loadData();
    }}

    handleRowSelection(event) {{
        const selectedRows = event.detail.selectedRows;
        this.dispatchEvent(new CustomEvent('selection', {{
            detail: {{ selectedRows }}
        }}));
    }}
}}
"""

    def _generate_modal_js(self, name: str, label: str) -> str:
        """Gera JavaScript de modal"""
        class_name = self._to_pascal_case(name)

        return f"""import {{ LightningElement, api, track }} from 'lwc';

/**
 * {class_name}
 * Componente Modal
 */
export default class {class_name} extends LightningElement {{
    @api title = '{label}';
    @api size = 'medium'; // small, medium, large

    @track isOpen = false;

    @api
    open() {{
        this.isOpen = true;
    }}

    @api
    close() {{
        this.isOpen = false;
        this.dispatchEvent(new CustomEvent('close'));
    }}

    get modalClass() {{
        return `slds-modal slds-fade-in-open slds-modal_${{this.size}}`;
    }}

    handleClose() {{
        this.close();
    }}

    handleConfirm() {{
        this.dispatchEvent(new CustomEvent('confirm'));
        this.close();
    }}

    // Fechar com tecla Escape
    connectedCallback() {{
        this._handleKeyDown = this.handleKeyDown.bind(this);
        document.addEventListener('keydown', this._handleKeyDown);
    }}

    disconnectedCallback() {{
        document.removeEventListener('keydown', this._handleKeyDown);
    }}

    handleKeyDown(event) {{
        if (event.key === 'Escape' && this.isOpen) {{
            this.close();
        }}
    }}
}}
"""

    def _generate_apex_wrapper_js(
        self,
        name: str,
        apex_class: str,
        apex_method: str
    ) -> str:
        """Gera JavaScript wrapper para Apex"""
        class_name = self._to_pascal_case(name)

        return f"""import {{ LightningElement, api, track, wire }} from 'lwc';
import {{ ShowToastEvent }} from 'lightning/platformShowToastEvent';
import {apex_method} from '@salesforce/apex/{apex_class}.{apex_method}';

/**
 * {class_name}
 * Wrapper para {apex_class}.{apex_method}
 */
export default class {class_name} extends LightningElement {{
    @api recordId;

    @track data;
    @track error;
    @track isLoading = false;

    // Usar @wire para chamada reativa
    // @wire({apex_method}, {{ recordId: '$recordId' }})
    // wiredData({{ error, data }}) {{
    //     if (data) {{
    //         this.data = data;
    //         this.error = undefined;
    //     }} else if (error) {{
    //         this.error = error;
    //         this.data = undefined;
    //     }}
    // }}

    connectedCallback() {{
        this.loadData();
    }}

    async loadData() {{
        this.isLoading = true;

        try {{
            this.data = await {apex_method}({{ recordId: this.recordId }});
            this.error = undefined;
        }} catch (error) {{
            this.error = error;
            this.data = undefined;

            this.dispatchEvent(new ShowToastEvent({{
                title: 'Erro',
                message: error.body?.message || 'Erro ao carregar dados',
                variant: 'error'
            }}));
        }} finally {{
            this.isLoading = false;
        }}
    }}

    handleRefresh() {{
        this.loadData();
    }}
}}
"""

    # ==================== HTML GENERATORS ====================

    def _generate_basic_html(self, name: str) -> str:
        """Gera HTML basico"""
        return f"""<template>
    <lightning-card title="{self._to_title(name)}" icon-name="standard:default">
        <div class="slds-p-around_medium">
            <p>Componente {name}</p>

            <lightning-button
                label="Acao"
                onclick={{handleClick}}
                variant="brand"
                class="slds-m-top_medium">
            </lightning-button>
        </div>
    </lightning-card>
</template>
"""

    def _generate_form_html(
        self,
        name: str,
        sobject: str,
        fields: List[str]
    ) -> str:
        """Gera HTML de formulario"""
        fields_html = "\n                    ".join([
            f'<lightning-input-field field-name="{f}"></lightning-input-field>'
            for f in fields
        ])

        return f"""<template>
    <lightning-card title="{self._to_title(name)}" icon-name="standard:record">
        <div class="slds-p-around_medium">
            <template if:true={{isLoading}}>
                <lightning-spinner alternative-text="Carregando..." size="medium"></lightning-spinner>
            </template>

            <template if:true={{error}}>
                <div class="slds-text-color_error slds-m-bottom_medium">
                    {{error}}
                </div>
            </template>

            <lightning-record-edit-form
                object-api-name={{objectApiName}}
                record-id={{recordId}}
                onsubmit={{handleSubmit}}
                onsuccess={{handleSuccess}}
                onerror={{handleError}}>

                <lightning-messages></lightning-messages>

                <div class="slds-grid slds-wrap">
                    {fields_html}
                </div>

                <div class="slds-m-top_medium slds-clearfix">
                    <lightning-button
                        label="Cancelar"
                        onclick={{handleCancel}}
                        class="slds-float_left">
                    </lightning-button>
                    <lightning-button
                        type="submit"
                        label="Salvar"
                        variant="brand"
                        class="slds-float_right">
                    </lightning-button>
                </div>
            </lightning-record-edit-form>
        </div>
    </lightning-card>
</template>
"""

    def _generate_list_html(
        self,
        name: str,
        columns: List[LWCColumn],
        include_search: bool,
        include_pagination: bool
    ) -> str:
        """Gera HTML de lista"""
        search_html = ""
        if include_search:
            search_html = """
            <div slot="actions">
                <lightning-input
                    type="search"
                    label="Buscar"
                    variant="label-hidden"
                    placeholder="Buscar..."
                    onchange={handleSearch}
                    class="slds-m-right_small">
                </lightning-input>
                <lightning-button-icon
                    icon-name="utility:refresh"
                    onclick={handleRefresh}
                    alternative-text="Atualizar">
                </lightning-button-icon>
            </div>"""

        pagination_html = ""
        if include_pagination:
            pagination_html = """
            <div class="slds-m-top_medium slds-grid slds-grid_align-center">
                <lightning-button
                    label="Anterior"
                    onclick={handlePrevious}
                    disabled={disablePrevious}>
                </lightning-button>
                <span class="slds-m-horizontal_medium">
                    Pagina {currentPage} de {totalPages}
                </span>
                <lightning-button
                    label="Proximo"
                    onclick={handleNext}
                    disabled={disableNext}>
                </lightning-button>
            </div>"""

        return f"""<template>
    <lightning-card title="{self._to_title(name)}" icon-name="standard:list">
        {search_html}

        <div class="slds-p-around_medium">
            <template if:true={{isLoading}}>
                <lightning-spinner alternative-text="Carregando..." size="medium"></lightning-spinner>
            </template>

            <template if:false={{isLoading}}>
                <template if:true={{error}}>
                    <div class="slds-text-color_error">
                        {{error.body.message}}
                    </div>
                </template>

                <template if:false={{error}}>
                    <lightning-datatable
                        key-field="Id"
                        data={{paginatedData}}
                        columns={{columns}}
                        onrowaction={{handleRowAction}}
                        onrowselection={{handleRowSelection}}
                        show-row-number-column
                        hide-checkbox-column>
                    </lightning-datatable>
                    {pagination_html}
                </template>
            </template>
        </div>
    </lightning-card>
</template>
"""

    def _generate_modal_html(self, name: str, label: str) -> str:
        """Gera HTML de modal"""
        return f"""<template>
    <template if:true={{isOpen}}>
        <section role="dialog" tabindex="-1" class={{modalClass}} aria-modal="true">
            <div class="slds-modal__container">
                <!-- Header -->
                <header class="slds-modal__header">
                    <lightning-button-icon
                        icon-name="utility:close"
                        onclick={{handleClose}}
                        alternative-text="Fechar"
                        variant="bare-inverse"
                        class="slds-modal__close">
                    </lightning-button-icon>
                    <h2 class="slds-modal__title slds-hyphenate">{{title}}</h2>
                </header>

                <!-- Body -->
                <div class="slds-modal__content slds-p-around_medium">
                    <slot></slot>
                </div>

                <!-- Footer -->
                <footer class="slds-modal__footer">
                    <lightning-button
                        label="Cancelar"
                        onclick={{handleClose}}>
                    </lightning-button>
                    <lightning-button
                        label="Confirmar"
                        variant="brand"
                        onclick={{handleConfirm}}
                        class="slds-m-left_x-small">
                    </lightning-button>
                </footer>
            </div>
        </section>
        <div class="slds-backdrop slds-backdrop_open"></div>
    </template>
</template>
"""

    def _generate_apex_wrapper_html(self, name: str) -> str:
        """Gera HTML wrapper para Apex"""
        return f"""<template>
    <lightning-card title="{self._to_title(name)}" icon-name="standard:apex">
        <div slot="actions">
            <lightning-button-icon
                icon-name="utility:refresh"
                onclick={{handleRefresh}}
                alternative-text="Atualizar">
            </lightning-button-icon>
        </div>

        <div class="slds-p-around_medium">
            <template if:true={{isLoading}}>
                <lightning-spinner alternative-text="Carregando..." size="medium"></lightning-spinner>
            </template>

            <template if:true={{error}}>
                <div class="slds-text-color_error">
                    {{error.body.message}}
                </div>
            </template>

            <template if:true={{data}}>
                <pre class="slds-text-body_small">
                    {{dataJson}}
                </pre>
            </template>
        </div>
    </lightning-card>
</template>
"""

    # ==================== CSS GENERATORS ====================

    def _generate_basic_css(self) -> str:
        """Gera CSS basico"""
        return """:host {
    display: block;
}

.container {
    padding: 1rem;
}
"""

    def _generate_form_css(self) -> str:
        """Gera CSS de formulario"""
        return """:host {
    display: block;
}

.slds-form-element {
    margin-bottom: 1rem;
}

lightning-input-field {
    padding: 0.5rem;
}
"""

    def _generate_list_css(self) -> str:
        """Gera CSS de lista"""
        return """:host {
    display: block;
}

.datatable-container {
    min-height: 200px;
}

lightning-datatable {
    height: 100%;
}
"""

    def _generate_modal_css(self) -> str:
        """Gera CSS de modal"""
        return """:host {
    display: block;
}

.slds-modal__content {
    max-height: 60vh;
    overflow-y: auto;
}
"""

    # ==================== META XML GENERATOR ====================

    def _generate_meta_xml(
        self,
        name: str,
        label: str,
        description: str,
        targets: List[str],
        is_exposed: bool
    ) -> str:
        """Gera arquivo meta XML"""
        targets_xml = ""
        if targets:
            targets_list = "\n            ".join([f"<target>{t}</target>" for t in targets])
            targets_xml = f"""
        <targets>
            {targets_list}
        </targets>"""

        return f"""<?xml version="1.0" encoding="UTF-8"?>
<LightningComponentBundle xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>{self.api_version}</apiVersion>
    <isExposed>{str(is_exposed).lower()}</isExposed>
    <masterLabel>{label}</masterLabel>
    <description>{description}</description>{targets_xml}
</LightningComponentBundle>
"""

    # ==================== HELPERS ====================

    def _to_pascal_case(self, name: str) -> str:
        """Converte para PascalCase"""
        return name[0].upper() + name[1:]

    def _to_title(self, name: str) -> str:
        """Converte camelCase para titulo"""
        import re
        words = re.findall(r'[A-Z]?[a-z]+|[A-Z]+(?=[A-Z]|$)', name)
        return ' '.join(word.capitalize() for word in words)
