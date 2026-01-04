"""
Language Templates - Plataforma E
Templates para geracao de codigo em multiplas linguagens
"""

# =============================================================================
# NODE.JS / EXPRESS + TYPESCRIPT TEMPLATES
# =============================================================================

NODEJS_EXPRESS_ROUTER = '''/**
 * Router para {name}
 * Gerado automaticamente pela Plataforma E
 */

import {{ Router, Request, Response }} from 'express';
import {{ {model_class} }} from '../models/{model_name}';
import {{ validate{model_class} }} from '../validators/{model_name}';

const router = Router();

// GET - Lista todos
router.get('/', async (req: Request, res: Response) => {{
  try {{
    const items = await {model_class}.findAll({{
      limit: parseInt(req.query.limit as string) || 100,
      offset: parseInt(req.query.offset as string) || 0
    }});
    res.json(items);
  }} catch (error) {{
    res.status(500).json({{ error: 'Erro ao listar {name}' }});
  }}
}});

// GET - Busca por ID
router.get('/:id', async (req: Request, res: Response) => {{
  try {{
    const item = await {model_class}.findByPk(req.params.id);
    if (!item) {{
      return res.status(404).json({{ error: '{name} nao encontrado' }});
    }}
    res.json(item);
  }} catch (error) {{
    res.status(500).json({{ error: 'Erro ao buscar {name}' }});
  }}
}});

// POST - Cria novo
router.post('/', validate{model_class}, async (req: Request, res: Response) => {{
  try {{
    const item = await {model_class}.create(req.body);
    res.status(201).json(item);
  }} catch (error) {{
    res.status(400).json({{ error: 'Erro ao criar {name}' }});
  }}
}});

// PUT - Atualiza
router.put('/:id', validate{model_class}, async (req: Request, res: Response) => {{
  try {{
    const item = await {model_class}.findByPk(req.params.id);
    if (!item) {{
      return res.status(404).json({{ error: '{name} nao encontrado' }});
    }}
    await item.update(req.body);
    res.json(item);
  }} catch (error) {{
    res.status(400).json({{ error: 'Erro ao atualizar {name}' }});
  }}
}});

// DELETE - Remove
router.delete('/:id', async (req: Request, res: Response) => {{
  try {{
    const item = await {model_class}.findByPk(req.params.id);
    if (!item) {{
      return res.status(404).json({{ error: '{name} nao encontrado' }});
    }}
    await item.destroy();
    res.json({{ message: '{name} removido com sucesso' }});
  }} catch (error) {{
    res.status(500).json({{ error: 'Erro ao remover {name}' }});
  }}
}});

export default router;
'''

NODEJS_SEQUELIZE_MODEL = '''/**
 * Model {name}
 * Gerado automaticamente pela Plataforma E
 */

import {{ Model, DataTypes, Optional }} from 'sequelize';
import {{ sequelize }} from '../database';

interface {model_class}Attributes {{
  id: number;
{interface_fields}
  createdAt?: Date;
  updatedAt?: Date;
}}

interface {model_class}CreationAttributes extends Optional<{model_class}Attributes, 'id'> {{}}

class {model_class} extends Model<{model_class}Attributes, {model_class}CreationAttributes> implements {model_class}Attributes {{
  public id!: number;
{class_fields}
  public readonly createdAt!: Date;
  public readonly updatedAt!: Date;
}}

{model_class}.init(
  {{
    id: {{
      type: DataTypes.INTEGER,
      autoIncrement: true,
      primaryKey: true,
    }},
{model_fields}
  }},
  {{
    sequelize,
    tableName: '{table_name}',
    timestamps: true,
  }}
);

export {{ {model_class} }};
'''


# =============================================================================
# GO / GIN TEMPLATES
# =============================================================================

GO_GIN_HANDLER = '''// Handler para {name}
// Gerado automaticamente pela Plataforma E

package handlers

import (
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"
	"gorm.io/gorm"

	"app/models"
)

type {handler_name}Handler struct {{
	DB *gorm.DB
}}

func New{handler_name}Handler(db *gorm.DB) *{handler_name}Handler {{
	return &{handler_name}Handler{{DB: db}}
}}

// List{handler_name}s godoc
// @Summary Lista todos os {name}
// @Tags {tag}
// @Accept json
// @Produce json
// @Success 200 {{array}} models.{model_class}
// @Router /{route_prefix} [get]
func (h *{handler_name}Handler) List(c *gin.Context) {{
	var items []models.{model_class}

	limit, _ := strconv.Atoi(c.DefaultQuery("limit", "100"))
	offset, _ := strconv.Atoi(c.DefaultQuery("offset", "0"))

	if err := h.DB.Limit(limit).Offset(offset).Find(&items).Error; err != nil {{
		c.JSON(http.StatusInternalServerError, gin.H{{"error": err.Error()}})
		return
	}}

	c.JSON(http.StatusOK, items)
}}

// Get{handler_name} godoc
// @Summary Busca {name} por ID
// @Tags {tag}
// @Accept json
// @Produce json
// @Param id path int true "{name} ID"
// @Success 200 {{object}} models.{model_class}
// @Router /{route_prefix}/{{id}} [get]
func (h *{handler_name}Handler) Get(c *gin.Context) {{
	id := c.Param("id")
	var item models.{model_class}

	if err := h.DB.First(&item, id).Error; err != nil {{
		c.JSON(http.StatusNotFound, gin.H{{"error": "{name} nao encontrado"}})
		return
	}}

	c.JSON(http.StatusOK, item)
}}

// Create{handler_name} godoc
// @Summary Cria novo {name}
// @Tags {tag}
// @Accept json
// @Produce json
// @Param item body models.{model_class} true "{name} data"
// @Success 201 {{object}} models.{model_class}
// @Router /{route_prefix} [post]
func (h *{handler_name}Handler) Create(c *gin.Context) {{
	var item models.{model_class}

	if err := c.ShouldBindJSON(&item); err != nil {{
		c.JSON(http.StatusBadRequest, gin.H{{"error": err.Error()}})
		return
	}}

	if err := h.DB.Create(&item).Error; err != nil {{
		c.JSON(http.StatusInternalServerError, gin.H{{"error": err.Error()}})
		return
	}}

	c.JSON(http.StatusCreated, item)
}}

// Update{handler_name} godoc
// @Summary Atualiza {name}
// @Tags {tag}
// @Accept json
// @Produce json
// @Param id path int true "{name} ID"
// @Param item body models.{model_class} true "{name} data"
// @Success 200 {{object}} models.{model_class}
// @Router /{route_prefix}/{{id}} [put]
func (h *{handler_name}Handler) Update(c *gin.Context) {{
	id := c.Param("id")
	var item models.{model_class}

	if err := h.DB.First(&item, id).Error; err != nil {{
		c.JSON(http.StatusNotFound, gin.H{{"error": "{name} nao encontrado"}})
		return
	}}

	if err := c.ShouldBindJSON(&item); err != nil {{
		c.JSON(http.StatusBadRequest, gin.H{{"error": err.Error()}})
		return
	}}

	h.DB.Save(&item)
	c.JSON(http.StatusOK, item)
}}

// Delete{handler_name} godoc
// @Summary Remove {name}
// @Tags {tag}
// @Accept json
// @Produce json
// @Param id path int true "{name} ID"
// @Success 200 {{object}} map[string]string
// @Router /{route_prefix}/{{id}} [delete]
func (h *{handler_name}Handler) Delete(c *gin.Context) {{
	id := c.Param("id")
	var item models.{model_class}

	if err := h.DB.First(&item, id).Error; err != nil {{
		c.JSON(http.StatusNotFound, gin.H{{"error": "{name} nao encontrado"}})
		return
	}}

	h.DB.Delete(&item)
	c.JSON(http.StatusOK, gin.H{{"message": "{name} removido com sucesso"}})
}}
'''

GO_GORM_MODEL = '''// Model {name}
// Gerado automaticamente pela Plataforma E

package models

import (
	"time"

	"gorm.io/gorm"
)

type {model_class} struct {{
	ID        uint           `gorm:"primaryKey" json:"id"`
{fields}
	CreatedAt time.Time      `json:"created_at"`
	UpdatedAt time.Time      `json:"updated_at"`
	DeletedAt gorm.DeletedAt `gorm:"index" json:"-"`
}}

func (m *{model_class}) TableName() string {{
	return "{table_name}"
}}
'''


# =============================================================================
# REACT TYPESCRIPT TEMPLATES
# =============================================================================

REACT_COMPONENT = '''/**
 * Componente {component_name}
 * Gerado automaticamente pela Plataforma E
 */

import React, {{ useState, useEffect }} from 'react';
import axios from 'axios';

interface {model_class} {{
  id: number;
{interface_fields}
}}

interface {component_name}Props {{
  apiUrl?: string;
}}

const {component_name}: React.FC<{component_name}Props> = ({{ apiUrl = '/api/{route_prefix}' }}) => {{
  const [items, setItems] = useState<{model_class}[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [showForm, setShowForm] = useState(false);
  const [editingId, setEditingId] = useState<number | null>(null);
  const [formData, setFormData] = useState<Partial<{model_class}>({{}});

  useEffect(() => {{
    fetchItems();
  }}, []);

  const fetchItems = async () => {{
    try {{
      setLoading(true);
      const response = await axios.get(apiUrl);
      setItems(response.data);
      setError(null);
    }} catch (err) {{
      setError('Erro ao carregar dados');
      console.error(err);
    }} finally {{
      setLoading(false);
    }}
  }};

  const handleSubmit = async (e: React.FormEvent) => {{
    e.preventDefault();
    try {{
      if (editingId) {{
        await axios.put(`${{apiUrl}}/${{editingId}}`, formData);
      }} else {{
        await axios.post(apiUrl, formData);
      }}
      fetchItems();
      setShowForm(false);
      setFormData({{}});
      setEditingId(null);
    }} catch (err) {{
      setError('Erro ao salvar');
    }}
  }};

  const handleDelete = async (id: number) => {{
    if (window.confirm('Confirma exclusao?')) {{
      try {{
        await axios.delete(`${{apiUrl}}/${{id}}`);
        fetchItems();
      }} catch (err) {{
        setError('Erro ao excluir');
      }}
    }}
  }};

  const handleEdit = (item: {model_class}) => {{
    setFormData(item);
    setEditingId(item.id);
    setShowForm(true);
  }};

  if (loading) return <div className="loading">Carregando...</div>;
  if (error) return <div className="error">{{error}}</div>;

  return (
    <div className="{component_name_lower}">
      <h2>{name}</h2>

      <button onClick={{() => setShowForm(true)}} className="btn-add">
        + Adicionar
      </button>

      <div className="items-list">
        {{items.map(item => (
          <div key={{item.id}} className="item-card">
            <div className="item-content">
              {{/* Renderizar campos aqui */}}
              <pre>{{JSON.stringify(item, null, 2)}}</pre>
            </div>
            <div className="item-actions">
              <button onClick={{() => handleEdit(item)}}>Editar</button>
              <button onClick={{() => handleDelete(item.id)}} className="danger">Excluir</button>
            </div>
          </div>
        ))}}
      </div>

      {{showForm && (
        <div className="modal-overlay">
          <div className="modal">
            <h3>{{editingId ? 'Editar' : 'Novo'}} {name}</h3>
            <form onSubmit={{handleSubmit}}>
              {{/* Campos do formulario aqui */}}
              <div className="form-actions">
                <button type="submit">Salvar</button>
                <button type="button" onClick={{() => setShowForm(false)}}>Cancelar</button>
              </div>
            </form>
          </div>
        </div>
      )}}
    </div>
  );
}};

export default {component_name};
'''

REACT_HOOK = '''/**
 * Hook use{hook_name}
 * Gerado automaticamente pela Plataforma E
 */

import {{ useState, useEffect, useCallback }} from 'react';
import axios from 'axios';

interface {model_class} {{
  id: number;
{interface_fields}
}}

interface Use{hook_name}Result {{
  items: {model_class}[];
  loading: boolean;
  error: string | null;
  fetchItems: () => Promise<void>;
  createItem: (data: Omit<{model_class}, 'id'>) => Promise<{model_class}>;
  updateItem: (id: number, data: Partial<{model_class}>) => Promise<{model_class}>;
  deleteItem: (id: number) => Promise<void>;
}}

export const use{hook_name} = (apiUrl: string = '/api/{route_prefix}'): Use{hook_name}Result => {{
  const [items, setItems] = useState<{model_class}[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchItems = useCallback(async () => {{
    try {{
      setLoading(true);
      const response = await axios.get(apiUrl);
      setItems(response.data);
      setError(null);
    }} catch (err) {{
      setError('Erro ao carregar dados');
    }} finally {{
      setLoading(false);
    }}
  }}, [apiUrl]);

  const createItem = useCallback(async (data: Omit<{model_class}, 'id'>) => {{
    const response = await axios.post(apiUrl, data);
    await fetchItems();
    return response.data;
  }}, [apiUrl, fetchItems]);

  const updateItem = useCallback(async (id: number, data: Partial<{model_class}>) => {{
    const response = await axios.put(`${{apiUrl}}/${{id}}`, data);
    await fetchItems();
    return response.data;
  }}, [apiUrl, fetchItems]);

  const deleteItem = useCallback(async (id: number) => {{
    await axios.delete(`${{apiUrl}}/${{id}}`);
    await fetchItems();
  }}, [apiUrl, fetchItems]);

  useEffect(() => {{
    fetchItems();
  }}, [fetchItems]);

  return {{ items, loading, error, fetchItems, createItem, updateItem, deleteItem }};
}};
'''


# =============================================================================
# JAVA / SPRING BOOT TEMPLATES
# =============================================================================

JAVA_SPRING_CONTROLLER = '''/**
 * Controller para {name}
 * Gerado automaticamente pela Plataforma E
 */

package com.example.app.controllers;

import com.example.app.models.{model_class};
import com.example.app.repositories.{model_class}Repository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/{route_prefix}")
@CrossOrigin(origins = "*")
public class {model_class}Controller {{

    @Autowired
    private {model_class}Repository repository;

    @GetMapping
    public List<{model_class}> list() {{
        return repository.findAll();
    }}

    @GetMapping("/{{id}}")
    public ResponseEntity<{model_class}> getById(@PathVariable Long id) {{
        Optional<{model_class}> item = repository.findById(id);
        return item.map(ResponseEntity::ok)
                   .orElse(ResponseEntity.notFound().build());
    }}

    @PostMapping
    public ResponseEntity<{model_class}> create(@RequestBody {model_class} item) {{
        {model_class} saved = repository.save(item);
        return ResponseEntity.status(HttpStatus.CREATED).body(saved);
    }}

    @PutMapping("/{{id}}")
    public ResponseEntity<{model_class}> update(@PathVariable Long id, @RequestBody {model_class} item) {{
        if (!repository.existsById(id)) {{
            return ResponseEntity.notFound().build();
        }}
        item.setId(id);
        return ResponseEntity.ok(repository.save(item));
    }}

    @DeleteMapping("/{{id}}")
    public ResponseEntity<Void> delete(@PathVariable Long id) {{
        if (!repository.existsById(id)) {{
            return ResponseEntity.notFound().build();
        }}
        repository.deleteById(id);
        return ResponseEntity.noContent().build();
    }}
}}
'''

JAVA_JPA_ENTITY = '''/**
 * Entity {name}
 * Gerado automaticamente pela Plataforma E
 */

package com.example.app.models;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "{table_name}")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class {model_class} {{

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

{fields}

    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
}}
'''

JAVA_REPOSITORY = '''/**
 * Repository para {name}
 * Gerado automaticamente pela Plataforma E
 */

package com.example.app.repositories;

import com.example.app.models.{model_class};
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface {model_class}Repository extends JpaRepository<{model_class}, Long> {{
    // Metodos customizados podem ser adicionados aqui
}}
'''


# =============================================================================
# MAPPING DE LINGUAGENS DISPONIVEIS
# =============================================================================

AVAILABLE_LANGUAGES = {{
    "python": {{
        "name": "Python",
        "frameworks": ["FastAPI", "Flask", "Django"],
        "templates": ["fastapi_router", "sqlalchemy_model", "pydantic_schema", "pytest_test"]
    }},
    "nodejs": {{
        "name": "Node.js / TypeScript",
        "frameworks": ["Express", "NestJS", "Fastify"],
        "templates": ["express_router", "sequelize_model", "jest_test"]
    }},
    "go": {{
        "name": "Go",
        "frameworks": ["Gin", "Echo", "Fiber"],
        "templates": ["gin_handler", "gorm_model"]
    }},
    "java": {{
        "name": "Java",
        "frameworks": ["Spring Boot", "Quarkus", "Micronaut"],
        "templates": ["spring_controller", "jpa_entity", "jpa_repository"]
    }},
    "frontend": {{
        "name": "Frontend",
        "frameworks": ["Vue.js", "React", "Angular"],
        "templates": ["vue_component", "react_component", "react_hook"]
    }}
}}


def get_template(language: str, template_type: str) -> str:
    """Retorna template para linguagem e tipo especificados"""
    templates = {{
        "nodejs_router": NODEJS_EXPRESS_ROUTER,
        "nodejs_model": NODEJS_SEQUELIZE_MODEL,
        "go_handler": GO_GIN_HANDLER,
        "go_model": GO_GORM_MODEL,
        "react_component": REACT_COMPONENT,
        "react_hook": REACT_HOOK,
        "java_controller": JAVA_SPRING_CONTROLLER,
        "java_entity": JAVA_JPA_ENTITY,
        "java_repository": JAVA_REPOSITORY,
    }}

    key = f"{{language}}_{{template_type}}"
    return templates.get(key, "")


def list_available_languages() -> dict:
    """Lista linguagens e frameworks disponiveis"""
    return AVAILABLE_LANGUAGES
