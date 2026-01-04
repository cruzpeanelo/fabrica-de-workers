#!/usr/bin/env python3
"""
Gerador de icones PWA - Plataforma E
===========================================
Gera icones PNG a partir do SVG base.

Requisitos:
    pip install pillow cairosvg

Uso:
    python generate-icons.py
"""

import os
from pathlib import Path

# Tentar importar bibliotecas
try:
    from PIL import Image
    import cairosvg
    HAS_DEPS = True
except ImportError:
    HAS_DEPS = False
    print("Para gerar icones PNG, instale: pip install pillow cairosvg")

# Tamanhos de icones PWA
SIZES = [72, 96, 128, 144, 152, 192, 384, 512]

# Diretorio atual
ICONS_DIR = Path(__file__).parent
SVG_FILE = ICONS_DIR / "icon.svg"


def generate_icons():
    """Gerar icones PNG em todos os tamanhos"""
    if not HAS_DEPS:
        print("Dependencias nao instaladas. Usando placeholder.")
        return generate_placeholder_icons()

    if not SVG_FILE.exists():
        print(f"Arquivo SVG nao encontrado: {SVG_FILE}")
        return False

    print(f"Gerando icones a partir de: {SVG_FILE}")

    for size in SIZES:
        output_file = ICONS_DIR / f"icon-{size}x{size}.png"

        # Converter SVG para PNG
        cairosvg.svg2png(
            url=str(SVG_FILE),
            write_to=str(output_file),
            output_width=size,
            output_height=size
        )

        print(f"  Gerado: {output_file.name}")

    print("Icones gerados com sucesso!")
    return True


def generate_placeholder_icons():
    """Gerar icones placeholder simples"""
    try:
        from PIL import Image, ImageDraw, ImageFont
    except ImportError:
        print("Pillow nao instalado. Nao e possivel gerar placeholders.")
        return False

    for size in SIZES:
        output_file = ICONS_DIR / f"icon-{size}x{size}.png"

        # Criar imagem
        img = Image.new('RGB', (size, size), '#003B4A')
        draw = ImageDraw.Draw(img)

        # Adicionar texto "FA"
        text = "FA"
        font_size = size // 3

        try:
            font = ImageFont.truetype("arial.ttf", font_size)
        except:
            font = ImageFont.load_default()

        # Centralizar texto
        bbox = draw.textbbox((0, 0), text, font=font)
        text_width = bbox[2] - bbox[0]
        text_height = bbox[3] - bbox[1]
        x = (size - text_width) // 2
        y = (size - text_height) // 2

        draw.text((x, y), text, fill='white', font=font)

        # Salvar
        img.save(output_file, 'PNG')
        print(f"  Placeholder: {output_file.name}")

    return True


if __name__ == "__main__":
    generate_icons()
