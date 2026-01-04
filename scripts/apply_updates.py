#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Script para aplicar atualizacoes de titulos das issues.
"""

import json
import subprocess
import os
import time
import sys

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    updates_path = os.path.join(script_dir, 'updates.json')

    with open(updates_path, 'r', encoding='utf-8') as f:
        updates = json.load(f)

    print(f"Total de issues a atualizar: {len(updates)}")

    # Processar em lotes para evitar rate limiting
    success = 0
    errors = []

    for i, update in enumerate(updates):
        number = update['number']
        new_title = update['new_title']

        # Escapar titulo para shell
        escaped_title = new_title.replace('"', '\\"')

        # Executar gh issue edit
        cmd = f'gh issue edit {number} --title "{escaped_title}"'

        try:
            result = subprocess.run(
                cmd,
                shell=True,
                capture_output=True,
                text=True,
                cwd=os.path.dirname(script_dir)  # Pasta do projeto
            )

            if result.returncode == 0:
                success += 1
                print(f"[{i+1}/{len(updates)}] #{number}: OK")
            else:
                errors.append({
                    'number': number,
                    'error': result.stderr
                })
                print(f"[{i+1}/{len(updates)}] #{number}: ERRO - {result.stderr}")

        except Exception as e:
            errors.append({
                'number': number,
                'error': str(e)
            })
            print(f"[{i+1}/{len(updates)}] #{number}: EXCECAO - {e}")

        # Rate limiting - pequeno delay entre requests
        if (i + 1) % 10 == 0:
            time.sleep(1)

    print(f"\n=== RESUMO ===")
    print(f"Sucesso: {success}")
    print(f"Erros: {len(errors)}")

    if errors:
        print("\nIssues com erro:")
        for err in errors[:10]:
            print(f"  #{err['number']}: {err['error']}")

if __name__ == '__main__':
    main()
