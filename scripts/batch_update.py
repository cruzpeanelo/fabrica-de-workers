#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Gera scripts batch para atualizar issues.
"""

import json
import os

script_dir = os.path.dirname(os.path.abspath(__file__))

with open(os.path.join(script_dir, 'updates.json'), 'r', encoding='utf-8') as f:
    updates = json.load(f)

# Gerar comandos
commands = []
for update in updates:
    number = update['number']
    new_title = update['new_title'].replace('"', '\\"').replace('`', '\\`')
    commands.append(f'gh issue edit {number} --title "{new_title}"')

# Salvar como shell script
batch_path = os.path.join(script_dir, 'update_issues.sh')
with open(batch_path, 'w', encoding='utf-8') as f:
    f.write('#!/bin/bash\n')
    f.write('cd "/c/Users/lcruz/Plataforma E"\n')
    f.write('echo "Updating issues..."\n')
    for i, cmd in enumerate(commands):
        f.write(f'echo "[{i+1}/{len(commands)}]"\n')
        f.write(f'{cmd}\n')
        if (i + 1) % 20 == 0:
            f.write('sleep 1\n')
    f.write('echo "Done!"\n')

print(f"Script gerado: {batch_path}")
print(f"Total de comandos: {len(commands)}")
