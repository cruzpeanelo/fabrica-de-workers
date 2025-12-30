# -*- coding: utf-8 -*-
"""Script to add voice input to return statement in app_v6_agile.py"""

# Read file
with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Check if already added
if 'voiceRecording, toggleVoiceRecording' not in content:
    # Add to return statement
    old_text = '''                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh
            };
        }
    }).mount('#app');'''

    new_text = '''                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh,
                // Issue #155 - Voice Input
                voiceRecording, toggleVoiceRecording
            };
        }
    }).mount('#app');'''

    content = content.replace(old_text, new_text, 1)

    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)
    print('Return statement updated successfully')
else:
    print('Return statement already updated')
