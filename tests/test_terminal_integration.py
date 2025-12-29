#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Test script for terminal integration
Run this to verify the terminal integration is working correctly
"""
import sys
import requests
import time

# Fix UTF-8 output on Windows
if sys.platform == 'win32':
    sys.stdout.reconfigure(encoding='utf-8')

def test_dashboard_running():
    """Test if dashboard is running"""
    print("1. Testing dashboard availability...")
    try:
        response = requests.get("http://localhost:9001/api/status", timeout=5)
        if response.status_code == 200:
            data = response.json()
            print(f"   ✓ Dashboard is running (version {data.get('version')})")
            return True
        else:
            print(f"   ✗ Dashboard returned status code {response.status_code}")
            return False
    except requests.exceptions.ConnectionError:
        print("   ✗ Dashboard is not running")
        print("   → Start it with: python factory/dashboard/app_v6_agile.py")
        return False
    except Exception as e:
        print(f"   ✗ Error: {e}")
        return False

def test_terminal_endpoints():
    """Test if terminal endpoints exist"""
    print("\n2. Testing terminal API endpoints...")

    # We need a project ID - let's get the first one
    try:
        response = requests.get("http://localhost:9001/api/projects", timeout=5)
        projects = response.json()

        if not projects:
            print("   ! No projects found - create one first")
            return False

        project_id = projects[0]['project_id']
        print(f"   Using project: {project_id}")

        # Test execute endpoint (dry run)
        try:
            response = requests.post(
                f"http://localhost:9001/api/projects/{project_id}/terminal/execute",
                json={"command": "echo test"},
                timeout=5
            )
            if response.status_code == 200:
                print("   ✓ Execute endpoint works")
            else:
                print(f"   ✗ Execute endpoint failed: {response.status_code}")
                return False
        except Exception as e:
            print(f"   ✗ Execute endpoint error: {e}")
            return False

        # Test output endpoint
        try:
            response = requests.get(
                f"http://localhost:9001/api/projects/{project_id}/terminal/output",
                timeout=5
            )
            if response.status_code == 200:
                print("   ✓ Output endpoint works")
            else:
                print(f"   ✗ Output endpoint failed: {response.status_code}")
                return False
        except Exception as e:
            print(f"   ✗ Output endpoint error: {e}")
            return False

        return True
    except Exception as e:
        print(f"   ✗ Error: {e}")
        return False

def test_ui_components():
    """Test if UI components are in the HTML"""
    print("\n3. Testing UI components...")
    try:
        response = requests.get("http://localhost:9001/", timeout=5)
        html = response.text

        checks = [
            ("xterm.js CSS", "xterm@5.3.0/css/xterm.min.css"),
            ("xterm.js JS", "xterm@5.3.0/lib/xterm.min.js"),
            ("Terminal section", "Terminal and Preview Section"),
            ("Terminal container", "terminal-container"),
            ("Start button", "Iniciar App"),
            ("Test button", "Testes"),
            ("Stop button", "Parar"),
            ("Preview iframe", "previewFrame"),
        ]

        all_ok = True
        for name, text in checks:
            if text in html:
                print(f"   ✓ {name} found")
            else:
                print(f"   ✗ {name} missing")
                all_ok = False

        return all_ok
    except Exception as e:
        print(f"   ✗ Error: {e}")
        return False

def test_vue_integration():
    """Test if Vue.js integration is correct"""
    print("\n4. Testing Vue.js integration...")
    try:
        response = requests.get("http://localhost:9001/", timeout=5)
        html = response.text

        checks = [
            ("terminalCommand state", "const terminalCommand = ref"),
            ("terminalRunning state", "const terminalRunning = ref"),
            ("initTerminal method", "const initTerminal = ()"),
            ("executeTerminalCommand method", "const executeTerminalCommand = async ()"),
            ("startApp method", "const startApp = async ()"),
            ("Return statement", "terminalCommand, terminalRunning, previewUrl"),
        ]

        all_ok = True
        for name, text in checks:
            if text in html:
                print(f"   ✓ {name} found")
            else:
                print(f"   ✗ {name} missing")
                all_ok = False

        return all_ok
    except Exception as e:
        print(f"   ✗ Error: {e}")
        return False

def main():
    """Run all tests"""
    print("=" * 60)
    print("  Terminal Integration Test Suite")
    print("=" * 60)

    results = []

    # Test 1: Dashboard running
    results.append(("Dashboard", test_dashboard_running()))

    if not results[0][1]:
        print("\n⚠️  Dashboard is not running. Start it first:")
        print("   python factory/dashboard/app_v6_agile.py")
        return

    # Test 2: Terminal endpoints
    results.append(("Terminal API", test_terminal_endpoints()))

    # Test 3: UI components
    results.append(("UI Components", test_ui_components()))

    # Test 4: Vue integration
    results.append(("Vue.js Integration", test_vue_integration()))

    # Summary
    print("\n" + "=" * 60)
    print("  Test Summary")
    print("=" * 60)

    passed = sum(1 for _, result in results if result)
    total = len(results)

    for name, result in results:
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"  {status} - {name}")

    print("=" * 60)
    print(f"  {passed}/{total} tests passed")

    if passed == total:
        print("\n🎉 All tests passed! Terminal integration is ready.")
        print("\nNext steps:")
        print("  1. Open http://localhost:9001 in your browser")
        print("  2. Select a project")
        print("  3. Scroll down to see 'Ambiente de Teste'")
        print("  4. Try the quick action buttons")
    else:
        print("\n⚠️  Some tests failed. Check the output above for details.")

    print("=" * 60)

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nTests interrupted by user")
    except Exception as e:
        print(f"\n\n❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
