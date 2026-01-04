# -*- coding: utf-8 -*-
"""
QA Test Runner - Comprehensive Testing Suite
============================================
Tests all screens, UX/UI, multi-tenancy, and data segregation.
"""

import asyncio
import json
import os
from datetime import datetime
from playwright.async_api import async_playwright

BASE_URL = "http://localhost:9001"

# Test users from database (credenciais do demo_seed.py)
USERS = [
    {"username": "platform_admin", "password": "admin123", "role": "SUPER_ADMIN", "expected_tenant": "all"},
    {"username": "belgo_admin", "password": "belgo123", "role": "ADMIN", "expected_tenant": "BELGO-001"},
    {"username": "belgo_pm", "password": "belgo123", "role": "PROJECT_MANAGER", "expected_tenant": "BELGO-001"},
    {"username": "tech_admin", "password": "tech123", "role": "ADMIN", "expected_tenant": "TECH-001"},
    {"username": "tech_dev", "password": "tech123", "role": "DEVELOPER", "expected_tenant": "TECH-001"},
    {"username": "startup_dev", "password": "startup123", "role": "DEVELOPER", "expected_tenant": "STARTUP-001"},
    {"username": "consultor", "password": "consul123", "role": "DEVELOPER", "expected_tenant": "multi"},
]

# Screens to test
SCREENS = [
    {"name": "Login", "path": "/login", "auth_required": False},
    {"name": "Dashboard Principal", "path": "/", "auth_required": True},
    {"name": "Projetos", "path": "/projects", "auth_required": True},
    {"name": "Analytics", "path": "/analytics", "auth_required": True},
    {"name": "Executive", "path": "/executive", "auth_required": True},
    {"name": "Admin", "path": "/admin", "auth_required": True},
    {"name": "Billing", "path": "/billing", "auth_required": True},
    {"name": "Settings", "path": "/settings", "auth_required": True},
]

# UX/UI Criteria
UX_CRITERIA = {
    "max_overlays": 2,          # Max simultaneous overlays
    "min_content_area": 60,     # Minimum % of screen for content
    "min_clickable_size": 44,   # Min clickable element size (px)
    "max_z_index_layers": 10,   # Max z-index layers
}

class QATestRunner:
    def __init__(self):
        self.results = {
            "timestamp": datetime.now().isoformat(),
            "screens": [],
            "issues": [],
            "ux_analysis": [],
            "segregation_tests": [],
            "summary": {}
        }

    async def run_all_tests(self):
        """Run complete QA test suite."""
        print("\n" + "="*60)
        print("QA TEST SUITE - Plataforma E")
        print("="*60)

        async with async_playwright() as p:
            browser = await p.chromium.launch(headless=False, slow_mo=500)
            context = await browser.new_context(viewport={"width": 1920, "height": 1080})
            page = await context.new_page()

            # Capture console errors
            errors = []
            page.on("console", lambda msg: errors.append({
                "type": msg.type,
                "text": msg.text[:500]
            }) if msg.type in ["error", "warning"] else None)

            page.on("pageerror", lambda err: errors.append({
                "type": "page_error",
                "text": str(err)[:500]
            }))

            # 1. Test Login Screen
            print("\n[1/5] Testing Login Screen...")
            await self.test_login_screen(page, errors)

            # 2. Test with different personas
            print("\n[2/5] Testing User Personas...")
            for user in USERS[:2]:  # Test first 2 users
                await self.test_user_persona(page, context, user, errors)

            # 3. Test all screens
            print("\n[3/5] Testing All Screens...")
            await self.test_all_screens(page, errors)

            # 4. UX/UI Analysis
            print("\n[4/5] Analyzing UX/UI...")
            await self.analyze_ux_ui(page, errors)

            # 5. Multi-tenancy segregation
            print("\n[5/5] Testing Data Segregation...")
            await self.test_data_segregation(page, context, errors)

            # Keep browser open for inspection
            print("\n" + "="*60)
            print("TESTS COMPLETED - Browser open for inspection")
            print("="*60)

            self.generate_summary()
            self.save_results()
            self.print_summary()

            # Keep open for manual inspection
            await asyncio.sleep(60)
            await browser.close()

    async def test_login_screen(self, page, errors):
        """Test login screen functionality."""
        errors.clear()
        await page.goto(f"{BASE_URL}/login")
        await page.wait_for_load_state("networkidle")

        result = {
            "name": "Login",
            "path": "/login",
            "status": "ok",
            "issues": [],
            "elements": {}
        }

        # Check required elements
        elements = {
            "username_field": await page.locator("input[name='username'], input[type='text']").count(),
            "password_field": await page.locator("input[type='password']").count(),
            "login_button": await page.locator("button[type='submit'], button:has-text('Login'), button:has-text('Entrar')").count(),
            "logo": await page.locator("img, .logo").count(),
        }
        result["elements"] = elements

        # Check for errors
        if errors:
            result["status"] = "warning"
            result["issues"].extend([e["text"][:200] for e in errors])

        # Check missing elements
        if elements["username_field"] == 0:
            result["issues"].append("Missing username field")
            result["status"] = "error"
        if elements["password_field"] == 0:
            result["issues"].append("Missing password field")
            result["status"] = "error"
        if elements["login_button"] == 0:
            result["issues"].append("Missing login button")
            result["status"] = "error"

        # Screenshot
        await page.screenshot(path="analysis/qa_01_login.png")
        result["screenshot"] = "analysis/qa_01_login.png"

        self.results["screens"].append(result)
        print(f"  Login: {result['status'].upper()}")
        for issue in result["issues"]:
            print(f"    - {issue[:80]}")

    async def test_user_persona(self, page, context, user, errors):
        """Test login and navigation for a specific user persona."""
        print(f"\n  Testing persona: {user['username']} ({user['role']})")

        errors.clear()

        # Try to login via API first
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{BASE_URL}/api/auth/login",
                    json={"username": user["username"], "password": user["password"]}
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        token = data.get("access_token")
                        role = data.get("role")
                        print(f"    API Login: OK - role={role}")

                        # Store token in localStorage via page
                        await page.goto(f"{BASE_URL}/")
                        await page.evaluate(f"""() => {{
                            localStorage.setItem('access_token', '{token}');
                            localStorage.setItem('user_id', '{user["username"]}');
                            localStorage.setItem('user_name', '{user["username"]}');
                            localStorage.setItem('user_role', '{role}');
                        }}""")
                        await page.reload()
                        await page.wait_for_timeout(2000)
                        print(f"    Session established for {user['username']}")

                        # Check visible content
                        body_text = await page.inner_text("body")

                        # Verify tenant data
                        if user["expected_tenant"] not in ["all", "multi"]:
                            # Check if user sees only their tenant's data
                            if "BELGO" in body_text.upper() and user["expected_tenant"] != "BELGO-001":
                                self.results["issues"].append({
                                    "type": "segregation",
                                    "severity": "critical",
                                    "message": f"User {user['username']} can see BELGO data but belongs to {user['expected_tenant']}",
                                    "agent": "[SEC]"
                                })

                        # Screenshot
                        await page.screenshot(path=f"analysis/qa_persona_{user['username']}.png")
                    else:
                        print(f"    API Login failed: HTTP {resp.status}")
                        self.results["issues"].append({
                            "type": "auth",
                            "severity": "high",
                            "message": f"Login failed for user {user['username']}",
                            "agent": "[BACK]"
                        })
        except Exception as e:
            print(f"    Error: {str(e)[:100]}")

    async def test_all_screens(self, page, errors):
        """Test all screens for functionality and errors."""
        for screen in SCREENS:
            errors.clear()
            print(f"\n  Testing: {screen['name']} ({screen['path']})")

            try:
                await page.goto(f"{BASE_URL}{screen['path']}")
                await page.wait_for_load_state("networkidle")
                await page.wait_for_timeout(1000)

                result = {
                    "name": screen["name"],
                    "path": screen["path"],
                    "status": "ok",
                    "issues": [],
                    "http_status": 200
                }

                # Check HTTP status
                response = await page.goto(f"{BASE_URL}{screen['path']}")
                if response:
                    result["http_status"] = response.status
                    if response.status >= 400:
                        result["status"] = "error"
                        result["issues"].append(f"HTTP {response.status}")

                # Check for console errors
                if errors:
                    critical_errors = [e for e in errors if e["type"] == "error"]
                    if critical_errors:
                        result["status"] = "error" if len(critical_errors) > 2 else "warning"
                        for e in critical_errors[:3]:
                            result["issues"].append(e["text"][:150])

                # Check for Vue rendering issues
                vue_templates = await page.locator("text=/\\{\\{.*\\}\\}/").count()
                if vue_templates > 0:
                    result["status"] = "error"
                    result["issues"].append(f"Vue templates not rendered ({vue_templates} found)")
                    self.add_issue("[FRONT]", "Vue templates visible in UI", f"Path: {screen['path']}", "high")

                # Screenshot
                screenshot_path = f"analysis/qa_{screen['name'].lower().replace(' ', '_')}.png"
                await page.screenshot(path=screenshot_path)
                result["screenshot"] = screenshot_path

                self.results["screens"].append(result)
                print(f"    Status: {result['status'].upper()}")
                for issue in result["issues"][:2]:
                    print(f"    - {issue[:80]}")

            except Exception as e:
                print(f"    Error loading: {str(e)[:100]}")
                self.results["screens"].append({
                    "name": screen["name"],
                    "path": screen["path"],
                    "status": "error",
                    "issues": [str(e)[:200]]
                })

    async def analyze_ux_ui(self, page, errors):
        """Analyze UX/UI for overlays, screen utilization, etc."""
        print("\n  Analyzing UX/UI metrics...")

        await page.goto(f"{BASE_URL}/")
        await page.wait_for_load_state("networkidle")
        await page.wait_for_timeout(2000)

        ux_result = {
            "screen": "Dashboard Principal",
            "metrics": {},
            "issues": []
        }

        try:
            # Check overlays
            overlays = await page.evaluate("""() => {
                const overlays = document.querySelectorAll('.modal, .overlay, .popup, .dialog, [role="dialog"]');
                return {
                    count: overlays.length,
                    visible: Array.from(overlays).filter(el =>
                        getComputedStyle(el).display !== 'none' &&
                        getComputedStyle(el).visibility !== 'hidden'
                    ).length
                };
            }""")
            ux_result["metrics"]["overlays"] = overlays

            if overlays["visible"] > UX_CRITERIA["max_overlays"]:
                ux_result["issues"].append(f"Too many visible overlays: {overlays['visible']}")
                self.add_issue("[FRONT]", "Too many simultaneous overlays",
                             f"Found {overlays['visible']} visible overlays, max is {UX_CRITERIA['max_overlays']}", "medium")

            # Check z-index layers
            z_indexes = await page.evaluate("""() => {
                const elements = document.querySelectorAll('*');
                const zIndexes = new Set();
                elements.forEach(el => {
                    const z = parseInt(getComputedStyle(el).zIndex);
                    if (!isNaN(z) && z > 0) zIndexes.add(z);
                });
                return Array.from(zIndexes).sort((a, b) => b - a);
            }""")
            ux_result["metrics"]["z_index_layers"] = len(z_indexes)
            ux_result["metrics"]["max_z_index"] = z_indexes[0] if z_indexes else 0

            if len(z_indexes) > UX_CRITERIA["max_z_index_layers"]:
                ux_result["issues"].append(f"Too many z-index layers: {len(z_indexes)}")

            # Check content area
            content_area = await page.evaluate("""() => {
                const main = document.querySelector('main, .main-content, .content, #app');
                if (!main) return 0;
                const rect = main.getBoundingClientRect();
                const viewportArea = window.innerWidth * window.innerHeight;
                const contentArea = rect.width * rect.height;
                return Math.round((contentArea / viewportArea) * 100);
            }""")
            ux_result["metrics"]["content_area_percent"] = content_area

            if content_area < UX_CRITERIA["min_content_area"]:
                ux_result["issues"].append(f"Content area too small: {content_area}%")
                self.add_issue("[FRONT]", "Poor screen utilization",
                             f"Content area is only {content_area}%, should be at least {UX_CRITERIA['min_content_area']}%", "medium")

            # Check clickable element sizes
            small_buttons = await page.evaluate(f"""() => {{
                const clickables = document.querySelectorAll('button, a, [role="button"], input[type="submit"]');
                let small = 0;
                clickables.forEach(el => {{
                    const rect = el.getBoundingClientRect();
                    if (rect.width < {UX_CRITERIA['min_clickable_size']} || rect.height < {UX_CRITERIA['min_clickable_size']}) {{
                        if (rect.width > 0 && rect.height > 0) small++;
                    }}
                }});
                return small;
            }}""")
            ux_result["metrics"]["small_clickables"] = small_buttons

            if small_buttons > 5:
                ux_result["issues"].append(f"Too many small clickable elements: {small_buttons}")
                self.add_issue("[FRONT]", "Clickable elements too small",
                             f"{small_buttons} elements smaller than {UX_CRITERIA['min_clickable_size']}px", "low")

            print(f"    Overlays: {overlays['visible']} visible")
            print(f"    Z-index layers: {len(z_indexes)}")
            print(f"    Content area: {content_area}%")
            print(f"    Small clickables: {small_buttons}")

        except Exception as e:
            print(f"    Error analyzing UX: {str(e)[:100]}")
            ux_result["issues"].append(str(e)[:200])

        self.results["ux_analysis"].append(ux_result)

    async def test_data_segregation(self, page, context, errors):
        """Test multi-tenancy data segregation."""
        print("\n  Testing data segregation...")

        # Test that different tenants see different data
        segregation_result = {
            "test": "multi_tenancy_segregation",
            "status": "ok",
            "issues": []
        }

        # Check if API returns segregated data
        await page.goto(f"{BASE_URL}/")
        await page.wait_for_load_state("networkidle")

        # Check for tenant selector visibility
        tenant_selector = await page.locator(".tenant-selector, [data-testid='tenant-selector'], select[name='tenant']").count()

        if tenant_selector > 0:
            # Non-admin users shouldn't see tenant selector
            segregation_result["issues"].append("Tenant selector visible - verify only for SUPER_ADMIN")
            self.add_issue("[SEC]", "Tenant selector visibility check needed",
                         "Verify tenant selector is only visible to SUPER_ADMIN users", "high")

        # Check story data visible
        stories_visible = await page.locator(".story-card, .kanban-card, [data-story]").count()
        print(f"    Stories visible: {stories_visible}")

        # Check if multiple tenant data visible
        body_text = await page.inner_text("body")
        tenants_mentioned = []
        if "BELGO" in body_text.upper():
            tenants_mentioned.append("BELGO")
        if "TECH" in body_text.upper():
            tenants_mentioned.append("TECH")
        if "STARTUP" in body_text.upper():
            tenants_mentioned.append("STARTUP")

        print(f"    Tenants mentioned: {tenants_mentioned}")

        if len(tenants_mentioned) > 1:
            segregation_result["issues"].append(f"Multiple tenant data visible: {tenants_mentioned}")
            self.add_issue("[SEC]", "Data segregation issue",
                         f"Multiple tenant data visible on same screen: {tenants_mentioned}", "critical")
            segregation_result["status"] = "error"

        self.results["segregation_tests"].append(segregation_result)

    def add_issue(self, agent_prefix, title, description, severity):
        """Add an issue to be created."""
        self.results["issues"].append({
            "agent": agent_prefix,
            "title": title,
            "description": description,
            "severity": severity,
            "created_at": datetime.now().isoformat()
        })

    def generate_summary(self):
        """Generate test summary."""
        screens_ok = len([s for s in self.results["screens"] if s.get("status") == "ok"])
        screens_warning = len([s for s in self.results["screens"] if s.get("status") == "warning"])
        screens_error = len([s for s in self.results["screens"] if s.get("status") == "error"])

        self.results["summary"] = {
            "total_screens": len(self.results["screens"]),
            "screens_ok": screens_ok,
            "screens_warning": screens_warning,
            "screens_error": screens_error,
            "total_issues": len(self.results["issues"]),
            "issues_by_agent": {}
        }

        # Count issues by agent
        for issue in self.results["issues"]:
            agent = issue.get("agent", "[UNKNOWN]")
            if agent not in self.results["summary"]["issues_by_agent"]:
                self.results["summary"]["issues_by_agent"][agent] = 0
            self.results["summary"]["issues_by_agent"][agent] += 1

    def save_results(self):
        """Save results to JSON."""
        os.makedirs("analysis", exist_ok=True)
        with open("analysis/qa_results.json", "w", encoding="utf-8") as f:
            json.dump(self.results, f, indent=2, ensure_ascii=False)
        print(f"\n  Results saved to analysis/qa_results.json")

    def print_summary(self):
        """Print test summary."""
        print("\n" + "="*60)
        print("TEST SUMMARY")
        print("="*60)

        s = self.results["summary"]
        print(f"\nScreens Tested: {s['total_screens']}")
        print(f"  - OK: {s['screens_ok']}")
        print(f"  - Warning: {s['screens_warning']}")
        print(f"  - Error: {s['screens_error']}")

        print(f"\nTotal Issues Found: {s['total_issues']}")
        if s["issues_by_agent"]:
            print("\nIssues by Agent:")
            for agent, count in s["issues_by_agent"].items():
                print(f"  {agent}: {count}")

        if self.results["issues"]:
            print("\n" + "-"*60)
            print("ISSUES TO CREATE:")
            print("-"*60)
            for i, issue in enumerate(self.results["issues"], 1):
                title = issue.get('title', issue.get('message', 'Unknown issue'))
                desc = issue.get('description', issue.get('message', ''))
                severity = issue.get('severity', 'medium')
                print(f"\n{i}. {issue['agent']} - {title}")
                print(f"   Severity: {severity}")
                if desc:
                    print(f"   {desc[:100]}")


async def main():
    runner = QATestRunner()
    await runner.run_all_tests()


if __name__ == "__main__":
    asyncio.run(main())
