# -*- coding: utf-8 -*-
"""
Entry point para execucao do MCP Server como modulo

Uso:
    python -m factory.mcp
    python -m factory.mcp --test
    python -m factory.mcp --list-tools
"""

from factory.mcp.server import run_server, test_tools, MCPServer
import asyncio
import argparse


def main():
    """Funcao principal"""
    parser = argparse.ArgumentParser(
        description="MCP Server para Fabrica de Agentes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
  python -m factory.mcp                # Inicia servidor MCP (modo stdio)
  python -m factory.mcp --test         # Testa ferramentas
  python -m factory.mcp --list-tools   # Lista ferramentas disponiveis

Configuracao para Claude Desktop (claude_desktop_config.json):
{
    "mcpServers": {
        "fabrica-agentes": {
            "command": "python",
            "args": ["-m", "factory.mcp"]
        }
    }
}
        """
    )

    parser.add_argument(
        "--test",
        action="store_true",
        help="Testar ferramentas MCP"
    )

    parser.add_argument(
        "--list-tools",
        action="store_true",
        help="Listar todas as ferramentas disponiveis"
    )

    parser.add_argument(
        "--json",
        action="store_true",
        help="Output em formato JSON (para --list-tools)"
    )

    args = parser.parse_args()

    if args.test:
        asyncio.run(test_tools())
    elif args.list_tools:
        server = MCPServer()
        if args.json:
            import json
            tools = server.get_tools_list()
            print(json.dumps(tools, indent=2, ensure_ascii=False))
        else:
            print("\n" + "=" * 60)
            print("FERRAMENTAS MCP - FABRICA DE AGENTES")
            print("=" * 60 + "\n")

            # Agrupar por categoria
            from collections import defaultdict
            by_category = defaultdict(list)
            for tool in server.tools:
                by_category[tool.category.value].append(tool)

            for category, tools in sorted(by_category.items()):
                print(f"\n[{category.upper()}]")
                print("-" * 40)
                for tool in tools:
                    print(f"\n  {tool.name}")
                    print(f"    {tool.description}")
                    if tool.parameters:
                        print("    Parametros:")
                        for param in tool.parameters:
                            req = "*" if param.required else ""
                            print(f"      - {param.name}{req}: {param.description}")

            print("\n" + "=" * 60)
            print(f"Total: {len(server.tools)} ferramentas")
            print("=" * 60 + "\n")
    else:
        run_server()


if __name__ == "__main__":
    main()
