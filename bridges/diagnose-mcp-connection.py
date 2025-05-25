#!/usr/bin/env python3
"""
MCP Connection Diagnostic Tool
Helps troubleshoot AnythingLLM + MCP integration issues
"""

import requests
import subprocess
import json
import sys
import time
from urllib.parse import urlparse

def check_port(host, port):
    """Check if a port is open"""
    import socket
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((host, port))
        sock.close()
        return result == 0
    except:
        return False

def test_ollama():
    """Test Ollama connection"""
    print("🦙 Testing Ollama...")
    try:
        response = requests.get("http://localhost:11434/api/tags", timeout=5)
        if response.status_code == 200:
            models = response.json().get("models", [])
            print(f"  ✅ Ollama running with {len(models)} models")
            for model in models[:3]:  # Show first 3 models
                print(f"     • {model['name']}")
            return True
        else:
            print(f"  ❌ Ollama returned status {response.status_code}")
            return False
    except Exception as e:
        print(f"  ❌ Ollama not accessible: {e}")
        print("     💡 Try: ollama serve")
        return False

def test_haskell_mcp():
    """Test Haskell MCP server"""
    print("⚡ Testing Haskell MCP server...")
    try:
        # Test if stack build works
        result = subprocess.run(
            ["stack", "build"],
            cwd=".",
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            print("  ✅ Haskell MCP server builds successfully")
        else:
            print("  ❌ Haskell build failed:")
            print(f"     {result.stderr[:200]}...")
            return False

        # Test basic MCP communication
        test_msg = '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"diagnostic","version":"1.0"}},"id":0}\n'

        process = subprocess.Popen(
            ["stack", "exec", "haskell-mcp-server-exe", "--", "--stdio"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        stdout, stderr = process.communicate(input=test_msg, timeout=10)

        if "jsonrpc" in stdout and "result" in stdout:
            print("  ✅ MCP server responds to initialize")
            return True
        else:
            print("  ❌ MCP server not responding properly")
            print(f"     stdout: {stdout[:100]}...")
            print(f"     stderr: {stderr[:100]}...")
            return False

    except subprocess.TimeoutExpired:
        print("  ❌ MCP server timed out")
        return False
    except FileNotFoundError:
        print("  ❌ Stack not found")
        print("     💡 Install Stack: https://docs.haskellstack.org/")
        return False
    except Exception as e:
        print(f"  ❌ MCP test failed: {e}")
        return False

def test_bridge():
    """Test MCP bridge"""
    print("🌉 Testing MCP bridge...")

    # Check if bridge is running
    if not check_port("localhost", 8000):
        print("  ❌ Bridge not running on port 8000")
        print("     💡 Start with: python anythingllm-mcp-bridge.py")
        return False

    try:
        # Test health endpoint
        response = requests.get("http://localhost:8000/health", timeout=5)
        if response.status_code == 200:
            data = response.json()
            print(f"  ✅ Bridge healthy with {data.get('mcp_tools', 0)} tools")
            if data.get('mcp_initialized'):
                print("     ✅ MCP integration initialized")
            else:
                print("     ❌ MCP not initialized")
                return False
        else:
            print(f"  ❌ Bridge health check failed: {response.status_code}")
            return False

        # Test chat completion
        chat_data = {
            "model": "llama2",
            "messages": [{"role": "user", "content": "Hello"}]
        }

        response = requests.post(
            "http://localhost:8000/v1/chat/completions",
            headers={"Content-Type": "application/json"},
            json=chat_data,
            timeout=30
        )

        if response.status_code == 200:
            result = response.json()
            if "choices" in result and result["choices"]:
                print("  ✅ Chat completions working")
                return True
            else:
                print("  ❌ Invalid chat response format")
                return False
        else:
            print(f"  ❌ Chat completion failed: {response.status_code}")
            print(f"     Response: {response.text[:200]}...")
            return False

    except Exception as e:
        print(f"  ❌ Bridge test failed: {e}")
        return False

def test_anythingllm_config():
    """Check AnythingLLM configuration"""
    print("📚 AnythingLLM Configuration Checklist:")
    print("  📋 In AnythingLLM Settings → LLM Configuration:")
    print("     • Provider: OpenAI")
    print("     • Base URL: http://localhost:8000/v1")
    print("     • API Key: sk-anythingllm (any value works)")
    print("     • Model: llama2")
    print()
    print("  🔍 If AnythingLLM is running in Docker:")
    print("     • Base URL: http://host.docker.internal:8000/v1")
    print("     • (Docker needs special host to reach localhost)")

def main():
    print("🔍 MCP Connection Diagnostic Tool")
    print("=" * 50)
    print()

    results = []

    # Run all tests
    results.append(("Ollama", test_ollama()))
    results.append(("Haskell MCP", test_haskell_mcp()))
    results.append(("Bridge", test_bridge()))

    print()
    print("📊 Diagnostic Summary:")
    print("-" * 30)

    all_passed = True
    for test_name, passed in results:
        status = "✅ PASS" if passed else "❌ FAIL"
        print(f"  {test_name:<15} {status}")
        if not passed:
            all_passed = False

    print()

    if all_passed:
        print("🎉 All tests passed! Your MCP integration should work.")
        print("   If AnythingLLM still isn't connecting, check the configuration below:")
        print()
        test_anythingllm_config()
    else:
        print("❌ Some tests failed. Fix the issues above and try again.")
        print()
        print("🔧 Common fixes:")
        print("  • Start Ollama: ollama serve")
        print("  • Build MCP server: cd haskell-mcp-server && stack build")
        print("  • Start bridge: python anythingllm-mcp-bridge.py")

    print()
    print("🆘 If you need help, share the output above!")

if __name__ == "__main__":
    main()
