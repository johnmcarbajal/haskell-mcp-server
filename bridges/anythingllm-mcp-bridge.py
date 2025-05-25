#!/usr/bin/env python3
"""
AnythingLLM MCP Bridge - Enhanced integration with document analysis capabilities
Provides OpenAI-compatible API with MCP tool integration optimized for AnythingLLM
"""

from fastapi import FastAPI, HTTPException, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any, Union
import asyncio
import json
import subprocess
import requests
import uuid
from datetime import datetime
import logging
import base64
import io

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="AnythingLLM MCP Bridge",
    version="1.0.0",
    description="OpenAI-compatible API with Haskell MCP tools for AnythingLLM"
)

# CORS for AnythingLLM
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

class Message(BaseModel):
    role: str
    content: str

class ChatCompletionRequest(BaseModel):
    model: str
    messages: List[Message]
    temperature: Optional[float] = 0.7
    max_tokens: Optional[int] = 4096
    stream: Optional[bool] = False
    functions: Optional[List[Dict]] = None
    function_call: Optional[Union[str, Dict]] = None

class AnythingLLMMcpBridge:
    def __init__(self, mcp_command="stack exec haskell-mcp-server-exe -- --stdio",
                 ollama_host="http://localhost:11434"):
        self.mcp_command = mcp_command.split()
        self.ollama_host = ollama_host
        self.mcp_process = None
        self.tools = []
        self.request_id = 0
        self.initialized = False

    async def start_mcp_server(self):
        """Start and initialize the Haskell MCP server"""
        if self.initialized:
            return

        logger.info(f"🚀 Starting Haskell MCP server for AnythingLLM")

        try:
            self.mcp_process = await asyncio.create_subprocess_exec(
                *self.mcp_command,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )

            # MCP initialization
            await self.send_mcp_message({
                "jsonrpc": "2.0",
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "anythingllm-bridge", "version": "1.0.0"}
                },
                "id": self.get_next_id()
            })

            await self.send_mcp_message({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            })

            await self.load_tools()
            self.initialized = True
            logger.info(f"✅ MCP server ready with {len(self.tools)} tools for AnythingLLM")

        except Exception as e:
            logger.error(f"❌ MCP server startup failed: {e}")
            raise

    async def send_mcp_message(self, message):
        """Send message to MCP server"""
        if not self.mcp_process:
            raise Exception("MCP server not started")

        message_str = json.dumps(message) + "\n"
        self.mcp_process.stdin.write(message_str.encode())
        await self.mcp_process.stdin.drain()

        if "id" in message:
            line = await self.mcp_process.stdout.readline()
            if line:
                return json.loads(line.decode().strip())
        return None

    async def load_tools(self):
        """Load MCP tools"""
        response = await self.send_mcp_message({
            "jsonrpc": "2.0",
            "method": "tools/list",
            "params": {},
            "id": self.get_next_id()
        })

        if response and "result" in response:
            self.tools = response["result"].get("tools", [])
            logger.info("📦 Available MCP tools for AnythingLLM:")
            for tool in self.tools:
                logger.info(f"  🔧 {tool['name']}: {tool['description']}")

    async def call_tool(self, tool_name, arguments):
        """Execute MCP tool"""
        try:
            response = await self.send_mcp_message({
                "jsonrpc": "2.0",
                "method": "tools/call",
                "params": {"name": tool_name, "arguments": arguments},
                "id": self.get_next_id()
            })

            if response and "result" in response:
                result = response["result"]
                if "content" in result and result["content"]:
                    content_text = "\n".join([
                        item.get("text", "") for item in result["content"]
                        if item.get("type") == "text"
                    ])
                    return content_text
                return str(result)
            return "Tool execution failed"

        except Exception as e:
            logger.error(f"Tool error: {e}")
            return f"Error executing {tool_name}: {str(e)}"

    def get_next_id(self):
        self.request_id += 1
        return self.request_id

    def call_ollama(self, model: str, messages: List[Dict]):
        """Call Ollama API"""
        try:
            response = requests.post(f"{self.ollama_host}/api/chat", json={
                "model": model,
                "messages": messages,
                "stream": False
            }, timeout=60)

            if response.status_code == 200:
                return response.json()["message"]["content"]
            else:
                return f"Ollama error: HTTP {response.status_code}"

        except Exception as e:
            logger.error(f"Ollama request failed: {e}")
            return f"Error: Could not connect to Ollama at {self.ollama_host}"

    def create_anythingllm_system_prompt(self):
        """Create system prompt optimized for AnythingLLM"""
        if not self.tools:
            return ""

        tools_desc = []
        for tool in self.tools:
            tools_desc.append("• **{}**: {}".format(tool['name'], tool['description']))

        tools_list = "\n".join(tools_desc)

        system_prompt = """You are an AI assistant integrated with AnythingLLM, with access to these specialized tools:

{}

**Tool Usage Instructions:**
- Use tools when they would be helpful for the user's request
- Call tools using: EXECUTE_TOOL[tool_name](json_arguments)
- You can use multiple tools in sequence if needed
- Tools are especially useful for:
  * Mathematical calculations
  * Text analysis of documents
  * Data processing and generation
  * Time-based predictions
  * Encoding/decoding operations

**Examples:**
- EXECUTE_TOOL[text_analysis]({{"text": "document content here"}})
- EXECUTE_TOOL[calculate]({{"expression": "25 * 7 + 12"}})
- EXECUTE_TOOL[jeff_invite_predictor]({{}})

**Context Awareness:**
- When analyzing documents, use text_analysis tool for insights
- For mathematical questions in documents, use the calculator
- When users ask about predictions or random elements, use appropriate tools
- Always provide helpful, contextual responses that enhance the document experience

Focus on being helpful with document analysis, knowledge extraction, and providing enhanced insights through your tools.""".format(tools_list)

        return system_prompt

    async def process_with_mcp_tools(self, messages: List[Message], model: str):
        """Process messages with MCP tool integration"""
        ollama_messages = [{"role": msg.role, "content": msg.content} for msg in messages]

        # Add MCP tools context for AnythingLLM
        if self.tools:
            system_prompt = self.create_anythingllm_system_prompt()
            system_message = {"role": "system", "content": system_prompt}
            ollama_messages = [system_message] + ollama_messages

        # Get response from Ollama
        initial_response = self.call_ollama(model, ollama_messages)

        # Process tool calls
        if "EXECUTE_TOOL[" in initial_response:
            import re
            tool_calls = re.findall(r'EXECUTE_TOOL\[([^\]]+)\]\(([^)]*)\)', initial_response)

            if tool_calls:
                tool_results = []
                for tool_name, args_str in tool_calls:
                    try:
                        args = json.loads(args_str) if args_str.strip() else {}
                        logger.info(f"🔧 AnythingLLM executing: {tool_name}")

                        result = await self.call_tool(tool_name, args)
                        tool_results.append({
                            "tool": tool_name,
                            "result": result,
                            "formatted": f"**📊 {tool_name.replace('_', ' ').title()} Result:**\n{result}"
                        })

                    except Exception as e:
                        logger.error(f"Tool execution error: {e}")
                        tool_results.append({
                            "tool": tool_name,
                            "result": f"Error: {str(e)}",
                            "formatted": f"**❌ {tool_name} Error:** {str(e)}"
                        })

                if tool_results:
                    # Create enhanced response with tool results
                    results_text = "\n\n".join([tr["formatted"] for tr in tool_results])

                    followup_messages = ollama_messages + [
                        {"role": "assistant", "content": initial_response},
                        {"role": "user", "content": f"""Here are the tool execution results:

{results_text}

Please provide a comprehensive response that:
1. Incorporates these tool results naturally
2. Provides additional context and insights
3. Relates the results back to the original question
4. Maintains a helpful, informative tone suitable for AnythingLLM

Don't mention the tool syntax or execution details - just present the enhanced information."""}
                    ]

                    enhanced_response = self.call_ollama(model, followup_messages)
                    return enhanced_response

        return initial_response

    def convert_to_openai_functions(self):
        """Convert MCP tools to OpenAI function format for AnythingLLM"""
        functions = []
        for tool in self.tools:
            function = {
                "name": tool["name"],
                "description": tool["description"],
                "parameters": tool.get("inputSchema", {"type": "object", "properties": {}})
            }
            functions.append(function)
        return functions

# Global bridge instance
bridge = AnythingLLMMcpBridge()

@app.on_event("startup")
async def startup_event():
    """Initialize MCP server on startup"""
    try:
        await bridge.start_mcp_server()
    except Exception as e:
        logger.error(f"Startup failed: {e}")

@app.post("/v1/chat/completions")
async def create_chat_completion(request: ChatCompletionRequest):
    """OpenAI-compatible chat completions for AnythingLLM"""
    try:
        if not bridge.initialized:
            await bridge.start_mcp_server()

        # Process with MCP tools
        content = await bridge.process_with_mcp_tools(request.messages, request.model)

        # Calculate token usage (approximate)
        prompt_tokens = sum(len(msg.content.split()) for msg in request.messages)
        completion_tokens = len(content.split())

        response = {
            "id": f"chatcmpl-{uuid.uuid4().hex[:12]}",
            "object": "chat.completion",
            "created": int(datetime.now().timestamp()),
            "model": request.model,
            "choices": [{
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": content
                },
                "finish_reason": "stop"
            }],
            "usage": {
                "prompt_tokens": prompt_tokens,
                "completion_tokens": completion_tokens,
                "total_tokens": prompt_tokens + completion_tokens
            }
        }

        return response

    except Exception as e:
        logger.error(f"Chat completion error: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/v1/models")
async def list_models():
    """List available models for AnythingLLM"""
    models = [
        "llama2", "llama2:7b", "llama2:13b", "llama2:70b",
        "mistral", "mistral:7b", "mixtral", "mixtral:8x7b",
        "codellama", "codellama:7b", "codellama:13b",
        "llama3", "llama3:8b", "llama3:70b",
        "phi3", "phi3:mini", "phi3:medium",
        "gemma", "gemma:2b", "gemma:7b"
    ]

    return {
        "object": "list",
        "data": [
            {
                "id": model,
                "object": "model",
                "created": int(datetime.now().timestamp()),
                "owned_by": "ollama",
                "permission": [],
                "root": model,
                "parent": None
            }
            for model in models
        ]
    }

@app.get("/v1/functions")
async def list_functions():
    """List available MCP functions for AnythingLLM"""
    if not bridge.initialized:
        await bridge.start_mcp_server()

    return {
        "functions": bridge.convert_to_openai_functions(),
        "count": len(bridge.tools)
    }

@app.get("/health")
async def health_check():
    """Health check for AnythingLLM integration"""
    return {
        "status": "healthy",
        "service": "AnythingLLM MCP Bridge",
        "mcp_initialized": bridge.initialized,
        "mcp_tools": len(bridge.tools),
        "available_tools": [tool["name"] for tool in bridge.tools],
        "integration": "AnythingLLM ready"
    }

@app.get("/")
async def root():
    """Root endpoint with AnythingLLM setup info"""
    return {
        "service": "AnythingLLM MCP Bridge",
        "version": "1.0.0",
        "status": "running",
        "mcp_tools": len(bridge.tools),
        "anythingllm_setup": {
            "base_url": "http://localhost:8000",
            "api_key": "sk-anythingllm",
            "model_options": ["llama2", "mistral", "codellama", "llama3"],
            "features": [
                "Document analysis with MCP tools",
                "Enhanced text processing",
                "Mathematical calculations",
                "Random predictions and data generation",
                "Base64 encoding/decoding",
                "Jeff's bacon cheeseburger predictions 🍔"
            ]
        }
    }

if __name__ == "__main__":
    import uvicorn

    print("🚀 Starting AnythingLLM MCP Bridge...")
    print("📚 Enhanced for document analysis and knowledge management")
    print("🔧 Your Haskell MCP tools will be available in AnythingLLM")
    print("")
    print("📋 AnythingLLM Configuration:")
    print("   • LLM Provider: OpenAI (Custom)")
    print("   • Base URL: http://localhost:8000")
    print("   • API Key: sk-anythingllm")
    print("   • Model: llama2 (or any Ollama model)")
    print("")
    print("🍔 Available tools: All 9 including Jeff's bacon cheeseburger predictor!")

    uvicorn.run(app, host="0.0.0.0", port=8000, log_level="info")
