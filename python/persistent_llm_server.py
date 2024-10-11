# persistent_llm_server.py
# WebSocket server with persistent memory, summarization capability, and passive/active modes

import asyncio
import websockets
import ollama
from collections import deque

# Persistent memory for conversation context
conversation_context = deque(maxlen=100)  # Keeps the last 100 interactions for summarization

# Initial prompt to instruct the LLM
initial_prompt = (
    "You are watching a program run, observing the logs. "
    "Prepare to provide summaries or answer questions when asked."
)
conversation_context.append(initial_prompt)

# Command constants
COMMAND_RESPOND = "respond"
COMMAND_SUMMARIZE = "summarize"


# Function to summarize conversation context
def summarize_context():
    total_figures = len([line for line in conversation_context if 'saving' in line])
    total_files = len([line for line in conversation_context if 'file' in line])
    recent_entries = '; '.join(list(conversation_context)[-5:])
    summary = f"Total figures: {total_figures}, Total files: {total_files}, Recent entries: {recent_entries}..."
    return summary

# Function to handle the respond command
async def handle_respond_command(websocket):
    response = ollama.ask("\n".join(conversation_context))
    conversation_context.append(response)
    await websocket.send(response)

# Function to handle the summarize command
async def handle_summarize_command(websocket):
    summary = summarize_context()
    await websocket.send(f"[SUMMARY]: {summary}")

# Function to handle an unknown command
async def handle_unknown_command(websocket, command):
    await websocket.send(f"[ERROR]: Unknown command '{command}'")

# Function to handle a log entry in passive mode
async def handle_log_entry(websocket, prompt):
    conversation_context.append(prompt)
    # Optionally send a periodic summary (e.g., every 10th interaction)
    if len(conversation_context) % 10 == 0:
        summary = summarize_context()
        await websocket.send(f"[SUMMARY]: {summary}")

# Function to handle a single client connection
async def handle_connection(websocket):
    while True:
        try:
            prompt = await websocket.recv()
            # Check if the prompt is a command
            if prompt.startswith("COMMAND: "):
                command = prompt[len("COMMAND: "):].strip().lower()
                if command == COMMAND_RESPOND:
                    await handle_respond_command(websocket)
                    continue
                if command == COMMAND_SUMMARIZE:
                    await handle_summarize_command(websocket)
                    continue
                await handle_unknown_command(websocket, command)
                continue
            # Handle log entry in passive mode
            await handle_log_entry(websocket, prompt)
        except websockets.ConnectionClosed:
            break

# Function to start the WebSocket server
async def start_server(host="localhost", port=8765):
    async with websockets.serve(handle_connection, host, port):
        await asyncio.Future()  # Keep the server running indefinitely

# Main function to run the server
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="WebSocket server with persistent memory, summarization, and passive/active modes.")
    parser.add_argument("--host", type=str, default="localhost", help="Host address to run the server on")
    parser.add_argument("--port", type=int, default=8765, help="Port to run the server on")
    args = parser.parse_args()

    asyncio.run(start_server(host=args.host, port=args.port))
