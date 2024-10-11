# persistent_llm_server.py
# WebSocket server with persistent memory and summarization capability

import asyncio
import websockets
import ollama
from collections import deque

# Persistent memory for conversation context
conversation_context = deque(maxlen=100)  # Keeps the last 100 interactions for summarization

# Function to summarize conversation context
def summarize_context():
    total_figures = len([line for line in conversation_context if 'saving' in line])
    total_files = len([line for line in conversation_context if 'file' in line])
    recent_entries = '; '.join(list(conversation_context)[-5:])
    summary = f"Total figures: {total_figures}, Total files: {total_files}, Recent entries: {recent_entries}..."
    return summary

# Function to handle a single client connection
async def handle_connection(websocket):
    while True:
        try:
            prompt = await websocket.recv()
            conversation_context.append(prompt)
            response = ollama.ask("\n".join(conversation_context))
            conversation_context.append(response)
            await websocket.send(response)

            # Send summary periodically (e.g., every 10th interaction)
            if len(conversation_context) % 10 == 0:
                summary = summarize_context()
                await websocket.send(f"[SUMMARY]: {summary}")
        except websockets.ConnectionClosed:
            break

# Function to start the WebSocket server
async def start_server(host="localhost", port=8765):
    async with websockets.serve(handle_connection, host, port):
        await asyncio.Future()  # Keep the server running indefinitely

# Main function to run the server
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="WebSocket server with persistent memory and summarization capability.")
    parser.add_argument("--host", type=str, default="localhost", help="Host address to run the server on")
    parser.add_argument("--port", type=int, default=8765, help="Port to run the server on")
    args = parser.parse_args()

    asyncio.run(start_server(host=args.host, port=args.port))
