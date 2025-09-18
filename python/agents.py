# agents.py
# Setting up multiple agents, each powered by different LLMs via the Ollama Python package

import sys
import asyncio
import re
import json
import aiohttp
import heapq
from collections import deque
import ollama
from ollama import AsyncClient

from . import voice
from .voice import (
    speak_text,
    speak_text_streaming,
    submit_to_voice_queue,
)  # Import the new function from voice.py


# from .voice import speak_text_in_chunks  # Import the new function from voice.py


# Default agents and models
default_agents = {
    "summarizer": "llama3.2:3b",
    "thinker": "llama3.2:3b",
    # "logger": "llama-2-7b",
}


# Function to list available models using the Ollama Python package
def list_available_models():
    return [x["name"] for x in ollama.list()["models"]]


# Function to set agent models/convers
agent_models = {}


def initialize_agents(defaults=default_agents):
    available_models = list_available_models()
    for agent, model in defaults.items():
        if model in available_models:
            agent_models[agent] = model
        else:
            # Fallback to a default model if specified model not available
            agent_models[agent] = available_models[list(available_models.keys())[0]]
            # Using the first available model as a fallback


def split_text_to_chunks(text):
    """
    Split the given text into chunks by sentences.

    Parameters:
        text (str): The text to be split.

    Returns:
        list: A list of sentence chunks.
    """
    # Split the text into chunks by sentences
    chunks = re.split(r"(?<=[.!?])\s+", text)
    return [chunk for chunk in chunks if chunk]


# Function to query a specific agent (updated version)
async def query_agent(agent_name, prompt, conversation_context):
    print(f"Querying agent: {agent_name}")

    if agent_name not in agent_models:
        raise ValueError(f"Unknown agent: {agent_name}")

    model_name = agent_models[agent_name]
    print(f"Using model: {model_name}")

    async with aiohttp.ClientSession() as session:
        post_str = f"http://localhost:11434/api/generate"
        json_data = {"model": model_name, "prompt": "\n".join(conversation_context)}
        print(f"POST {post_str} with data: {json_data}")

        async with session.post(post_str, json=json_data) as resp:
            if resp.status == 200:
                buffer = ""
                async for line in resp.content:
                    part = line.decode("utf-8")
                    part_json = json.loads(part)
                    response_part = part_json.get("response", "")
                    print(response_part, end="")  # Stream response to terminal

                    # Yield each part of the response incrementally
                    yield response_part

                    # Buffer for speaking text
                    buffer += response_part

                    # Check if the buffer contains a complete sentence
                    chunks = split_text_to_chunks(buffer)
                    if len(chunks) > 1:
                        # Speak all but the last incomplete chunk in real-time
                        for chunk in chunks[:-1]:
                            await asyncio.to_thread(
                                speak_text, chunk, voice_accent="en"
                            )
                        buffer = chunks[-1]

                # Speak any remaining text in the buffer after the full response
                if buffer:
                    await asyncio.to_thread(speak_text, buffer, voice_accent="en")
            else:
                raise RuntimeError(
                    f"Failed to query model {model_name}, status: {resp.status}"
                )

    # response = ollama.chat(
    #     model=model_name, message="\n".join(conversation_context), stream=True
    # )
    # response = AsyncClient().chat(model=model_name, message="\n".join(conversation_context))
    # conversation_context.append(response)
    # yield response
    # # return response


# Function to summarize the conversation context using the summarizer agent
async def summarize_conversation(conversation_context):
    response_parts = []
    async for part in query_agent(
        "summarizer", "Summarize the following conversation:", conversation_context
    ):
        response_parts.append(part)
    return "".join(response_parts)


# Function to handle a thinking task using the thinker agent
async def think_on_conversation(conversation_context):
    response_parts = []
    async for part in query_agent(
        agent_name="thinker",
        prompt=f"""
            < event loop tick >,
            < can say 'wait' to wait or "speak' to speak response >,
            {conversation_context},
            """,
        conversation_context=conversation_context,
    ):
        response_parts.append(part)
        yield part


# Task class for managing tasks in the queue
class Task:
    def __init__(self, message, agent, priority):
        self.message = message
        self.agent = agent
        self.priority = priority

    def __lt__(self, other):
        return self.priority < other.priority

    def __repr__(self):
        return f"Task: {self.message}, agent: {self.agent}, priority: {self.priority}"


# PriorityQueue class to manage tasks
# agents.py
class PriorityQueue:
    def __init__(self):
        self._queue = []
        self._index = 0

    async def put(self, task):
        heapq.heappush(self._queue, (task.priority, self._index, task))
        self._index += 1

    async def get(self):
        if self._queue:
            return heapq.heappop(self._queue)[-1]
        raise QueueEmpty()


class QueueEmpty(Exception):
    pass


# Global instance of PriorityQueue
priority_queue = PriorityQueue()


# Function to submit a new task to the queue
async def submit_task(message, agent=None, priority=1):
    print(f"Submitting task: {message}, agent: {agent}, priority: {priority}")
    if agent is None:
        agent = "thinker"
    task = Task(message, agent, priority)
    await priority_queue.put(task)


# def stream_to_terminal(gen):
#     for item in gen:
#         print(item, flush=True)


async def event_loop():
    # Initialize agents with default or available models
    initialize_agents()
    print("Initialized agents:", agent_models)
    conversation_context = deque(maxlen=100)
    conversation_context.append("Initial conversation message to set up context.")

    global priority_queue

    # Add initial tasks to the queue
    await priority_queue.put(
        Task(
            """
        <This is an automated message> <Starting up>
        <You are a computer monitoring io via an event loop. YOu may get user messages. You may respond to them>
        <You can respond by saying the word "speak" in your response>
        <You can also choose to wait by saying "wait">
""",
            "thinker",
            priority=1,
        )
    )
    # await priority_queue.put(Task("Hello", "thinker", priority=2))

    # Process tasks in the event loop
    ticks = 0
    print("Starting event loop")
    while True:
        try:
            task = await priority_queue.get()
            print(f"Processing {task}")

            if task.agent == "wait":
                conversation_context.append("waiting...")
                continue
            elif task.agent == "speak":
                # Example: Handle speaking a message directly
                conversation_context.append(f"my response: '{task.message}'")
                await submit_to_voice_queue(task.message)

            elif task.agent == "thinker":
                # Query the agent (e.g., an LLM) for processing the conversation
                response_parts = []
                async for part in think_on_conversation(conversation_context):
                    response_parts.append(part)
                    # return "".join(response_parts)
                response = "".join(response_parts)
                conversation_context.append(f"new message: '{response}'")
                sys.stdout.write(response)
                # Optionally, submit the response for further analysis or as a task for speaking
                if "speak" in response.lower():
                    await submit_to_voice_queue(response)

                # # After thinking, add a new task to continue the conversation
                # await submit_task(
                #     "Think about the conversation further", "thinker", priority=2
                # )

            elif task.agent == "summarizer":
                # Handle summarization of the current conversation context
                summary = await summarize_conversation(conversation_context)
                conversation_context.append(f"summary: '{summary}'")
                print(f"Generated summary: {summary}")

                # Add a task to think about the summary if needed
                await priority_queue.put(
                    Task("Summarize the conversation", "summarizer", priority=1)
                )
                # await submit_task("Think about the conversation", "thinker", priority=2)

            else:
                print(f"Unknown agent: {task.agent}")

        except QueueEmpty:
            ticks += 1
            await asyncio.sleep(1)
            if ticks > 10:
                print(f"Been waiting for {ticks} ticks, adding a reminder task.")
                ticks = 0
                await submit_task("Please provide input", "thinker", priority=3)


def main():
    async def run():
        await asyncio.run(event_loop())


# Run the event loop
if __name__ == "__main__":
    asyncio.run(event_loop())
