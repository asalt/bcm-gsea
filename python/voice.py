# voice.py
# A module to handle text-to-speech functionality using spd-say for Unix-like systems
import subprocess
import re
import asyncio

voice_queue = asyncio.Queue()


def speak_text(text, voice_accent="en", speed=None):
    """
    Speak the given text using spd-say.

    Parameters:
        text (str): The text to be spoken.
        voice_accent (str): The voice accent/language to use. Default is 'en'.
        speed (int, optional): The speed at which to speak. Default is None.
    """
    # Construct the command arguments
    command = ["spd-say", "-w", "-l", voice_accent, text]

    # Add speed option if specified
    if speed is not None:
        command.extend(["-r", str(speed)])

    # Execute the command
    try:
        subprocess.run(command, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error while running spd-say: {e}")


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


async def speak_text_streaming(text_stream, voice_accent="en", speed=None):
    """
    Asynchronously speak the text in complete sentences from a streaming source.

    Parameters:
        text_stream (async generator): An async generator yielding parts of the text.
        voice_accent (str): The voice accent/language to use. Default is 'en'.
        speed (int, optional): The speed at which to speak. Default is None.
    """
    buffer = ""
    async for part in text_stream:
        buffer += part
        # Check if the buffer contains a complete sentence
        chunks = split_text_to_chunks(buffer)
        if len(chunks) > 1:
            # Speak all but the last incomplete chunk
            for chunk in chunks[:-1]:
                speak_text(chunk, voice_accent=voice_accent, speed=speed)
            # Keep the last incomplete chunk in the buffer
            buffer = chunks[-1]
    # Speak any remaining text in the buffer
    if buffer:
        speak_text(buffer, voice_accent=voice_accent, speed=speed)


# voice.py


async def voice_stream():
    """
    Async generator to pull text messages from the shared queue.
    Waits for new messages to be added to the queue and yields them.
    """
    while True:
        text = await voice_queue.get()  # Wait until a new message is available
        yield text


async def submit_to_voice_queue(message):
    """
    Submit a new message to the voice queue to be spoken.
    """
    await voice_queue.put(message)


async def voice_main():
    """
    Main function to process text streams for speaking from the voice queue.
    """
    await speak_text_streaming(voice_stream(), voice_accent="en", speed=50)


# async def main():
#     """
#     Main function to be used in the server to process text streams for speaking.
#     This should be called by the server to run concurrently with other tasks.
#     Uses the shared voice queue for streaming.
#     """
#     await speak_text_streaming(voice_stream(), voice_accent="en", speed=50)

# Example usage
if __name__ == "__main__":

    async def run():
        async def example_stream():
            for sentence in [
                "Hello, this is a test message.",
                "Please listen carefully.",
                "This is only a test.",
            ]:
                yield sentence
                await asyncio.sleep(1)

        await speak_text_streaming(example_stream(), voice_accent="en", speed=50)

    asyncio.run(run())
