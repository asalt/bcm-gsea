import asyncpg

# Database connection function
async def create_db_pool():
    return await asyncpg.create_pool(dsn="your_database_url_here")

# In your main function:
db_pool = await create_db_pool()

# Example of a database operation
async def save_conversation(db_pool, context):
    async with db_pool.acquire() as conn:
        await conn.execute(
            "INSERT INTO conversations (context) VALUES ($1)",
            "\n".join(context)
        )

# Update your handle_log_entry function:
async def handle_log_entry(websocket, prompt, db_pool):
    conversation_context.append(prompt)
    await save_conversation(db_pool, conversation_context)
    # ... rest of your function ...

# Don't forget to pass db_pool to your handler functions
