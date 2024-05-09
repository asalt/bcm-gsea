# db.py
import os
from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from contextlib import contextmanager

from .models import Base

def get_database_url(database_url=None):
    """
    Retrieve the database URL from the environment,
    with a sensible default for production.
    """
    default_db_url = "sqlite:///./prod.db"
    if database_url:
        return database_url
    return os.environ.get('DATABASE_URL', default_db_url)

def init_db(database_url=None, connect_args=None):
    """
    Initialize the database connection and create tables.
    This function should be called explicitly to control when the database is initialized.
    """
    default_connect_args = {"check_same_thread": False}
    if connect_args is None:
        connect_args = default_connect_args
    engine = create_engine(
        get_database_url(database_url),
        connect_args=connect_args  # Necessary for SQLite, adjust if using other databases
    )
    Base.metadata.create_all(bind=engine)
    return engine

def get_session_factory(engine):
    """
    Create and return a scoped session factory.
    """
    session_factory = sessionmaker(autocommit=False, autoflush=False, bind=engine)
    return scoped_session(session_factory)



# Globally accessible session factory (initialized later)
Session = None

def initialize_session():
    """
    Initializes the Session using a freshly initialized engine.
    This should be called explicitly at the application start.
    """
    global Session
    engine = init_db()  # Initialize the database and engine
    Session = get_session_factory(engine)  # Create a session factory


# Function to get a new session
def get_session(engine):
    # Using scoped_session to ensure thread safety in a web app context or similar
    session_factory = sessionmaker(bind=engine)
    Session = scoped_session(session_factory)
    return Session


# Function to provide a global point of access to the session
def get_global_session(db_url):
    engine = init_db(db_url)
    return get_session(engine)

# Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)

# Dependency for FastAPI to use in endpoints
def get_db():
    db = Session()
    try:
        yield db
    finally:
        db.close()
