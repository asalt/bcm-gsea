# test_db.py
import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from python.db import init_db, get_session


ADDRESS = "sqlite3:///:memory:"
ADDRESS = "sqlite:///test_db.db"


def test_init_db():
    ADDRESS = locals().get("ADDRESS", "sqlite:///:memory:")
    engine = init_db(ADDRESS)
    assert engine is not None


def test_get_session():
    ADDRESS = locals().get("ADDRESS", "sqlite:///:memory:")
    engine = create_engine(ADDRESS)
    session = get_session(engine)
    assert session is not None
    assert session.bind == engine
    session.close()


def test_get_session_with_engine():
    ADDRESS = locals().get("ADDRESS", "sqlite:///:memory:")
    engine = create_engine(ADDRESS)
    session = get_session(engine)
    assert session is not None
    assert session.bind == engine
    session.close()