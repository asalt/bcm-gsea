import sys, os

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import numpy as np

from python.crud import create_project, get_project, update_project, delete_project
from python.models import Base # only need this for the fixture


@pytest.fixture(scope="function")
def session():
    # engine = create_engine('sqlite:///:memory:')  # Use an in-memory SQLite database for tests
    engine = create_engine(
        # "sqlite:///test_db.db"
        "sqlite:///:memory:"
    )  # Use an in-memory SQLite database for tests
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)()
    yield Session
    Session.close()



def test_create_project(session):
    project = create_project(session, "New Project", "A new project description")
    assert project.id is not None
    assert project.name == "New Project"

def test_read_project(session):
    project = create_project(session, "Existing Project", "Description")
    session.commit()  # Ensure the project is saved and can be retrieved
    retrieved_project = get_project(session, project.id)
    assert retrieved_project == project

def test_update_project(session):
    project = create_project(session, "Old Name", "Old Description")
    updated_project = update_project(session, project.id, new_name="New Name", new_description="New Description")
    assert updated_project.name == "New Name"
    assert updated_project.description == "New Description"

def test_delete_project(session):
    project = create_project(session, "To Delete", "Delete me")
    delete_project(session, project.id)
    assert get_project(session, project.id) is None
