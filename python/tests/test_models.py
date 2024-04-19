import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))



import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import numpy as np
# TODO: fix this import
from python.models import Base, Project, Ranks, Comparison

@pytest.fixture
def session():
    # engine = create_engine('sqlite:///:memory:')  # Use an in-memory SQLite database for tests
    engine = create_engine(
        "sqlite:///test_db.db"
    )  # Use an in-memory SQLite database for tests
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)()
    yield Session
    Session.close()

def test_project(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    assert project.id is not None
    assert project.name == "test_project"
    assert project.description == "test_description"

if __name__ == "__main__":
    # session = session()
    # project = Project(name="test_project", description="test_description")
    # session.add(project)
    # session.commit()
    # print(session.query(Project).all())
    # session.close()
    pass