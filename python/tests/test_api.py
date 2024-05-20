# test_api.py
import os
import sys

# "hack" to be able to import
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))


from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from python.api import app  # Import your FastAPI app
from python.db import init_db, get_session, get_db
import pytest

# Configure your database for testing (use a test database if possible)
SQLALCHEMY_DATABASE_URL = "sqlite:///./test.db"

engine = init_db(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})

TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


# Dependency override to use test database
def override_get_db():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()


app.dependency_overrides[get_db] = override_get_db


@pytest.fixture(scope="module")
def client():
    with TestClient(app) as c:
        yield c


# Test to check if API is up and running
def test_read_main(client):
    response = client.get("/")
    assert response.status_code == 200
    assert response.json() == {"msg": "Hello World"}


def test_create_project(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "New Project"
    assert "id" in data  # Check if an ID was assigned


def test_read_project(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    data = response.json()
    project_id = data["id"]

    response = client.get(f"/projects/{project_id}")
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "New Project"
    assert data["description"] == "A test project"


def test_update_project(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    data = response.json()
    project_id = data["id"]

    response = client.put(
        f"/projects/{project_id}",
        json={"name": "Updated Project", "description": "An updated project"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Updated Project"
    assert data["description"] == "An updated project"


def xx_test_delete_project(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    data = response.json()
    project_id = data["id"]

    # import pdb; pdb.set_trace()
    response = client.delete(f"/projects/{project_id}")
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "New Project"
    assert data["description"] == "A test project"

    response = client.get(f"/projects/{project_id}")
    assert response.status_code == 404
    assert response.json() == {"detail": "Project not found"}


def test_create_comparison(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    data = response.json()
    project_id = data["id"]

    response = client.post(
        "/comparisons/",
        json={
            "project_id": project_id,
            "name": "Comparison 1",
            "description": "A test comparison",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Comparison 1"
    assert data["project_id"] == project_id


def test_create_rank(client):
    response = client.post(
        "/projects/", json={"name": "New Project", "description": "A test project"}
    )
    assert response.status_code == 200
    data = response.json()
    project_id = data["id"]

    response = client.post(
        "/comparisons/",
        json={
            "project_id": project_id,
            "name": "Comparison 1",
            "description": "A test comparison",
        },
    )
    assert response.status_code == 200
    data = response.json()
    comparison_id = data["id"]

    response = client.post(
        "/ranks/",
        json={
            "project_id": project_id,
            "comparison_id": comparison_id,
            "name": "Rank 1",
            "description": "A test rank",
            "feature_id": "F100",
            "feature_value": 1.5,
            "comparison_id": 2,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Rank 1"
    assert data["project_id"] == project_id
