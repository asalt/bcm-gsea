import sys, os

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import numpy as np

from python.models import Base, Project, Ranks, Comparison, Edge


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


def test_project(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    assert project.id is not None
    assert project.name == "test_project"
    assert project.description == "test_description"


def test_ranks(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    ranks = Ranks(
        name="test_ranks", description="test_description", project_id=project.id
    )
    session.add(ranks)
    session.commit()
    assert ranks.id is not None
    assert ranks.name == "test_ranks"
    assert ranks.description == "test_description"
    assert ranks.project_id == project.id


def test_comparison(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    comparison = Comparison(
        name="test_comparison", description="test_description", project_id=project.id
    )
    session.add(comparison)
    session.commit()
    assert comparison.id is not None
    assert comparison.name == "test_comparison"
    assert comparison.description == "test_description"
    assert comparison.project_id == project.id


def test_project_ranks(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    ranks = Ranks(
        name="test_ranks", description="test_description", project_id=project.id
    )
    session.add(ranks)
    session.commit()
    assert project.ranks[0].id == ranks.id
    assert project.ranks[0].name == ranks.name
    assert project.ranks[0].description == ranks.description
    assert project.ranks[0].project_id == ranks.project_id


def test_project_comparison(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    comparison = Comparison(
        name="test_comparison", description="test_description", project_id=project.id
    )
    session.add(comparison)
    session.commit()
    assert project.comparisons[0].id == comparison.id
    assert project.comparisons[0].name == comparison.name
    assert project.comparisons[0].description == comparison.description
    assert project.comparisons[0].project_id == comparison.project_id


def test_comparison_ranks(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    comparison = Comparison(
        name="test_comparison", description="test_description", project_id=project.id
    )
    session.add(comparison)
    session.commit()
    ranks = Ranks(
        name="test_ranks",
        description="test_description",
        project_id=project.id,
        comparison_id=comparison.id,
    )
    session.add(ranks)
    session.commit()
    assert comparison.ranks[0].id == ranks.id
    assert comparison.ranks[0].name == ranks.name
    assert comparison.ranks[0].description == ranks.description
    assert comparison.ranks[0].project_id == ranks.project_id
    assert comparison.ranks[0].comparison_id == ranks.comparison_id


def test_project_edges(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    edge = Edge(name="test_edge", description="test_description", project_id=project.id)
    session.add(edge)
    session.commit()
    assert project.edges[0].id == edge.id
    assert project.edges[0].name == edge.name
    assert project.edges[0].description == edge.description
    assert project.edges[0].project_id == edge.project_id


def test_comparison_edges(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    comparison = Comparison(
        name="test_comparison", description="test_description", project_id=project.id
    )
    session.add(comparison)
    session.commit()
    edge = Edge(
        name="test_edge",
        description="test_description",
        project_id=project.id,
        comparison_id=comparison.id,
    )
    session.add(edge)
    session.commit()
    assert comparison.edges[0].id == edge.id
    assert comparison.edges[0].name == edge.name
    assert comparison.edges[0].description == edge.description
    assert comparison.edges[0].project_id == edge.project_id
    assert comparison.edges[0].comparison_id == edge.comparison_id


def test_project_edges(session):
    project = Project(name="test_project", description="test_description")
    session.add(project)
    session.commit()
    edge = Edge(name="test_edge", description="test_description", project_id=project.id)
    session.add(edge)
    session.commit()
    assert project.edges[0].id == edge.id
    assert project.edges[0].name == edge.name
    assert project.edges[0].description == edge.description
    assert project.edges[0].project_id == edge.project_id


def test_comparison_deletion_cascade_query(session):
    """
    this approach is nice because it checks for explicit deletion via query
    """
    project = Project(name="test_project", description="test_description")
    comparison = Comparison(
        name="test_comparison", description="test_description", project=project
    )
    rank = Ranks(
        name="test_rank",
        description="test_description",
        project=project,
        comparison=comparison,
    )
    session.add_all([project, comparison, rank])
    session.commit()

    # Check initial conditions
    assert session.query(Ranks).filter_by(id=rank.id).one_or_none() is not None

    # Delete comparison and commit to test cascade effects
    session.delete(comparison)
    session.commit()
    # session.expire_all() # this shouldn't be necessary, I think


    # Check if rank has been deleted or updated
    refreshed_rank = session.query(Ranks).filter_by(id=rank.id).one_or_none()
    assert (
        refreshed_rank is None
    )  # This line will pass if CASCADE DELETE is set up correctly



def test_comparison_deletion_cascade_pyobjects(session):
    """
    basically a redundant test of test_comparison_deletion_cascade_query,
    but with a different approach to check if the rank object has been deleted
    """
    project = Project(name="test_project", description="test_description")
    comparison = Comparison(
        name="test_comparison", description="test_description", project=project
    )
    rank = Ranks(
        name="test_rank",
        description="test_description",
        project=project,
        comparison=comparison,
    )
    session.add_all([project, comparison, rank])
    session.commit()

    # == Check initial conditions ==
    assert rank in session

    # Delete comparison and commit to test cascade effects
    session.delete(comparison)
    session.commit()

    # Check if rank has been deleted or updated
    assert rank not in session

    # # Use SQLAlchemy inspect to check if rank is still present
    # inspector = inspect(rank)
    # assert inspector.deleted  # This assertion checks if the rank object has been marked as deleted
    # this doesn't work
    # inspector.deleted is False: This means that from the perspective of the current session's public API, the object is not considered deleted. That is, it hasn't been flushed from the session in a way that marks it as being removed from the database permanently during that session's transaction.
    # inspector.__dict__['_deleted'] is True: This indicates that internally (within the lower-level mechanics of SQLAlchemy's session and transaction management), the object has been marked for deletion. This usually happens after a delete() operation has been issued but before the session has been flushed or committed.


def test_complex_project_relationships(session):
    project = Project(
        name="Complex Project",
        description="A project with multiple comparisons and ranks",
    )
    session.add(project)
    session.commit()

    comparison1 = Comparison(
        name="Comparison 1", description="First comparison", project=project
    )
    comparison2 = Comparison(
        name="Comparison 2", description="Second comparison", project=project
    )
    session.add_all([comparison1, comparison2])
    session.commit()

    rank1 = Ranks(
        name="Rank 1",
        feature_id="F1",
        feature_value=1.0,
        project=project,
        comparison=comparison1,
    )
    rank2 = Ranks(
        name="Rank 2",
        feature_id="F2",
        feature_value=2.0,
        project=project,
        comparison=comparison2,
    )
    session.add_all([rank1, rank2])
    session.commit()

    assert len(project.comparisons) == 2
    assert project.comparisons[1].name == "Comparison 2"
    assert len(comparison1.ranks) == 1
    assert rank1 in comparison1.ranks
    assert rank2.project_id == project.id
    assert rank2.comparison.description == "Second comparison"

    # Testing deletion and backref integrity
    # session.delete(comparison2)
    session.delete(comparison2)
    session.commit()
    assert rank2 not in session  # Assuming CASCADE delete or SET NULL with handling


if __name__ == "__main__":
    # os.system("pytest -s test_models.py")
    os.system(f"pytest -s {__file__}")
