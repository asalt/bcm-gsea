import sys
import os

# "hack" to be able to import
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import numpy as np

from python.crud import (
    create_project,
    get_project,
    update_project,
    delete_project,
    create_rank,
    delete_rank,
    create_comparison,
    delete_comparison,
    add_many_and_commit,
    add_many_and_commit_dicts,
)
from python.models import Base  # only need this for the fixture
from python.models import Project, Ranks, Comparison, Edge


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
    updated_project = update_project(
        session, project.id, new_name="New Name", new_description="New Description"
    )
    assert updated_project.name == "New Name"
    assert updated_project.description == "New Description"


def test_delete_project(session):
    project = create_project(session, "To Delete", "Delete me")
    delete_project(session, project.id)
    assert get_project(session, project.id) is None


def test_add_many_and_commit(session):
    project = Project(name="test_project", description="test_description")
    add_many_and_commit(project, session)
    assert project.id is not None
    assert project.name == "test_project"
    assert project.description == "test_description"


def test_add_many_and_commit_iterable(session):
    project1 = Project(name="test_project1", description="test_description1")
    project2 = Project(name="test_project2", description="test_description2")
    add_many_and_commit([project1, project2], session)

    assert project1.id is not None
    assert project1.name == "test_project1"
    assert project1.description == "test_description1"
    assert project2.id is not None
    assert project2.name == "test_project2"
    assert project2.description == "test_description2"


def test_add_many_and_commit_invalid(session):
    with pytest.raises(TypeError):
        add_many_and_commit("invalid", session)


def test_add_many_and_commit_more(session):
    project1 = Project(name="test_project1", description="test_description1")
    project2 = Project(name="test_project2", description="test_description2")

    rankobjs = [
        Ranks(
            project_id=project1.id,
            # comparison_id=1,
            name="test_rank1",
            description="test_description1",
            feature_id="feature1",
            feature_value=1.0,
        ),
        Ranks(
            project_id=project1.id,
            # comparison_id=1,
            name="test_rank2",
            description="test_description2",
            feature_id="feature2",
            feature_value=2.0,
        ),
        Ranks(
            project=project1,
            # comparison_id=1,
            name="test_rank1",
            description="test_description3",
            feature_id="feature3",
            feature_value=1.0,
        ),
    ]

    # commit_all_objects([{"project1": project1}, {"project2": project2}], session)
    # assert get_project(session, project1.id) == project1
    # assert get_project(session, project2.id) == project2
    add_many_and_commit(rankobjs, session)


def test_add_many_and_commit_more_dict(session):
    project1 = Project(name="test_project1", description="test_description1")
    project2 = Project(name="test_project2", description="test_description2")

    rankobjs = {
        "rnkobjects": {
            "r1": Ranks(
                project=project1,
                # comparison_id=1,
                name="test_rank1",
                description="test_description1",
                feature_id="feature1",
                feature_value=1.0,
            ),
            "r2": Ranks(
                project_id=project1.id,
                # comparison_id=1,
                name="test_rank2",
                description="test_description2",
                feature_id="feature2",
                feature_value=2.0,
            ),
            "r3": Ranks(
                project=project1,
                # comparison_id=1,
                name="test_rank1",
                description="test_description3",
                feature_id="feature3",
                feature_value=1.0,
            ),
        }
    }

    # commit_all_objects([{"project1": project1}, {"project2": project2}], session)
    # assert get_project(session, project1.id) == project1
    # assert get_project(session, project2.id) == project2
    add_many_and_commit_dicts(rankobjs, session)

    assert get_project(session, project1.id) == project1
    assert (
        get_project(session, project2.id) == None
    )  # none because no rank objects were made with project2


def test_add_many_and_commit_more_dict(session):
    project1 = Project(name="test_project1", description="test_description1")
    project2 = Project(name="test_project2", description="test_description2")

    rankobjs = {
        "rnkobjects": {
            "r1": Ranks(
                project=project1,
                # comparison_id=1,
                name="test_rank1",
                description="test_description1",
                feature_id="feature1",
                feature_value=1.0,
            ),
            "r2": Ranks(
                project=project1,
                # comparison_id=1,
                name="test_rank2",
                description="test_description2",
                feature_id="feature2",
                feature_value=2.0,
            ),
            "r3": Ranks(
                project=project2,
                # comparison_id=1,
                name="test_rank1",
                description="test_description3",
                feature_id="feature3",
                feature_value=1.0,
            ),
        }
    }

    add_many_and_commit_dicts(rankobjs, session)
    assert project1.id is not None
    assert project2.id is not None
    assert get_project(session, project1.id) == project1
    assert get_project(session, project2.id) == project2


def test_proj_rank_misc(session):
    # there is some model testing logic that could be moved out
    project = Project(name="test_project", description="test_description")
    comparison1 = Comparison(
        name="test_comparison1", description="test_description1", project=project
    )
    comparison2 = Comparison(
        name="test_comparison2", description="test_description2", project=project
    )
    rankobjs = [
        Ranks(
            project=project,
            comparison=comparison1,
            name="test_rank1",
            description="test_description1",
            feature_id="feature1",
            feature_value=x,
        )
        for x in np.random.rand(10)
    ]
    add_many_and_commit(rankobjs, session)
    # this is all already tested in test_models.py
    assert project.id is not None
    assert project.ranks[0].id is not None
    assert comparison1.id is not None
    assert project.comparisons[0].id is not None
    assert len(project.comparisons) == 2
    assert len(comparison2.ranks) == 0
    assert len(project.ranks) > 0

    delete_comparison(session, comparison1.id)

    assert len(project.comparisons) == 1
    assert len(project.ranks) == 0
