# crud.py
import collections

from .db import get_session
from .models import Base, Project, Ranks, Comparison, Edge

def add_and_commit(session, obj):
    session.add(obj)
    session.commit()
    session.refresh(obj)
    return obj


def add_many_and_commit(obj, session):
    """
    Recursively adds objects to the SQLAlchemy session. Handles single Base instances or any iterable collections of them.

    Args:
    obj: The object or collection of objects to add to the session.
    session: The SQLAlchemy session instance to which objects will be added.
    """
    if isinstance(obj, Base):
        session.add(obj)
    elif isinstance(obj, collections.abc.Iterable) and not isinstance(obj, (str, bytes)):
        for item in obj:
            add_many_and_commit(item, session)  # Recursive call
    else:
        raise TypeError(f"Object of type {type(obj).__name__} is not addable to the session")

    session.commit()


def add_many_and_commit_dicts(obj, session):
    """
    Recursively adds objects to the SQLAlchemy session. Handles single Base instances or any iterable collection key value pair.

    Args:
    obj: The object or collection of objects to add to the session.
    session: The SQLAlchemy session instance to which objects will be added.
    """
    if isinstance(obj, Base):
        session.add(obj)
    elif isinstance(obj, collections.abc.Iterable) and not isinstance(obj, (str, bytes)):
        if not hasattr(obj, "items"):
            # TODO can just warn
            raise TypeError(f"Object of type {type(obj).__name__} is not addable to the session")
        for name, item in obj.items():
            add_many_and_commit_dicts(item, session)  # Recursive call
    else:
        raise TypeError(f"Object of type {type(obj).__name__} is not addable to the session")

    session.commit()



def create_project(session, name, description):
    project = Project(name=name, description=description)
    add_and_commit(session, project)
    return project

def get_project(session, project_id):
    project = session.query(Project).filter(Project.id == project_id).one_or_none()
    return project

def update_project(session, project_id, new_name=None, new_description=None):
    project = session.query(Project).filter(Project.id == project_id).one()
    if new_name:
        project.name = new_name
    if new_description:
        project.description = new_description
    session.commit()
    return project

def delete_project(session, project_id):
    project = session.query(Project).filter(Project.id == project_id).one()
    session.delete(project)
    session.commit()

def create_rank(session, project_id, comparison_id, name, description, feature_id, feature_value):
    rank = Ranks(
        project_id=project_id,
        comparison_id=comparison_id,
        name=name,
        description=description,
        feature_id=feature_id,
        feature_value=feature_value
    )
    session.add(rank)
    session.commit()
    return rank


def delete_rank(session, rank_id):
    rank = session.query(Ranks).filter(Ranks.id == rank_id).one()
    session.delete(rank)
    session.commit()

def create_comparison(session, project_id, name, description):
    comparison = Comparison(
        project_id=project_id,
        name=name,
        description=description
    )
    session.add(comparison)
    session.commit()
    return comparison


def delete_comparison(session, comparison_id):
    comparison = session.query(Comparison).filter(Comparison.id == comparison_id).one()
    session.delete(comparison)
    session.commit()