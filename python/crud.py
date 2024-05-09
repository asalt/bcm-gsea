# crud.py
import collections

from typing import Optional, Any, Type, TypeVar, Generic, List, Union, Dict
from fastapi.encoders import jsonable_encoder

from sqlmodel import Session
from sqlalchemy import select


from .db import get_session
from .models import Base, Project, Ranks, Comparison, Edge

# leave query alone please just leave it

# crud.py
class CRUDBase:
    def __init__(self, model):
        self.model = model

    def get(self, db: Session, id:Any):
        # stmt = select(self.model).where(self.model.id == id)
        # db.execute(stmt) # this is also being depreciated??/
        # db.exec does not exist. Session does not (yet) have exec method.
        #
        return db.query(self.model).filter(self.model.id == id).first()

    def get_all(self,db: Session):
        return db.query(self.model).all()

    def create(self, db: Session, *,obj_in):
        obj_in_data = jsonable_encoder(obj_in)
        db_obj = self.model(**obj_in_data)
        db.add(db_obj)
        db.commit()
        db.refresh(db_obj)
        return db_obj

    def delete(self, db: Session, *, id: int):
        db_obj = db.query(self.model).get(id)
        db.delete(db_obj)
        db.commit()
        return db_obj

    def update( self, db: Session, *, db_obj, obj_in):
        db_obj_data = jsonable_encoder(db_obj)
        if isinstance(obj_in, dict):
            update_dict = obj_in
        else:
            update_dict = obj_in.dict(exclude_unset=True)

        for field in db_obj_data:
            if field in update_dict:
                setattr(db_obj, field, update_dict[field])
        db.add(db_obj)
        db.commit()
        db.refresh(db_obj)
        return db_obj




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