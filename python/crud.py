# crud.py

from .db import get_session
from .models import Project, Ranks, Comparison, Edge

def create_project(session, name, description):
    project = Project(name=name, description=description)
    session.add(project)
    session.commit()
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
