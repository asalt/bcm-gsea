# api.py
from typing import List, Optional, Type
from fastapi import Depends, FastAPI, HTTPException
from fastapi import BackgroundTasks
from fastapi.responses import HTMLResponse

from sqlalchemy.orm import Session
from pydantic import BaseModel

from .models import Base, Project, Ranks, Comparison, Edge

# Generic CRUD Model Wrapper for things like delete, update, get, get all. Will need 'niche' SQL queries for other things like search, aggregation, dynamic filtering, etc.
from .crud import CRUDBase

from .db import get_db

from fastapi import FastAPI

app = FastAPI()
# some basic endpoints defined here

@app.get("/")
async def root():
    return {"msg": "Hello World"}


#

#

## old func
# def register_crud_routes(
#     app: FastAPI, Model: Type[Base], model_schema: Type[BaseModel]
# ):
#     crud_helper = CRUDBase(Model)
#     type_name = Model.__tablename__

#     @app.post(f"/{type_name}", response_model=model_schema)
#     def create_entity(payload: model_schema, db: Session = Depends(get_db)):
#         return crud_helper.create(db, obj_in=payload)

#     @app.get(f"/{type_name}", response_model=List[model_schema])
#     def get_all(db: Session = Depends(get_db)):
#         return crud_helper.get_all(db)

#     @app.get(f"/{type_name}/{{id}}", response_model=model_schema)
#     def get_entity(id: int, db: Session = Depends(get_db)):
#         return crud_helper.get(db, id)

#     @app.put(f"/{type_name}/{{id}}", response_model=model_schema)
#     def update_entity(id: int, payload: model_schema, db: Session = Depends(get_db)):
#         db_obj = crud_helper.get(db, id)
#         return crud_helper.update(db, db_obj=db_obj, obj_in=payload)

#     @app.delete(f"/{type_name}/{{id}}", response_model=model_schema)
#     def delete_entity(id: int, db: Session = Depends(get_db)):
#         return crud_helper.delete(db, id=id)


# this new version includes the schema for create, read, and update
def register_crud_routes(
    app: FastAPI,
    Model: Type[Base],
    create_schema: Type[BaseModel],
    read_schema: Type[BaseModel],
    update_schema: Type[BaseModel],
):
    crud_helper = CRUDBase(Model)
    type_name = Model.__tablename__

    @app.post(f"/{type_name}/", response_model=read_schema)
    async def create_entity(entity: create_schema, db: Session = Depends(get_db)):
        return crud_helper.create(db, obj_in=entity)

    @app.get(f"/{type_name}/", response_model=List[read_schema])
    async def get_all(db: Session = Depends(get_db)):
        return crud_helper.get_all(db)

    @app.get(f"/{type_name}/{{id}}", response_model=read_schema)
    async def get_entity(id: int, db: Session = Depends(get_db)):
        return crud_helper.get(db, id)

    @app.put(f"/{type_name}/{{id}}", response_model=read_schema)
    async def update_entity(id: int, entity: update_schema, db: Session = Depends(get_db)):
        db_obj = crud_helper.get(db, id)
        if not db_obj:
            raise HTTPException(status_code=404, detail="Entity not found")
        return crud_helper.update(db, db_obj=db_obj, obj_in=entity)

    @app.delete(f"/{type_name}/{{id}}", response_model=read_schema)
    async def delete_entity(id: int, db: Session = Depends(get_db)):
        return crud_helper.delete(db, id=id)


from .schemas import (
    ProjectCreate, ProjectSchema, ProjectUpdate,
    RanksCreate, RanksSchema, RanksUpdate,
    ComparisonCreate, ComparisonSchema, ComparisonUpdate,
    EdgeCreate, EdgeSchema, EdgeUpdate
)

# Register CRUD routes for each model with specific schemas for create, read, and update
register_crud_routes(app, Project, create_schema=ProjectCreate, read_schema=ProjectSchema, update_schema=ProjectUpdate)
register_crud_routes(app, Ranks, create_schema=RanksCreate, read_schema=RanksSchema, update_schema=RanksUpdate)
register_crud_routes(app, Comparison, create_schema=ComparisonCreate, read_schema=ComparisonSchema, update_schema=ComparisonUpdate)
register_crud_routes(app, Edge, create_schema=EdgeCreate, read_schema=EdgeSchema, update_schema=EdgeUpdate)