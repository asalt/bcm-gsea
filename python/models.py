from sqlalchemy import (
    create_engine,
    Column,
    Integer,
    Float,
    String,
    ForeignKey,
    JSON,
    LargeBinary,
)
from sqlalchemy.orm import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.orm import relationship

Base = declarative_base()


class Project(Base):
    __tablename__ = "projects"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    description = Column(String)

    edges = relationship("Edge", back_populates="project")
    # created_at = Column(String)
    # updated_at = Column(String)
    # user_id = Column(Integer, ForeignKey("users.id"))
    # user = relationship("User", back_populates="projects")
    # models = relationship("Model", back_populates="project")


class Ranks(Base):
    __tablename__ = "ranks"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    description = Column(String)

    feature_id = Column(String)
    feature_value = Column(Float)



    project_id = Column(Integer, ForeignKey("projects.id"))
    project = relationship("Project", back_populates="ranks")

    comparison_id = Column(Integer, ForeignKey("comparisons.id"))


    # user_id = Column(Integer, ForeignKey("users.id"))
    # user = relationship("User", back_populates="projects")
    # models = relationship("Model", back_populates="project")


class Comparison(Base):
    __tablename__ = "comparisons"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    description = Column(String)

    project_id = Column(Integer, ForeignKey("projects.id"))
    project = relationship("Project", back_populates="comparisons")

    rank_id = Column(Integer, ForeignKey("ranks.id"))
    ranks = relationship("Ranks", back_populates="comparisons")

    edges = relationship("Edge", back_populates="comparison")


class Edge(Base):
    __tablename__ = "edges"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    description = Column(String)

    project_id = Column(Integer, ForeignKey("projects.id"))
    project = relationship("Project", back_populates="edges")

    comparison_id = Column(Integer, ForeignKey("comparisons.id"))
    comparison = relationship("Comparison", back_populates="edges")
