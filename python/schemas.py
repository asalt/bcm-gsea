from enum import Enum
from pydantic import BaseModel, Field


from typing import Optional

# Base Schema for common fields
class ProjectBase(BaseModel):
    name: str
    description: str

# Schema for creation - might include all base fields and any defaults or additional validation
class ProjectCreate(ProjectBase):
    pass

# Schema for updates - might make some fields optional if they're not required for every update
class ProjectUpdate(ProjectBase):
    name: Optional[str] = None
    description: Optional[str] = None

# Full Schema for reading - includes all model fields
class ProjectSchema(ProjectBase):
    id: int
    # created_at: str
    # updated_at: str

    # New configuration style in Pydantic V2
    model_config = {
        "from_attributes": True,  # Equivalent to the deprecated `orm_mode = True`
        "validate_default": True,
    }



## ==============

class RanksBase(BaseModel):
    name: str
    description: str
    feature_id: str
    feature_value: float
    project_id: int
    comparison_id: int

class RanksCreate(RanksBase):
    pass

class RanksUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    feature_id: Optional[str] = None
    feature_value: Optional[float] = None
    project_id: Optional[int] = None
    comparison_id: Optional[int] = None

class RanksSchema(RanksBase):
    id: int

    # New configuration style in Pydantic V2
    model_config = {
        "from_attributes": True,  # Equivalent to the deprecated `orm_mode = True`
        "validate_default": True,
    }

###
class ComparisonBase(BaseModel):
    name: str
    description: str
    project_id: int

class ComparisonCreate(ComparisonBase):
    pass

class ComparisonUpdate(BaseModel):
    description: Optional[str] = None
    project_id: Optional[int] = None

class ComparisonSchema(ComparisonBase):
    id: int

    # New configuration style in Pydantic V2
    model_config = {
        "from_attributes": True,  # Equivalent to the deprecated `orm_mode = True`
        "validate_default": True,
    }

class ResultBase(BaseModel):
    description: str
    geneset: str
    p_value: float
    adj_p_value: float
    ES: float
    NES: float
    size: int
    project_id: int
    comparison_id: int


class ResultCreate(ResultBase):
    pass

class ResultUpdate(BaseModel):
    description: Optional[str] = None
    geneset: Optional[str] = None
    p_value: Optional[float] = None
    adj_p_value: Optional[float] = None
    ES: Optional[float] = None
    NES: Optional[float] = None
    size: Optional[int] = None
    project_id: Optional[int] = None
    comparison_id: Optional[int] = None

class ResultSchema(ResultBase):
    id: int

    # New configuration style in Pydantic V2
    model_config = {
        "from_attributes": True,  # Equivalent to the deprecated `orm_mode = True`
        "validate_default": True,
    }


class EdgeBase(BaseModel):
    name: str
    description: str
    project_id: int
    comparison_id: int
    result_id: int

class EdgeCreate(EdgeBase):
    pass

class EdgeUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    project_id: Optional[int] = None
    comparison_id: Optional[int] = None
    result_id: Optional[int] = None


class EdgeSchema(EdgeBase):
    id: int

    # New configuration style in Pydantic V2
    model_config = {
        "from_attributes": True,  # Equivalent to the deprecated `orm_mode = True`
        "validate_default": True,
    }


