from pydantic import ValidationError
import pytest
from python.schemas import RanksCreate # python is the name of the folder

def test_ranks_create():
    # Test successful creation
    payload = {
        "name": "Rank 1",
        "description": "A test rank",
        "feature_id": "F100",
        "feature_value": 1.5,
        "project_id": 1,
        "comparison_id": 2
    }

    rank = RanksCreate(**payload)
    assert rank.name == "Rank 1"

    # Test validation error
    with pytest.raises(ValidationError):
        payload = {
            "name": "Rank 1",
            "description": "A test rank",
            "feature_id": "F100",
            "feature_value": "should be float",  # Incorrect type
            "project_id": 1,
            "comparison_id": 2
        }
        rank = RanksCreate(**payload)
