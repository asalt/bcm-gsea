#!/bin/bash

echo "Running Python tests"
pytest ./python/tests/
# ========================

# echo "Running R tests"
# for f in ./R/tests/test_*R; do
#     cd ./R/tests/
#     echo "Running $f"
#     Rscript $f
#     cd ../../
# done

# Running R tests
echo "Running R tests..."
for f in ./R/tests/test_*R; do
    echo "Running $f..."
    (
        cd $(dirname $f)       # Switch to the directory where the test file is located
        Rscript $(basename $f) # Run the R script
    )
done
