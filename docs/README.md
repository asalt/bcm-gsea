# gsea-web

Entire analysis driven by run.Rmd
` Rscript -e "rmarkdown::render('run.Rmd')"   `

`R`:
  - data loading
  - analysis
  - results routing


`python` :
  - database management
  - API hosting
  - results storage in db


test everything with test.sh (no .bat file yet)
python testing with `pytest ./python/tests/`
R tests located in `./R/tests/` using `testthat`, mostly
test coverage << 100%