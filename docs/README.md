# gsea-web


##  Intro

This is is a versatile tool for running (preranked) Gene Set Enrichment Analysis (GSEA) across combinations of ranks and gene sets.
It mainly consists of a series of wrapper functions to perform the analysis and sift through the results.


clone the repository:
```bash
git clone https://github.com/asalt/bcm-gsea
```


## Features
R Analysis: The main analysis is performed using R, with fgsea for fast gene set enrichment.
Python Integration: Python manages the database, hosts APIs, and stores results.
  This component is still under development.
Testing: Comprehensive testing using pytest for Python and testthat for R.
Visualization: Utilizes ggplot2 for high-quality plots.

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

## Modules

There are a variety of modules to facilitate data loading, geneset retrievement, formatting/wrangling, and more.
The following are descriptions of the available modules loosly in order for running a new analysis

### io.R

Currently there are two options to input data.
First is a tsv file containing statistics regarding a comparison - e.g. t test between two groups.

The function that handles this datasource is:

```
create_rnkfiles_from_volcano
```

After the identifier and value are properly loaded, a rankfile is saved locally for future use.
Identifier currently is expected to be Entrez GeneID.
Value currently expected to be a zero centered quantitative value about the comparison.
Good choices of value are signedlogp value or logFoldChange

Second, for the purpose of ssGSEA, takes gct file format as input.
Each sample (cid) is separated into a separate rank file.
An option for scaling by zscore is available if data are not zero centered.
Generally expected for data to be approximately normally distributed.
Note: setting the GSEA paramater `p` to `0` should allow for proper analysis of non-normalized data, though explicit control over this parameter is not yet available.

## geneset_utils.R

Facitiates fetching of publically available genesets using `msigdbr`.
Caches fetched genesets to a local file.
Support for multiple species is available through `msigdbr`.

### fgsea.R

Execution performed by `run_one` wrapper around `fgsea::fgsea`.
Also has an option to "collapse" pathways to reduce redundancy.
This is performed with `fgsea::collapsePathways`.
No filtering is performed at this stage; if `collapse == TRUE` redundant pathways will be calculated and indicated.

### plot.R



## Citations
To cite package ‘fgsea’ in publications use:

  G. Korotkevich, V. Sukhov, A. Sergushichev. Fast gene set enrichment analysis. bioRxiv (2019), doi:10.1101/060012

```bibtex
  @Article{,
    author = {Gennady Korotkevich and Vladimir Sukhov and Alexey Sergushichev},
    title = {Fast gene set enrichment analysis},
    year = {2019},
    doi = {10.1101/060012},
    publisher = {Cold Spring Harbor Labs Journals},
    url = {http://biorxiv.org/content/early/2016/06/20/060012},
    journal = {bioRxiv},
  }
```


To cite package ‘msigdbr’ in publications use:

  Dolgalev I (2022). _msigdbr: MSigDB Gene Sets for Multiple Organisms in a Tidy Data Format_. R package version 7.5.1, <https://CRAN.R-project.org/package=msigdbr>.

```bibtex

  @Manual{,
    title = {msigdbr: MSigDB Gene Sets for Multiple Organisms in a Tidy Data Format},
    author = {Igor Dolgalev},
    year = {2022},
    note = {R package version 7.5.1},
    url = {https://CRAN.R-project.org/package=msigdbr},
  }
```

To cite ggplot2 in publications, please use

  H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.


```bibtex
  @Book{,
    author = {Hadley Wickham},
    title = {ggplot2: Elegant Graphics for Data Analysis},
    publisher = {Springer-Verlag New York},
    year = {2016},
    isbn = {978-3-319-24277-4},
    url = {https://ggplot2.tidyverse.org},
  }
```

To cite package ‘purrr’ in publications use:

  Wickham H, Henry L (2023). _purrr: Functional Programming Tools_. R package version 1.0.2, <https://CRAN.R-project.org/package=purrr>.

```bibtex

@Manual{,
    title = {purrr: Functional Programming Tools},
    author = {Hadley Wickham and Lionel Henry},
    year = {2023},
    note = {R package version 1.0.2},
    url = {https://CRAN.R-project.org/package=purrr},
  }

```


