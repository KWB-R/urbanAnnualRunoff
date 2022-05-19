[![R-CMD-check](https://github.com/KWB-R/urbanAnnualRunoff/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/urbanAnnualRunoff/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/urbanAnnualRunoff/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/urbanAnnualRunoff/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/urbanAnnualRunoff/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/urbanAnnualRunoff)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/urbanAnnualRunoff)]()
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.6561481.svg)](https://doi.org/10.5281/zenodo.6561481)

**R Package for Deriving Urban Surfaces for Storm Runoff 
Analysis**

Used in Project KEYS for generating inputs to
runoff model ABIMO for application in cities with data scarcity.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install keys.lid in R
install.packages('urbanAnnualRunoff')

# Browse the keys.lid manual pages
help(package = 'urbanAnnualRunoff')
```

## Workflow

The workflow for modelling urban annual runoff using the water balance model [ABIMO](https://github.com/umweltatlas/abimo) is described for the 
two case-study sites in China here: 

- [Jinxi](articles/workflow_jinxi.html) and 

- [Tongzhou (Beijing)](articles/workflow_beijing.html)


