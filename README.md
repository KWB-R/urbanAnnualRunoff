[![R-CMD-check](https://github.com/KWB-R/urbanAnnualRunoff/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/urbanAnnualRunoff/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/urbanAnnualRunoff/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/urbanAnnualRunoff/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/urbanAnnualRunoff/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/urbanAnnualRunoff)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/urbanAnnualRunoff)]()
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.6561481.svg)](https://doi.org/10.5281/zenodo.6561481)

# urbanAnnualRunoff

**R Package for Deriving Urban Surfaces for Storm Runoff 
Analysis**

Used in Project [KEYS](https://www.kompetenz-wasser.de/en/forschung/projekte/keys) for generating inputs to runoff model [ABIMO](https://github.com/umweltatlas/abimo) for application in cities with data scarcity.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'urbanAnnualRunoff' from GitHub
remotes::install_github("KWB-R/urbanAnnualRunoff")
```

## Documentation

Release: [https://kwb-r.github.io/urbanAnnualRunoff](https://kwb-r.github.io/urbanAnnualRunoff)

Development: [https://kwb-r.github.io/urbanAnnualRunoff/dev](https://kwb-r.github.io/urbanAnnualRunoff/dev)
