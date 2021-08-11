### How to build an R package from scratch

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


# author <- list(name = "Hauke Sonnenberg",
#                orcid = "0000-0001-9134-2871",
#                url = "https://github.com/hsonne")

author <- list(name = "Roberto Tatis-Muvdi",
               orcid = "0000-0003-0490-7999",
               url = "https://github.com/robetatis")

pkg <- list(name = "urbanAnnualRunoff",
            title = "R Package for Deriving Urban Surfaces for Storm Runoff
            Analysis",
            desc  = paste("Used in Project KEYS for generating inputs to runoff
                          model ABIMO for application in cities with data scarcity"))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("Workflow")

### R functions
pkg_dependencies <- c("dplyr", "foreign", "fs", "lubridate",
                      "kwb.utils", "rlang", "raster", "rgdal", "remotes")

sapply(pkg_dependencies, usethis::use_package)

desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)

usethis::use_pipe()

kwb.pkgbuild::use_autopkgdown()

kwb.pkgbuild::create_empty_branch_ghpages("urbanAnnualRunoff")
