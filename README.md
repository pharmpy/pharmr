<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pharmr)](https://cran.r-project.org/package=pharmr)
[![](https://img.shields.io/github/r-package/v/pharmpy/pharmr?label=github%20version&logo=github)](https://github.com/pharmpy/pharmr)
[![R-CMD-check](https://github.com/pharmpy/pharmr/workflows/R-CMD-check/badge.svg)](https://github.com/pharmpy/pharmr/actions)
<!-- badges: end -->

# pharmr

pharmr is an R package and toolkit for pharmacometrics. It consists of 

* A model abstraction as a foundation for higher level operations
* Functions for
    * manipulation of models
    * extraction of results
    * manipulation of datasets 
    * various calculations and evaluations
    * running models
    * running higher level workflows or tools

pharmr is implemented as an R wrapper to [**Pharmpy**](https://pharmpy.github.io/), a Python library for 
pharmacometrics. It provides an R interface to all functions found in the `modeling` and `tools` modules 
(documented [here](https://pharmpy.github.io/latest/reference/pharmpy.modeling.html)). Each
function is also available via the `help`-function (or `?`).


## Installation

### Install from github

pharmr uses the package [**reticulate**](https://rstudio.github.io/reticulate/) for calling 
Python from R. When installing pharmr, reticulate will give a prompt to set up the reticulate 
environment. In order to use pharmr, you need to have Python 3.8 or 3.9. To make reticulate use 
Python 3.9 in its environment, run the following: 

```R
Sys.setenv(RETICULATE_MINICONDA_PYTHON_VERSION="3.9")
```

Then install pharmr and Pharmpy:

```R
remotes::install_github("pharmpy/pharmr", ref="main")
pharmr::install_pharmpy()
```

### Install from CRAN

pharmr is available on CRAN and can be installed with

```R
install.packages("pharmr")
pharmr::install_pharmpy()
```

Note that pharmr and Pharmpy are under rapid development and the version on github will be a much more up to date version than the version on CRAN. Check the badges above to see the version numbers for CRAN and github.

## Using pharmr

A simple example of reading a model, performing a simple transformation, and run the model in NONMEM:

```R
model <- load_example_model('pheno') %>%
  add_peripheral_compartment() %>%
  fit()
```

Note: If you try to access data frames belonging to a Pharmpy object you often need to reset the index. All functions available in pharmr do this automatically, it is only when you have data frames nested in objects (such as a model object) that you need to do this. An example:

```R
model <- load_example_model('pheno')
residuals <- reset_index(model$modelfit_results$residuals)
```
