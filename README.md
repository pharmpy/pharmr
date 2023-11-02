<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pharmr)](https://cran.r-project.org/package=pharmr)
[![](https://img.shields.io/github/r-package/v/pharmpy/pharmr?label=github%20version&logo=github)](https://github.com/pharmpy/pharmr)
[![check](https://github.com/pharmpy/pharmr/workflows/check/badge.svg)](https://github.com/pharmpy/pharmr/actions)
<!-- badges: end -->

# ![](https://github.com/pharmpy/pharmpy/blob/master/docs/pharmr_logo.svg)

pharmr is an R package and toolkit for pharmacometrics. It consists of 

* A model abstraction as a foundation for higher level operations
* Functions for
    * manipulation of models
    * extraction of model estimation results
    * manipulation of datasets 
    * various calculations and evaluations
    * running models
    * running higher level workflows or tools

pharmr is implemented as an R wrapper to [**Pharmpy**](https://pharmpy.github.io/).
It provides an R interface to all functions found in the `modeling` and `tools` modules
(documented [here](https://pharmpy.github.io/latest/api.html)). Each
function is also available via the `help`-function (or `?`).


## Installation

### Install from github

pharmr uses the package [**reticulate**](https://rstudio.github.io/reticulate/) for calling 
Python from R. When installing pharmr, reticulate will give a prompt to set up the reticulate 
environment. In order to use pharmr, you need to have Python 3.9, 3.10 or 3.11.

To install pharmr and Pharmpy:

```R
remotes::install_github("pharmpy/pharmr", ref="main")
pharmr::install_pharmpy()
```

Sometimes it is necessary to specify which version of Python to use. To make reticulate use 
Python 3.9 in its environment, run the following: 

```R
Sys.setenv(RETICULATE_MINICONDA_PYTHON_VERSION="3.9")
```

### Install from CRAN

pharmr is available on CRAN and can be installed with

```R
install.packages("pharmr")
pharmr::install_pharmpy()
```

Note that pharmr and Pharmpy are under rapid development and the version on github will be a much more up to date version than the version on CRAN. Check the badges above to see the version numbers for CRAN and github.

## Using pharmr

```R
>>> library(pharmr)
>>> res <- read_modelfit_results("run1.mod")
>>> res$parameter_estimates
  THETA(1)   THETA(2)   THETA(3) OMEGA(1,1) OMEGA(2,2) SIGMA(1,1)
0.00469555 0.98425800 0.15892000 0.02935080 0.02790600 0.01324100
>>> model <- read_model("run1.mod")
>>> model$parameters
       name     value  lower    upper    fix
   THETA(1)  0.004693   0.00  1000000  False
   THETA(2)  1.009160   0.00  1000000  False
   THETA(3)  0.100000  -0.99  1000000  False
 OMEGA(1,1)  0.030963   0.00       oo  False
 OMEGA(2,2)  0.031128   0.00       oo  False
 SIGMA(1,1)  0.013086   0.00       oo  False
>>>
```

A simple example of reading a model, performing a simple transformation, and running the model in NONMEM:

```R
model <- load_example_model('pheno') %>%
  add_peripheral_compartment() %>%
  fit()
```
