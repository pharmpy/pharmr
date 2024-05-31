<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pharmr)](https://cran.r-project.org/package=pharmr)
[![](https://img.shields.io/github/r-package/v/pharmpy/pharmr?label=github%20version&logo=github)](https://github.com/pharmpy/pharmr)
[![check](https://github.com/pharmpy/pharmr/workflows/check.yaml/badge.svg)](https://github.com/pharmpy/pharmr/actions)
<!-- badges: end -->

# ![](https://github.com/pharmpy/pharmpy/blob/main/docs/pharmr_logo.svg)

pharmr is an R wrapper to [**Pharmpy**](https://github.com/pharmpy/pharmpy): an open-source software package for 
pharmacometric modeling. It provides an R interface to all functions found in the `modeling` and `tools` modules
(documented [here](https://pharmpy.github.io/latest/api.html)). The documentation for each function is also available via the `help`-function (or `?`). 

For comprehensive information and documentation, see: https://pharmpy.github.io

## Installation and setup

### Install from CRAN

The latest stable version of pharmr is available via CRAN, and can be installed via:

```R
install.packages("pharmr")
```

After installing pharmr, Pharmpy needs to be set up:

```R
pharmr::install_pharmpy()
```

For a more comprehensive description of the process of setting up, see the [following guide](https://pharmpy.github.io/latest/using_r.html).

### Install from GitHub

It is also possible to install the development version of pharmr from GitHub, with the following command:

```R
remotes::install_github("pharmpy/pharmr", ref="main")
```

Pharmpy can then be installed:

```R
pharmr::install_pharmpy()
```

## Using pharmr

```R
>>> library(pharmr)
>>> model <- load_example_model("pheno")
>>> model$parameters
            value  lower upper    fix
POP_CL   0.004693   0.00     ∞  False
POP_VC   1.009160   0.00     ∞  False
COVAPGR  0.100000  -0.99     ∞  False
IIV_CL   0.030963   0.00     ∞  False
IIV_VC   0.031128   0.00     ∞  False
SIGMA    0.013086   0.00     ∞  False
>>> res <- load_example_modelfit_results("pheno")
>>> res$parameter_estimates
    POP_CL     POP_VC    COVAPGR     IIV_CL     IIV_VC      SIGMA 
0.00469555 0.98425800 0.15892000 0.02935080 0.02790600 0.01324100 
>>>
```

A simple example of reading a model, performing a simple transformation, and running the model in NONMEM. Note that
you need NONMEM installed to be able to fit the model, see the [following guide](https://pharmpy.github.io/latest/configuration.html) 
on how to set up the config file.

```R
model <- load_example_model('pheno') %>%
  add_peripheral_compartment() %>%
  fit()
```
