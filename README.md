# Pharmr

Pharmr is an R wrapper to [**Pharmpy**](https://pharmpy.github.io/), a Python library for 
pharmacometrics. It provides an R interface to all functions found in the `modeling`-module 
(documented [here](https://pharmpy.github.io/latest/reference/pharmpy.modeling.html)). Each
function is also available via the `help`-function (or `?`).


## Installation

Pharmr uses the package [**reticulate**](https://rstudio.github.io/reticulate/) for calling 
Python from R. When installing Pharmr, reticulate will give a prompt to set up the reticulate 
environment. To use Python 3.9 in your environment run the following code before installing Pharmr:

```R
Sys.setenv(RETICULATE_MINICONDA_PYTHON_VERSION="3.9")
```

Then install Pharmr and Pharmpy:

```R
remotes::install_github("pharmpy/pharmr", ref="main")
pharmr::install_pharmpy()
```

## Using Pharmr

A simple example of reading a model, performing a simple transformation, and run the model in NONMEM:

```R
model <- load_example_model('pheno') %>%
  add_parameter('MAT') %>%
  fit()
```

Note: If you try to access data frames belonging to a Pharmpy object you need to reset the index. All functions available in Pharmr do this internally, it is only when you have data frames nested in objects (such as a model object) that you need to do this. An example:

```R
model <- load_example_model('pheno')
residuals <- reset_index(model$modelfit_results$residuals)
```
