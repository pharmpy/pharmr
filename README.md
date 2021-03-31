# pharmr
Pharmr is an R wrapper to Pharmpy, a Python library for pharmacometrics. It provides an R interface to
all functions found in the `modeling`-module (documented 
[here](https://pharmpy.github.io/latest/reference/pharmpy.modeling.html)).

## Installation

```R
remotes::install_github("pharmpy/pharmr", ref="main")
pharmr::install_pharmpy()
```

If python was not installed on your system you will get a question if you want help with installing miniconda.

You might need to restart the R session before doing

```R
library(pharmr)
```

## Documentation

[Pharmpy documentation](https://pharmpy.github.io)

[Getting started in R](https://pharmpy.github.io/latest/using_r.html#using-r)

A simple example of reading a model, performing a simple transformation, and run the model in NONMEM:

```R
model <- read_model(here('demo', 'pheno.mod')) %>%
  add_parameter('MAT') %>%
  update_source() %>%
  fit()
```
