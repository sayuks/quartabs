
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quartabs

<!-- badges: start -->

[![R-CMD-check](https://github.com/sayuks/quartabs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/quartabs/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/quartabs/graph/badge.svg)](https://app.codecov.io/gh/sayuks/quartabs)
[![lint.yaml](https://github.com/sayuks/quartabs/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/quartabs/actions/workflows/lint.yaml)
<!-- badges: end -->

Dynamically Generate Tabset Panels in a Quarto HTML Document.

## Installation

``` r
install.packages("quartabs")
```

You can install the development version of quartabs from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sayuks/quartabs")
```

## Examples

`render_tabset()` takes a data frame as input and outputs the markdown
of the
[tabset](https://quarto.org/docs/output-formats/html-basics.html#tabsets).

**In the actual .qmd file, specify the chunk option `results: asis`.**

``` r
library(quartabs)

data.frame(
  tab = c("A", "B"),
  value = c("Tab content for A", "Tab content for B")
) |>
  render_tabset(tab, value)
#> ::: {.panel-tabset}
#> 
#> # A
#> 
#> Tab content for A
#> 
#> # B
#> 
#> Tab content for B
#> 
#> :::
```

See [Get
started](https://sayuks.github.io/quartabs/vignettes/get_started.html)
for details.

## Code of Conduct

Please note that the quartabs project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
