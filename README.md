
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quartabs

<!-- badges: start -->

<a href = "https://cran.r-project.org/web/packages/quartabs/index.html" target = "_blank"><img src="https://www.r-pkg.org/badges/version/quartabs"></a>
[![R-CMD-check](https://github.com/sayuks/quartabs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/quartabs/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/quartabs/graph/badge.svg)](https://app.codecov.io/gh/sayuks/quartabs)
[![lint.yaml](https://github.com/sayuks/quartabs/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/quartabs/actions/workflows/lint.yaml)
<a href = "https://sayuks.github.io/quartabs/" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/quartabs"></a>
<!-- badges: end -->

The `quartabs` is an R package that dynamically generates [Tabset
Panels](https://quarto.org/docs/output-formats/html-basics.html#tabsets)
in Quarto HTML documents.

## Installation

You can install `quartabs` from CRAN:

``` r
install.packages("quartabs")
```

You can install the development version of `quartabs` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sayuks/quartabs")
```

## Examples

`render_tabset()` takes a data frame as input and outputs the markdown
that generates the
[tabset](https://quarto.org/docs/output-formats/html-basics.html#tabsets)
to stdout (console).

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
