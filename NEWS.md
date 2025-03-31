# quartabs 0.1.1

## Breaking changes

* The `sort` argument of `render_tabset()` has been removed. The data is always sorted by `tabset_vars`.

## Minor improvements

* Added `{stats}` to Imports.
* Added CITATION.
* Added a link to the [Get started](https://sayuks.github.io/quartabs/vignettes/get_started.html) page in the `render_tabset()` documentation.  
* Minor edits to the [Get started](https://sayuks.github.io/quartabs/vignettes/get_started.html) documentation.
* Added CRAN and monthly downloads badges to README.

## Bug fixes

* `render_tabset()` now works correctly even if `tabset_vars` contains missing values (#3).

# quartabs 0.1.0

* Initial CRAN submission.
* New `render_tabset()` takes a data frame as input and outputs the markdown that generates the [tabset](https://quarto.org/docs/output-formats/html-basics.html#tabsets) to stdout (console).
