# quartabs (development version)

## New features

* `render_tabset()` gains an `active` argument to choose the tab shown by default (Quarto's `.active` class, Quarto 1.7 or later). Each tabset column takes a value, or a function such as `max` that picks the active tab under each parent tab (#4).

## Minor improvements

* Removed `{stats}` from Imports; quartabs no longer imports any packages.
* Documented that the tab order of `render_tabset()` can be controlled by factor levels, e.g. `factor(x, levels = unique(x))` to keep the order of first appearance, with an example in the [Get started](https://sayuks.github.io/quartabs/vignettes/get_started.html) (#5).
* Documented that `render_tabset()` cannot place plots inside tab panels when the chunk label starts with `fig-`, and added an example of cross-referenceable figures in each tab to the [Get started](https://sayuks.github.io/quartabs/vignettes/get_started.html) (#6).

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
