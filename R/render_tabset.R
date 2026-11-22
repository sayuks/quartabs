# nolint start: line_length_linter
#' Dynamically Generate Tabset Panels in Quarto HTML Documents
#'
#' @description
#' `render_tabset()` takes a data frame as input and outputs the markdown
#' that generates the [tabset](https://quarto.org/docs/output-formats/html-basics.html#tabsets)
#' to stdout (console). ***Only works with Quarto HTML documents.***
#' See [Get started](https://sayuks.github.io/quartabs/vignettes/get_started.html) for details.
#' @details
#' - Write `#| results: asis` at the beginning of the chunk or
#'   `results='asis'` in the chunk options.
#' - If multiple `tabset_vars` are given, create nested tabsets.
#' - For columns specified in `output_vars`, columns of type list are output with
#'   [print()] and normal columns are output with [cat()].
#' - The `data` is sorted internally by `tabset_vars`, so tabs are displayed
#'   in ascending order. Factor columns are sorted by their levels (before
#'   the conversion to character described below), so the tab order can be
#'   controlled with the levels. For example,
#'   `factor(x, levels = unique(x))` keeps the order of first appearance in
#'   the data. Since levels apply to the whole column, nested tabs cannot have
#'   a different order under each parent tab.
#' - If `tabset_vars` or `output_vars` have "factor", "Date" and "POSIXt"
#'   columns, they are converted internally to character. This is to prevent it
#'   being displayed as numeric when [cat()] is executed.
#'   Sorting by `tabset_vars` is performed before conversion to string.
#' @param data A data frame.
#' @param tabset_vars Columns to use as tabset labels. Internally passed
#'   to the `select` argument of [subset()]. Accepts raw column names,
#'   strings, numbers and logical values.
#' @param output_vars Columns to display in each tabset panel. Internally
#'   passed to the `select` argument of [subset()]. Accepts raw column names,
#'   strings, numbers and logical values.
#' @param layout `NULL` or a character vector of length 1 for specifying layout
#'   in tabset panel. If not `NULL`, `layout` must begin with at least three
#'   or more repetitions of ":" (e.g. ":::"). Closing div (e.g. ":::") is
#'   inserted automatically.
#'   See for details: \url{https://quarto.org/docs/authoring/figures.html#complex-layouts}.
#' @param heading_levels `NULL` or a vector consisting of natural numbers
#'   and missing values. The length is equal to the number of columns specified
#'   in `tabset_vars`. This controls whether it is partially (or entirely)
#'   displayed as normal header instead of tabset.
#'   - If `heading_levels` is a `NULL`, all output is tabset.
#'   - If `heading_levels` is a vector of positive natural number, the elements
#'     of the vector correspond to the columns specified in `tabset_vars`.
#'      - If the element is integer, the tabset column is displayed as headers
#'        with their level, not tabset. (e.g. 2 means h2 header).
#'        Levels 1 to 6 are recommended. The reason is that quarto supports
#'        headers up to 6. 7 and above will also work, but they are displayed
#'        as normal text. In addition, considering the chapter format,
#'        it is preferable to gradually increase the level, as in 1, 2 and 3.
#'      - If the element is NA, tabset is displayed.
#' @param active `NULL` or a named list that chooses the active tab, the tab
#'   shown by default when the page loads (Quarto's `.active` class, which
#'   requires Quarto 1.7 or later; older versions ignore it). Each name is a
#'   column specified in `tabset_vars`, and each element is either:
#'   - A single non-missing value: the tab with this value is active in every
#'     tabset of that column that contains it. It is an error if no tab of
#'     that column has this value.
#'   - A function: it is called once per tabset of that column (that is, for
#'     the sibling tabs under each parent tab) with the unique tab values in
#'     display order, and must return one of them, or `NA` for no active tab.
#'     For example, `max` makes the latest tab active under each parent tab,
#'     and `function(x) x[length(x)]` the last one.
#'
#'   Functions are applied before the conversion of factor, Date and POSIXt
#'   columns to character, so numbers are compared as numbers and factors
#'   keep their levels. A character value is compared with the tab labels
#'   (e.g. `"2024-01-01"` for a Date tab); other values are compared with the
#'   original column (e.g. `as.Date("2024-01-01")`). Without an active tab, a tabset
#'   shows its first tab. Columns displayed as headings by `heading_levels`
#'   cannot be specified.
#' @param pills Logical, use pills or not.
#'   See <https://getbootstrap.com/docs/5.2/components/navs-tabs/#pills>
#'   for details. If `heading_levels` is specified, this will be ignored.
#' @param tabset_width Character, one of "default", "fill" and "justified".
#'   See <https://getbootstrap.com/docs/5.2/components/navs-tabs/#fill-and-justify>
#'   for details. If `heading_levels` is specified, this will be ignored.
#' @return `NULL` invisibly. This function outputs the markdown
#' that generates the [tabset](https://quarto.org/docs/output-formats/html-basics.html#tabsets)
#' to stdout (console).
#' @section Limitations:
#' - `layout` is intended for simplified use cases and
#'   complex layouts may not work.
#' - When outputting tables or figures that use JavaScript
#'   (such as `{plotly}`, `{leaflet}`, `{DT}`, `{reactable}`, etc.),
#'   it seems JavaScript dependencies need to be resolved.
#'   A simple solution is to wrap the output in [htmltools::div()]
#'   and create a dummy plot in another chunk. See the Get started for details.
#' - When `tabset_vars` and `output_vars` have the following columns,
#'   they may not display well:
#'     - A column of type list contains a named vector or list
#'       (This is for `output_vars`. `tabset_vars` must not contain list
#'       columns).
#'     - Classes with their own printing methods,
#'       such as "difftime", "ts", .etc.
#' - When specifying a list-type column that includes ggplot objects in
#'   `output_vars`, setting the chunk option `echo: fenced` may cause
#'   the plots to not display correctly.
#' - If the chunk label starts with `fig-` (e.g. `#| label: fig-example`),
#'   Quarto holds all plots until the end of the chunk, so they are displayed
#'   below the tabset instead of inside each tab panel. Use a label that does
#'   not start with `fig-`, or no label. To make each plot a
#'   cross-referenceable figure, see the Get started for details.
#' @references As this function is focused on quickly and dynamically
#'   generating tabsets and chunks, it is difficult to customize it on a
#'   chunk-by-chunk basis. The regular way to dynamically create chunks is
#'   to use functions such as [knitr::knit()], [knitr::knit_child()],
#'   [knitr::knit_expand()], etc. For more information on these,
#'   see the following links.
#'
#' - Heiss, Andrew. 2024. “Guide to Generating and Rendering Computational
#'   Markdown Content Programmatically with Quarto.” November 4, 2024.
#'   \doi{https://doi.org/10.59350/pa44j-cc302}.
#' - <https://pkg.yihui.org/rmarkdown-cookbook/child-document.html#child-document>
#' - <https://pkg.yihui.org/rmarkdown-cookbook/knit-expand.html>
#' @examples
#' # sample data
#' df <- data.frame(
#'   group1 = c(rep("A", 3), rep("B", 3)),
#'   group2 = rep(c("X", "Y", "Z"), 2),
#'   value1 = 1:6,
#'   value2 = letters[1:6]
#' )
#'
#' # Here are examples of the output before it is converted to tabset.
#' # If you want it to actually work, in the .qmd file,
#' # set `results='asis'` in the chunk options or
#' # write `#| results: asis` at the beginning of the chunk.
#'
#' # Basic usage
#' render_tabset(df, group1, value1)
#'
#' # Nested tabset, two outputs side by side with a width of 1:1
#' render_tabset(
#'   df,
#'   c(group1, group2),
#'   c(value1, value2),
#'   layout = "::: {layout-ncol=2}"
#' )
#'
#' # Use heading instead of tabset
#' render_tabset(
#'   df,
#'   c(group1, group2),
#'   value1,
#'   heading_levels = c(2, 3)
#' )
#'
#' # Show tab "B" and, under each tab, the last nested tab by default
#' render_tabset(
#'   df,
#'   c(group1, group2),
#'   value1,
#'   active = list(group1 = "B", group2 = max)
#' )
#' @export
# nolint end
render_tabset <- function(data,
                          tabset_vars,
                          output_vars,
                          layout = NULL,
                          heading_levels = NULL,
                          active = NULL,
                          pills = FALSE,
                          tabset_width = "default") {
  tabset_div <- make_tabset_div(pills, tabset_width)

  l <- do.call(
    validate_data,
    list(
      data = data,
      tabset_vars = substitute(tabset_vars),
      output_vars = substitute(output_vars),
      layout = layout,
      heading_levels = heading_levels
    )
  )

  tabset_names <- l$tabset_names
  output_names <- l$output_names
  heading_levels <- l$heading_levels
  len_tab <- length(tabset_names)
  active <- validate_active(active, tabset_names, heading_levels)

  data <- sort_data(data, tabset_names, output_names)
  tabset_master <- get_tabset_master(data, tabset_names)
  active_master <- get_active_master(data, tabset_names, active, tabset_master)
  data <- convert_data(data)

  # For each row of the data, print the tabset and output panels
  lapply(seq_len(nrow(data)), function(i) {
    print_row_tabsets(
      data = data,
      heading_levels = heading_levels,
      layout = layout,
      i = i,
      tabset_names = tabset_names,
      len_tab = len_tab,
      output_names = output_names,
      tabset_master = tabset_master,
      tabset_div = tabset_div,
      active_master = active_master
    )
  })

  invisible()
}

# Function to print tabsets and outputs for a single row
print_row_tabsets <- function(data,
                              heading_levels,
                              layout,
                              i,
                              tabset_names,
                              len_tab,
                              output_names,
                              tabset_master,
                              tabset_div,
                              active_master = NULL) {
  print_tabset_start(
    heading_levels = heading_levels,
    i = i,
    tabset_master = tabset_master,
    tabset_div = tabset_div
  )
  print_nested_tabsets(
    data = data,
    heading_levels = heading_levels,
    i,
    tabset_names = tabset_names,
    len_tab = len_tab,
    tabset_master = tabset_master,
    tabset_div = tabset_div,
    active_master = active_master
  )
  print_outputs(
    data = data,
    heading_levels = heading_levels,
    layout = layout,
    i = i,
    tabset_names = tabset_names,
    len_tab = len_tab,
    output_names = output_names,
    active_master = active_master
  )
  print_tabset_end(
    heading_levels = heading_levels,
    i = i,
    len_tab = len_tab,
    tabset_master = tabset_master
  )
}

make_tabset_div <- function(pills, tabset_width) {
  assert_logical(pills)

  tabset_width <- match.arg(tabset_width, c("default", "fill", "justified"))

  div <- ".panel-tabset"

  if (pills) {
    div <- paste(div, ".nav-pills")
  }

  if (tabset_width %in% c("fill", "justified")) {
    div <- sprintf("%s .nav-%s", div, tabset_width)
  }

  paste0("::: {", div, "}")
}

# Function to print the start of a tabset
print_tabset_start <- function(heading_levels,
                               i,
                               tabset_master,
                               tabset_div) {
  if (is.na(heading_levels[1]) &&
        tabset_master[[i, "tabset1_start"]]) {
    cat(tabset_div)
    cat("\n\n")
  }
}

# Function to print nested tabsets
print_nested_tabsets <- function(data,
                                 heading_levels,
                                 i,
                                 tabset_names,
                                 len_tab,
                                 tabset_master,
                                 tabset_div,
                                 active_master = NULL) {
  if (len_tab >= 2) {
    lapply(2:len_tab, function(j) {
      if (tabset_master[[i, paste0("tabset", j, "_start")]]) {
        heading_level <- ifelse(
          is.na(heading_levels[j - 1]),
          j - 1,
          heading_levels[j - 1]
        )
        print_heading(
          heading_level,
          data[[i, tabset_names[j - 1]]],
          is_active(active_master, i, j - 1)
        )
        if (is.na(heading_levels[j])) {
          cat(tabset_div)
          cat("\n\n")
        }
      }
    })
  }
  invisible()
}

# Function to print the outputs
print_outputs <- function(data,
                          heading_levels,
                          layout,
                          i,
                          tabset_names,
                          len_tab,
                          output_names,
                          active_master = NULL) {
  heading_level <- ifelse(
    is.na(heading_levels[len_tab]),
    len_tab,
    heading_levels[len_tab]
  )

  print_heading(
    heading_level,
    data[[i, tabset_names[len_tab]]],
    is_active(active_master, i, len_tab)
  )

  if (!is.null(layout)) {
    cat(layout)
    cat("\n\n")
  }

  lapply(
    seq_along(output_names),
    function(j) {
      out_col <- data[[output_names[j]]]
      out <- out_col[[i]]

      if (is.list(out_col)) {
        print(out)
      } else {
        cat(out)
      }

      cat("\n\n")
    }
  )

  if (!is.null(layout)) {
    cat(sub("^(:+).*", "\\1", layout))
    cat("\n\n")
  }
}

# Function to print a tab heading, with the `.active` class if needed
print_heading <- function(heading_level, label, active = FALSE) {
  cat(strrep("#", heading_level), label, if (active) "{.active}")
  cat("\n\n")
}

is_active <- function(active_master, i, j) {
  !is.null(active_master) && active_master[i, j]
}

# Function to print the end of tabsets
print_tabset_end <- function(heading_levels,
                             i,
                             len_tab,
                             tabset_master) {
  lapply(rev(seq_len(len_tab)), function(j) {
    if (is.na(heading_levels[j]) &&
          tabset_master[[i, paste0("tabset", j, "_end")]]) {
      cat(":::")
      cat("\n\n")
    }
  })
  invisible()
}

validate_data <- function(data,
                          tabset_vars,
                          output_vars,
                          layout = NULL,
                          heading_levels = NULL) {
  stopifnot(
    "`data` must be a data frame." =
      is.data.frame(data),
    "`data` must have one or more rows." =
      nrow(data) >= 1,
    "`data` must have two or more columns." =
      ncol(data) >= 2
  )

  if (!is.null(layout)) {
    stopifnot(
      "`layout` must be length 1." =
        length(layout) == 1,
      "`layout` must be character." =
        is.character(layout),
      '`layout` must begin with at least three or more repetitions of ":".' =
        grepl("^:{3,}", layout)
    )
  }

  if (!is.null(heading_levels)) {
    stopifnot(
      "`heading_levels` must be numeric." =
        is.numeric(heading_levels),
      "`heading_levels` must be length 1 or greater." =
        length(heading_levels) > 0,
      "`heading_levels` must not include NaN." =
        !is.nan(heading_levels),
      "`heading_levels` must not be infinite." =
        !is.infinite(heading_levels)
    )

    nums <- heading_levels[!is.na(heading_levels)]

    if (length(nums) > 0) {
      stopifnot(
        "`heading_levels` except for NAs must be positive." =
          nums > 0
      )
    }

    heading_levels <- as.integer(heading_levels)
  }

  # Get tabset column names from data based on tabset_vars
  tabset_names <- do.call(
    subset,
    list(x = data, select = substitute(tabset_vars))
  )
  tabset_names <- colnames(tabset_names)

  len_tab <- length(tabset_names)

  stopifnot(
    "`tabset_vars` must be of length 1 or more." =
      len_tab > 0
  )

  valid_class_for_tabset_name <- c(
    "logical",
    "numeric",
    "integer",
    "character",
    "factor",
    "Date",
    "POSIXt"
  )

  lapply(
    tabset_names,
    function(nm) {
      col <- data[[nm]]
      if (!inherits(col, valid_class_for_tabset_name)) {
        stop(
          "The column ",
          sprintf("`%s`", nm),
          " specified in `tabset_vars` ",
          "must have one of the following classes: ",
          toString(valid_class_for_tabset_name),
          ".\n",
          sprintf("But the column `%s` has the following class(es): ", nm),
          toString(class(col)),
          "."
        )
      }
    }
  )

  if (is.null(heading_levels)) {
    heading_levels <- rep(NA_integer_, len_tab)
  }

  stopifnot(
    "The number of columns specified in `tabset_vars`
    and the length of `heading_levels` must be the same." =
      length(heading_levels) == len_tab
  )

  # Get output column names from data based on output_vars
  output_names <- do.call(
    subset,
    list(x = data, select = substitute(output_vars))
  )

  output_names <- colnames(output_names)

  stopifnot(
    "`output_vars` must be of length 1 or more." =
      length(output_names) > 0,

    "There must not be variables that are included in both
    `tabset_vars` and `output_vars`." =
      length(intersect(tabset_names, output_names)) == 0
  )

  list(
    tabset_names = tabset_names,
    output_names = output_names,
    heading_levels = heading_levels
  )
}

sort_data <- function(data, tabset_names, output_names) {
  data <- data[, c(tabset_names, output_names)]
  data[do.call(order, data[, tabset_names, drop = FALSE]), ]
}

convert_data <- function(data) {
  data[] <- lapply(
    data,
    function(x) {
      if (inherits(x, c("factor", "Date", "POSIXt"))) {
        as.character(x)
      } else {
        x
      }
    }
  )
  data
}

validate_active <- function(active, tabset_names, heading_levels) {
  if (is.null(active)) {
    return(NULL)
  }

  stopifnot(
    "`active` must be `NULL` or a list." =
      is.list(active) && !is.data.frame(active)
  )

  if (length(active) == 0) {
    return(NULL)
  }

  nms <- names(active)

  stopifnot(
    "All elements of `active` must be named." =
      !is.null(nms) && !anyNA(nms) && all(nzchar(nms)),
    "The names of `active` must be unique." =
      !anyDuplicated(nms)
  )

  lapply(nms, function(nm) {
    if (!nm %in% tabset_names) {
      stop(
        sprintf("The name `%s` in `active` ", nm),
        "is not a column specified in `tabset_vars`."
      )
    }
    if (!is.na(heading_levels[match(nm, tabset_names)])) {
      stop(
        sprintf("The column `%s` in `active` ", nm),
        "is displayed as headings by `heading_levels`, not as a tabset."
      )
    }
    x <- active[[nm]]
    if (!(is.function(x) || (is.atomic(x) && length(x) == 1 && !is.na(x)))) {
      stop(
        sprintf("The element `%s` of `active` ", nm),
        "must be a function or a single non-missing value."
      )
    }
  })

  active
}

# Logical matrix (rows of `data` x tabset columns) flagging the rows whose
# tab heading gets the `.active` class. `data` must be sorted by
# `tabset_names` and must not have been converted to character yet.
# `tabset_master` is the result of `get_tabset_master()`.
get_active_master <- function(data, tabset_names, active, tabset_master) {
  if (is.null(active)) {
    return(NULL)
  }

  n <- nrow(data)
  res <- matrix(FALSE, nrow = n, ncol = length(tabset_names))

  for (nm in names(active)) {
    j <- match(nm, tabset_names)
    choice <- active[[nm]]
    col <- data[[nm]]

    # Row indices of each tabset of this column (the sibling tabs that
    # share a parent tab)
    tabset_rows <- split(
      seq_len(n),
      cumsum(tabset_master[[paste0("tabset", j, "_start")]])
    )

    any_active <- FALSE

    for (rows in tabset_rows) {
      tab_values <- unique(col[rows])
      active_tab <- choose_active_tab(choice, tab_values, nm)
      if (is.na(active_tab)) {
        next
      }
      any_active <- TRUE
      # Only the first row of a tab prints its heading.
      first_row <- rows[match(active_tab, match(col[rows], tab_values))]
      res[first_row, j] <- TRUE
    }

    if (!is.function(choice) && !any_active) {
      stop(
        sprintf("The value of `active` for `%s` ", nm),
        sprintf("was not found in the column `%s`.", nm)
      )
    }
  }

  res
}

# Position of the active tab among `tab_values` (the unique values of one
# tabset in display order), or NA for none.
choose_active_tab <- function(choice, tab_values, nm) {
  if (!is.function(choice)) {
    # A character value is compared with the tab labels, so it also
    # matches Date, POSIXt and numeric tabs as they are displayed.
    if (is.character(choice)) {
      return(match(choice, as.character(tab_values)))
    }
    return(match(choice, tab_values))
  }

  chosen <- choice(tab_values)
  if (!(is.atomic(chosen) && length(chosen) == 1)) {
    stop(
      sprintf("The function in `active` for `%s` ", nm),
      "must return a single value."
    )
  }
  if (is.na(chosen)) {
    return(NA_integer_)
  }
  pos <- match(chosen, tab_values)
  if (is.na(pos)) {
    stop(
      sprintf("The function in `active` for `%s` ", nm),
      "must return one of the values it was given."
    )
  }
  pos
}

# Logical columns `tabset<j>_start` / `tabset<j>_end` flagging the first and
# last row of each tabset of tabset column j. `data` must be sorted by
# `tabset_names`, so the rows sharing a parent tab are contiguous. Two values
# are the same tab when their labels are the same; a missing value is its own
# tab, apart from any label such as "NA" or "NaN".
get_tabset_master <- function(data, tabset_names) {
  n <- nrow(data)
  # Whether row i + 1 has a different parent tab from row i
  parent_changed <- rep(FALSE, max(n - 1, 0))
  flags <- list()

  for (j in seq_along(tabset_names)) {
    flags[[paste0("tabset", j, "_start")]] <- c(TRUE, parent_changed)
    flags[[paste0("tabset", j, "_end")]] <- c(parent_changed, TRUE)
    tab <- as.integer(factor(data[[tabset_names[j]]], exclude = NULL))
    parent_changed <- parent_changed | tab[-1] != tab[-n]
  }

  as.data.frame(flags)
}

assert_logical <- function(x) {
  nm <- deparse(substitute(x))
  if (!(isTRUE(x) || isFALSE(x))) {
    stop(
      sprintf("`%s` must be a logical scalar (`TRUE` or `FALSE`).", nm)
    )
  }
}
