# The active tab is specified per tabset column, by a value or a function

`render_tabset()` takes `active`, a named list that maps a tabset column to either a value or a function. A function is called separately for each set of sibling tabs, so `max` picks the latest tab under each parent tab. A value marks the tab with that value in every set of sibling tabs that contains it; it is an error only when no tab of the column has it. A character value is compared with the tab labels, so it also matches Date, POSIXt and numeric tabs as displayed. Functions and other values are evaluated on the column's original type, before the internal conversion of factor, Date and POSIXt to character. Otherwise `max()` would compare numbers as strings (`"9"` > `"10"`) and factors would lose their levels. Keying the setting on the tabset column matches how tab order is controlled (ADR-0001), and Quarto renders the active tab from the `.active` class (Quarto >= 1.7). Decided in #4.

## Considered Options

- **A logical "active" column in `data`**: rejected. Flagging a row also activates every ancestor tab on its path. Choosing a different active child under each parent would then mark several parents active at the same time.
- **Positional keywords such as `"last"`**: rejected. They cover only one case, and the function form already expresses them (`function(x) x[length(x)]`).
