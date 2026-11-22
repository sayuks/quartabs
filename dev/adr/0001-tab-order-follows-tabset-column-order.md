# Tab order follows the sort order of the tabset column

`render_tabset()` always sorts `data` by the tabset columns, and there is no `sort` argument: it was removed in 0.1.1 and we declined to re-add it in #5. Nested tabsets can only be opened and closed correctly when the rows of one tabset are contiguous, so the raw row order cannot be used as-is. Users control tab order with factor levels instead, for example `factor(x, levels = unique(x))` to keep the order of first appearance.

## Consequences

The order is set once per column. Under every parent tab, the sibling tabs follow the same order.
