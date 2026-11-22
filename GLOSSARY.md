# quartabs

quartabs turns a data frame into the markdown for Quarto HTML tabsets. Each row of the data frame becomes one tab panel.

## Language

### Tabsets

**Tabset**:
A group of tabs that are shown one at a time. Quarto renders it from a `.panel-tabset` div.
_Avoid_: Tab group, tab bar

**Tab**:
One selectable entry in a tabset. Its label is a value of a tabset column.
_Avoid_: Tab header, nav item

**Tab panel**:
The content shown when its tab is selected. It is filled from the output columns.
_Avoid_: Tab body, pane

**Tabset column**:
A data frame column (selected by `tabset_vars`) whose values become tab labels. Each additional column adds one nesting level.
_Avoid_: Group column, tab variable

**Output column**:
A data frame column (selected by `output_vars`) whose values are written into the tab panels.
_Avoid_: Content column, value column

**Nested tabset**:
A tabset that sits inside the tab panel of a tab from the previous tabset column.
_Avoid_: Sub-tabset, child tabset

**Parent tab**:
The tab whose panel holds a nested tabset.

**Sibling tabs**:
The tabs of one tabset, that is, the tabs that share the same parent tab. For the top level, all tabs of the first tabset column.

**Heading level**:
A tabset column rendered as ordinary section headings instead of a tabset (`heading_levels`).
_Avoid_: Header mode

### Tab selection

**Tab order**:
The left-to-right order of sibling tabs. It follows the sort order of the tabset column, so for a factor it follows the levels.
_Avoid_: Tab sort

**Active tab**:
The tab that a tabset shows when the page first loads. Without one, Quarto shows the first tab.
_Avoid_: Default tab, selected tab, initial tab
