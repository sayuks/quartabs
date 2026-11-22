# Headings (tab labels) printed by render_tabset()
tab_headings <- function(...) {
  grep("^#", utils::capture.output(render_tabset(...)), value = TRUE)
}
