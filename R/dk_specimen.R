#' Embed the mitosis specimen delighter
#'
#' A self-contained, mouse/touch-reactive organic shape with no function --
#' species #1 of `delightkitr`'s interactive surface, ported from the
#' Artifact prototype "A Part That Does Nothing". Click it, drag it, hover
#' over it, watch it split. Nothing is saved; nothing is measured except a
#' tally that means nothing.
#'
#' Fully self-contained (fonts inlined, no external requests, no JS
#' dependencies) and safe to embed more than once on the same page -- each
#' call generates its own unique element IDs and scopes its CSS to a wrapper
#' class, so instances never collide with each other or with the host
#' page's own styles.
#'
#' @param width CSS width for the widget's stage, e.g. `"420px"` or `"100%"`.
#'   The stage never exceeds this value but will shrink to fit a narrower
#'   container (a sidebar, a Shiny panel column, ...).
#' @return A [htmltools::browsable()]-wrapped [htmltools::HTML()] object --
#'   printing it at the console opens it in the RStudio Viewer, printing it
#'   in a Quarto/R Markdown chunk renders it inline (no `results='asis'`
#'   needed), and it works directly as Shiny UI.
#' @examples
#' if (interactive()) {
#'   dk_specimen()
#' }
#' @export
dk_specimen <- function(width = "420px") {
  path <- system.file("js", "specimen.html", package = "delightkitr")
  if (!nzchar(path)) {
    stop("Could not find the specimen template -- is delightkitr installed/loaded correctly?", call. = FALSE)
  }
  template <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  id <- paste0("dk", paste(sample(c(0:9, letters), 8, replace = TRUE), collapse = ""))
  html <- gsub("{{ID}}", id, template, fixed = TRUE)
  html <- gsub("{{WIDTH}}", width, html, fixed = TRUE)

  htmltools::browsable(htmltools::HTML(html))
}
