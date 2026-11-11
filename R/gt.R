#' Frame a gt table with ornaments
#'
#' Puts an ornament above the table (in the header, over the title) and another
#' below it (as a source note). Ornaments are inlined as SVG and scale to the
#' width of the table. Because they are HTML, they show in HTML output
#' (Quarto, R Markdown, the Viewer), not in PDF or Word.
#'
#' Sets the table header, so any existing title or subtitle is replaced; pass
#' `title` and `subtitle` here to keep them.
#'
#' @param data A `gt_tbl`.
#' @param top,bottom Ornament id from [dk_ornaments()], or a grob, or `NULL`
#'   for none.
#' @param title,subtitle Optional title and subtitle text, shown under the top
#'   ornament.
#' @param width,height Ornament box in inches; the ratio sets its shape, and it
#'   scales to the table's width.
#' @param ink Colour for ornaments given by id.
#' @param seed Integer seed; the bottom ornament uses `seed + 1`.
#' @return The modified `gt_tbl`.
#' @examples
#' if (requireNamespace("gt", quietly = TRUE) &&
#'     requireNamespace("svglite", quietly = TRUE)) {
#'   gt::gt(head(mtcars[, 1:4])) |>
#'     gt_ornament_frame(top = "swash", bottom = "fleuron_rule",
#'                       title = "Motor Trend cars")
#' }
#' @export
gt_ornament_frame <- function(data, top = "swelled_rule", bottom = "fleuron_rule",
                              title = NULL, subtitle = NULL,
                              width = 6, height = 0.6, ink = "#C9A227",
                              seed = 1) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt_ornament_frame() needs the 'gt' package.", call. = FALSE)
  html_of <- function(spec, sd) {
    g <- dk_side(spec, ink, sd)
    ornament_html(g, width, height, fluid = TRUE)
  }
  top_html <- if (is.null(top)) "" else html_of(top, seed)
  if (is.null(title) && is.null(top)) {
    title <- ""
  }
  head_html <- paste0(top_html,
                      if (!is.null(title)) sprintf('<div style="padding-top:0.2rem">%s</div>', title))
  out <- gt::tab_header(data, title = gt::html(head_html),
                        subtitle = if (is.null(subtitle)) NULL else subtitle)
  if (!is.null(bottom)) {
    out <- gt::tab_source_note(out, source_note = gt::html(html_of(bottom, seed + 1)))
  }
  out
}
