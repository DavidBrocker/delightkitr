#' Render an ornament grob to an inline SVG string
#'
#' The bridge from a delightkitr grob (grid graphics) to HTML outputs — gt
#' tables, Quarto HTML, R Markdown. grid draws to a graphics device, not into
#' HTML, so this renders the grob once to an in-memory SVG and returns the
#' markup. Form-agnostic: works for any ornament grob, medallion or rule.
#'
#' @param grob   A grid grob, e.g. [compass_rose_grob()].
#' @param width,height Device size in inches; also fixes the ornament's aspect
#'   ratio (keep equal for a square medallion; wide + short for a rule).
#' @param bg     Background colour; "transparent" lets the table/page show through.
#' @return A length-1 character string of `<svg>...</svg>` markup.
#' @export
ornament_svg <- function(grob, width = 1, height = 1, bg = "transparent") {
  if (!requireNamespace("svglite", quietly = TRUE))
    stop("ornament_svg() needs the 'svglite' package.", call. = FALSE)
  getp <- svglite::svgstring(width = width, height = height,
                             bg = bg, standalone = FALSE)   # inline fragment, no XML prolog
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(grob)
  paste(getp(), collapse = "")
}

#' Wrap an ornament in a centred HTML block
#'
#' Reusable by both gt and Quarto. `line-height:0` keeps the SVG from picking up
#' stray vertical space from its container.
#'
#' @inheritParams ornament_svg
#' @param align "center", "left", or "right".
#' @param pad   CSS vertical padding around the ornament.
#' @param fluid If `TRUE`, the SVG scales down to fit its container (never
#'   larger than `width` inches) instead of keeping a fixed size.
#' @return A length-1 character string of HTML.
#' @export
ornament_html <- function(grob, width = 1, height = 1, bg = "transparent",
                          align = c("center", "left", "right"), pad = "0.25rem",
                          fluid = FALSE) {
  align <- match.arg(align)
  svg <- ornament_svg(grob, width, height, bg)
  if (fluid) {
    svg <- sub("width='[^']*' height='[^']*'",
               sprintf("style='width:100%%;height:auto;max-width:%sin'", format(width)),
               svg)
  }
  sprintf('<div style="text-align:%s;padding:%s 0;line-height:0">%s</div>',
          align, pad, svg)
}

#' Place an ornament into a gt table
#'
#' Proven end-to-end path: grob -> inline SVG -> gt. Non-destructive by default
#' (adds a source-note flourish). `where = "header"` sets the table title to the
#' ornament instead, which REPLACES any existing title — pass your title text
#' separately if you need both.
#'
#' @param data  A `gt_tbl`.
#' @param grob  An ornament grob.
#' @param where "source_note" (default) or "header".
#' @param width,height Ornament size in inches.
#' @return The modified `gt_tbl`.
#' @export
gt_ornament <- function(data, grob, where = c("source_note", "header"),
                        width = 0.9, height = 0.9) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt_ornament() needs the 'gt' package.", call. = FALSE)
  where <- match.arg(where)
  html  <- gt::html(ornament_html(grob, width, height))
  switch(where,
    source_note = gt::tab_source_note(data, source_note = html),
    header      = gt::tab_header(data, title = html)
  )
}

# --- usage -------------------------------------------------------------------
# gt (end to end):
#   library(gt)
#   gt(head(mtcars[, 1:4])) |>
#     gt_ornament(compass_rose_grob(n_ticks = 48), where = "header")
#
# Quarto / R Markdown HTML — in a chunk with  #| output: asis
#   cat(ornament_html(compass_rose_grob(points = 12), width = 1.2, height = 1.2))
#
# The same ornament_svg() string also drops into any htmltools::HTML(),
# shiny::HTML(), or a Quarto shortcode later — the bridge is written once.
