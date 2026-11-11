# Frames: ornaments as borders around content -------------------------------

# Rotates a (wide, short) grob to run along a tall, narrow cell. Needs the
# cell's real size, so the rotated viewport is built at draw time.
dk_rot <- function(inner, angle) {
  grid::gTree(inner = inner, angle = angle, cl = "dk_rot")
}

#' @importFrom grid makeContent
#' @export
makeContent.dk_rot <- function(x) {
  w <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  h <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  ch <- x$inner
  ch$vp <- grid::viewport(width = grid::unit(h, "in"),
                          height = grid::unit(w, "in"), angle = x$angle)
  grid::setChildren(x, grid::gList(ch))
}

# Turn a side spec into a grob: NULL (nothing), an ornament id, or a grob.
dk_side <- function(spec, ink, seed) {
  if (is.null(spec)) return(NULL)
  if (grid::is.grob(spec)) return(spec)
  if (is.character(spec) && length(spec) == 1L) {
    tbl  <- dk_ornaments()
    args <- list(ink = ink)
    if (spec %in% tbl$id && tbl$seeded[tbl$id == spec]) args$seed <- seed
    return(do.call(dk_ornament, c(list(spec), args)))
  }
  stop("Each side must be NULL, an ornament id, or a grob.", call. = FALSE)
}

# A frame: optional `content` in the middle, ornaments around it. With no
# content it is a transparent overlay (used for ggplot2 panels).
dk_frame_grob <- function(content = NULL, top = NULL, bottom = NULL,
                          left = NULL, right = NULL, corners = NULL,
                          ink = "#C9A227", thickness = 0.5, seed = 1,
                          name = NULL, gp = grid::gpar(), vp = NULL) {
  th   <- if (inherits(thickness, "unit")) thickness else grid::unit(thickness, "in")
  zero <- grid::unit(0, "in")
  corner_on <- !is.null(corners) && !isFALSE(corners)
  corner_id <- if (is.character(corners)) corners else "corner"

  size <- function(side_on) if (side_on || corner_on) th else zero
  lay <- grid::grid.layout(
    3, 3,
    widths  = grid::unit.c(size(!is.null(left)),  grid::unit(1, "null"), size(!is.null(right))),
    heights = grid::unit.c(size(!is.null(top)),   grid::unit(1, "null"), size(!is.null(bottom))))
  layout_vp <- grid::viewport(layout = lay)

  cell <- function(row, col) grid::viewport(layout.pos.row = row, layout.pos.col = col)
  put  <- function(g, row, col) { if (!is.null(g)) g$vp <- cell(row, col); g }
  span <- if (corner_on) 2L else c(1L, 3L)

  kids <- list(
    if (!is.null(content)) put(content, 2L, 2L),
    put(dk_side(top, ink, seed),       1L, span),
    put(dk_side(bottom, ink, seed + 1), 3L, span),
    if (!is.null(left))  put(dk_rot(dk_side(left, ink, seed + 2),  90),  span, 1L),
    if (!is.null(right)) put(dk_rot(dk_side(right, ink, seed + 3), -90), span, 3L))
  if (corner_on) {
    pos <- list(tl = c(1L, 1L), tr = c(1L, 3L), bl = c(3L, 1L), br = c(3L, 3L))
    k <- 4L
    for (nm in names(pos)) {
      args <- list(nm, ink = ink)
      if (dk_ornaments()$seeded[dk_ornaments()$id == corner_id]) args$seed <- seed + k
      g <- if (corner_id == "corner") do.call(corner_grob, args) else
        do.call(dk_ornament, c(list(corner_id), args[-1]))
      kids[[length(kids) + 1L]] <- put(g, pos[[nm]][1], pos[[nm]][2])
      k <- k + 1L
    }
  }
  kids <- Filter(Negate(is.null), kids)
  # The layout lives on an inner tree so that callers (such as ggplot2's
  # annotation_custom) can replace the outer grob's viewport without losing it.
  inner <- grid::gTree(children = do.call(grid::gList, kids), vp = layout_vp)
  grid::gTree(children = grid::gList(inner), name = name, gp = gp, vp = vp,
              cl = "dk_framed")
}

#' @export
print.dk_framed <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}

#' Frame a ggplot2 plot with ornaments
#'
#' Wraps a plot (or any grob) in ornaments: a border ornament along each side
#' you ask for, and a corner flourish in each corner. The border ornaments fit
#' themselves to the length of each side, so the frame works for any plot
#' shape.
#'
#' The result prints like a plot and can be saved with [ggplot2::ggsave()].
#' It is a grob, not a ggplot, so further `+ layer` additions go on the plot
#' *before* framing it.
#'
#' @param plot A ggplot, or any grid grob.
#' @param top,bottom,left,right Ornament for that side: an id from
#'   [dk_ornaments()] (best are the borders, `"greek_key"`, `"scallop"`,
#'   `"rinceau"`, and rules), a grob, or `NULL` for none. Left and right
#'   ornaments are rotated to run along the side.
#' @param corners `TRUE` or `"corner"` for the corner flourish, `NULL` or
#'   `FALSE` for none.
#' @param ink Colour for ornaments given by id.
#' @param thickness Width of the frame band, in inches (or a [grid::unit()]).
#' @param seed Integer seed; each side gets its own offset from it.
#' @return A `gTree` of class `dk_framed`.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   ornament_frame(p, top = "scallop", bottom = "scallop", corners = TRUE)
#' }
#' @export
ornament_frame <- function(plot, top = NULL, bottom = NULL, left = NULL,
                           right = NULL, corners = NULL, ink = "#C9A227",
                           thickness = 0.5, seed = 1) {
  content <- if (inherits(plot, "ggplot")) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("ornament_frame() needs the 'ggplot2' package for ggplot objects.", call. = FALSE)
    ggplot2::ggplotGrob(plot)
  } else if (grid::is.grob(plot)) plot else
    stop("`plot` must be a ggplot or a grid grob.", call. = FALSE)
  dk_frame_grob(content, top, bottom, left, right, corners, ink, thickness, seed)
}

#' Border a ggplot2 panel with ornaments
#'
#' A layer that draws ornaments along the edges of the plotting panel, over
#' the data area -- a decorated panel border. Add it to a plot with `+`.
#'
#' The ornaments sit *inside* the panel, so give the scales a little extra room
#' (for example `scale_y_continuous(expand = expansion(mult = 0.12))`) if they
#' would cover data. To frame the whole plot, axes and all, use
#' [ornament_frame()] instead.
#'
#' @inheritParams ornament_frame
#' @param thickness Width of the border band, in inches (or a [grid::unit()]).
#' @return A ggplot2 layer.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
#'     panel_ornament(top = "greek_key", bottom = "greek_key", thickness = 0.3)
#' }
#' @export
panel_ornament <- function(top = NULL, bottom = NULL, left = NULL, right = NULL,
                           corners = NULL, ink = "#C9A227", thickness = 0.35,
                           seed = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("panel_ornament() needs the 'ggplot2' package.", call. = FALSE)
  g <- dk_frame_grob(NULL, top, bottom, left, right, corners, ink, thickness, seed)
  ggplot2::annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
