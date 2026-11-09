#' A parametric compass-rose ornament
#'
#' Builds a compass rose entirely from geometry (no imported assets), returned
#' as a grid grob so it composes into base grid, ggplot2 (`annotation_custom`),
#' gt, and Quarto. Part of the delightkitr *print ornaments* family.
#'
#' @param points      Number of star points (>= 4). Even values alternate
#'                    long cardinal / short intercardinal rays.
#' @param size        Overall diameter in npc units (0-1) within its viewport.
#' @param minor_ratio Length of intercardinal rays relative to cardinal rays
#'                    (even `points` only). Use 1 for a uniform star.
#' @param waist       Notch radius as a fraction of tip radius; smaller = slimmer rays.
#' @param rot         Rotation in degrees. 90 puts a point straight up (North).
#' @param fill        Length-2 vector `c(light, dark)` for the two-tone shading.
#' @param ink         Colour for the tick ring, hub outline, and north marker.
#' @param ticks       Draw the degree tick ring?
#' @param n_ticks     Number of ticks around the ring. Every
#'                    `round(n_ticks / 8)`-th tick is drawn long; with fewer
#'                    than 12 ticks all ticks are long.
#' @param north       Draw the north trefoil marker?
#' @param x,y         Centre of the rose in npc units.
#' @param name,gp,vp  Standard grob arguments.
#'
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage(); grid::grid.draw(compass_rose_grob())
#' grid::grid.newpage()
#' grid::grid.draw(compass_rose_grob(points = 16, fill = c("#7FB7BE", "#3A6F7A"),
#'                                   ink = "#9ECFD4"))
#' @export
compass_rose_grob <- function(points = 8,
                              size = 0.9,
                              minor_ratio = 0.45,
                              waist = 0.14,
                              rot = 90,
                              fill = c("#D9B75C", "#9E7C2C"),  # c(light, dark)
                              ink = "#C9A227",
                              ticks = TRUE,
                              n_ticks = 72,
                              north = TRUE,
                              x = 0.5, y = 0.5,
                              name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(points >= 4, length(fill) == 2)
  R   <- size / 2
  sp  <- 2 * pi / points
  th0 <- rot * pi / 180
  idx <- seq_len(points) - 1L                      # 0-based ray indices

  tip_ang  <- th0 + idx * sp
  is_minor <- (idx %% 2L == 1L) & (points %% 2L == 0L)
  tip_r    <- ifelse(is_minor, R * minor_ratio, R)

  notch_ang <- tip_ang + sp / 2
  notch_r   <- R * waist

  tx <- tip_r   * cos(tip_ang);   ty <- tip_r   * sin(tip_ang)
  nx <- notch_r * cos(notch_ang); ny <- notch_r * sin(notch_ang)

  right <- ((idx - 1L) %% points) + 1L             # notch before tip i (1-based)
  left  <- idx + 1L                                # notch after  tip i (1-based)

  # two triangles per ray (tip -> notch -> centre); dark half + light half
  tri_x <- function(notch) as.vector(rbind(tx, nx[notch], rep(0, points)))
  tri_y <- function(notch) as.vector(rbind(ty, ny[notch], rep(0, points)))
  id3   <- rep(seq_len(points), each = 3L)

  npcx <- function(v) grid::unit(x + v, "npc")
  npcy <- function(v) grid::unit(y + v, "npc")

  dark  <- grid::polygonGrob(npcx(tri_x(right)), npcy(tri_y(right)), id = id3,
                             gp = grid::gpar(fill = fill[2], col = NA))
  light <- grid::polygonGrob(npcx(tri_x(left)),  npcy(tri_y(left)),  id = id3,
                             gp = grid::gpar(fill = fill[1], col = NA))

  kids <- grid::gList()

  if (ticks) {
    ring_r <- R * 1.06
    ca     <- seq(0, 2 * pi, length.out = n_ticks + 1L)[-(n_ticks + 1L)]
    step   <- max(1L, round(n_ticks / 8))
    # every `step`-th tick (starting with the first) is a long "major" tick;
    # 0-based index avoids the `%% 1 == 1` never-matches case when n_ticks < 12
    len    <- ifelse((seq_along(ca) - 1L) %% step == 0L, R * 0.10, R * 0.05)
    ring   <- grid::circleGrob(grid::unit(x, "npc"), grid::unit(y, "npc"),
                               r = grid::unit(ring_r, "npc"),
                               gp = grid::gpar(col = ink, fill = NA, lwd = 0.9))
    tk     <- grid::segmentsGrob(npcx(ring_r * cos(ca)),         npcy(ring_r * sin(ca)),
                                 npcx((ring_r - len) * cos(ca)), npcy((ring_r - len) * sin(ca)),
                                 gp = grid::gpar(col = ink, lwd = 0.8, lineend = "round"))
    kids <- grid::gList(kids, ring, tk)
  }

  kids <- grid::gList(
    kids, dark, light,
    grid::circleGrob(grid::unit(x, "npc"), grid::unit(y, "npc"),
                     r = grid::unit(R * 0.10, "npc"),
                     gp = grid::gpar(fill = fill[2], col = ink, lwd = 0.9)),
    grid::circleGrob(grid::unit(x, "npc"), grid::unit(y, "npc"),
                     r = grid::unit(R * 0.035, "npc"),
                     gp = grid::gpar(fill = ink, col = NA))
  )

  if (north) {
    base <- R * 1.16; dot <- R * 0.052
    off  <- rbind(c(0, dot * 1.6), c(-dot * 1.3, 0), c(dot * 1.3, 0))
    kids <- grid::gList(
      kids,
      grid::circleGrob(npcx(off[, 1]), npcy(base + off[, 2]),
                       r = grid::unit(dot, "npc"),
                       gp = grid::gpar(fill = ink, col = NA))
    )
  }

  grid::gTree(children = kids, name = name, gp = gp, vp = vp, cl = "compass_rose")
}

# --- usage -------------------------------------------------------------------
# Standalone:
#   grid::grid.newpage(); grid::grid.draw(compass_rose_grob(points = 12))
#
# ggplot2 (keep the box square or it will stretch — coord_fixed() or a 1:1 box):
#   library(ggplot2)
#   ggplot(mpg, aes(displ, hwy)) +
#     geom_point(colour = "grey70") +
#     annotation_custom(compass_rose_grob(n_ticks = 48),
#                       xmin = 5.3, xmax = 7.1, ymin = 30, ymax = 44)
#
# gt / Quarto-HTML: grid draws to a device, not into HTML, so render once to SVG
#   and inline it, e.g. svglite::svglite() -> grid.draw() -> read the string into
#   a gt::html() cell or a Quarto shortcode. (That wrapper is the next piece.)
