# Internal geometry helpers shared by the ornament kinds ----------------------
#
# Ornaments are described in plain numeric coordinates centred on (0, 0) and
# only turned into grid units at the last moment. `unit` is either "snpc" (the
# shorter side of the viewport, for square ornaments) or "in" (inches, for
# ornaments that fit themselves to the box they are drawn into).

dk_u <- function(x, y, unit) {
  list(x = grid::unit(0.5, "npc") + grid::unit(x, unit),
       y = grid::unit(0.5, "npc") + grid::unit(y, unit))
}

dk_poly <- function(x, y, fill, unit = "snpc", id = NULL, col = NA, lwd = 1) {
  p <- dk_u(x, y, unit)
  grid::polygonGrob(p$x, p$y, id = id,
                    gp = grid::gpar(fill = fill, col = col, lwd = lwd))
}

dk_line <- function(x, y, col, lwd = 1, unit = "snpc", id = NULL,
                    lineend = "round", linejoin = "round") {
  p <- dk_u(x, y, unit)
  grid::polylineGrob(p$x, p$y, id = id,
                     gp = grid::gpar(col = col, lwd = lwd, fill = NA,
                                     lineend = lineend, linejoin = linejoin))
}

dk_dot <- function(x, y, r, fill, unit = "snpc") {
  p <- dk_u(x, y, unit)
  grid::circleGrob(p$x, p$y, r = grid::unit(r, unit),
                   gp = grid::gpar(fill = fill, col = NA))
}

# A "ribbon": a stroke of varying width along any curve. Takes the spine
# (x, y) and the half-width at each spine point, returns one closed outline.
dk_ribbon <- function(x, y, half) {
  n  <- length(x)
  dx <- c(x[2] - x[1], x[3:n] - x[1:(n - 2)], x[n] - x[n - 1])
  dy <- c(y[2] - y[1], y[3:n] - y[1:(n - 2)], y[n] - y[n - 1])
  len <- sqrt(dx^2 + dy^2)
  len[len == 0] <- 1
  nx <- -dy / len
  ny <- dx / len
  list(x = c(x + nx * half, rev(x - nx * half)),
       y = c(y + ny * half, rev(y - ny * half)))
}

# Fraction (0-1) of the way along a spine, by arc length.
dk_arc <- function(x, y) {
  s <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
  s / s[length(s)]
}

# A spiral "curl" leaving the point (ex, ey) in direction (tx, ty) and winding
# inward. `side` is +1 to curl to the left of travel, -1 to the right.
dk_curl <- function(ex, ey, tx, ty, side, r0, turns = 1.25, n = 90) {
  tl <- sqrt(tx^2 + ty^2)
  tx <- tx / tl
  ty <- ty / tl
  cx <- ex + side * r0 * (-ty)
  cy <- ey + side * r0 * tx
  phi0 <- atan2(ey - cy, ex - cx)
  u   <- seq(0, 1, length.out = n)
  phi <- phi0 + side * u * turns * 2 * pi
  r   <- r0 * (1 - 0.88 * u)
  list(x = cx + r * cos(phi), y = cy + r * sin(phi))
}

# A star polygon centred at (cx, cy); `inner` is the inner/outer radius ratio.
dk_star <- function(cx, cy, r, points = 5, inner = 0.42, rot = pi / 2) {
  k <- seq_len(points * 2) - 1L
  rad <- ifelse(k %% 2L == 0L, r, r * inner)
  ang <- rot + k * pi / points
  list(x = cx + rad * cos(ang), y = cy + rad * sin(ang))
}

# Ornaments that fit themselves to their box: the shape is rebuilt at draw
# time from the viewport's real size in inches, so tiles stay square and the
# repeat count always fits exactly. `builder(w, h)` returns a gList; it must
# create its own RNG from the stored seed so redrawing gives the same result.
dk_fit_grob <- function(builder, name = NULL, gp = grid::gpar(), vp = NULL,
                        cl = NULL) {
  grid::gTree(builder = builder, name = name, gp = gp, vp = vp,
              cl = c(cl, "dk_fit"))
}

#' @importFrom grid makeContent
#' @export
makeContent.dk_fit <- function(x) {
  w <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  h <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  grid::setChildren(x, x$builder(w, h))
}
