#' A guilloche rosette
#'
#' The woven, engraved rosette seen on banknotes and certificates: many wavy
#' circles, each rotated a little from the last, whose crossings make a lattice.
#' Built from one family of curves, `r(theta) = base + amp * cos(lobes * theta +
#' phase)`; no imported assets.
#'
#' With a `seed`, the number of lobes, the number of overlaid curves and the
#' wave depth vary, so every seed is a different rosette.
#'
#' @param lobes Number of lobes per curve, or `NULL` to choose from `seed`.
#' @param copies Number of overlaid curves, or `NULL` to choose from `seed`.
#' @param size Overall diameter in snpc units (0-1).
#' @param seed Integer seed, or `NULL` for a random one.
#' @param ink Colour.
#' @param lwd Line width.
#' @param name,gp,vp Standard grob arguments.
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(guilloche_grob(seed = 2))
#' @export
guilloche_grob <- function(lobes = NULL, copies = NULL, size = 0.9, seed = NULL,
                           ink = "#C9A227", lwd = 0.5,
                           name = NULL, gp = grid::gpar(), vp = NULL) {
  rng <- dk_rng(seed)
  if (is.null(lobes))  lobes  <- as.integer(round(rng(5, 13)))
  if (is.null(copies)) copies <- as.integer(round(rng(26, 44)))
  amp <- rng(0.16, 0.30)
  R   <- size / 2
  th  <- seq(0, 2 * pi, length.out = 241)

  curves <- function(base, amp, k, m) {
    ph <- 2 * pi * (seq_len(m) - 1) / m
    list(x = unlist(lapply(ph, function(p) R * (base + amp * cos(k * th + p)) * cos(th))),
         y = unlist(lapply(ph, function(p) R * (base + amp * cos(k * th + p)) * sin(th))),
         id = rep(seq_len(m), each = length(th)))
  }
  outer <- curves(0.66, amp, lobes, copies)
  inner <- curves(0.30, amp * 0.55, max(3L, lobes - 2L), max(8L, copies %/% 2L))

  grid::gTree(
    children = grid::gList(
      dk_line(outer$x, outer$y, ink, lwd, id = outer$id),
      dk_line(inner$x, inner$y, ink, lwd, id = inner$id),
      dk_line(R * 0.97 * cos(th), R * 0.97 * sin(th), ink, lwd * 1.6)),
    name = name, gp = gp, vp = vp, cl = "dk_medallion")
}

#' A petal rosette
#'
#' A flower-like medallion: layers of petals, each layer rotated half a petal
#' from the one behind, in two alternating tones, around a jewelled centre.
#' Petals are ribbons (see the package notes on geometry) whose width swells and
#' tapers, so no imported assets are used.
#'
#' With a `seed`, the petal count, petal fatness and number of layers vary.
#'
#' @param petals Number of petals per layer, or `NULL` to choose from `seed`.
#' @param layers Number of layers (1-4), or `NULL` to choose from `seed`.
#' @param size Overall diameter in snpc units (0-1).
#' @param fill Length-2 vector `c(light, dark)` for the alternating layers.
#' @param ink Outline and centre colour.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(rosette_grob(seed = 4))
#' @export
rosette_grob <- function(petals = NULL, layers = NULL, size = 0.9, seed = NULL,
                         fill = c("#D9B75C", "#9E7C2C"), ink = "#C9A227",
                         name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(length(fill) == 2)
  rng <- dk_rng(seed)
  if (is.null(petals)) petals <- as.integer(round(rng(6, 14)))
  if (is.null(layers)) layers <- as.integer(round(rng(2, 3.49)))
  fat <- rng(0.26, 0.42)
  R   <- size / 2
  t   <- seq(0, 1, length.out = 31)

  kids <- list()
  for (l in seq_len(layers)) {
    L   <- R * (1 - 0.27 * (l - 1))
    off <- (l - 1) * pi / petals
    px <- py <- id <- NULL
    for (i in seq_len(petals)) {
      a  <- off + 2 * pi * (i - 1) / petals
      rb <- dk_ribbon(L * t * cos(a), L * t * sin(a),
                      L * sin(pi / petals) * 0.66 * (fat / 0.34) * sin(pi * t^0.7) + 1e-6)
      px <- c(px, rb$x); py <- c(py, rb$y); id <- c(id, rep(i, length(rb$x)))
    }
    kids[[length(kids) + 1]] <- dk_poly(px, py, fill[(l %% 2) + 1], id = id,
                                        col = ink, lwd = 0.6)
  }
  kids[[length(kids) + 1]] <- dk_dot(0, 0, R * 0.13, ink)
  kids[[length(kids) + 1]] <- dk_dot(0, 0, R * 0.05, fill[2])
  grid::gTree(children = do.call(grid::gList, kids),
              name = name, gp = gp, vp = vp, cl = "dk_medallion")
}

#' A sunburst
#'
#' Alternating long and short rays around a ringed hub, the classic Art Deco
#' and engraved-title sun. Each ray is a thin triangle.
#'
#' With a `seed`, the ray count and the small differences in ray length vary.
#'
#' @param rays Number of rays (even), or `NULL` to choose from `seed`.
#' @param size Overall diameter in snpc units (0-1).
#' @param variation How much the seed may vary ray lengths, 0 to 1.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(sunburst_grob(seed = 5))
#' @export
sunburst_grob <- function(rays = NULL, size = 0.9, seed = NULL, variation = 0.5,
                          ink = "#C9A227",
                          name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(variation >= 0, variation <= 1)
  rng <- dk_rng(seed)
  if (is.null(rays)) rays <- 2L * as.integer(round(rng(8, 16)))
  R  <- size / 2
  r1 <- R * 0.26
  idx <- seq_len(rays) - 1L
  ang <- 2 * pi * idx / rays + pi / 2
  tip <- ifelse(idx %% 2L == 0L, 1, 0.62) *
    R * (1 + vapply(idx, function(i) rng(-0.07, 0.07), 0) * variation)
  del <- pi / rays * 0.5
  tx <- as.vector(rbind(r1 * cos(ang - del), tip * cos(ang), r1 * cos(ang + del)))
  ty <- as.vector(rbind(r1 * sin(ang - del), tip * sin(ang), r1 * sin(ang + del)))
  th <- seq(0, 2 * pi, length.out = 121)
  grid::gTree(
    children = grid::gList(
      dk_poly(tx, ty, ink, id = rep(seq_len(rays), each = 3)),
      dk_line(r1 * 0.78 * cos(th), r1 * 0.78 * sin(th), ink, 1.2),
      dk_line(r1 * 0.50 * cos(th), r1 * 0.50 * sin(th), ink, 0.8),
      dk_dot(0, 0, r1 * 0.2, ink)),
    name = name, gp = gp, vp = vp, cl = "dk_medallion")
}

#' A corner flourish
#'
#' A bracket for the corner of a page, frame or card: two arms that meet in a
#' rounded bend and finish in inward-turning curls, with a small cluster of
#' beads tucked into the angle. The arms are a single tapering ribbon.
#'
#' Draw it in a square box. Use `corner` to place it at any of the four corners
#' of the box.
#'
#' With a `seed`, the arm length, curl size, number of curl turns and bead
#' cluster vary.
#'
#' @param corner Which corner the bend sits in: `"bl"`, `"br"`, `"tl"` or
#'   `"tr"` (bottom/top, left/right).
#' @param thickness Maximum arm thickness in snpc units.
#' @param variation How much the seed may vary the shape, 0 to 1.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(corner_grob(seed = 3))
#' @export
corner_grob <- function(corner = c("bl", "br", "tl", "tr"), thickness = 0.05,
                        seed = NULL, variation = 0.5, ink = "#C9A227",
                        name = NULL, gp = grid::gpar(), vp = NULL) {
  corner <- match.arg(corner)
  stopifnot(variation >= 0, variation <= 1)
  rng <- dk_rng(seed)
  arm   <- 0.40 + rng(-0.06, 0.04) * variation
  rb    <- 0.07 + rng(0, 0.04) * variation
  r0    <- 0.085 + rng(-0.02, 0.03) * variation
  turns <- 1.0 + rng(0, 0.6) * variation
  c0    <- -0.42

  a   <- seq(-pi / 2, -pi, length.out = 30)
  bx  <- c0 + rb + rb * cos(a)
  by  <- c0 + rb + rb * sin(a)
  e1  <- dk_curl(arm, c0, 1, 0, +1, r0, turns)
  e2  <- dk_curl(c0, arm, 0, 1, -1, r0, turns)
  # Spine: arm along x -> bend -> arm along y, with a curl at each end.
  ax <- seq(arm, c0 + rb, length.out = 40)
  ay <- rep(c0, 40)
  ay2 <- seq(c0 + rb, arm, length.out = 40)
  ax2 <- rep(c0, 40)
  sx <- c(rev(e1$x[-1]), ax, bx[-1], ax2[-1], e2$x[-1])
  sy <- c(rev(e1$y[-1]), ay, by[-1], ay2[-1], e2$y[-1])

  s    <- dk_arc(sx, sy)
  half <- thickness / 2 * pmax(sin(pi * s)^0.45, 0.03)
  rib  <- dk_ribbon(sx, sy, half)

  nb <- as.integer(round(rng(2, 4.49)))
  bd <- seq_len(nb)
  bx2 <- c0 + 0.14 + (bd - 1) * 0.085
  by2 <- c0 + 0.14 + (bd - 1) * 0.085
  fl <- switch(corner, bl = c(1, 1), br = c(-1, 1), tl = c(1, -1), tr = c(-1, -1))

  grid::gTree(
    children = grid::gList(
      dk_poly(rib$x * fl[1], rib$y * fl[2], ink),
      dk_dot(bx2 * fl[1], by2 * fl[2], thickness * (1.15 - 0.2 * bd), ink)),
    name = name, gp = gp, vp = vp, cl = "dk_medallion")
}

#' A row of stars: dinkus and asterism
#'
#' The small breaks printers put between sections. `"dinkus"` is three stars
#' in a row; `"asterism"` is the triangle of three (⁂). Each star is a polygon.
#'
#' With a `seed`, the number of star points, their sharpness and a slight tilt
#' vary.
#'
#' @param kind `"dinkus"` or `"asterism"`.
#' @param size Star size in snpc units.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(asterism_grob("asterism", seed = 2))
#' @export
asterism_grob <- function(kind = c("dinkus", "asterism"), size = 0.34,
                          seed = NULL, ink = "#C9A227",
                          name = NULL, gp = grid::gpar(), vp = NULL) {
  kind <- match.arg(kind)
  rng  <- dk_rng(seed)
  pts   <- as.integer(round(rng(5, 8.49)))
  inner <- rng(0.36, 0.52)
  tilt  <- rng(-0.15, 0.15)
  r <- size / 2
  pos <- if (kind == "dinkus") {
    list(x = c(-2.4, 0, 2.4) * r, y = c(0, 0, 0))
  } else {
    list(x = c(-1.15, 1.15, 0) * r, y = c(-0.7, -0.7, 0.9) * r)
  }
  kids <- lapply(seq_along(pos$x), function(i) {
    st <- dk_star(pos$x[i], pos$y[i], r, pts, inner, rot = pi / 2 + tilt * i)
    dk_poly(st$x, st$y, ink)
  })
  grid::gTree(children = do.call(grid::gList, kids),
              name = name, gp = gp, vp = vp, cl = "dk_medallion")
}
