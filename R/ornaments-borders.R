#' A pen swash
#'
#' A long S-shaped flourish whose stroke swells in the middle and thins to
#' hairlines, finishing in an inward-turning curl at each end -- the kind of
#' flourish a copperplate pen leaves under a title. The stroke is a single
#' ribbon along the curve.
#'
#' Fits itself to the box it is drawn in (a wide, short box works best, about
#' 6 times wider than tall).
#'
#' With a `seed`, the sway of the S, the curl size and the number of curl turns
#' vary.
#'
#' @param thickness Maximum stroke thickness as a fraction of the box height.
#' @param variation How much the seed may vary the shape, 0 to 1.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(swash_grob(seed = 2))
#' @export
swash_grob <- function(thickness = 0.12, seed = NULL, variation = 0.5,
                       ink = "#C9A227",
                       name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(variation >= 0, variation <= 1)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  dk_fit_grob(function(w, h) {
    rng   <- dk_rng(seed)
    amp   <- h * (0.17 + rng(-0.05, 0.08) * variation)
    r0    <- h * (0.17 + rng(-0.04, 0.05) * variation)
    turns <- 1.1 + rng(0, 0.7) * variation
    a     <- w / 2 * 0.86
    x  <- seq(-a, a, length.out = 140)
    y  <- amp * sin(pi * x / a)
    k  <- amp * pi / a * cos(pi)          # slope at both ends is the same
    e2 <- dk_curl(a, 0, 1, k, +1, r0, turns)
    e1 <- dk_curl(-a, 0, -1, -k, +1, r0, turns)
    sx <- c(rev(e1$x[-1]), x, e2$x[-1])
    sy <- c(rev(e1$y[-1]), y, e2$y[-1])
    s  <- dk_arc(sx, sy)
    rb <- dk_ribbon(sx, sy, h * thickness / 2 * pmax(sin(pi * s)^0.8, 0.03))
    grid::gList(dk_poly(rb$x, rb$y, ink, unit = "in"))
  }, name = name, gp = gp, vp = vp, cl = "dk_border")
}

#' A rinceau: a scrolling vine border
#'
#' A wavy stem with a spiral curl springing from every crest and trough,
#' alternating above and below, and a bead at the heart of each curl: the
#' classical running scroll used on frames and friezes. Stem and curls are
#' ribbons.
#'
#' Fits itself to the box it is drawn in, adding as many scrolls as fit (a wide,
#' short box works best, about 8 times wider than tall).
#'
#' With a `seed`, the wave length, curl size and turns vary.
#'
#' @inheritParams swash_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(rinceau_grob(seed = 1))
#' @export
rinceau_grob <- function(thickness = 0.06, seed = NULL, variation = 0.5,
                         ink = "#C9A227",
                         name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(variation >= 0, variation <= 1)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  dk_fit_grob(function(w, h) {
    rng    <- dk_rng(seed)
    lambda <- h * (2.5 + rng(-0.3, 0.9) * variation)   # one full wave
    n_half <- max(2L, as.integer(round(w * 0.94 / (lambda / 2))))
    lambda <- w * 0.94 / (n_half / 2)
    amp    <- h * 0.20
    r0     <- h * (0.17 + rng(-0.03, 0.04) * variation)
    turns  <- 1.15 + rng(0, 0.5) * variation
    tk     <- h * thickness

    x  <- seq(-w * 0.47, w * 0.47, length.out = 40 * n_half)
    y  <- amp * sin(2 * pi * x / lambda + pi / 2 * 0 + pi)   # starts at a trough side
    stem <- dk_ribbon(x, y, rep(tk / 2, length(x)))
    kids <- list(dk_poly(stem$x, stem$y, ink, unit = "in"))

    # a curl at each crest (curls up, anticlockwise) and trough (curls down)
    xs <- -w * 0.47 + (seq_len(n_half) - 0.5) * (lambda / 2)
    for (i in seq_along(xs)) {
      ys   <- amp * sin(2 * pi * xs[i] / lambda + pi)
      side <- if (ys > 0) +1 else -1
      cl   <- dk_curl(xs[i], ys, 1, 0, side, r0, turns)
      s    <- dk_arc(cl$x, cl$y)
      rb   <- dk_ribbon(cl$x, cl$y, tk / 2 * pmax(1 - 0.85 * s, 0.1))
      kids[[length(kids) + 1]] <- dk_poly(rb$x, rb$y, ink, unit = "in")
      kids[[length(kids) + 1]] <- dk_dot(cl$x[length(cl$x)], cl$y[length(cl$y)],
                                         tk * 0.9, ink, unit = "in")
    }
    do.call(grid::gList, kids)
  }, name = name, gp = gp, vp = vp, cl = "dk_border")
}

#' A Greek key border
#'
#' The square meander: a continuous line that runs along a baseline and winds
#' into a rectangular spiral, tile after tile. Drawn as a mitred polyline, so
#' every corner is a true right angle.
#'
#' Fits itself to the box it is drawn in, using as many whole tiles as fit and
#' centring them (a wide, short box works best, about 10 times wider than tall).
#'
#' With a `seed`, the key is the same shape every time but the line weight
#' varies a little; `variation` scales that.
#'
#' @param weight Line weight as a fraction of one grid step (about 0.3-0.7).
#' @param variation How much the seed may vary the line weight, 0 to 1.
#' @inheritParams guilloche_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(greek_key_grob(seed = 1))
#' @export
greek_key_grob <- function(weight = 0.42, seed = NULL, variation = 0.5,
                           ink = "#C9A227",
                           name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(variation >= 0, variation <= 1)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  dk_fit_grob(function(w, h) {
    rng <- dk_rng(seed)
    wt  <- weight * (1 + rng(-0.2, 0.25) * variation)
    u   <- h / 5.4                       # one grid step
    tile <- 6 * u
    n   <- max(1L, as.integer(floor(w * 0.96 / tile)))
    x0  <- -n * tile / 2
    y0  <- -2.5 * u
    # one tile of the meander, in grid steps
    kx <- c(0, 0, 4, 4, 2, 2, 3)
    ky <- c(0, 4, 4, 1, 1, 3, 3)
    px <- unlist(lapply(seq_len(n) - 1L, function(i) x0 + (i * 6 + kx) * u))
    py <- rep(y0 + ky * u, n)
    id <- rep(seq_len(n), each = length(kx))
    base <- dk_line(c(x0, x0 + n * tile), c(y0, y0), ink, u * 96 * wt,
                    unit = "in", lineend = "square", linejoin = "mitre")
    key  <- dk_line(px, py, ink, u * 96 * wt, unit = "in", id = id,
                    lineend = "square", linejoin = "mitre")
    grid::gList(base, key)
  }, name = name, gp = gp, vp = vp, cl = "dk_border")
}

#' A scallop border
#'
#' A row of arches, each with a smaller arch inside it and a bead where
#' neighbours meet: the shell-and-fan edge used on frames and lace.
#'
#' Fits itself to the box it is drawn in, choosing the number of scallops so
#' they fill the width exactly (a wide, short box works best, about 10 times
#' wider than tall).
#'
#' With a `seed`, the number of scallops, the inner-arch size and the bead
#' size vary a little.
#'
#' @inheritParams swash_grob
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(scallop_grob(seed = 1))
#' @export
scallop_grob <- function(thickness = 0.05, seed = NULL, variation = 0.5,
                         ink = "#C9A227",
                         name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(variation >= 0, variation <= 1)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  dk_fit_grob(function(w, h) {
    rng   <- dk_rng(seed)
    n     <- as.integer(ceiling(w * 0.96 / h)) + as.integer(round(rng(0, 2.4) * variation))
    r     <- w * 0.96 / (2 * n)
    inner <- 0.60 + rng(-0.1, 0.12) * variation
    bead  <- r * (0.10 + rng(0, 0.06) * variation)
    lwd   <- h * thickness * 96
    th    <- seq(pi, 0, length.out = 40)
    cx    <- -w * 0.48 + r + (seq_len(n) - 1L) * 2 * r
    y0    <- -r / 2
    arc <- function(k) {
      list(x = unlist(lapply(cx, function(c) c + k * r * cos(th))),
           y = unlist(lapply(cx, function(c) y0 + k * r * sin(th))),
           id = rep(seq_len(n), each = length(th)))
    }
    o <- arc(1); i <- arc(inner)
    grid::gList(
      dk_line(o$x, o$y, ink, lwd, unit = "in", id = o$id),
      dk_line(i$x, i$y, ink, lwd, unit = "in", id = i$id),
      dk_dot(c(cx - r, cx[n] + r), rep(y0, n + 1L), bead, ink, unit = "in"))
  }, name = name, gp = gp, vp = vp, cl = "dk_border")
}
