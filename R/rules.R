# Seeded RNG ------------------------------------------------------------------
# A tiny Park-Miller generator so a `seed` always gives the same ornament on
# every platform, without touching (or depending on) R's global RNG state.
dk_rng <- function(seed = NULL) {
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  # Scramble the seed first: Park-Miller's first draws are almost identical for
  # small neighbouring seeds (1, 2, 3, ...), which would make them look alike.
  state <- ((abs(as.numeric(seed)) %% 1048576) * 2654435761) %% 2147483646 + 1
  step <- function() {
    state <<- (16807 * state) %% 2147483647
    state / 2147483647
  }
  for (i in 1:4) step()
  function(lo = 0, hi = 1) lo + (hi - lo) * step()
}

# All shape geometry is expressed in "snpc" units (the shorter side of the
# viewport) so that dots stay round and diamonds stay square however wide and
# short the ornament's box is. Only horizontal positions along the rule use
# "npc".
dk_y <- function(v) grid::unit(0.5, "npc") + grid::unit(v, "snpc")
dk_x <- function(x, v = 0) grid::unit(x, "npc") + grid::unit(v, "snpc")

#' An endcap for a rule
#'
#' Small terminal shapes built from geometry (circles and polygons), meant to
#' finish the ends of a rule or flourish. Used internally by
#' [swelled_rule_grob()] and [fleuron_rule_grob()], and exported so you can
#' place caps on your own lines.
#'
#' Sizes are in `"snpc"` units (the shorter side of the viewport), so caps stay
#' round/square in a wide, short ornament box.
#'
#' @param kind `"dot"`, `"diamond"`, `"fleuron"` (a diamond flanked by two
#'   beads, like a tiny trefoil), or `"none"`.
#' @param x Horizontal position of the cap's centre, in npc units.
#' @param size Cap size (roughly its diameter) in snpc units.
#' @param ink Colour.
#' @param dir `1` for a cap on the right end of a rule (points right),
#'   `-1` for the left end.
#' @param y Vertical position of the cap's centre, in npc units.
#' @param name,gp,vp Standard grob arguments.
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(endcap_grob("fleuron", x = 0.5, size = 0.3))
#' @export
endcap_grob <- function(kind = c("dot", "diamond", "fleuron", "none"),
                        x = 0.5, size = 0.3, ink = "#C9A227", dir = 1,
                        y = 0.5, name = NULL, gp = grid::gpar(), vp = NULL) {
  kind <- match.arg(kind)
  r <- size / 2
  yc <- function(v = 0) grid::unit(y, "npc") + grid::unit(v, "snpc")

  bead <- function(dx, rad) {
    grid::circleGrob(dk_x(x, dx * dir), yc(0), r = grid::unit(rad, "snpc"),
                     gp = grid::gpar(fill = ink, col = NA))
  }
  diamond <- function(dx, rad, wide = 1) {
    grid::polygonGrob(
      dk_x(x, dx * dir + c(rad * wide, 0, -rad * wide, 0)),
      yc(c(0, rad, 0, -rad)),
      gp = grid::gpar(fill = ink, col = NA))
  }

  kids <- switch(kind,
    none    = grid::gList(),
    dot     = grid::gList(bead(0, r * 0.7)),
    diamond = grid::gList(diamond(0, r, wide = 1.35)),
    fleuron = grid::gList(
      diamond(0, r, wide = 1.35),
      bead(r * 1.9,  r * 0.28),
      bead(-r * 1.9, r * 0.28))
  )
  grid::gTree(children = kids, name = name, gp = gp, vp = vp, cl = "dk_endcap")
}

# Half-thickness profile along a stroke, t in [0, 1]. `peak` moves the swell off
# centre (peak < 1 pushes it toward the start), `power` sharpens or broadens it.
dk_swell <- function(t, peak = 1, power = 1) sin(pi * t^peak)^power

#' A swelled rule: a line that thickens and tapers like a pen stroke
#'
#' A horizontal rule whose thickness swells to a maximum and tapers to fine
#' points at both ends, optionally finished with [endcap_grob()]s. Built from a
#' single polygon of two smooth edges -- pure geometry, no imported assets.
#'
#' With a `seed`, the swell's position and sharpness vary a little, so a set of
#' rules with different seeds looks hand-drawn rather than stamped. The same
#' seed always gives the same rule; the global RNG is never touched.
#'
#' @param width Length of the rule in npc units (0-1).
#' @param thickness Maximum thickness in snpc units (the shorter side of the
#'   viewport; keep the ornament box wide and short, e.g. 4 x 0.5 inches).
#' @param ends Endcap kind for both ends: `"dot"`, `"diamond"`, `"fleuron"`, or
#'   `"none"`. See [endcap_grob()].
#' @param seed Integer seed for the small variations, or `NULL` for a random
#'   one.
#' @param variation How much the seed may vary the shape, 0 (none) to 1.
#' @param ink Colour.
#' @param name,gp,vp Standard grob arguments.
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(swelled_rule_grob(seed = 1))
#' @export
swelled_rule_grob <- function(width = 0.9, thickness = 0.16, ends = "dot",
                              seed = NULL, variation = 0.5, ink = "#C9A227",
                              name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(width > 0, width <= 1, thickness > 0, variation >= 0, variation <= 1)
  rng   <- dk_rng(seed)
  peak  <- 1 + rng(-0.6, 0.6) * variation
  power <- 0.8 + rng(-0.3, 1.0) * variation
  size  <- thickness * 1.6

  t    <- seq(0, 1, length.out = 121)
  half <- thickness / 2 * dk_swell(t, peak, power)
  x0   <- 0.5 - width / 2
  xs   <- x0 + t * width
  body <- grid::polygonGrob(
    dk_x(c(xs, rev(xs))), dk_y(c(half, rev(-half))),
    gp = grid::gpar(fill = ink, col = NA))

  kids <- grid::gList(
    body,
    endcap_grob(ends, x = x0,          size = size, ink = ink, dir = -1),
    endcap_grob(ends, x = x0 + width,  size = size, ink = ink, dir = 1))
  grid::gTree(children = kids, name = name, gp = gp, vp = vp, cl = "dk_rule")
}

#' A fleuron rule: a centred ornament flanked by tapering rules and beads
#'
#' A centred fleuron (a diamond with two beads) with a fine rule running out
#' to each side. Each side thickens toward the centre and tapers to a point
#' outward, with a few small beads strung along it, then an optional endcap.
#' Built from circles and polygons only.
#'
#' With a `seed`, the length of the gap around the fleuron, the taper and the
#' number and position of beads vary a little, so no two rules need match.
#'
#' @inheritParams swelled_rule_grob
#' @param centre Centre ornament kind: `"fleuron"`, `"diamond"`, `"dot"`, or
#'   `"none"`. See [endcap_grob()].
#' @param beads Maximum number of beads along each side (0 for none).
#' @return A `gTree` grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(fleuron_rule_grob(seed = 7))
#' @export
fleuron_rule_grob <- function(width = 0.9, thickness = 0.12, ends = "dot",
                              centre = "fleuron", beads = 3, seed = NULL,
                              variation = 0.5, ink = "#C9A227",
                              name = NULL, gp = grid::gpar(), vp = NULL) {
  stopifnot(width > 0, width <= 1, thickness > 0, beads >= 0,
            variation >= 0, variation <= 1)
  rng   <- dk_rng(seed)
  gap   <- 0.07 + rng(0, 0.05) * variation          # clear space around centre
  power <- 1.1 + rng(0, 0.8) * variation
  size  <- thickness * 1.8

  centre_grob <- endcap_grob(centre, x = 0.5, size = thickness * 3.2, ink = ink)

  # One side, built outward from the centre; `dir` is -1 (left) or +1 (right).
  side <- function(dir) {
    x_in  <- 0.5 + dir * gap
    x_out <- 0.5 + dir * width / 2
    t     <- seq(0, 1, length.out = 91)
    xs    <- x_in + t * (x_out - x_in)
    # thickest just outside the centre gap, easing in from a point at the
    # inner end and tapering to a fine point outward
    half  <- thickness / 2 * (1 - t)^power * sin(pi / 2 * pmin(t / 0.12, 1))^0.6
    body  <- grid::polygonGrob(
      dk_x(c(xs, rev(xs))), dk_y(c(half, rev(-half))),
      gp = grid::gpar(fill = ink, col = NA))
    n_beads <- if (beads > 0) round(rng(1, beads + 0.49)) else 0
    bead_grobs <- lapply(seq_len(n_beads), function(i) {
      u <- (i - 0.5) / n_beads * 0.7 + rng(-0.04, 0.04) * variation
      endcap_grob("dot", x = x_in + u * (x_out - x_in),
                  size = thickness * (1.6 - 0.8 * u), ink = ink)
    })
    cap <- endcap_grob(ends, x = x_out, size = size, ink = ink, dir = dir)
    do.call(grid::gList, c(list(body), bead_grobs, list(cap)))
  }

  kids <- grid::gList(centre_grob, side(-1), side(1))
  grid::gTree(children = kids, name = name, gp = gp, vp = vp, cl = "dk_rule")
}
