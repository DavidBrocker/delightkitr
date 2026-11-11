# delightkitr

Small, purposeless decorative flourishes — "delighters" — in the tradition
of modelmaking greebles: no function, kept anyway because they feel good.
Companion to `musickitr`, `themekitr`, and `showkitr`.

Grew out of an Artifact prototype, ["A Part That Does
Nothing"](https://claude.ai/code/artifact/be7dc691-debd-4a1c-8051-e61197164e3e)
— an organic shape with no job except reacting to you. `delightkitr` is
that idea generalized across three surfaces, each with its own
implementation, sharing one visual language: traits (color, faceted-ness)
that mutate a small, bounded amount at each "division" instead of being
randomized outright, so a decoration never looks arbitrary.

## The three surfaces

1. **Interactive** (`inst/js/`) — self-contained HTML/JS/CSS, canvas-based,
   zero external dependencies. For Quarto HTML docs, Shiny apps, standalone
   pages. Copy-paste a file, or drop it in via an R helper
   (`htmltools::tags`-style).
2. **Static** (`R/`, via `grid::xsplineGrob()`) — generated SVG/PNG
   appliques and borders for Quarto PDF/Word reports and `ggplot2` figures.
   `xspline()`'s native `shape` parameter is the same smooth-to-faceted
   blend the canvas version built by hand — no headless-browser render step
   needed. Scoped to page chrome (margins, dividers, covers), never inside
   a plot panel — decoration shouldn't compete with the data encoding.
3. **LaTeX** (`inst/tikz/`) — native TikZ macros for borders and frames, not
   a shared asset with the other two (TikZ needs to stay vector-scalable
   and tile correctly at any page size). Same visual language, separate
   implementation.

## Install

```r
# devtools::install_github("DavidBrocker/delightkitr")
devtools::load_all()                                       # while developing
```

## Use

```r
dk_specimen()                 # default stage, caps at 420px wide
dk_specimen(width = "300px")  # narrower, e.g. a sidebar or Shiny column
```

Call it at the console and it opens in the RStudio Viewer (via
`htmltools::browsable()`); print it in a Quarto/R Markdown chunk and it
renders inline with no `results='asis'` needed; use it as Shiny UI
directly. Safe to call more than once on the same page — each call
generates its own unique element IDs and scopes its CSS to a wrapper
class, so instances never collide with each other or the host page.

### Print ornaments

Twelve ornament kinds, listed by `dk_ornaments()` and buildable by id with
`dk_ornament()`. **See them all in the
[gallery](https://davidbrocker.github.io/delightkitr/)** (source:
`docs/index.html`, rebuilt with `Rscript data-raw/build-gallery.R`).

| Family | Kinds |
|---|---|
| Rules | `swelled_rule_grob()`, `fleuron_rule_grob()`, `swash_grob()` |
| Borders | `rinceau_grob()`, `greek_key_grob()`, `scallop_grob()` |
| Medallions | `compass_rose_grob()`, `guilloche_grob()`, `rosette_grob()`, `sunburst_grob()` |
| Corners | `corner_grob()` |
| Marks | `asterism_grob()` (dinkus and asterism) |

Borders and swashes fit themselves to the box they are drawn in, so tiles
stay square and the repeat count always fits exactly.

```r
dk_ornaments()                       # the catalogue
grid::grid.draw(dk_ornament("guilloche", seed = 3))
```

Static ornaments are `grid` grobs, so they compose into base grid, ggplot2
(`annotation_custom()`), gt, and Quarto, and are built from geometry alone
(no imported assets).

```r
grid::grid.newpage()
grid::grid.draw(compass_rose_grob(points = 12))

# ggplot2 (keep the box square, e.g. with coord_fixed())
ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
  ggplot2::geom_point(colour = "grey70") +
  ggplot2::annotation_custom(compass_rose_grob(n_ticks = 48),
                             xmin = 5.3, xmax = 7.1, ymin = 30, ymax = 44)

# gt: needs the 'gt' and 'svglite' packages
gt::gt(head(mtcars[, 1:4])) |>
  gt_ornament(compass_rose_grob(n_ticks = 48), where = "header")

# Quarto / R Markdown HTML, in a chunk with `#| output: asis`
cat(ornament_html(compass_rose_grob(points = 12), width = 1.2, height = 1.2))
```

Rules and endcaps are wide, short ornaments: draw them in a box about
8 times wider than tall (e.g. 4 x 0.5 inches). A `seed` makes each one a
little different — always the same for the same seed, and without touching
your global RNG.

```r
grid::grid.draw(swelled_rule_grob(seed = 1, ends = "diamond"))
grid::grid.draw(fleuron_rule_grob(seed = 7, ends = "fleuron"))

# as a divider in a Quarto HTML doc (chunk option `#| output: asis`)
cat(ornament_html(fleuron_rule_grob(seed = 2), width = 5, height = 0.6))
```

### In ggplot2 and gt

```r
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

# Frame the whole plot: border ornaments on any sides, flourishes in the corners
ornament_frame(p, top = "greek_key", bottom = "greek_key",
               left = "scallop", right = "scallop", corners = TRUE)

# Or border just the panel, as a layer (give the scales a little room)
p + scale_y_continuous(expand = expansion(mult = 0.15)) +
  panel_ornament(top = "scallop", bottom = "scallop", corners = TRUE)

# gt: an ornament above the title and another below the table
gt::gt(head(mtcars[, 1:4])) |>
  gt_ornament_frame(top = "swash", bottom = "fleuron_rule",
                    title = "Motor Trend cars")
```

Border ornaments fit themselves to the length of each side, so a frame suits
any plot shape. `ornament_frame()` returns a grob that prints like a plot and
saves with `ggsave()`. The gt helpers produce HTML (Quarto, R Markdown, the
Viewer), not PDF or Word output.

`ornament_svg()` renders any ornament grob to an inline SVG string, so the
same bridge works for gt, Quarto, `htmltools::HTML()`, and Shiny.

## Status

Species #1 (`dk_specimen()`, the interactive mitosis specimen) is in —
ported from the Artifact prototype, CSS rescoped off `:root` onto a
`.delightkitr-specimen` wrapper, classes namespaced `dk-`, element IDs
templated per-instance. The static surface has started: `compass_rose_grob()`
plus the grob-to-HTML bridge (`ornament_svg()`, `ornament_html()`,
`gt_ornament()`). Twelve ornament kinds are in, all with seeded variation, plus the gallery.
Next: more kinds (chains, dentils, cartouches, tailpieces), then the TikZ side.

## Credits and inspiration

The design principle of the print ornaments — small ornament *kinds*
generated from geometry with seeded variation, rather than imported
artwork — follows [tikz-flora](https://github.com/wn-mitch/tikz-flora)
by wn-mitch (MIT licence; [site](https://flora.alpacasoft.dev/)), which in
turn credits Kevin Raymond's
[florilegium](https://github.com/kevinraymond/florilegium) (MIT).
delightkitr does not copy their code or constructions: the ornaments here
are independently written in R/`grid`. Where a future ornament adapts a
specific tikz-flora construction, it will be credited in its own
documentation and the MIT notice reproduced.
