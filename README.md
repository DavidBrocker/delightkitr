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

## Status

Species #1 (`dk_specimen()`, the interactive mitosis specimen) is in —
ported from the Artifact prototype, CSS rescoped off `:root` onto a
`.delightkitr-specimen` wrapper, classes namespaced `dk-`, element IDs
templated per-instance. Next: proving out the R/xspline static surface as
species #2, then the TikZ side.
