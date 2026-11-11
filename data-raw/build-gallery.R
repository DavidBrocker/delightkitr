# Build the delightkitr gallery: a single self-contained HTML page.
#
#   Rscript data-raw/build-gallery.R            # writes docs/index.html
#
# Every ornament is rendered once per seed to an inline SVG with the package's
# own ornament_svg(). The page recolours them in the browser by swapping the
# three sentinel colours below, so the palette picker needs no re-render.

library(delightkitr)

out_file <- "docs/index.html"
n_seeds  <- 9L

# Sentinel colours the page swaps for the chosen palette: ink, light, dark.
sent <- c(ink = "#C9A227", light = "#D9B75C", dark = "#9E7C2C")

# How each kind is shown: box size in inches (width, height) and the extra
# arguments for each variant (a `seed`, or distinct arguments for kinds the
# seed does not vary).
shows <- list(
  swelled_rule = list(size = c(6, 0.7), fn = function(s) swelled_rule_grob(seed = s, ends = c("dot", "diamond", "fleuron")[s %% 3L + 1L]),
                      code = function(s) sprintf('swelled_rule_grob(seed = %d, ends = "%s")', s, c("dot", "diamond", "fleuron")[s %% 3L + 1L])),
  fleuron_rule = list(size = c(6, 0.7), fn = function(s) fleuron_rule_grob(seed = s, ends = c("dot", "diamond", "fleuron", "none")[s %% 4L + 1L]),
                      code = function(s) sprintf('fleuron_rule_grob(seed = %d, ends = "%s")', s, c("dot", "diamond", "fleuron", "none")[s %% 4L + 1L])),
  swash        = list(size = c(6, 1), fn = function(s) swash_grob(seed = s),
                      code = function(s) sprintf("swash_grob(seed = %d)", s)),
  rinceau      = list(size = c(6, 0.8), fn = function(s) rinceau_grob(seed = s),
                      code = function(s) sprintf("rinceau_grob(seed = %d)", s)),
  greek_key    = list(size = c(6, 0.6), fn = function(s) greek_key_grob(seed = s),
                      code = function(s) sprintf("greek_key_grob(seed = %d)", s)),
  scallop      = list(size = c(6, 0.6), fn = function(s) scallop_grob(seed = s),
                      code = function(s) sprintf("scallop_grob(seed = %d)", s)),
  compass_rose = list(size = c(2, 2),
                      fn = function(s) compass_rose_grob(points = c(8, 12, 16, 6, 8, 12, 16, 24, 10)[s],
                                                         minor_ratio = c(0.45, 0.5, 0.55, 1, 0.38, 0.6, 0.5, 0.45, 0.4)[s],
                                                         waist = c(0.14, 0.12, 0.10, 0.18, 0.16, 0.12, 0.09, 0.10, 0.14)[s],
                                                         n_ticks = c(72, 48, 96, 36, 24, 60, 120, 72, 48)[s]),
                      code = function(s) sprintf("compass_rose_grob(points = %d)", c(8, 12, 16, 6, 8, 12, 16, 24, 10)[s])),
  guilloche    = list(size = c(2, 2), fn = function(s) guilloche_grob(seed = s),
                      code = function(s) sprintf("guilloche_grob(seed = %d)", s)),
  rosette      = list(size = c(2, 2), fn = function(s) rosette_grob(seed = s),
                      code = function(s) sprintf("rosette_grob(seed = %d)", s)),
  sunburst     = list(size = c(2, 2), fn = function(s) sunburst_grob(seed = s),
                      code = function(s) sprintf("sunburst_grob(seed = %d)", s)),
  corner       = list(size = c(2, 2), fn = function(s) corner_grob(c("bl", "br", "tl", "tr")[s %% 4L + 1L], seed = s),
                      code = function(s) sprintf('corner_grob("%s", seed = %d)', c("bl", "br", "tl", "tr")[s %% 4L + 1L], s)),
  asterism     = list(size = c(3, 1), fn = function(s) asterism_grob(c("dinkus", "asterism")[s %% 2L + 1L], seed = s),
                      code = function(s) sprintf('asterism_grob("%s", seed = %d)', c("dinkus", "asterism")[s %% 2L + 1L], s))
)

blurb <- c(
  swelled_rule = "A line that thickens and tapers like a pen stroke, finished with dot, diamond or fleuron endcaps.",
  fleuron_rule = "A centred fleuron with tapering rules and a few beads strung along each side.",
  swash        = "An S-shaped copperplate flourish that swells in the middle and curls at both ends.",
  rinceau      = "A running scroll: a wavy stem with a spiral curl at every crest and trough.",
  greek_key    = "The square meander: one continuous line winding into a rectangular spiral, tile after tile.",
  scallop      = "A row of arches, each with a smaller arch inside and a bead where neighbours meet.",
  compass_rose = "A star of long and short rays with two-tone shading, a tick ring and a north marker.",
  guilloche    = "The woven engraved rosette of banknotes: many wavy circles, each rotated a little.",
  rosette      = "A flower-like medallion of alternating petal layers around a jewelled centre.",
  sunburst     = "Alternating long and short rays around a ringed hub, in the Art Deco manner.",
  corner       = "A bracket for the corner of a page or frame, ending in inward-turning curls.",
  asterism     = "The small break between sections: three stars in a row, or in a triangle."
)

cat_tbl <- dk_ornaments()
families <- c(rule = "Rules", border = "Borders", medallion = "Medallions",
              corner = "Corners", mark = "Marks", use = "In use")

cards <- vapply(seq_len(nrow(cat_tbl)), function(i) {
  id <- cat_tbl$id[i]
  sh <- shows[[id]]
  items <- vapply(seq_len(n_seeds), function(s) {
    svg <- ornament_svg(sh$fn(s), width = sh$size[1], height = sh$size[2])
    sprintf('<figure data-i="%d">%s<figcaption><code>%s</code></figcaption></figure>',
            s, svg, sh$code(s))
  }, "")
  sprintf(paste0(
    '<section class="kind" data-family="%s" id="%s"><header><h2>%s</h2><p>%s</p></header>',
    '<div class="row row-%s">%s</div></section>'),
    cat_tbl$family[i], id, cat_tbl$label[i], blurb[[id]], cat_tbl$family[i],
    paste(items, collapse = ""))
}, "")

# "In use": the ornaments framing a ggplot2 plot, bordering a panel, and in gt.
plot_svg <- function(x, width, height) {
  getp <- svglite::svgstring(width = width, height = height, bg = "white", standalone = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(x)
  paste(getp(), collapse = "")
}
library(ggplot2)
base_plot <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point(colour = "#3b3b3b", size = 1.8) +
  labs(title = "Fuel economy by weight", x = "Weight (1000 lb)", y = "Miles per gallon") +
  theme_minimal(base_size = 9)
framed <- ornament_frame(base_plot, top = "greek_key", bottom = "greek_key",
                         left = "scallop", right = "scallop", corners = TRUE,
                         thickness = 0.36)
panelled <- base_plot +
  scale_y_continuous(expand = expansion(mult = 0.14)) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  panel_ornament(top = "rinceau", bottom = "rinceau", corners = TRUE, thickness = 0.32)
gt_tbl <- gt::gt(head(mtcars[, 1:5], 5), rownames_to_stub = TRUE) |>
  gt_ornament_frame(top = "swash", bottom = "fleuron_rule",
                    title = "Motor Trend cars", subtitle = "Five models, five measures") |>
  gt::tab_options(table.font.names = "Georgia", table.width = gt::pct(100),
                  table.font.size = gt::px(13))
use_items <- list(
  list(svg = plot_svg(framed, 6, 4.2), code = "ornament_frame(p, top = \"greek_key\", left = \"scallop\", corners = TRUE)"),
  list(svg = plot_svg(panelled, 6, 4.2), code = "p + panel_ornament(top = \"rinceau\", bottom = \"rinceau\", corners = TRUE)"),
  list(svg = gt::as_raw_html(gt_tbl, inline_css = TRUE), code = "gt_ornament_frame(top = \"swash\", bottom = \"fleuron_rule\", title = ...)"))
use_cards <- sprintf('<figure data-i="%d">%s<figcaption><code>%s</code></figcaption></figure>',
                     seq_along(use_items), vapply(use_items, `[[`, "", "svg"),
                     vapply(use_items, `[[`, "", "code"))
use_section <- sprintf(paste0(
  '<section class="kind" data-family="use" data-static id="use"><header><h2>In ggplot2 and gt</h2>',
  '<p>Frame a whole plot, border just its panel, or dress a table: border ornaments fit themselves to each side.</p></header>',
  '<div class="row row-use">%s</div></section>'), paste(use_cards, collapse = ""))
cards <- c(cards, use_section)

palettes <- list(
  Gilt     = c("#C9A227", "#D9B75C", "#9E7C2C"),
  Indigo   = c("#2F4B7C", "#8FA8D6", "#1F3358"),
  `Ember and moss` = c("#8A3B1E", "#C9A56B", "#4F5B2E"),
  Plum     = c("#6B2D5C", "#C79BB8", "#4A1F40"),
  Slate    = c("#3F4A52", "#A5B3BC", "#2A3238"),
  Jade     = c("#1F7A6A", "#8FCDBF", "#14524A")
)
pal_js <- jsonlite::toJSON(unname(palettes), auto_unbox = FALSE)
pal_names <- names(palettes)
sent_js <- jsonlite::toJSON(unname(sent))

css <- '
:root{--bg:#f6f1e7;--paper:#fffdf8;--ink:#2a2620;--muted:#7a7164;--line:#e2d9c6;--accent:#8a3b1e}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){--bg:#17150f;--paper:#201d15;--ink:#ece4d2;--muted:#9a917f;--line:#33301f;--accent:#e0a96d}}
:root[data-theme="dark"]{--bg:#17150f;--paper:#201d15;--ink:#ece4d2;--muted:#9a917f;--line:#33301f;--accent:#e0a96d}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font:17px/1.55 "Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif}
.wrap{max-width:1080px;margin:0 auto;padding:0 16px}
.hero{padding:56px 0 24px;text-align:center}
.hero .mark{width:min(420px,90%);margin:0 auto 8px}.hero .mark svg{width:100%;height:auto}
.hero h1{font-size:clamp(34px,6vw,54px);font-weight:500;margin:.1em 0 .15em;letter-spacing:.01em}
.hero p{color:var(--muted);max-width:620px;margin:0 auto}
.bar{position:sticky;top:0;z-index:5;background:var(--bg);border-bottom:1px solid var(--line);padding:10px 0;margin-top:20px}
.bar .wrap{display:flex;flex-wrap:wrap;gap:10px 18px;align-items:center;justify-content:space-between}
.tabs,.pals{display:flex;flex-wrap:wrap;gap:4px 14px;align-items:center}
button{font:inherit;font-size:.82em;background:none;border:0;border-bottom:2px solid transparent;color:var(--muted);cursor:pointer;padding:3px 0;letter-spacing:.06em;text-transform:uppercase}
button[aria-pressed="true"]{color:var(--ink);border-bottom-color:var(--accent)}
button:hover{color:var(--ink)}
.sw{width:20px;height:20px;border-radius:50%;border:2px solid var(--line);padding:0;text-transform:none}
.sw[aria-pressed="true"]{border-color:var(--ink)}
.btn{border:1px solid var(--line);border-radius:4px;padding:5px 12px;color:var(--ink);background:var(--paper)}
.btn:hover{border-color:var(--accent)}
.kind{padding:34px 0 18px;border-bottom:1px solid var(--line)}
.kind header h2{font-size:1.5em;font-weight:500;margin:0}
.kind header p{color:var(--muted);margin:.15em 0 1em;font-size:.95em;max-width:640px}
.row{display:grid;gap:14px;grid-template-columns:repeat(3,minmax(0,1fr))}
.row-rule,.row-border{grid-template-columns:minmax(0,1fr)}
.row-mark{grid-template-columns:repeat(3,minmax(0,1fr))}
.row-use{grid-template-columns:repeat(2,minmax(0,1fr))}.row-use figure:nth-child(3){grid-column:1/-1;max-width:640px;margin:0 auto;width:100%}.row-use figure svg{max-height:none}.row-use figure .gt_table{margin:0 auto!important}
figure{min-width:0;margin:0;background:var(--paper);border:1px solid var(--line);border-radius:6px;padding:14px 12px 8px;text-align:center}
svg{max-width:100%}figure svg{width:100%;height:auto;max-height:230px}
.row-medallion figure svg,.row-corner figure svg{max-width:230px}
figcaption{font-size:.7em;color:var(--muted);margin-top:6px}
code{font:.95em ui-monospace,SFMono-Regular,Menlo,monospace}
footer{padding:40px 0 70px;color:var(--muted);font-size:.9em;text-align:center}
footer a{color:var(--accent)}
@media (max-width:700px){.row-use{grid-template-columns:minmax(0,1fr)}.row-medallion,.row-corner,.row-mark{grid-template-columns:repeat(3,minmax(0,1fr));gap:8px}figure{padding:8px 4px 4px}figcaption{display:none}}
'

js <- sprintf('
const PALS = %s, SENT = %s, N = %d;
let pal = 0, fam = "all";
const kinds = [...document.querySelectorAll(".kind")];
const orig = new Map();
document.querySelectorAll("figure").forEach(f => orig.set(f, f.innerHTML));
function recolor(){
  const p = PALS[pal];
  document.querySelectorAll("figure").forEach(f => {
    let h = orig.get(f);
    SENT.forEach((c, i) => { h = h.replace(new RegExp(c.slice(1), "gi"), p[i].slice(1)); });
    f.innerHTML = h;
  });
}
function pick(k){
  if (k.hasAttribute("data-static")) return;
  const figs = [...k.querySelectorAll("figure")];
  const per = k.dataset.family === "rule" || k.dataset.family === "border" ? 3 : 3;
  const idx = figs.map((_, i) => i).sort(() => Math.random() - .5).slice(0, per);
  figs.forEach((f, i) => f.hidden = !idx.includes(i));
}
function filter(){ kinds.forEach(k => k.hidden = !(fam === "all" || k.dataset.family === fam)); }
document.querySelectorAll("[data-fam]").forEach(b => b.onclick = () => {
  fam = b.dataset.fam;
  document.querySelectorAll("[data-fam]").forEach(x => x.setAttribute("aria-pressed", x === b));
  filter();
});
document.querySelectorAll("[data-pal]").forEach(b => b.onclick = () => {
  pal = +b.dataset.pal;
  document.querySelectorAll("[data-pal]").forEach(x => x.setAttribute("aria-pressed", x === b));
  recolor();
});
document.getElementById("new").onclick = () => { kinds.forEach(pick); recolor(); };
kinds.forEach(pick); filter();
', pal_js, sent_js, n_seeds)

tabs <- paste0(
  '<button data-fam="all" aria-pressed="true">All</button>',
  paste(sprintf('<button data-fam="%s" aria-pressed="false">%s</button>', names(families), families), collapse = ""))
pals <- paste(sprintf(
  '<button class="sw" data-pal="%d" aria-pressed="%s" title="%s" style="background:%s"></button>',
  seq_along(palettes) - 1L, c("true", rep("false", length(palettes) - 1L)),
  pal_names, vapply(palettes, `[`, "", 1)), collapse = "")

hero_mark <- ornament_svg(fleuron_rule_grob(seed = 7, ends = "fleuron"), width = 6, height = 0.7)

html <- sprintf('<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>delightkitr gallery</title>
<meta name="description" content="Small, purposeless decorative flourishes for R: rules, borders, medallions and corners, drawn from geometry.">
<style>%s</style></head><body>
<div class="wrap">
<div class="hero"><div class="mark" aria-hidden="true">%s</div>
<h1>delightkitr</h1>
<p>Small, purposeless decorative flourishes for R, drawn from geometry alone. Every ornament takes a <code>seed</code>, so the same seed always gives the same flourish and a new one gives a new sibling.</p></div>
</div>
<div class="bar"><div class="wrap">
<div class="tabs" role="group" aria-label="Family">%s</div>
<div class="pals" role="group" aria-label="Ink">%s<button class="btn" id="new">New seeds</button></div>
</div></div>
<main class="wrap">%s</main>
<footer class="wrap">Generated by <code>data-raw/build-gallery.R</code> with the package&rsquo;s own <code>ornament_svg()</code>.<br>
<a href="https://github.com/DavidBrocker/delightkitr">github.com/DavidBrocker/delightkitr</a><br><br>
Design principle &mdash; small ornament kinds generated from geometry with seeded variation &mdash; follows
<a href="https://github.com/wn-mitch/tikz-flora">tikz-flora</a> by wn-mitch (MIT). The shapes here are independently written in R.</footer>
<script>%s</script></body></html>',
  css, hero_mark, tabs, pals, paste(cards, collapse = "\n"), js)

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
writeLines(html, out_file, useBytes = TRUE)
message(sprintf("Wrote %s (%.1f MB)", out_file, file.size(out_file) / 1e6))
