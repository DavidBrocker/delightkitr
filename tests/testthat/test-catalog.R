draw_in_box <- function(g, w = 4, h = 1) {
  grDevices::pdf(NULL, width = w, height = h)
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(g)
  TRUE
}

test_that("the catalogue lists every ornament once", {
  cat_tbl <- dk_ornaments()
  expect_s3_class(cat_tbl, "data.frame")
  expect_equal(anyDuplicated(cat_tbl$id), 0)
  expect_true(all(c("id", "label", "family", "ratio", "seeded") %in% names(cat_tbl)))
  expect_true(all(cat_tbl$family %in% c("rule", "border", "medallion", "corner", "mark")))
})

test_that("dk_ornament() builds every catalogue entry", {
  for (id in dk_ornaments()$id) {
    ratio <- dk_ornaments()$ratio[dk_ornaments()$id == id]
    args <- if (id == "compass_rose") list() else list(seed = 1)
    g <- do.call(dk_ornament, c(list(id), args))
    expect_s3_class(g, "gTree")
    expect_true(draw_in_box(g, w = 2 * ratio^0.5, h = 2 / ratio^0.5))
  }
  expect_error(dk_ornament("nope"), "must be one of")
})

test_that("self-fitting ornaments adapt to very different boxes", {
  for (fn in list(swash_grob, rinceau_grob, greek_key_grob, scallop_grob)) {
    g <- fn(seed = 3)
    expect_true(draw_in_box(g, w = 8, h = 0.8))
    expect_true(draw_in_box(g, w = 1.2, h = 1.2))
    expect_true(draw_in_box(g, w = 0.6, h = 3))
  }
})

test_that("the same seed gives identical output, different seeds differ", {
  svg <- function(id, seed) ornament_svg(dk_ornament(id, seed = seed), 3, 1)
  for (id in setdiff(dk_ornaments()$id, "compass_rose")) {
    expect_identical(svg(id, 5), svg(id, 5))
    expect_false(identical(svg(id, 5), svg(id, 6)), info = id)
  }
})

test_that("corner_grob() supports all four corners", {
  for (k in c("bl", "br", "tl", "tr")) {
    expect_true(draw_in_box(corner_grob(k, seed = 1), 2, 2))
  }
  expect_error(corner_grob("middle"))
})

test_that("ornaments embed in HTML and gt", {
  html <- ornament_html(fleuron_rule_grob(seed = 2), width = 4, height = 0.5)
  expect_match(html, "<svg")
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars[, 1:3]))
  tbl <- gt_ornament(tbl, rinceau_grob(seed = 2), where = "header",
                     width = 4, height = 0.5)
  expect_s3_class(tbl, "gt_tbl")
})
