render <- function(x, w = 6, h = 4) {
  grDevices::pdf(NULL, width = w, height = h)
  on.exit(grDevices::dev.off())
  print(x)
  TRUE
}

test_that("ornament_frame() frames a ggplot in many configurations", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_s3_class(ornament_frame(p, top = "greek_key"), "dk_framed")
  expect_true(render(ornament_frame(p, top = "scallop", bottom = "scallop",
                                    left = "rinceau", right = "rinceau",
                                    corners = TRUE)))
  expect_true(render(ornament_frame(p, top = "swash", bottom = "fleuron_rule")))
  expect_true(render(ornament_frame(p, corners = "corner")))
  expect_true(render(ornament_frame(p, top = "greek_key", left = "greek_key"),
                     w = 3, h = 7))
  # a grob works as a side, and a plain grob works as content
  expect_true(render(ornament_frame(grid::rectGrob(), top = swelled_rule_grob(seed = 1))))
})

test_that("ornament_frame() validates its inputs", {
  expect_error(ornament_frame("nope"), "ggplot or a grid grob")
  expect_error(ornament_frame(grid::rectGrob(), top = 3), "NULL, an ornament id, or a grob")
  expect_error(ornament_frame(grid::rectGrob(), top = "nope"), "must be one of")
})

test_that("panel_ornament() adds a layer that renders", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    panel_ornament(top = "scallop", bottom = "greek_key", left = "rinceau",
                   corners = TRUE)
  expect_s3_class(p, "ggplot")
  expect_true(render(p))
})

test_that("frames are reproducible for a seed", {
  skip_if_not_installed("svglite")
  svg <- function(seed) ornament_svg(
    dk_frame_grob(NULL, top = "swelled_rule", bottom = "rinceau", seed = seed), 4, 2)
  expect_identical(svg(2), svg(2))
  expect_false(identical(svg(2), svg(3)))
})

test_that("ornament_html(fluid = TRUE) scales to its container", {
  g <- fleuron_rule_grob(seed = 1)
  fixed <- ornament_html(g, 4, 0.5)
  fluid <- ornament_html(g, 4, 0.5, fluid = TRUE)
  expect_match(fixed, "width='[0-9.]+pt'")
  expect_match(fluid, "width:100%")
  expect_match(fluid, "max-width:4in")
  expect_false(grepl("width='[0-9.]+pt'", fluid))
})

test_that("gt_ornament_frame() puts ornaments above and below the table", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars[, 1:3])) |>
    gt_ornament_frame(top = "swash", bottom = "scallop", title = "Cars",
                      subtitle = "A few of them")
  expect_s3_class(tbl, "gt_tbl")
  html <- gt::as_raw_html(tbl)
  expect_equal(lengths(regmatches(html, gregexpr("<svg", html))), 2L)
  expect_match(html, "Cars")
  expect_match(html, "A few of them")
  # only a top ornament, no title
  top_only <- gt::gt(head(mtcars[, 1:3])) |> gt_ornament_frame(bottom = NULL)
  expect_equal(lengths(regmatches(gt::as_raw_html(top_only),
                                  gregexpr("<svg", gt::as_raw_html(top_only)))), 1L)
})
