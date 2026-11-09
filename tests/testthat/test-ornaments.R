draw_ok <- function(g) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(g)
  TRUE
}

test_that("compass_rose_grob() draws for several tick counts", {
  for (n in c(4, 8, 12, 72)) {
    expect_true(draw_ok(compass_rose_grob(n_ticks = n)))
  }
  expect_s3_class(compass_rose_grob(), "compass_rose")
  expect_error(compass_rose_grob(points = 3))
})

test_that("endcap_grob() supports every kind", {
  for (k in c("dot", "diamond", "fleuron", "none")) {
    expect_true(draw_ok(endcap_grob(k)))
  }
  expect_error(endcap_grob("star"))
})

test_that("rules draw and validate their arguments", {
  expect_true(draw_ok(swelled_rule_grob(seed = 1)))
  expect_true(draw_ok(fleuron_rule_grob(seed = 1)))
  expect_true(draw_ok(fleuron_rule_grob(seed = 1, beads = 0, centre = "none")))
  expect_error(swelled_rule_grob(width = 2))
  expect_error(fleuron_rule_grob(variation = 2))
})

test_that("a seed gives the same rule, different seeds differ", {
  a <- swelled_rule_grob(seed = 3)
  b <- swelled_rule_grob(seed = 3)
  expect_identical(grid::convertUnit(a$children[[1]]$y, "npc", valueOnly = TRUE),
                   grid::convertUnit(b$children[[1]]$y, "npc", valueOnly = TRUE))
  ya <- as.character(a$children[[1]]$y)
  yc <- as.character(swelled_rule_grob(seed = 4)$children[[1]]$y)
  expect_false(identical(ya, yc))
})

test_that("seeding does not touch the global RNG", {
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  invisible(swelled_rule_grob(seed = 9))
  invisible(fleuron_rule_grob(seed = 9))
  expect_identical(runif(1), before)
})
