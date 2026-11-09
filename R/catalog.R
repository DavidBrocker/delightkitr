#' The catalogue of ornaments
#'
#' A table of every ornament kind in the package: its id, a readable label, its
#' family, the box shape it looks best in, and whether a `seed` varies it.
#' Use [dk_ornament()] to build one by id.
#'
#' @return A data frame with columns `id`, `label`, `family` (`"rule"`,
#'   `"border"`, `"medallion"`, `"corner"` or `"mark"`), `ratio` (suggested
#'   box width / height), and `seeded`.
#' @examples
#' dk_ornaments()
#' @export
dk_ornaments <- function() {
  data.frame(
    id = c("swelled_rule", "fleuron_rule", "swash", "rinceau", "greek_key",
           "scallop", "compass_rose", "guilloche", "rosette", "sunburst",
           "corner", "asterism"),
    label = c("Swelled rule", "Fleuron rule", "Pen swash", "Rinceau",
              "Greek key", "Scallop border", "Compass rose", "Guilloche",
              "Petal rosette", "Sunburst", "Corner flourish",
              "Dinkus / asterism"),
    family = c("rule", "rule", "rule", "border", "border", "border",
               "medallion", "medallion", "medallion", "medallion",
               "corner", "mark"),
    ratio = c(8, 8, 6, 8, 10, 10, 1, 1, 1, 1, 1, 3),
    seeded = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
               TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

#' Build an ornament by id
#'
#' A single entry point for every ornament in [dk_ornaments()]: `dk_ornament("rosette",
#' seed = 3)` is the same as `rosette_grob(seed = 3)`.
#'
#' @param id An ornament id from [dk_ornaments()].
#' @param ... Passed on to the ornament's constructor (for example `seed`,
#'   `ink`).
#' @return A grid grob.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(dk_ornament("rosette", seed = 3))
#' @export
dk_ornament <- function(id, ...) {
  ids <- dk_ornaments()$id
  if (!is.character(id) || length(id) != 1L || !id %in% ids) {
    stop("`id` must be one of: ", paste(ids, collapse = ", "), call. = FALSE)
  }
  fn <- switch(id,
    swelled_rule = swelled_rule_grob, fleuron_rule = fleuron_rule_grob,
    swash = swash_grob, rinceau = rinceau_grob, greek_key = greek_key_grob,
    scallop = scallop_grob, compass_rose = compass_rose_grob,
    guilloche = guilloche_grob, rosette = rosette_grob,
    sunburst = sunburst_grob, corner = corner_grob, asterism = asterism_grob)
  fn(...)
}
