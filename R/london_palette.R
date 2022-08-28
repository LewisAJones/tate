#' Generate colour palette
#'
#' A function to generate a colour palette based on the available palettes in
#' `london`.
#'
#' @param name \code{character}. Name of desired palette. Either:
#' \code{Cholmondeley}, \code{Copley}, \code{Blake}, \code{Turner},
#' \code{Millais}, \code{Sargent}, \code{Waterhouse}, \code{Grant},
#' \code{Lowry}, \code{Bacon}, \code{Hockney} or \code{All}, if all are desired.
#' @param n \code{numeric}. Number of requested colours. If no value is
#' provided, the default number of colours is returned.
#' @param type \code{character}. Type of colour scale.
#' Either "continuous" or "discrete". Use "continuous" if you want
#' to automatically interpolate between colours.
#' @param direction \code{numeric}. Sets the orders of colours in the scale.
#' Default order is 1. Palette is reversed by specifying -1.
#' @param display \code{logical}. Should the colour palette be displayed?
#'
#' @return A vector of hex colour codes.
#'
#' @importFrom grDevices colorRampPalette
#' @examples
#' london_palette(name = "Hockney", display = TRUE)
#' london_palette(name = "Sargent", n = 90, type = "continuous", display = TRUE)
#' london_palette(name = "All", display = TRUE)
#' @keywords colours
#' @export
london_palette <- function(name, n = NULL, type = "discrete", direction = 1,
                           display = FALSE) {

  # Error handling ----------------------------------------------------------
  if (missing(name)) {
    stop("`name` has not been supplied.")
  }
  # If ALL required, call function
  if (name == "All") {
    pal <- lapply(names(london_palettes), function(x){
      if (is.null(n)) {
        n <- length(london_palettes[[x]]$palette)
      }
      pal <- london_palette(name = x, n = n, type = type,
                            direction = direction, display = FALSE)
    })
    names(pal) <- names(london_palettes)
    pal <- structure(pal, class = "london_palette_list",
                     name = "london_palette")
    if (display == TRUE) {
      london_display(x = pal)
    }
    return(pal)
  }

  # Extract desired palette
  pal <- london_palettes[[name]]
  # Does the palette exist?
  if (is.null(pal)) {
    stop("Specified palette not found. Please check spelling.")
  }
  # If n does not exist using default length of palette
  if (is.null(n)) {
    n <- length(pal$palette)
  }
  # Is n a numeric value?
  if (!is.numeric(n)) {
    stop("`n` should be a numeric value")
  }
  # Is specified type available?
  if (!type %in% c("discrete", "continuous")) {
    stop('`type` should be either "discrete" or "continuous"')
  }
  # Is direction available?
  if (!direction %in% c(1, -1)) {
    stop('`direction` should be either 1 or -1')
  }
  # Is display logical?
  if (!is.logical(display)) {
    stop('`display` should be either TRUE/FALSE')
  }
  # Enough values in discrete scale?
  if (type == "discrete" && n > length(pal$palette)) {
    stop(paste("`n` is greater than the available colours for this palette: "),
         length(pal$palette),
         '\nUser may wish to switch to "continuous" `type`')
  }
  # Generate palette -----------------------------------------------------------
  # Continuous scale
  if (type == "continuous") {
    if (direction == 1) {
      pal <- grDevices::colorRampPalette(pal$palette)(n)
    } else {
      pal <- grDevices::colorRampPalette(rev(pal$palette))(n)
    }
  }
  # Discrete scale
  if (type == "discrete") {
    if (direction == 1) {
      pal <- pal$palette[
        round(seq(from = 1, to = length(pal$palette), length.out = n))]
    } else {
      pal <- pal$palette[
        round(seq(from = length(pal$palette), to = 1, length.out = n))]
    }
  }
  pal <- structure(pal, class = "london_palette", name = name)
  # Wrap-up and return palette -------------------------------------------------
  # Display palette?
  if (display == TRUE) {
    london_display(x = pal)
  }
  return(pal)
}
