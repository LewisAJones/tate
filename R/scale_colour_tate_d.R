#' Discrete tate palettes for plotting with ggplot2
#'
#' A function to use colour palettes from `tate` with `ggplot2`. Use
#' \code{\link{scale_colour_met_d}} and \code{\link{scale_fill_met_d}} for
#' discrete scales, and \code{\link{scale_colour_met_c}} and
#' \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param name \code{character}. Name of desired palette. Either:
#' \code{Cholmondeley}, \code{Copley}, \code{Blake}, \code{Turner},
#' \code{Millais}, \code{Sargent}, \code{Waterhouse}, \code{Grant},
#' \code{Lowry}, \code{Bacon} or \code{Hockney}.
#' @param direction \code{numeric}. Sets the order of colours in the scale.
#' Default order is 1. Palette is reversed by specifying -1.
#' @param ... Other arguments passed on to
#' \code{\link[ggplot2]{scale_colour_gradientn}}
#' @import ggplot2
#' @examples
#' # Load libraries
#' library(ggplot2)
#' library(palmerpenguins)
#' # Plot data
#' ggplot(data = penguins,
#'        aes(x = bill_length_mm, y = bill_depth_mm, colour = species)) +
#'   geom_point() +
#'   scale_colour_tate_d("Millais", direction = 1)
#' @export
scale_colour_tate_d <- function(name, direction = 1, ...) {
  tate_discrete <- function(name, direction = 1) {

    pal <- tate_palettes[[name]]

    if (is.null(palette)){
      stop("Palette does not exist.")
    }

    if (!direction %in% c(1, -1)){
      stop("Palette direction not valid. Use 1 (standard) or -1 (reversed).")
    }

    function(n) {
      if (direction == 1) {
        pal <- pal$palette[
          round(seq(from = 1, to = length(pal$palette), length.out = n))]
      } else {
        pal <- pal$palette[
          round(seq(from = length(pal$palette), to = 1, length.out = n))]
      }
      pal
    }
  }
  discrete_scale(aesthetics = "colour", scale_name = "tate_d",
                 palette = tate_discrete(name = name, direction = direction))
}
