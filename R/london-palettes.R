#' List of colour palettes
#'
#'
#' Complete list of palettes
#'
#' Use \code{\link{london.palette}} to construct palettes of desired length.
#'
#' @export
london.palettes <- list(
  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707")
)
#'
#' This function allows you to generate a colour palette from an image.
#' @param path The file path or url of the image you wish to sample colours from.
#' @param n The number of colours to sample from the image. If n is not provided, the length of the palette defaults to 4.
#' @param type Specify either "discrete" or "continuous". If "continuous", three colours are sampled from the image and automatically interpolated between. Defaults to "discrete".
#' @param direction Specify the order of colours. Defaults to 1. If direction is -1, palette is reversed.
#' @param plot (Logical) Plot sampled colour palette. Defaults to TRUE.
#' @import magick imager scales
#' @export
#' @examples
#' url <- "https://t4.ftcdn.net/jpg/02/90/91/11/240_F_290911157_rJMt9bp5sq64hyY6TklM9XS5GoDg07Vh.jpg"
#' london.sample(path = url, n = 4, type = "discrete")
#'
