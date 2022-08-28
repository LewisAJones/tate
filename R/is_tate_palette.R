#' Tate palette object
#'
#' Tests for \code{tate_palette} object.
#'
#' @param x \code{vector}. A vector containing palette data.
#'
#' @return A `logical` output of the test.
#' @noRd
is_tate_palette <- function(x){
  if (class(x) == "tate_palette") {
    state <- TRUE
  } else { state <- FALSE}
  return(state)
}
