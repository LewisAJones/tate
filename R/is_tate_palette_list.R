#' Tate palette list object
#'
#' Tests for \code{tate_palette_list} object.
#'
#' @param x \code{list}. A list containing palette data.
#'
#' @return A `logical` output of the test.
#' @noRd
is_tate_palette_list <- function(x){
  if (class(x) == "tate_palette_list") {
    state <- TRUE
  } else { state <- FALSE}
  return(state)
}
