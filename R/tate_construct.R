#' Construct a tate palette object.
#'
#' Constructs a \code{tate_palette} or \code{tate_palette_list} object.
#'
#' @param x \code{vector/list}. A one-component vector or list with each
#' element containing the palette colours.
#' @param name \code{character}. Name(s) of the palette(s).
#'
#' @return A `tate_palette` or `tate_palette_list` object.
#'
#' @export
tate_construct <- function(x, name){
  if (is.list(x)) {
    x <- lapply(1:length(x), function(i) {
      structure(x[[i]], class = "tate_palette", name = name[[i]])
    })
    x <- structure(x, class = "tate_palette_list",
                     name = "tate_palette")
  } else {
    pal <- structure(x, class = "tate_palette", name = name)
  }
}
