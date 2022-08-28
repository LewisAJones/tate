#' Display palette
#'
#' A function for visualising palettes.
#'
#' @param x \code{tate_palette} or \code{tate_palette_list}.
#' These objects are output by the
#' \code{\link[tate:tate_palette]{tate::tate_palette()}} or
#' \code{\link[tate:tate_sample]{tate::tate_sample()}} functions or can
#' be constructed via the
#' \code{\link[tate:tate_construct]{tate::tate_construct()}}.
#'
#' @return A plot of the input palette(s).
#'
#' @importFrom graphics polygon par image text
#' @importFrom grDevices rgb
#'
#' @export
#'
#' @examples
#' Hockney <- tate_palette(name = "Hockney")
#' tate_display(x = Hockney)
#' all_palettes <- tate_palette(name = "All")
#' tate_display(x = all_palettes)
tate_display <- function(x) {
  if (class(x) != "tate_palette_list" &&
      class(x) != "tate_palette") {
    stop(paste('x should be either a "tate_palette" or "tate_palette_list"',
               "\nUse `tate_construct()`"))
  }
  # length of palette
  n <- length(x)
  # Reset user par later
  user_par <- par()
  par(mar = c(0.1, 0.1, 0.1, 0.1), family = "mono", font = 2)

  if (is_tate_palette_list(x)) {
    # Number of palettes to plot
    n_pals <- 1:n
    # Set up layout
    layout(matrix(data = n_pals))
    for (j in n_pals) {
      tmp_pal <- x[[j]]
      # Plot palette
      plot(0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
           xlab = NA, ylab = NA,
           ylim = c(0, 1), xlim = c(0, length(tmp_pal)), main = NA)

      # Plot polygons
      for(i in 1:length(tmp_pal)){
        polygon(x = c(i-1,i-1,i,i), y = c(0,1,1,0), border = tmp_pal[i],
                col = tmp_pal[i])
      }

      graphics::polygon(x = c(0, 0, length(tmp_pal), length(tmp_pal)),
              y = c(0.4, 0.6, 0.6, 0.4),
              col = grDevices::rgb(1, 1, 1, 0.8), border = NA)
      text(x = length(tmp_pal) / 2, y = 0.5, labels = attr(tmp_pal, "name"),
           cex = 1.25)
    }

  } else if(is_tate_palette(x)) {
  # Plot palette
  plot(0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
       ylim = c(0, 1), xlim = c(0, n), main = NA)

  # Plot polygons
  for(i in 1:n){
    graphics::polygon(x = c(i-1,i-1,i,i), y = c(0,1,1,0),
                      border = x[i], col = x[i])
  }

  graphics::polygon(x = c(0, 0, n, n), y = c(0.4, 0.6, 0.6, 0.4),
          col = rgb(1, 1, 1, 0.8), border = NA)
  text(x = n / 2, y = 0.5, labels = attr(x, "name"), cex = 1)
  }
  # Reset layout and par
  layout(matrix(1,1,1))
  par(mar = user_par$mar, family = user_par$family, font = user_par$font)
}
