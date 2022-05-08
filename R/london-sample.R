#' Generate colour palette from image
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
london.sample <- function(path, n = 4, type = "discrete", direction = 1, plot = TRUE){
  #error handling
  if(is.null(path)){
    stop("Path is missing")
  }
  #read image
  tmp <- magick::image_read(path)
  #resize image for sampling and plotting
  tmp <- magick::image_resize(tmp, "500")
  #convert to img object
  img <- imager::magick2cimg(tmp)
  #convert to HSV
  img <- imager::RGBtoHSV(img)
  #convert to dataframe for easy extraction
  img <- as.data.frame(img, wide = "c")
  #retain unique values
  img <- unique(img[,3:5])
  #rescale hue
  img$c.1 <- scales::rescale(img$c.1, from=c(0,360))
  #order by saturation
  img <- img[order(img$c.2),]
  #get hex codes
  img <- hsv(img$c.1, img$c.2,img$c.3)
  #extract n colours for palette
  img <- switch(type,
                continuous = grDevices::colorRampPalette(img[ceiling(seq(from = 1, to = length(img), length.out = 3))])(n),
                discrete = img[ceiling(seq(from = 1, to = length(img), length.out = n))]
                )
  #sort sampled colours
  img <- sort(img)
  #reverse colour direction
  if(direction == -1){
    img <- rev(img)
  }
  #plot colours
  if(plot == TRUE){
    par(mfrow=c(2,1), mar = c(0.1,0.1,0.1,0.1))
    plot(tmp)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='', ylim = c(0,1),xlim=c(0,length(img)))
    for(i in 1:length(img)){
      polygon(x = c(i-1,i-1,i,i), y = c(0.25,0.75,0.75,0.25), border = "white", lwd = 10, col = img[i])
      text(x = i-0.5,  y = 0.2, cex = 0.75, labels = img[i])}
    text(x = length(img)/2, y = 0.85, label = "Colour palette", cex = 1.25, font = 2)
    text(x = length(img)/2, y = 0.05, label = "Hex codes", cex = 1.25, font = 2)
  }
  return(img)
}
