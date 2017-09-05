#' Histogram of pixel intensitie
#'
#' This function displays a histogram of pixel intensities from images. This is a wrapper function for hist.
#' @param x An image of Image or imagedata class or an array.
#' @param breaks A single number giving the number of cells for the histogram.
#' @param xlim,ylim the range of x and y values with sensible defaults.
#' @param col A colour to be used to fill the bars.
#' @export
#' @examples
#' histogram()

histogram <- function(x, breaks=512, xlim=c(0, 1), ylim=NULL, main="", col="black") {
  if(is.Image(x)==T){
    hist(x@.Data, breaks=breaks, main=main, xlim=c(0, 1), xlab="Intensity", ylim=ylim, col=col)
  } else {
    if(class(x)[1]=="imagedata"){
      hist(x, breaks=256, main=main, xlim=c(0, 255), xlab="Intensity", ylim=ylim, col=col)
    } else {
      hist(x, breaks=breaks, main=main, xlim=xlim, xlab="Intensity", ylim=ylim, col=col)
      }
    }
}
