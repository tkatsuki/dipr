#' Gaussian Kernel
#'
#' This function generates a Gaussian kernel of desired sizes, which might be useful for spatial and frequency filtering.
#' @param x,y Size of the kernel in x-axis and y-axis.
#' @param s Standard deviation, sigma.
#' @export
#' @examples
#' GaussianKernel()

GaussianKernel <- function(x, y, s=20){
  fn <- function(x, y, s) exp(-(x^2+y^2)/(2*s^2))/(2*pi*s^2)
  x <- seq(-floor(x/2), floor(x/2), len=x)
  y <- seq(-floor(y/2), floor(y/2), len=y)
  w <- outer(x, y, fn, s)
  w/sum(w)
}
