#' LoG calculation
#'
#' @param x x-axis kernel size
#' @param y y-axis kernel size
#' @param s Sigma
#' @export
#' @examples
#' LoG()

LoG <- function(x,y,s){
  fn <- function(x, y, s) 1/(pi*s^4)*((x^2 + y^2)/(2 * s^2)-1)*exp(-(x^2 + y^2)/(2*s^2))
  x <- seq(-floor(x/2), floor(x/2), len = x)
  y <- seq(-floor(y/2), floor(y/2), len = y)
  w <- outer(x, y, fn, s)
  w
}