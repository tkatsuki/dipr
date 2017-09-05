#' Calculate distance matrics
#'
#' @param x1 x coordinate of the first point
#' @param y1 y coordinate of the first point
#' @param x2 x coordinate of the second point
#' @param y2 y coordinate of the second point
#' @export
#' @examples
#' distmat()

distmat <- function(x1, y1, x2, y2){
  x <- outer(x1, x2, function(c1, c2) (c1 - c2)^2)
  y <- outer(y1, y2, function(c1, c2) (c1 - c2)^2)
  sqrt(x + y)
}
