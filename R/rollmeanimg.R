#' Rolling mean calculation for images
#'
#' @param obj A target image of Image object or an array.
#' @param n Window size.
#' @export
#' @examples
#' rollmeanimg()
rollmeanimg <- function(obj, n){
  rollmeanimgC(obj, dim(obj), n)/n
}
