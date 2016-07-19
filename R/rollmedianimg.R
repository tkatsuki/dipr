#' Rolling median calculation for images
#'
#' @param obj A target image of Image object or an array.
#' @param n Window size.
#' @export
#' @examples
#' rollmedianimg()
rollmedianimg <- function(obj, n){
  rollmedianimgC(obj, dim(obj), n)/n
}
