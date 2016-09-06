#' 2 byte raw to integer conversion
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' raw2intcpp()

raw2intR <- function(obj){
  val <- raw2intC(obj)
  val
}
