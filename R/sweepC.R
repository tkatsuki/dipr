#' Fast sweep function
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @param op An operator.
#' @export
#' @examples
#' ssweep()

ssweep <- function(obj, ref, op){
  if(op == "-") {
    sweepC(obj, ref, dim(obj), 1)
  } else if(op == "*") {
    sweepC(obj, ref, dim(obj), 2)
  }
}
