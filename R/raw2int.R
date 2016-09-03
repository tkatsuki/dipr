#' A raw2int Function
#'
#' This function converts raw to integer.
#'
#' @param rawdata raw value
#' @keywords raw
#' @export
#' @examples
#' raw2int()

raw2int <- function(rawdata) {
  return(as.integer(paste("0x", paste(rawdata, collapse=""), sep="")))
}
