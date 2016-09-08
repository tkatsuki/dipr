#' raw to integer conversion
#'
#' @param rawdata A target image of Image object or an array.
#' @export
#' @examples
#' raw2num()

raw2num <- function(rawdata) {
  return(as.numeric(paste("0x", paste(rawdata, collapse=""), sep="")))
}
