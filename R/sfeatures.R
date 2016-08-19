#' Moment calculation
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' sfeatures()

sfeatures <- function(obj, ref=0){
  if(length(ref)!=1 & !identical(dim(obj), dim(ref))) stop("Dimension mismatch.")
  width <- dim(obj)[1]
  height <- dim(obj)[2]
  nframes <- EBImage::numberOfFrames(obj)
  sfeaturesC(obj, ref, width, height, nframes)
}
