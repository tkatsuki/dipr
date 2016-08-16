#' Convert fmf file format to tif format
#'
#' @param filename fmf file name
#' @param skip Skip frames
#' @export
#' @examples
#' fmf2tif()
#'
fmf2tif <- function(filename, skip=0){
  print("Converting an fmf file to tif...")
  fmfimg <- readFMF(file=filename, skip=skip)
  prefix <- substr(filename, 1, nchar(filename) - 4)
  EBImage::writeImage(fmfimg/255, paste0(prefix, ".tif"))
}
