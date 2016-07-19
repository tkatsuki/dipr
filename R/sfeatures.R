sfeatures <- function(obj, ref=0){
  if(length(ref)!=1 & !identical(dim(obj), dim(ref))) stop("Dimension mismatch.")
  width <- dim(obj)[1]
  height <- dim(obj)[2]
  nframes <- getNumberOfFrames(obj)
  sfeaturesC(obj, ref, width, height, nframes)
}
