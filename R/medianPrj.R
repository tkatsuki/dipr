#' @export
#'
medianPrj <- function(img){
  if(!require(matrixStats)){
  install.packages("matrixStats")
  library("matrixStats")
  }
  row <- nrow(img[,,1])
  col <- ncol(img[,,1])
  img <- apply(img, 3, as.vector)
  img <- rowMedians(img)
  img <- matrix(img, row, col)
  img
}
