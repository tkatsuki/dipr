#' Conversion between different file formats
#'
#' This function converts all image files in a directory from one file format to other using \code{readImage} and \code{writeImage} function of EBImage.
#' @param dirname Path to a directory.
#' @param format1 A string, giving an input file extension.
#' @param format2 A string, giving an output file extension.
#' @param quality A numeric, ranging from 1 to 100. Default is 100.
#' @export
#' @examples
#' imgconv()

imgconv <- function(dirname=0, format1, format2, quality=100){
  if(dirname == 0){
    dirname <- readline("Enter a directory name:")
  }
  filelist <- list.files(dirname, pattern=format1)
  filelist <- substr(filelist, 1, nchar(filelist)-4)
	for (i in 1:length(filelist)){
    img <- readImage(paste("./", dirname, "/", filelist[i], ".",
                           format1, sep=""))
    writeImage(img, file=paste("./", dirname, "/", filelist[i], ".",
                               format2, sep=""), quality=quality)
	}
}
