#' A readTIFF Function
#'
#' This function allows you to load a grayscale TIFF file.
#'
#' @param filepath A caracter string of the path to the file. Required.
#' @param start An integer of the start frame. Default = 1.
#' @param end An integer of the end frame. Default = 0 (last frame of the file).
#' @param skip An integer of skip frame. Default = 0 (read every frame).
#' @param getFrames Return number of frames. Default = False.
#' @param crop An integer vector depicting the xy coordinate of the top-left corner and the bottom-right corner you want to crop.
#' @param silent Whether or not show message. Default = False.
#' @keywords tiff
#' @export
#' @examples
#' readTIFF()

readTIFF <- function(filename, start=1, end=0, skip=0, crop=c(0,0,0,0), frames=NULL, getFrames=F) {
  imagetags <- data.frame()

  # Open a lsm file and close it
  con <- file(filename, open="rb")
  raw <- readBin(con, "raw", file.info(filename)$size)
  close(con)

  # Check header
  if(raw2int(rev(raw[0x3:0x4]))!=42) stop("This file is not TIFF.")

  # A function that returns integer value of a directory entry
  value <- function(tag1, tag2, IFD){
    tag <- raw2int(c(tag2, tag1))
    len <- raw2int(rev(IFD[1:2]))
    IFDmat <- matrix(IFD[3:(2+12*len)], ncol=12, byrow=T)
    tags <- apply(IFDmat[,2:1], 1, raw2int)
    val <- raw2int(rev(IFDmat[which(tags==tag),9:12]))
    val
  }

  # A function that collects image information tags.
  info <- function(offset, n, imageinfo){
    while (offset > 0) {
      imageinfosize <- raw[(offset+1):(offset+2)]
      tmpinfo <- (raw[(offset+1):(offset+12*raw2int(rev(imageinfosize))+2+4)])
      imageinfo <- append(imageinfo, list(tmpinfo))
      offset <- raw2int(rev(tail(tmpinfo, 4)))
    }
    return(imageinfo)
  }
  imagetags <- info(raw2int(rev(raw[0x5:0x8])), 1, imagetags)

  # Check compression
  if(value("03", "01", imagetags[[1]]) == 5) stop("Only uncompressed images can be read.")

  # Number of frames
  max_n_frames <- length(imagetags)
  if(getFrames==T) {
    return(max_n_frames)
  }

  if(end==0 | end > max_n_frames) end <- max_n_frames

  if(is.null(frames)){
    nf <- end - start + 1
    fr <- start:end
  } else {
    if(max(frames) > max_n_frames) stop("Incorrect frames!")
    nf <- length(frames)
    fr <- frames
  }

  # Number of channels
  nch <- value("15", "01", imagetags[[1]])

  # Size of the image
  w <- value("00", "01", imagetags[[1]])
  h <- value("01", "01", imagetags[[1]])

  # Bits per pixel
  if(nch==1) {
    bitspersample <- value("02", "01", imagetags[[1]])
  }else{
    bpsoffset <- value("02", "01", imagetags[[1]])
    bitspersample <- raw2int(rev(raw[(bpsoffset+1):(bpsoffset+2)]))
  }

  ByteGenerator <- function(i, j, bitspersample){

    # pixel data start point
    if(nch==1){
      px.start <- value("11", "01", imagetags[[j]]) + 1
    }else{
      px.start <- raw2int(rev(raw[(value("11", "01", imagetags[[j]])+1+(i-1)*4):
                                    (value("11", "01", imagetags[[j]])+4+(i-1)*4)]))+1
    }

    # Collect image data
    if(bitspersample==8){
      imagedata <- raw[px.start:(px.start-1+w*h)]
      matrix(as.numeric(imagedata), w, h)
    }
    if(bitspersample==16){
      imagedata <- raw[px.start:(px.start-1+w*h*2)]
      imagedata
    }
  }

  # Prepare a raw vector
  tmpdata <- raw(w*h*nch*nf*2)

  # Store image in the array
  if(nch==1){
    if(nf==1){
      tmpdata[1:(2*w*h)] <- ByteGenerator(1, 1, bitspersample)
      odd_col <- tmpdata[seq(1,2*w*h,2)]
      even_col <- tmpdata[seq(2,2*w*h,2)]
      outputimg <- matrix(as.integer(paste("0x", even_col, odd_col, sep="")), w, h)
    }else{
      for (j in 1:nf) {
        tmpdata[(2*w*h*(j-1)):(2*w*h*j)] <- ByteGenerator(1, fr[j], bitspersample)
      }
      odd_col <- tmpdata[seq(1,2*w*h,2)]
      even_col <- tmpdata[seq(2,2*w*h,2)]
      outputimg <- array(as.integer(paste("0x", even_col, odd_col, sep="")), dim=c(w, h, nf))
    }
  }else{
    if(nf==1){
      for(i in 1:nch){
        outputimg[,,i] <- ByteGenerator(i, 1, bitspersample)
      }
    }else{
      for (j in 1:nf) for(i in 1:nch){
        outputimg[,,i,j] <- ByteGenerator(i, fr[j], bitspersample)
      }
    }
  }

  # Return an array
  outputimg
}
