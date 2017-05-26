#' A readTIFF2 Function
#'
#' This function allows you to load a TIFF file.
#'
#' @param filepath A caracter string of the path to the file. Required.
#' @param start An integer of the start frame. Default = 1.
#' @param end An integer of the end frame. Default = 0 (last frame of the file).
#' @param frames A vector specifying which frames to be loaded.
#' @param getFrames Return number of frames. Default = False.
#' @param crop Not implemented yet.
#' @param intensity Whether or not return mean intensity of each frame. Default = False.
#' @keywords tiff
#' @export
#' @examples
#' readTIFF2()

readTIFF2 <- function(filename, start=1, end=0, crop=c(0,0,0,0), frames=NULL, getFrames=F, intensity=F) {
  ## TO DO
  ## Implement cropping

  imagetags <- data.frame()

  # Open a tiff file
  con <- file(filename, open="rb")
  init_raw <- readBin(con, "raw", 8)

  # Check header
  if(raw2num(rev(init_raw[0x3:0x4]))!=42) stop("This file is not TIFF.")

  # A function that returns integer value of a directory entry
  value <- function(tag1, tag2, IFD){
    tag <- raw2num(c(tag2, tag1))
    len <- raw2num(rev(IFD[1:2]))
    IFDmat <- matrix(IFD[3:(2+12*len)], ncol=12, byrow=T)
    tags <- apply(IFDmat[,2:1], 1, raw2num)
    if(length(which(tags==tag))==0) val <- NA
    else{
      val <- raw2num(rev(IFDmat[which(tags==tag),9:12]))
    }
    val
  }

  # A function that collects image information tags.
  info <- function(offset, n, imageinfo){
    while (offset > 0) {
      seek(con, where = offset, origin="start")
      imageinfosize <-  readBin(con, "raw", 2)
      seek(con, where = offset, origin="start")
      tmpinfo <- readBin(con, "raw", (12*raw2num(rev(imageinfosize))+2+4))
      imageinfo <- append(imageinfo, list(tmpinfo))
      offset <- raw2num(rev(tail(tmpinfo, 4)))
    }
    return(imageinfo)
  }
  imagetags <- info(raw2num(rev(init_raw[0x5:0x8])), 1, imagetags)

  # Check compression
  if(is.na(value("03", "01", imagetags[[1]]))==F & value("03", "01", imagetags[[1]]) == 5) stop("Only uncompressed images can be read.")

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
    seek(con, where=bpsoffset, origin="start")
    bitspersample <- raw2num(rev(readBin(con, "raw", 2)))
  }
  print(paste0(bitspersample, " bit image."))

  ByteGenerator <- function(i, j, bitspersample, intensity=F){
    # pixel data start point
    if(nch==1){
      px.start <- value("11", "01", imagetags[[j]])
    }else{
      seek(con, where=(value("11", "01", imagetags[[j]])+(i-1)*4), origin="start")
      px.start <- raw2num(rev(readBin(con, "raw", 4)))
    }

    # Collect image data
    if(bitspersample==8){
      seek(con, where=px.start, origin="start")
      imagedata <- readBin(con, what="integer", n=w*h, size=1, signed=F)
      if(intensity==T)
      {
        mean(imagedata)
      } else{
        imagedata
      }
    }
    if(bitspersample==16){
      seek(con, where=px.start, origin="start")
      imagedata <- readBin(con, "integer", n=w*h, size=2, signed=F)
      if(intensity==T)
      {
        mean(imagedata)
      } else{
        imagedata
      }
    }
  }

  # Prepare a raw vector
  tmpdata <- rep(0, w*h*nf*nch)
  intensity_mean <- rep(0, nf)

  # Store image in the array
  if(nch==1){
    if(nf==1){
      if(intensity==T){
        intensity_mean <- ByteGenerator(1, fr, bitspersample, intensity=T)
        close(con)
        return(intensity_mean)
      }else{
        tmpdata[1:(2*w*h)] <- ByteGenerator(1, fr, bitspersample)
        outputimg <- array(tmpdata, dim=c(w,h))
        close(con)
        return(outputimg)
      }
    }else{
      if(intensity==T){
        for (j in 1:nf) {
          intensity_mean[j] <- ByteGenerator(1, fr[j], bitspersample, intensity=T)
        }
        close(con)
        return(intensity_mean)
      }else{
        for (j in 1:nf) {
          tmpdata[((j-1)*w*h+1):(j*w*h)] <- ByteGenerator(1, fr[j], bitspersample)
        }
        outputimg <- array(tmpdata, dim=c(w,h,nf))
        close(con)
        return(outputimg)
      }
    }
  }else{
    if(nf==1){
      if(intensity==T){
        for(i in 1:nch){
          intensity_mean[i] <- ByteGenerator(i, 1, bitspersample, intensity=T)
        }
        close(con)
        return(intensity_mean)
      }else{
        for(i in 1:nch){
          tmpdatatmpdata[((i-1)*w*h+1):(i*w*h)] <- ByteGenerator(i, 1, bitspersample)
        }
        outputimg <- array(tmpdata, dim=c(w,h,nch))
        close(con)
        return(outputimg)
      }
    }else{
      close(con)
      stop("Multiple frames is only supported for grayscale images.")
    }
  }
}

