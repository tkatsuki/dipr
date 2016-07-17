#' An FNCC Function
#'
#' These functions perform template matching using SAD (Sum of absolute differences), NCC (Normalized cross correlation), or FNCC (fast normalized cross correlation).
#' @param filepath A caracter string of the path to the file. Required.
#' @param start An integer of the start frame. Default = 1.
#' @param end An integer of the end frame. Default = 0 (last frame of the file).
#' @param skip An integer of skip frame. Default = 0 (read every frame).
#' @param getFrames Return number of frames. Default = False.
#' @param crop An integer vector depicting the xy coordinate of the top-left corner and the bottom-right corner you want to crop.
#' @param silent Whether or not show message. Default = False.
#' @keywords AVI
#' @export
#' @examples
#' FNCC()

FNCC <- function(a,b){
  a <- a - sum(a)/length(a)
  b <- b - sum(b)/length(b)
  w <- ncol(a) + ncol(b) - 1
  h <- nrow(a) + nrow(b) - 1
  amat <- matrix(0, nrow=h, ncol=w)
  bmat <- matrix(0, nrow=h, ncol=w)
  amat[1:nrow(a), 1:ncol(a)] <- a
  bmat[1:nrow(b), 1:ncol(b)] <- b
  fa <- .fft2d(amat)
  fb <- .fft2d(bmat, TRUE)
  abs(.fft2d(fa*fb, TRUE))/(h*w)
}
