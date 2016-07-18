#' Template matching
#'
#' These functions perform template matching using SAD (Sum of absolute differences), NCC (Normalized cross correlation), or FNCC (fast normalized cross correlation).
#' @param trg A target image of Image object or an array.
#' @param tmp A template image of Image object or an array.
#' @aliases SAD NCC
#' @return Returns a grayscale image.
#' @export
#' @examples
#' FNCC()

FNCC <- function(trg,tmp){
  trg <- trg - sum(trg)/length(trg)
  tmp <- tmp - sum(tmp)/length(tmp)
  w <- ncol(trg) + ncol(tmp) - 1
  h <- nrow(trg) + nrow(tmp) - 1
  amat <- matrix(0, nrow=h, ncol=w)
  bmat <- matrix(0, nrow=h, ncol=w)
  amat[1:nrow(trg), 1:ncol(trg)] <- trg
  bmat[1:nrow(tmp), 1:ncol(tmp)] <- tmp
  fa <- .fft2d(amat)
  fb <- .fft2d(bmat, TRUE)
  abs(.fft2d(fa*fb, TRUE))/(h*w)
}

