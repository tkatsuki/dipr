#' @rdname FNCC
#' @export
#' @examples
#' NCC()

NCC <- function(trg,tmp){
  w <- ncol(trg) + 2 * ncol(tmp) - 1
  h <- nrow(trg) + 2 * nrow(tmp) - 1
  ma <- matrix(0,h,w)
  ma[nrow(tmp):(nrow(tmp)+nrow(trg)-1),ncol(tmp):(ncol(tmp)+ncol(trg)-1)] <- trg
  mb <- .norm(tmp)
  cor <- matrix(0,h,w)
  for (i in 1:(w-ncol(tmp))) for (j in 1:(h-nrow(tmp))){
    sub <- ma[j:(j+nrow(tmp)-1),i:(i+ncol(tmp)-1)]
    cor[j, i] <- as.vector(.norm(sub)) %*% as.vector(mb)
    }
  cor
}
