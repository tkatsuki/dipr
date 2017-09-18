#' @export

thinning <- function(x){
  l1 <- .lut1()
  l2 <- .lut2()
  m <- matrix(c(2^7, 2^0, 2^1, 2^6, 0, 2^2, 2^5, 2^4, 2^3),
              3, 3, byrow=TRUE)

  repeat{
    key1 <- round(filter2(x, m))
    flag1 <- matrix(l1[key1+1], nrow(key1), ncol(key1))
    x1 <- x*flag1
    key2 <- round(filter2(x1, m))
    flag2 <- matrix(l2[key2+1], nrow(key2), ncol(key2))
    x2 <- x1*flag2
    if (identical(x, x2)) break
    x <- x2
  }

}
