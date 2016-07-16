ssweep <- function(obj, ref, op){
  if(op == "-") {
    sweepC(obj, ref, dim(obj), 1)
  } else if(op == "*") {
    sweepC(obj, ref, dim(obj), 2)
  }
}
