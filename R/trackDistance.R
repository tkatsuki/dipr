#' Template matching
#'
#' These functions are for tracking and analyzing objects in movies. tracking calculates various values related to object motion. trackDistance calculates distance traveled by objects during the specified time window. trackSpeed calculates average speed of objects during the specified time window. angularVelocity calculates angular velocity of moving objects. msd calculates mean square diviation of object trajectories.
#' @name trackDistance
#' @param mask	Labeled binary movie.
#' @param maxdist	Maximum distance for object association.
#' @param bin Window size for averaging.
#' @param interval Time intervals.
#' @param unit Unit length.
#' @param size Maximum size difference for objects association.
#' @param xy A matrix of object coordinates.
#' @param dist Traveled distance (e.g. the output of trackDistance).
#' @param xy A target image of Image object or an array.
#' @aliases tracking trackSpeed angularVelocity msd
#' @export
#' @examples
#' tracking()
NULL

trackDistance <- function (xy, unit = 1) {
  sqrt((head(xy[,1], -1) - xy[-1, 1])^2 + (head(xy[,2], -1) - xy[-1, 2])^2)*unit
}

#' @rdname trackDistance
#' @return Returns a grayscale image.
#' @export

trackSpeed <- function(dist, bin=5, interval=1){
  v <- c()
  for (i in 1:(length(dist)-bin)){
    v[i] <- sum(dist[i:(i+bin-1)])/bin
  }
  return(v/interval)
}
