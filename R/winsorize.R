#' Winsorize Function
#'
#' This function truncates vector to the fractionth-ile elements from the top and the bottom
#' @param x numeric vector of real numbers
#' @keywords winsorize
#' @return numeric vector of x with all elements below the franctionth percentile truncated to the limiting value from the top and bottom
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(10)
#' x <- sort(x)
#' x
#' winsorize(x,0.2)

winsorize <- function(x,fraction){
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}
