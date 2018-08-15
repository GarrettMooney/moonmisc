#' Softmax Function
#'
#' This function maps a numeric vector of real numbers to a vector of proportions, between 0 and 1, summing to 1.
#' @param x numeric vector of real numbers
#' @keywords softmax
#' @return numeric vector of proportions between 0 and 1 with a sum of 1
#' @export
#' @examples
#' x = c(5,6,1,10)
#' softmax(x)

softmax <- function(x){
  exp(x)/sum(exp(x))
}
