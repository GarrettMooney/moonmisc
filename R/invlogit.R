#' Inverse Logit Function
#'
#' This function maps a number x on the real line to invlogit(x) = 1/(1+exp(-x)
#' @param x numeric on the real line
#' @keywords logit, invlogit
#' @return numeric between 0 and 1
#' @export
#' @examples
#' logit(-0.8472979)

invlogit <- function(x){
  return(1/(1+exp(-x)))
}
