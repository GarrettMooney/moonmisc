#' Logit Function
#'
#' This function maps a number x between 0 and 1 to logit(x) = log(x/(1-x))
#' @param x numeric between 0 and 1
#' @keywords logit
#' @return numeric on the real line
#' @export
#' @examples
#' logit(.3)

logit <- function(x){
  return(log(x/(1-x)))
}
