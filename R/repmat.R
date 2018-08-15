#' Rep Mat Function
#'
#' This function takes a matrix X and replicates it n times column-wise and m times row-wise, similarly to repmat in Matlab
#' @param X a numeric matrix
#' @param m number of row-wise replications
#' @param n number of column-wise replications
#' @keywords repmat
#' @return numeric matrix with X replicated n-times across and m-times down
#' @export
#' @examples
#' set.seed(1)
#' x = matrix(rnorm(6),2,3)
#' x
#' repmat(x,3,2)

repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}
