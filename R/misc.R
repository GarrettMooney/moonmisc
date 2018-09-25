##' Number of non-NA unique elements in a vector
##'  
##' Returns the number of non-NA unique elements in a vector. A wrapper to
##' \code{length( unique( x[!is.na(x)], ... ) )}. 
##' Primarily intended for interactive, not programmatic, use.
##' @export
##' @param x a vector
##' @param ... passed to \code{\link{unique}}
lu <- function( x, ...) {
  length( unique( x[!is.na(x)], ... ) ) 
}

##' Unique elements in a vector
##' 
##' Returns the unique elements in a vector. A wrapper to
##' \code{\link{unique}()}.
##' Primarily intended for interactive, not programmatic, use.
##' @param ... passed to \code{\link{unique}}.
u <- function(...) { 
  unique( ... ) 
}

##' length( grep( ... ) )
##' 
##' This is a wrapper to a \code{length( grep( ... ) )}. See examples for usage.
##' Primarily intended for interactive, not programmatic, use.
##' @param x a vector on which we attempt to match \code{pattern} on.
##' @param pattern regex pattern passed to \code{grep}.
##' @param perl boolean. use perl-compatible regular expressions?
##' @param ... additional arguments passed to \code{\link{grep}}.
##' @seealso \code{\link{re_exists}}
##' @export
##' @examples
##' x <- c("apple", "banana", "cherry")
##' if( lg( x, "^ap" ) > 0 ) {
##'   print( "regular expression '^ap' found in 'x'" )
##'   }
lg <- function(x, pattern, perl=TRUE, ...) {
  length( grep( x, pattern, perl=perl, ... ) ) 
}

##' unlist( strsplit( ... ) )
##' 
##' This is a thin wrapper to \code{ unlist( strsplit( ... ) ) }.
##' Primarily intended for interactive, not programmatic, use.
##' @param x vector of items, as passed to \code{\link{strsplit}}
##' @param split the delimiter to split on
##' @param ... optional arguments passed to strsplit
##' @export
##' @seealso \code{\link{unlist}}, \code{\link{strsplit}}
##' @examples
##' x <- "apple_banana_cherry"
##' us(x, "_")
us <- function(x, split="", ...) { unlist( strsplit( x, split=split, ...) ) }

##' Set Working Directory
##' 
##' A small convenience function that wraps \code{file.path} into a
##' \code{setwd} call.
##' 
##' @param ... the set of strings to paste together. if no arguments are 
##' submitted, then we return to the home directory.
##' @export
##' @examples
##' x <- "my_favourite_dir"
##' #setwd( "C:/", x, "really_awesome_stuff" )
##' ## calls setwd( paste( "C:/", x, "really_awesome_stuff", collapse="" ) )
cd <- function(...) {
  args <- list(...)
  if( length(args) == 0 ) {
    base::setwd("~")
    return( invisible(NULL) )
  }
  base::setwd( file.path( ... ) )
  return( invisible(NULL) )
}

##' k-means Diagnostic Plot
##' 
##' Using \code{kmeans}, plot percentage variance explained vs. number of clusters.
##' Used as a means of picking \code{k}.
##' 
##' @importFrom lattice xyplot panel.grid panel.polygon panel.xyplot panel.abline
##' @param dat numeric matrix of data, or an object that can be coerced to 
##' such a matrix (such as a numeric vector or a data frame with all 
##' numeric columns).
##' @param nmax maximum number of clusters to examine
##' @param ... optional arguments passed to xyplot
##' @seealso \code{\link{kmeans}}
##' @export
##' @examples
##' data(iris)
##' kmeans_plot(iris[,1:4])
kmeans_plot <- function( dat, nmax=20, ... ) {
  y <- rep(0,nmax)
  for( i in 1:nmax ) {
    tmp <- kmeans( dat, i )
    y[i] <- tmp$betweenss / tmp$totss
  }
  print( xyplot( y ~ (1:nmax), type = c("p", "l"), ... ) )
}

##' Strip File Extension
##' 
##' Strips the extension from a file name. By default, we assume the extension 
##' is separated from the file name by a single period; however, the \code{lvl} 
##' argument lets us specify how many periods are used in forming the file 
##' extension.
##' @param x the file name, including extension.
##' @param lvl the number of \code{'.'} used in defining the file extension.
##' @export
##' @examples
##' x <- "path_to_file.tar.gz"
##' strip_extension(x, lvl=2)
strip_extension <- function(x, lvl=1) {
  if( length(x) == 0 ) { 
    return(x)
  }
  tmp <- unlist( strsplit( x, "/" ) )
  tmp2 <- tmp[length(tmp)]
  tmp2 <- unlist( strsplit( tmp2, "\\." ) )
  tmp2 <- paste( tmp2[1:(length(tmp2)-lvl)], collapse ="." )
  tmp <- paste( tmp[-length(tmp)], collapse="/")
  if( tmp != "" ) {
    tmp2 <- paste( tmp, tmp2, sep = "/" )
  }
  return(tmp2)
}

##' Remove NA Entries from a Vector
##' 
##' This function removes all \code{NA} entries from a vector.
##' 
##' For \code{data.frames}, we use \code{complete.cases} to remove \code{NA}s,
##' and hence remove all rows for which an \code{NA} value in encountered.
##' 
##' @param x An (atomic) vector, or a list / data.frame.
##' @export
remove_na <- function(x) {
  if( is.data.frame(x) ) {
    return( x[complete.cases(x), ] )
  } else if( is.list(x) ) {
    return( lapply(x, remove_na) )
  } else {
    return( x[ !is.na(x) ] )
  }
}

##' Midrange
##' 
##' Point halfway between the min and the max.
##' 
##' The point halfway between the min and the max. The mid-range minimizes the maximum distance to a set of points.
##' 
##' @param x A numeric vector.
##' @return A numeric scalar vector.
##' @export
##' @examples
##' midrange(iris[,1])
##' midrange(iris[,2])
##' midrange(iris[,3])
##' midrange(iris[,4])
midrange <- function(x){
  r <- range(x)
  (r[2]-r[1])/2+r[1]
}
