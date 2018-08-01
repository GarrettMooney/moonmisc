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
##' @param pattern regex pattern passed to \code{grep}.
##' @param x a vector on which we attempt to match \code{pattern} on.
##' @param perl boolean. use perl-compatible regular expressions?
##' @param ... additional arguments passed to \code{\link{grep}}.
##' @seealso \code{\link{re_exists}}
##' @export
##' @examples
##' x <- c("apple", "banana", "cherry")
##' if( lg( "^ap", x ) > 0 ) {
##'   print( "regular expression '^ap' found in 'x'" )
##'   }
lg <- function(pattern, x, perl=TRUE, ...) {
  length( grep( pattern, x, perl=perl, ... ) ) 
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