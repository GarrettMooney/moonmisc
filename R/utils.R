#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Default value for `NULL`.
#'
#' This infix function makes it easy to replace `NULL`s with a
#' default value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns
#'   `x`.
#' @export
#' @name null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
    if (is.null(x)) {
          y
  } else {
        x
    }
}


#' Increase by 1
#' @export
inc <- function(x) {
    x + 1L
}

#' Decrease by 1
#' @export
dec <- function(x) {
    x - 1L
}

#' Create character vector without quotes
#' @export
lzy_chr <- function(...) {
  as.character(sys.call())[-1]
}

#' First five and last five rows.
#' @export
i <- function(d, n=6) rbind(head(d, n), tail(d, n))

#' First five columns and rows.
#' @export
hh <- function(d) d[1:5, 1:5]

#' Glue to shell functions
#' @export
shell_transformer <- function(code, envir) {
  shQuote(eval(parse(text = code), envir))
}
#' @export
glue_sh <- function(..., .envir = parent.frame()) {
  glue::glue(..., .envir = .envir, .transformer = shell_transformer)
}
