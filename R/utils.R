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

#' Default value for `length(x) == 0`.
#'
#' This infix function makes it easy to replace a length 0 value with a
#' default value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `length(x) == 0`, will return `y`; otherwise returns
#'   `x`.
#' @export
#' @name length-zero-default
#' @examples
#' "bacon" %|0|% "eggs"
#' NULL %|0|% "eggs"
`%|0|%` <- function(x, y) { if (length(x) == 0) y else x }

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

#' System convenience functions ------------------------------------------------
#' Glue to shell functions
#' @export
shell_transformer <- function(code, envir) {
  shQuote(eval(parse(text = code), envir))
}
#' @export
glue_sh <- function(..., .envir = parent.frame()) {
  glue::glue(..., .envir = .envir, .transformer = shell_transformer)
}

#' Open pdf with okular
#' @export
okular <- function(path_to_file) {
  ok_path <- normalizePath(Sys.which('okular'))
  system(glue_sh('{ok_path} {path_to_file}'), wait = F)
}

#' Compile LaTeX via xelatex
#' @export
xelatex <- function(path_to_file) {
  system(glue_sh('xelatex --interaction=nonstopmode {path_to_file}'))
  ## NOTE: ignores warnings/errors
}

#' Search github
#' @export
github <- function(string) {
  browseURL(glue("https://github.com/search?q={string};"))
}


#' Search google
#' @export
google <- function(string) {
  browseURL(glue("https://www.google.com/search?hl=en#q={string};"))
}

#' Search stackoverlow
#' @export
stackoverflow <- function(string) {
  browseURL(glue("https://stackoverflow.com/search?q={string};"))
}

#' Search twitter
#' @export
twitter <- function(string) {
  browseURL(glue("https://twitter.com/search?q={string};"))
}

#' Search youtube
#' @export
youtube <- function(string) {
  browseURL(
    glue(
      "https://www.youtube.com/results?search_query=<<string>>&page={startPage?}&utm_source=opensearch;",
      .open = "<<",
      .close = ">>"
    )
  )
}
