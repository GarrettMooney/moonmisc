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

#' Encode text (used as an argument for google())
encode <- function(string) {
  system(glue_sh("echo -n {string} |
    perl -pe's/([^-_.~A-Za-z0-9])/sprintf(\"%%%02X\", ord($1))/seg'; "
  ), intern = T)
}

#' Alias for google-chrome
chrome <- normalizePath(Sys.which('google-chrome'))

#' Search github
#' @export
github <- function(string) {
  system(
    glue_sh(
      "{chrome} https://github.com/search?q={encode(string)};"
    )
  )
}


#' Search google
#' @export
google <- function(string) {
  system(
    glue_sh(
      "{chrome} https://www.google.com/search?hl=en#q={encode(string)};"
    )
  )
}

#' Search stackoverlow
#' @export
stackoverflow <- function(string) {
  system(
    glue_sh(
      "{chrome} https://stackoverflow.com/search?q={encode(string)};"
    )
  )
}

#' Search twitter
#' @export
twitter <- function(string) {
  system(
    glue_sh(
      "{chrome} https://twitter.com/search?q={encode(string)};"
    )
  )
}

#' Search youtube
#' @export
youtube <- function(string) {
  system(
    glue_sh(
      "<<chrome>> https://www.youtube.com/results?search_query=<<encode(string)>>&page={startPage?}&utm_source=opensearch;",
      .open = "<<",
      .close = ">>"
    )
  )
}


