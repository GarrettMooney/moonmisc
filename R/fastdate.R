##' fastDate
##'
##' A faster alternative to \code{lubridate::as_date} and \code{base::as.Date}.
##'
##' @importFrom fasttime fastPOSIXct
##' @param date A character string in the format \code{yyyy-mm-dd}
##' @export
##' @examples
##' dates <- as.Date( seq.int( 1, to = 1e4, by = 1), origin = "1970-01-01" )
##' x <- as.character( dates )
##' fastDate( dates )
fastDate <- function(datestring) {
    as.Date( fasttime::fastPOSIXct( datestring, "UTC", 3L ) )
}
