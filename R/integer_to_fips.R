#' convert an integer to fips
#'
#' The Federal Information Processing Standards (fips) codes
#' are available as 5-digit, 0-padded numbers. To standardize
#' these codes for joins across datasets, one may need to convert
#' raw integers (non-0-padded) to 5-digit numeric strings. This
#' function does that.
#'
#' @references
#' List of United States FIPS codes by county: \url{https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county}.
#'
#' @param v integer vector of length 1 or more
#'
#' @return
#' character vector with each member a 0-padded representation of the original
#' integer value.
#'
#' @examples
#' integer_to_fips(50)
#' class(integer_to_fips(50))
#' nchar(integer_to_fips(50))==5
#' integer_to_fips(sample(1:50000,5))
#'
#' @export
integer_to_fips <- function(v) {
    sprintf('%05d', v)
}

#' @describeIn integer_to_fips
#'
#' @param v integer vector of state-level fips
#'   to be converted
#'
#' @author Sean Davis <seandavi@gmail.com>
#'
#' @examples
#'
#' # integer to state fips
#' sl = integer_to_state_fips(5)
#' sl
#' sl == '05000'
#'
#' @export
integer_to_state_fips = function(v) {
    paste0(sprintf('%02d', fips),'000')
}

