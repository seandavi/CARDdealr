#' @import lgr
#' @importFrom httr2 request req_perform req_user_agent
#' @importFrom readr read_tsv
.bls_read_tsv <- function(url, ...) {
  lg <- get_carddealr_logger()
  lg$info('Reading url {url}')
  httr2::request(url) |>
    httr2::req_user_agent('example@gmail.com') |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    readr::read_tsv(col_types = readr::cols(), ...)
}

# urls:
# - https://download.bls.gov/pub/time.series/la/la.data.64.County
# - https://download.bls.gov/pub/time.series/la/la.data.65.City

#' Bureau of Labor Statistics Unemployment Data
#'
#' @param \dots passed to [readr::read_tsv] just in case....
#'
#' @importFrom dplyr select mutate bind_rows
#' @importFrom tidyr separate
#'
#' @family data accessors
#'
#' @examples
#'
#' bls_unemployment = src_bls_unemployment_data()
#'
#' colnames(bls_unemployment)
#'
#' dplyr::glimpse(bls_unemployment)
#'
#' @export
src_bls_unemployment_data <- function(...) {
  lg <- get_carddealr_logger()

  lg$info('Starting bls_unemployment_data')

  url_county = 'https://download.bls.gov/pub/time.series/la/la.data.64.County'
  url_city   = 'https://download.bls.gov/pub/time.series/la/la.data.65.City'
  url_series = 'https://download.bls.gov/pub/time.series/la/la.series'
  url_footnotes = 'https://download.bls.gov/pub/time.series/la/la.footnote'

  # Series processing

  series = .bls_read_tsv(url_series, ...) |>
    dplyr::select(series_id,series_title) |>
    tidyr::separate(series_title, c('measure','location'), ': ') |>
    dplyr::mutate(seasonal_adj=grepl('(S)', location)) |>
    dplyr::mutate(location = sub(' \\([US]+\\)$','', location))

  footnotes = .bls_read_tsv(url_footnotes, ...)

  counties = .bls_read_tsv(url_county,na = c('-',''), ...) |>
    dplyr::mutate(location_type='county') |>
    dplyr::left_join(series, 'series_id') |>
    dplyr::mutate(location = sub(' County','',location))

  cities = .bls_read_tsv(url_city, na = c('-',''), ...) |>
    dplyr::mutate(location_type='city') |>
    dplyr::left_join(series, 'series_id') |>
    dplyr::mutate(location = sub(' city','',location))

  dat = dplyr::bind_rows(
    counties,
    cities
  )

  lg$info('Completing bls_unemployment_data')

  # The fill below is to deal with District of Columbia (DC)
  # state is left empty
  dat |>
    tidyr::separate(location, c('location', 'state'), ', ', fill='right')
}
