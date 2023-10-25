#' Behavioral Risk Factor Surveillance System (BRFSS) dataset
#'
#' The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s premier system of health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services.
#' Established in 1984 with 15 states, BRFSS now collects data in all 50 states as well as the District of Columbia and three U.S. territories.
#' BRFSS completes more than 400,000 adult interviews each year, making it the largest continuously conducted health survey system in the world.
#'
#'
#' @details
#' Fact sheet is available [here](https://www.cdc.gov/brfss/factsheets/pdf/DBS_BRFSS_survey.pdf).
#'
#' @param \dots passed to [readr::read_csv()] and useful for limiting
#'   the number of rows read for testing or glimpsing data.
#'
#'
#' @source - [Data Source](https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu)
#'
#' @importFrom readr read_csv
#'
#' @family data accessors
#'
#' @examples
#'
#' brfss = src_brfss(n_max=1000)
#'
#' colnames(brfss)
#'
#' dplyr::glimpse(brfss)
#'
#' @export
src_brfss <- function(...) {
  fname = s2p_cached_url('https://data.cdc.gov/api/views/dttw-5yxu/rows.csv?date=20231024&accessType=DOWNLOAD')
  res = readr::read_csv(fname, ...)
  res
}

