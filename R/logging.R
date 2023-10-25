#' The CARDdealr logger
#'
#' @import lgr
#' @import glue
#'
#' @returns a [lgr::LoggerGlue] instance
#'
#' @examples
#'
#' lg <- get_carddealr_logger()
#'
#' lg$info('this is a log entry')
#'
#'
#' @export
get_carddealr_logger <- function() {
  return(lgr::get_logger_glue('CARDdealr'))
}
