#' Available datasets via CARDdealr
#' 
#' @returns A `data.frame` of datasets available. The `accessor` column
#' contains the name of the function that returns that dataset.
#' 
#' @examples
#' available_datasets()
#' 
#' @export
available_datasets <- function() {
  ad = readr::read_csv(system.file('data_catalog/catalog.csv', package='CARDdealr'))
  return(ad)
}