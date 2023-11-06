#' CDC PLACES
#'
#' @description
#' `src_cdc_places_data()` returns a dataframe of estimates from CDC PLACES.
#' 
#' @details
#' PLACES is a collaboration between CDC, the Robert Wood Johnson Foundation, 
#' and the CDC Foundation. PLACES provides health data for small areas across 
#' the country. This allows local health departments and jurisdictions, regardless 
#' of population size and rurality, to better understand the burden and 
#' geographic distribution of health measures in their areas and assist them 
#' in planning public health interventions. PLACES provides model-based, 
#' population-level analysis and community estimates of health measures to all 
#' counties, places (incorporated and census designated places), census tracts, 
#' and ZIP Code Tabulation Areas (ZCTAs) across the United States.
#' 
#' @import lgr
#' @importFrom dplyr case_when
#'   
#' @param level Character string for desired geographic level. Available at 'county',
#' "tract", or 'zcta'. Defaults to 'county'.
#' 
#' @param year The year of the CDC PLACES estimates. Estimates are available from
#' 2020 through 2023. Defaults to 2023.
#' 
#' @return A data frame (data.frame) containing data pulled from the CDC PLACES API.
#'
#' @source - [Data Source](https://www.cdc.gov/places/measure-definitions/index.html)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' places = src_cdc_places_data(level = 'tract', nrows = 500)
#'
#' colnames(places)
#'
#' dplyr::glimpse(places)
#'
#' @export

src_cdc_places_data <- function(level = 'county', year = 2023, ...){
    lg <- get_carddealr_logger()
    
    lg$info('Starting cdc_places_data')
    
    url = dplyr::case_when(
        year == 2023 & level == 'county' ~ 
            'https://data.cdc.gov/api/views/i46a-9kgh/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2023 & level == 'tract' ~ 
            'https://data.cdc.gov/api/views/yjkw-uj5s/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2023 & level == 'zcta' ~ 
            'https://data.cdc.gov/api/views/kee5-23sr/rows.csv?date=20231106&accessType=DOWNLOAD',
        
        year == 2022 & level == 'county' ~ 
            'https://data.cdc.gov/api/views/xyst-f73f/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2022 & level == 'tract' ~ 
            'https://data.cdc.gov/api/views/shc3-fzig/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2022 & level == 'zcta' ~ 
            'https://data.cdc.gov/api/views/c76y-7pzg/rows.csv?date=20231106&accessType=DOWNLOAD',
        
        year == 2021 & level == 'county' ~ 
            'https://data.cdc.gov/api/views/kmvs-jkvx/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2021 & level == 'tract' ~ 
            'https://data.cdc.gov/api/views/mb5y-ytti/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2021 & level == 'zcta' ~ 
            'https://data.cdc.gov/api/views/9xb7-9z99/rows.csv?date=20231106&accessType=DOWNLOAD',
        
        year == 2020 & level == 'county' ~ 
            'https://data.cdc.gov/api/views/mssc-ksj7/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2020 & level == 'tract' ~ 
            'https://data.cdc.gov/api/views/ib3w-k9rq/rows.csv?date=20231106&accessType=DOWNLOAD',
        year == 2020 & level == 'zcta' ~ 
            'https://data.cdc.gov/api/views/bdsk-unrd/rows.csv?date=20231106&accessType=DOWNLOAD'
    )
    
    res = read.csv(url, ...)
    
    lg$info('Completing cdc_places_data')
    
    return(res)
}
