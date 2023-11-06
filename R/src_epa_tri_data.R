#' EPA Toxic Release Inventory (TRI) Dataset
#'
#' @description
#' `src_epa_tri_data()` returns a dataframe of facilities reporting into the Toxic
#' Release Inventory.
#' 
#' @details
#' The Envirofacts Multisystem Search integrates information from a variety of 
#' databases and includes latitude and longitude information. Each of these databases 
#' contains information about facilities that are required to report activity to 
#' a state or federal system. The TRI contains information about all facilities 
#' releasing reportable toxic materials, identified as carcinogenic or not.
#' 
#' @import lgr
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_pad str_to_title str_c str_split_fixed
#' 
#' @return A data frame (data.frame) containing data pulled from the Envirofacts API.
#'
#' @source - [Data Source](https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' tri = src_epa_tri_data()
#'
#' colnames(tri)
#'
#' dplyr::glimpse(tri)
#'
#' @export

src_epa_tri_data = function(){
    options(dplyr.summarise.inform = FALSE)
    
    lg <- get_carddealr_logger()
    
    lg$info('Starting epa_tri_data')
    
    yr = as.integer(format(Sys.Date(), "%Y")) -1
    
    url = paste0('https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/',
                 yr, '_US/csv')
    
    tri = try(read.csv(url, header=T), silent = TRUE)
    
    if (class(tri) == 'try-error'){
        url = paste0('https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/',
                     yr-1, '_US/csv')
        
        tri = read.csv(url, header=T)
    }
    
    colnames(tri) = stringr::str_split_fixed(colnames(tri), pattern = '\\.\\.', 2)[,2]
    
    tri = tri |>
        dplyr::filter(!(ST %in% c('AS', 'GU', 'MP', 'PR', 'VI'))) |>
        dplyr::select(FRS.ID, FACILITY.NAME, STREET.ADDRESS, CITY, ST, ZIP, COUNTY,
                      LATITUDE, LONGITUDE, CHEMICAL, CARCINOGEN) |>
        dplyr::mutate(
            FACILITY.NAME = stringr::str_to_title(FACILITY.NAME),
            ZIP = stringr::str_pad(ZIP, side='left', width=5, pad=0),
            Address = paste0(stringr::str_to_title(STREET.ADDRESS), ', ', 
                             stringr::str_to_title(CITY), ', ', 
                             ST, ' ', ZIP),
            Type = 'Toxic Release Inventory Facility'
        ) |>
        dplyr::rename(
            Name = FACILITY.NAME,
            State = ST,
            latitude = LATITUDE,
            longitude = LONGITUDE
        ) |>
        dplyr::group_by(FRS.ID, Type, Name, Address, State, latitude, longitude, CARCINOGEN) |>
        dplyr::summarise(chemicals = list(unique(CHEMICAL))) |>
        tidyr::pivot_wider(names_from = CARCINOGEN, values_from = chemicals) |>
        dplyr::ungroup() |>
        dplyr::rename(
            Noncarcinogenic = NO,
            Carcinogenic = YES
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            Noncarcinogenic = paste0(stringr::str_c(Noncarcinogenic, collapse = ', ')),
            Carcinogenic = paste0(stringr::str_c(Carcinogenic, collapse = ', ')),
            Phone_number = ''
        ) |>
        dplyr::ungroup() |>
        dplyr::select(Type, Name, Address, State, Phone_number, Noncarcinogenic, Carcinogenic, latitude, longitude)
    
    lg$info('Completing epa_tri_data')
    
    return(tri)
}