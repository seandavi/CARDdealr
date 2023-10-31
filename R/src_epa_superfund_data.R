#' Environmental Protection Agency Superfund API
#'
#' @description
#' `src_epa_superfund_data()` returns a dataframe of Superfund sites from the Envirofacts API.
#' 
#' @details
#' The Envirofacts Multisystem Search integrates information from a variety of databases and 
#' includes latitude and longitude information. Each of these databases contains information 
#' about facilities that are required to report activity to a state or federal system.
#' 
#' @import jsonlite
#' @importFrom dplyr mutate select filter
#' 
#' @return A data frame (data.frame) containing data pulled from the Envirofacts API.
#'
#' @source - [Data Source](https://www.epa.gov/enviro/web-services)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' sf = src_epa_superfund_data()
#' dim(sf)
#' 
#' if(requireNamespace('ggplot2')) {
#'   ggplot2::ggplot(sf, ggplot2::aes(x=State)) + 
#'     ggplot2::geom_bar() +
#'     ggplot2::coord_flip()
#' }
#' 
#' sf_ky = sf |> dplyr::filter(State=='KY')
#' dim(sf_ky)
#'
#' colnames(sf)
#'
#' dplyr::glimpse(sf)
#'
#' @export
src_epa_superfund_data = function(){
    lg <- get_carddealr_logger()
    
    lg$info('Starting epa_superfund_data')

    url = paste0('https://data.epa.gov/efservice/ENVIROFACTS_SITE/JSON')
        
    sf = jsonlite::fromJSON(url) |> 
        dplyr::filter(npl_status_name %in% 
                          c('Currently on the Final NPL', 'Deleted from the Final NPL',
                            'Site is Part of NPL Site')) |>
        dplyr::mutate(
            zip_code = substr(zip_code, 1, 5),
            Address = paste0(street_addr_txt, ', ', city_name, ', ', 
                             fk_ref_state_code, ' ', zip_code),
            Type = 'Superfund Site',
            Phone_number = ''
        ) |>
        dplyr::rename(
            Name = name,
            Notes = npl_status_name,
            latitude = primary_latitude_decimal_val,
            longitude = primary_longitude_decimal_val,
            State = fk_ref_state_code
        ) |>
        dplyr::select(Type, Name, Address, State, Phone_number, Notes, latitude, longitude)
    
    lg$info('Completing epa_superfund_data')
    
    return(sf)
}
