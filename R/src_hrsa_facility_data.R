#' Health Resources and Services Administration (HRSA) facilities
#'
#' @description
#' `src_hrsa_facility_data()` returns a dataframe of FQHC and other HPSA facilities
#' from HRSA.
#' 
#' @details
#' HRSA programs provide health care to people who are geographically isolated, 
#' economically or medically vulnerable. This includes people living with HIV/AIDS, 
#' pregnant women, mothers and their families, and those otherwise unable to access 
#' high quality health care. HRSA also supports access to health care in rural areas, 
#' the training of health professionals, the distribution of providers to areas where 
#' they are needed most, and improvements in health care delivery.
#' 
#' @import lgr
#' @importFrom dplyr mutate select filter rename
#' @importFrom tidyr unnest
#' 
#' @return A data frame (data.frame) containing data pulled from HRSA.
#'
#' @source - [Data Source](https://data.hrsa.gov/data/about?tab=DataUsage)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' facs = src_hrsa_facility_data()
#'
#' colnames(facs)
#'
#' dplyr::glimpse(facs)
#'
#' @export

src_hrsa_facility_data = function(){
    options(dplyr.summarise.inform = FALSE)
    
    lg <- get_carddealr_logger()
    
    lg$info('Starting hrsa_facility_data')
    
    lg$info('Obtaining non-FQHC HPSAs')
    
    url = 'https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_PC.csv'
    
    hpsa = read.csv(url, header=T) |> 
        dplyr::filter(HPSA.Status == 'Designated' & 
                          !(Designation.Type %in% c('Federally Qualified Health Center', 'HPSA Population',
                                                    'Geographic HPSA', 'High Needs Geographic HPSA'))) |> 
        dplyr::select(HPSA.Name, HPSA.ID, Designation.Type, HPSA.Score, HPSA.Address,
                      HPSA.City, State.Abbreviation, Common.State.County.FIPS.Code,
                      HPSA.Postal.Code, Longitude, Latitude) |> 
        dplyr::mutate(HPSA.Street = paste0(HPSA.Address, ', ', HPSA.City, ', ', State.Abbreviation, ' ', HPSA.Postal.Code),
                      Type = paste0('HPSA ', Designation.Type),
                      Phone_number = '',
                      Notes = '') |> 
        dplyr::rename(
            Name = HPSA.Name, 
            Address = HPSA.Street,
            FIPS = Common.State.County.FIPS.Code,
            State = State.Abbreviation,
            longitude = Longitude,
            latitude = Latitude
            ) |> 
        dplyr::select(Type, Name, Address, State, Phone_number, Notes, latitude, longitude) 
    
    lg$info('Obtaining FQHCs')
    
    url2 = 'https://data.hrsa.gov//DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.csv'
    
    fqhc = read.csv(url2, header=T) |> 
        dplyr::filter(!is.na(Site.Address),
                      Health.Center.Type.Description != 'Administrative') |> 
        dplyr::mutate(Type = 'FQHC',
                      Address = paste0(Site.Address, ', ', Site.City, ', ', Site.State.Abbreviation, ' ', Site.Postal.Code),
                      Phone_number = Site.Telephone.Number) |> 
        dplyr::rename(
            Name = Site.Name,
            State = Site.State.Abbreviation,
            Notes = Health.Center.Service.Delivery.Site.Location.Setting.Description,
            longitude = Geocoding.Artifact.Address.Primary.X.Coordinate,
            latitude = Geocoding.Artifact.Address.Primary.Y.Coordinate
        ) |> 
        dplyr::select(Type, Name, Address, State, Phone_number, Notes, latitude, longitude)
        

    lg$info('Completing hrsa_facility_data')
    
    hrsa = rbind(fqhc, hpsa)
    
    return(hrsa)
}