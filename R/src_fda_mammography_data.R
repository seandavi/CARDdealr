#' FDA Certified Mammography Facilities Dataset
#'
#' @description
#' `src_fda_mammography_data()` returns a dataframe of certified mammography facilities
#' from the FDA Certified Mammography Facilities dataset.
#' 
#' @details
#' The Mammography Quality Standards Act requires mammography facilities across the nation 
#' to meet uniform quality standards. Congress passed this law in 1992 to assure high-quality 
#' mammography for early breast cancer detection, which can lead to early treatment, a range of 
#' treatment options, and increased chances of survival. Under the law, all mammography facilities 
#' must: 1) be accredited by an FDA-approved accreditation body, 2) be certified by FDA, 
#' or its State, as meeting the standards, 3) undergo an annual MQSA inspection, 
#' and 4) prominently display the certificate issued by the agency. All facilities meeting this criteria
#' are listed in the FDA Certified Mammography Facilities dataset
#' 
#' @import lgr
#' @importFrom dplyr mutate select filter case_when
#' @importFrom stringr str_detect
#' 
#' @return A data frame (data.frame) containing data pulled from the Envirofacts API.
#'
#' @source - [Data Source](https://www.fda.gov/radiation-emitting-products/consumer-information-mqsa/search-certified-facility)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' mam = src_fda_mammography_data()
#'
#' colnames(mam)
#'
#' dplyr::glimpse(mam)
#'
#' @export
src_fda_mammography_data = function(){
    lg <- get_carddealr_logger()
    
    lg$info('Starting fda_mammography_data')
    
    url = "http://www.accessdata.fda.gov/premarket/ftparea/public.zip"
    
    #create tempfile to store zip file in
    temp = tempfile()
    
    #download zip file
    download.file(url, temp)
    
    #read files inside zip file
    files = unzip(temp, list = TRUE)
    
    #locate files inside zip file
    txt = files$Name[str_detect(files$Name, pattern = '.txt')]
    
    #read file to dataframe
    df = read.csv(unz(temp, txt), sep = '|', header=FALSE)
    
    #unlink the temp file
    unlink(temp)
    
    #change column names
    colnames(df) = c('Name','Street','Street2','Street3','City','State','Zip_code','Phone_number', 'Fax')
    
    #manipulate dataframe
    df = df |> 
        dplyr::filter(!(State %in% c('AP', 'AE', 'GU', 'VI'))) |> 
        dplyr::mutate(
            BestStreet = dplyr::case_when(
                stringr::str_detect(Street, 'dba') ~ Street2,
                stringr::str_detect(Street, '[0-9]|[One]', negate = T) ~ Street2,
                .default = Street
                ),
            Type = 'Mammography',
            Notes = '',
            Address = paste0(BestStreet, ', ', City, ', ', State, ' ', Zip_code),
            latitude = '',
            longitude = ''
            ) |> 
        dplyr::select('Type','Name','Address','State', 'Phone_number', 'Notes', 'latitude', 'longitude')
    
    lg$info('Completing fda_mammography_data')
    
    return(df)
}
