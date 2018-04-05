#' Clean the location name by removing the country name with colon
#'
#' @param locname character vector with the location name
#'
#' @return character vector with removed country name with colon
#' @export
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#'   eq_location_clean("GREECE:  THERA ISLAND (SANTORINI)")
#' }
eq_location_clean <- function (locname) {

  cleaned_locname <- stringr::str_to_title(gsub(".*: ", "", locname))
  return(cleaned_locname)
}


#' Read the earthquake data from file and tidy it up
#'
#' @param file character vector with the file path containing the earthquake data
#'
#' @return data table that contains the cleaned earthquake data
#' @export
#' @importFrom readr read_delim
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' eqdata <- eq_clean_data (file = system.file("extdata", "signif.txt", package="earthquakes"))
#' }
#'
eq_clean_data <- function (file) {

  eqtable <- read_delim(file, delim = "\t")

  eqtable$DATE <- eqtable$YEAR
  eqtable$LONGITUDE <- as.numeric(eqtable$LONGITUDE)
  eqtable$LATITUDE <-  as.numeric(eqtable$LATITUDE)
  eqtable$LOCATION_NAME <- eq_location_clean(eqtable$LOCATION_NAME)
  eqtable$LOCATION_NAME <- eq_location_clean(eqtable$LOCATION_NAME)
  eqtable$DEATHS <- as.numeric(eqtable$DEATHS)


  eqtable[is.na(eqtable)] <- 0
  return (eqtable)
}

