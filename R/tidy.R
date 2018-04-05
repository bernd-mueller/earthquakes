library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(testthat)

#' Clean the location name by removing the country name with colon
#'
#' @param locname character vector with the location name
#'
#' @return character vector with removed country name with colon
#' @export
#'
#' @examples
#' \dontrun {
#'   eq_location_clean("GREECE:  THERA ISLAND (SANTORINI)")
#' }
eq_location_clean <- function (locname) {

  cleaned_locname <- str_to_title(gsub(".*: ", "", locname))
  return(cleaned_locname)
}


#' Read the earthquake data from file and tidy it up
#'
#' @param filename character vector with the file path containing the earthquake data
#'
#' @return data table that contains the cleaned earthquake data
#' @export
#'
#' @examples
#' \dontrun {
#'   eqdata <- eq_clean_data ("data/signif.txt")
#' }
#'
eq_clean_data <- function (filename) {

  eqtable <- read_delim(filename, delim = "\t")

  test_cols = c("YEAR", "MONTH", "DAY", "LATITUDE", "LONGITUDE", "DEATHS")
  if (!all(test_cols %in% colnames(eqtable))) {
    stop(paste("Not all coloumns", test_cols, "found in", colnames(eqtable)))
  }


  cleaned_eqtable <- eqtable %>%
    mutate(DATE=YEAR, LONGITUDE = as.numeric(LONGITUDE),
           LATITUDE = as.numeric(LATITUDE),
           LOCATION_NAME = eq_location_clean(LOCATION_NAME),
           DEATHS = as.numeric(DEATHS)) %>%
    select (DATE, LATITUDE, LONGITUDE, LOCATION_NAME, DEATHS, COUNTRY)
    #select (YEAR, MONTH, DAY, LATITUDE, LONGITUDE, LOCATION_NAME) %>%
    #unite(DATE, YEAR, MONTH, DAY, sep = "-", remove = TRUE)

  return (cleaned_eqtable)
}

