library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#' Title
#'
#' @param locname
#'
#' @return
#' @export
#'
#' @examples
eq_location_clean <- function (locname) {
  cleaned_locname <- str_to_title(gsub(".*:  ", "", locname))
  cleaned_locname
}


#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#'   eqdata <- eq_clean_data ("data/signif.txt")
#' }
#'
eq_clean_data <- function (filename) {
  eqtable <- read_delim(filename, delim = "\t")
  cleaned_eqtable <- eqtable %>%
    mutate(LONGITUDE = as.numeric(LONGITUDE),
           LATITUDE = as.numeric(LATITUDE),
           LOCATION_NAME = eq_location_clean(LOCATION_NAME)) %>%
    select (YEAR, MONTH, DAY, LATITUDE, LONGITUDE, LOCATION_NAME) %>%
    unite(DATE, YEAR, MONTH, DAY, sep = "-", remove = TRUE)
  cleaned_eqtable
}

