#' This method uses leaflet to plot latitude and longitude for earthquakes on the map.
#' The map is interactive with markers that are clickable for showing a text.
#'
#' @param df The data frame with earthquake data.
#' @param annot_col The column name with the text displayed for the markers.
#' @return The leaflet as htmlwidget with the interactive map
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet leaflet
#' @importFrom lubridate year
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun{
#'  readr::read_delim("inst/extdata/signif.dat", delim = "\t") %>%
#'  eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "DATE")
#' }
eq_map <- function(df, annot_col) {
#  test_that (nrow(df) == 0) {
#    stop (paste("Data frame is empty"))
#  }

#  test_cols = c("LATITUDE", "LONGITUDE", "EQ_PRIMARY", annot_col)
# if (!all(test_cols %in% colnames(df))) {
#    stop(paste("Not all coloumns", test_cols, "found in", colnames(df)))
#  }

  # Get map
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
    leaflet::addCircleMarkers(lng=df$LONGITUDE, lat=df$LATITUDE, popup=df[[annot_col]],
                              radius=as.numeric(df$DEATHS), fillOpacity = 0.2, weight=1)
  return(m)
}

#' This method returns a popup-text for each in the data frame with location name, magnitude and
#' deaths.
#'
#' @param df The data frame for the popup-texts.
#' @return list of popup-texts.
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun{
#'  eq_clean_data("inst/extdata/signif.dat") %>%
#'  dplyr::filter(COUNTRY == "MEXICO" && lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#' }
eq_create_label <- function(df) {
  required_cols <- c("LOCATION_NAME", "EQ_PRIMARY", "TOTAL_DEATHS")
  if (!all(required_cols %in% colnames(df))) {
    stop(paste("Data frame must have columns:", required_cols))
  }
  df_location_cleaned <- eq_location_clean(df)
  popup_text <- apply(df_location_cleaned, 1,
                      FUN = function(x) paste0(
                        if (!is.na(x[["LOCATION_NAME"]])) paste0("<b>Location:</b> ",
                                                                 x[["LOCATION_NAME"]],
                                                                 "<br>") else "",
                        if (!is.na(x[["EQ_PRIMARY"]])) paste0("<b>Magnitude: </b>",
                                                              x[["EQ_PRIMARY"]], "<br>") else "",
                        if (!is.na(x[["TOTAL_DEATHS"]])) paste0("<b>Total deaths: </b>",
                                                                x[["TOTAL_DEATHS"]]) else "")
  )
}
