% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Clean the location name by removing the country name with colon}
\usage{
eq_location_clean(locname)
}
\arguments{
\item{locname}{character vector with the location name}
}
\value{
character vector with removed country name with colon
}
\description{
Clean the location name by removing the country name with colon
}
\examples{
\dontrun{
  eq_location_clean("GREECE:  THERA ISLAND (SANTORINI)")
}
}
