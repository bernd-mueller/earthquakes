library(earthquakes)
context("test_eq_location_clean")

loc <- ""

test_that("Test eq_location_clean if it cleans a location name", {
  expect_that(eq_location_clean("GREECE: THERA ISLAND (SANTORINI)"), equals("Thera Island (Santorini)"))
})
