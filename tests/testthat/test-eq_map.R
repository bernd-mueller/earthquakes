library(earthquakes)
context("test-eq_map")

filename <- "data/signif.dat"
eq <- eq_clean_data(filename)

test_that("Check is data is a leaflet object", {
  expect_that(eq_map(eq), is_a("leaflet"))
})
