library(earthquakes)
context("test-eq_map")

df <-eq_clean_data(system.file("extdata", "signif.txt", package="earthquakes"))
map <- df %>% eq_map("DATE")

test_that("Check is data is a leaflet object", {
  expect_that(map, is_a("leaflet"))
})
