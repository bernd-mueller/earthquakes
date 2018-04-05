library(earthquakes)
context("test_eq_clean_data")

filename <- "extdata/signif.dat"

test_that("Test eq_clean_data if it's a dataframe", {
  expect_that(eq_clean_data(filename), is_a("data.frame"))
})
