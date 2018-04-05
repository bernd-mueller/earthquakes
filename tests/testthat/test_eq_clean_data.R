library(earthquakes)
context("test_eq_clean_data")

file <- system.file("extdata", "signif.txt", package="earthquakes")

test_that("Test eq_clean_data if it's a dataframe", {
  expect_that(eq_clean_data(file), is_a("data.frame"))
})
