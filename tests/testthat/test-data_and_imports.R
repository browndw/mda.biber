# Test file for data validation and data integrity
library(testthat)
library(mda.biber)

test_that("micusp_biber data is properly formatted", {

  # Load the data
  data(micusp_biber)

  # Test basic structure
  expect_s3_class(micusp_biber, "data.frame")
  expect_true(nrow(micusp_biber) > 0)
  expect_true(ncol(micusp_biber) > 0)

  # Test that doc_id column exists and is character
  expect_true("doc_id" %in% colnames(micusp_biber))
  expect_type(micusp_biber$doc_id, "character")

  # Test that most columns are numeric (except doc_id)
  numeric_cols <- sapply(micusp_biber[, -1], is.numeric)
  expect_true(all(numeric_cols))

  # Test for no missing values in critical columns
  expect_false(any(is.na(micusp_biber$doc_id)))

  # Test that all values are non-negative (rates should be >= 0)
  numeric_data <- micusp_biber[, sapply(micusp_biber, is.numeric)]
  expect_true(all(numeric_data >= 0, na.rm = TRUE))
})

test_that("data can be used for MDA analysis", {

  data(micusp_biber)

  # Create factor variable
  test_data <- micusp_biber
  test_data$discipline <- as.factor(substr(test_data$doc_id, 1, 3))

  # Should have reasonable number of groups
  expect_true(nlevels(test_data$discipline) >= 3)
  expect_true(nlevels(test_data$discipline) <= 20)

  # Test that we can run MDA on a subset
  test_subset <- test_data[1:60, c("discipline", 
                                   "f_01_past_tense", 
                                   "f_14_nominalizations",
                                   "f_16_other_nouns",
                                   "f_39_prepositions")]

  expect_no_error({
    result <- mda_loadings(test_subset, n_factors = 1)
  })
})

test_that("package imports work correctly", {

  # Test that required packages are available
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("dplyr", quietly = TRUE))
  expect_true(requireNamespace("tidyr", quietly = TRUE))
  expect_true(requireNamespace("ggrepel", quietly = TRUE))
  expect_true(requireNamespace("ggpubr", quietly = TRUE))
  expect_true(requireNamespace("viridis", quietly = TRUE))
  expect_true(requireNamespace("nFactors", quietly = TRUE))
})
