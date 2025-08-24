# Test file for mda_loadings function
library(testthat)
library(mda.biber)

# Create test data for the tests
test_that("mda_loadings function works correctly", {

  # Load the package data and prepare it
  data(micusp_biber)

  # Create a factor variable for testing
  test_data <- micusp_biber
  test_data$discipline <- as.factor(substr(test_data$doc_id, 1, 3))

  # Select a subset of columns for testing (factor + numeric variables)
  test_subset <- test_data[1:50, c("discipline", 
                                   "f_01_past_tense", 
                                   "f_02_perfect_aspect",
                                   "f_03_present_tense",
                                   "f_14_nominalizations",
                                   "f_16_other_nouns",
                                   "f_39_prepositions")]

  # Test basic functionality
  expect_no_error({
    result <- mda_loadings(test_subset, n_factors = 2)
  })

  # Test the result structure
  result <- mda_loadings(test_subset, n_factors = 2)

  expect_s3_class(result, "mda")
  expect_s3_class(result, "data.frame")
  expect_true("group" %in% colnames(result))
  expect_true("Factor1" %in% colnames(result))
  expect_true("Factor2" %in% colnames(result))

  # Test attributes
  expect_true("threshold" %in% names(attributes(result)))
  expect_true("loadings" %in% names(attributes(result)))
  expect_true("group_means" %in% names(attributes(result)))

  # Test that loadings is a data frame with correct dimensions
  loadings <- attributes(result)$loadings
  expect_s3_class(loadings, "data.frame")
  expect_equal(ncol(loadings), 2)  # Should have 2 factors

  # Test that group_means is a data frame
  group_means <- attributes(result)$group_means
  expect_s3_class(group_means, "data.frame")
  expect_true("group" %in% colnames(group_means))
})

test_that("mda_loadings input validation works", {

  # Test missing data frame
  expect_error(mda_loadings("not_a_dataframe", n_factors = 2),
               "obs_by_group must be a data frame")

  # Test invalid n_factors
  expect_error(mda_loadings(data.frame(x = 1:5, y = 1:5, z = factor(rep(c("A", "B"), c(2, 3)))), 
                           n_factors = "not_numeric"),
               "n_factors must be a positive integer")

  expect_error(mda_loadings(data.frame(x = 1:5, y = 1:5, z = factor(rep(c("A", "B"), c(2, 3)))), 
                           n_factors = -1),
               "n_factors must be a positive integer")

  # Test missing factor variable
  test_no_factor <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  expect_error(mda_loadings(test_no_factor, n_factors = 1),
               "You must have a single categorical variable formatted as a factor")

  # Test multiple factor variables
  test_multiple_factors <- data.frame(x = 1:10, y = 1:10, 
                                     z = factor(rep(c("A", "B"), 5)),
                                     w = factor(rep(c("C", "D"), 5)))
  expect_error(mda_loadings(test_multiple_factors, n_factors = 1),
               "You must have a single categorical variable formatted as a factor")

  # Test insufficient numeric variables
  test_few_numeric <- data.frame(x = 1:10, z = factor(rep(c("A", "B"), 5)))
  expect_error(mda_loadings(test_few_numeric, n_factors = 1),
               "You must have multiple numeric variables")
})

test_that("mda_loadings handles edge cases", {

  # Test with minimum viable data
  min_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 10)),
    var1 = c(rnorm(10, 5), rnorm(10, 10), rnorm(10, 15)),
    var2 = c(rnorm(10, 2), rnorm(10, 8), rnorm(10, 12)),
    var3 = c(rnorm(10, 1), rnorm(10, 3), rnorm(10, 7))
  )

  expect_no_error({
    result <- mda_loadings(min_data, n_factors = 1, cor_min = 0.1, threshold = 0.3)
  })

  # Test with different parameter values
  result_diff_params <- mda_loadings(min_data, n_factors = 1, cor_min = 0.5, threshold = 0.5)
  expect_equal(attributes(result_diff_params)$threshold, 0.5)
})
