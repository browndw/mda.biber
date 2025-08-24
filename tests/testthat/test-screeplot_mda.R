# Test file for screeplot_mda function
library(testthat)
library(mda.biber)

test_that("screeplot_mda function works correctly", {

  # Load test data
  data(micusp_biber)
  test_data <- micusp_biber
  test_data$discipline <- as.factor(substr(test_data$doc_id, 1, 3))

  # Select subset for testing (need more variables for eigenvalues)
  test_subset <- test_data[1:50, c("discipline", 
                                   "f_01_past_tense", 
                                   "f_02_perfect_aspect",
                                   "f_03_present_tense",
                                   "f_14_nominalizations",
                                   "f_16_other_nouns",
                                   "f_39_prepositions",
                                   "f_42_adverbs",
                                   "f_44_mean_word_length")]

  # Test basic functionality (this should create a plot without errors)
  expect_no_error({
    screeplot_mda(test_subset)
  })

  # Test with different cor_min parameter
  expect_no_error({
    screeplot_mda(test_subset, cor_min = 0.1)
  })

  expect_no_error({
    screeplot_mda(test_subset, cor_min = 0.5)
  })
})

test_that("screeplot_mda input validation works", {

  # Test with non-data frame input
  expect_error(screeplot_mda("not_a_dataframe"),
               "obs_by_group must be a data frame")

  # Test with insufficient numeric variables
  test_few_vars <- data.frame(x = 1:10, group = factor(rep(c("A", "B"), 5)))
  expect_error(screeplot_mda(test_few_vars),
               "You must have multiple numeric variables")

  # Test with no numeric variables
  test_no_numeric <- data.frame(group1 = factor(rep(c("A", "B"), 5)),
                               group2 = factor(rep(c("C", "D"), 5)))
  expect_error(screeplot_mda(test_no_numeric),
               "You must have multiple numeric variables")
})

test_that("screeplot_mda handles data preprocessing correctly", {

  # Test data with some zero columns (should be removed) 
  # Need enough variables to get 3+ eigenvalues
  test_data_with_zeros <- data.frame(
    group = factor(rep(c("A", "B"), each = 20)),
    var1 = c(rnorm(20, 5), rnorm(20, 10)),
    var2 = rep(0, 40),  # All zeros - should be removed
    var3 = c(rnorm(20, 2), rnorm(20, 8)),
    var4 = c(rnorm(20, 1), rnorm(20, 3)),
    var5 = c(rnorm(20, 0), rnorm(20, 2)),
    var6 = c(rnorm(20, 3), rnorm(20, 7)),
    var7 = c(rnorm(20, 1), rnorm(20, 4))
  )

  # Should work despite zero column
  expect_no_error({
    screeplot_mda(test_data_with_zeros)
  })
})
