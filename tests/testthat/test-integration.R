# Integration tests and edge cases
library(testthat)
library(mda.biber)

test_that("complete workflow integration test", {

  # Load and prepare data
  data(micusp_biber)
  test_data <- micusp_biber[1:100, ]
  test_data$discipline <- as.factor(substr(test_data$doc_id, 1, 3))

  # Select meaningful subset of variables
  biber_subset <- test_data[, c("discipline", 
                               "f_01_past_tense", 
                               "f_02_perfect_aspect",
                               "f_03_present_tense", 
                               "f_14_nominalizations",
                               "f_16_other_nouns",
                               "f_17_agentless_passives",
                               "f_39_prepositions",
                               "f_42_adverbs")]

  # Complete MDA workflow
  expect_no_error({
    # Step 1: Run MDA
    mda_result <- mda_loadings(biber_subset, n_factors = 2, 
                               cor_min = 0.15, threshold = 0.30)

    # Step 2: Create all plots
    stick_plot <- stickplot_mda(mda_result, n_factor = 1)
    suppressWarnings(heat_plot <- heatmap_mda(mda_result, n_factor = 1))
    box_plot <- boxplot_mda(mda_result, n_factor = 1)
    screeplot_mda(biber_subset, cor_min = 0.15)
  })

  # Verify results have expected structure
  mda_result <- mda_loadings(biber_subset, n_factors = 2)

  expect_true(nrow(mda_result) == nrow(biber_subset))
  expect_true(ncol(mda_result) >= 3)  # group + at least 2 factors
  expect_true(all(c("group", "Factor1", "Factor2") %in% colnames(mda_result)))
})

test_that("MDA handles different parameter combinations", {

  # Create test data
  test_data <- data.frame(
    category = factor(rep(c("Type1", "Type2", "Type3"), each = 20)),
    measure1 = c(rnorm(20, 10, 2), rnorm(20, 15, 3), rnorm(20, 8, 1.5)),
    measure2 = c(rnorm(20, 5, 1), rnorm(20, 12, 2), rnorm(20, 7, 1)),
    measure3 = c(rnorm(20, 20, 4), rnorm(20, 18, 3), rnorm(20, 25, 5)),
    measure4 = c(rnorm(20, 3, 0.5), rnorm(20, 8, 1), rnorm(20, 5, 0.8)),
    measure5 = c(rnorm(20, 12, 2), rnorm(20, 10, 1.5), rnorm(20, 15, 3))
  )

  # Test different parameter combinations
  expect_no_error({
    # High correlation threshold
    result1 <- mda_loadings(test_data, n_factors = 1, cor_min = 0.3, threshold = 0.4)

    # Low correlation threshold  
    result2 <- mda_loadings(test_data, n_factors = 1, cor_min = 0.1, threshold = 0.2)

    # Multiple factors
    result3 <- mda_loadings(test_data, n_factors = 2, cor_min = 0.2, threshold = 0.3)
  })
})

test_that("error handling works across all functions", {

  # Test various error conditions
  expect_error(mda_loadings(NULL, 1))
  expect_error(stickplot_mda(NULL))
  expect_error(heatmap_mda(data.frame(x = 1:5)))
  expect_error(boxplot_mda("not_mda"))
  expect_error(screeplot_mda(list(a = 1, b = 2)))

  # Test with realistic minimal data 
  set.seed(123)  # For reproducible test
  edge_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 8)),
    var1 = c(rnorm(8, 5), rnorm(8, 10), rnorm(8, 15)),
    var2 = c(rnorm(8, 3), rnorm(8, 7), rnorm(8, 12)),
    var3 = c(rnorm(8, 8), rnorm(8, 6), rnorm(8, 9))
  )

  # Should work with minimal realistic data
  expect_no_error({
    result <- mda_loadings(edge_data, n_factors = 1, cor_min = 0.1, threshold = 0.1)
  })
})
