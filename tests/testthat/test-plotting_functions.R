# Test file for plotting functions: stickplot_mda, heatmap_mda, boxplot_mda
library(testthat)
library(mda.biber)

# Create shared test data for plotting functions
create_test_mda <- function() {
  data(micusp_biber)
  test_data <- micusp_biber
  test_data$discipline <- as.factor(substr(test_data$doc_id, 1, 3))

  test_subset <- test_data[1:100, c("discipline", 
                                    "f_01_past_tense", 
                                    "f_02_perfect_aspect",
                                    "f_03_present_tense",
                                    "f_14_nominalizations",
                                    "f_16_other_nouns",
                                    "f_39_prepositions",
                                    "f_42_adverbs")]

  mda_loadings(test_subset, n_factors = 2)
}

test_that("stickplot_mda function works correctly", {

  mda_result <- create_test_mda()

  # Test basic functionality
  expect_no_error({
    plot1 <- stickplot_mda(mda_result)
  })

  # Test with different factor
  expect_no_error({
    plot2 <- stickplot_mda(mda_result, n_factor = 2)
  })

  # Test that result is a ggplot
  plot_result <- stickplot_mda(mda_result)
  expect_s3_class(plot_result, "gg")
  expect_s3_class(plot_result, "ggplot")
})

test_that("stickplot_mda input validation works", {

  # Test with non-mda object
  expect_error(stickplot_mda(data.frame(x = 1:5, y = 1:5)),
               "Your mda_data must be an mda object")

  # Test with invalid n_factor
  mda_result <- create_test_mda()

  expect_error(stickplot_mda(mda_result, n_factor = "not_numeric"),
               "n_factor must be a positive integer")

  expect_error(stickplot_mda(mda_result, n_factor = -1),
               "n_factor must be a positive integer")

  expect_error(stickplot_mda(mda_result, n_factor = 0),
               "n_factor must be a positive integer")
})

test_that("heatmap_mda function works correctly", {

  mda_result <- create_test_mda()

  # Test basic functionality (expect some ggplot warnings)
  expect_no_error({
    suppressWarnings(plot1 <- heatmap_mda(mda_result))
  })

  # Test with different factor
  expect_no_error({
    suppressWarnings(plot2 <- heatmap_mda(mda_result, n_factor = 2))
  })

  # Test that result is a plot object (ggarrange returns different class)
  plot_result <- suppressWarnings(heatmap_mda(mda_result))
  expect_true(!is.null(plot_result))
})

test_that("heatmap_mda input validation works", {

  # Test with non-mda object
  expect_error(heatmap_mda(data.frame(x = 1:5, y = 1:5)),
               "Your mda_data must be an mda object")

  # Test with invalid n_factor
  mda_result <- create_test_mda()

  expect_error(heatmap_mda(mda_result, n_factor = "not_numeric"),
               "n_factor must be a positive integer")

  expect_error(heatmap_mda(mda_result, n_factor = -1),
               "n_factor must be a positive integer")
})

test_that("boxplot_mda function works correctly", {

  mda_result <- create_test_mda()

  # Test basic functionality
  expect_no_error({
    plot1 <- boxplot_mda(mda_result)
  })

  # Test with different factor
  expect_no_error({
    plot2 <- boxplot_mda(mda_result, n_factor = 2)
  })

  # Test that result is a plot object
  plot_result <- boxplot_mda(mda_result)
  expect_true(!is.null(plot_result))
})

test_that("boxplot_mda input validation works", {

  # Test with non-mda object
  expect_error(boxplot_mda(data.frame(x = 1:5, y = 1:5)),
               "Your mda_data must be an mda object")

  # Test with invalid n_factor
  mda_result <- create_test_mda()

  expect_error(boxplot_mda(mda_result, n_factor = "not_numeric"),
               "n_factor must be a positive integer")

  expect_error(boxplot_mda(mda_result, n_factor = -1),
               "n_factor must be a positive integer")
})

test_that("plotting functions handle edge cases", {

  # Create MDA result with minimal data
  min_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 15)),
    var1 = c(rnorm(15, 5), rnorm(15, 10), rnorm(15, 15)),
    var2 = c(rnorm(15, 2), rnorm(15, 8), rnorm(15, 12)),
    var3 = c(rnorm(15, 1), rnorm(15, 3), rnorm(15, 7)),
    var4 = c(rnorm(15, 0), rnorm(15, 2), rnorm(15, 5))
  )

  mda_min <- mda_loadings(min_data, n_factors = 1, threshold = 0.1)

  # All plotting functions should handle minimal data
  expect_no_error({
    stickplot_mda(mda_min)
  })

  expect_no_error({
    suppressWarnings(heatmap_mda(mda_min))
  })

  expect_no_error({
    boxplot_mda(mda_min)
  })
})
