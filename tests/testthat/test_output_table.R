library(citrus)
library(testthat)
library(dplyr)

preprocessed_data <- citrus::preprocessed_data
hyperparameters <- list(dependent_variable = 'response',
                        min_segmentation_fraction = 0.05,
                        number_of_personas = 6,
                        print_plot = FALSE,
                        print_safety_check=20,
                        saveoutput = T)

model <- citrus::tree_segment(preprocessed_data, hyperparameters)
model <- citrus::tree_segment_prettify(model,print_plot = T)
model <- citrus::tree_abstract(model, preprocessed_data)
output <- citrus::output_table(preprocessed_data,model)

test_that("Number of Columns", {
  expect_equal(ncol(output), 16)
})


test_that("Number of Rows", {
  expect_equal(nrow(output), 6)
})

test_that("No nulls", {
  expect_equal(ncol(output[complete.cases(output), ]), 16)
})