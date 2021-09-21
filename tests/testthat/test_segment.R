library(citrus)
library(testthat)
library(dplyr)

transactional_data <- citrus::transactional_data
data <- transactional_data %>% select(c('transactionid', 'transactionvalue', 'id', 'orderdate'))

test_that("Supervised default output object check", {
  output_supervised <- segment(data, modeltype = 'tree')
  
  # Should contain all the right variables
  expect_true(all(names(output_supervised) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 6 rows
  expect_equal(nrow(output_supervised$OutputTable), 6)
  # No nulls allowed in the output table
  expect_equal(ncol(output_supervised$OutputTable[complete.cases(output_supervised$OutputTable), ]), 10)
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_supervised$segments$segment)))
  expect_true(all(!is.na(output_supervised$segments$id)))
})

test_that("Supervised custom output object check", {
  
  output_supervised <- segment(data, modeltype = 'tree', hyperparameters = list(dependent_variable = 'response',
                                                                                min_segmentation_fraction = 0.1,
                                                                                print_safety_check = 20,
                                                                                number_of_segments = 4,
                                                                                print_plot = FALSE))
  
  # Should contain all the right variables
  expect_true(all(names(output_supervised) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 4 rows
  expect_equal(nrow(output_supervised$OutputTable), 4)
  # No nulls allowed in the output table
  expect_equal(ncol(output_supervised$OutputTable[complete.cases(output_supervised$OutputTable), ]), 10)
  # There shouldn't be segments smaller than 10% of the total population
  expect_true(all(output_supervised$OutputTable$percentage >= 0.1))
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_supervised$segments$segment)))
  expect_true(all(!is.na(output_supervised$segments$id)))
  # The model hyperparameters should agree with the custom ones
  expect_true(output_supervised$CitrusModel$model_hyperparameters$dependent_variable == 'response')
  expect_true(output_supervised$CitrusModel$model_hyperparameters$min_segmentation_fraction == 0.1)
  expect_true(output_supervised$CitrusModel$model_hyperparameters$number_of_segments == 4)
})

test_that("Unsupervised default output object check", {
  output_unsupervised <- segment(data, modeltype = 'unsupervised')
  
  # Should contain all the right variables
  expect_true(all(names(output_unsupervised) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 3 rows
  expect_equal(nrow(output_unsupervised$OutputTable), 3)
  # No nulls allowed in the output table
  expect_equal(ncol(output_unsupervised$OutputTable[complete.cases(output_unsupervised$OutputTable), ]), 3)
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_unsupervised$segments$segment)))
  expect_true(all(!is.na(output_unsupervised$segments$id)))
})

test_that("Supervised custom output object check", {
  
  output_unsupervised <- segment(data, modeltype = 'unsupervised', hyperparameters = list(centers = 'auto',
                                                                                          iter_max = 35,
                                                                                          nstart = 2,
                                                                                          max_centers = 3, 
                                                                                          segmentation_variables = NULL,
                                                                                          standardize = TRUE))
  
  # Should contain all the right variables
  expect_true(all(names(output_unsupervised) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 2 rows
  expect_equal(nrow(output_unsupervised$OutputTable), 2)
  # No nulls allowed in the output table
  expect_equal(ncol(output_unsupervised$OutputTable[complete.cases(output_unsupervised$OutputTable), ]), 3)
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_unsupervised$segments$segment)))
  expect_true(all(!is.na(output_unsupervised$segments$id)))
  # The model hyperparameters should agree with the custom ones
  expect_true(output_unsupervised$CitrusModel$model_hyperparameters$centers == 'auto')
  expect_true(output_unsupervised$CitrusModel$model_hyperparameters$iter_max == 35)
  expect_true(output_unsupervised$CitrusModel$model_hyperparameters$nstart == 2)
})
