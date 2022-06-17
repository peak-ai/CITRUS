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

test_that("No error with single type of feature (categorical/numeric)", {
  
  # Only numeric feature columns
  output_supervised <- segment(customer_data %>% select(id, recency, frequency, monetary, response),
                               steps = c('model'),
                               modeltype = 'tree', 
                               hyperparameters = list(dependent_variable = 'response',
                                                      min_segmentation_fraction = 0.1,
                                                      print_safety_check = 20,
                                                      number_of_segments = 4,
                                                      print_plot = FALSE))
  
  # Only categorical feature columns
  output_supervised <- segment(customer_data %>% select(id, top_country, spend_range, response),
                               steps = c('model'),
                               modeltype = 'tree', 
                               hyperparameters = list(dependent_variable = 'response',
                                                      min_segmentation_fraction = 0.01,
                                                      print_safety_check = 20,
                                                      number_of_segments = 2,
                                                      print_plot = FALSE))
  
  expect_true(TRUE)
})

test_that("Dependent variable other than 'response'", {
  
  output_supervised <- segment(customer_data %>% select(id, recency, frequency, monetary, response) %>% rename('target_var' = response),
                               steps = c('model'),
                               modeltype = 'tree', 
                               hyperparameters = list(dependent_variable = 'target_var',
                                                      min_segmentation_fraction = 0.1,
                                                      print_safety_check = 20,
                                                      number_of_segments = 4,
                                                      print_plot = FALSE))
  
  expect_true(TRUE)
})

test_that("Only run 'preprocess' step", {
  
  # Default supervised preprocessing
  output <- segment(data, steps = c('preprocess'))
  
  # Check rows
  expect_equal(nrow(output), 410)
  
  # Check column names
  expect_true(all(names(output) == c('id', 'recency', 'frequency', 'monetary', 'response')))
  
  
  # Unsupervised preprocessing
  output <- segment(data, steps = c('preprocess'), modeltype = 'k-clusters')
  
  # Check rows
  expect_equal(nrow(output), 410)
  
  # Check column names
  expect_true(all(names(output) == c('id', 'recency', 'frequency', 'monetary', 'transactionvalue_mean')))
})

test_that("k-clusters default output object check", {
  output_kclust <- segment(data, modeltype = 'k-clusters')
  
  # Should contain all the right variables
  expect_true(all(names(output_kclust) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 3 rows
  expect_equal(nrow(output_kclust$OutputTable), 3)
  # No nulls allowed in the output table
  expect_equal(ncol(output_kclust$OutputTable[complete.cases(output_kclust$OutputTable), ]), 3)
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_kclust$segments$segment)))
  expect_true(all(!is.na(output_kclust$segments$id)))
})

test_that("k-clusters custom output object check", {
  
  output_kclust <- segment(data, modeltype = 'k-clusters', hyperparameters = list(centers = 'auto',
                                                                                          iter_max = 35,
                                                                                          nstart = 2,
                                                                                          max_centers = 3, 
                                                                                          segmentation_variables = NULL,
                                                                                          standardize = TRUE))
  
  # Should contain all the right variables
  expect_true(all(names(output_kclust) %in% c('OutputTable', 'segments', 'CitrusModel')))
  # Output table should be 2 rows
  expect_equal(nrow(output_kclust$OutputTable), 2)
  # No nulls allowed in the output table
  expect_equal(ncol(output_kclust$OutputTable[complete.cases(output_kclust$OutputTable), ]), 3)
  # The segment lookup should not contain any NAs
  expect_true(all(!is.na(output_kclust$segments$segment)))
  expect_true(all(!is.na(output_kclust$segments$id)))
  # The model hyperparameters should agree with the custom ones
  expect_true(output_kclust$CitrusModel$model_hyperparameters$centers == 'auto')
  expect_true(output_kclust$CitrusModel$model_hyperparameters$iter_max == 35)
  expect_true(output_kclust$CitrusModel$model_hyperparameters$nstart == 2)
})
