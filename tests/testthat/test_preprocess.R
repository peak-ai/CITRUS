library(citrus)
library(testthat)
library(dplyr)

transactional_data <- citrus::transactional_data
data <- transactional_data %>% select(c('transactionid', 'transactionvalue', 'customerid', 'orderdate'))
output_preprocess <- citrus::preprocess(data, numeric_operation_list = 'mean')
output_preprocess_na <- citrus::preprocess(data, numeric_operation_list = NULL)

test_that("Number of Columns", {
  expect_equal(ncol(output_preprocess), 5)
})


test_that("String Customerid Check", {
  expect_true(is.character(typeof(output_preprocess$customerid)))
})

test_that("Passing NA to numeric_operations_list defaults to RFM", {
  expect_equal(ncol(output_preprocess_na), 4)
  expect_equal(sort(colnames(output_preprocess_na)), sort(c('customerid', 'recency', 'frequency', 'monetary')))
})


test_that("Correct Labelling", {
  expect_equal(colnames(output_preprocess), c('customerid', 'recency', 'frequency', 'monetary', 'transactionvalue_mean'))
})
