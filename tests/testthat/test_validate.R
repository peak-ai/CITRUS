library(citrus)
library(testthat)
library(dplyr)

transactional_data <- citrus::transactional_data
data <- transactional_data %>% select(c('transactionid', 'transactionvalue', 'customerid', 'orderdate'))
output_preprocess <- citrus::preprocess(data, numeric_operation_list = 'mean')
preprocess_too_many_categories <- citrus::preprocessed_data %>%
  mutate(faulty_feature = as.character(customerid))
preprocess_no_customerid <- citrus::preprocessed_data %>%
  select(-customerid)

test_that("Supervised without response variable", {
  expect_error(citrus::validate(output_preprocess), regexp = "Columns missing: response")
})

test_that("Correct error when customerid is missing.", {
  expect_error(citrus::validate(preprocess_no_customerid), regexp = "Columns missing: customerid")
})

test_that("Throw error when too many categorical levels", {
  expect_error(citrus::validate(preprocess_too_many_categories), regexp = "Categorical Columns have too many levels: faulty_feature")
})

test_that("Unique customer count", {
  expect_equal(nrow(output_preprocess), length(unique(output_preprocess[["customerid"]])))
})
