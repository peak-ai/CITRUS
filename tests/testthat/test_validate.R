library(citrus)
library(testthat)
library(dplyr)

transactional_data <- citrus::transactional_data
data <- transactional_data %>% select(c('transactionid', 'transactionvalue', 'customerid', 'orderdate'))
output_preprocess <- citrus::preprocess(data, numeric_operation_list = 'mean')

test_that("Supervised without response variable", {
  expect_error(validate(output_preprocess))
})


test_that("Unique customer count", {
  expect_equal(nrow(output_preprocess), length(unique(output_preprocess[["customerid"]])))
})