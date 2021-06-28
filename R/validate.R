#' Validation function
#'
#' Validates that the input data adheres to the expected format for modelling.
#' @param df data.frame, the data to validate
#' @param supervised logical, TRUE for supervised learning, FALSE for unsupervised
#' @importFrom dplyr n_distinct
#' @export
validate <- function(df, supervised = TRUE) {
  missing_columns <- c()
  other_errors <- c()
  toomanylevels_columns <- c()
  categorical_columns <- df[,names(df) != 'customerid'] %>% select_if(is.character) %>% summarise_all(n_distinct)
  
  if (!('response' %in% names(df)) & (supervised == TRUE)) {
    missing_columns <- c(missing_columns, 'response')
  }
  
  if (!('customerid' %in% names(df))) {
    missing_columns <- c(missing_columns, 'customerid')
  } else {
    if (n_distinct(df$customerid) != nrow(df)) {
      error_message <- 'Customer observations are not unique. nrow(df) > n_distinct(df$customerid).'
      other_errors <- c(other_errors, error_message)
    }
  }
  
  if (sum(!(names(df) %in% c('customerid', 'response'))) == 0) {
    error_message <- 'The dataframe does not contain any feature columns.'
    other_errors <- c(other_errors, error_message)
  }
  
  if (length(missing_columns) > 0 & length(other_errors) > 0) {
    stop(paste0('\n\nColumns missing: ', paste(missing_columns, collapse = ', '), '\n',
                '\nOther errors: \n', paste(other_errors, collapse = '\n')))
  } else if (length(missing_columns) > 0) {
    stop(paste0('\n\nColumns missing: ', paste(missing_columns, collapse = ', ')))
  } else if (length(other_errors) > 0) {
    stop(paste0('\n\n', paste(other_errors, collapse = '\n')))
  }
  
  if((nrow(df)>50)&(sum(categorical_columns>(nrow(df)*0.5))>0)){
    toomanylevels_columns <- names(categorical_columns)[categorical_columns>(nrow(df)*0.5)]
    stop(paste0('\n\nCategorical Columns have too many levels: ', paste(toomanylevels_columns, collapse = ', ')))
  }
    
  return (TRUE)
}


# df_test <- formatted %>%
#   select(customerid)
# validate(df_test, supervised = TRUE)
