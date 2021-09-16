#' Validation function
#'
#' Validates that the input data adheres to the expected format for modelling.
#' @param df data.frame, the data to validate
#' @param supervised logical, TRUE for supervised learning, FALSE for unsupervised
#' @param force logical, TRUE to ignore error on categorical columns
#' @param hyperparameters list of hyperparameters used in the model
#' @importFrom dplyr n_distinct summarise_all select_if
#' @export

validate <- function(df, supervised = TRUE, force, hyperparameters) {
  missing_columns <- c()
  other_errors <- c()
  toomanylevels_columns <- c()

  categorical_columns <- df[,names(df) != 'id'] %>% select_if(is.character) %>% summarise_all(n_distinct)

  
  if(supervised == TRUE) {
    index <- which(colnames(df) == hyperparameters$dependent_variable)
    colnames(df)[index] <- "response"
  }
  
  if (!('response' %in% names(df)) & (supervised == TRUE)) {
    missing_columns <- c(missing_columns, hyperparameters$dependent_variable)
  }
  
  if (!('id' %in% names(df))) {
    missing_columns <- c(missing_columns, 'id')
  } else {
    if (n_distinct(df$id) != nrow(df)) {
      error_message <- 'ID observations are not unique. nrow(df) > n_distinct(df$id).'
      other_errors <- c(other_errors, error_message)
    }
    if (!is.null(hyperparameters$segmentation_variables)) {
      variables <- c("id", hyperparameters$segmentation_variables)
      df <- df[, variables]
    }
  }
  
  if (sum(!(names(df) %in% c('id', 'response'))) == 0) {
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
    if(force == FALSE) {
      stop(paste0('\n\nCategorical Columns have too many levels: ', paste(toomanylevels_columns, collapse = ', ')))
    } 
  }
  
  if(sum(infinitecounts>0) > 0){
    infinitecolumns <- names(infinitecounts)[which(infinitecounts> 0 )]
    stop(paste0('\n\ Columns have infinite values (remove Inf to continue): ', paste(infinitecolumns, collapse = ', ')))
  }                                                                   
                                                                     
  return (TRUE)
}


# df_test <- formatted %>%
#   select(id)
# validate(df_test, supervised = TRUE)
