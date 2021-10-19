#' Preprocess Function
#'
#' Transforms a transactional table into an id aggregated table with custom options for aggregation methods for numeric and categorical columns.
#' @param df data.frame, the data to preprocess
#' @param samplesize numeric, the fraction of ids used to create a sub-sample of the input df
#' @param numeric_operation_list list, a list of the aggregation functions to apply to numeric columns
#' @param categories list, a list of the categorical columns to aggregate
#' @param target character, the column to use as a response variable for supervised learning
#' @param target_agg character, the aggregation function to use to aggregate the target column
#' @importFrom dplyr group_by summarise n_distinct ungroup select summarise_if inner_join n arrange desc filter row_number left_join %>%
#' @importFrom rlang .data
#' @param verbose logical whether information about the preprocessing should be given
#' @export
preprocess <- function(df, 
                       samplesize = NA,
                       numeric_operation_list = c('mean'),
                       categories = NULL,
                       target = NA,
                       target_agg = 'mean', verbose = TRUE) {
  
  # Warning: Rename data
  print('Please ensure columns are renamed accordingly:')
  print('Unique Identifier: id')
  print('Transaction Identifier: transactionid')
  print('Transaction Date: orderdate')
  print('Value Column: transactionvalue')
  print(paste0('Target column: ', target, ' (', target_agg, ')'))
  
  # Column name check
  need_to_have <- c('id', 'transactionid', 'orderdate', 'transactionvalue')
  if (!all(need_to_have %in% names(df))) {
    stop('Missing need to haves')
  }
  
  if(is.null(categories)){
    othercols <- names(df)[!names(df) %in% need_to_have]
    df_other <- df[,othercols]
    characterlevel <- lapply(df_other,is.character)==TRUE
    if(sum(characterlevel)>=1){
      categories <- names(df_other)[characterlevel]
    }
  }
  
  # Sampling
  if (!is.na(samplesize) & samplesize <= 1) {
    n_samples <- as.integer(round(nrow(df)*samplesize))
    df <- df[sample(nrow(df), n_samples), ]
  } else if (!is.na(samplesize) & samplesize > 1) {
    stop('Samplesize argument must be between 0 and 1')
  }
  
  # Standard column formatting
  df$orderdate <- as.Date(df$orderdate)
  df$id <- as.character(df$id)
  df$transactionvalue <- as.numeric(df$transactionvalue)
  
  # RFM aggregations
  latest_date <- max(df$orderdate)
  final_df <- df %>%
    group_by(.data$id) %>%
    summarise(recency = as.integer(latest_date - max(.data$orderdate, na.rm = TRUE)),
              frequency = n_distinct(.data$transactionid),
              monetary = sum(.data$transactionvalue, na.rm = TRUE)) %>%
    ungroup()

  # Filters numeric columns and performs aggregations defined by input
  # numeric_operation_list parameter.
  if (any(!(is.null(numeric_operation_list) | is.na(numeric_operation_list)))) {
    function_vector <- strings_to_functions(numeric_operation_list)
    names(function_vector) <- numeric_operation_list
    
    if(!is.na(target)) {
      numeric_df <- df %>% 
        select(-target) %>%
        group_by(.data$id) %>% 
        summarise_if(is.numeric, function_vector) %>% 
        ungroup()
    } else {
      numeric_df <- df %>% 
        group_by(.data$id) %>% 
        summarise_if(is.numeric, function_vector) %>% 
        ungroup()
    }

    if (is.na(target)) {
      evaluated_columns <- names(df)[sapply(df, is.numeric) & names(df) != 'id']
    } else {
      evaluated_columns <- names(df)[sapply(df, is.numeric) & names(df) != 'id' & names(df) != target]
    }
    

    if (length(evaluated_columns) == 1) {
      adjusted_name <- paste0(evaluated_columns, '_', names(numeric_df)[!(names(numeric_df) %in% c('id', target))])
      names(numeric_df) <- c('id', adjusted_name)
    }

    # Filters categorical columns and grabs the top n category for each
    # categorical column
    final_df <- inner_join(final_df, numeric_df, by = 'id')
  }
  

  if (!is.null(categories)) {
    for (col_name in categories) {
      if(!is.na(target)) {
        temp_df <- df %>%
          select(-target) %>%
          group_by(.data$id, !!as.symbol(col_name)) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(.data$id) %>%
          arrange(desc(n)) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          select(-n)
        var <- paste0('top_', col_name)
        temp_df[var] <- temp_df[col_name]

      } else {
        temp_df <- df %>%
          group_by(.data$id, !!as.symbol(col_name)) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(.data$id) %>%
          arrange(desc(n)) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          select(-n)
        var <- paste0('top_', col_name)
        temp_df[var] <- temp_df[col_name]
      }      
      final_df <- inner_join(final_df, temp_df, by = 'id')
    }
    
    final_df <- select(final_df, -categories)
  }
  
  # Labels the dataset with the 'target' column if given
  if (!is.na(target)) {
    if(verbose == TRUE) {message('Calculating target values')}
    target_df <- df %>%
      group_by(.data$id) %>%
      summarise(response = get(target_agg)(!!as.symbol(target), na.rm = TRUE)) %>%
      ungroup()
    
    final_df <- left_join(final_df, target_df, by = 'id')
  }
  
  return(final_df)
}
strings_to_functions <- function(string_vector) {
  function_vector <- c()
  for (obj_name in string_vector) {
    function_vector <- c(function_vector, get(obj_name))
  }
  return(function_vector)
}
