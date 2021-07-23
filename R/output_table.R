#' Output Table
#'
#' Generates the output table for model and data
#' @param data A dataframe generated from the pre-processing step
#' @param model A model object used to classify customers with, generated from the model selection layer
#' @importFrom dplyr left_join select mutate group_by summarise summarise_each funs
#' @importFrom rlang .data
#' @export
output_table <- function(data, model) {
  #TODO: Add summary stats for the predictors
  output <- data.frame(segment = model$predicted_values$persona,
                       customerid = as.character(model$predicted_values$customerid), 
                       stringsAsFactors = FALSE)
  
  response <- model$model_hyperparameters$dependent_variable
  
  df <- left_join(data, output, by = 'customerid')
  
  segmentation_vars <- model$model_hyperparameters$segmentation_variables
  
  if(is.null(segmentation_vars)){
    allcolumnnames <- colnames(df)
    segmentation_vars <- allcolumnnames[!allcolumnnames %in% c('customerid', response , 'segment')]  
  }
  
  df_agg <- df %>% select(c('segment',model$model_hyperparameters$segmentation_variables)) 
  characterlevel <- lapply(df_agg,is.character)==T
  df_agg <- df_agg %>% 
    group_by(.data$segment) %>% 
    summarise_each(funs(if(is.numeric(.data$.)) mean(.data$., na.rm = TRUE) else mode(.data$.)))
  names(df_agg)[characterlevel] <- paste0(names(df_agg)[characterlevel],'_mode')
  names(df_agg)[!characterlevel] <- paste0(names(df_agg)[!characterlevel],'_mean')
  names(df_agg)[1] <- 'segment'
  
  
  df_agg2 <- 
    df %>% select(c('segment',model$model_hyperparameters$segmentation_variables)) %>% 
    group_by(.data$segment) %>% 
    summarise_each(funs(if(is.numeric(.data$.)) range_output(.data$.) else mode(.data$.,max =F)))
  names(df_agg2)[characterlevel] <- paste0(names(df_agg2)[characterlevel],'_min')
  names(df_agg2)[!characterlevel] <- paste0(names(df_agg2)[!characterlevel],'_range')
  names(df_agg2)[1] <- 'segment'
  
  df_agg <- df_agg %>% left_join(df_agg2, by = 'segment') 
  df_agg <- df_agg[,c(1,order(colnames(df_agg)[-1])+1)]
  
  if(response %in% names(df)) {
    df <- df %>%
      group_by(.data$segment)%>%
      summarise(n = n(), mean_value = mean(as.numeric(as.character(.data[[response]])),na.rm=T)) %>%
      mutate(percentage = .data$n/sum(.data$n)) %>% 
        left_join(df_agg, by = 'segment')

  } else {
    df <- df %>%
      group_by(.data$segment)%>%
      summarise(n = n()) %>%
      mutate(mean_value = NULL, percentage = 100*round(.data$n/sum(.data$n),3)) %>% 
        left_join(df_agg, by = 'segment')
    
  }

  return(df)

}

mode <- function(codes, max = T){
  codes <- as.factor(codes)
  if(max == T){
    levels(codes)[which.max(tabulate(codes))]
  }else{
    levels(codes)[which.min(tabulate(codes))]
  }
}
range_output <- function(codes){
  min_codes <- min(codes, na.rm = TRUE)
  max_codes <- max(codes, na.rm = TRUE)
  output <- paste0(min_codes,' - ',max_codes)
  return(output)
}
