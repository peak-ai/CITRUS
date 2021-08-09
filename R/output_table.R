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
  
  df <- left_join(data, output, by = 'customerid')
  
  segmentation_vars <- model$model_hyperparameters$segmentation_variables
  
  if(is.null(segmentation_vars)){
    allcolumnnames <- colnames(df)
    segmentation_vars <- allcolumnnames[!allcolumnnames %in% c('customerid', 'response', 'segment')]  
  }
  
  df_agg <- df %>% select(c('segment',model$model_hyperparameters$segmentation_variables)) 
  characterlevel <- lapply(df_agg,is.character)==T
  df_agg <- df_agg %>% 
    group_by(.data$segment) %>% 
    summarise_each(funs(if(is.numeric(.data$.)) round(mean(.data$., na.rm = TRUE),2) else mode(.data$.)))
  names(df_agg)[characterlevel] <- paste0(names(df_agg)[characterlevel],'_mode')
  names(df_agg)[!characterlevel] <- paste0(names(df_agg)[!characterlevel],'_mean')
  names(df_agg)[1] <- 'segment'
  
  
  df_agg2 <- 
    df %>% select(c('segment',model$model_hyperparameters$segmentation_variables)) %>% 
    group_by(.data$segment) %>% 
    summarise_each(funs(if(is.numeric(.data$.)) range_output(.data$.) else top5categories(.data$.)))
  names(df_agg2)[characterlevel] <- paste0(names(df_agg2)[characterlevel],'_top5')
  names(df_agg2)[!characterlevel] <- paste0(names(df_agg2)[!characterlevel],'_range')
  names(df_agg2)[1] <- 'segment'
  
  df_agg <- df_agg %>% left_join(df_agg2, by = 'segment') 
  df_agg <- df_agg[,c(1,order(colnames(df_agg)[-1])+1)]
  
  if('response' %in% names(df)) {
    df <- df %>%
      group_by(.data$segment)%>%
      summarise(n = n(), mean_value = mean(as.numeric(as.character(.data$response)),na.rm=T)) %>%
      mutate(percentage = paste0(100*round((.data$n/sum(.data$n)),3),'%')) %>% 
        left_join(df_agg, by = 'segment')

  } else {
    df <- df %>%
      group_by(.data$segment)%>%
      summarise(n = n()) %>%
      mutate(mean_value = NULL, percentage = paste0(100*round(.data$n/sum(.data$n),3),'%')) %>% 
        left_join(df_agg, by = 'segment')
    
  }

  return(df)

}

top5categories <- function(codes){
  codes <- as.factor(codes)
  codes_table <- tabulate(codes)
  top5categories_input <- round(100*codes_table/sum(codes_table),2)
  top5categories_input_values <- top5categories_input[order(top5categories_input,decreasing = T)[1:5]]
  top5categories_input_names <- levels(codes)[order(top5categories_input,decreasing = T)[1:5]]
  top5categories_input_values <- top5categories_input_values[!is.na(top5categories_input_values)]
  top5categories_input_names <- top5categories_input_names[!is.na(top5categories_input_names)]
  top5categories_output <- paste0(top5categories_input_names, ' - ',top5categories_input_values,'%',collapse = '; ')
  return(top5categories_output)
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
  min_codes <- round(min(codes,na.rm = T),2)
  max_codes <- round(max(codes,na.rm = T),2)
  output <- paste0(min_codes,' - ',max_codes)
  return(output)
}
