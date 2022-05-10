
#' Creates pair plot from data table
#' @param model list, a citrus segmentation model
#' @param vars data.frame, the data to segment
#' @importFrom ggplot2 aes theme element_text
#' @return GGally object displaying the segment feature pair plots.
#' @export

citrus_pair_plot <- function(model,vars = NULL) {
 
  segments <- model$predicted_values$segment
  data <- model$input_data
  data <- data[ , -which(names(data) == "id")]
  if(!is.null(vars)){
    data <- data[,vars]
  }
 
  
  
  return(GGally::ggpairs(data, aes(colour = as.factor(segments), alpha = 0.5)) 
         + theme(text=element_text(size=13,  family="sans")))
  
}



