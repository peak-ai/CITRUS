#' Model management function
#'
#' Saves the model and its settings so that it can be recreated
#' @param model data.frame, the model to save
#' @param hyperparameters list, list of hyperparameters of the model
#' @importFrom utils object.size
#' @importFrom grDevices pdf dev.off
#' @export
model_management <- function(model,hyperparameters){
  if(exists('hyperparameters')){
    if(!is.null(hyperparameters$saveoutput)){
      if(hyperparameters$saveoutput){
        #Create Directory
        if(!dir.exists('~/segmentationoutputs')){
          dir.create('~/segmentationoutputs')
        }
        directory_path <- paste0('~/segmentationoutputs/',format(Sys.time(),format = '%Y-%m-%d-%H-%M-%S'))
        dir.create(directory_path)
        #Save model
        segment_model <- model$segment_model
        save(segment_model,
             file=paste0(directory_path,'/segment_model.RData'), ascii=TRUE)
        #Save hyperparameters
        model_hyperparameters <- model$model_hyperparameters
        save(model_hyperparameters,
             file=paste0(directory_path,'/model_hyperparameters.RData'), ascii=TRUE)
      
        predicted_values_size <- as.numeric(object.size(model$predicted_values))/1000000
        #Save predicted values
        predicted_values <- model$predicted_values
        if(predicted_values_size < 100){
          save(predicted_values,
               file=paste0(directory_path,'/predicted_values.RData'), ascii=TRUE)
        }
        #Save input data
        if(exists('hyperparameters')){
          if(!is.null(hyperparameters$saveinputdata)){
            if(hyperparameters$saveinputdata){
              input_data <- model$input_data
              input_data_size <- as.numeric(object.size(model$input_data))/1000000
              if(input_data_size < 100){
                save(input_data,
                     file=paste0(directory_path,'/input_data.RData'), ascii=TRUE)
              }
            }
          }
        }
        #Bespoke management layers - if(class(model) == 'abc'){...}
        #TODO: Save segment_table?
        # if(class(model) == 'abc'){
        #   save(model$segment_table,
        #        file=paste0(directory_path,'/segment_table.RData'), ascii=TRUE)
        # }
        if(class(model) == 'unsupervised'){
          outliers <- model$outliers_table
          if(nrow(outliers) > 0) {
            save(outliers,
                 file=paste0(directory_path,'/outliers_table.RData'), ascii=TRUE)
          }
        }  
      
        #Save rpart.plot
        if(class(model) == 'tree_model'){
           pdf(paste0(directory_path,'/tree.pdf'))
           rpart.plot_pretty(segment_model)
           dev.off()
        }
      }
    }
  }
}
