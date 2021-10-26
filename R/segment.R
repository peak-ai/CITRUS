#' Segment Function
#'
#' Segments the data by running all steps in the segmentation pipeline, including output table
#' @param data data.frame, the data to segment
#' @param modeltype character, the type of model to use to segment choices are: 'tree', 'k-clusters'
#' @param FUN function, A user specified function to segment, if the standard methods are not wanting to be used
#' @param FUN_preprocess function, A user specified function to preprocess, if the standard methods are not wanting to be used
#' @param steps list, names of the steps the user want to run the data on. Options are 'preprocess' and 'model'
#' @param prettify logical, TRUE if want cleaned up outputs, FALSE for raw output
#' @param print_plot logical, TRUE if want to print the plot
#' @param hyperparameters list of hyperparameters to use in the model.
#' @param force logical, TRUE to ignore errors in validation step and force model execution.
#' @param verbose logical whether information about the segmentation pipeline should be given.
#' @return A list of three objects. A tibble providing high-level segment attributes, a lookup table (data frame)
#' with the id and predicted segment number, and an rpart object defining the model.
#' @importFrom utils modifyList
#' @export
segment <- function(data,
                    modeltype = c('tree', 'k-clusters'),
                    FUN = NULL,
                    FUN_preprocess = NULL,
                    steps = c('preprocess', 'model'),
                    prettify = FALSE,
                    print_plot = FALSE,
                    hyperparameters = NULL, 
                    force = FALSE, 
                    verbose = FALSE) {
  
  steps <- match.arg(steps, several.ok = TRUE)
  modeltype <- match.arg(modeltype)

  # Data processing layer
  # returns data in appropriate format called 'data'
  if ('preprocess' %in% steps) {
    if(verbose == TRUE) {message('Preprocessing data')}
    if (is.null(FUN_preprocess)) {
      if(verbose == TRUE) {message('Using default preprocessing')}
      if (modeltype == 'tree') {
        data <- preprocess(data, target = 'transactionvalue', target_agg = 'mean', verbose = verbose)
      } else if (modeltype == 'k-clusters') {
        data <- preprocess(data, verbose = verbose)
      }
    } else {
      if(verbose == TRUE) {message('Using custom preprocessing')}
      data <- FUN_preprocess(data)
    }
  }

  # Model selection layer
  if ('model' %in% steps) {
    if(verbose == TRUE) {message('Setting up model')}
    if (is.null(FUN)) {

      # Tree Model
      if (modeltype == 'tree') {
        if(verbose == TRUE) {message('Tree based model chosen')}
        if(verbose == TRUE) {message('Validating input data')}
        
        # Default hyperparameters
        default_hyperparameters = list(dependent_variable = 'response',
                                       min_segmentation_fraction = 0.05,
                                       number_of_segments = 6,
                                       print_plot = ifelse(prettify == FALSE, print_plot, FALSE),
                                       print_safety_check=20)
        if(is.null(hyperparameters)){
          if(verbose == TRUE) {message('Using default hyper-parameters')}
          hyperparameters = default_hyperparameters
        }else{
          hyperparameters = modifyList(default_hyperparameters, hyperparameters)
        }
        
        validate(data, supervised = TRUE, force = force, hyperparameters)
        
        if(verbose == TRUE) {message('Training model')}
        model = tree_segment(data, hyperparameters, verbose = verbose)
        View(model)
        if(verbose == TRUE) {message('Number of segments: ', paste0(max(model$segment_table$segment, '\n')))}

        # Prettify layer
        if(prettify == TRUE){
          if(verbose == TRUE) {message('Prettifying output data')}
          model <- tree_segment_prettify(model, print_plot = print_plot)
        }

        # Abstraction layer
        if(verbose == TRUE) {message('Abstracting model')}
        model <- tree_abstract(model, data)
      }

      # Model B
      if (modeltype == 'k-clusters') {
        if(verbose == TRUE) {message('k-clusters model chosen')}
  
        if(verbose == TRUE) {message('Validating input data')}
  
        # Default hyperparameters
        default_hyperparameters = list(centers = 'auto',
                                       iter_max = 50,
                                       nstart = 5,
                                       max_centers = 5, 
                                       segmentation_variables = NULL,
                                       standardize = TRUE)
        
        if(is.null(hyperparameters)){
          if(verbose == TRUE) {message('Using default hyper-parameters')}
          hyperparameters = default_hyperparameters
        }else{
          hyperparameters = modifyList(default_hyperparameters, hyperparameters)
        }
        
        validate(data, supervised = FALSE, force = force, hyperparameters)
  
        if(verbose == TRUE) {message('Training model')}
        model = k_clusters(data, hyperparameters, verbose = verbose)
  
        # Prettify layer
        if(prettify == TRUE){
          if(verbose == TRUE) {message('Prettifying output data')}
          citrus_pair_plot(model)
        }
      }

    } else {
      # User defined model
      if(verbose == TRUE) {message('Using custom model')}
      model <- FUN(data)
      # Abstraction layer
    }
  }
  # Model management layer
  model_management(model,hyperparameters)

  # Output
  if(verbose == TRUE) {message('Generating output table')}
  output <- output_table(data, model)
  
  

  if(verbose == TRUE) {message('Finished!')}
  return(list('OutputTable' = output,"segments" =  model$predicted_values ,"CitrusModel" = model))

}

