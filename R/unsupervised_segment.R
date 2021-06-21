#' Unsupervised model
#'
#' Unsupervised method for segmentation. It can handle segmentation for both numerical data types only, by using k-means algorithm, and mixed data types (numerical and categorical) by using k-prototypes algorithm 
#' @param data data.frame, the data to segment
#' @param hyperparameters list of hyperparameters to pass. They include
#' centers: number of clusters or a set of initial (distinct) cluster centers, or 'auto'. When 'auto' is chosen, the number of clusters is optimised; \cr
#' iter_max: the maximum number of iterations allowed; 
#' n_start: how many random sets of cluster centers should be tried;
#' max_centers: maximum number of clusters when 'auto' option is selected for the centers; 
#' segmentation_variables: the columns to use to segment on.
#' standardize: whether to standardize numeric columns.
#' @importFrom stats kmeans ave sd dist var
#' @importFrom clustMixType lambdaest kproto
#' @importFrom dplyr n_distinct %>% mutate_if
#' @importFrom utils capture.output
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_line geom_point geom_text scale_x_continuous xlab ylab
#' @param verbose logical whether information about the clustering procedure should be given.
#' @export
#'
unsupervised_segment <- function(data, hyperparameters, verbose = TRUE){
  
  
  if(is.null(hyperparameters$segmentation_variables)){
    segmentation_variables <- colnames(data)
    
  }else{
    segmentation_variables <- hyperparameters$segmentation_variables
    data <- data[, segmentation_variables]

  }
  input_params <- list(centers = hyperparameters$centers,
                       iter_max = hyperparameters$iter_max,
                       nstart = hyperparameters$nstart,
                       segmentation_variables = hyperparameters$segmentation_variables)
  
  # treatment of missings
  if(verbose == TRUE) {message("Checking for NAs in variables")}
  NAcount <- apply(data, 2, function(z) sum(is.na(z)))
  
  if(any(NAcount == nrow(data))) stop(paste("Variables have only NAs please remove them:",names(NAcount)[NAcount == nrow(data)],"!"))
  miss <- apply(data, 1, function(z) any(is.na(z)))
  
  if(sum(miss) > 0) {warning("# NAs in variables:\n", paste(capture.output(print(NAcount)), collapse = "\n"), "\n", 
          paste0(sum(miss), " observation(s) with NAs.\n", 
          "Observations with NAs are removed.\n")) }
  else{ 
    if(verbose == TRUE) message("Variables have 0 NAs")}
  
  # remove missings
  data <- data[!miss,]
  
  ids <- data[,'customerid']
  data <- data[,(names(data) != 'customerid')]
  
  #Standardize
  zscore <- function(x){
    return((x-mean(x))/sd(x))
  }
  if(hyperparameters$standardize == TRUE){
    data <- data %>% mutate_if(is.numeric, ~ zscore(.x))
  }
  
  #Kmeans ++ algorithm
  kmeanspp <- function(data, centers = 2,
                       start = "random", iter.max = 100,
                       nstart = 10, ...) {
    
    kk <- centers
    
    if (length(dim(data)) == 0) {
      data <- matrix(data, ncol = 1)
    } else {
      data <- cbind(data)
    }
    
    
    if(length(centers) != 1L){
      out <- kmeans(data, centers = centers, iter.max = iter.max)
    }else{
      num.samples <- nrow(data)
      ndim <- ncol(data)
      out <- list()
      unique <- which(!duplicated(data))
      out$tot.withinss <- Inf
      for (restart in seq_len(nstart)) {
        center_ids <- rep(0, length = kk)
        if (start == "random"){
          center_ids[1:2] = sample(unique, 1)
          for (ii in 2:kk) { # the plus-plus step in kmeans
            if (ndim == 1){
              dists <- apply(cbind(data[center_ids, ]), 1,
                             function(center) {rowSums((data - center)^2)})
            } else {
              dists <- apply(data[center_ids, ], 1,
                             function(center) {rowSums((data - center)^2)})
            }
            probs <- apply(dists, 1, min)
            probs[center_ids] <- 0
            
            center_ids[ii] <- sample(unique, 1, prob = probs[unique],replace = F)
          }
        } else{
          center_ids = start
        }
        
        tmp.out <- kmeans(data, centers = data[center_ids, ], iter.max = iter.max, ...)
        tmp.out$inicial.centers <- data[center_ids, ]
        if (tmp.out$tot.withinss < out$tot.withinss){
          out <- tmp.out
        }
      }
    }
    return(out)
  }
  
  outliers_kproto <- function(data, km) {
    
    withinss <- km$withinss
    size <- km$size
    cmd <- data.frame(withinss/size)
    dists <- data.frame(d = km$dists, cluster = km$cluster)
    cl_d <- c()
    cl_md <- c()
    for(i in 1:length(km$cluster)) {
      cl_d <- append(cl_d, dists[i, dists$cluster[i]])
      cl_md <-  append(cl_md, cmd[dists$cluster[i],2])
    }
    cluster_dist <- data.frame(distance = cl_d, cluster = km$cluster)
    cl_sd <- ave(cluster_dist$distance, cluster_dist$cluster,FUN = function(x) sd(x, na.rm=TRUE))
    cluster_z_score <- (cl_d - cl_md)/cl_sd
    data_outliers <- data.frame(data, cluster =  km$cluster, c_dist = cl_d, cluster_z_score,
                                cluster_outlier = ifelse(cluster_z_score > 2, T , F))
    #return table only for outliers
    return(data_outliers[data_outliers$cluster_z_score > 2,])
  }
  
  outliers_kmeans <- function(data, km) {
    
    centers <- km$centers[km$cluster, ]
    distances <- sqrt(rowSums((data - centers)^2))
    cm <- ave(distances, km$cluster, FUN = function(x) mean(x, na.rm=TRUE))
    csd <- ave(distances, km$cluster,FUN = function(x) sd(x, na.rm=TRUE))
    cluster_z_score <- (distances - cm)/csd
    data_outliers <- data.frame(data, cluster =  km$cluster, c_dist = distances, cluster_z_score,
                                cluster_outlier = ifelse(cluster_z_score > 2, T , F))
    #return table only for outliers
    return(data_outliers[data_outliers$cluster_z_score > 2,])
  }
  
  #Get optimal k using automated elbow method
  koptimize <- function(data,max_centers,type){
    scores <- data.frame()
    for (i in 1:max_centers){
      scores[i,"k"] <- i
      if (type == "kmeans"){
        
        
        kk <- kmeanspp(data,centers = i,iter.max=hyperparameters$iter_max, nstart = hyperparameters$nstart)
        scores[i,"withinss"] <-kk$tot.withinss
        
      }else{
        kk <- kproto(data, k=i, iter.max=hyperparameters$iter_max, nstart= hyperparameters$nstart, lambda = lambda, verbose = TRUE)
        scores[i,"withinss"] <-kk$tot.withinss
        
      }
    }
    for(i in 1:(max_centers-1)){
      scores[i+1,"delta"] <- scores[i,"withinss"] - scores[i+1,"withinss"]
      scores[i+1,"delta2"] <- scores[i,"delta"] - scores[i+1,"delta"]
    }
    
    for(i in 1:max_centers){
      scores[i,"strength"] <- (scores[i+1,"delta2"] - scores[i+1,"delta"]) / scores[i,"k"]
      
    }
    return(scores)
    
  }
  

  
  if(all(grepl('factor|character', sapply(data,class)))) {
    stop("Data contain only categorical variables: cannot run the unsupervised model")
  }
  
  if(any(grepl('factor|character', sapply(data,class)))) {
    # data contain at least one categorical column
    if(verbose == TRUE) {message("Data contain both categorical and numerical feautures: using k-prototype algorithm")}
    if(any(grepl('character', sapply(data,class)))) {
      # if character tranform into factor
      cols.to.factor <-sapply( data, function(col) class(col) == "character")
      data[cols.to.factor] <- lapply(data[cols.to.factor], factor)
    }
    
    if(hyperparameters$centers == 'auto') {
      if(verbose == TRUE) {message("Optimising number of clusters")}
      if(verbose == TRUE) {message("Estimating lambda for categorical variables")}
      lambda <- lambdaestimation(data, verbose = verbose)
      k_opt_scores <- koptimize(data,max_centers = hyperparameters$max_centers,type = "kproto")
      k_opt <- k_opt_scores[which.max(k_opt_scores$strength),"k"]
      if(verbose == TRUE) {message(paste0("Number of optimal clusters: ", k_opt))}
      km <- kproto(data, k=k_opt, iter.max=hyperparameters$iter_max, nstart= hyperparameters$nstart, lambda = lambda, verbose = FALSE)
      
      #outliers detection
      data_outliers <- outliers_kproto(data, km)
      
    } else {
      if(verbose == TRUE) {message("Estimating lambda for categorical variables")}
      lambda <- lambdaestimation(data, verbose = verbose)
      km <- kproto(data, k=hyperparameters$centers, iter.max=hyperparameters$iter_max, nstart= hyperparameters$nstart, lambda = lambda, verbose = FALSE)
      if(verbose == TRUE) {
        message(paste0("Number of clusters: ", max(km$cluster)))
      }
      #outliers detection
      data_outliers <- outliers_kproto(data, km)
    }
    
  } else {
    #data contain only numeric values
    if(verbose == TRUE) {message("Data contain only numeric feautures: using kmeans algorithm")}
    
    if(hyperparameters$centers == 'auto'){
      if(verbose == TRUE) {message("Optimising number of clusters")}
      
      k_opt_scores <- koptimize(data,max_centers = hyperparameters$max_centers,type = "kmeans")
      k_opt <- k_opt_scores[which.max(k_opt_scores$strength),"k"]
      if(verbose == TRUE) {message(paste0("Number of optimal clusters: ", k_opt))}
      
      km <- kmeanspp(data, centers = k_opt,iter.max=hyperparameters$iter_max, nstart = hyperparameters$nstart)
      #outliers detection
      data_outliers <- outliers_kmeans(data, km)
      
    } else {
      
      km <- kmeans(x = data, centers = hyperparameters$centers, iter.max=hyperparameters$iter_max,nstart=hyperparameters$nstart)
      #outliers detection
      data_outliers <- outliers_kmeans(data, km)
    }
  }
  if(hyperparameters$centers == 'auto'){
    elbow_plot <- ggplot(k_opt_scores, aes(x = .data$k, y = .data$withinss)) +
      geom_line() + geom_point()+
      scale_x_continuous(breaks = 1:10) +
      xlab("Clusters") + 
      ylab("Total Within Cluster Sum of Squares") +
      geom_text(data = k_opt_scores[which.max(k_opt_scores$strength),], aes(x = .data$k+0.75, y = .data$withinss*1.01, label = "Optimal clusters"))+
      geom_point(data=k_opt_scores[which.max(k_opt_scores$strength),], 
                 aes(x=.data$k,y=.data$withinss), 
                 color='magenta3',
                 size=4)
  }else{
    elbow_plot <-  NULL
  }
  if(verbose == TRUE) { message(paste0("Number of rows: ", nrow(data)))}
  out <- list(persona_model = km,
              input_data = cbind("customerid" = ids,data),
              model_hyperparameters =input_params,
              outliers_table = data_outliers,
              elbow_plot = elbow_plot,
              predicted_values = data.frame("customerid" = ids,"persona" = km$cluster)
  )
  
  class(out) <- 'unsupervised'
  
  
  
  return(out)
  
}

#' @importFrom stats predict
predict.unsupervised <- function(object,newdata,...){
  object <-  object$persona_model
  if (class(object) == "kmeans") {
    list(cluster = apply(newdata, 1, function(r) which.min(colSums((t(object$centers) - r)^2))),
         dists = t(apply(newdata, 1, function(r) colSums((t(object$centers) - r)^2))))
  }else if (class(object) == "kproto"){
    if(any(grepl('factor|character', sapply(newdata,class)))) {
      # data contain at least one categorical column
      if(any(grepl('character', sapply(newdata,class)))) {
        # if character tranform into factor
        cols.to.factor <-sapply( newdata, function(col) class(col) == "character")
        newdata[cols.to.factor] <- lapply(newdata[cols.to.factor], factor)
      }
      
    }
    predict(object, data.frame(newdata))
  }
}

#' @importFrom rlang .data
lambdaestimation <- function(x, num.method = 1, fac.method = 1, outtype = "numeric", verbose = FALSE){
  # initial error checks
  if(!is.data.frame(x)) stop("x should be a data frame!")
  if(!num.method %in% 1:2) stop("Argument 'num.method' must be either 1 or 2!")
  if(!fac.method %in% 1:2) stop("Argument 'fac.method' must be either 1 or 2!")
  if(!outtype %in% c("numeric","vector","variation")) stop("Wrong specificytion of argument 'outtype'!")
  
  # check for numeric and factor variables
  numvars <- sapply(x, is.numeric)
  anynum <- any(numvars)
  catvars <- sapply(x, is.factor)
  anyfact <- any(catvars)
  if(!anynum) cat("\n No numeric variables in x! \n\n")
  if(!anyfact) cat("\n No factor variables in x! \n\n")
  
  if(anynum & num.method == 1) vnum <- sapply(x[,numvars, drop = FALSE], var, na.rm =TRUE)
  if(anynum & num.method == 2) vnum <- sapply(x[,numvars, drop = FALSE], sd, na.rm = TRUE)
  
  if(anyfact & fac.method == 1) vcat <- sapply(x[,catvars, drop = FALSE], function(z) return(1-sum((table(z)/sum(!is.na(z)))^2)))
  if(anyfact & fac.method == 2) vcat <- sapply(x[,catvars, drop = FALSE], function(z) return(1-max(table(z)/sum(!is.na(z)))))
  if (mean(vnum) == 0){
    warning("All numerical variables have zero variance.\n
            No meaninful estimation for lambda.\n
            Rather use kmodes{klaR} instead of kprotos().")
    anynum <- FALSE
  } 
  if (mean(vcat) == 0){
    warning("All categorical variables have zero variance.\n
            No meaninful estimation for lambda!\n
            Rather use kmeans() instead of kprotos().")
    anyfact <- FALSE
  } 
  if(num.method == 1 & verbose == TRUE) cat("Numeric variances:\n")
  if(num.method == 2 & verbose == TRUE) cat("Numeric standard deviations:\n")
  print(vnum)
  if(num.method == 1 & verbose == TRUE) cat("Average numeric variance:", mean(vnum), "\n\n")
  if(num.method == 2& verbose == TRUE) cat("Average numeric standard deviation:", mean(vnum), "\n\n")
  
  if(verbose == TRUE) {
    cat(paste("Heuristic for categorical variables: (method = ",fac.method,") \n", sep = ""))
    print(vcat)
    cat("Average categorical variation:", mean(vcat), "\n\n")
  }
  
  if(anynum & anyfact) {
    if(outtype == "numeric") {lambda <- mean(vnum)/mean(vcat); if(verbose == TRUE) cat("Estimated lambda:", lambda, "\n\n")}
    if(outtype != "numeric") {
      lambda <- rep(0,ncol(x))
      names(lambda) <- names(x)
      lambda[numvars] <- vnum
      lambda[catvars] <- vcat
    }
    if(outtype == "vector") lambda <- 1/lambda
    return(lambda)
  }
  if(!(anynum & anyfact)) invisible()
}



