#' rpart.rules function
#' 
#' THIS HAS BEEN COPIED FROM THE ARCHIVED rpart.utils PACKAGE AND THIS CODE WAS WRITTEN BY THE AUTHORS OF THAT PACKAGE
#' Returns a list of strings summarizing the branch path to each node.
#'
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit<-rpart(Reliability~.,data=car.test.frame)
#' rpart.rules(fit)
rpart.rules<-function(object)
{
  frame<-object$frame
  ruleNums<-as.numeric(row.names(frame))  ##Convert the row names into a list of rule numbers
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf,"order"]<-seq_along(which(!is.leaf)) ##Number the branches to number them for matching with subrule sets
  rules<-replicate(max(ruleNums),NULL)
  rules[1]<-"NULL"
  
  ##The rule numbering convention contains the information to determine branch lineage. 
  ##Most of the potential rule numbers don't actually exist, but this will result in the creation of a NULL rule.
  for (i in as.numeric(row.names(frame))[-1])
  {
    if(i%%2==0)
    {
      rules[i]<-paste(rules[i/2],paste('L',frame[as.character(i/2),"order"],sep=''),sep=',')
    }
    else
    {
        rules[i]<-paste(rules[(i-1)/2],paste('R',frame[as.character((i-1)/2),"order"],sep=''),sep=',')
    }
  }
  rules<-lapply(rules,function (x) gsub("NULL,",'',x))
  return(rules)
}

#' rpart.lists function
#' 
#' THIS HAS BEEN COPIED FROM THE ARCHIVED rpart.utils PACKAGE AND THIS CODE WAS WRITTEN BY THE AUTHORS OF THAT PACKAGE
#' Creates lists of variable values (factor levels) associated with each rule in an \pkg{rpart} object.  
#'
#'
#' @param object an rpart object
#' @return a list of lists
#' @export
#' @examples
#' library(rpart)
#' fit<-rpart(Reliability~.,data=car.test.frame)
#' rpart.lists(fit)
rpart.lists <- function(object)
{
  
  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L) return("root")            # special case of no splits
  
  
  ##This section  borrowed from the rpart source to identify the appropriate locations from the splits table.
  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow] # the variable names for the primary splits
  
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
  irow <- index[c(whichrow, FALSE)] # we only care about the primary split
  ncat <- object$splits[irow, 2L]
  ##
  
  lsplit <- rsplit <- list()  
  
  if (any(ncat < 2L)) 
  {               # any continuous vars ?
    
    jrow <- irow[ncat < 2L]
    cutpoint <- object$splits[jrow, 4L]
    temp1 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
    lsplit[ncat<2L] <- cutpoint
    #lsplit[ncat<2L] <- lapply(lsplit[ncat<2L],function (x) structure(x, 'numeric'=TRUE))
    
    rsplit[ncat<2L] <- cutpoint
    #rsplit[ncat<2L] <- lapply(rsplit[ncat<2L],function (x) structure(x, 'numeric'=TRUE))
    
  }
  
  if (any(ncat > 1L)) 
  {               # any categorical variables ?
    xlevels <- attr(object, "xlevels")
    ##
    ## jrow will be the row numbers of factors within lsplit and rsplit
    ## crow the row number in "csplit"
    ## and cindex the index on the "xlevels" list
    ##
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L] #row number in csplit
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

    
    lsplit[jrow]<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==1L])
    rsplit[jrow]<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==3L])

  }


  lsplit<-lapply(seq_along(lsplit), function (i) structure(lsplit[[i]], "compare"=ifelse(ncat[i]<2L,ifelse(ncat[i]<0,"<",">="),"=")))
  rsplit<-lapply(seq_along(lsplit), function (i) structure(rsplit[[i]], "compare"=ifelse(ncat[i]<2L,ifelse(ncat[i]<0,">=","<"),"=")))
  
  
  names(lsplit)<-vnames
  names(rsplit)<-vnames
  
  results<-list("L"=lsplit,"R"=rsplit)  

  return(results)
}

#' rpart.rules.table function
#' 
#' THIS HAS BEEN COPIED FROM THE ARCHIVED rpart.utils PACKAGE AND THIS CODE WAS WRITTEN BY THE AUTHORS OF THAT PACKAGE
#' Returns an unpivoted table of branch paths (subrules) associated with each node.
#'
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit<-rpart(Reliability~.,data=car.test.frame)
#' rpart.rules.table(fit)
rpart.rules.table<-function(object)
{
  rules<-rpart.rules(object)
  ff<-object$frame
  ff$rules<-unlist(rules[as.numeric(row.names(ff))])
  ruleList<-lapply(row.names(ff),function (name) setNames(data.frame(name,
                                                                     (strsplit(ff[name,'rules'],split=',')),
                                                                     ff[name,'var']=="<leaf>"
                                                                     ),
                                                          c("Rule","Subrule","Leaf")))
  combinedRules<-Reduce(rbind,ruleList)
  
  return(combinedRules)
  
}

#' rpart.subrules.table function
#' 
#' THIS HAS BEEN COPIED FROM THE ARCHIVED rpart.utils PACKAGE AND THIS CODE WAS WRITTEN BY THE AUTHORS OF THAT PACKAGE                   
#' Returns an unpivoted table of variable values (factor levels) associated with each branch.
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit<-rpart(Reliability~.,data=car.test.frame)
#' rpart.subrules.table(fit)
rpart.subrules.table<-function(object)  
{
  lists<-rpart.lists(object)
  leftCompares<-lapply(lists$L,function (x) attr(x,"compare"))
  rightCompares<-lapply(lists$R,function (x) attr(x,"compare"))
  leftRules<-lapply(seq_along(lists$L),function (i) setNames(data.frame(paste('L',i,sep=''),names(lists$L)[i],as.character(unlist(lists$L[i],use.names=FALSE)),NA,NA),c("Subrule","Variable","Value","Less","Greater")))
  rightRules<-lapply(seq_along(lists$R),function (i) setNames(data.frame(paste('R',i,sep=''),names(lists$R)[i],as.character(unlist(lists$R[i]),use.names=FALSE),NA,NA),c("Subrule","Variable","Value","Less","Greater")))
  
  reassign.columns<-function(object,compare)
  {
    if(grepl("<",compare))
      object$Less<-object$Value
    if(grepl(">",compare))
      object$Greater<-object$Value
    if(!grepl("=",compare))
      object$Value=NA
    return(object)
  }
  
  leftTable<-Reduce(rbind,Map(reassign.columns, leftRules, leftCompares))
  rightTable<-Reduce(rbind,Map(reassign.columns, rightRules, rightCompares))
  
  
  return(rbind(leftTable,rightTable))
}                   

#' Tree Segment Function
#'
#' Runs decision tree optimisation on the data to segment ids.
#' @param data data.frame, the data to segment
#' @param hyperparameters list, list of hyperparameters to pass. They include
#' segmentation_variables: a vector or list with variable names that will be used as segmentation variables; 
#' dependent_variable: a string with the name of the dependent variable that is used in the clustering;
#' min_segmentation_fraction: integer, the minimum segment size as a proportion of the total data set;
#' number_of_segments: integer, number of leaves you want the decision tree to have.
#' @importFrom dplyr mutate_all left_join select %>%
#' @importFrom treeClust rpart.predict.leaves 
#' @importFrom rpart.plot rpart.plot
#' @importFrom rlang .data
#' @param verbose logical whether information about the segmentation procedure should be given.
#' @author Stuart Davie, \email{stuart.davie@@peak.ai}
#' @return List of 4 objects. The rpart object defining the model, a data frame providing high-level segment attributes,
#' a lookup table (data frame) with the id and predicted segment number, and a list of the hyperparameters used.
#' @export
tree_segment <- function(data, hyperparameters, verbose = TRUE){
  
  if(is.null(hyperparameters$segmentation_variables)){
    segmentation_variables <- colnames(data)[colnames(data)!= hyperparameters$dependent_variable & colnames(data)!='id']
  }else{
    segmentation_variables <- hyperparameters$segmentation_variables
  }
  inputs_params <- list(segmentation_variables=segmentation_variables,
                        dependent_variable=hyperparameters$dependent_variable,
                        min_segmentation_fraction=hyperparameters$min_segmentation_fraction,
                        number_of_segments=hyperparameters$number_of_segments)
  
  int_colnames <- names(data)[unname(sapply(data, typeof)) == 'integer']
  
  types <- unname(sapply(data, typeof))
  
  if(sum(as.character(types) == 'logical')>0) {
    
    indices <- which(types == 'logical')
    
    data[,indices] <- data[,indices] %>%
      mutate_all(as.character)
    
  }
  
  first_tree  <- decision_tree_user_defined_leafs.make(df=data,
                                                       segmentation_variables=segmentation_variables,
                                                       dependent_variable=hyperparameters$dependent_variable,
                                                       min_segmentation_fraction=hyperparameters$min_segmentation_fraction,
                                                       number_of_leafs=hyperparameters$number_of_segments)
  
  if(nrow(first_tree$frame)==1){
    warning('Only 1 segment. Change parameters or inputs!')
  } else {
    segment_table <- tree_table.make(first_tree, int_colnames)
    segment_tree  <- segment_tree.make(first_tree)
    segment_tree_df <- segment_tree$df
    segment_tree <- segment_tree$tree
    segment_predicted <- data.frame(id = data$id, orig_row=as.numeric(rpart.predict.leaves(segment_tree, data, type = "where")))
    segment_predicted <- left_join(segment_predicted,segment_tree_df %>% select(.data$orig_row,.data$segment), by = "orig_row") %>% select(.data$id, .data$segment)
    
    if(hyperparameters$print_plot&(hyperparameters$number_of_segments<hyperparameters$print_safety_check)){rpart.plot(first_tree)}
    
    return(
      list(segment_model = segment_tree,
           segment_table = segment_table,
           segment_predicted = segment_predicted,
           model_inputs = inputs_params)
    )
  }
}

#' @importFrom dplyr filter %>%
#' @importFrom rpart rpart.control rpart prune
#' @importFrom tibble rownames_to_column 
#' @importFrom rlang .data
#' @author Stuart Davie, \email{stuart.davie@@peak.ai}
decision_tree_user_defined_leafs.make <- function(df,segmentation_variables,dependent_variable='response',min_segmentation_fraction=0.05,number_of_leafs=6){
  
  minbucket = floor(nrow(df)*min_segmentation_fraction)
  minsplit=2*minbucket
  f <- paste(dependent_variable, paste(segmentation_variables,collapse = ' + '),sep=' ~ ')
  control <- rpart.control(cp=-1,minbucket = minbucket,minsplit = minsplit)
  tree <- rpart(f,data=df,method='anova',control = control)
  
  if(nrow(tree$frame %>% filter(.data$var=='<leaf>')) < number_of_leafs) {
    warning('Output number of segments is less than than the requested amount. Reduce the minimum segmentation fraction, increase the number of segmentation variables, get more data etc.')
    pruned_tree <- tree
  } else{
    cp_adjusted_tree <- tree
    cp_adjusted_tree$frame$complexity[cp_adjusted_tree$frame$var!='<leaf>'] <-
      cp_adjusted_tree$frame$complexity[cp_adjusted_tree$frame$var!='<leaf>']-
      cp_adjusted_tree$frame$complexity[cp_adjusted_tree$frame$var!='<leaf>']*0.001*
      (cp_adjusted_tree$frame %>% rownames_to_column() %>% filter(.data$var!='<leaf>') %>% mutate(rowname=as.numeric(.data$rowname)))$rowname
    
    min_cp <- 0;            max_cp <- 1
    number_check <- FALSE;  stopcount <- 0
    while (number_check == FALSE){
      cur_cp <- (min_cp+max_cp)/2
      pruned_tree <- prune(cp_adjusted_tree,cur_cp)
      leafs <- nrow(pruned_tree$frame %>% filter(.data$var=='<leaf>'))
      if(leafs==number_of_leafs){
        number_check <- TRUE
      }else{
        stopcount <- stopcount+1
        if(leafs>number_of_leafs){
          min_cp <- cur_cp
        }else{
          max_cp <- cur_cp
        }
      }
      if(stopcount==1000){
        number_check <- TRUE
        warning(paste('Pruning was unable to converge. Number of leafs likely to not match what was requested. min_cp=',min_cp,'  max_cp=',max_cp))
      }
    }
    return(pruned_tree)
  }
}

#' @importFrom tibble rownames_to_column tibble
#' @importFrom dplyr arrange bind_cols filter transmute row_number select group_by summarise mutate n ungroup full_join rename %>% everything
#' @importFrom stringr str_split
#' @importFrom rlang .data
#' @author Stuart Davie, \email{stuart.davie@@peak.ai}
tree_table.make <- function(tree, integer_columns){
  
  df1 <- rownames_to_column(tree$frame) %>% arrange(as.numeric(.data$rowname)) %>%
    bind_cols(tibble(rules=unlist(rpart.rules(tree))) %>% filter(nchar(.data$rules)>0)) %>%
    filter(.data$var=='<leaf>') %>%
    transmute(segment=row_number(),n,.data$yval,.data$rules)
  var_names <- tree$frame %>% filter(.data$var!='<leaf>') %>% select(.data$var) %>% unique()
  df2 <- df1 %>% bind_cols(as.data.frame(matrix(data=NA,nrow = nrow(df1),ncol = nrow(var_names),dimnames = list(c(),var_names$var))))
  
  if(nrow(df2)>1){
    subrule_table <- rpart.subrules.table(tree)
    for (j in 1:nrow(df2)){
      rules <- str_split(df2$rules[j],',')[[1]]
      rule_vals <- subrule_table[subrule_table$Subrule %in% rules,]
      if(nrow(rule_vals %>% filter(!is.na(.data$Less)|!is.na(.data$Greater)))==0){
        less_n_greater <- rule_vals %>% select(.data$Variable,.data$Less,.data$Greater) %>% unique()
      }else{
        less_n_greater <- suppressWarnings(rule_vals %>% group_by(.data$Variable) %>% summarise(Less=min(as.numeric(as.character(.data$Less)),na.rm=TRUE),Greater=max(as.numeric(as.character(.data$Greater)),na.rm=TRUE)))
        less_n_greater <- less_n_greater %>% mutate(Less=ifelse(is.infinite(.data$Less),NA,.data$Less),Greater=ifelse(is.infinite(.data$Greater),NA,.data$Greater))
      }
      
      categories <- suppressWarnings(rule_vals %>% 
                                       group_by(.data$Variable,.data$Value) %>% 
                                       filter(is.na(.data$Less),is.na(.data$Greater)) %>% 
                                       summarise(count=n()) %>% 
                                       ungroup() %>%
                                       group_by(.data$Variable) %>% 
                                       filter(.data$count==max(.data$count),!is.na(.data$Value)) %>%
                                       summarise(Value=paste(.data$Value,collapse=', ')))
      together <- full_join(less_n_greater, categories %>% filter(!is.na(.data$Variable)), by='Variable') %>% 
        group_by(.data$Variable) %>%
        mutate(out=paste(ifelse(!is.na(.data$Value),.data$Value,''),
                         ifelse(!is.na(.data$Less)&is.na(.data$Greater),paste('<',.data$Less),''),
                         ifelse(!is.na(.data$Greater)&is.na(.data$Less),paste('>',.data$Greater),''),
                         ifelse(!is.na(.data$Greater)&!is.na(.data$Less),paste(.data$Greater,'-',.data$Less),''),
                         collapse=' ')) %>% select(.data$Variable,.data$out) %>% ungroup()
      for (i in 1:nrow(together)){
        df2[,(together$Variable[i]) %>% as.character()][j] <- trimws(together$out[i],which = 'both')
      }
    }
    
    df3 <- df2 %>% mutate(percentage=n/sum(n)*100) %>% select(.data$segment,.data$yval,.data$percentage,everything()) %>%
      rename(mean_value=.data$yval)%>% select(-.data$rules)
    df3[,5:ncol(df3)][is.na( df3[,5:ncol(df3)])] <- 'All'
    
    # Ensures that the conditions for integer columns in the table remain formatted as integers.
    # Without this step, a condition for an integer column could be, e.g., > 1.5.
    # With this step, this condition gets changed to >= 2.
    
    # Select the columns in the segment table that are integers in the raw DF
    
    if (sum(names(df3) %in% integer_columns) == 1) {
      df_to_change <- data.frame(df3[, names(df3) %in% integer_columns], stringsAsFactors = FALSE)
      names(df_to_change)[1] <- names(df3)[names(df3) %in% integer_columns]
    } else if (sum(names(df3) %in% integer_columns) > 1) {
      df_to_change <- df3[, names(df3) %in% integer_columns]
    }
    else if (sum(names(df3) %in% integer_columns) == 0) {
      df_to_change <- df3[, names(df3) %in% integer_columns]
    }
    
    if(length(df_to_change) != 0){
      # Loops through all occurrences of integer conditions and ensures they are floored or ceilinged appropriately
      for (i in 1:length(names(df_to_change))) {
        for (k in 1:nrow(df_to_change)) {
          if (df_to_change[k, i] != 'All') {
            if (grepl('>', df_to_change[k, i])) {
              
              value <- as.numeric(str_split(df_to_change[k, i], '> ')[[1]][2])
              new_value <- ceiling(value)
              
              df_to_change[k, i] <- paste0('>= ', new_value)
              
            } else if (grepl('<', df_to_change[k, i])) {
              value <- as.numeric(str_split(df_to_change[k, i], '< ')[[1]][2])
              new_value <- floor(value)
              
              df_to_change[k, i] <- paste0('<= ', new_value)
            }
          }
        }
      }
      # Replaces the decimal conditions with the new integer formatted conditions instead
      df3[, names(df3) %in% integer_columns] <- df_to_change
    }
    
    return(df3)
  } else {
    warning("Only one node! This isn't a tree - it's a stump!")
  }
}

#' @importFrom tibble rownames_to_column tibble
#' @importFrom dplyr mutate row_number arrange bind_cols filter transmute %>%
#' @importFrom rlang .data
#' @author Stuart Davie, \email{stuart.davie@@peak.ai}
segment_tree.make <- function(tree){
  
  df1 <- rownames_to_column(tree$frame) %>% mutate(orig_row=row_number()) %>% arrange(as.numeric(.data$rowname)) %>%
    bind_cols(tibble(rules=unlist(rpart.rules(tree))) %>% filter(nchar(.data$rules)>0)) %>%
    filter(.data$var=='<leaf>') %>%
    transmute(segment=row_number(),n,.data$yval,.data$rules,.data$orig_row) %>% arrange(.data$orig_row)
  tree$frame$yval[tree$frame$var=='<leaf>'] <- df1$segment
  return(list(tree = tree,
              df = df1))
}

#' @importFrom stringr str_locate_all
dynamic_binning <- 
  function(x, maxchar_length = 45){
    if (nchar(x) > maxchar_length){
      comma_index <- str_locate_all(x,',')[[1]][,1]
      nchar_diff <- comma_index - maxchar_length
      #nchar_diff <- nchar_diff[nchar_diff<0]
      maxcomma_index <- comma_index[which.min(abs(nchar_diff))]
      x <- paste0(substr(x, 1, maxcomma_index), ' Other')
    }
    return(x)
  }

#' Plot a prettified rpart model
#'
#' Plot an rpart model and prettifies it. Wrap around the rpart.plot::prp function
#' @param model an rpart model object
#' @param main main title
#' @param sub fixing captions in line 
#' @param caption character, caption to use in the plot
#' @param palettes list, list of colours to use in the plot
#' @param type type of plot. Default is 2. Possible values are: 
#' 0 Default. Draw a split label at each split and a node label at each leaf.
#' 1 Label all nodes, not just leaves.
#' 2 Like 1 but draw the split labels below the node labels.
#' 3 Draw separate split labels for the left and right directions.
#' 4 Like 3 but label all nodes, not just leaves. 
#' 5 Show the split variable name in the interior nodes. 
#' @param fontfamily Names of the font family to use for the text in the plots.
#' @param ... Additional arguments.
#' @return 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rpart.plot prp
#' @return An rpart.plot object. This plot object can be plotted using the rpart::prp function.
#' @export

rpart.plot_pretty <- function(model,main="",sub,caption,palettes,type=2,fontfamily='sans',...){
  
  if (!inherits(model, "rpart"))
    
    stop("The model object must be an rpart object. ",
         "Instead we found: ", paste(class(model), collapse=", "), ".")
  
  # For new version of rpart.plot (20180710 v3.0.0).
  
  roundint <- ! is.null(model$model)
  
  # Migrate to replacing sub with caption in line with ggplot.
  
  if (missing(sub) & missing(caption))
  {
    sub <- paste("Rattle",
                 format(Sys.time(), "%Y-%b-%d %H:%M:%S"), 
                 Sys.info()["user"])
  } else
  {
    if (missing(sub)) sub <- caption
  } 
  
  num.classes <- length(attr(model, "ylevels"))
  
  # Generate a colour palette, with a range of 5 (palsize) colours for
  # each of the 6 (numpals) palettes. The palette is collapsed into
  # one list. We index it according to the class. Keep to the lighter
  # end of the palette to ensure printing is okay otherwise the black
  # text is hard to read.
  
  default.palettes <- c("Greens", "Blues", "Oranges", "Purples", "Reds", "Greys")
  if (missing(palettes))
    palettes <- default.palettes
  missed <- setdiff(1:6, seq(length(palettes)))
  palettes <- c(palettes, default.palettes[missed])
  
  numpals <- 6
  palsize <- 5
  pals <- c(brewer.pal(9, palettes[1])[1:5],
            brewer.pal(9, palettes[2])[1:5],
            brewer.pal(9, palettes[3])[1:5],
            brewer.pal(9, palettes[4])[1:5],
            brewer.pal(9, palettes[5])[1:5],
            brewer.pal(9, palettes[6])[1:5])
  
  pals <- brewer.pal(9, palettes[4])[2:7]
  
  # Extract the scores/percentages for each of the nodes for the
  # majority decision.  The decisions are in column 1 of yval2 and the
  # percentages are in the final num.classes columns.
  
  # 121106 Need to handle regression as pointed out by Yana
  # Kane-Esrig, 26 October 2012.
  
  if (model$method == "class")
  {
    yval2per <- -(1:num.classes)-1
    per <- apply(model$frame$yval2[,yval2per], 1, function(x) x[1+x[1]])
  }
  else
  {
    # 130329 This is the deviance relative the the total deviance measured at
    # the root node. We use this to colour the strength of the node -
    # so more intense colour means less relative deviance.
    
    #per <- 1 - (model$frame$dev/model$frame$dev[1])
    
    # 130329 Perhaps instead we want to use the yval as the intensity
    # of the predicted value. Currently not handling negative values.
    
    per <- model$frame$yval/max(model$frame$yval)
    
  }
  
  # The conversion of a tree in CORElearn to an rpart tree results in these
  # being character, so ensure we have numerics.
  
  per <- as.numeric(per)
  
  # Calculate an index into the combined colour sequence. Once we go
  # above numpals * palsize (30) start over.
  
  if (model$method == "class")
    col.index <- ((palsize*(model$frame$yval-1) +
                     trunc(pmin(1 + (per * palsize), palsize))) %%
                    (numpals * palsize))
  else
    col.index <- round(per * (palsize-1)) + 1
  
  # Ensure the index is positive. Thanks to John Vorwald, 8 Dec
  # 2014. The bug can arise when model$frame$yval are all
  # negative. The error is:
  #
  #  fancyRpartPlot(rtreeFit,main=paste('RPART:',cName))
  #  Error in pals[col.index] : only 0's may be mixed with negative subscripts
  
  col.index <- abs(col.index)
  
  # Determine the amount of extra information added to the nodes.
  
  if (model$method == "class")
    extra <- 104
  else
    extra <- 101
  
  # Generate the plot and title.
  
  split.fun <- function(x, labs, digits, varlen, faclen)
  {
    # replace commas with spaces (needed for strwrap)
    for(i in 1:length(labs)) {
      # split labs[i] into multiple lines
      labs[i] <- dynamic_binning(labs[i])
      labs[i] <- gsub(",", ", ", labs[i])
      labs[i] <- paste(strwrap(labs[i], width = 10), collapse = "\n")
    }
    labs
  }
  node.fun <- function(x, labs, digits, varlen) {    
    avg  <- sprintf("%0.3f", x$frame$yval)
    pct   <- sprintf("%1.1f%%",100*x$frame$wt/x$frame$wt[1]) 
    rows <- format(x$frame$n, big.mark=",")
    paste0(avg, "\n", " n=", rows,"   ", pct)
  }
  
  prp(model, type=type, extra=extra,
      box.col=pals[col.index],
      nn=TRUE,
      varlen=0, faclen=0,
      # shadow.col="grey",
      fallen.leaves=TRUE,
      branch.lty=3,
      roundint=roundint,
      split.fun=split.fun,node.fun = node.fun,digits=-2,
      split.family = fontfamily,
      split.font = 1,
      split.yshift = -1,
      shadow.col = 0,
      ...)
}


#' Tree Segment Prettify Function
#'
#' Returns a prettier version of the decision tree.
#' @param tree The decision tree model to prettify
#' @param char_length integer, the character limit before truncating categories and putting them into an "other" group
#' @param print_plot logical, indicates whether to print the generated plot or not
#' @importFrom dplyr select %>%
#' @importFrom stringr str_remove_all str_remove str_split
#' @return A formatted and "prettified" rpart.plot object. This plot object can be plotted using the rpart::prp function.
#' @export
tree_segment_prettify <- function(tree, char_length = 20, print_plot = FALSE){
  
  if(print_plot){rpart.plot_pretty(tree$segment_model)}
  
  features_used <- names(tree$segment_table)
  features_used <- features_used[!features_used %in% c("segment","mean_value","percentage","n")]
  split_data <- tree$segment_table %>% select(features_used)
  
  character_check <- function(x){
    words <- unique(x)
    words <- str_remove_all(str_remove_all(str_remove_all(string = words,pattern = 'c\\('),'\\\\'),'\\"')
    words[length(words)] <- str_remove(words[length(words)],'\\)$')
    character_counts <- nchar(words)
    return_words <- words[which(character_counts > char_length)]
    return(return_words)
  }
  exceeding_words <- suppressWarnings(lapply(str_split(split_data,pattern = ', '),character_check))
  column_names <- names(split_data)
  for(col_number in 1:ncol(split_data)){
    
    exceeding_word <- exceeding_words[[col_number]]
    
    if(length(exceeding_word)>0){
      
      message(paste0("Column (", column_names[col_number], ") has entries with too many characters: ", paste0(exceeding_word, collapse = ', '),'\n\rChange this for easier interpretation'))
      
    }
    split_data[,col_number] <- sapply(split_data[,col_number],dynamic_binning)
  }
  
  tree$segment_table[,features_used] <- split_data
  
  return(tree)
}

#' Abstraction layer function
#'
#' Organises the model outputs, predictions and settings in a general structure
#' @param model The model to organise
#' @param inputdata The data used to train the model
#' @return A structure with the class name "tree_model" which contains a list of all the relevant model data, 
#' including the rpart model object, hyper-parameters, segment table, labelled customer lookup table, 
#' and the input data used to train the model.
#' @export
tree_abstract <- function(model){
  #TODO: add performance statistics
  #tree_performance()
  structure(
    list(segment_model = model$segment_model,
         model_hyperparameters = model$model_inputs,
         segment_table = model$segment_table,
         predicted_values = model$segment_predicted),
    
    class = "tree_model")
}
