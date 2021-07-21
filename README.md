# CITRUS - Customer Intelligence Tool for Rapid Understandable Segmentation

## Overview  

Customer segmentation is the process of dividing customers into groups based on common characteristics so companies can market to each group effectively and appropriately. **CITRUS** is a package developed by Peak to provide easy and understandable segmentation utilities.

## Contributing

If you are interested in contributing to the package, please follow guidelines found [here](https://github.com/PeakBI/citrus/blob/main/.github/CONTRIBUTING.md) 

## Installation

```r
# install.packages("devtools")
devtools::install_github("peak-ai/citrus")
```

## Expected Data Model

In its current state, we are assuming that the input data is a transactions table with 4 required columns (transactionid, customerid, orderdate, transactionvalue). The preprocess function requires that these fields (or equivalent fields) are named in that manner. The function will raise a warning if these column names are not found.

## Usage

Using the segmentation functions could be as simple as calling a one-liner:

```r
output <- segment(citrus::transactional_data, 
                  modeltype = 'tree',
                  steps = c('preprocess','model'),
                  prettify = TRUE,
                  print_plot = TRUE)
```

The above code uses default hyperparameters. There are two types of hyperparameters in this package, global ones and method specific ones. Global ones are independent of the method chosen and the method specific ones are related to the approach taken (eg. size of tree).

### Hyperparameters

Hyperparameters can be passed into the functions at any time, most of the modules of the CITRUS pipeline has `hyperparameters` as a function input. The hyperparameters should be passed as a list as follows

```r
hyperparameters = list(dependent_variable = 'response',
                       min_segmentation_fraction = 0.05,
                       number_of_personas = 6,
                       print_safety_check=20)
```
or for unsupervised

```r
hyperparameters = list(centers = 'auto',
                                 iter_max = 50,
                                 nstart = 5,
                                 max_centers = 5, 
                                 segmentation_variables = NULL,
                                 standardize = TRUE)
```


<center>

| Type   | Method       | Hyperparameter Name    | Description                                                   |
|--------|--------------|------------------------|---------------------------------------------------------------|
| Global |              | segmentation_variables | `array`, names of all variables to segment on                   |
| Global |              | saveoutput             | `boolean`, saves the output to local directory if TRUE          |
| Method | tree         | dependent_variable     | `character`, name of the variable to be used as dependent variable |
| Method | tree         | min_segmentation_fraction     | `numeric`, the maximum size of the smallest possible segment (0.05 is 5% of the number of customers passed) |
| Method | tree         | number_of_personas     | `numeric`, the number of segments to be produced |
| Method | unsupervised | centers                | `numeric` or `string`, either the number of clusters, or a set of initial (distinct) cluster centres, or 'auto'. When 'auto' is chosen, the number of clusters is optimised |
| Method | unsupervised | iter_max               | `numeric`, the maximum number of iterations allowed                      |
| Method | unsupervised | nstart                 | `numeric`, how many random sets of cluster centers should be tried       |
| Method | unsupervised | max_centers            | `numeric`, maximum number of possible clusters when set to auto (the number of clusters is being optimised) |

</center>

Modularity was a primary design consideration when building **CITRUS**. This allows you to use certain functions as and when you need them. Below is an overview of some of the different ways the **CITRUS** modules can be used:

### Preprocessing
#### Preprocessing Example 1 
MVP preprocessing. If nothing other than numeric_operation_list = NA, the preprocessing will default to RFM preprocessing
Can be used in e.g. unsupervised learning
```r
formatted <- preprocess(citrus::transactional_data, numeric_operation_list = NA)
```

#### Preprocessing Example 2
Aggregating the numeric columns using the 'min' and standard deviation 'sd'. 
Uses the most common category for each user in the column 'country'.
The target column is the mean of the transactionvalue column.

```r
formatted <- preprocess(citrus::transactional_data,
                        categories = c('country'), 
                        numeric_operation_list = c('min', 'sd'),
                        target = 'desc_chars', 
                        target_agg = 'mean')
```

### Data Validation 
#### Data Validation Example 1
DF invalid because the 'customerid' column is missing

```r
invalid_df <- formatted %>% 
  select(-customerid)

validate(invalid_df, supervised = TRUE)
```

#### Data Validation Example 2 
DF invalid because the 'response' column is missing and the customerid column is not unique

```r
invalid_df <- formatted %>% 
  rbind(formatted[rep(1, 5), ]) %>%
  select(-response)

validate(invalid_df, supervised = TRUE)
```

#### Data Validation Example 3 
DF invalid because only the customerid, and response column exists, there are no feature columns to predict over.

```r
invalid_df <- formatted %>% 
  select(customerid, response)

validate(invalid_df, supervised = TRUE)
```

#### Data Validation Example 4
Successful validation

```r
validate(formatted, supervised = TRUE)
```
### Segment
#### Segment Example 1
Runs the default decision tree optimisation function to segment the customers.
Skips the 'preprocess' step and goes straight from the 'model' step and onwards.
Instructed to prettify the plot and display it

```r
output <- segment(citrus::preprocessed_data, 
                  modeltype = 'tree',
                  steps = c('model'),
                  prettify = T,
                  print_plot = TRUE)
```

#### Segment Example 2
Uses a user defined function to classify the customers
Skips the 'preprocess' step and goes straight from the 'model' step and onwards.
No hyper parameters passed

```r
my_custom_function <- function(df) {
  
  final_df <- df %>%
    mutate(first_letter = substring(top_country, 1, 1)) %>%
    mutate(persona = ifelse(match(tolower(first_letter), tolower(LETTERS)) <= 8, 'Segment A',
                                      ifelse(match(tolower(first_letter), tolower(LETTERS)) <= 17, 'Segment B', 'Segment C'))) %>%
    arrange(desc(customerid))
  
  #output <- list('predicted_values' = final_df[,c('persona')])
  output <- list('predicted_values' = final_df[,c('customerid', 'persona')])
  
  return(output)
}
output <- segment(citrus::preprocessed_data, 
                  modeltype = 'tree',
                  FUN = my_custom_function,
                  FUN_preprocess = NULL,
                  steps = c('model'))                  
```


#### Segment Example 3 
MVP segmentation. Uses both the default preprocess step and the default model step

```r
output <- segment(citrus::transactional_data, prettify = TRUE, print_plot = TRUE)
```

#### Segment Example 4 
MVP segmentation for unsupervised learning
Uses both the default preprocess step and the default model step

```r
output <- segment(citrus::transactional_data %>% select(-desc_chars), 
                  modeltype = 'unsupervised', 
                  prettify = TRUE)
```

### Model Abstraction
#### Model Abstraction Example 1 
The model abstraction layer takes the output of the model segment layer and extracts the relevant information and converts it into a model class ready for the model management layer.

```r
hyperparameters <- list(dependent_variable = 'response',
                       min_segmentation_fraction = 0.05,
                       number_of_personas = 6,
                       print_plot = FALSE,
                       print_safety_check=20)

model <- tree_segment(citrus::preprocessed_data, hyperparameters)
model <- tree_segment_prettify(model,print_plot = T)
model <- tree_abstract(model, citrus::preprocessed_data)
```

### Model Management
#### Model Management Example 1 
The model management layer is used to store the model outputs and relevant metadata, this is defaulted to not save. To get this layer to work, in the hyperparameters at the start, the saveoutput hyperparameter can be changed to save the relevant model outputs.

```r
hyperparameters <- list(dependent_variable = 'response',
                       min_segmentation_fraction = 0.05,
                       number_of_personas = 6,
                       print_plot = FALSE,
                       print_safety_check=20,
                       saveoutput = T)

model <- tree_segment(citrus::preprocessed_data, hyperparameters)
model <- tree_segment_prettify(model,print_plot = T)
model <- tree_abstract(model, citrus::preprocessed_data)
model_management(model,hyperparameters)
```

### Output Layer
#### Output Layer Example 1 
The output layer takes the output of the model segment and abstraction layer and produces the output table.

```r
hyperparameters <- list(dependent_variable = 'response',
                       min_segmentation_fraction = 0.05,
                       number_of_personas = 6,
                       print_plot = FALSE,
                       print_safety_check=20)

model <- tree_segment(citrus::preprocessed_data, hyperparameters)
model <- tree_segment_prettify(model,print_plot = T)
model <- tree_abstract(model, citrus::preprocessed_data)
model_management(model,hyperparameters)
output <- output_table(citrus::preprocessed_data,model)
```

### Pair-plot vizualisation

```r
formatted <- preprocess(citrus::transactional_data,
                        categories = c('country'), 
                        numeric_operation_list = c('min', 'sd'),
                        target = 'desc_chars', 
                        target_agg = 'mean')

output <- segment(formatted, 
                  modeltype = 'tree',
                  steps = c('model'),
                  prettify = TRUE,
                  print_plot = TRUE)
                  
plot <- citrus_pair_plot(data = formatted, segments = output$segments, vars = c("frequency","monetary"))
```

### Full Workflow
**CITRUS** can be run simply with the `segment` function or can be more advanced, using each module by line as below.

```r
hyperparameters <- list(dependent_variable = 'response',
                       min_segmentation_fraction = 0.05,
                       number_of_personas = 6,
                       print_plot = FALSE,
                       print_safety_check=20)

formatted <- preprocess(citrus::transactional_data,
                       categories = c('country'), 
                       numeric_operation_list = c('min', 'sd'),
                       target = 'desc_chars', 
                       target_agg = 'mean')
validate(formatted, supervised = TRUE)
model <- tree_segment(formatted, hyperparameters)
model <- tree_segment_prettify(model,print_plot = T)
model <- tree_abstract(model, citrus::preprocessed_data)
model_management(model,hyperparameters)
output <- output_table(citrus::preprocessed_data,model)
```
