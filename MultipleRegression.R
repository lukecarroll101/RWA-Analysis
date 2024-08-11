###############################
# Function for producing rwa results with confidence intervals
###############################
library(boot)
library(purrr)
suppressPackageStartupMessages(library(tidyverse))
library(rwa)
library(broom)
library(knitr)

#########################
#function to get relative weights in presence of a random variable for tests of significance
rwa.rand <- function (df, outcome, predictors, applysigns = FALSE, plot = TRUE) 
{
  thedata <- df %>% dplyr::select(all_of(outcome), all_of(predictors)) %>% 
    tidyr::drop_na(all_of(outcome)) %>% 
    ##Add Random variable to data set +pipes----
  dplyr::mutate(rand = rnorm(n(),0,1))
  ###
  numVar <- NCOL(thedata)
  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>% 
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>% 
    remove_all_na_cols() %>% tidyr::drop_na()
  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]
  Variables <- cor_matrix %>% names() %>% .[. != outcome]
  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY
  rsquare <- sum(beta^2)
  ##reformat weights to be each value minus the last one produced by the random variable ---
  RawWgt <- as.vector(lambdasq %*% beta^2) #turn into vector for later
  RawWgt <- RawWgt - tail(RawWgt, n=1) #calculate diff
  head(RawWgt, -1) #drop last value
}
############################

###########################
#Function to calculate difference between relative weights of two predictors
rwa.comp <- function (df, outcome, predictors, focal) 
{ 
  #Create list of comparisons
  comparisons <<- paste(predictors[-(grep(focal, predictors))], focal, sep = " - ") 
  
  thedata <- df %>% dplyr::select(all_of(outcome), all_of(predictors)) %>% 
    tidyr::drop_na(all_of(outcome)) %>% 
    ##puts focal variable in last column ----
  relocate(all_of(focal), .after = last_col())
  ###
  numVar <- NCOL(thedata)
  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>% 
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>% 
    remove_all_na_cols() %>% tidyr::drop_na()
  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]
  Variables <- cor_matrix %>% names() %>% .[. != outcome]
  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY
  rsquare <- sum(beta^2)
  ##reformat weights to be each value minus the last one produced by the focal variable ---
  RawWgt <- as.vector(lambdasq %*% beta^2) #turn into vector for later
  RawWgt <- RawWgt - tail(RawWgt, n=1) #calculate diff
  head(RawWgt, -1) #drop last value
  ###
}

#function for bootsrapping
#build a function to calculate 3 things that I can bootstrap later
#1 raw weights
#2 diff between raw Weight  and random variable
#3 diff between raw Weight for focal variable and random variable
my.boot <- function(data, indices, outcome, predictors, focal = NULL){
  if (compare == "Yes") { 
    dt <- data[indices,]
    c(
      rwa(dt,  outcome, predictors)$result$Raw.RelWeight,
      rwa.rand(dt, outcome, predictors),
      rwa.comp(dt,  outcome, predictors, focal)
    )
  }
  else if (compare == "No") {
    dt <- data[indices,]
    c(
      rwa(dt,  outcome, predictors)$result$Raw.RelWeight,
      rwa.rand(dt, outcome, predictors)
    )
  }
}
