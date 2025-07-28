library(mice)
library(mirt)
library(lattice)
library(randomForest)
library(tidyverse)
library(ppcor)
library(lavaan)


# Define the functions ----------------------------------------------------


# (1) listwise deletion 
listwise_function <- function(data) {
  # Generate the imputed data
  miss_indicator <- numeric(length = nrow(data))
  
  # Identify rows with missing values
  for (i in 1:nrow(data)) {
    miss_indicator[i] <- sum(is.na(data[i, ]))
  }
  
  # Find rows with no missing values
  complete_index <- which(miss_indicator == 0)
  
  # Check if there are any complete rows
  if (length(complete_index) != 0) {
    # Try to extract complete data
    complete_data <- try(data[complete_index, ])
    if (!inherits(complete_data, "try-error")) {
      # Try to fit the linear model
      model <- try(lm(V1 ~ V2 + V3, complete_data))
      if (!inherits(model, "try-error")) {
        # Get the model summary and extract the relevant coefficients
        result <- summary(model)
        return(c(result$coefficients)[c(2, 3, 5, 6, 11, 12)])
      }
    }
  }
  
  # Return NA vector if there are no complete rows or if an error occurs
  return(rep(NA, 6)) # Adjust length depending on the output requirements
}



# (2) FIML 
fiml_function <- function(data){
  model <- try(sem(model = "V1 ~ V2 + V3", 
                   data = data, 
                   missing = "fiml", 
                   fixed.x = FALSE))
  
  if(!inherits(model, "try-error")){
    result <- try(summary(model))
    if(!inherits(result, "try-error")){
      final_result <- try(as.numeric(unlist(result$pe[1:2,c(5,6,8)])))
      if(!inherits(final_result, "try-error")){
        return(final_result)
      }
    }
  }
  return(rep(NA, 6))
}


# (3) MICE-PMM
##Default quickpred()
pmm_function_default <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "pmm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}



##Set threshold for quickpred()
pmm_function_0.2 <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "pmm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data = data,
                                                        mincor = 0.2)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}



##Define the prediction matrix
pmm_function_manual <- function(data) {
  
  pred_matrix <- make.predictorMatrix(data)
  
  pred_matrix[1,-c(2,3)] <- 0   # only use x1 and x2 to impute y
  pred_matrix[2,-c(1,3)] <- 0   # only use y and x2 to impute x1
  pred_matrix[3,-c(1,2)] <- 0   # only use y and x1 to impute x2
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "pmm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = pred_matrix))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}






# (4) MICE-CART
## Default quickpred()
cart_function_default <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "cart",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}


## Set threshold for quickpred()
cart_function_0.2 <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "cart",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data = data,
                                                        mincor = 0.2)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}


## Define the prediction matrix
cart_function_manual <- function(data) {
  
  pred_matrix <- make.predictorMatrix(data)
  
  pred_matrix[1,-c(2,3)] <- 0   # only use x1 and x2 to impute y
  pred_matrix[2,-c(1,3)] <- 0   # only use y and x2 to impute x1
  pred_matrix[3,-c(1,2)] <- 0   # only use y and x1 to impute x2
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "cart",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = pred_matrix))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}




# (5) MICE-NORM
## Default quickpred()
norm_function_default <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "norm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}


## Set threshold for quickpred()
norm_function_0.2 <- function(data) {
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "norm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = quickpred(data = data,
                                                        mincor = 0.2)))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}


## Define the prediction matrix
norm_function_manual <- function(data) {
  
  pred_matrix <- make.predictorMatrix(data)
  
  pred_matrix[1,-c(2,3)] <- 0   # only use x1 and x2 to impute y
  pred_matrix[2,-c(1,3)] <- 0   # only use y and x2 to impute x1
  pred_matrix[3,-c(1,2)] <- 0   # only use y and x1 to impute x2
  
  complete_data <- try(mice(data = data,
                            m = 5, # generate 5 imputed data
                            method = "norm",
                            maxit = 5,
                            seed = 1, 
                            predictorMatrix = pred_matrix))
  
  if (!inherits(complete_data, "try-error")) {
    # Try to extract the result from the pooled model
    result <- try(as.numeric(unlist(summary(pool(with(complete_data, lm(V1 ~ V2 + V3))))))[c(5, 6, 8, 9, 17, 18)])
    if (!inherits(result, "try-error")){
      return(result)
    }
  }
  
  # If an error occurred at any stage, return an NA vector of length 6
  return(rep(NA, 6))
}



# (6) complete data result generator 
complete_function <- function(data){
  # fit the regression model
  model <- lm(V1 ~ V2 + V3, data)
  
  result <- summary(model)
  
  # return the result 
  return(c(result$coefficients)[c(2,3,5,6,11,12)])
}



# (7) parameter estimate calculator 
para_calculator <- function(result, true_x1_est, true_x2_est) {
  true_x1 <- true_x1_est
  true_x2 <- true_x2_est
  alpha <- 0.05
  
  # bias x1
  bias_x1_listwise <- mean(result[,1], na.rm = TRUE) - true_x1
  bias_x1_fiml <- mean(result[,7], na.rm = TRUE) - true_x1
  bias_x1_pmm_default <- mean(result[,13], na.rm = TRUE) - true_x1
  bias_x1_pmm_0.2 <- mean(result[,19], na.rm = TRUE) - true_x1
  bias_x1_pmm_manual <- mean(result[,25], na.rm = TRUE) - true_x1
  bias_x1_cart_default <- mean(result[,31], na.rm = TRUE) - true_x1
  bias_x1_cart_0.2 <- mean(result[,37], na.rm = TRUE) - true_x1
  bias_x1_cart_manual <- mean(result[,43], na.rm = TRUE) - true_x1
  bias_x1_norm_default <- mean(result[,49], na.rm = TRUE) - true_x1
  bias_x1_norm_0.2 <- mean(result[,55], na.rm = TRUE) - true_x1
  bias_x1_norm_manual <- mean(result[,61], na.rm = TRUE) - true_x1
  
  # relative bias x1
  relative_bias_x1_listwise <- bias_x1_listwise / true_x1
  relative_bias_x1_fiml <- bias_x1_fiml / true_x1
  relative_bias_x1_pmm_default <- bias_x1_pmm_default / true_x1
  relative_bias_x1_pmm_0.2 <- bias_x1_pmm_0.2 / true_x1
  relative_bias_x1_pmm_manual <- bias_x1_pmm_manual / true_x1
  relative_bias_x1_cart_default <- bias_x1_cart_default / true_x1
  relative_bias_x1_cart_0.2 <- bias_x1_cart_0.2 / true_x1
  relative_bias_x1_cart_manual <- bias_x1_cart_manual / true_x1
  relative_bias_x1_norm_default <- bias_x1_norm_default / true_x1
  relative_bias_x1_norm_0.2 <- bias_x1_norm_0.2 / true_x1
  relative_bias_x1_norm_manual <- bias_x1_norm_manual / true_x1
  
  # power x1
  power_x1_listwise     <- sum(result[,5]  < alpha & !is.na(result[,5])  & !is.na(result[,1]))  / sum(!is.na(result[,5])  & !is.na(result[,1]))
  power_x1_fiml         <- sum(result[,11] < alpha & !is.na(result[,11]) & !is.na(result[,7]))  / sum(!is.na(result[,11]) & !is.na(result[,7]))
  power_x1_pmm_default  <- sum(result[,17] < alpha & !is.na(result[,17]) & !is.na(result[,13])) / sum(!is.na(result[,17]) & !is.na(result[,13]))
  power_x1_pmm_0.2      <- sum(result[,23] < alpha & !is.na(result[,23]) & !is.na(result[,19])) / sum(!is.na(result[,23]) & !is.na(result[,19]))
  power_x1_pmm_manual   <- sum(result[,29] < alpha & !is.na(result[,29]) & !is.na(result[,25])) / sum(!is.na(result[,29]) & !is.na(result[,25]))
  power_x1_cart_default <- sum(result[,35] < alpha & !is.na(result[,35]) & !is.na(result[,31])) / sum(!is.na(result[,35]) & !is.na(result[,31]))
  power_x1_cart_0.2     <- sum(result[,41] < alpha & !is.na(result[,41]) & !is.na(result[,37])) / sum(!is.na(result[,41]) & !is.na(result[,37]))
  power_x1_cart_manual  <- sum(result[,47] < alpha & !is.na(result[,47]) & !is.na(result[,43])) / sum(!is.na(result[,47]) & !is.na(result[,43]))
  power_x1_norm_default <- sum(result[,53] < alpha & !is.na(result[,53]) & !is.na(result[,49])) / sum(!is.na(result[,53]) & !is.na(result[,49]))
  power_x1_norm_0.2     <- sum(result[,59] < alpha & !is.na(result[,59]) & !is.na(result[,55])) / sum(!is.na(result[,59]) & !is.na(result[,55]))
  power_x1_norm_manual  <- sum(result[,65] < alpha & !is.na(result[,65]) & !is.na(result[,61])) / sum(!is.na(result[,65]) & !is.na(result[,61]))
  
  
  # average standard error (ASE) x1
  ase_x1_listwise <- mean(result[,3], na.rm = TRUE) 
  ase_x1_fiml <- mean(result[,9], na.rm = TRUE) 
  ase_x1_pmm_default <- mean(result[,15], na.rm = TRUE) 
  ase_x1_pmm_0.2 <- mean(result[,21], na.rm = TRUE) 
  ase_x1_pmm_manual <- mean(result[,27], na.rm = TRUE) 
  ase_x1_cart_default <- mean(result[,33], na.rm = TRUE) 
  ase_x1_cart_0.2 <- mean(result[,39], na.rm = TRUE) 
  ase_x1_cart_manual <- mean(result[,45], na.rm = TRUE) 
  ase_x1_norm_default <- mean(result[,51], na.rm = TRUE) 
  ase_x1_norm_0.2 <- mean(result[,57], na.rm = TRUE) 
  ase_x1_norm_manual <- mean(result[,63], na.rm = TRUE) 
  
  # empirical standard error (ESE) x1
  ese_x1_listwise <- sd(result[,1], na.rm = TRUE)
  ese_x1_fiml <- sd(result[,7], na.rm = TRUE)
  ese_x1_pmm_default <- sd(result[,13], na.rm = TRUE)
  ese_x1_pmm_0.2 <- sd(result[,19], na.rm = TRUE)
  ese_x1_pmm_manual <- sd(result[,25], na.rm = TRUE)
  ese_x1_cart_default <- sd(result[,31], na.rm = TRUE)
  ese_x1_cart_0.2 <- sd(result[,37], na.rm = TRUE)
  ese_x1_cart_manual <- sd(result[,43], na.rm = TRUE)
  ese_x1_norm_default <- sd(result[,49], na.rm = TRUE)
  ese_x1_norm_0.2 <- sd(result[,55], na.rm = TRUE)
  ese_x1_norm_manual <- sd(result[,61], na.rm = TRUE)
  
  # MSE (bias^2 + ESE^2) x1
  mse_x1_listwise <- bias_x1_listwise^2 + ese_x1_listwise^2
  mse_x1_fiml <- bias_x1_fiml^2 + ese_x1_fiml^2
  mse_x1_pmm_default <- bias_x1_pmm_default^2 + ese_x1_pmm_default^2
  mse_x1_pmm_0.2 <- bias_x1_pmm_0.2^2 + ese_x1_pmm_0.2^2
  mse_x1_pmm_manual <- bias_x1_pmm_manual^2 + ese_x1_pmm_manual^2
  mse_x1_cart_default <- bias_x1_cart_default^2 + ese_x1_cart_default^2
  mse_x1_cart_0.2 <- bias_x1_cart_0.2^2 + ese_x1_cart_0.2^2
  mse_x1_cart_manual <- bias_x1_cart_manual^2 + ese_x1_cart_manual^2
  mse_x1_norm_default <- bias_x1_norm_default^2 + ese_x1_norm_default^2
  mse_x1_norm_0.2 <- bias_x1_norm_0.2^2 + ese_x1_norm_0.2^2
  mse_x1_norm_manual <- bias_x1_norm_manual^2 + ese_x1_norm_manual^2
  
  # Bias x2
  bias_x2_listwise <- mean(result[,2], na.rm = TRUE) - true_x2
  bias_x2_fiml <- mean(result[,8], na.rm = TRUE) - true_x2
  bias_x2_pmm_default <- mean(result[,14], na.rm = TRUE) - true_x2
  bias_x2_pmm_0.2 <- mean(result[,20], na.rm = TRUE) - true_x2
  bias_x2_pmm_manual <- mean(result[,26], na.rm = TRUE) - true_x2
  bias_x2_cart_default <- mean(result[,32], na.rm = TRUE) - true_x2
  bias_x2_cart_0.2 <- mean(result[,38], na.rm = TRUE) - true_x2
  bias_x2_cart_manual <- mean(result[,44], na.rm = TRUE) - true_x2
  bias_x2_norm_default <- mean(result[,50], na.rm = TRUE) - true_x2
  bias_x2_norm_0.2 <- mean(result[,56], na.rm = TRUE) - true_x2
  bias_x2_norm_manual <- mean(result[,62], na.rm = TRUE) - true_x2
  
  # relative bias x2
  relative_bias_x2_listwise <- bias_x2_listwise / true_x2
  relative_bias_x2_fiml <- bias_x2_fiml / true_x2
  relative_bias_x2_pmm_default <- bias_x2_pmm_default / true_x2
  relative_bias_x2_pmm_0.2 <- bias_x2_pmm_0.2 / true_x2
  relative_bias_x2_pmm_manual <- bias_x2_pmm_manual / true_x2
  relative_bias_x2_cart_default <- bias_x2_cart_default / true_x2
  relative_bias_x2_cart_0.2 <- bias_x2_cart_0.2 / true_x2
  relative_bias_x2_cart_manual <- bias_x2_cart_manual / true_x2
  relative_bias_x2_norm_default <- bias_x2_norm_default / true_x2
  relative_bias_x2_norm_0.2 <- bias_x2_norm_0.2 / true_x2
  relative_bias_x2_norm_manual <- bias_x2_norm_manual / true_x2
  
  # power x2
  power_x2_listwise     <- sum(result[,6]  < alpha & !is.na(result[,6])  & !is.na(result[,2]))  / sum(!is.na(result[,6])  & !is.na(result[,2]))
  power_x2_fiml         <- sum(result[,12] < alpha & !is.na(result[,12]) & !is.na(result[,8]))  / sum(!is.na(result[,12]) & !is.na(result[,8]))
  power_x2_pmm_default  <- sum(result[,18] < alpha & !is.na(result[,18]) & !is.na(result[,14])) / sum(!is.na(result[,18]) & !is.na(result[,14]))
  power_x2_pmm_0.2      <- sum(result[,24] < alpha & !is.na(result[,24]) & !is.na(result[,20])) / sum(!is.na(result[,24]) & !is.na(result[,20]))
  power_x2_pmm_manual   <- sum(result[,30] < alpha & !is.na(result[,30]) & !is.na(result[,26])) / sum(!is.na(result[,30]) & !is.na(result[,26]))
  power_x2_cart_default <- sum(result[,36] < alpha & !is.na(result[,36]) & !is.na(result[,32])) / sum(!is.na(result[,36]) & !is.na(result[,32]))
  power_x2_cart_0.2     <- sum(result[,42] < alpha & !is.na(result[,42]) & !is.na(result[,38])) / sum(!is.na(result[,42]) & !is.na(result[,38]))
  power_x2_cart_manual  <- sum(result[,48] < alpha & !is.na(result[,48]) & !is.na(result[,44])) / sum(!is.na(result[,48]) & !is.na(result[,44]))
  power_x2_norm_default <- sum(result[,54] < alpha & !is.na(result[,54]) & !is.na(result[,50])) / sum(!is.na(result[,54]) & !is.na(result[,50]))
  power_x2_norm_0.2     <- sum(result[,60] < alpha & !is.na(result[,60]) & !is.na(result[,56])) / sum(!is.na(result[,60]) & !is.na(result[,56]))
  power_x2_norm_manual  <- sum(result[,66] < alpha & !is.na(result[,66]) & !is.na(result[,62])) / sum(!is.na(result[,66]) & !is.na(result[,62]))
  
  
  
  # average standard error (ASE) x2
  ase_x2_listwise <- mean(result[,4], na.rm = TRUE) 
  ase_x2_fiml <- mean(result[,10], na.rm = TRUE) 
  ase_x2_pmm_default <- mean(result[,16], na.rm = TRUE) 
  ase_x2_pmm_0.2 <- mean(result[,22], na.rm = TRUE) 
  ase_x2_pmm_manual <- mean(result[,28], na.rm = TRUE) 
  ase_x2_cart_default <- mean(result[,34], na.rm = TRUE) 
  ase_x2_cart_0.2 <- mean(result[,40], na.rm = TRUE) 
  ase_x2_cart_manual <- mean(result[,46], na.rm = TRUE) 
  ase_x2_norm_default <- mean(result[,52], na.rm = TRUE) 
  ase_x2_norm_0.2 <- mean(result[,58], na.rm = TRUE) 
  ase_x2_norm_manual <- mean(result[,64], na.rm = TRUE) 
  
  # empirical standard error (ESE) x2
  ese_x2_listwise <- sd(result[,2], na.rm = TRUE)
  ese_x2_fiml <- sd(result[,8], na.rm = TRUE)
  ese_x2_pmm_default <- sd(result[,14], na.rm = TRUE)
  ese_x2_pmm_0.2 <- sd(result[,20], na.rm = TRUE)
  ese_x2_pmm_manual <- sd(result[,26], na.rm = TRUE)
  ese_x2_cart_default <- sd(result[,32], na.rm = TRUE)
  ese_x2_cart_0.2 <- sd(result[,38], na.rm = TRUE)
  ese_x2_cart_manual <- sd(result[,44], na.rm = TRUE)
  ese_x2_norm_default <- sd(result[,50], na.rm = TRUE)
  ese_x2_norm_0.2 <- sd(result[,56], na.rm = TRUE)
  ese_x2_norm_manual <- sd(result[,62], na.rm = TRUE)
  
  # MSE (bias^2 + ESE^2) x2
  mse_x2_listwise <- bias_x2_listwise^2 + ese_x2_listwise^2
  mse_x2_fiml <- bias_x2_fiml^2 + ese_x2_fiml^2
  mse_x2_pmm_default <- bias_x2_pmm_default^2 + ese_x2_pmm_default^2
  mse_x2_pmm_0.2 <- bias_x2_pmm_0.2^2 + ese_x2_pmm_0.2^2
  mse_x2_pmm_manual <- bias_x2_pmm_manual^2 + ese_x2_pmm_manual^2
  mse_x2_cart_default <- bias_x2_cart_default^2 + ese_x2_cart_default^2
  mse_x2_cart_0.2 <- bias_x2_cart_0.2^2 + ese_x2_cart_0.2^2
  mse_x2_cart_manual <- bias_x2_cart_manual^2 + ese_x2_cart_manual^2
  mse_x2_norm_default <- bias_x2_norm_default^2 + ese_x2_norm_default^2
  mse_x2_norm_0.2 <- bias_x2_norm_0.2^2 + ese_x2_norm_0.2^2
  mse_x2_norm_manual <- bias_x2_norm_manual^2 + ese_x2_norm_manual^2
  
  result_vector <- c(
    # Bias X1
    bias_x1_listwise, bias_x1_fiml, bias_x1_pmm_default, bias_x1_pmm_0.2, bias_x1_pmm_manual,
    bias_x1_cart_default, bias_x1_cart_0.2, bias_x1_cart_manual,
    bias_x1_norm_default, bias_x1_norm_0.2, bias_x1_norm_manual,
    
    # Relative Bias X1
    relative_bias_x1_listwise, relative_bias_x1_fiml, relative_bias_x1_pmm_default,
    relative_bias_x1_pmm_0.2, relative_bias_x1_pmm_manual, relative_bias_x1_cart_default,
    relative_bias_x1_cart_0.2, relative_bias_x1_cart_manual,
    relative_bias_x1_norm_default, relative_bias_x1_norm_0.2, relative_bias_x1_norm_manual,
    
    # Type I Error Rate / Power X1
    power_x1_listwise, power_x1_fiml, power_x1_pmm_default,
    power_x1_pmm_0.2, power_x1_pmm_manual, power_x1_cart_default,
    power_x1_cart_0.2, power_x1_cart_manual,
    power_x1_norm_default, power_x1_norm_0.2, power_x1_norm_manual,
    
    # ASE X1
    ase_x1_listwise, ase_x1_fiml, ase_x1_pmm_default, ase_x1_pmm_0.2,
    ase_x1_pmm_manual, ase_x1_cart_default, ase_x1_cart_0.2,
    ase_x1_cart_manual, ase_x1_norm_default, ase_x1_norm_0.2,
    ase_x1_norm_manual,
    
    # ESE X1
    ese_x1_listwise, ese_x1_fiml, ese_x1_pmm_default, ese_x1_pmm_0.2,
    ese_x1_pmm_manual, ese_x1_cart_default, ese_x1_cart_0.2,
    ese_x1_cart_manual, ese_x1_norm_default, ese_x1_norm_0.2,
    ese_x1_norm_manual,
    
    # MSE X1
    mse_x1_listwise, mse_x1_fiml, mse_x1_pmm_default, mse_x1_pmm_0.2,
    mse_x1_pmm_manual, mse_x1_cart_default, mse_x1_cart_0.2,
    mse_x1_cart_manual, mse_x1_norm_default, mse_x1_norm_0.2,
    mse_x1_norm_manual,
    
    # Bias X2
    bias_x2_listwise, bias_x2_fiml, bias_x2_pmm_default, bias_x2_pmm_0.2, bias_x2_pmm_manual,
    bias_x2_cart_default, bias_x2_cart_0.2, bias_x2_cart_manual,
    bias_x2_norm_default, bias_x2_norm_0.2, bias_x2_norm_manual,
    
    # Relative Bias X2
    relative_bias_x2_listwise, relative_bias_x2_fiml, relative_bias_x2_pmm_default,
    relative_bias_x2_pmm_0.2, relative_bias_x2_pmm_manual, relative_bias_x2_cart_default,
    relative_bias_x2_cart_0.2, relative_bias_x2_cart_manual,
    relative_bias_x2_norm_default, relative_bias_x2_norm_0.2, relative_bias_x2_norm_manual,
    
    # Type I Error Rate / Power X2
    power_x2_listwise, power_x2_fiml, power_x2_pmm_default,
    power_x2_pmm_0.2, power_x2_pmm_manual, power_x2_cart_default,
    power_x2_cart_0.2, power_x2_cart_manual,
    power_x2_norm_default, power_x2_norm_0.2, power_x2_norm_manual,
    
    # ASE X2
    ase_x2_listwise, ase_x2_fiml, ase_x2_pmm_default, ase_x2_pmm_0.2,
    ase_x2_pmm_manual, ase_x2_cart_default, ase_x2_cart_0.2,
    ase_x2_cart_manual, ase_x2_norm_default, ase_x2_norm_0.2,
    ase_x2_norm_manual,
    
    # ESE X2
    ese_x2_listwise, ese_x2_fiml, ese_x2_pmm_default, ese_x2_pmm_0.2,
    ese_x2_pmm_manual, ese_x2_cart_default, ese_x2_cart_0.2,
    ese_x2_cart_manual, ese_x2_norm_default, ese_x2_norm_0.2,
    ese_x2_norm_manual,
    
    # MSE X2
    mse_x2_listwise, mse_x2_fiml, mse_x2_pmm_default, mse_x2_pmm_0.2,
    mse_x2_pmm_manual, mse_x2_cart_default, mse_x2_cart_0.2,
    mse_x2_cart_manual, mse_x2_norm_default, mse_x2_norm_0.2,
    mse_x2_norm_manual
  )
  
  return(result_vector)
}




# (8) name list for parameter_matrix
name_list <- c(
  
  "N", "b2", "num_aux", "mr_y", "mr_x", "missing_type",
  
  # Bias X1
  "bias_x1_listwise", "bias_x1_fiml", "bias_x1_pmm_default", "bias_x1_pmm_0.2", "bias_x1_pmm_manual",
  "bias_x1_cart_default", "bias_x1_cart_0.2", "bias_x1_cart_manual",
  "bias_x1_norm_default", "bias_x1_norm_0.2", "bias_x1_norm_manual",
  
  # Relative Bias X1
  "relative_bias_x1_listwise", "relative_bias_x1_fiml", "relative_bias_x1_pmm_default",
  "relative_bias_x1_pmm_0.2", "relative_bias_x1_pmm_manual", "relative_bias_x1_cart_default",
  "relative_bias_x1_cart_0.2", "relative_bias_x1_cart_manual",
  "relative_bias_x1_norm_default", "relative_bias_x1_norm_0.2", "relative_bias_x1_norm_manual",
  
  # Type I Error Rate / Power X1
  "power_x1_listwise", "power_x1_fiml", "power_x1_pmm_default",
  "power_x1_pmm_0.2", "power_x1_pmm_manual", "power_x1_cart_default",
  "power_x1_cart_0.2", "power_x1_cart_manual",
  "power_x1_norm_default", "power_x1_norm_0.2", "power_x1_norm_manual",
  
  
  # ASE X1
  "ase_x1_listwise", "ase_x1_fiml", "ase_x1_pmm_default", "ase_x1_pmm_0.2",
  "ase_x1_pmm_manual", "ase_x1_cart_default", "ase_x1_cart_0.2",
  "ase_x1_cart_manual", "ase_x1_norm_default", "ase_x1_norm_0.2",
  "ase_x1_norm_manual",
  
  # ESE X1
  "ese_x1_listwise", "ese_x1_fiml", "ese_x1_pmm_default", "ese_x1_pmm_0.2",
  "ese_x1_pmm_manual", "ese_x1_cart_default", "ese_x1_cart_0.2",
  "ese_x1_cart_manual", "ese_x1_norm_default", "ese_x1_norm_0.2",
  "ese_x1_norm_manual",
  
  # MSE X1
  "mse_x1_listwise", "mse_x1_fiml", "mse_x1_pmm_default", "mse_x1_pmm_0.2",
  "mse_x1_pmm_manual", "mse_x1_cart_default", "mse_x1_cart_0.2",
  "mse_x1_cart_manual", "mse_x1_norm_default", "mse_x1_norm_0.2",
  "mse_x1_norm_manual",
  
  # Bias X2
  "bias_x2_listwise", "bias_x2_fiml", "bias_x2_pmm_default", "bias_x2_pmm_0.2", "bias_x2_pmm_manual",
  "bias_x2_cart_default", "bias_x2_cart_0.2", "bias_x2_cart_manual",
  "bias_x2_norm_default", "bias_x2_norm_0.2", "bias_x2_norm_manual",
  
  # Relative Bias X2
  "relative_bias_x2_listwise", "relative_bias_x2_fiml", "relative_bias_x2_pmm_default",
  "relative_bias_x2_pmm_0.2", "relative_bias_x2_pmm_manual", "relative_bias_x2_cart_default",
  "relative_bias_x2_cart_0.2", "relative_bias_x2_cart_manual",
  "relative_bias_x2_norm_default", "relative_bias_x2_norm_0.2", "relative_bias_x2_norm_manual",
  
  # Type I Error Rate / Power X2
  "power_x2_listwise", "power_x2_fiml", "power_x2_pmm_default",
  "power_x2_pmm_0.2", "power_x2_pmm_manual", "power_x2_cart_default",
  "power_x2_cart_0.2", "power_x2_cart_manual",
  "power_x2_norm_default", "power_x2_norm_0.2", "power_x2_norm_manual",
  
  
  # ASE X2
  "ase_x2_listwise", "ase_x2_fiml", "ase_x2_pmm_default", "ase_x2_pmm_0.2",
  "ase_x2_pmm_manual", "ase_x2_cart_default", "ase_x2_cart_0.2",
  "ase_x2_cart_manual", "ase_x2_norm_default", "ase_x2_norm_0.2",
  "ase_x2_norm_manual",
  
  # ESE X2
  "ese_x2_listwise", "ese_x2_fiml", "ese_x2_pmm_default", "ese_x2_pmm_0.2",
  "ese_x2_pmm_manual", "ese_x2_cart_default", "ese_x2_cart_0.2",
  "ese_x2_cart_manual", "ese_x2_norm_default", "ese_x2_norm_0.2",
  "ese_x2_norm_manual",
  
  # MSE X2
  "mse_x2_listwise", "mse_x2_fiml", "mse_x2_pmm_default", "mse_x2_pmm_0.2",
  "mse_x2_pmm_manual", "mse_x2_cart_default", "mse_x2_cart_0.2",
  "mse_x2_cart_manual", "mse_x2_norm_default", "mse_x2_norm_0.2",
  "mse_x2_norm_manual"
)



# Run the analysis  -------------------------------------------------------

# create a big matrix to store the final parameter result
parameter_matrix <- matrix(NA, nrow = 1, ncol = length(name_list)) 

# Initialize a matrix to store the estimates for 200 repetitions
result_matrix <- matrix(NA, nrow = 200, ncol = 66)  # 11 methods * 6 coefficients per method

# read the data 
for (index in 1:200){
  
  file_path <- paste0("/scratch/your account/Data/XYmiss/", "n500", "/",
                      "replace1", ".", "replace2", ".", "replace3", ".", "replace4", ".", "replace5",
                      "/data", index, ".txt")
  
  data <- read.table(file_path)  # use the setting working directory above to read the dataset 
  
  if (
    length(listwise_function(data)) == 6 &&
    length(fiml_function(data)) == 6 &&
    length(pmm_function_default(data)) == 6 &&
    length(pmm_function_0.2(data)) == 6 &&
    length(pmm_function_manual(data)) == 6 &&
    length(cart_function_default(data)) == 6 &&
    length(cart_function_0.2(data)) == 6 &&
    length(cart_function_manual(data)) == 6 &&
    length(norm_function_default(data)) == 6 &&
    length(norm_function_0.2(data)) == 6 &&
    length(norm_function_manual(data)) == 6
  ) {
    result_matrix[index, ] <- c(
      listwise_function(data),               # listwise deletion results
      fiml_function(data),                   # FIML results
      pmm_function_default(data),            # PMM default
      pmm_function_0.2(data),                # PMM with quickpred threshold
      pmm_function_manual(data),             # PMM manual
      cart_function_default(data),           # CART default
      cart_function_0.2(data),               # CART with quickpred threshold
      cart_function_manual(data),            # CART manual
      norm_function_default(data),           # NORM default
      norm_function_0.2(data),               # NORM with quickpred threshold
      norm_function_manual(data)             # NORM manual
    )
  }
  
}


# Store the original result matrix ----------------------------------------

# set working directory to the result folder 
setwd('/scratch/your account/Result/XYmiss/n500')

# save the result matrix
filename <- paste('/scratch/your account/Result/XYmiss/n500/replace1.replace2.replace3.replace4.replace5.result.txt',sep='')

write.table(result_matrix, file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE)


# Store the parameter result --------------------------------------------------------

# define the N, b2, num_aux, mr, missing type
replace1 <- "replace1"
replace2 <- "replace2"
replace3 <- "replace3"
replace4 <- "replace4"
replace5 <- "replace5"

# Extracting values
value1 <- sub("^b2\\.", "", replace1)
value2 <- sub("^aux", "", replace2)
value3 <- sub("^mr.", "", replace3)
value4 <- sub("^mr.", "", replace4)
value5 <- sub("^mt\\.", "", replace5)

# define the parameter matrix
parameter_matrix[1, ] <- c(nrow(data), value1, value2, value3, value4, value5, para_calculator(result_matrix, true_x1_est = 0.1, true_x2_est = as.numeric(value1)))

parameter_df <- as.data.frame(parameter_matrix)

names(parameter_df) <- name_list # add the column name for the dataframe

# save the result 
filename <- paste('/scratch/your account/Result/XYmiss/n500/replace1.replace2.replace3.replace4.replace5.parameter.txt',sep='')

write.table(parameter_df,filename,row.names=FALSE,col.names=TRUE,quote = FALSE)

