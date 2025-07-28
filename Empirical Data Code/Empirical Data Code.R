library(mice)
library(mirt)
library(lattice)
library(randomForest)
library(tidyverse)
library(ppcor)
library(lavaan)
library(readr)


# 1. Read the data --------------------------------------------------------

my_data <- read_csv("data.csv")
test.data <- my_data[,-c(101:108)] 


# 2. Check variables for outflux and influx -------------------------------

flux.data <- flux(test.data, local = names(test.data))  


# 3. Remove variables with low outflux or with an excessive proportion --------

select_var <- row.names(flux.data)[flux.data$outflux > 0.1 & flux.data$pobs > 0.3] 


# 4. check if any variables are excluded in step 3 ------------------------

length(select_var) 
dim(test.data)[2]


# factorize ---------------------------------------------------------------   
my_data$income <- as.numeric(factor(my_data$income, levels = c("at or below 200% poverty", "above 200% poverty"))) - 1



# 5. Check for collinearity issues by running an imputation with 0 max iterations and examining logged events--------
ini <- mice(test.data, 
            defaultMethod = c("pmm", "logreg", "polyreg", "polr"), 
            m = 5, 
            maxit = 0, 
            seed = 29725)

ini$loggedEvents 
# No warnings found, so all variables pass the test and can be included in the model initially.
ini$method
table(ini$nmis)



# 6. Fit a model using FIML -----------------------------------------------

fiml_function <- function(data) {
  model <- sem(
    model = "epigeneticage ~ averagedisc + male + white + income + smoking", 
    data = data, 
    missing = "fiml", 
    fixed.x = FALSE
  )
  result <- summary(model)
  return(result)
}

fiml_result <- fiml_function(my_data)$pe[1:5,]


# 7. Define functions for different imputation setups ---------------------

default <- function(data, method_list) {
  complete_data <- mice(
    data = data,
    m = 5,  
    method = method_list,
    maxit = 5,
    seed = 29725,  
    predictorMatrix = quickpred(data)
  )
  return(complete_data)
}



# 8. List of method combinations (for continuous and binary variab --------

methods_list <- list(
  PMM = "pmm",
  CART = "cart",
  NORM = "norm"
)


# 9. Create lists for the three different imputation strategies -----------

result_default <- list()



# 10. Apply imputation and model fitting to each method combination -------

for (name in names(methods_list)) {
  complete_data <- try(default(data = my_data, method = methods_list[[name]]), silent = TRUE)
  
  if (!inherits(complete_data, "try-error")) {
    result <- try(
      summary(
        pool(
          with(complete_data, lm(epigeneticage ~ averagedisc + male + white + income + smoking))
        )
      ),
      silent = TRUE
    )
    
    if (!inherits(result, "try-error")) {
      result_default[[name]] <- result
    } else {
      result_default[[name]] <- "model fit error"
    }
    
  } else {
    result_default[[name]] <- "imputation error"
  }
}



# 11. Print the result ----------------------------------------------------
result_default

fiml_result
