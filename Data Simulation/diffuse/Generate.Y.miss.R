# Function ----------------------------------------------------------------

# (1) Complete data generator
generate_data <- function(N, b2, num_aux, rep) {
  set.seed(rep)
  x1 <- rnorm(N, mean = 0, sd = 1)
  x2 <- rnorm(N, mean = 0, sd = 1)
  y <- 0.1 * x1 + b2 * x2 + rnorm(N, mean = 0, sd = 1)
  
  z <- matrix(NA, N, 50)  # generate 50 auxiliary variable
  for (j in 1:5) { z[, j] <- 0.3 * y + rnorm(N, mean = 0, sd = 1) }  
  for (j in 6:10) { z[, j] <- 0.1 * y + rnorm(N, mean = 0, sd = 1) }
  for (j in 11:50) { z[, j] <- 0 * y + rnorm(N, mean = 0, sd = 1) }
  
  colnames(z) <- paste0("z", 1:50)
  data <- as.data.frame(cbind(y, x1, x2, z[, 1:num_aux])) # depend on how many auxiliary variables need, extract from auxiliary variable data frame
  return(data)
}


# (2) Logistic missingness helper
get_alpha <- function(eta, mr) {
  
  alpha <- uniroot(function(a) mean(plogis(a + eta)) - mr, interval = c(-50, 50))$root # calibrate the intercept by finding alpha such that mean(plogis(alpha + eta)) equals the target missing rate; the interval [-50, 50] is used because plogis(-50) is close to 0 and plogis(50) is close to 1
  
  return(alpha)
}


apply_logistic_missing_Y <- function(data, eta, mr, rep) {
  
  set.seed(rep)
  
  alpha <- get_alpha(eta, mr) # alpha is a single intercept value applied to all rows
  prob_missing <- plogis(alpha + eta) # eta contains multiple values because it is computed row-wise as beta1 * data$x1 + beta2 * data$x2, or as beta_y * data$y + beta1 * data$x1 + beta2 * data$x2
  
  n_missing <- round(rep * nrow(data) * mr) - round((rep - 1) * nrow(data) * mr) # when N * mr is not an integer (e.g., N = 30 and mr = 0.15), this balances the number of missing cases across replications so the total missing count reaches the target
  indicator <- sample(seq_len(nrow(data)), size = n_missing, replace = F, prob = prob_missing)
  
  data[indicator, 1] <- NA
  
  return(data)
}


# (3) MAR_X1_X2 generator 
MAR_generator_Y_X1_X2 <- function(data, mr, rep, beta1 = 1, beta2 = 1){ 
  
  eta <- beta1 * data$x1 + beta2 * data$x2
  
  data <- apply_logistic_missing_Y(data, eta, mr, rep)
  
  return(data) 
}


# (4) MNAR_Y_X1_X2 generator
MNAR_generator_Y_Y_X1_X2 <- function(data, mr, rep, beta_y = 1, beta1 = 1, beta2 = 1){ 
  
  eta <- beta_y * data$y + beta1 * data$x1 + beta2 * data$x2
  
  data <- apply_logistic_missing_Y(data, eta, mr, rep)
  
  return(data) 
}






# Generate Data -----------------------------------------------------------
set.seed(1)

for (N in c(30, 50, 200, 500, 1000)) {
  for (b2 in c(0.1, 0.37, 0.5)) {
    for (num_aux in c(5, 20, 50)) {
      for (mr in c(0.15, 0.30, 0.60, 0.80)) {
        for (missing_type in c("Y_MAR_X1_X2", "Y_MNAR_Y_X1_X2")) {
            
            
            #create subfolders
            subf1 <- paste('mkdir /Data/Ymiss/n',N,'/b2.',b2, '.aux',num_aux, '.mr',mr, '.mt.', missing_type ,sep='')
            system(subf1)
            
            # Track the row (for repetition) in the result_matrix
            path <- paste('/Data/Ymiss/n',N,'/b2.',b2, '.aux',num_aux, '.mr',mr, '.mt.', missing_type ,sep='')
            setwd(path)
            
            # set up the repetition index for track purpose
            repetition_index <- 1
            
            for (rep in 1:200) {
              # Generate complete data
              data <- generate_data(N, b2, num_aux, rep)
              
              # Apply the missing data mechanism
              if (missing_type == "Y_MAR_X1_X2") {
                data_copy <- MAR_generator_Y_X1_X2(data, mr, rep)
                
              } else if (missing_type == "Y_MNAR_Y_X1_X2") {
                data_copy <- MNAR_generator_Y_Y_X1_X2(data, mr, rep)
                
              }
              
              # store the missing data 
              filename<-paste('data',repetition_index,'.txt', sep='')
              write.table(data_copy,filename,row.names=FALSE,col.names=FALSE)
              
              repetition_index <- repetition_index + 1  # Move to the next repetition
          }
        }
      }
    }
  }
}