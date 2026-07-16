# Function ----------------------------------------------------------------

# (1) Generate data
generate_data <- function(N, b2, num_aux, rep) {
  set.seed(rep)
  x1 <- rnorm(N)
  x2 <- rnorm(N)
  y <- 0.1 * x1 + b2 * x2 + rnorm(N)
  
  z <- matrix(NA, N, 50)  # generate 50 auxiliary variable
  for (j in 1:5) { z[, j] <- 0.3 * y + rnorm(N) }  
  for (j in 6:10) { z[, j] <- 0.1 * y + rnorm(N) }
  for (j in 11:50) { z[, j] <- 0 * y + rnorm(N) }
  
  colnames(z) <- paste0("z", 1:50)
  data <- as.data.frame(cbind(y, x1, x2, z[, 1:num_aux])) # depend on how many auxiliary variables need, extract from auxiliary variable data frame
  return(data)
}




# (2) Logistic missingness helper (Y)
get_alpha <- function(eta, mr) {
  
  alpha <- uniroot(function(a) mean(plogis(a + eta)) - mr, interval = c(-50, 50))$root # calibrate the intercept by finding alpha such that mean(plogis(alpha + eta)) equals the target missing rate; the interval [-50, 50] is used because plogis(-50) is close to 0 and plogis(50) is close to 1
  
  return(alpha)
}


apply_logistic_missing_Y <- function(data, eta, mr, rep) {
  
  set.seed(rep)
  
  alpha <- get_alpha(eta, mr) # alpha is a single intercept value applied to all rows
  prob_missing <- plogis(alpha + eta) # eta is a vector of row-wise linear predictors based on x1 and x2, or on y, x1, and x2
  
  n_missing <- round(rep * nrow(data) * mr) - round((rep - 1) * nrow(data) * mr) # when N * mr is not an integer, this balances the number of missing cases across replications so the total missing count reaches the target
  indicator <- sample(seq_len(nrow(data)), size = n_missing, replace = F, prob = prob_missing)
  
  data[indicator,1] <- NA
  
  return(data[,1,drop=F]) # drop=F mean return data frame
}


# (3) MCAR generator (X & Z; Xs and Zs miss the same row)
MCAR_generator_XZ <- function(data, mr, rep){  
  
  set.seed(rep+1000) 
  
  for (i in 2:ncol(data)){
    
    indicator <- sample(c(0,1), nrow(data), replace = T, prob = c(mr, 1-mr))
    
    data[which(indicator==0), i] <- NA
  }
  
  return(data[,2:ncol(data),drop=F])
}


# (4) MAR generator (Y, X1, X2)
MAR_generator_Y_X1_X2 <- function(data, mr, rep, beta1 = 1, beta2 = 1){ 
  
  eta <- beta1 * data$x1 + beta2 * data$x2
  
  data <- apply_logistic_missing_Y(data, eta, mr, rep)
  
  return(data) 
}


# (5) MNAR generator (Y, Y, X1, X2)
MNAR_generator_Y_Y_X1_X2 <- function(data, mr, rep, beta_y = 1, beta1 = 1, beta2 = 1){ 
  
  eta <- beta_y * data$y + beta1 * data$x1 + beta2 * data$x2
  
  data <- apply_logistic_missing_Y(data, eta, mr, rep)
  
  return(data) 
}


# (6) MNAR generator (X & Z)
MNAR_generator_XZ <- function(data, mr){ 
  
  for (col_index in 2:ncol(data)){
    
    indicator <- which(data[,col_index] > qnorm(1-mr, mean = mean(data[,col_index]),sd = sd(data[,col_index])))
    
    data[indicator,col_index] <- NA 
    
  }
  
  return(data[,2:ncol(data), drop=F]) 
}





# Generate Data -----------------------------------------------------------
set.seed(1)

for (N in c(30, 50, 200, 500, 1000)) {
  for (b2 in c(0.1, 0.37, 0.5)) {
    for (num_aux in c(5, 20, 50)) {
      for (mr_y in c(0.15, 0.30, 0.60)) {
        for (mr_x in c(0.10, 0.30)) {
          for (missing_type in c("Y_MAR_X1_X2_XZ_MCAR", "Y_MAR_X1_X2_XZ_MNAR",
                                 "Y_MNAR_Y_X1_X2_XZ_MCAR", "Y_MNAR_Y_X1_X2_XZ_MNAR")) {
            
            
            #create subfolders
            subf1 <- paste('mkdir /Data/XYmiss/n',N,'/b2.',b2, '.aux',num_aux, '.mry',mr_y, '.mrx', mr_x, '.mt.', missing_type ,sep='')
            system(subf1)
            
            # Track the row (for repetition) in the result_matrix
            path <- paste('/Data/XYmiss/n',N,'/b2.',b2, '.aux',num_aux, '.mry',mr_y, '.mrx', mr_x, '.mt.', missing_type ,sep='')
            setwd(path)
            
            # set up the repetition index for track purpose
            repetition_index <- 1
            
            for (rep in 1:200) {
              # Generate complete data
              data <- generate_data(N, b2, num_aux, rep)
              
              # Apply the missing data mechanism
              if (missing_type == "Y_MAR_X1_X2_XZ_MCAR") {
                
                data_y <- MAR_generator_Y_X1_X2(data, mr_y, rep)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MAR_X1_X2_XZ_MNAR") {
                
                data_y <- MAR_generator_Y_X1_X2(data, mr_y, rep)
                
                data_xz <- MNAR_generator_XZ(data, mr_x)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MNAR_Y_X1_X2_XZ_MCAR") {
                
                data_y <- MNAR_generator_Y_Y_X1_X2(data, mr_y, rep)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MNAR_Y_X1_X2_XZ_MNAR") {
                
                data_y <- MNAR_generator_Y_Y_X1_X2(data, mr_y, rep)
                
                data_xz <- MNAR_generator_XZ(data, mr_x)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
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
}