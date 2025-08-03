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



# (2) MCAR generator 
MCAR_generator_Y <- function(data, mr, rep){ 
  
  set.seed(rep)
  
  indicator <- sample(c(0,1), nrow(data), replace = T, prob = c(mr, 1-mr))
  
  data[which(indicator==0),1] <- NA # only introduce missing to first column (y) of data 
  
  return(data) # return the missing data
}


# (3) MAR_X1 generator 
MAR_generator_Y_X1 <- function(data, mr){ 
  
  indicator <- which(data$x1 > qnorm(1-mr, mean=mean(data$x1), sd=sd(data$x1)))
  
  data[indicator,1] <- NA 
  
  return(data) 
}


# (4) MNAR generator
## first type: MNAR_1   OR    MAR_z1
MNAR_generator_Y_Z1 <- function(data, mr){ 
  
  indicator <- which(data$z1 > qnorm(1-mr, mean = mean(data$z1), sd = sd(data$z1)))
  
  data[indicator,1] <- NA 
  
  return(data) 
  
}

## second type: use y to determine the missingness in y, since y will never be used to imputed itself during the multiple imputation, so the missingness is related to missingness itself, which will alway be MNAR
MNAR_generator_Y_Self <- function(data, mr){ 
  
  indicator <- which(data$y > qnorm(1-mr, mean = mean(data$y), sd = sd(data$y)))
  
  data[indicator,1] <- NA 
  
  return(data) 
}






# Generate Data -----------------------------------------------------------
set.seed(1)

for (N in c(30, 50, 200, 500, 1000)) {
  for (b2 in c(0.1, 0.37, 0.5)) {
    for (num_aux in c(5, 20, 50)) {
      for (mr in c(0.15, 0.30, 0.60, 0.80)) {
        for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")) {
            
            
            #create subfolders
            subf1 <- paste('mkdir /scratch/your account/Data/Ymiss/n',N,'/b2.',b2, '.aux',num_aux, '.mr',mr, '.mt.', missing_type ,sep='')
            system(subf1)
            
            # Track the row (for repetition) in the result_matrix
            path <- paste('/scratch/your account/Data/Ymiss/n',N,'/b2.',b2, '.aux',num_aux, '.mr',mr, '.mt.', missing_type ,sep='')
            setwd(path)
            
            # set up the repetition index for track purpose
            repetition_index <- 1
            
            for (rep in 1:200) {
              # Generate complete data
              data <- generate_data(N, b2, num_aux, rep)
              
              # Apply the missing data mechanism
              if (missing_type == "Y_MCAR") {
                data_copy <- MCAR_generator_Y(data, mr, rep) # Because mcar is randomly drawn each time and is not based on the value of existing data, rep insurance is added
                
              } else if (missing_type == "Y_MAR_X1") {
                data_copy <- MAR_generator_Y_X1(data, mr)
                
              } else if (missing_type == "Y_MNAR_Z1") {
                data_copy <- MNAR_generator_Y_Z1(data, mr)
                
              } else if (missing_type == "Y_MNAR_Self") {
                data_copy <- MNAR_generator_Y_Self(data, mr)
                
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


