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




# (2) MCAR generator  (Y)
MCAR_generator_Y <- function(data, mr, rep){ 
  
  set.seed(rep)
  
  indicator <- sample(c(0,1), nrow(data), replace = T, prob = c(mr, 1-mr))
  
  data[which(indicator==0),1] <- NA # only introduce missing to first column (y) of data 
  
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


# (4) MAR generator (Y, X1)
MAR_generator_Y_X1 <- function(data, mr){ 
  
  indicator <- which(data$x1 > qnorm(1-mr, mean = mean(data$x1), sd = sd(data$x1)))
  
  data[indicator,1] <- NA 
  
  return(data[,1,drop=F]) 
}


# (5) MNAR generator (Y, Z1)
MNAR_generator_Y_Z1 <- function(data, mr){ 
  
  indicator <- which(data$z1 > qnorm(1-mr, mean = mean(data$z1), sd = sd(data$z1)))
  
  data[indicator,1] <- NA 
  
  return(data[,1,drop=F]) 
}


MNAR_generator_Y_Self <- function(data, mr){ 
  
  indicator <- which(data$y > qnorm(1-mr, mean = mean(data$y), sd = sd(data$y)))
  
  data[indicator,1] <- NA 
  
  return(data[,1,drop=F]) 
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
          for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                                 "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
            
            
            #create subfolders
            subf1 <- paste('mkdir /scratch/your account/Data/XYmiss/n',N,'/b2.',b2, '.aux',num_aux, '.mry',mr_y, '.mrx', mr_x, '.mt.', missing_type ,sep='')
            system(subf1)
            
            # Track the row (for repetition) in the result_matrix
            path <- paste('/scratch/your account/Data/XYmiss/n',N,'/b2.',b2, '.aux',num_aux, '.mry',mr_y, '.mrx', mr_x, '.mt.', missing_type ,sep='')
            setwd(path)
            
            # set up the repetition index for track purpose
            repetition_index <- 1
            
            for (rep in 1:200) {
              # Generate complete data
              data <- generate_data(N, b2, num_aux, rep)
              
              # Apply the missing data mechanism
              if (missing_type == "Y_MCAR_XZ_MCAR") {
                
                data_y <- MCAR_generator_Y(data, mr_y, rep)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
          
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
              } else if (missing_type == "Y_MCAR_XZ_MNAR") {
                
                data_y <- MCAR_generator_Y(data, mr_y, rep)

                data_xz <- MNAR_generator_XZ(data, mr_x)

                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
              } else if (missing_type == "Y_MAR_X1_XZ_MCAR") {
                
                data_y <- MAR_generator_Y_X1(data, mr_y)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MAR_X1_XZ_MNAR") {
                
                data_y <- MAR_generator_Y_X1(data, mr_y)
                
                data_xz <- MNAR_generator_XZ(data, mr_x)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MNAR_Z1_XZ_MCAR") {
                
                data_y <- MNAR_generator_Y_Z1(data, mr_y)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
                
              } else if (missing_type == "Y_MNAR_Z1_XZ_MNAR") {
                
                data_y <- MNAR_generator_Y_Z1(data, mr_y)
                
                data_xz <- MNAR_generator_XZ(data, mr_x)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
              } else if (missing_type == "Y_MNAR_Self_XZ_MCAR") {
                
                data_y <- MNAR_generator_Y_Self(data, mr_y)
                
                data_xz <- MCAR_generator_XZ(data, mr_x, rep)
                
                data_copy <- as.data.frame(cbind(data_y, data_xz))
                
              } else if (missing_type == "Y_MNAR_Self_XZ_MNAR") {
                
                data_y <- MNAR_generator_Y_Self(data, mr_y)
                
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

