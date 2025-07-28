# 全新方法 --------------------------------------------------------------------

load("df_Y_all_parameter_original.RData")

# -------------------------------------------------------------------------


df_Y_30 <- matrix(NA, nrow = 144, ncol = 137)
index <- 1

for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        filename <- paste0("b2.", b2, ".aux", num_aux, ".mr", mr, ".mt.", missing_type, ".parameter", ".txt")
        data <- read.table(filename, header = TRUE)
        
        df_Y_30[index, 1:4] <- as.numeric(data[1, 1:4])
        df_Y_30[index, 5]   <- as.character(data[1, 5])
        if (ncol(data) > 5) {
          df_Y_30[index, 6:ncol(data)] <- as.numeric(data[1, 6:ncol(data)])
        }
        
        index <- index + 1
      }
    }
  }
}

df_Y_30 <- as.data.frame(df_Y_30)
colnames(df_Y_30) <- colnames(data)
View(df_Y_30)




df_Y_50 <- matrix(NA, nrow = 144, ncol = 137)
index <- 1

for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        filename <- paste0("b2.", b2, ".aux", num_aux, ".mr", mr, ".mt.", missing_type, ".parameter", ".txt")
        data <- read.table(filename, header = TRUE)
        
        df_Y_50[index, 1:4] <- as.numeric(data[1, 1:4])
        df_Y_50[index, 5]   <- as.character(data[1, 5])
        if (ncol(data) > 5) {
          df_Y_50[index, 6:ncol(data)] <- as.numeric(data[1, 6:ncol(data)])
        }
        
        index <- index + 1
      }
    }
  }
}

df_Y_50 <- as.data.frame(df_Y_50)
colnames(df_Y_50) <- colnames(data)
View(df_Y_50)





df_Y_200 <- matrix(NA, nrow = 144, ncol = 137)
index <- 1

for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        filename <- paste0("b2.", b2, ".aux", num_aux, ".mr", mr, ".mt.", missing_type, ".parameter", ".txt")
        data <- read.table(filename, header = TRUE)
        
        df_Y_200[index, 1:4] <- as.numeric(data[1, 1:4])
        df_Y_200[index, 5]   <- as.character(data[1, 5])
        if (ncol(data) > 5) {
          df_Y_200[index, 6:ncol(data)] <- as.numeric(data[1, 6:ncol(data)])
        }
        
        index <- index + 1
      }
    }
  }
}

df_Y_200 <- as.data.frame(df_Y_200)
colnames(df_Y_200) <- colnames(data)
View(df_Y_200)



df_Y_500 <- matrix(NA, nrow = 144, ncol = 137)
index <- 1

for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        filename <- paste0("b2.", b2, ".aux", num_aux, ".mr", mr, ".mt.", missing_type, ".parameter", ".txt")
        data <- read.table(filename, header = TRUE)
        
        df_Y_500[index, 1:4] <- as.numeric(data[1, 1:4])
        df_Y_500[index, 5]   <- as.character(data[1, 5])
        if (ncol(data) > 5) {
          df_Y_500[index, 6:ncol(data)] <- as.numeric(data[1, 6:ncol(data)])
        }
        
        index <- index + 1
      }
    }
  }
}

df_Y_500 <- as.data.frame(df_Y_500)
colnames(df_Y_500) <- colnames(data)
View(df_Y_500)




df_Y_1000 <- matrix(NA, nrow = 144, ncol = 137)
index <- 1

for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        filename <- paste0("b2.", b2, ".aux", num_aux, ".mr", mr, ".mt.", missing_type, ".parameter", ".txt")
        data <- read.table(filename, header = TRUE)
        
        df_Y_1000[index, 1:4] <- as.numeric(data[1, 1:4])
        df_Y_1000[index, 5]   <- as.character(data[1, 5])
        if (ncol(data) > 5) {
          df_Y_1000[index, 6:ncol(data)] <- as.numeric(data[1, 6:ncol(data)])
        }
        
        index <- index + 1
      }
    }
  }
}

df_Y_1000 <- as.data.frame(df_Y_1000)
colnames(df_Y_1000) <- colnames(data)
View(df_Y_1000)



df_Y_all_parameter <- rbind(df_Y_30, df_Y_50, df_Y_200, df_Y_500, df_Y_1000)
# 保证除了第5列外其余全数值，第5列是字符
df_Y_all_parameter[,-5] <- lapply(df_Y_all_parameter[,-5], as.numeric)
df_Y_all_parameter[[5]] <- as.character(df_Y_all_parameter[[5]])
View(df_Y_all_parameter)



save(df_Y_all_parameter, file = "df_Y_all_parameter.RData")

load("df_Y_all_parameter.RData")
