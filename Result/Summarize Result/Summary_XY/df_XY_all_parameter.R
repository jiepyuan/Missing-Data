# N = 30
example_file <- "b2.0.1.aux5.mry0.15.mrx0.1.mt.Y_MCAR_XZ_MCAR.parameter.txt"
example_data <- read.table(example_file, header = TRUE)
col_names <- colnames(example_data)

df_XY_30 <- data.frame(matrix(NA, nrow = 432, ncol = length(col_names)))
colnames(df_XY_30) <- col_names

for (i in c(1:5, 7:ncol(df_XY_30))) {
  df_XY_30[[i]] <- as.numeric(df_XY_30[[i]])
}
df_XY_30[[6]] <- as.character(df_XY_30[[6]])  # missing_type


index <- 1

for (b2 in c(0.1, 0.37, 0.5)) {
  for (num_aux in c(5, 20, 50)) {
    for (mr_y in c(0.15, 0.30, 0.60)) {
      for (mr_x in c(0.1, 0.3)) {
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
          
          filename <- paste0("b2.", b2, ".aux", num_aux,
                             ".mry", mr_y, ".mrx", mr_x,
                             ".mt.", missing_type, ".parameter", ".txt")
          
          data <- read.table(filename, header = TRUE)
          
          df_XY_30[index, 1:5] <- as.numeric(data[1, 1:5])
          df_XY_30[index, 6]   <- as.character(data[1, 6])
          if (ncol(data) > 6) {
            df_XY_30[index, 7:ncol(data)] <- as.numeric(data[1, 7:ncol(data)])
          }
          
          index <- index + 1
        }
      }
    }
  }
}



# N = 50
example_file <- "b2.0.1.aux5.mry0.15.mrx0.1.mt.Y_MCAR_XZ_MCAR.parameter.txt"
example_data <- read.table(example_file, header = TRUE)
col_names <- colnames(example_data)

df_XY_50 <- data.frame(matrix(NA, nrow = 432, ncol = length(col_names)))
colnames(df_XY_50) <- col_names

for (i in c(1:5, 7:ncol(df_XY_50))) df_XY_50[[i]] <- as.numeric(df_XY_50[[i]])
df_XY_50[[6]] <- as.character(df_XY_50[[6]])

index <- 1
for (b2 in c(0.1, 0.37, 0.5)) {
  for (num_aux in c(5, 20, 50)) {
    for (mr_y in c(0.15, 0.30, 0.60)) {
      for (mr_x in c(0.1, 0.3)) {
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
          
          filename <- paste0("b2.", b2, ".aux", num_aux, ".mry", mr_y, ".mrx", mr_x,
                             ".mt.", missing_type, ".parameter", ".txt")
          
          data <- read.table(filename, header = TRUE)
          df_XY_50[index, 1:5] <- as.numeric(data[1, 1:5])
          df_XY_50[index, 6] <- as.character(data[1, 6])
          if (ncol(data) > 6) df_XY_50[index, 7:ncol(data)] <- as.numeric(data[1, 7:ncol(data)])
          
          index <- index + 1
        }
      }
    }
  }
}


# N = 200
df_XY_200 <- data.frame(matrix(NA, nrow = 432, ncol = length(col_names)))
colnames(df_XY_200) <- col_names

for (i in c(1:5, 7:ncol(df_XY_200))) df_XY_200[[i]] <- as.numeric(df_XY_200[[i]])
df_XY_200[[6]] <- as.character(df_XY_200[[6]])

index <- 1
for (b2 in c(0.1, 0.37, 0.5)) {
  for (num_aux in c(5, 20, 50)) {
    for (mr_y in c(0.15, 0.30, 0.60)) {
      for (mr_x in c(0.1, 0.3)) {
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
          
          filename <- paste0("b2.", b2, ".aux", num_aux, ".mry", mr_y, ".mrx", mr_x,
                             ".mt.", missing_type, ".parameter", ".txt")
          
          data <- read.table(filename, header = TRUE)
          df_XY_200[index, 1:5] <- as.numeric(data[1, 1:5])
          df_XY_200[index, 6] <- as.character(data[1, 6])
          if (ncol(data) > 6) df_XY_200[index, 7:ncol(data)] <- as.numeric(data[1, 7:ncol(data)])
          
          index <- index + 1
        }
      }
    }
  }
}




# N = 500
df_XY_500 <- data.frame(matrix(NA, nrow = 432, ncol = length(col_names)))
colnames(df_XY_500) <- col_names

for (i in c(1:5, 7:ncol(df_XY_500))) df_XY_500[[i]] <- as.numeric(df_XY_500[[i]])
df_XY_500[[6]] <- as.character(df_XY_500[[6]])

index <- 1
for (b2 in c(0.1, 0.37, 0.5)) {
  for (num_aux in c(5, 20, 50)) {
    for (mr_y in c(0.15, 0.30, 0.60)) {
      for (mr_x in c(0.1, 0.3)) {
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
          
          filename <- paste0("b2.", b2, ".aux", num_aux, ".mry", mr_y, ".mrx", mr_x,
                             ".mt.", missing_type, ".parameter", ".txt")
          
          data <- read.table(filename, header = TRUE)
          df_XY_500[index, 1:5] <- as.numeric(data[1, 1:5])
          df_XY_500[index, 6] <- as.character(data[1, 6])
          if (ncol(data) > 6) df_XY_500[index, 7:ncol(data)] <- as.numeric(data[1, 7:ncol(data)])
          
          index <- index + 1
        }
      }
    }
  }
}




# N = 1000
df_XY_1000 <- data.frame(matrix(NA, nrow = 432, ncol = length(col_names)))
colnames(df_XY_1000) <- col_names

for (i in c(1:5, 7:ncol(df_XY_1000))) df_XY_1000[[i]] <- as.numeric(df_XY_1000[[i]])
df_XY_1000[[6]] <- as.character(df_XY_1000[[6]])

index <- 1
for (b2 in c(0.1, 0.37, 0.5)) {
  for (num_aux in c(5, 20, 50)) {
    for (mr_y in c(0.15, 0.30, 0.60)) {
      for (mr_x in c(0.1, 0.3)) {
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")) {
          
          filename <- paste0("b2.", b2, ".aux", num_aux, ".mry", mr_y, ".mrx", mr_x,
                             ".mt.", missing_type, ".parameter", ".txt")
          
          data <- read.table(filename, header = TRUE)
          df_XY_1000[index, 1:5] <- as.numeric(data[1, 1:5])
          df_XY_1000[index, 6] <- as.character(data[1, 6])
          if (ncol(data) > 6) df_XY_1000[index, 7:ncol(data)] <- as.numeric(data[1, 7:ncol(data)])
          
          index <- index + 1
        }
      }
    }
  }
}



df_XY_all_parameter <- rbind(df_XY_30, df_XY_50, df_XY_200, df_XY_500, df_XY_1000)

save(df_XY_all_parameter, file = "df_XY_all_parameter.RData")

load("df_XY_all_parameter.RData")
