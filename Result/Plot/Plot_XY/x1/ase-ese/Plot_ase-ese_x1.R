# Load necessary libraries
library(reshape2)
library(ggplot2)

# Load the data frame
load("df_XY_all_parameter.RData")
df <- df_XY_all_parameter

# Define ASE columns and corresponding ESE columns
ase_columns <- c(
  "ase_x1_fiml", 
  "ase_x1_pmm_default", "ase_x1_pmm_0.2", "ase_x1_pmm_manual", 
  "ase_x1_norm_default", "ase_x1_norm_0.2","ase_x1_norm_manual",
  "ase_x1_cart_default", "ase_x1_cart_0.2","ase_x1_cart_manual"
)

ese_columns <- c(
  "ese_x1_fiml", 
  "ese_x1_pmm_default", "ese_x1_pmm_0.2","ese_x1_pmm_manual",
  "ese_x1_norm_default", "ese_x1_norm_0.2","ese_x1_norm_manual",
  "ese_x1_cart_default", "ese_x1_cart_0.2","ese_x1_cart_manual"
)

# Create a mapping of ASE columns to shorter imputation method names
imputation_methods <- c(
  "FIML", 
  "PMM_default", "PMM_0.2", "PMM_manual",
  "norm_default", "norm_0.2", "norm_manual",  # same order
  "CART_default", "CART_0.2", "CART_manual"
)
names(imputation_methods) <- ase_columns

# Create new difference column names (ASE - ESE)
dif_columns <- paste0("dif_", imputation_methods)
# For example, the difference for listwise is stored in "dif_listwise"

# Compute the difference columns one by one
for (i in seq_along(ase_columns)) {
  ase_col <- ase_columns[i]
  ese_col <- ese_columns[i]
  diff_col <- dif_columns[i]
  
  df[[diff_col]] <- df[[ase_col]] - df[[ese_col]]
}

# Ensure 'missing_type' and 'num_aux' are factors with the desired levels
df$missing_type[df$missing_type == "Y_MCAR_XZ_MCAR"] <- "y_MCAR_xz_MCAR"
df$missing_type[df$missing_type == "Y_MCAR_XZ_MNAR"] <- "y_MCAR_xz_MNAR"


df$missing_type[df$missing_type == "Y_MAR_X1_XZ_MCAR"] <- "y_MAR_x1_xz_MCAR"
df$missing_type[df$missing_type == "Y_MAR_X1_XZ_MNAR"] <- "y_MAR_x1_xz_MNAR"

df$missing_type[df$missing_type == "Y_MNAR_Z1_XZ_MCAR"] <- "y_MAR_z1_xz_MCAR"
df$missing_type[df$missing_type == "Y_MNAR_Z1_XZ_MNAR"] <- "y_MAR_z1_xz_MNAR"

df$missing_type[df$missing_type == "Y_MNAR_Self_XZ_MCAR"] <- "y_MNAR_xz_MCAR"
df$missing_type[df$missing_type == "Y_MNAR_Self_XZ_MNAR"] <- "y_MNAR_xz_MNAR"


df$missing_type <- factor(df$missing_type, levels = c("y_MCAR_xz_MCAR", "y_MCAR_xz_MNAR",
                                                      "y_MAR_x1_xz_MCAR", "y_MAR_x1_xz_MNAR",
                                                      "y_MAR_z1_xz_MCAR", "y_MAR_z1_xz_MNAR",
                                                      "y_MNAR_xz_MCAR", "y_MNAR_xz_MNAR"))
df$num_aux <- factor(df$num_aux)

# Convert 'mr_x' and 'b2' to factors with descriptive labels
df$mr_x <- factor(df$mr_x, levels = sort(unique(df$mr_x)),
                  labels = paste0("mr_x=", sort(unique(df$mr_x))))
df$b2 <- factor(df$b2, levels = sort(unique(df$b2)),
                labels = paste0("b2=", sort(unique(df$b2))))

# Create a combined factor for 'b2' and 'mr_x' with line breaks for facet labels
df$b2_mr_x <- interaction(df$b2, df$mr_x, sep = "\n")

# Loop over each unique 'N' value and each unique 'mr_y' value
for (N_value in sort(unique(df$N))) {
  for (mr_y_value in sort(unique(df$mr_y))) {
    # Subset the data for the current N and mr_y values
    df_sub <- subset(df, N == N_value & mr_y == mr_y_value)
    
    # Select the necessary columns, including the newly computed difference columns
    df_plot <- df_sub[, c("missing_type", "num_aux", "b2_mr_x", dif_columns)]
    
    # Reshape the data to long format
    df_long <- melt(df_plot, id.vars = c("missing_type", "num_aux", "b2_mr_x"),
                    variable.name = "imputation_method", value.name = "difference")
    
    # Map the difference column names (e.g., "dif_listwise") to the imputation method names (e.g., "listwise")
    imputation_methods_diff <- setNames(imputation_methods, dif_columns)
    df_long$imputation_method <- imputation_methods_diff[as.character(df_long$imputation_method)]
    
    # Ensure 'imputation_method' is a factor with the desired order
    df_long$imputation_method <- factor(df_long$imputation_method, levels = imputation_methods)
    
    # Create the plot using ggplot2
    plot <- ggplot(df_long, aes(x = imputation_method, y = difference, color = num_aux, group = num_aux)) +
      geom_line() +
      geom_point() +
      # Adjust the y-axis limits as needed (here set to allow negative values)
      coord_cartesian(ylim = c(-0.20, 0.20)) +
      facet_grid(missing_type ~ b2_mr_x, labeller = label_value) +
      labs(title = paste("N =", N_value, "mr_y =", mr_y_value),
           x = "Imputation Method",
           y = "ASE - ESE_x1",
           color = "Number of Auxiliary Variables") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 14, face = "bold", colour = "black",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.y = element_text(size = 9, colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing = unit(0, "lines"),
        legend.position = "bottom"
      ) +
      guides(color = guide_legend(nrow = 1)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
    
    # Print the plot
    print(plot)
    
    # Optionally, save the plot to a file
    ggsave(filename = paste0("ase_ese_diff_n_", N_value, "_mr_y_", mr_y_value, "_x1", ".png"),
           plot = plot, units = "in", width = 17, height = 16)
  }
}
