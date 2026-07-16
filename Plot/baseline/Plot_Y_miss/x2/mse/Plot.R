# Load necessary libraries
library(reshape2)
library(ggplot2)

# Load data
load("Y_all_parameter.RData")
df <- df_Y_all_parameter
# First, select the columns of interest
mse_columns <- c(
  "mse_x2_fiml", 
  "mse_x2_pmm_default", "mse_x2_pmm_0.2","mse_x2_pmm_manual", 
  "mse_x2_norm_default", "mse_x2_norm_0.2","mse_x2_norm_manual", 
  "mse_x2_cart_default", "mse_x2_cart_0.2","mse_x2_cart_manual"
)


imputation_methods <- c(
  "FIML", 
  "PMM_default", "PMM_0.2", "PMM_manual",
  "norm_default", "norm_0.2", "norm_manual",  # same order
  "CART_default", "CART_0.2", "CART_manual"
)

names(imputation_methods) <- mse_columns

# Ensure 'missing_type' and 'num_aux' are factors with desired levels
df$missing_type[df$missing_type == "Y_MCAR"] <- "MCAR"
df$missing_type[df$missing_type == "Y_MAR_X1"] <- "MAR_x1"
df$missing_type[df$missing_type == "Y_MNAR_Z1"] <- "MAR_z1"
df$missing_type[df$missing_type == "Y_MNAR_Self"] <- "MNAR"

df$missing_type <- factor(df$missing_type, levels = c("MCAR", "MAR_x1", "MAR_z1", "MNAR"))
df$num_aux <- factor(df$num_aux)

# Convert 'mr' to a factor with descriptive labels
df$mr <- factor(df$mr, levels = sort(unique(df$mr)),
                labels = paste0("mr=", sort(unique(df$mr))))

# Convert 'b2' to a factor with descriptive labels
df$b2 <- factor(df$b2, levels = sort(unique(df$b2)),
                labels = paste0("b2=", sort(unique(df$b2))))

# Create a combined factor for 'b2' and 'mr' with line breaks for facet labels
df$b2_mr <- interaction(df$b2, df$mr, sep = "\n")

# Loop over each unique 'N' value
for (N_value in sort(unique(df$N))) {
  # Subset the data for the current 'N' value
  df_N <- subset(df, N == N_value)
  
  # Select the necessary columns
  df_plot <- df_N[, c("missing_type", "num_aux", "b2_mr", mse_columns)]
  
  # Reshape the data to long format
  df_long <- melt(df_plot, id.vars = c("missing_type", "num_aux", "b2_mr"),
                  variable.name = "imputation_method", value.name = "mse")
  
  # Map the mse column names to imputation method names
  df_long$imputation_method <- imputation_methods[as.character(df_long$imputation_method)]
  
  # Ensure 'imputation_method' is a factor with the desired order
  df_long$imputation_method <- factor(df_long$imputation_method, levels = imputation_methods)

  # Use different y-axis ranges to show method differences within each sample size
  y_limits <- if (N_value == 30) {
    c(0, 0.3)
  } else {
    c(0, 0.2)
  }
  
  # Create the plot using ggplot2
  plot <- ggplot(df_long, aes(x = imputation_method, y = mse, fill = num_aux)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.58) +
    scale_fill_manual(
      values = c("#4477AA", "#EEAA33", "#333333"),
      name = "Number of Auxiliary Variables"
    ) +
    coord_cartesian(ylim = y_limits) +
    facet_grid(missing_type ~ b2_mr, labeller = label_value) +  # add scales = "free_y" if want free y-axis
    labs(x = "Imputation Methods",
         y = "MSE_x2") +
    theme_bw() +
    theme(
      plot.title = element_text(size=11, face= "bold", colour= "black" ),
      axis.title.x = element_text(size=11, face="bold", colour = "black", 
                                  margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size=11, face="bold", colour = "black"),
      axis.text.y = element_text(size=11, colour = "black"),
      axis.text.x = element_text(size = 6, angle = 70, hjust = 1, vjust = 1),
      strip.text = element_text(size = 11),
      panel.spacing.y = unit(0.5, "lines"),
      panel.spacing.x = unit(0.35, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.3),
      legend.position = "bottom"       # Move legend to the bottom
    ) +
    guides(fill = guide_legend(nrow = 1))  + # Minimize legend size
    geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth =0.5)
  
  # Print or save the plot
  # print(plot)
  
  # ggsave(filename = paste0("y_mse_x2_n_", N_value, ".pdf"), plot = plot, device = cairo_pdf, units = "in", width = 15, height = 10)
  
  ggsave(filename = paste0("y_mse_x2_n_", N_value, ".png"), plot = plot, units = "in", width = 15, height = 10, dpi = 600)
}
