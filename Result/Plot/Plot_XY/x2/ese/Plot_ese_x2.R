# Load necessary libraries
library(reshape2)
library(ggplot2)

# Assume your data frame is named 'df'
load("df_XY_all_parameter.RData")
df <- df_XY_all_parameter
# First, select the columns of interest
ese_columns <- c(
  "ese_x2_fiml", 
  "ese_x2_pmm_default", "ese_x2_pmm_0.2","ese_x2_pmm_manual",
  "ese_x2_norm_default", "ese_x2_norm_0.2","ese_x2_norm_manual",
  "ese_x2_cart_default", "ese_x2_cart_0.2","ese_x2_cart_manual"
)


# Create a mapping of the ese columns to shorter method names
imputation_methods <- c(
  "FIML", 
  "PMM_default", "PMM_0.2", "PMM_manual",
  "norm_default", "norm_0.2", "norm_manual",  # same order
  "CART_default", "CART_0.2", "CART_manual"
)

names(imputation_methods) <- ese_columns

# Ensure 'missing_type' and 'num_aux' are factors with desired levels
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

# Convert 'mr' to a factor with descriptive labels
df$mr_x <- factor(df$mr_x, levels = sort(unique(df$mr_x)),
                  labels = paste0("mr_x=", sort(unique(df$mr_x))))

# Convert 'b2' to a factor with descriptive labels
df$b2 <- factor(df$b2, levels = sort(unique(df$b2)),
                labels = paste0("b2=", sort(unique(df$b2))))

# Create a combined factor for 'b2' and 'mr' with line breaks for facet labels
df$b2_mr_x <- interaction(df$b2, df$mr_x, sep = "\n")

# Loop over each unique 'N' value
for (N_value in sort(unique(df$N))) {
  for (mr_y_value in sort(unique(df$mr_y))){
    # Subset the data for the current 'N' value
    df_N <- subset(df, N == N_value & mr_y == mr_y_value)
    
    # Select the necessary columns
    df_plot <- df_N[, c("missing_type", "num_aux", "b2_mr_x", ese_columns)]
    
    # Reshape the data to long format
    df_long <- melt(df_plot, id.vars = c("missing_type", "num_aux", "b2_mr_x"),
                    variable.name = "imputation_method", value.name = "ese")
    
    # Map the ese column names to imputation method names
    df_long$imputation_method <- imputation_methods[as.character(df_long$imputation_method)]
    
    # Ensure 'imputation_method' is a factor with the desired order
    df_long$imputation_method <- factor(df_long$imputation_method, levels = imputation_methods)
    
    # Create the plot using ggplot2
    plot <- ggplot(df_long, aes(x = imputation_method, y = ese, color = num_aux, group = num_aux)) +
      geom_line() +
      geom_point() +
      coord_cartesian(ylim=c(0, 0.5)) +   # we need to use the coord_cartesian because it will prevent R to remove the value this is out of bound
      facet_grid(missing_type ~ b2_mr_x, labeller = label_value) +  # add scales = "free_y" if want free y-axis
      labs(title = paste("N =", N_value, "mr_y=", mr_y_value),
           x = "Imputation Method",
           y = "ESE_x2",
           color = "Number of Auxiliary Variables") +
      theme_bw() +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=14, face="bold", colour = "black", 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing = unit(0, "lines"),
        # legend.title = element_blank(),   Remove legend title to save space
        legend.position = "bottom"       # Move legend to the bottom
      ) +
      guides(color = guide_legend(nrow = 1))  + # Minimize legend size
      geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth =0.5)
    
    # Print or save the plot
    print(plot)
    
    # Optionally, save the plot to a file with increased width
    ggsave(filename = paste0("ese_x2_n_", N_value, "_", mr_y_value, ".png"), plot = plot, units = "in", width = 17, height = 16)
  }
}