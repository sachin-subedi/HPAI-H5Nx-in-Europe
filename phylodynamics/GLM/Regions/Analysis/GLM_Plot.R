# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Set working directory and load data
setwd("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/GLM")
data <- read_tsv("GLMSummary.txt")

# Arrange variables to display each "Origin" variable followed by its corresponding "Destination"
data$Variable <- factor(data$Variable, levels = c(
  "Sample Size Origin", "Sample Size Destination", 
  "Case Count Origin", "Case Count Destination", 
  "Anatidae Count Origin", "Anatidae Count Destination",
  "Accipitridae Count Origin", "Accipitridae Count Destination",
  "Laridae Count Origin", "Laridae Count Destination",
  "Products Origin", "Products Destination"
))

# Prepare data for plotting
data <- data %>%
  mutate(
    Significant = case_when(
      CoefLowerHPD > 0 ~ "Positive",             # Statistically supported positive association (red)
      CoefUpperHPD < 0 ~ "Negative",             # Statistically supported negative association (blue)
      TRUE ~ "Non-Significant"                   # No association (gray)
    ),
    AboveThreshold = case_when(
      pp > 0.20 & Significant == "Negative" ~ "AboveNegative",  # For bars above threshold with negative effect
      pp > 0.20 ~ "Above",                                   # For bars above threshold without negative effect
      TRUE ~ "Below"                                         # Below threshold
    )
  )

# Ensure that 'Significant' and 'AboveThreshold' are factors with the correct levels
data$Significant <- factor(data$Significant, levels = c("Positive", "Negative", "Non-Significant"))
data$AboveThreshold <- factor(data$AboveThreshold, levels = c("AboveNegative", "Above", "Below"))

# Define custom colors based on significance and threshold
custom_colors <- c("Positive" = "coral", "Negative" = "cyan", "Non-Significant" = "gray")
threshold_colors <- c("AboveNegative" = "cyan", "Above" = "coral", "Below" = "lightgray")

# Part 1: Plot for Conditional Effect Size with HPD Intervals
effect_size_plot <- ggplot(data, aes(x = CoefMedian, y = Variable)) +
  geom_point(aes(color = Significant, size = pp, fill = Significant), shape = 21) +  # Shape 21 allows filling
  geom_errorbarh(aes(xmin = CoefLowerHPD, xmax = CoefUpperHPD), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(
    x = "Conditional Effect Size", 
    y = "",
    title = "Regions-Predictors of 2.3.4.4b H5Nx HPAI in Europe"
  ) +
  theme_minimal() +
  theme(
    text             = element_text(family = "Times New Roman"),
    legend.position  = "none",
    # Larger, bold, black axis labels:
    axis.title.x     = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y     = element_text(size = 14, face = "bold", color = "black"),
    # Make axis text also black and slightly larger:
    axis.text.x      = element_text(size = 12, color = "black"),
    axis.text.y      = element_text(size = 12, color = "black"),
    # Optionally make the title larger/bolder as well:
    plot.title       = element_text(size = 14, face = "bold", color = "black", hjust = 0.5)
  )

# Part 2: Plot for Posterior Probability with additional BF = 3 and BF = 100 threshold lines
posterior_prob_plot <- ggplot(data, aes(x = pp, y = Variable, fill = AboveThreshold)) +
  geom_col(color = "black", width = 0.6) +
  # Keep only the dashed line at 0.20 if that is your desired reference
  geom_vline(xintercept = 0.20, linetype = "dashed", color = "darkblue") +
  
  # REMOVE or comment out the BF annotations:
  # annotate("text", x = 0.18, y = max(as.numeric(data$Variable)) + 0.5, 
  #          label = "BF = 3", color = "black", size = 3, hjust = 0.5) +
  # annotate("text", x = 0.97, y = max(as.numeric(data$Variable)) + 0.5, 
  #          label = "100", color = "black", size = 3, hjust = 0.5) +
  
  # If you no longer want the second dashed line:
  # geom_vline(xintercept = 1.0, linetype = "dashed", color = "darkblue") +
  
  # Turn on the legend for fill. Adjust the name and labels as needed:
scale_fill_manual(
  name   = "Threshold",
  values = threshold_colors,
  labels = c("AboveNegative" = "Above (Negative)",
             "Above"         = "Above",
             "Below"         = "Below")
) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    x = "Posterior Probability",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text              = element_text(family = "Times New Roman"),
    legend.position   = "right",
    # We keep the y-axis text blank as in the original code:
    axis.text.y       = element_blank(),
    # Larger, bold, black x-axis label:
    axis.title.x      = element_text(size = 14, face = "bold", color = "black"),
    # Make x-axis text black and reasonably sized:
    axis.text.x       = element_text(size = 12, color = "black")
  )

# Arrange plots side-by-side
grid.arrange(effect_size_plot, posterior_prob_plot, ncol = 2)

