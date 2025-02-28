# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)

setwd("~/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/BSSVS/Regions")
# Load necessary libraries
# Define the regions
regions <- c("Russia", "North Central", "North East", "North West",
             "South Central", "South East", "South West")

# Create a directory to store the output maps
output_dir <- "region_maps"
dir.create(output_dir, showWarnings = FALSE)

# Generate maps for each region
for (region in regions) {
  region_data <- europe_data %>% filter(zone == region)
  
  # Plot for the current region
  p <- ggplot(region_data) +
    geom_sf(aes(fill = factor(zone)), color = "black") +
    scale_fill_manual(values = region_pal_light) +
    
    # Ensure a pure white background
    theme_void() +  
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "none") +
    ggtitle(region)
  
  # Save each plot
  ggsave(filename = file.path(output_dir, paste0(region, "_map.png")),
         plot = p, width = 6, height = 5, dpi = 300)
}


