library(ape)
library(treeio)
library(ggtree)
library(ggplot2)
library(dplyr)

options(ignore.negative.edge = TRUE)

### Markov Jumps tree
### Habitat tree
both_mj_tree <- read.beast("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/HR_mj_combined_masked.tree")
p <- ggtree(both_mj_tree)
tree_data <- p$data

colnames(tree_data)

print(tree_data)


# Ladderize the tree for clarity
both_mj_tree_ladderized <- ladderize(both_mj_tree@phylo, right = FALSE)

habitat_full_names <- c(
  "FW" = "Freshwater Wetlands", 
  "HH"  = "Human",
  "MM" = "Mammals",
  "PO" = "Poultry",
  "SB" = "Seabirds",
  "TFO" = "Terrestial Forest Open"
)
# Filter for tips with Habitat information only
habitat_tree_data <- tree_data %>%
  mutate(Habitat = recode(Habitat, !!!habitat_full_names))

habitat_state_colors_full <- c(
  "Freshwater Wetlands" = "#F4D6A0", 
  "Human"  = "#A3D977",
  "Mammals" = "#B7E3E0",
  "Poultry" = "#E8A8B8",
  "Seabirds" = "#C4E8C2",
  "Terrestial Forest Open" = "#A6BCD8"
)

# Create the tree with circular points and no tip labels
habitat_both_mj <- ggtree(both_mj_tree_ladderized, layout = "rectangular", mrsd = "2024-11-12") %<+% habitat_tree_data +
  geom_tree(color = "black", size = 1) +  # Gray tree with thinner branches
  geom_tippoint(aes(color = Habitat), size = 4) +  # Circular points at tips
  theme_tree2() +
  scale_color_manual(values = habitat_state_colors_full) +  # Apply colors to full names
  scale_x_continuous(
    breaks = seq(2004, 2024, 2),
    labels = seq(2004, 2024, 2)
  ) +
  labs(x = "Year", y = "", color = "Habitat") +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(),    # Keep axis lines
    axis.text.y = element_blank(), # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    legend.position = "right",     # Place legend on the right
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_text(size = 16, face = "bold")# Add plot margins
  )

# Display the final plot
habitat_both_mj

ggsave(
  filename = "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/habitat_both_mj_masked.png",
  plot = habitat_both_mj,
  width = 14,
  height =20,
  dpi = 300
)


### Regions

Regions_full_names <- c(
  "CA" = "Russia", 
  "NC"  = "North Central",
  "NE" = "North East",
  "NW" = "North West",
  "SC" = "South Central",
  "SE" = "South East",
  "SW" = "South West"
)
# Filter for tips with Regions information only
Regions_tree_data <- tree_data %>%
  mutate(Regions = recode(Regions, !!!Regions_full_names))

Regions_state_colors_full <- c(
  "Russia"         = "#F4D6A0", 
  "North Central"  = "#A3D977",
  "North East"     = "#B7E3E0",
  "North West"     = "#E8A8B8",
  "South Central"  = "#F7D560",
  "South East"     = "#C4E8C2",
  "South West"     = "#A6BCD8"
)


# Create the tree with circular points and no tip labels
Regions_both_mj <- ggtree(both_mj_tree_ladderized, layout = "rectangular", mrsd = "2024-11-12") %<+% Regions_tree_data +
  geom_tree(color = "black", size = 1) +  # Gray tree with thinner branches
  geom_tippoint(aes(color = Regions), size = 4) +  # Circular points at tips
  theme_tree2() +
  scale_color_manual(values = Regions_state_colors_full) +  # Apply colors to full names
  scale_x_continuous(
    breaks = seq(2004, 2024, 2),
    labels = seq(2004, 2024, 2)
  ) +
  labs(x = "Year", y = "", color = "Regions") +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(),    # Keep axis lines
    axis.text.y = element_blank(), # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    legend.position = "right",     # Place legend on the right
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_text(size = 16, face = "bold")# Add plot margins
  )

# Display the final plot
Regions_both_mj

ggsave(
  filename = "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/regions_both_mj_masked.png",
  plot = Regions_both_mj,
  width = 14,
  height =20,
  dpi = 300
)

