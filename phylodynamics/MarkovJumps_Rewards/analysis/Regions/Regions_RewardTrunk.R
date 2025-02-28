library(ggplot2)
library(dplyr)
library(ggtree)
require(treeio)
library(tidytree)
library(reshape2)
library(tidyr)
library(ggnewscale)
library(RColorBrewer)
library(lubridate)
library(MASS)

Regions_tree <- read.beast("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Regions_complete_mj_masked.tree")

Regions_data <-as_tibble(Regions_tree)
## filter data
df <- Regions_data %>% dplyr::select(ends_with('reward'), height)


# input date of earliest seq
df <- df %>% mutate(year = 2024.863387978142 - as.numeric(height))
## get the propotrion of rewards
df = as.data.frame(sapply(df, as.numeric))

# Aggregate the rewards for all states by year
states <- c("CA", "NC", "NE", "NW", "SC", "SE", "SW")

# Dynamically create column names for rewards
reward_columns <- paste0(states, "_reward")

# Aggregate rewards for all states by year
Reward <- aggregate(cbind(df[, reward_columns]), by = list(year = df$year), FUN = sum)

# Calculate the sum of rewards across all states for each year
Reward <- Reward %>%
  mutate(sum = rowSums(across(all_of(reward_columns))))

# Calculate proportions for each state
for (state in states) {
  reward_col <- paste0(state, "_reward")
  proportion_col <- paste0(state, "_reward_p")
  Reward <- Reward %>%
    mutate(!!proportion_col := .data[[reward_col]] / sum)
}

# View the results
head(Reward)

# Filter out rows with sum = 0
Reward <- Reward %>% filter(sum != 0)

# Replace NA values with 0
Reward_all <- Reward %>% mutate_all(~ replace_na(., 0))

# Select proportion columns and the year
Reward_all <- Reward %>% dplyr::select(ends_with('reward_p'), year)

# Gather all states into a single column
Reward_all1 <- Reward_all %>% gather(location, value, -year)

# Save and reload for compatibility (optional, adjust if needed)
write.csv(Reward_all1, "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Reward_all1.csv", row.names = FALSE)
Reward_final <- read.csv("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Reward_all1.csv")
Reward_final <- as.data.frame(Reward_final)

# Define interval width for aggregation
interval_width <- 1/12 # one week

# Create intervals based on year
Reward_final$interval <- cut(Reward_final$year, 
                             breaks = seq(min(Reward_final$year), max(Reward_final$year) + interval_width, by = interval_width))

# Aggregate values within intervals for each location
final_result <- aggregate(value ~ location + interval, data = Reward_final, FUN = mean)

# Extract the upper bounds of intervals for plotting
final_result$interval_upper <- as.numeric(gsub('.*,(\\d+\\.\\d+).*', '\\1', final_result$interval))
final_result$location <- toupper(final_result$location)

colors <- c(
  "CA_REWARD_P" = "#F4D6A0",  # Warm Sand
  "NC_REWARD_P" = "#A3D977",  # Fresh Green
  "NE_REWARD_P" = "#B7E3E0",  # Soft Aqua
  "NW_REWARD_P" = "#E8A8B8",  # Subtle Rose
  "SC_REWARD_P" = "#F7D560",  # Golden Meadow
  "SE_REWARD_P" = "#C4E8C2",  # Light Mint
  "SW_REWARD_P" = "#A6BCD8"   # Sky Blue
)


# Define full names for the legend
full_names <- c(
  "CA_REWARD_P" = "Russia", 
  "NC_REWARD_P"  = "North Central",
  "NE_REWARD_P" = "North East",
  "NW_REWARD_P" = "North West",
  "SC_REWARD_P" = "South Central",
  "SE_REWARD_P" = "South East",
  "SW_REWARD_P" = "South West"
)


# Markov rewards trunk proportion showing the waiting time for a given status across branches of the phylogeny over time. 
# 2008-2024
Regions_rewards_1024 <- ggplot(final_result, aes(x = interval_upper, y = value, fill = location)) +
  geom_area() +
  xlab("Year") +
  ylab("Reward Proportion") +
  labs(fill = "Location", title = "Reward Proportion by Regions") +
  scale_x_continuous(
    limits = c(2011, 2025),
    breaks = seq(2011, 2025, by = 2),  # Yearly breaks
    labels = seq(2011, 2025, by = 2),  # Display each year
    expand = c(0.01, 0)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    legend.title = element_blank(),
    axis.ticks = element_line(),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  scale_fill_manual(values = colors, labels = full_names)

# Display the chart
Regions_rewards_1024

# Save the updated plot
ggsave(
  filename = "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Regions_Reward_Proportion_1024.png",
  plot = Regions_rewards_1024,
  width = 14,
  height = 8,
  dpi = 300
)

# 2016-2025
Regions_rewards_1625 <- ggplot(final_result, aes(x = interval_upper, y = value, fill = location)) +
  geom_area() +
  xlab("Year") +
  ylab("Reward Proportion") +
  labs(fill = "Location", title = "Reward Proportion by Regions") +
  scale_x_continuous(
    limits = c(2016, 2025),
    breaks = seq(2016, 2025, by = 1),  # Yearly breaks
    labels = seq(2016, 2025, by = 1),  # Display each year
    expand = c(0.01, 0)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    legend.title = element_blank(),
    axis.ticks = element_line(),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  scale_fill_manual(values = colors, labels = full_names)

# Display the chart
Regions_rewards_1625
ggsave(
  filename = "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Regions_Reward_Proportion_1624.png",
  plot = Regions_rewards_1625,
  width = 14,
  height = 8,
  dpi = 300
)

