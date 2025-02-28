# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Step 0: Read in data (adjust path as needed)
jumps <- read.delim(
  "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Regions_jumpTimes.txt",
  sep = '', 
  header = TRUE
)
state <- n_distinct(jumps$state) #Number of total state counts
jumps$from_to = paste(jumps$from,jumps$to, sep=".")
jumps <- jumps %>% mutate(time = 2024.863387978142 - as.numeric(time))
jumps$year <- format(date_decimal(jumps$time), "%Y")

#From, To, Year
count<-jumps %>% group_by(from_to,year)%>% count()
count2<-cbind(count, read.table(text = as.character(count$from_to), sep = "."))

# Check the structure of the 'jumps' dataset
if (!"from_to" %in% names(jumps)) {
  stop("The 'from_to' column is not present in the 'jumps' dataset.")
}

# Summarize transitions from "From" to "To"
count_total <- jumps %>% 
  group_by(from_to) %>% 
  count(name = "n") # Explicitly name the count column as 'n'

# Split the "from_to" column into separate "From" and "To" columns
count_total <- count_total %>% 
  separate(from_to, into = c("From", "To"), sep = "\\.")

# Ensure "From" and "To" contain only the specified state codes
valid_states <- c("CA", "NC", "NE", "NW", "SC", "SE", "SW")

count_total <- count_total %>%
  filter(From %in% valid_states & To %in% valid_states)

# Summarize the total counts for each unique "From" and "To" pair
host <- count_total %>%
  group_by(From, To) %>%
  summarize(total = sum(n), .groups = "drop")

# Final result
host



#count_total$from_to = paste(count_total$F,count_total$T, sep=".")
#host <- count_total %>%
#  group_by(from_to) %>%
#  summarize(total = sum(n))
#host <- host %>% mutate(ave=total/state)

full_names <- c(
  "CA" = "Russia", 
  "NC"  = "North Central",
  "NE" = "North East",
  "NW" = "North West",
  "SC" = "South Central",
  "SE" = "South East",
  "SW" = "South West"
)
count_ave <- count_total %>%
  mutate(From = full_names[From],
         To = full_names[To],
         ave = n/state)
summary(count_ave$ave)
avg_regions_jumps <- ggplot(count_ave, aes(x=To, y=From, fill= ave)) +
  theme_bw()+
  scale_fill_gradient2(low = 'aliceblue', mid = 'lightblue', high = 'royalblue', midpoint = 0.70904, limits = c(0, 130))+
  xlab("Sink") +
  ylab("Source") +
  ggtitle("Average Markov Jump Counts") +
  geom_tile() +
  guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increased font size
        axis.text.y = element_text(angle = 45, hjust = 1, size = 14),  # Increased font size for y-axis as well
        axis.title.x = element_text(size = 16),  # Increased font size for x-axis title
        axis.title.y = element_text(size = 16),  # Increased font size for y-axis title
        plot.title = element_text(size = 18, face = "bold"))  # Larger and bold plot title


avg_regions_jumps

ggsave(
  filename = "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/MarkovJumps/Combined/masked/Average_Regions_Jumps.png",
  plot = avg_regions_jumps,
  width = 14,
  height = 9,
  dpi = 300
)

