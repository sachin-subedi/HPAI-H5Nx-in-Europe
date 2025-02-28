library(dplyr)

# Load the case data
case_data <- read_csv("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/datasets/GLM/Habitat/case_counts.csv")
colnames(case_data)

unique_species <- unique(case_data$Species)
print(unique_species)


europe_case_data <- case_data %>%
  filter(Region %in% c("Europe"))

library(dplyr)
library(stringr)

case_data_cleaned <- europe_case_data %>%
  mutate(Cleaned_Species = str_remove(Species, "^(Wild|Domestic|Captive),?\\s*")) %>%
  mutate(Cleaned_Species = case_when(
    str_detect(Cleaned_Species, "^Unspecified bird") ~ Cleaned_Species,  # Keep as is
    str_detect(Cleaned_Species, "^[^,]+$") ~ Cleaned_Species,  # Keep if single term before comma
    TRUE ~ str_extract(Cleaned_Species, "^[^:(,]+")  # Extract text before the first colon, parenthesis, or comma
  ))

# Add new column with last word extracted
case_data_cleaned <- case_data_cleaned %>%
  mutate(
    Cleaned_Species = str_trim(Cleaned_Species),  # Remove trailing spaces
    Last_Word = ifelse(
      str_detect(Cleaned_Species, "\\s"), 
      word(Cleaned_Species, -1),  # Extract last word
      Cleaned_Species  # Keep single-word entries unchanged
    )
  )

library(stringr)
library(dplyr)

# Define habitat groups with lowercase values
MM <- tolower(c("Mink", "Fox", "Racoon", "Dog", "Lynx", "Caracal", "Polecat", "Marten", "Ferret", 
                "Badger", "Bear", "Otter", "Walrus", "Seal", "Porpoise", "Dolphin"))
FW <- tolower(c("Goose", "Teal", "Swan", "Coot", "Mallard", "Duck", "Wigeon", "Gadwall",  
                "Heron", "Egret", "Stork", "Lapwing", "Crane", "Flamingo", "Grebe", 
                "Moorhen", "Red Knot", "Sanderling", "Curlew", "Spoonbill", "Anatidae"))
SB <- tolower(c("Fulmar", "Skua", "Gannet", "Murre", "Auk", "Gull", "Tern", "Guillemot", 
                "Razorbill", "Oystercatcher", "Cormorant", "Pelican", "Penguin", "Laridae", "Kittiwake"))
PO <- tolower(c("Chicken", "Quail", "Turkey", "Hen", "Muscovy", "Peking", "Breeder"))
TFO <- tolower(c("Eagle", "Vulture", "Buzzard", "Hawk", "Harrier", "Warbler", "Pigeon",  
                 "Raven", "Crow", "Jackdaw", "Magpie", "Rook", "Kestrel", "Falcon", "Fowl",  
                 "Sparrow", "Pheasant", "Peacock", "Peafowl", "Rhea", "Owl", "Thrush", 
                 "Colchicus", "Sparrowhawk", "Falcon"))

# Assign habitat categories with case insensitivity
case_data_cleaned <- case_data_cleaned %>%
  mutate(
    Last_Word = tolower(Last_Word),  # Convert Last_Word to lowercase
    Habitat = case_when(
      Last_Word %in% MM ~ "MM",
      Last_Word %in% FW ~ "FW",
      Last_Word %in% SB ~ "SB",
      Last_Word %in% PO ~ "PO",
      Last_Word %in% TFO ~ "TFO",
      TRUE ~ "Other"  # For species not classified
    )
  )

# Display a preview of the dataset with the new column
head(case_data_cleaned)

# Save the cleaned European case data as a CSV file
write_csv(case_data_cleaned, "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/datasets/GLM/Habitat/europe_case_data_cleaned.csv")


case_data1 <- read_excel("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/datasets/GLM/Habitat/europe_case_data_cleaned_again.xlsx")
colnames(case_data1)

library(dplyr)

# Count Event ID occurrences by Habitat
event_count_by_habitat <- case_data1 %>%
  group_by(Habitat) %>%
  summarise(Event_Count = n_distinct(`Event ID`)) %>%
  arrange(desc(Event_Count))  # Sort in descending order

# Display the count
print(event_count_by_habitat)
