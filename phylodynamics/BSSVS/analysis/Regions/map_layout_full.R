library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
regions <- c("Russia", "North Central", "North East", "North West", 
             "South Central", "South East", "South West")
countries <- list(
  c("Russia"),
  c("Czechia", "Denmark", "Germany", "Lithuania", "Norway", "Poland", "Sweden"),
  c("Estonia", "Finland", "Latvia", "Belarus"),
  c("Belgium", "Ireland", "Iceland", "Luxembourg", "Netherlands", "United Kingdom"),
  c("Albania", "Austria", "Bosnia and Herz.", "Montenegro", "Croatia", 
    "North Macedonia", "Greece", "Hungary", "Italy", "Kosovo", 
    "Slovakia", "Slovenia", "Switzerland"),
  c("Bulgaria", "Cyprus", "Moldova", "Romania", "Serbia", "Ukraine"),
  c("France", "Portugal", "Spain")
)
country_data <- data.frame(
  country = unlist(countries),
  region = rep(regions, sapply(countries, length))
)
europe <- ne_countries(continent = "Europe", returnclass = "sf") %>%
  filter(name != "Russia")
russia <- ne_countries(country = "Russia", returnclass = "sf")
russia <- st_make_valid(russia)
bbox_russia <- st_as_sfc(st_bbox(c(xmin = 30, xmax = 60, ymin = 50, ymax = 70), 
                                 crs = st_crs(russia)))
russia_subset <- st_intersection(russia, bbox_russia)
europe_with_russia <- bind_rows(europe, russia_subset)
europe_data <- europe_with_russia %>%
  left_join(country_data, by = c("name" = "country"))

region_colors <- c(
  "Russia"         = "#F4D6A0", 
  "North Central"  = "#A3D977",
  "North East"     = "#B7E3E0",
  "North West"     = "#E8A8B8",
  "South Central"  = "#F7D560",
  "South East"     = "#C4E8C2",
  "South West"     = "#A6BCD8"
)

ggplot(data = europe_data) +
  geom_sf(aes(fill = factor(region)), color = "black") +
  scale_fill_manual(values = region_colors, name = "Regions") +
  coord_sf(xlim = c(-10, 60), ylim = c(30, 82), expand = FALSE) +
  theme_classic() +
  labs(fill = "Regions") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank()
  )


