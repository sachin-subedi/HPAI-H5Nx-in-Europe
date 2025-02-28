###############################################################################
# Load libraries
###############################################################################
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)         # For fill(.)
library(rnaturalearth)
library(RColorBrewer)
library(grid)          # For arrow() customization
library(lwgeom)        # Needed for some geometry transforms

###############################################################################
# 2. Define zones & countries (as before)
###############################################################################
zone <- c("Russia","North Central", "North East", "North West", 
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
  Country = unlist(countries),
  zone = rep(zone, sapply(countries, length))
)

###############################################################################
# 3. Fetch Europe + Russia shapefiles & clip Russia
###############################################################################
europe <- ne_countries(continent = "Europe", returnclass = "sf") %>%
  filter(name != "Russia")
russia <- ne_countries(country = "Russia", returnclass = "sf")

russia <- st_make_valid(russia)

# Clip Russia to bounding box so we only keep part near Europe
bbox_russia <- st_as_sfc(st_bbox(c(xmin = 30, xmax = 60, ymin = 50, ymax = 70), 
                                 crs = st_crs(russia)))
bbox_russia <- st_transform(bbox_russia, st_crs(russia))
russia_subset <- st_intersection(russia, bbox_russia)

if (nrow(russia_subset) == 0) {
  stop("Intersection failed: No valid portion of Russia extracted.")
}

# Ensure CRS compatibility
russia_subset <- st_transform(russia_subset, st_crs(europe))

# Combine Russia subset with Europe
europe_with_russia <- bind_rows(europe, russia_subset)

###############################################################################
# 5. Join zone info to the new map data
###############################################################################
europe_data <- europe_with_russia %>%
  left_join(country_data, by = c("name" = "Country"))

###############################################################################
# 6. Manual color palette for each zone
###############################################################################
region_pal <- c(
  "Russia"         = "#E64B35",  # example color
  "North Central"  = "#4DBBD5",
  "North East"     = "#00A087",
  "North West"     = "#3C5488",
  "South Central"  = "lightgreen",
  "South East"     = "#7E6148",
  "South West"     = "#A2A2A2"
)

###############################################################################
# 7. Approximate coordinates for zone “centers”
###############################################################################
zone_centers <- data.frame(
  zone = c("North Central", "South West", "North West", 
           "South East", "South Central", "Russia", "North East"),
  x = c(10, -4, -4, 30, 13, 45, 25),
  y = c(60, 40, 55, 48, 42, 57, 62)
)

###############################################################################
# 8. Example arrow data + BF categories
###############################################################################
arrows <- data.frame(
  from = c("Russia", "North Central", "North Central", "North Central", 
           "North Central", "North West", "South Central", "North Central", 
           "North West", "South Central", "South West"),
  to = c("North East", "North East", "North West", "South Central", 
         "South West", "South West", "South East", "Russia", 
         "North Central", "North Central", "South Central"),
  bf = c(150, 85000, 85000, 85000, 179, 85000, 
         1732, 85000, 85000, 3386, 132)
)
arrows <- arrows %>%
  mutate(bf_category = ifelse(bf < 1000, "Strong", "Decisive"))

# Merge arrow data with zone centers
arrow_data <- arrows %>%
  left_join(zone_centers, by = c("from" = "zone")) %>%
  rename(x_start = x, y_start = y) %>%
  left_join(zone_centers, by = c("to" = "zone")) %>%
  rename(x_end = x, y_end = y) %>%
  fill(c(x_start, y_start, x_end, y_end), .direction = "down")

###############################################################################
# 9. Plot
###############################################################################
ggplot(data = europe_data) +
  geom_sf(aes(fill = factor(zone))) +
  scale_fill_manual(values = region_pal, name = "Regions") +  # Use original colors
  
  # Make sure Russia is visible
  coord_sf(xlim = c(-10, 60), ylim = c(30, 82), expand = FALSE) +
  
  # Arrows
  geom_curve(
    data = arrow_data,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        size = bf_category, color = bf_category),
    arrow = arrow(type = "open", length = unit(0.15, "inches")),
    curvature = 0.3,
    show.legend = TRUE
  ) +
  scale_size_manual(
    values = c("Strong" = 0.5, "Decisive" = 2), 
    name = "Bayes Factor"
  ) +
  scale_color_manual(
    values = c("Strong" = "black", "Decisive" = "black"), 
    name = "Bayes Factor"
  ) +
  
  # Clean theme
  theme_classic() +
  labs(fill = "Regions") +
  guides(
    fill  = guide_legend(override.aes = list(linetype = 0, size = 5)),
    size  = guide_legend(title = "Bayes Factor"),  
    color = guide_legend(title = "Bayes Factor")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    panel.grid   = element_blank(),
    axis.line    = element_blank()
  )

