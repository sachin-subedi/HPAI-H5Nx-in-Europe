##############################################################################
# 1) Load libraries
##############################################################################
library(circlize)
library(tidytree)
library(RColorBrewer)

##############################################################################
# 2) Helper to lighten colors, if needed
##############################################################################
lighten_color <- function(color, factor = 0.5) {
  col <- col2rgb(color)
  col <- col + (255 - col) * factor
  rgb(t(col), maxColorValue = 255)
}

##############################################################################
# 3) Read & preprocess your data
##############################################################################
par(mfrow = c(1, 1), cex = 0.9, family = "serif")

setwd("/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/BSSVS/Habitat")
df <- read.csv("Habitat.csv")

# Clean up from/to columns
df$FROM <- gsub("_", "", as.character(df$from))
df$TO   <- gsub("_", "", as.character(df$to))
df$FROM <- gsub("\\s+", "", df$FROM)
df$TO   <- gsub("\\s+", "", df$TO)

# Prefix "src-" and "snk-"
df$FROM <- paste0("src-", df$FROM)
df$TO   <- paste0("snk-", df$TO)

##############################################################################
# 4) Define exactly four BF categories + matching colors & labels
##############################################################################
# First, turn BF into numeric
df$BF <- as.numeric(as.character(df$bf))

# Map BF to one of four categories (NA if BF < 3, or define another color if needed)
df$BFcategory <- with(df,
                      ifelse(BF >= 100, ">100",
                             ifelse(BF >= 31,  "31-100",
                                    ifelse(BF >= 11,  "11-30",
                                           ifelse(BF >= 3,   "3-10", NA)))))

# Now define a set of lighter grays for these four categories
bayes_colors <- c(
  ">100"   = "grey40",  # darkest
  "31-100" = "grey60",
  "11-30"  = "grey80",
  "3-10"   = "grey90"   # lightest
)

# And the text that should appear in the legend
bayes_labels <- c(
  ">100"   = "Decisive support (>100)",
  "31-100" = "Very strong support (31–100)",
  "11-30"  = "Strong support (11–30)",
  "3-10"   = "Substantial support (3–10)"
)

# Finally, convert each row to the actual color
df$BAYES_FACTOR <- bayes_colors[df$BFcategory]

##############################################################################
# 5) Define Habitat colors & names
##############################################################################
custom_colors <- c(
  "FW"  = "#3C5488",  # Freshwater Wetlands
  "HH"  = "#4DBBD5",  # Human
  "MM"  = "lightgreen",  # Mammals
  "PO"  = "#E64B35",  # Poultry
  "SB"  = "#00A087",  # Seabirds
  "TFO" = "#7E6148"  # Terrestrial Forest Open
)
full_names <- c(
  "FW"  = "Freshwater Wetlands",
  "HH"  = "Human",
  "MM"  = "Mammals",
  "PO"  = "Poultry",
  "SB"  = "Seabirds",
  "TFO" = "Terrestial Forest Open"
)

# Optionally lighten these colors
lightened_colors  <- sapply(custom_colors, lighten_color, factor=0.5)

# Match up with unique source/sink labels in your data
Habitat_names  <- gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))
Habitat_colors <- lightened_colors[Habitat_names]
names(Habitat_colors) <- Habitat_names

# Build a mapping of each "sector" (src-FOO or snk-FOO) to the color
grid.col <- setNames(
  Habitat_colors[gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))],
  unique(c(df$FROM, df$TO))
)

##############################################################################
# 6) Prepare circos layout
##############################################################################
unique_sectors <- unique(c(df$FROM, df$TO))
gap.after      <- rep(1, length(unique_sectors))
gap.after[length(unique(df$FROM))] <- 30
gap.after[length(unique_sectors)]  <- 30

circos.clear()
circos.par(start.degree = 90, gap.after = gap.after)

# Reverse sector order so that "src-" is on the left, "snk-" on the right
reversed_Habitats <- rev(unique_sectors)

##############################################################################
# 7) Draw chord diagram, coloring links by BF category
##############################################################################
chordDiagram(
  df[, c("FROM", "TO", "posterior.mean.of.Habitat.rates")],
  order           = reversed_Habitats,
  grid.col        = grid.col,
  col             = df$BAYES_FACTOR,      # Use the newly assigned BF color
  directional     = 1,
  direction.type  = "arrows",
  link.arr.type   = "big.arrow",
  annotationTrack = "grid",
  preAllocateTracks = 1
)

##############################################################################
# 8) Add short sector labels (FW, HH, etc.)
##############################################################################
labels <- gsub("src-|snk-", "", reversed_Habitats)
labels <- ifelse(labels == "FreshwaterWetlands", "FW",
                 ifelse(labels == "Human",             "HH",
                        ifelse(labels == "Mammals",           "MM",
                               ifelse(labels == "Poultry",           "PO",
                                      ifelse(labels == "Seabirds",          "SB",
                                             ifelse(labels == "TerrestialForestOpen", "TFO", labels))))))

circos.trackPlotRegion(
  track.index = 1,
  panel.fun   = function(x, y) {
    sector.index <- get.cell.meta.data("sector.index")
    my_label     <- labels[which(reversed_Habitats == sector.index)]
    circos.text(CELL_META$xcenter,
                CELL_META$ylim[1],
                my_label,
                facing     = "clockwise",
                niceFacing = TRUE,
                adj        = c(0, 0.5),
                cex        = 0.9,
                family     = "serif")
  },
  bg.border = NA
)

##############################################################################
# 9) Optionally add dashed line & Source/Sink text
##############################################################################
abline(v = 0, col = "black", lty = 2)
segments(x0 = 0, y0 = -0.5, x1 = 0, y1 = 0.5, col = "black", lty = 2)

text( 0.05, 1, "Sink",   adj = c(0, 0.5), cex = 1.5, font = 2, family = "serif")
text(-0.05, 1, "Source", adj = c(1, 0.5), cex = 1.5, font = 2, family = "serif")

##############################################################################
# 10) Add legends
##############################################################################

# (A) Habitat legend (top-left, for example)
legend(
  "topleft",
  title  = "Habitat",
  legend = full_names,
  col    = lightened_colors,  # The color assigned above
  pch    = 15,
  cex    = 1,
  bty    = "n"
)

# (B) BF legend (bottom-right, for example)
# Use the text from 'bayes_labels' & colors from 'bayes_colors'
# LEFT side: the first two categories
legend(
  x      = -1,   # negative x so it's on the left side
  y      = -0.8,   # move up/down as you see fit
  legend = bayes_labels[1:2],
  col    = bayes_colors[1:2],
  pch    = 15,
  cex    = 1,
  bty    = "n",
  xpd    = TRUE    # allows the legend to be drawn outside the plot region
)

# RIGHT side: the last two categories
legend(
  x      =  0.1,   # positive x so it's on the right side
  y      = -0.8,
  legend = bayes_labels[3:4],
  col    = bayes_colors[3:4],
  pch    = 15,
  cex    = 1,
  bty    = "n",
  xpd    = TRUE
)

title("", family = "serif")

# Clear circos when done
circos.clear()

