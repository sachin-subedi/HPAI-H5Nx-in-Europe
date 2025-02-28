##############################################################################
# 1) Load libraries
##############################################################################
library(circlize)
library(tidytree)
library(RColorBrewer)

##############################################################################
# 2) Read & preprocess your data
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
# 3) Define exactly four BF categories + matching colors & labels
##############################################################################
df$BF <- as.numeric(as.character(df$bf))

df$BFcategory <- with(df,
                      ifelse(BF >= 100, ">100",
                             ifelse(BF >= 31,  "31-100",
                                    ifelse(BF >= 11,  "11-30",
                                           ifelse(BF >= 3,   "3-10", NA)))))

bayes_colors <- c(
  ">100"   = "grey40",
  "31-100" = "grey60",
  "11-30"  = "grey80",
  "3-10"   = "grey90"
)

bayes_labels <- c(
  ">100"   = "Decisive support (>100)",
  "31-100" = "Very strong support (31–100)",
  "11-30"  = "Strong support (11–30)",
  "3-10"   = "Substantial support (3–10)"
)

df$BAYES_FACTOR <- bayes_colors[df$BFcategory]

##############################################################################
# 4) Define Habitat colors & names
##############################################################################
custom_colors <- c(
  "FW" = "#F4D6A0",  # Warm Sand
  "HH" = "#A3D977",  # Fresh Green
  "MM" = "#B7E3E0",  # Soft Aqua
  "PO" = "#E8A8B8",  # Subtle Rose
  "SB" = "#C4E8C2",  # Golden Meadow
  "TFO" = "#A6BCD8"   # Sky Blue
)
full_names <- c(
  "FW"  = "Freshwater Wetlands",
  "HH"  = "Human",
  "MM"  = "Mammals",
  "PO"  = "Poultry",
  "SB"  = "Seabirds",
  "TFO" = "Terrestrial Forest Open"
)

Habitat_names  <- gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))
Habitat_colors <- custom_colors[Habitat_names]
names(Habitat_colors) <- Habitat_names

grid.col <- setNames(
  Habitat_colors[gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))],
  unique(c(df$FROM, df$TO))
)

##############################################################################
# 5) Prepare circos layout
##############################################################################
unique_sectors <- unique(c(df$FROM, df$TO))
gap.after      <- rep(1, length(unique_sectors))
gap.after[length(unique(df$FROM))] <- 30
gap.after[length(unique_sectors)]  <- 30

circos.clear()
circos.par(start.degree = 90, gap.after = gap.after)

reversed_Habitats <- rev(unique_sectors)

##############################################################################
# 6) Draw chord diagram, coloring links by BF category
##############################################################################
chordDiagram(
  df[, c("FROM", "TO", "posterior.mean.of.Habitat.rates")],
  order           = reversed_Habitats,
  grid.col        = grid.col,
  col             = df$BAYES_FACTOR,
  directional     = 1,
  direction.type  = "arrows",
  link.arr.type   = "big.arrow",
  annotationTrack = "grid",
  preAllocateTracks = 1
)

abline(v = 0, lty = 2, col = "black", lwd = 1.2)  # Dashed line

##############################################################################
# 7) Add short sector labels with spacing adjustment
##############################################################################
labels <- gsub("src-|snk-", "", reversed_Habitats)
labels <- ifelse(labels == "FreshwaterWetlands", "FW",
                 ifelse(labels == "Human",             "HH",
                        ifelse(labels == "Mammals",           "MM",
                               ifelse(labels == "Poultry",           "PO",
                                      ifelse(labels == "Seabirds",          "SB",
                                             ifelse(labels == "TerrestrialForestOpen", "TFO", labels))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  sector.name <- get.cell.meta.data("sector.index")
  xlim <- get.cell.meta.data("xlim")
  ylim <- get.cell.meta.data("ylim")
  
  # Adjust label positions slightly away from chords
  circos.text(mean(xlim), ylim[2] + 0.15, labels[sector.name], 
              facing = "clockwise", niceFacing = TRUE, cex = 1.2, adj = c(0.5, 0))
}, bg.border = NA)

##############################################################################
# 8) Add "Source" and "Sink" labels on either side of the dashed line
##############################################################################
text(-1.2, 1.2, "Source", font = 2, cex = 1.5)
text(1.2, 1.2, "Sink", font = 2, cex = 1.5)

##############################################################################
# 9) Add legends
##############################################################################
legend(
  "topleft",
  title  = "Habitat",
  legend = full_names,
  col    = custom_colors,
  pch    = 15,
  cex    = 1.5,
  bty    = "n"
)

legend(
  x      = -1,
  y      = -0.8,
  legend = bayes_labels[1:2],
  col    = bayes_colors[1:2],
  pch    = 15,
  cex    = 1,
  bty    = "n",
  xpd    = TRUE
)

legend(
  x      =  0.1,
  y      = -0.8,
  legend = bayes_labels[3:4],
  col    = bayes_colors[3:4],
  pch    = 15,
  cex    = 1,
  bty    = "n",
  xpd    = TRUE
)

title("", family = "serif")
circos.clear()

