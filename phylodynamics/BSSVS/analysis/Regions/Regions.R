library(circlize)
library(tidytree)
library(RColorBrewer)

lighten_color <- function(color, factor = 0.5) {
  col <- col2rgb(color)
  col <- col + (255 - col) * factor
  rgb(t(col), maxColorValue = 255)
}

# Chord diagram Louvain 2
par(mfrow = c(1, 1), cex = 0.9, family = "serif")

# Read and preprocess the data
data <- read.csv("2024Regions.csv")
df <- data.frame(data)
df$FROM <- gsub("_", "", as.character(df$from))
df$TO <- gsub("_", "", as.character(df$to))
df$FROM <- paste0("src-", df$FROM)
df$FROM <- gsub('\\s+', '', df$FROM)
df$TO <- paste0("snk-", df$TO)
df$TO <- gsub('\\s+', '', df$TO)
df$BF <- as.character(df$bf)

# Convert "10000" to a numeric representation for comparison
#df$BF[df$BF == "10000"] <- "101"  # Treat "10000" as a very high value for color mapping

# Convert BF to numeric after handling "10000"
#df$BF <- as.numeric(df$BF)

# Handle Bayes Factor coloring
df$BAYES_FACTOR <- with(df, ifelse(BF == "inf", "black",
                                   ifelse(as.numeric(BF) < 3, "grey60",
                                          ifelse(as.numeric(BF) < 10, "grey",
                                                 ifelse(as.numeric(BF) < 30, "grey90",
                                                        ifelse(as.numeric(BF) < 100, "honeydew", "black"))))))

# Define a colorblind-friendly palette
colorblind_palette <- c(
  "brown",    # C1 - Dark Green
  "orange",   # C2 - Dark Purple
  "cyan3",    # C4 - Light Gray
  "purple",   # C5 - Medium Gray
  "#F0E442"  # C6 - Light Green
)

lightened_colors <- sapply(colorblind_palette, lighten_color, factor=0.5)

# Define custom colors for each region type based on provided legend
region_names <- c("SC", "NC", "NW", "SE", "SW", "SC")
custom_colors <- setNames(lightened_colors, region_names)

full_names <- c("SC" = "South Central", "NC" = "North Central", "NW" = "North West", 
                "SE" = "South East", "SW" = "South West")

regions_names <- gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))
regions_colors <- custom_colors[regions_names]
names(regions_colors) <- regions_names

# Correct the grid.col mapping
grid.col <- setNames(regions_colors[gsub("src-|snk-", "", unique(c(df$FROM, df$TO)))], unique(c(df$FROM, df$TO)))

# Set custom gap.after for better spacing
gap.after <- rep(1, length(unique(c(df$FROM, df$TO))))
gap.after[length(unique(df$FROM))] <- 30  # Larger gap between the last source and the first sink
gap.after[length(unique(c(df$FROM, df$TO)))] <- 30  # Larger gap after the last sink

# Initialize circos parameters
circos.clear()  # Clear any previous plot if exists
circos.par(start.degree = 90, gap.after = gap.after)  # Start at 90 degrees to match the orientation in the example

# Reverse the order of sectors
reversed_regions <- rev(unique(c(df$FROM, df$TO)))

# Plot the chord diagram
chordDiagram(df[, c("FROM", "TO", "posterior.mean.of.location.rates")], 
             order = reversed_regions,
             grid.col = grid.col, col = df$BAYES_FACTOR, directional = 1, direction.type = c("arrows"), link.arr.type = "big.arrow",
             annotationTrack = "grid", preAllocateTracks = 1, annotationTrackHeight = mm_h(5))

# Define labels with abbreviations
labels <- gsub("src-|snk-", "", reversed_regions)
labels <- ifelse(labels == "SC", "SC", 
                 ifelse(labels == "NC", "NC", 
                        ifelse(labels == "NW", "NW",
                               ifelse(labels == "SE", "SE",
                                      ifelse(labels == "SW", "SW", labels)))))

# Add sector labels
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  label <- labels[which(reversed_regions == sector.index)]
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], label, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.9, family = "serif")
}, bg.border = NA)

# Draw dashed line
abline(v = 0, col = "black", lty = 2)
segments(x0 = 0, y0 = -0.5, x1 = 0, y1 = 0.5, col = "black", lty = 2)

# Add annotations for Source and Sink very close to the dashed line
text(0.05, 1, "Sink", adj = c(0, 0.5), cex = 1.5, font = 2, family = "serif")
text(-0.05, 1, "Source", adj = c(1, 0.5), cex = 1.5, font = 2, family = "serif")

# Add Bayes Factor legend inside the plot, closer to the dashed line
bayes_labels <- c(">100" = "Decisive support (>100)", "31-100" = "Very strong support (31-100)", "11-30" = "Strong support (11-30)", "3-10" = "Substantial support (3-10)")
bayes_colors <- c("black", "grey60", "grey", "grey90")

# Position two labels on the left side and two on the right side
legend("topleft", title = "Regions", legend = full_names, col = custom_colors, pch = 15, cex = 1.2, bty = "n")
legend(x = -1.2, y = -0.8, legend = bayes_labels[1:2], col = bayes_colors[1:2], pch = 15, cex = 1.2, bty = "n", xpd = TRUE)
legend(x = 0.1, y = -0.8, legend = bayes_labels[3:4], col = bayes_colors[3:4], pch = 15, cex = 1.2, bty = "n", xpd = TRUE)

title("", family = "serif")

circos.clear()

