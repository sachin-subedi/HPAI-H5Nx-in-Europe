library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define a mapping from varID to descriptive names from the XML
var_name_mapping <- c(
  "1" = "Sample Size Origin",
  "2" = "Sample Size Destination",
  "3" = "Products Origin",
  "4" = "Products Destination",
  "5" = "Accipitridae Count Origin",
  "6" = "Accipitridae Count Destination",
  "7" = "Anatidae Count Origin",
  "8" = "Anatidae Count Destination",
  "9" = "Laridae Count Origin",
  "10" = "Laridae Count Destination",
  "11" = "Case Count Origin",
  "12" = "Case Count Destination"
)

# Function to calculate the HPD interval for a given vector
calc_min_interval <- function(x, alpha = 0.05) {
  x <- sort(x)
  n <- length(x)
  cred_mass <- 1.0 - alpha
  
  interval_idx_inc <- floor(cred_mass * n)
  n_intervals <- n - interval_idx_inc
  interval_width <- x[(interval_idx_inc + 1):n] - x[1:n_intervals]
  
  if (length(interval_width) == 0) {
    stop("Too few elements for interval calculation")
  }
  
  min_idx <- which.min(interval_width)
  hdi_min <- x[min_idx]
  hdi_max <- x[min_idx + interval_idx_inc]
  
  return(c(hdi_min, hdi_max))
}

# Function to calculate the Bayes Factor for indicators
BayesFactor <- function(indicators, n) {
  prior <- 1 - exp(log(0.5) / n)
  posterior <- mean(indicators)
  if (posterior == 1) {
    posterior <- (length(indicators) - 1) / length(indicators)
  }
  BF <- (posterior / (1 - posterior)) / (prior / (1 - prior))
  return(BF)
}

# Main function to summarize GLM data from multiple files
GLMSummarize <- function(dirname) {
  # Set directory path
  setwd(dirname)
  
  # Create an empty data frame for output
  output <- data.frame(
    Model = character(),
    Trait = character(),
    Variable = character(),
    CoefMedian = numeric(),
    CoefLowerHPD = numeric(),
    CoefUpperHPD = numeric(),
    pp = numeric(),
    BF = numeric(),
    stringsAsFactors = FALSE
  )
  
  # List all .log files in the directory
  files <- list.files(path = dirname, pattern = "\\.log$")
  print("Files found:")
  print(files)
  
  # Loop through files in the directory
  for (filename in files) {
    print(paste("Processing file:", filename))  # Debugging statement
    model <- tools::file_path_sans_ext(filename)
    
    # Load data and print the first few rows for verification
    GLMdata <- read_tsv(filename, show_col_types = FALSE)
    print("Loaded data:")
    print(head(GLMdata))
    
    if (nrow(GLMdata) == 0) {
      print(paste("Warning: File", filename, "is empty. Skipping..."))
      next
    }
    
    # Detect the prefix for coefIndicators columns (e.g., "Regions")
    prefix <- str_extract(names(GLMdata)[grep("coefIndicators", names(GLMdata))[1]], "^[^.]+")
    print(paste("Detected prefix:", prefix))
    
    # Loop through the detected `coefIndicators` columns
    for (col in names(GLMdata)) {
      if (grepl("coefIndicators", col)) {
        varID <- str_extract(col, "\\d+")
        indcol <- paste0(prefix, ".coefIndicators", varID)
        
        # Ensure the indicator column exists
        indicators <- GLMdata[[indcol]]
        print(paste("Processing indicator column:", indcol))
        
        if (max(indicators, na.rm = TRUE) == 0) {
          interval <- c(0, 0)
          median <- 0
        } else {
          NoZero <- GLMdata[[paste0(prefix, ".coefficients", varID)]]
          NoZero[NoZero == 0] <- NA
          NoNan <- na.omit(NoZero)
          interval <- calc_min_interval(NoNan)
          median <- median(NoNan, na.rm = TRUE)
        }
        
        pp <- mean(indicators, na.rm = TRUE)
        bf <- BayesFactor(indicators, length(grep("coefIndicators", names(GLMdata))))
        
        # Use descriptive name if available, otherwise use varID
        descriptive_name <- ifelse(varID %in% names(var_name_mapping), var_name_mapping[[varID]], varID)
        
        output <- rbind(output, data.frame(
          Model = model, Trait = prefix, Variable = descriptive_name,
          CoefMedian = median, CoefLowerHPD = interval[1], 
          CoefUpperHPD = interval[2], pp = pp, BF = bf
        ))
      }
    }
  }
  
  # Save output to file
  if (nrow(output) > 0) {
    write.table(output, file = file.path(dirname, "GLMSummary.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
    print("Output saved to GLMSummary.txt")
  } else {
    print("No data to save. Output file is empty.")
  }
}

# Define the directory path containing .log files
dirname <- "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/GLM"
GLMSummarize(dirname)
