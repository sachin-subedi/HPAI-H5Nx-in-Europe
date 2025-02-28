library(magrittr)
library(dplyr)
find_k <- function(m) {
  for (n in 1:m) {
    if (m == n * (n - 1)) {
      return(n)
    }
  }
  return(NULL)
}
extract_second_last <- function(x) {
  parts <- unlist(strsplit(x, "\\."))
  return(parts[length(parts) - 1])
}
extract_last <- function(x) {
  parts <- unlist(strsplit(x, "\\."))
  return(parts[length(parts)])
}

workdir <- "/Users/sachinsubedi/Library/CloudStorage/OneDrive-UniversityofGeorgia/EU_H5/feb13/Results/BSSVS/Habitat"
pathToLog <- "Habitat_bssvs_combined.log"
burnin <- 50

myLog <- read.csv(paste0(workdir, "/", pathToLog), sep = "\t", header = TRUE)
myLog <- myLog[(ceiling(burnin*0.01*(dim(myLog)[1]))):(dim(myLog)[1]),]
selected_Log <- myLog %>%
  select(starts_with("Habitat.rates"), starts_with("Habitat.indicators")) %>% 
  mutate(across(starts_with("Habitat.rates"), ~ . * get(gsub("rates", "indicators", cur_column())), .names = "real.rates.{gsub('Habitat.rates.', '', col)}"))
k <- (length(colnames(selected_Log))/3) %>% find_k()
from <- colnames(selected_Log)[1:(dim(selected_Log)[2]/3)] %>% sapply(., extract_second_last) %>% unname(.)
to <- colnames(selected_Log)[1:(dim(selected_Log)[2]/3)] %>% sapply(., extract_last) %>% unname(.)
posterior.mean.of.indicators <- numeric(k*(k-1))
for(i in 1:(k*(k-1))){
  posterior.mean.of.indicators[i] <- paste0("Habitat.indicators.", from[i], ".", to[i]) %>% pull(selected_Log, .) %>% mean()
}
posterior.mean.of.Habitat.rates <- numeric(k*(k-1))
for(i in 1:(k*(k-1))){
  posterior.mean.of.Habitat.rates[i] <- paste0("Habitat.rates.", from[i], ".", to[i]) %>% pull(selected_Log, .) %>% mean()
}
posterior.mean.of.real.rates <- numeric(k*(k-1))
for(i in 1:(k*(k-1))){
  posterior.mean.of.real.rates[i] <- paste0("real.rates.", from[i], ".", to[i]) %>% pull(selected_Log, .) %>% mean()
}
bf <- numeric(k*(k-1))
q <- (log(2)+k-1)/(k*(k-1))
for(i in 1:(k*(k-1))){
  p <- posterior.mean.of.indicators[i]
  bf[i] <- (p*(1-q))/((1-p)*q)
}
bf_df <- data.frame(from, to, posterior.mean.of.indicators, posterior.mean.of.Habitat.rates, posterior.mean.of.real.rates, bf)

output_file <- paste0(workdir, "/Habitat.csv")
bf_df$posterior.mean.of.indicators <- format(bf_df$posterior.mean.of.indicators, scientific = FALSE)
bf_df$posterior.mean.of.Habitat.rates <- format(bf_df$posterior.mean.of.Habitat.rates, scientific = FALSE)
bf_df$posterior.mean.of.real.rates <- format(bf_df$posterior.mean.of.real.rates, scientific = FALSE)
bf_df$bf <- format(bf_df$bf, scientific = FALSE)
write.csv(bf_df, output_file, row.names = FALSE)

