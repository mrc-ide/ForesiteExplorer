# Load Libraries
library(dplyr)
library(tidyr)
library(purrr) # for map functions

# Configuration and Constants
debug <- TRUE
expand_year <- 5
delay <- 0
iso <- "NER"
environment_label <- ifelse(debug, "debug", "final")
method_label <- ifelse(delay > 0, "delay", "current")
base_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/raw/", environment_label, "/")
method_dir <- paste0(base_dir, method_label, "/")
post_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/post/", environment_label, "/")
model_types <- c("PyNets", "PyPyroNets", "PyPBONets")

# Ensure the post-processing directory exists
if (!dir.exists(post_dir)) {
  dir.create(post_dir, recursive = TRUE)
}

# Utility Functions
preprocess_data <- function(file_path, source_label) {
  readRDS(file_path) %>%
    select(timestep, n_inc_clinical_1825_5474) %>%
    mutate(timestep = floor((timestep / 365.25) + 2000),  # Convert days to whole years
           net_types = source_label)  # Label the net_types
}

aggregate_data <- function(data) {
  data %>%
    group_by(timestep, net_types) %>%
    summarise(n_inc_clinical_1825_5474 = sum(n_inc_clinical_1825_5474, na.rm = TRUE)) %>%
    ungroup()
}

# Main Execution Logic
process_and_combine_data <- function() {
  all_data <- map(model_types, function(model_type) {
    files <- list.files(path = paste0(method_dir, model_type, "/", iso, "/"), pattern = "*.RDS", full.names = TRUE)
    datasets <- lapply(files, preprocess_data, model_type) %>% bind_rows()
    aggregate_data(datasets)
  })
  
  bind_rows(all_data)
}

# Save Processed Data
combined_data <- process_and_combine_data()
processed_filename <- paste0(post_dir, "post_model_output_", iso, "_", environment_label, "_", method_label, "_incidence.RDS")
saveRDS(combined_data, file = processed_filename)
