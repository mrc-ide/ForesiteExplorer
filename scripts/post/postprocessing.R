# Load Libraries
library(dplyr)
library(tidyr)
library(purrr)

# Configuration and Constants
debug <- FALSE
iso <- "NER"
environment_label <- ifelse(debug, "debug", "final")
measure_type <- "prevalence"  # Set to "incidence" or "prevalence"
mode <- "counterfactual"      # Set to "current", "delay", or "counterfactual"
model_types <- c("PyNets", "PyPyroNets", "PyPBONets")

# Mode Settings Function
get_mode_settings <- function(mode) {
  switch(mode,
    "current" = list(expand_year = 1, delay = 0, counterfactual = FALSE),
    "delay" = list(expand_year = 5, delay = 3, counterfactual = FALSE),
    "counterfactual" = list(expand_year = 1, delay = 0, counterfactual = TRUE),
    stop("Invalid mode specified")
  )
}

# Apply mode-specific settings
mode_settings <- get_mode_settings(mode)

# Directory Paths
get_directory_paths <- function(environment_label, method_label) {
  base_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/raw/", environment_label, "/")
  method_dir <- paste0(base_dir, method_label, "/")
  post_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/post/", environment_label, "/")
  list(base_dir = base_dir, method_dir = method_dir, post_dir = post_dir)
}

dir_paths <- get_directory_paths(environment_label, mode)

# Ensure the post-processing directory exists
if (!dir.exists(dir_paths$post_dir)) {
  dir.create(dir_paths$post_dir, recursive = TRUE)
}

# Utility Functions
preprocess_data <- function(file_path, source_label, measure_type) {
  data <- readRDS(file_path)
  data <- data %>%
    mutate(timestep = floor((timestep / 365.25) + 2000),  # Convert days to whole years
           net_types = source_label)

  if (measure_type == "incidence") {
    data <- data %>%
      select(timestep, net_types, n_inc_clinical_1825_5474) %>%
      mutate(value = n_inc_clinical_1825_5474)
  } else if (measure_type == "prevalence") {
    data <- data %>%
      select(timestep, net_types, n_detect_730_3649, n_730_3649) %>%
      mutate(value = n_detect_730_3649 / n_730_3649)
  }

  return(data)
}

aggregate_data <- function(data, measure_type) {
  if (measure_type == "incidence") {
    data %>%
      group_by(timestep, net_types) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
  } else if (measure_type == "prevalence") {
    data %>%
      group_by(timestep, net_types) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      ungroup()
  }
}


# Main Execution Logic
process_and_combine_data <- function(mode_settings, dir_paths, measure_type, iso, model_types) {
  if (mode_settings$counterfactual) {
    files <- list.files(path = paste0(dir_paths$method_dir, iso, "/"), pattern = "*.RDS", full.names = TRUE)
    datasets <- lapply(files, preprocess_data, "Counterfactual", measure_type) %>% bind_rows()
    return(aggregate_data(datasets, measure_type))
  } else {
    all_data <- map(model_types, function(model_type) {
      files <- list.files(path = paste0(dir_paths$method_dir, model_type, "/", iso, "/"), pattern = "*.RDS", full.names = TRUE)
      datasets <- lapply(files, preprocess_data, model_type, measure_type) %>% bind_rows()
      return(aggregate_data(datasets, measure_type))
    })
    return(bind_rows(all_data))
  }
}

# Save Processed Data
combined_data <- process_and_combine_data(mode_settings, dir_paths, measure_type, iso, model_types)
processed_filename <- paste0(dir_paths$post_dir, "post_model_output_", iso, "_", environment_label, "_", mode, "_", measure_type, ".RDS")
saveRDS(combined_data, file = processed_filename)
