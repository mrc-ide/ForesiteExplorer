# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr) # for map functions

# Function to read and preprocess a dataset
preprocess_data <- function(file_path, source_label) {
  readRDS(file_path) %>%
    select(timestep, n_inc_clinical_1825_5474) %>%
    mutate(timestep = floor((timestep / 365.25) + 2000),  # Convert days to whole years
           net_types = source_label)  # Label the net_types
}

# Function to aggregate data at the XXX level and by year
aggregate_data <- function(data) {
  data %>%
    group_by(timestep, net_types) %>%
    summarise(n_inc_clinical_1825_5474 = sum(n_inc_clinical_1825_5474, na.rm = TRUE)) %>%
    ungroup()
}

# Function to create the plot
create_plot <- function(data, max_year_line) {
  ggplot(data, aes(x = timestep, y = n_inc_clinical_1825_5474, color = net_types)) +
    geom_line() +
    geom_vline(xintercept = max_year_line, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = floor(min(data$timestep)):ceiling(max(data$timestep))) + # X-axis ticks for each year
    labs(x = "Time (years)", y = "Incidence", title = "Incidence (5 - 15)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text
}

# Base directory and model types
base_dir <- "X:/Cosmo/projects/ForesiteExplorer/outputs/debug/delay/"
model_types <- c("PyNets", "PyPyroNets", "PyPBONets")

# Process, aggregate and combine datasets for each model type
all_data <- map(model_types, function(model_type) {
  # List RDS files in the directory

  files <- list.files(path = paste0(base_dir, model_type, "/NER/"), pattern = "*.RDS", full.names = TRUE)

  # Process each dataset
  datasets <- lapply(files, function(file_path) {
    preprocess_data(file_path, model_type)
  }) %>% bind_rows()

  # Aggregate data
  aggregate_data(datasets)
})

# Combine all data
combined_data <- bind_rows(all_data)

# Load site_data and define max_year (assuming you have foresite package and its functions available)
iso = "NER"
site_data <- foresite:::get_site(iso)
max_year <- max(site_data$interventions$year)

# Create and display the plot
create_plot(combined_data, max_year)
