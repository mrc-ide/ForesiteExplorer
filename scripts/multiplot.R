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
    geom_vline(xintercept = max_year_line + 3, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = floor(min(data$timestep)):ceiling(max(data$timestep))) + # X-axis ticks for each year
    labs(x = "Time (years)", y = "Incidence", title = "Incidence (5 - 15)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text
}

# Base directory and model types
base_dir <- "D:/Malaria/ForesiteExplorer/outputs/final/delay/"
model_types <- c("PyNets", "PyPyroNets", "PyPBONets")

# Process, aggregate and combine datasets for each model type
all_data <- map(model_types, function(model_type) {
  # List RDS files in the directory

  files <- list.files(path = paste0(base_dir, model_type, "/MLI/"), pattern = "*.RDS", full.names = TRUE)

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
create_plot(combined_data, max_year - 1)

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr) # for map functions

# Function to read and preprocess a dataset
preprocess_data <- function(file_path, source_label, data_label) {
  readRDS(file_path) %>%
    select(timestep, n_inc_clinical_1825_5474) %>%
    mutate(timestep = floor((timestep / 365.25) + 2000),  # Convert days to whole years
           net_types = source_label,
           data_type = data_label,  # Label the data_type (delay or current)
           region = iso) # Include the region in the data
}

# Function to aggregate data at the XXX level and by year
aggregate_data <- function(data) {
  data %>%
    group_by(timestep, net_types, data_type, region) %>%
    summarise(n_inc_clinical_1825_5474 = sum(n_inc_clinical_1825_5474, na.rm = TRUE)) %>%
    ungroup()
}

# Function to create the plot
create_plot <- function(data, max_year_line, region_name) {
  ggplot(data, aes(x = timestep, y = n_inc_clinical_1825_5474, color = net_types, linetype = data_type)) +
    geom_line() +
    geom_vline(xintercept = max_year_line, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = floor(min(data$timestep)):ceiling(max(data$timestep))) + # X-axis ticks for each year
    labs(x = "Time (years)", y = "Incidence", title = paste("Incidence (5 - 15) -", region_name)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text
}

# Base directories for delay and current
base_dirs <- c("final/delay/", "final/current/")
regions <- c("NER", "MLI")
model_types <- c("PyNets", "PyPyroNets", "PyPBONets")

for(region in regions) {
  # Initialize an empty list to store data from all models and types for this region
  region_data <- list()
  
  for(base_dir in base_dirs) {
    for(model_type in model_types) {
      # List RDS files in the directory
      files <- list.files(path = paste0("D:/Malaria/ForesiteExplorer/outputs/", base_dir, model_type, "/", region, "/"), pattern = "*.RDS", full.names = TRUE)
      
      # Process each dataset
      datasets <- lapply(files, function(file_path) {
        preprocess_data(file_path, model_type, base_dir)
      }) %>% bind_rows()
      
      # Aggregate data
      aggregated_data <- aggregate_data(datasets)
      
      # Append to the region_data list
      region_data[[paste(base_dir, model_type, sep = "_")]] <- aggregated_data
    }
  }
  
  # Combine all data for this region
  combined_region_data <- bind_rows(region_data, .id = "source_label")
  
  # Load site_data and define max_year for this region (assuming you have foresite package and its functions available)
  site_data <- foresite:::get_site(region)
  max_year <- max(site_data$interventions$year)

  # Create and display the plot for this region
  plot <- create_plot(combined_region_data, max_year - 1, region)
  print(plot) # Print the plot
}
