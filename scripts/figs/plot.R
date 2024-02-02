# Load Libraries
library(ggplot2)
library(stringr)  # For string operations

# Configuration and Constants
debug <- FALSE
iso <- "NER"
environment_label <- ifelse(debug, "debug", "final")
plot_base_dir <- "D:/Malaria/ForesiteExplorer/outputs/figs/"
plot_dir <- paste0(plot_base_dir, environment_label, "/")
post_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/post/", environment_label, "/")

# Mode Settings
mode <- "current" # Set mode to "current", "delay", or "counterfactual"
use_counterfactual <- TRUE  # Flag to indicate whether to overlay counterfactual data

# Data Type Choice (incidence or prevalence)
data_type <- "incidence" # Set this to either "incidence" or "prevalence"

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

# Ensure the plot directory exists
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Multiplot Function
create_multiplot <- function(base_data, counterfactual_data, max_year_line, iso, environment_label, base_method_label, counterfactual_method_label, data_type) {
  y_label <- ifelse(data_type == "incidence", "Incidence", "Prevalence")
  title_label <- paste(data_type, "(5 - 15) - Comparison: ", base_method_label, " vs ", counterfactual_method_label)

  # Base plot with current or delay data
  plot <- ggplot(base_data, aes(x = timestep, y = value, color = net_types)) +
    geom_line() +
    geom_vline(xintercept = max_year_line, linetype = "dashed", color = "black") +
    geom_vline(xintercept = max_year_line + delay, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = floor(min(base_data$timestep)):ceiling(max(base_data$timestep))) +
    labs(x = "Time (years)", y = y_label, title = title_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add counterfactual data
  if (!is.null(counterfactual_data)) {
    plot <- plot +
      geom_line(data = counterfactual_data, aes(x = timestep, y = value, color = "Counterfactual"), linetype = "dashed")
  }

  # Construct the filename
  plot_filename <- paste0(plot_dir, iso, "_", environment_label, "_", base_method_label, "_vs_", counterfactual_method_label, "_", data_type, "_comparison_plot.png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, bg = "white", width = 10, height = 8)
}

# Main Execution Logic
file_pattern <- paste0("post_model_output_", iso, "_", environment_label, "_", method_label, "_", data_type, "\\.RDS$")
processed_files <- list.files(
  post_dir,
  pattern = file_pattern,
  full.names = TRUE
)

if (length(processed_files) == 0) {
  stop(paste("No processed data files found for the specified ISO, environment, method, and data type:", iso, environment_label, method_label, data_type))
}

# Loop to process files
for (file_path in processed_files) {
  # Load the processed data
  base_data <- readRDS(file_path)
  site_data <- foresite:::get_site(iso)
  max_year <- max(site_data$interventions$year)
  
  # Generate and Save the Plot
  if (use_counterfactual) {
    # Load counterfactual data
    counterfactual_file_pattern <- paste0("post_model_output_", iso, "_", environment_label, "_counterfactual_", data_type, "\\.RDS$")
    counterfactual_files <- list.files(
      post_dir,
      pattern = counterfactual_file_pattern,
      full.names = TRUE
    )

    if (length(counterfactual_files) == 0) {
      stop(paste("No counterfactual data files found for the specified ISO, environment, and data type:", iso, environment_label, data_type))
    }

    counterfactual_data <- readRDS(counterfactual_files[1]) # Assuming one file for simplicity
    create_multiplot(base_data, counterfactual_data, max_year - 1, iso, environment_label, mode, "counterfactual", data_type)
  } else {
    # Create a plot with just the base data
    create_multiplot(base_data, NULL, max_year - 1, iso, environment_label, mode, "", data_type)
  }
}
