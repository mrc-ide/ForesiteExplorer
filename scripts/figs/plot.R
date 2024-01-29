# Load Libraries
library(ggplot2)

# Configuration and Constants
debug <- FALSE
delay <- 3
iso <- "NER"
environment_label <- ifelse(debug, "debug", "final")
plot_base_dir <- "D:/Malaria/ForesiteExplorer/outputs/figs/"
plot_dir <- paste0(plot_base_dir, environment_label, "/")
post_dir <- paste0("D:/Malaria/ForesiteExplorer/outputs/post/", environment_label, "/")

# Ensure the plot directory exists
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Plotting Function
create_plot <- function(data, max_year_line, iso, environment_label, method_label) {
  plot <- ggplot(data, aes(x = timestep, y = n_inc_clinical_1825_5474, color = net_types)) +
    geom_line() +
    geom_vline(xintercept = max_year_line, linetype = "dashed", color = "black") +
    geom_vline(xintercept = max_year_line + delay, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = floor(min(data$timestep)):ceiling(max(data$timestep))) +
    labs(x = "Time (years)", y = "Incidence", title = "Incidence (5 - 15)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Construct the filename
  plot_filename <- paste0(plot_dir, iso, "_", environment_label, "_", method_label, "_incidence_plot.png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, bg = "white", width = 10, height = 8)
}

# Main Execution Logic
if (delay > 0) method_preference = "delay"  else method_preference = "current"# Set to "current" or "delay" as per your need
processed_files <- list.files(
  post_dir,
  pattern = paste0("post_model_output_", iso, "_", environment_label, "_", method_preference, "_.*\\.RDS$"),
  full.names = TRUE
)

if (length(processed_files) == 0) {
  stop(paste("No processed data files found for the specified ISO and method preference:", method_preference))
}

for (file_path in processed_files) {
    # Extract details from filename
    filename_parts <- strsplit(basename(file_path), "_")[[1]]
    iso <- filename_parts[4]
    environment_label <- filename_parts[5]
    method_label <- filename_parts[6]
    metric_label <- filename_parts[7]
    metric_label <- gsub("\\.RDS$", "", metric_label)  # Remove the .RDS extension
    
    # Load the processed data
    combined_data <- readRDS(file_path)
    site_data <- foresite:::get_site(iso)
    max_year <- max(site_data$interventions$year)
    
    # Generate and Save the Plot
    create_plot(combined_data, max_year - 1, iso, environment_label, method_label)
}
