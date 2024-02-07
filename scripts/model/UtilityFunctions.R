
save_interventions_data <- function(site_data) {
  # Extract ISO code from site_data
  iso_code <- site_data$sites$iso3c[1]

  # Construct the path for saving the RDS file in the sitefile directory
  raw_sitefile_folder_path <- paste0("outputs/raw/sitefile/", iso_code, "/")
  create_directory(raw_sitefile_folder_path)

  # File name for the RDS file
  rds_file_name <- paste0(raw_sitefile_folder_path, "site_data_interventions_", iso_code, ".RDS")

  # Save the RDS file
  saveRDS(site_data$interventions, file = rds_file_name)
  message("Interventions data saved: ", rds_file_name)
}

# Initialize Environment
initialize_environment <- function() {
    if (parallel) {
        plan(multisession, workers = workers)
    }
}

# Utility Functions
create_directory <- function(path) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
}

read_net_data <- function() {
    base_path <- paste0(getwd(), "/data/")
    net_types <- setNames(lapply(paste0(base_path, net_files), read.csv), net_names)
    return(net_types)
}