library(foresite)
library(site)
library(malariasimulation)
library(data.table)
library(dplyr)
library(purrr)
library(remotes)
library(drat)
library(furrr)

# Function to prepare input data for a single site
prep_single_site_data <- function(site_data, site_index) {
  site <- site::single_site(site_data, index = site_index)

  # Retrieve site information
  site_name <- site$sites$name_1
  ur <- site$sites$urban_rural
  iso <- site$sites$iso3c

  # Skip urban sites
  if (ur == "urban") {
    message(paste0("Skipping urban site: ", site_name))
    return(NULL)
  }

  # Check EIR value and skip site if EIR <= 0
  if (site$eir$eir[1] <= 0) {
    message(paste0(site_name, " has EIR <= 0. Skipping."))
    return(NULL)
  }

  message(paste0("Prepping inputs for site ", site_name, " ", ur))
  print(site$eir)

  # Retrieve parameters for the site
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    overrides = list(human_population = human_population)
  )

  inputs <- list(
    "param_list" = params,
    "site_name" = site_name,
    "ur" = ur,
    "iso" = iso
  )

  return(inputs)
}

prep_all_site_data <- function(site_data) {
  jobs <- nrow(site_data$sites)

  # Filter for rural sites
  rural_sites <- site_data$sites[site_data$sites$urban_rural == "rural", ]
  message(paste0("Prepping ", nrow(rural_sites), " jobs for model launch"))

  # Prepare data for each site
  output <- lapply(seq_len(jobs), function(num) prep_single_site_data(site_data, num))
  
  # Remove NULL entries (skipped sites)
  output <- output[!sapply(output, is.null)]

  if (length(output) == 0) {
    message("All sites skipped. Exiting function.")
    return(NULL)
  } else {
    return(output)
  }
}

# Main function to prepare inputs
prep_inputs <- function(site_data) {
  prep_all_site_data(site_data)
}

# Function to run the malaria model simulation
run_simulation <- function(output) {
  malariasimulation::run_simulation(
    timesteps = output$param_list$timesteps,
    parameters = output$param_list
  )
}

# Function to augment model data with site information
augment_model_data <- function(model, output) {
  model <- data.table(model)
  model[, site_name := output$site_name]
  model[, urban_rural := output$ur]
  model[, iso := output$iso]
  return(model)
}

# Function to save the model output to a file
save_model_output <- function(model, output, folder) {
  file_path <- paste0(folder, "raw_model_output_", output$site_name, "_", output$ur, ".RDS")
  saveRDS(model, file = file_path)
  message("Model saved: ", file_path)
}

# Main function to run the malaria model and handle output
run_malaria_model <- function(output, folder) {
  start_time <- Sys.time()

  message("Running the model...")
  model <- run_simulation(output)
  model <- augment_model_data(model, output)
  save_model_output(model, output, folder)

  end_time <- Sys.time()
  time_taken <- end_time - start_time
  message("Time taken: ", time_taken)
}

# Function to filter interventions based on specified years and rural areas
filter_interventions <- function(interventions, select_years) {
  interventions |>
    dplyr::filter(year %in% select_years, urban_rural == "rural") |>
    dplyr::select(dn0, rn0, gamman, pyrethroid_resistance)
}

# Function to update intervention values based on resistance data
update_intervention_values <- function(filtered_data, net_data) {
  filtered_data |>
    dplyr::mutate(
      dn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$dn0_med[match(pyrethroid_resistance, net_data$resistance)], dn0),
      rn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$rn0_med[match(pyrethroid_resistance, net_data$resistance)], rn0),
      gamman = ifelse(pyrethroid_resistance %in% net_data$resistance,
                      net_data$gamman_med[match(pyrethroid_resistance, net_data$resistance)], gamman)
    )
}

# Function to replace intervention values in the original dataset
replace_intervention_values <- function(site_data, updated_data, select_years) {
  row_indexes <- which(site_data$interventions$year %in% select_years & site_data$interventions$urban_rural == "rural")
  site_data$interventions$dn0[row_indexes] <- updated_data$dn0
  site_data$interventions$rn0[row_indexes] <- updated_data$rn0
  site_data$interventions$gamman[row_indexes] <- updated_data$gamman
  return(site_data)
}

# Main function to filter and update interventions
filter_and_update_interventions <- function(site_data, net_data, select_years) {
  filtered_interventions <- filter_interventions(site_data$interventions, select_years)
  updated_interventions <- update_intervention_values(filtered_interventions, net_data)
  site_data <- replace_intervention_values(site_data, updated_interventions, select_years)
  return(site_data)
}

# Helper Functions
copy_and_prepare_rows <- function(row, num_copies, start_year) {
  new_rows <- replicate(num_copies, row, simplify = FALSE)
  for (i in seq_along(new_rows)) {
    new_rows[[i]]$year <- start_year + i
  }
  do.call(rbind, new_rows)
}

insert_new_rows <- function(site_data, new_rows, insert_after_idx) {
  site_data$interventions <- rbind(
    site_data$interventions[1:insert_after_idx, ],
    new_rows,
    site_data$interventions[(insert_after_idx + 1):nrow(site_data$interventions), ]
  )
  site_data
}

adjust_column_types <- function(site_data, updater_row_types) {
  for (j in seq_along(updater_row_types)) {
    if (typeof(site_data$interventions[[j]]) != updater_row_types[j]) {
      site_data$interventions[[j]] <- as(site_data$interventions[[j]], updater_row_types[j])
    }
  }
  site_data
}

adjust_row_indices <- function(ridx, num_added_rows, length_ri) {
  ridx + (num_added_rows) * 0:(length_ri - 1)
}

# Main Function
expand_interventions <- function(site_data, expand_year, delay) {
  max_year <- max(site_data$interventions$year)
  ridx <- which(site_data$interventions$year == max_year)
  updateridx <- adjust_row_indices(ridx, expand_year, length(ridx))

  for (i in seq_along(updateridx)) {
    updater_row <- site_data$interventions[updateridx[i], ]
    updater_row_types <- sapply(updater_row, class)
    new_rows <- copy_and_prepare_rows(updater_row, expand_year, max_year)
    colnames(new_rows) <- colnames(site_data$interventions)
    site_data <- insert_new_rows(site_data, new_rows, updateridx[i])
    site_data <- adjust_column_types(site_data, updater_row_types)
  }

  site_data$interventions <- site_data$interventions[1:(max(updateridx) + expand_year), ]

  new_max_year <- max(site_data$interventions$year) - expand_year - 1
  new_ridx <- which(site_data$interventions$year == new_max_year)
  total_inserted_rows <- 0

  for (idx in new_ridx) {
    adjusted_idx <- idx + total_inserted_rows
    delay_row <- site_data$interventions[adjusted_idx, ]
    new_rows <- copy_and_prepare_rows(delay_row, delay, delay_row$year)
    site_data <- insert_new_rows(site_data, new_rows, adjusted_idx)
    total_inserted_rows <- total_inserted_rows + delay

    update_start <- adjusted_idx + delay + 1
    update_end <- min(update_start + expand_year, nrow(site_data$interventions))
    if (update_start <= update_end) {
      site_data$interventions[update_start:update_end, "year"] <- 
        site_data$interventions[update_start:update_end, "year"] + delay
    }
  }

  return(site_data)
}


# Configuration and Constants
setwd("D:/Malaria")
debug <- TRUE
parallel <- TRUE
expand_year <- 5
delay <- 3
output_dir <- ifelse(debug, "debug", "final")
human_population <- 15000 # Replace with 100000 if needed
iso_codes <- ifelse(debug, c("NER"), c("MLI", "NER"))
method <- ifelse(delay > 0, "delay", "current")
net_files <- c("pyrethroid_only_nets.csv", "pyrethroid_pyrrole_nets.csv", "pyrethroid_pbo_nets.csv")
net_names <- c("PyNets", "PyPyroNets", "PyPBONets")
folder_base <- paste0("D:/Malaria/ForesiteExplorer/outputs/raw/", output_dir, "/", method, "/")

# Initialize Environment
initialize_environment <- function() {
    if (parallel) {
        plan(multisession, workers = 20)
    }
}

# Utility Functions
create_directory <- function(path) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
}

read_net_data <- function() {
    base_path <- "D:/Malaria/ForesiteExplorer/data/"
    net_types <- setNames(lapply(paste0(base_path, net_files), read.csv), net_names)
    return(net_types)
}

# Model Execution Functions
run_model_for_country <- function(iso, folder_base, net_data, expand_year) {
    site_data <- foresite:::get_site(iso)
    select_years <- max(site_data$interventions$year)
    
    site_data <- filter_and_update_interventions(site_data, net_data, select_years)
    site_data <- expand_interventions(site_data, expand_year, delay)
    
    output <- prep_inputs(site_data)
    
    if (parallel) {
        future_map(output, ~run_malaria_model(.x, folder_base))
    } else {
        lapply(output, function(x) {
            run_malaria_model(x, folder_base)
        })
    }
}

# Main Execution Logic
execute_models <- function() {
    net_types <- read_net_data()
    
    invisible(lapply(names(net_types), function(net_name) {
        net_data <- net_types[[net_name]]
        folder_net_type <- paste0(folder_base, net_name, "/")
        create_directory(folder_net_type)
        
        invisible(lapply(iso_codes, function(iso) {
            folder_iso <- paste0(folder_net_type, iso, "/")
            create_directory(folder_iso)
            
            run_model_for_country(iso, folder_iso, net_data, expand_year)
        }))
    }))
}


# Script Execution
initialize_environment()
execute_models()
