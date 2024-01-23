# library(foresite)
# library(site)
# library(malariasimulation)
# library(data.table)
# library(dplyr)
# library(purrr)
# library(remotes)
# library(drat)
# library(furrr)

# # Load site data and preprocess it for modeling
# prep_inputs <- function(site_data) {
#   rural_sites <- site_data$sites[site_data$sites$urban_rural == "rural", ]
#   message(paste0("Prepping ", nrow(rural_sites), " jobs for model launch"))
#   site_indices <- seq_len(nrow(rural_sites))
#   output <- lapply(site_indices, function(index) prep_site_data(site_data, index))
#   output <- output[!sapply(output, is.null)]
#   validate_output(output)
# }

# # Prepare data for a single site
# prep_site_data <- function(site_data, index) {
#   site <- site::single_site(site_data, index = index)
#   if (is_urban_site(site) || is_zero_eir(site)) return(NULL)
#   message(paste0("Prepping inputs for site ", site$sites$name_1, " ", site$sites$urban_rural))
#   create_input_list(site)
# }

# # Check if the site is urban
# is_urban_site <- function(site) {
#   if (site$sites$urban_rural == "urban") {
#     message(paste0("Skipping urban site: ", site$sites$name_1))
#     return(TRUE)
#   }
#   FALSE
# }

# # Check if the EIR value is zero
# is_zero_eir <- function(site) {
#   if (site$eir$eir[1] <= 0) {
#     message(paste0(site$sites$name_1, " has EIR <= 0. Skipping."))
#     return(TRUE)
#   }
#   FALSE
# }

# # Create input list for modeling
# create_input_list <- function(site) {
#   params <- site::site_parameters(
#     interventions = site$interventions,
#     demography = site$demography,
#     vectors = site$vectors,
#     seasonality = site$seasonality,
#     eir = site$eir$eir[1],
#     overrides = list(human_population = human_population)
#   )
#   list("param_list" = params,
#        "site_name" = site$sites$name_1,
#        "ur" = site$sites$urban_rural,
#        "iso" = site$sites$iso3c)
# }

# # Validate output before proceeding
# validate_output <- function(output) {
#   if (length(output) == 0) {
#     message("All sites skipped. Exiting function.")
#     return(NULL)
#   }
#   output
# }

# # Run the malaria model
# run_malaria_model <- function(output, folder) {
#   start_time <- Sys.time()
#   message("Running the model")
#   model <- malariasimulation::run_simulation(timesteps = output$param_list$timesteps,
#                                              parameters = output$param_list)
#   model <- data.table(model)
#   model[, c("site_name", "urban_rural", "iso") := .(output$site_name, output$ur, output$iso)]
#   save_model_output(model, folder, output$site_name, output$ur)
#   record_time_taken(start_time)
# }

# # Save model output to a file
# save_model_output <- function(model, folder, site_name, ur) {
#   message("Saving the model")
#   saveRDS(model, file = paste0(folder, "raw_model_output_", site_name, "_", ur, ".RDS"))
# }

# # Record and report time taken for model run
# record_time_taken <- function(start_time) {
#   end_time <- Sys.time()
#   time_taken <- end_time - start_time
#   message(paste0("Time taken: ", time_taken))
# }

# # Filter interventions based on selected years and update them with net data
# filter_and_update_interventions <- function(site_data, net_data, select_years) {
#   updated_interventions <- update_intervention_values(
#     filter_interventions(site_data$interventions, select_years),
#     net_data
#   )
#   apply_updated_interventions(site_data, updated_interventions, select_years)
# }

# # Filter interventions for selected years and rural sites
# filter_interventions <- function(interventions, select_years) {
#   interventions %>%
#     filter(year %in% select_years, urban_rural == "rural") %>%
#     select(dn0, rn0, gamman, pyrethroid_resistance)
# }

# # Update intervention values based on net data
# update_intervention_values <- function(interventions, net_data) {
#   interventions %>%
#     mutate(
#       dn0 = replace_values(pyrethroid_resistance, net_data$resistance, dn0, net_data$dn0_med),
#       rn0 = replace_values(pyrethroid_resistance, net_data$resistance, rn0, net_data$rn0_med),
#       gamman = replace_values(pyrethroid_resistance, net_data$resistance, gamman, net_data$gamman_med)
#     )
# }

# # Replace values based on matching criteria
# replace_values <- function(column, match_column, original_value, replacement_value) {
#   ifelse(column %in% match_column, replacement_value[match(column, match_column)], original_value)
# }

# # Apply updated intervention values to the site data
# apply_updated_interventions <- function(site_data, updated_interventions, select_years) {
#   row_indexes <- which(site_data$interventions$year %in% select_years & site_data$interventions$urban_rural == "rural")
#   site_data$interventions$dn0[row_indexes] <- updated_interventions$dn0
#   site_data$interventions$rn0[row_indexes] <- updated_interventions$rn0
#   site_data$interventions$gamman[row_indexes] <- updated_interventions$gamman
#   site_data
# }

# # Expand interventions for a given site
# expand_interventions <- function(site_data, expand_year, delay) {
#   site_data <- insert_expanded_rows(site_data, expand_year)
#   site_data <- insert_delayed_rows(site_data, delay)
#   return(site_data)
# }

# # Insert rows for expanding interventions and adjust data types
# insert_expanded_rows <- function(site_data, expand_year) {
#   # Logic for expanding interventions by copying rows and updating years
#   # ...
#   return(site_data)
# }

# # Insert rows for delayed interventions and adjust years
# insert_delayed_rows <- function(site_data, delay) {
#   # Logic for inserting delayed rows and adjusting years
#   # ...
#   return(site_data)
# }

# # Set up the environment for debugging
# setup_debug_environment <- function() {
#   output_dir <- "debug"
#   human_population <- 1500
#   iso_codes <- c("NER")
#   net_types <- list(
#     PyNets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_only_nets.csv"),
#     PyPyroNets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_pyrrole_nets.csv"),
#     PyPBONets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_pbo_nets.csv")
#   )
#   return(list(output_dir = output_dir, human_population = human_population, iso_codes = iso_codes, net_types = net_types))
# }

# # Set up the environment for production
# setup_production_environment <- function() {
#   output_dir <- "final"
#   human_population <- 15000
#   iso_codes <- c("MLI", "NER")
#   net_types <- list(
#     PyNets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_only_nets.csv"),
#     PyPyroNets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_pyrrole_nets.csv"),
#     PyPBONets = read.csv("D:/Malaria/ForesiteExplorer/data/pyrethroid_pbo_nets.csv")
#   )
#   return(list(output_dir = output_dir, human_population = human_population, iso_codes = iso_codes, net_types = net_types))
# }

# # Process models for each country
# process_country_models <- function(iso_codes, net_types, folder_base, expand_year, delay, run_model_for_country_func) {
#   env_settings <- if (debug) setup_debug_environment() else setup_production_environment()
#   folder_base <- paste0("D:/Malaria/ForesiteExplorer/outputs/", env_settings$output_dir, "/", if (delay > 0) "delay" else "current", "/")

#   furrr::future_map(iso_codes, function(iso) {
#     process_single_country(iso, net_types, folder_base, expand_year, delay, run_model_for_country_func)
#   })
# }

# # Process model for a single country
# process_single_country <- function(iso, net_types, folder_base, expand_year, delay, run_model_for_country_func) {
#   net_data <- net_types[[net_name]]
#   folder_net_type <- ensure_directory(folder_base, net_name)
#   folder_iso <- ensure_directory(folder_net_type, iso)
#   run_model_for_country_func(iso, folder_iso, net_data, expand_year)
# }

# # Ensure directory existence and create if not present
# ensure_directory <- function(base_path, sub_path) {
#   full_path <- paste0(base_path, sub_path, "/")
#   if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
#   return(full_path)
# }

# # Define the function for running the model for a country based on whether parallel processing is enabled
# run_model_for_country <- if (parallel) {
#   function(iso, folder_base, net_data, expand_year) {
#     # Parallel execution logic
#     # ...
#   }
# } else {
#   function(iso, folder_base, net_data, expand_year) {
#     # Sequential execution logic
#     # ...
#   }
# }

# # Main script execution
# setwd("D:/Malaria")
# debug <- TRUE
# expand_year <- 5
# delay <- 3

# process_country_models(iso_codes, net_types, folder_base, expand_year, delay, run_model_for_country)
library(foresite)
library(site)
library(malariasimulation)
library(data.table)
library(dplyr)
library(purrr)
library(remotes)
library(drat)
library(furrr)

# Helper function to check if the site is urban
is_urban <- function(site) {
  site$sites$urban_rural == "urban"
}

# Helper function to check if the EIR is less than or equal to 0
has_invalid_eir <- function(site) {
  site$eir$eir[1] <= 0
}

# Helper function to get parameters for the site
get_site_parameters <- function(site) {
  site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    overrides = list(human_population = human_population)
  )
}

# Helper function to prepare data for a single site
prep_single_site <- function(site) {
  site_name <- site$sites$name_1
  ur <- site$sites$urban_rural
  iso <- site$sites$iso3c
  
  if (is_urban(site)) {
    message(paste0("Skipping urban site: ", site_name))
    return(NULL)
  }
  
  if (has_invalid_eir(site)) {
    message(paste0(site_name, " has EIR <= 0. Skipping."))
    return(NULL)
  }
  
  message(paste0("prepping inputs for site ", site_name, " ", ur))
  print(site$eir)
  
  params <- get_site_parameters(site)
  
  inputs <- list("param_list" = params,
                  "site_name" = site_name,
                  "ur" = ur,
                  "iso" = iso)
  return(inputs)
}

prep_inputs <- function(site_data) {
  jobs <- nrow(site_data$sites)
  rural_sites <- site_data$sites[site_data$sites$urban_rural == "rural", ]
  message(paste0("prepping ", nrow(rural_sites), " jobs for model launch"))
  
  prep_site_data <- function(num) {
    site <- site::single_site(site_data, index = num)
    prep_single_site(site)
  }
  
  output <- lapply(c(1:jobs), prep_site_data)
  output <- output[!sapply(output, is.null)]
  
  if (length(output) == 0) {
    message("All sites skipped. Exiting function.")
    return(NULL)
  } else {
    return(output)
  }
}

# Helper function to run the simulation
run_simulation <- function(output) {
  malariasimulation::run_simulation(timesteps = output$param_list$timesteps,
                                    parameters = output$param_list)
}

# Helper function to augment the model with additional data
augment_model <- function(model, output) {
  setDT(model)  # Convert to data.table in place
  model[, site_name := output$site_name]
  model[, urban_rural := output$ur]
  model[, iso := output$iso]
  return(model)
}

# Helper function to save the model
save_model <- function(model, folder, output) {
  file_path <- paste0(folder, "raw_model_output_", output$site_name, "_", output$ur, ".RDS")
  saveRDS(model, file = file_path)
  message("Model saved: ", file_path)
}

# Main function to run and save the malaria model
run_malaria_model <- function(output, folder) {
  start_time <- Sys.time()  # Record the start time
  message("Running the model...")
  
  model <- run_simulation(output)
  model <- augment_model(model, output)
  
  save_model(model, folder, output)
  
  end_time <- Sys.time()  # Record the end time
  time_taken <- end_time - start_time  # Calculate the time taken
  
  message(sprintf("Time taken: %d minutes and %d seconds", as.integer(time_taken / 60), time_taken %% 60))
}

# Helper function to filter interventions by year and urban/rural status
filter_interventions <- function(interventions, select_years) {
  interventions |>
    dplyr::filter(year %in% select_years, urban_rural == "rural") |>
    dplyr::select(dn0, rn0, gamman, pyrethroid_resistance)
}

# Helper function to update intervention parameters based on resistance data
update_interventions <- function(interventions, net_data) {
  interventions |>
    dplyr::mutate(
      dn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$dn0_med[match(pyrethroid_resistance, net_data$resistance)], dn0),
      rn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$rn0_med[match(pyrethroid_resistance, net_data$resistance)], rn0),
      gamman = ifelse(pyrethroid_resistance %in% net_data$resistance,
                      net_data$gamman_med[match(pyrethroid_resistance, net_data$resistance)], gamman)
    )
}

# Main function to filter and update interventions
filter_and_update_interventions <- function(site_data, net_data, select_years) {
  filtered_interventions <- filter_interventions(site_data$interventions, select_years)
  updated_interventions <- update_interventions(filtered_interventions, net_data)
  
  row_indexes <- which(site_data$interventions$year %in% select_years & site_data$interventions$urban_rural == "rural")
  site_data$interventions$dn0[row_indexes] <- updated_interventions$dn0
  site_data$interventions$rn0[row_indexes] <- updated_interventions$rn0
  site_data$interventions$gamman[row_indexes] <- updated_interventions$gamman
  
  return(site_data)
}

expand_interventions <- function(site_data, expand_year, delay) {
  max_year <- max(site_data$interventions$year)

  # Identify the row indices where the year is max_year
  ridx <- which(site_data$interventions$year == max_year)

  # Calculate the indices where new rows should be inserted, adjusting for the shifting that occurs as rows are added
  # Calculation to account for the extra rows
  updateridx <- ridx + (expand_year) * 0:(length(ridx) - 1)

  # Loop through each row index in updateridx
  for (i in seq_along(updateridx)) {
    # Copy the row at the current index
    updater_row <- site_data$interventions[updateridx[i], ]

    # Store the data types of each column in the copied row
    updater_row_types <- sapply(updater_row, class)

    # Create expand_year number of copies of the updater_row
    new_rows <- t(replicate(expand_year, updater_row, simplify = "matrix"))

    # Set the column names for new_rows to match the original data frame
    colnames(new_rows) <- colnames(site_data$interventions)

    # Update the 'year' column in new_rows
    new_rows[, "year"] <- seq(max_year + 1, by = 1, length.out = expand_year)

    # Handle the 'itn_use' column
    # Calculate the mean of 'itn_use' for the original row and the 2 rows before it (if they exist)
    start_idx <- max(1, updateridx[i] - 2)
    end_idx <- updateridx[i]
    mean_itn_use <- colMeans(site_data$interventions[start_idx:end_idx, "itn_use"], na.rm = TRUE)
    # Assign the mean to the 'itn_use' column for all the new rows
    new_rows[, "itn_use"] <- mean_itn_use

    # Insert new_rows into site_data$interventions at the appropriate index
    site_data$interventions <- rbind(site_data$interventions[1:updateridx[i], ],
                                    as.data.frame(new_rows),
                                    site_data$interventions[(updateridx[i]+1):nrow(site_data$interventions), ])

    # Ensure that the data types of the columns in site_data$interventions match those in updater_row
    for (j in seq_along(updater_row_types)) {
      col_type <- typeof(site_data$interventions[[j]])
      if (col_type != updater_row_types[j]) {
        site_data$interventions[[j]] <- as(site_data$interventions[[j]], updater_row_types[j])
      }
    }
  }

  # Truncate the interventions data frame to include only up to the last inserted row
  site_data$interventions <- site_data$interventions[1:(max(updateridx) + expand_year), ]

  # New functionality starts here
  # Calculate the new maximum year for the extended data
  new_max_year <- max(site_data$interventions$year) - expand_year - 1

  # Identify the row indices for the new maximum year
  new_ridx <- which(site_data$interventions$year == new_max_year)

  # Initialize a variable to track the total number of rows inserted
  total_inserted_rows <- 0

  # Loop through each index in new_ridx to add 'delay' number of rows for each
  for (idx in new_ridx) {
    # Adjust the index by the total number of inserted rows so far
    adjusted_idx <- idx + total_inserted_rows

    # Copy the row at the current index
    delay_row <- site_data$interventions[adjusted_idx, ]

    # Create 'delay' number of copies of the delay_row
    new_rows <- replicate(delay, delay_row, simplify = FALSE)

    # Update the 'year' column in new_rows by incrementally increasing the year
    for (d in seq_len(delay)) {
      new_rows[[d]]$year <- delay_row$year + d
    }

    # Convert the list of new rows into a data frame
    new_rows_df <- do.call(rbind, new_rows)

    # Insert new_rows into site_data$interventions at the appropriate index
    site_data$interventions <- rbind(site_data$interventions[1:adjusted_idx, ],
                                     new_rows_df,
                                     site_data$interventions[(adjusted_idx + 1):nrow(site_data$interventions), ])

    # Update the total number of inserted rows
    total_inserted_rows <- total_inserted_rows + delay

    # Update the years for the rows that come after the insertion, up to a total of expand_year
    # Define the range of rows to update
    update_start <- adjusted_idx + delay + 1
    update_end <- min(update_start + expand_year, nrow(site_data$interventions))
    if (update_start <= update_end) {
      site_data$interventions[update_start:update_end, "year"] <- 
        site_data$interventions[update_start:update_end, "year"] + delay
    }
  }

  # Return the modified site_data
  return(site_data)


}

# Function to set up the environment based on debug mode
setup_environment <- function(debug) {
  if (debug) {
    list(
      output_dir = "debug",
      human_population = 1500,
      iso_codes = c("NER"),
      net_types = read_net_types("D:/Malaria/ForesiteExplorer/data/")
    )
  } else {
    list(
      output_dir = "final",
      human_population = 15000,
      iso_codes = c("MLI", "NER"),
      net_types = read_net_types("D:/Malaria/ForesiteExplorer/data/")
    )
  }
}

# Function to read net types from CSV files
read_net_types <- function(base_path) {
  list(
    PyNets = read.csv(paste0(base_path, "pyrethroid_only_nets.csv")),
    PyPyroNets = read.csv(paste0(base_path, "pyrethroid_pyrrole_nets.csv")),
    PyPBONets = read.csv(paste0(base_path, "pyrethroid_pbo_nets.csv"))
  )
}

# Function to create necessary directories
create_directories <- function(folder_base, net_name, iso_codes) {
  folder_net_type <- paste0(folder_base, net_name, "/")
  if (!dir.exists(folder_net_type)) {
    dir.create(folder_net_type, recursive = TRUE)
  }
  
  folders_iso <- lapply(iso_codes, function(iso) {
    folder_iso <- paste0(folder_net_type, iso, "/")
    if (!dir.exists(folder_iso)) {
      dir.create(folder_iso, recursive = TRUE)
    }
    folder_iso
  })
  
  list(folder_net_type = folder_net_type, folders_iso = folders_iso)
}

# Function to run model for a country
run_model_for_country <- function(iso, folder_base, net_data, expand_year, parallel) {
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

# Set working directory
setwd("D:/Malaria")

# Configuration
debug <- FALSE
parallel <- TRUE
if (parallel) plan(multisession, workers = 20)
expand_year <- 5
delay <- 3
method <- if (delay > 0) "delay" else "current"

# Set up environment
env <- setup_environment(debug)
output_dir <- env$output_dir
human_population <- env$human_population
iso_codes <- env$iso_codes
net_types <- env$net_types

# Set the base directory
folder_base <- paste0("D:/Malaria/ForesiteExplorer/outputs/", output_dir, "/", method, "/")

# Iterate over each net type
for (net_name in names(net_types)) {
  net_data <- net_types[[net_name]]
  folders <- create_directories(folder_base, net_name, iso_codes)
  
  # Iterate over each country (ISO code)
  for (iso in iso_codes) {
    run_model_for_country(iso, folders$folders_iso[[iso]], net_data, expand_year, parallel)
  }
}
