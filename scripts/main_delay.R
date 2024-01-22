library(foresite)
library(site)
library(netz)
library(malariasimulation)
library(cali)
library(data.table)
library(dplyr)
library(purrr)
library(remotes)
library(didehpc)
library(conan)
library(drat)
library(furrr)

prep_inputs <- function(site_data) {

  jobs <- nrow(site_data$sites)

  rural_sites <- site_data$sites[site_data$sites$urban_rural == "rural", ]

  message(paste0("prepping ", nrow(rural_sites), " jobs for model launch"))

  prep_site_data <- function(num) {

    site <- site::single_site(site_data, index = num)

    ## get site info
    site_name <- site$sites$name_1
    ur <- site$sites$urban_rural
    iso <- site$sites$iso3c

    # Skip the site if it"s urban
    if (ur == "urban") {
      message(paste0("Skipping urban site: ", site_name))
      return(NULL)
    }

    if (site$eir$eir[1] <= 0) {
      message(paste0(site_name, " has EIR <= 0. Skipping."))
      return(NULL)
    }

    message(paste0("prepping inputs for site ", site_name, " ", ur))
    print(site$eir)


    # pull parameters for this site
    params <- site::site_parameters(
      interventions = site$interventions,
      demography = site$demography,
      vectors = site$vectors,
      seasonality = site$seasonality,
      eir = site$eir$eir[1],
      overrides = list(human_population = human_population)
    )

    inputs <- list("param_list" = params,
                    "site_name" = site_name,
                    "ur" = ur,
                    "iso" = iso)
    return(inputs)

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

run_malaria_model <- function(output, folder){

  start_time <- Sys.time()  # Record the start time

  message("running the model")
  model <- malariasimulation::run_simulation(timesteps = output$param_list$timesteps,
                                            parameters = output$param_list)

  model <- data.table(model)
  model[, site_name := output$site_name]
  model[, urban_rural := output$ur]
  model[, iso := output$iso]

  # save model runs somewhere
  message("saving the model")
  saveRDS(model, file = paste0(folder, "raw_model_output_", output$site_name, "_", output$ur, ".RDS"))

  end_time <- Sys.time()  # Record the end time

  # Calculate the time taken and convert to minutes and seconds
  time_taken <- end_time - start_time

  message(paste0("Time taken: ", time_taken))
}

filter_and_update_interventions <- function(site_data, net_data, select_years) {
  to_sub <- site_data$interventions |>
    dplyr::filter(year %in% select_years, urban_rural == "rural") |>
    dplyr::select(dn0, rn0, gamman, pyrethroid_resistance)

  tmp_output <- to_sub |>
    dplyr::mutate(
      dn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$dn0_med[match(pyrethroid_resistance, net_data$resistance)], dn0),
      rn0 = ifelse(pyrethroid_resistance %in% net_data$resistance,
                   net_data$rn0_med[match(pyrethroid_resistance, net_data$resistance)], rn0),
      gamman = ifelse(pyrethroid_resistance %in% net_data$resistance,
                      net_data$gamman_med[match(pyrethroid_resistance, net_data$resistance)], gamman)
    )

  row_indexes <- which(site_data$interventions$year %in% select_years & site_data$interventions$urban_rural == "rural")
  site_data$interventions$dn0[row_indexes] <- tmp_output$dn0
  site_data$interventions$rn0[row_indexes] <- tmp_output$rn0
  site_data$interventions$gamman[row_indexes] <- tmp_output$gamman

  return(site_data)
}

expand_interventions <- function(site_data, expand_year, padding) {
    max_year <- max(site_data$interventions$year)
    # year = max_year
    # # Filter for rural areas and find the last row for each region
    # last_row_by_region <- site_data %>%
    # group_by(name_1) %>%
    # slice(which.max(year)) %>%
    # ungroup()  # Ungroup to prevent group-related issues in subsequent operations

    # if (padding > 0) {
    # for (region in unique(last_row_by_region$name_1)) {
    #     # Extract the last row for the current region
    #     last_row <- last_row_by_region[last_row_by_region$name_1 == region, ]
    #     max_year_region <- max(last_row$year)

    #     # Copy the last row 'padding' times for the current region
    #     for (p in 1:padding) {
    #     padded_row <- last_row
    #     padded_row$year <- max_year_region + p
    #     site_data$interventions <- rbind(site_data$interventions, padded_row)
    #     }
    # }
    # }

  # Original logic from here onwards, adjusted to start from max_year
#   ridx <- which(site_data$interventions$year == max_year)
#   updateridx <- ridx + (expand_year * 0:(length(ridx)-1))

#   for (i in seq_along(updateridx)) {
#     updater_row <- site_data$interventions[updateridx[i], ]
#     updater_row_types <- sapply(updater_row, class)
#     new_rows <- t(replicate(expand_year, updater_row, simplify = "matrix"))
#     colnames(new_rows) <- colnames(site_data$interventions)
#     new_rows[, "year"] <- seq(max_year + 1, length.out = expand_year)

#     start_idx <- max(1, updateridx[i] - 2)
#     end_idx <- updateridx[i]
#     mean_itn_use <- colMeans(site_data$interventions[start_idx:end_idx, "itn_use"], na.rm = TRUE)
#     new_rows[, "itn_use"] <- mean_itn_use

#     site_data$interventions <- rbind(site_data$interventions[1:updateridx[i], ],
#                                      as.data.frame(new_rows),
#                                      site_data$interventions[(updateridx[i]+1):nrow(site_data$interventions), ])

#     for (j in seq_along(updater_row_types)) {
#       col_type <- typeof(site_data$interventions[[j]])
#       if (col_type != updater_row_types[j]) {
#         site_data$interventions[[j]] <- as(site_data$interventions[[j]], updater_row_types[j])
#       }
#     }
#   }

#   site_data$interventions <- site_data$interventions[1:(max(updateridx) + expand_year), ]

#   return(site_data)
# Identify the row indices where the year is max_year

ridx <- which(site_data$interventions$year == max_year)

# Calculate the indices where new rows should be inserted, adjusting for the shifting that occurs as rows are added
updateridx <- ridx + (expand_year * 0:(length(ridx)-1))

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

  # Update the 'year' column in new_rows to be a sequence starting from max_year + 1
  new_rows[, "year"] <- seq(max_year + 1, by = 1, length.out = expand_year)

  # Calculate the mean of 'itn_use' from the range start_idx:end_idx and assign it to the 'itn_use' column in new_rows
  start_idx <- max(1, updateridx[i] - 2)
  end_idx <- updateridx[i]
  mean_itn_use <- colMeans(site_data$interventions[start_idx:end_idx, "itn_use"], na.rm = TRUE)
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

# Return the modified site_data
return(site_data)

}


setwd("X:/Cosmo")

debug = FALSE
parallel = FALSE
padding <- 0
expand_year <- 5
if (padding > 0) method = "delay" else method = "current"

if (parallel) {
    run_model_for_country <- function(iso, folder_base, net_data, expand_year) {
  site_data <- foresite:::get_site(iso)
  select_years <- max(site_data$interventions$year)

  site_data <- filter_and_update_interventions(site_data, net_data, select_years)
  site_data <- expand_interventions(site_data, expand_year, padding)

  output <- prep_inputs(site_data)

  # Using future_map to run the models in parallel
  future_map(output, ~run_malaria_model(.x, folder_base))
}
} else {
    run_model_for_country <- function(iso, folder_base, net_data, expand_year) {
  site_data <- foresite:::get_site(iso)
  select_years <- max(site_data$interventions$year)

  site_data <- filter_and_update_interventions(site_data, net_data, select_years)
  site_data <- expand_interventions(site_data, expand_year, padding)

  output <- prep_inputs(site_data)

  lapply(output, function(x) {
    run_malaria_model(x, folder_base)
  })
}
}


if (debug) {
    output_dir = "debug"
    human_population = 100
    iso_codes <- c("NER")
    net_types <- list(
    PyNets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_only_nets.csv"),
    PyPyroNets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_pyrrole_nets.csv"),
    PyPBONets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_pbo_nets.csv")
)

} else {
    output_dir = "final"
    human_population = 15000#100000
    iso_codes <- c("NER")
    net_types <- list(
    PyNets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_only_nets.csv"),
    PyPyroNets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_pyrrole_nets.csv"),
    PyPBONets = read.csv(
        "X:/Cosmo/projects/ForesiteExplorer/data/pyrethroid_pbo_nets.csv")
)

}

# Set the base directory
folder_base <- paste0("X:/Cosmo/projects/ForesiteExplorer/outputs/",
                        output_dir,
                        "/",
                        method,
                        "/")

# Iterate over each net type
for (net_name in names(net_types)) {
    net_data <- net_types[[net_name]]

    # Create a folder for the net type if it doesn"t exist
    folder_net_type <- paste0(folder_base, net_name, "/")
    if (!dir.exists(folder_net_type)) {
        dir.create(folder_net_type, recursive = TRUE)
    }

    # Iterate over each country (ISO code)
    for (iso in iso_codes) {
        # Construct the folder path for the current ISO
        folder_iso <- paste0(folder_net_type, iso, "/")

        # Create a folder for the ISO if it doesn"t exist
        if (!dir.exists(folder_iso)) {
            dir.create(folder_iso, recursive = TRUE)
        }

        # Run the model for the current net type and ISO
        run_model_for_country(iso, folder_iso, net_data, expand_year)
    }
}

