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