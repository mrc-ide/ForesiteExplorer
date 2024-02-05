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