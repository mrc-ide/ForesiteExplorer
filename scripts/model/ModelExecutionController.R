
# Model Execution Functions
run_model_for_country <- function(iso, folder_base, net_data, mode_settings) {
    site_data <- foresite:::get_site(iso)
    select_years <- max(site_data$interventions$year)
    
    site_data <- filter_and_update_interventions(site_data, net_data, select_years)
    site_data <- expand_interventions(site_data, mode_settings$expand_year, mode_settings$delay, mode_settings$counterfactual)

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

    # Adjust net types based on the mode
    net_names_to_use <- if (mode_settings$counterfactual) {
        # Use only PyNets for counterfactual and don't create a sub-folder for net types
        names(net_types)[names(net_types) == "PyNets"]
    } else {
        # Use all net types otherwise and create sub-folders for each
        names(net_types)
    }

    invisible(lapply(net_names_to_use, function(net_name) {
        net_data <- net_types[[net_name]]

        # Adjust folder structure based on counterfactual setting
        folder_net_type <- if (mode_settings$counterfactual) folder_base else paste0(folder_base, net_name, "/")
        create_directory(folder_net_type)

        invisible(lapply(iso_codes, function(iso) {
            folder_iso <- paste0(folder_net_type, iso, "/")
            create_directory(folder_iso)

            run_model_for_country(iso, folder_iso, net_data, mode_settings)
        }))
    }))
}