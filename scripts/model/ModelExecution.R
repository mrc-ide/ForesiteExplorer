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
  file_path <- paste0(folder, "pre_model_output_", output$site_name, "_", output$ur, ".RDS")
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