# Load Libraries
library(foresite)
library(site)
library(malariasimulation)
library(data.table)
library(dplyr)
library(purrr)
library(remotes)
library(drat)
library(furrr)

source("scripts/utils/global.R")
source("scripts/model/DataPreparation.R")
source("scripts/model/ModelExecution.R")
source("scripts/model/InterventionProcessing.R")
source("scripts/model/InterventionExpansion.R")
source("scripts/model/UtilityFunctions.R")
source("scripts/model/ModelExecutionController.R")

# Configuration and Constants
debug <- TRUE
parallel <- TRUE
mode <- "counterfactual" # Set mode to "current", "delay", or "counterfactual"

# Apply mode-specific settings
mode_settings <- get_mode_settings(mode)

# Parallel and Debug Settings
workers <- if(parallel) 22 else 1
output_dir <- ifelse(debug, "debug", "final")
human_population <- if(debug) 1500 else 150000
iso_codes <- c("NER") # Add additional ISO codes as required

# File and Folder Paths
net_files <- c("pyrethroid_only_nets.csv", "pyrethroid_pyrrole_nets.csv", "pyrethroid_pbo_nets.csv")
net_names <- c("PyNets", "PyPyroNets", "PyPBONets")
folder_base <- paste0(getwd(), "/outputs/raw/sim/", output_dir, "/", mode, "/")

# Script Execution
initialize_environment()
execute_models()
