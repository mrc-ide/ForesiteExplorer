delete_debug_folders <- function() {
    outputs_path <- file.path(getwd(), "ForesiteExplorer", "outputs")
    subfolders <- list.dirs(outputs_path, full.names = TRUE, recursive = FALSE)

    for (subfolder in subfolders) {
        debug_folder_path <- file.path(subfolder, "debug")

        if (file.exists(debug_folder_path)) {
            response <- readline(prompt = paste0("The 'debug' folder in '", basename(subfolder), 
                                                 "' will be deleted. Do you wish to continue? (yes/no): "))
            if (tolower(response) == "yes") {
                unlink(debug_folder_path, recursive = TRUE)
                cat(paste0("The 'debug' folder in '", basename(subfolder), "' has been deleted.\n"))
            } else {
                cat("Deletion aborted by the user.\n")
            }
        } else {
            cat(paste0("The 'debug' folder in '", basename(subfolder), "' does not exist.\n"))
        }
    }
}
