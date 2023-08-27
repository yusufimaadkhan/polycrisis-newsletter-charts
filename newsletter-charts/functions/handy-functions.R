# Function to import all CSVs from folder

import_csvs_from_folder <- function(folder_path) {
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  dfs <- lapply(csv_files, function(file) {
    file_name <- tools::file_path_sans_ext(basename(file))
    data <- read.csv(file)
    assign(file_name, data, envir = .GlobalEnv)
    return(data)
  })
  
  return(dfs)
}