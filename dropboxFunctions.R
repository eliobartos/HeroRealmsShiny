outputDir = 'HeroRealmsData'

# Save one row of data as a file to dropbox
saveDataToDrop <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", gsub(":", "-", sub(" ", "_", Sys.time())), digest::digest(data))
  
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}


# Read all files and load them as one data frame
loadDataFromDrop <- function() {
  
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

