# Install the required packages if not already installed
if (!requireNamespace("geosphere", quietly = TRUE)) {
  install.packages("geosphere")
}
if (!requireNamespace("exifr", quietly = TRUE)) {
  install.packages("exifr")
}

library(geosphere)
library(exifr)

# Read the CSV file with coordinates and IDs
data <- read.csv("/Users/marco/GitHub/graslandvielfalt/R_files/2023-joinedPlotSelection_v3.csv")

# Directory containing the pictures
pictureDirectory <- "/Users/marco/Desktop/drone_import"

# Function to calculate distance between two coordinates
calculateDistance <- function(lat1, lon1, lat2, lon2) {
  distHaversine(matrix(c(lon1, lat1), ncol = 2), matrix(c(lon2, lat2), ncol = 2))
}

# Function to find the closest ID for a given set of coordinates
findClosestID <- function(lat, lon, data) {
  lat_data <- as.numeric(data$Latitude)
  lon_data <- as.numeric(data$Longitude)
  
  distances <- sapply(seq_len(nrow(data)), function(i) {
    calculateDistance(lat, lon, lat_data[i], lon_data[i])
  })
  
  closestIndex <- order(distances)[1]
  data[closestIndex, "ID"]
}

# Get the list of picture files in the directory
pictureFiles <- list.files(pictureDirectory, pattern = "^DJI.*\\.(dng|jpg)$", ignore.case = TRUE, full.names = TRUE)
pictureFile <- pictureFiles[1]

# Initialize the sequence number
sequenceNumber <- 1

# Rename pictures based on closest ID and sequence number
for (pictureFile in pictureFiles) {
  # Extract the file name without the directory path
  pictureFilename <- basename(pictureFile)
  
  # Extract the GPS coordinates from the DNG file metadata
  metadata <- exifr::read_exif(pictureFile)
  lat <- as.numeric(metadata$GPSLatitude)
  lon <- as.numeric(metadata$GPSLongitude)
  
  closestID <- findClosestID(lat, lon, data)
  
  newPictureFilename <- paste0(closestID, "-", sequenceNumber, ".dng")
  newPictureFilePath <- file.path(pictureDirectory, newPictureFilename)
  
  # Increment the sequence number for the next picture
  sequenceNumber <- sequenceNumber + 1
  
  # Rename the picture file
  file.rename(pictureFile, newPictureFilePath)
  cat("Renamed", pictureFilename, "to", newPictureFilename, "\n")
}

# Group the files by ID
pictureFiles <- list.files(pictureDirectory, pattern = "*\\.(dng|jpg)$", ignore.case = TRUE, full.names = TRUE)
groupedFiles <- split(pictureFiles, sub("^(.*)-.*$", "\\1", basename(pictureFiles)))
groupID <- names(groupedFiles)[1]

# Process each group
for (groupID in names(groupedFiles)) {
  groupFiles <- groupedFiles[[groupID]]
  
  # Determine the file with the highest elevation within the group
  highestElevationFile <- groupFiles[which.max(sapply(groupFiles, function(file) {
    metadata <- exifr::read_exif(file)
    as.numeric(metadata$GPSAltitude)
  }))]
  
  # Rename the group files
  for (i in seq_along(groupFiles)) {
    file <- groupFiles[i]
    filename <- sub("^(.*)-.*$", "\\1", basename(file))
    extension <- tools::file_ext(file)
    indicator <- ifelse(file == highestElevationFile, "-H", "-L")
    newFilename <- paste0(sub("^(.*?-.*?-.*?-.*?)-.*$", "\\1", filename), indicator, "-", i)
    newFilepath <- file.path(pictureDirectory, paste0(newFilename, ".", extension))
    file.rename(file, newFilepath)
    cat("Renamed", basename(file), "to", basename(newFilepath), "\n")
  }
}

### Save file with plots of which the drone picture was taken #########

# Preallocate the idDateData data frame
max_files <- length(pictureFiles)
idDateData <- data.frame(ID = character(max_files), Date = character(max_files), stringsAsFactors = FALSE)
pictureFiles <- list.files(pictureDirectory, pattern = "*\\.(dng|jpg)$", ignore.case = TRUE, full.names = TRUE)

# Process each file using lapply
idDateData <- do.call(rbind, lapply(pictureFiles, function(file) {
  # Extract the ID from the filename
  filename <- basename(file)
  id <- sub("^(.*?)-[^-]*-[^-]*\\..*$", "\\1", filename)
  
  # Extract the date from the picture metadata
  metadata <- exifr::read_exif(file)
  createDate <- metadata$CreateDate
  
  # Parse the date using the appropriate format
  date <- as.Date(strptime(createDate, format = "%Y:%m:%d %H:%M:%S"))
  
  # Return ID and date as a data frame row
  data.frame(ID = id, Date = date, stringsAsFactors = FALSE)
}))

# Remove duplicate entries
idDateData <- unique(idDateData)

# Append the data to the existing CSV file
write.table(idDateData, "/Users/marco/GitHub/graslandvielfalt/R_files/drone/plots_wDronePic.csv",
            sep = ",", col.names = !file.exists("/Users/marco/GitHub/graslandvielfalt/R_files/drone/plots_wDronePic.csv"),
            row.names = FALSE, append = TRUE)

# Print the data frame
print(idDateData)



