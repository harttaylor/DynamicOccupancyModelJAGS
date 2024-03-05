# Loading necessary library
library(tidyr)
library(dplyr)


# Reading the CSV files that were created to run MSOM in unmakred a long time ago - they are in wide format 

btnw_data <- read.csv("Data/6_rearranged.BTNW.csv")
julian_data <- read.csv("Data/8_rearranged.julian.csv")
time_data <- read.csv("Data/14_rearranged.time.csv")

# Remove the first column 
btnw_data <- btnw_data[,-1]

# Extract the 'SS' column as site labels and then remove it from btnw_data
site_labels <- btnw_data$SS
btnw_data <- btnw_data[,-1]

# Define the years to include (excluding year 11 because it wasnt visited)
years <- c(0:10, 12:25)

# Number of sites, years, and visits
num_sites <- length(unique(site_labels))
num_years <- length(years)
num_visits <- 4  # 4 visits per year

# Initialize the array
detections_array <- array(NA, dim = c(num_sites, num_years, num_visits))

# Populate the array
for (site_index in 1:num_sites) {
  for (year_index in 1:num_years) {
    year <- years[year_index]
    for (visit in 1:num_visits) {
      column_name <- paste("X", year, "_", visit, sep = "")
      if (column_name %in% names(btnw_data)) {
        detections_array[site_index, year_index, visit] <- btnw_data[site_labels == unique(site_labels)[site_index], column_name]
      }
    }
  }
}
head(detections_array)
head(detections_array[, 5, ])

saveRDS(detections_array, file = "Data/detection_history_array.rds")


# Now the detection covariates array 
julian_data <- julian_data[,-1]
time_data <- time_data[,-1]

# Extract the 'SS' column from each and then remove it
julian_site_labels <- julian_data$SS
time_site_labels <- time_data$SS
julian_data <- julian_data[,-1]
time_data <- time_data[,-1]

# years to include (excluding year 11)
years <- c(0:10, 12:25)

# Initialize the detection_covariates_array
detection_covariates_array <- array(NA, dim = c(num_sites, num_years, num_visits, 2))

# Populate the array
for (site_index in 1:num_sites) {
  for (year_index in 1:num_years) {
    year <- years[year_index]
    for (visit in 1:num_visits) {
      column_name <- paste("X", year, "_", visit, sep = "")
      
      # Julian data
      if (column_name %in% names(julian_data)) {
        detection_covariates_array[site_index, year_index, visit, 1] <- julian_data[julian_site_labels == unique(site_labels)[site_index], column_name]
      }
      
      # Time data
      if (column_name %in% names(time_data)) {
        detection_covariates_array[site_index, year_index, visit, 2] <- time_data[time_site_labels == unique(site_labels)[site_index], column_name]
      }
    }
  }
}
head(detection_covariates_array)
head(detection_covariates_array[, 5, ,])


# Saving the 4D array as an .rds file
saveRDS(detection_covariates_array, file = "Data/detection_covariates_arrayV2.rds")
