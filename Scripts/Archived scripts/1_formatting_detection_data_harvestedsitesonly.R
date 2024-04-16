# Loading necessary library
library(tidyr)
library(dplyr)

# Run harvest model only with sites that have harvest 

# Get harvest related covariates to test the harvest*age interaction 
yearly_covariates <- read.csv("Data/UnscaledCovariatesJan31.csv")

# Select relevant covariates along with site names (SS column) and year and prepare for harvest covs array 
yearly_covariates <- yearly_covariates[c("SS", "YEAR",
                                         "NEAR.DIST.harvest", "MEANAGE.565.harvest")]
#, "MEANAGE.565.harvest"
yearly_covariates$NEAR.DIST.harvest <- yearly_covariates$NEAR.DIST.harvest / max(yearly_covariates$NEAR.DIST.harvest)
yearly_covariates$MEANAGE.565.harvest <- yearly_covariates$MEANAGE.565.harvest / max(yearly_covariates$MEANAGE.565.harvest)
yearly_covariates$harvest_interaction <- (yearly_covariates$NEAR.DIST.harvest)*(yearly_covariates$MEANAGE.565.harvest)

# Make an array for distance variables 
# Extract unique sites and years
sites <- unique(yearly_covariates$SS)
years <- unique(yearly_covariates$YEAR)

# Change as needed when adding in the quadratic distances or log distances 
num_covariates <- 3

# Initialize the array with NA values
yearly_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.harvest", "MEANAGE.565.harvest", "harvest_interaction")]
      yearly_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } 
  }
}



# Prepare detection data 
# Reading the CSV files that were created to run MSOM in unmarked a long time ago - they are in wide format 

btnw_data <- read.csv("Data/6_rearranged.BTNW.csv")
julian_data <- read.csv("Data/8_rearranged.julian.csv")
time_data <- read.csv("Data/14_rearranged.time.csv")

# Remove the first column 
btnw_data <- btnw_data[,-1]


# Filter for relevant sites
btnw_data <- btnw_data[btnw_data$SS %in% sites_w_harvest,]
julian_data <- julian_data[julian_data$SS %in% sites_w_harvest,]
time_data <- time_data[time_data$SS %in% sites_w_harvest,]


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

saveRDS(detections_array, file = "Data/harvestmodel_detection_history_array.rds")


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
saveRDS(detection_covariates_array, file = "Data/harvestmodel_detection_covariates_array.rds")
