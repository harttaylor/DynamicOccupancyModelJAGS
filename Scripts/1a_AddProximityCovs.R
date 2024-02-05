# Add proximity covariates 
# Load dets array to get 'sites_to_kep' object
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
#load("Data/dets_array.Rdata")

yearly_covariates <- read.csv("Data/UnscaledCovariatesJan31.csv") # These are the final covariates where when there is both pipe and road, pipe is set to 1 (1km away), and they only have 114 sites and year 2004 is removed

# Select relevant covariates along with site names (SS column) and year
distance_covariates <- yearly_covariates[c("SS", "YEAR", "NEAR.DIST.conventional.seismic", 
                                           "NEAR.DIST.unimproved.road", 
                                           "NEAR.DIST.pipeline",
                                           "NEAR.DIST.harvest")]

# Identify the columns to scale (exclude 'SS' and 'YEAR' columns)
covariate_columns_to_scale <- names(distance_covariates)[!names(distance_covariates) %in% c("SS", "YEAR")]

# Divide covariates by 1000 to 'scale' them 
scaled_covariates <- as.data.frame(lapply(covariate_columns_to_scale, function(column_name) {
  distance_covariates[[column_name]] / 1000
}))

# Retain original column names for the scaled covariates
names(scaled_covariates) <- covariate_columns_to_scale

# Combine the unscaled 'SS' and 'YEAR' columns with the scaled covariates
yearly_covariates <- cbind(distance_covariates[c("SS", "YEAR")], scaled_covariates)
yearly_covariates$log.SEIS <- log(distance_covariates$NEAR.DIST.conventional.seismic + 0.001)
yearly_covariates$log.ROAD <- log(distance_covariates$NEAR.DIST.unimproved.road + 0.001)
yearly_covariates$log.PIPE <- log(distance_covariates$NEAR.DIST.pipeline + 0.001)
yearly_covariates$log.HARV <- log(distance_covariates$NEAR.DIST.harvest + 0.001)

# Yearly covariates (human footprint and treatment)as a matrix (format for jags model) 
# Extract unique sites and years
sites <- unique(yearly_covariates$SS)
years <- unique(yearly_covariates$YEAR)


# Make an array for distance variables 
num_covariates <- 4

# Initialize the array with NA values
yearly_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.conventional.seismic", 
                                      "NEAR.DIST.unimproved.road", 
                                      "NEAR.DIST.pipeline",
                                      "NEAR.DIST.harvest")]
      yearly_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } else {
      # Print for diagnostic purposes
      print(paste("No data for site:", sites[i], "year:", years[j]))
      yearly_covariates_array[i, j, ] <- rep(NA, num_covariates)
    }
  }
}

#"log.SEIS", "log.ROAD", "log.PIPE", "log.HARV"

# Make an array for the log distances 
distance_columns <- c("NEAR.DIST.conventional.seismic", 
                      "NEAR.DIST.unimproved.road", 
                      "NEAR.DIST.pipeline",
                      "NEAR.DIST.harvest")

log_covariates <- as.data.frame(lapply(distance_columns, function(column_name) {
  log(yearly_covariates[[column_name]] + 0.001)
}))

# Original column names for the log-transformed covariates
names(log_covariates) <- paste("LOG", distance_columns, sep=".")

# Combine the 'SS' and 'YEAR' columns with the log-transformed covariates
yearly_covariates_log <- cbind(yearly_covariates[c("SS", "YEAR")], log_covariates)

# Preparing the array for log-transformed distance variables
log_dist_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for(i in 1:length(sites)) {
  for(j in 1:length(years)) {
    covs_rows <- yearly_covariates_log[yearly_covariates_log$SS == sites[i] & yearly_covariates_log$YEAR == years[j], ]
    
    if(nrow(covs_rows) > 0) {
      first_row <- covs_rows[1,]
      log_covariates <- first_row[, paste("LOG", distance_columns, sep=".")]
      log_dist_array[i, j, ] <- as.numeric(log_covariates)  # Assign the log-transformed covariates here
    }
  }
}





# Get harvest related covariates to test the harvest*age interaction 
yearly_covariates <- read.csv("UnscaledCovariatesJan31.csv")

# Select relevant covariates along with site names (SS column) and year and prepare for harvest covs array 
harvest_covariates <- yearly_covariates[c("SS", "YEAR",
                                           "NEAR.DIST.harvest", "MEANAGE.565.harvest")]
harvest_covariates$NEAR.DIST.harvest <- harvest_covariates$NEAR.DIST.harvest / max(harvest_covariates$NEAR.DIST.harvest)
harvest_covariates$MEANAGE.565.harvest <- harvest_covariates$MEANAGE.565.harvest / max(harvest_covariates$MEANAGE.565.harvest)
harvest_covariates$harvest_interaction <- (harvest_covariates$NEAR.DIST.harvest)*(harvest_covariates$MEANAGE.565.harvest)

# Yearly covariates (human footprint and treatment)as a matrix (format for jags model) 
# Extract unique sites and years
sites <- unique(harvest_covariates$SS)
years <- unique(harvest_covariates$YEAR)


# Make an array for distance variables 
num_covariates <- 3

# Initialize the array with NA values
harvest_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- harvest_covariates[harvest_covariates$SS == sites[i] & harvest_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.harvest", "MEANAGE.565.harvest",
                                      "harvest_interaction")]
      harvest_covariates_array[i, j, ] <- as.numeric(covariate_data)
    }
  }
}

# Make an array for no-interaction harvest model   
num_covariates <- 2

# Initialize the array with NA values
nointerac_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- harvest_covariates[harvest_covariates$SS == sites[i] & harvest_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.harvest", "MEANAGE.565.harvest")]
      nointerac_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } 
  }
}


##----- First year covariates----
# Now get stand variables for initial year 
firstyearcovs <- read.csv("Nov28CLcovariates1993only.csv")
firstyearcovs <- firstyearcovs[c("SS", "WhiteSpruce", "whitespruce.s", "Age", "age.s", "whitespruce.s.2")]
firstyearcovs <- firstyearcovs[firstyearcovs$SS %in% sites_to_keep, ]
mean(firstyearcovs$Age)
sd(firstyearcovs$Age)
min(firstyearcovs$WhiteSpruce)
max(firstyearcovs$WhiteSpruce)
# Prepare variables for dynamic occupancy model 

# First year covariates (percentconifer and standage)
firstyearcovs$percentconifer = firstyearcovs$WhiteSpruce  
firstyearcovs$standage = firstyearcovs$age.s
firstyearcovs$pecoquadratic = firstyearcovs$whitespruce.s.2
#firstyearcovs$Treatment <- as.numeric(as.factor(firstyearcovs$Treatment))
firstyearcovs <- firstyearcovs[c("SS", "percentconifer","pecoquadratic", "standage")]
first_year_covariates <- as.matrix(firstyearcovs[, -1]) # Exclude 'SS' column

# Assuming 'first_year_covariates' is a matrix or data frame
# Extract 'percentconifer' and 'standage'
percentconifer <- first_year_covariates[, "percentconifer"]  # Adjust the column name if needed
standage <- first_year_covariates[, "standage"]              # Adjust the column name if needed
pecoquadratic <- first_year_covariates[, "pecoquadratic"]
# Now check their lengths
print(length(percentconifer))
print(length(standage))
print(length(pecoquadratic))


## ---- Detection covariates - format data in a matrix to model detection ----
detcovs <- read.csv("detcovs.csv")

#Now it should be ready to go!
detcovs <- detcovs[detcovs$YYYY != "2004",]
detcovs <- detcovs[detcovs$SS %in% sites_to_keep, ]
detcovsready <- select(detcovs, c("SS", "YYYY", "jday", "tssr"))

# Extract unique sites and years
unique_sites <- unique(detcovsready$SS)
unique_years <- sort(unique(detcovsready$YYYY))  # Sorting years just in case
nsite <- length(unique_sites)
nyear <- length(unique_years)
nvisits <- 4  # Maximum number of visits per site per year
ncovariates <- 2  # JULIAN and MIN_SUN

# Initialize the 4D array
detection_covariates_array <- array(NA, dim = c(nsite, nyear, nvisits, ncovariates))

# Populate the array
for (site in 1:nsite) {
  for (year in 1:nyear) {
    # Subset data for the current site and year
    site_year_data <- detcovsready[detcovsready$SS == unique_sites[site] & detcovsready$YYYY == unique_years[year], ]
    
    # Loop through each visit
    for (visit in 1:min(nrow(site_year_data), nvisits)) {
      detection_covariates_array[site, year, visit, 1] <- site_year_data[visit, "jday"]
      detection_covariates_array[site, year, visit, 2] <- site_year_data[visit, "tssr"]
    }
  }
}

save(detection_covariates_array, harvest_covariates_array, nonharvest_covariates_array , first_year_covariates, sites_to_keep, max_values, min_values, file = "harvestvsnoharvestcovariates29Jan2024.Rdata")





#save(detection_covariates_array, yearly_covariates_array, first_year_covariates, site_list, file = "allCLcovsRaggedArray.Rdata")

# Check for multicollinearity using VIF 
library(car)

linear_model <- lm(MEANAGE.565.harvest ~ NEAR.DIST.harvest + NEAR.DIST.conventional.seismic + NEAR.DIST.unimproved.road + NEAR.DIST.pipeline, data = yearly_covs)
lm <- lm(MEANAGE.565.harvest ~ NEAR.DIST.harvest + NEAR.DIST.conventional.seismic + NEAR.DIST.unimproved.road + NEAR.DIST.pipeline +
           covs$PROP150.conventional.seismic + covs$PROP565.conventional.seismic + covs$PROP150.unimproved.road + 
         covs$PROP150.harvest + covs$PROP565.harvest + covs$PROP150.pipeline + covs$PROP565.pipeline, data = covs)
# Calculate VIF
vif_values <- vif(lm)

# Print the VIF values
print(vif_values)

#NEAR.DIST.harvest    NEAR.DIST.conventional.seismic         NEAR.DIST.unimproved.road                NEAR.DIST.pipeline 
#3.363738                          2.334334                          7.782104                          3.589904 
#covs$PROP150.conventional.seismic covs$PROP565.conventional.seismic      covs$PROP150.unimproved.road      covs$PROP565.unimproved.road 
#1.974856                          2.419600                          1.719772                          8.364695 
#covs$PROP150.harvest              covs$PROP565.harvest             covs$PROP150.pipeline             covs$PROP565.pipeline 
#1.418767                          3.434395                          2.016130                          3.928257 
