# Add proximity covariates 
# Load dets array to get 'sites_to_kep' object
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
load("Data/dets_array.Rdata")

#yearly_covariates <- read.csv("Data/UnscaledCovariatesJan31.csv") # These are the final covariates where when there is both pipe and road, pipe is set to 1 (1km away), and they only have 114 sites and year 2004 is removed

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
# Add log distances to test the log relationship 
yearly_covariates$log.SEIS <- log(distance_covariates$NEAR.DIST.conventional.seismic + 1)
yearly_covariates$log.ROAD <- log(distance_covariates$NEAR.DIST.unimproved.road + 1)
yearly_covariates$log.PIPE <- log(distance_covariates$NEAR.DIST.pipeline + 1)
yearly_covariates$log.HARV <- log(distance_covariates$NEAR.DIST.harvest + 1)

# try with quadratic distances 
yearly_covariates$seis.2 <- (yearly_covariates$NEAR.DIST.conventional.seismic)^2
yearly_covariates$road.2 <- (yearly_covariates$NEAR.DIST.unimproved.road)^2
yearly_covariates$pipe.2 <- (yearly_covariates$NEAR.DIST.pipeline)^2
yearly_covariates$harv.2 <- (yearly_covariates$NEAR.DIST.harvest)^2

# Yearly covariates (human footprint and treatment)as a matrix (format for jags model) 
# Extract unique sites and years
sites <- unique(yearly_covariates$SS)
years <- unique(yearly_covariates$YEAR)


# Make an array for distance variables 
# Change as needed when adding in the quadratic distances or log distances 
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
    } 
  }
}

#"log.SEIS", "log.ROAD", "log.PIPE", "log.HARV"
#"seis.2", "road.2", "pipe.2", "harv.2"




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
library(dplyr)
detcovs <- read.csv("Data/detcovs.csv")

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

## ---- Detection covariates - format data in a matrix to model detection ----
# Read in the detection covariates
detcovs <- read.csv("Data/detcovs.csv")

# Filtering
detcovs <- detcovs[detcovs$YYYY != "2004",]
detcovs <- detcovs[detcovs$SS %in% sites_to_keep, ]
detcovsready <- select(detcovs, c("SS", "ROUND", "YYYY", "jday", "tssr"))

# Extract unique sites and years
unique_sites <- unique(detcovsready$SS)
unique_years <- sort(unique(detcovsready$YYYY))  # Sorting years
nsite <- length(unique_sites)
nyear <- length(unique_years)
nvisits <- 4  # Maximum number of visits per site per year
ncovariates <- 2  # Number of covariates

# Initialize the 4D array with NAs
detection_covariates_array <- array(NA, dim = c(nsite, nyear, nvisits, ncovariates))

# Populate the array
for (site in 1:nsite) {
  for (year in 1:nyear) {
    # Subset data for the current site and year
    site_year_data <- detcovsready[detcovsready$SS == unique_sites[site] & detcovsready$YYYY == unique_years[year], ]
    
    # Loop through the maximum number of visits
    for (visit in 1:nvisits) {
      # Check if this visit is present in the data
      if (visit %in% site_year_data$ROUND) {
        # Get data for this specific visit
        visit_data <- site_year_data[site_year_data$ROUND == visit, ]
        
        # Check if visit_data contains exactly one row
        if (nrow(visit_data) == 1) {
          detection_covariates_array[site, year, visit, 1] <- visit_data$jday
          detection_covariates_array[site, year, visit, 2] <- visit_data$tssr
        } 
    }
  }
  }
}

head(detection_covariates_array)

save(detection_covariates_array, yearly_covariates_array, first_year_covariates, sites_to_keep, file = "covariatearraysFeb16.Rdata")

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
