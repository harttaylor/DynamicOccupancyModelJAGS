# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array

load("Data/allCLcovs24Jan2024.Rdata")
#length(which(is.na(dets_array)))#1042

# Get harvest related covariates to test the harvest*age interaction 
yearly_covariates <- read.csv("Data/UnscaledCovariatesJan31.csv")

# Select relevant covariates along with site names (SS column) and year and prepare for harvest covs array 
harvest_covariates <- yearly_covariates[c("SS", "YEAR",
                                          "NEAR.DIST.harvest")]
#, "MEANAGE.565.harvest"
harvest_covariates$NEAR.DIST.harvest <- harvest_covariates$NEAR.DIST.harvest / max(harvest_covariates$NEAR.DIST.harvest)
#harvest_covariates$MEANAGE.565.harvest <- harvest_covariates$MEANAGE.565.harvest / max(harvest_covariates$MEANAGE.565.harvest)
#harvest_covariates$harvest_interaction <- (harvest_covariates$NEAR.DIST.harvest)*(harvest_covariates$MEANAGE.565.harvest)

# Get unique sites and years
sites <- unique(harvest_covariates$SS)
years <- unique(harvest_covariates$YEAR)

# Initialize a 3-dimensional array
# The third dimension is 1 because we only have one covariate (harvest distance)
harvest_distances_array <- array(NA, dim = c(length(sites), length(years), 1))

# Assign names to dimensions for clarity
dimnames(harvest_distances_array) <- list(Site = sites, Year = years, Covariate = "Harvest Distance")

# Fill the array with harvest distances for each site-year combination
for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    # Extract the corresponding row from harvest_covariates
    covariate_row <- harvest_covariates[harvest_covariates$SS == sites[i] & harvest_covariates$YEAR == years[j], "NEAR.DIST.harvest"]
    
    # Check if there is exactly one row for each site-year combination
    if (length(covariate_row) == 1) {
      harvest_distances_array[i, j, 1] <- covariate_row
    }
  }
}


# Select relevant covariates along with site names (SS column) and year and prepare the harvest age matrix 
harvest_covariates <- yearly_covariates[c("SS", "YEAR",
                                          "MEANAGE.565.harvest")]
harvest_covariates$MEANAGE.565.harvest <- harvest_covariates$MEANAGE.565.harvest / max(harvest_covariates$MEANAGE.565.harvest)

# Reshape from long to wide format
harvest_age_matrix <- reshape(harvest_covariates, 
                              idvar = "SS", 
                              timevar = "YEAR", 
                              direction = "wide")

# Extracting only the harvest age values and dropping the site names from the row names
harvest_age_matrix <- data.matrix(harvest_age_matrix[, -1])

# Rename columns to just be the years (optional)
colnames(harvest_age_matrix) <- sub("MEANAGE.565.harvest.", "", colnames(harvest_age_matrix))




# Set up some arrays to run the model 
# Add na.rm = TRUE to the inits function (otherwise most of the intial values will be NA)
inits <- function() { 
  list(z = apply(y, c(1, 2), max, na.rm = TRUE))
}

# Need to make a different survey count array, because of how your data is structured 
nsurv <- array(NA, dim = c(dim(y)[1], dim(y)[2], dim(y)[3]))
for(i in 1:dim(y)[1]){
  for(j in 1:dim(y)[2]){
    s <- which(!is.na(y[i, j, ]))
    nsurv[i, j, 1:length(s)] <- s
  }
}

# Get the actual number of surveys in each year at each point
J <- do.call(rbind, lapply(1:dim(y)[1], function(i){
  sapply(1:dim(y)[2], function(j){
    length(which(!is.na(y[i, j, ])))
  })
}))


# Add covariates on psi 
x.psi <- cbind(rep(1, nrow(first_year_covariates)), first_year_covariates) 


# Add covariates on gamma and phi
# Create x.phi.harvest and x.gamma.harvest arrays for just distance to harvest 
# For harvest covariates 
# Or instead of abova, don't add an intercept, using yearly random effect as intercept instead 
x.phi.harvest <- array(NA, dim = c(dim(harvest_distances_array)[1], dim(harvest_distances_array)[2], dim(harvest_distances_array)[3]))
x.gamma.harvest <- array(NA, dim = c(dim(harvest_distances_array)[1], dim(harvest_distances_array)[2], dim(harvest_distances_array)[3]))
for(i in 1:dim(harvest_distances_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi.harvest[, i, ] <- harvest_distances_array[, i, ]
  x.gamma.harvest[, i, ] <- harvest_distances_array[, i, ]
}



# Create x.phi.harvest and x.gamma.harvest arrays from prepared harvest covariate arrays 
# For harvest covariates 
#x.phi.harvest <- array(NA, dim = c(dim(harvest_covariates_array)[1], dim(harvest_covariates_array)[2], dim(harvest_covariates_array)[3] + 1))
#for(i in 1:dim(harvest_covariates_array)[2]){
#  t <- cbind(rep(1, dim(harvest_covariates_array)[1]), harvest_covariates_array[, i, ])
#  x.phi.harvest[, i, ] <- t
#}

#x.gamma.harvest <- x.phi.harvest

# for no interaction 
#x.phi.harvest <- array(NA, dim = c(dim(nointerac_covariates_array)[1], dim(nointerac_covariates_array)[2], dim(nointerac_covariates_array)[3] + 1))
#for(i in 1:dim(nointerac_covariates_array)[2]){
#  t <- cbind(rep(1, dim(nointerac_covariates_array)[1]), nointerac_covariates_array[, i, ])
#  x.phi.harvest[, i, ] <- t
#}

#x.gamma.harvest <- x.phi.harvest


# Prepare harvest vs no harvest matrix so the model knows which betas to use 
# Get unique sites and years
yearly_covariates <- read.csv("Data/harvestages.csv")
unique_sites = unique(yearly_covariates$SS)
unique_years = unique(yearly_covariates$YEAR)
num_sites = length(unique_sites)
num_years = length(unique_years)

harvest <- matrix(2, nrow = num_sites, ncol = num_years)  # default to 2 (not harvested)

for (row in 1:nrow(yearly_covariates)) {
  site_index <- which(unique_sites == yearly_covariates$SS[row])
  year_index <- which(unique_years == yearly_covariates$YEAR[row])
  if (yearly_covariates$MEANAGE.565.harvest[row] < 30) {
    harvest[site_index, year_index] <- 1  # Set to 1 if harvested
  }
}

# Add covariates on p
# Adding an intercept
ncovariates_with_intercept <- dim(detection_covariates_array)[4] + 1

# Initialize x.p with an additional dimension for the intercept
x.p <- array(NA, dim = c(dim(detection_covariates_array)[1], dim(detection_covariates_array)[2], 
                         dim(detection_covariates_array)[3], ncovariates_with_intercept))

# Populate x.p
for (i in 1:dim(x.p)[1]) {
  for (k in 1:dim(x.p)[2]) {
    for (j in 1:dim(x.p)[3]) {
      # Inserting intercept as the first covariate
      x.p[i, k, j, 1] <- 1  # Intercept
      # Fill in other covariates
      x.p[i, k, j, 2:ncovariates_with_intercept] <- detection_covariates_array[i, k, j, ]
    }
  }
}

# Create the site x year indicator matrix
# indicator of whether the species was ever detected at that site in that year, used in the likelihood calculation in jags model 
ind = apply(y, c(1, 2), max, na.rm = TRUE)

# Run model for harvest with the interaction effect and random year effects and tell model that when there is no harvest for a site x year 
# combination that harvest age wont have variability (priors are set to 0 variability when there is no harvest(2's))
#alpha is intercept now and delta is coefficient for the age which priors vary depending on whether harvest is 1 or 2 at site 

params <- c("beta.psi", "delta.phi", "beta.phi", "delta.gamma", "beta.gamma", "beta.p",  "alpha.phi", "alpha.gamma", "phi", "gamma", "psi", "N", "z", "muZ")


# MCMC settings
ni <- 24000
nt <- 1
nb <- 12000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi.harvest = x.phi.harvest, 
                 nbeta.phi = dim(x.phi.harvest)[3], x.gamma.harvest = x.gamma.harvest, nbeta.gamma = dim(x.gamma.harvest)[3], x.p = x.p, nbeta.p = dim(x.p)[4], 
                 harvest = harvest, harvest_age = harvest_age_matrix)


system.time({
  out_harvRE <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                       model.file = "HarvestModel_RE.txt", n.chains = nc, 
                       n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_harvRE)


saveRDS(out_harvRE, "Results/harvest_RE_24000.rds")





# Run model for harvest WITHOUT the interaction effect and no random year effects 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ")


# MCMC settings
ni <- 24000
nt <- 1
nb <- 12000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi.harvest = x.phi.harvest, 
                 nbeta.phi = dim(x.phi.harvest)[3], x.gamma.harvest = x.gamma.harvest, nbeta.gamma = dim(x.gamma.harvest)[3], x.p = x.p, nbeta.p = dim(x.p)[4], harvest = harvest)


system.time({
  out_harvest <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                    model.file = "Harvest_Model.txt", n.chains = nc, 
                    n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_harvest)



saveRDS(harvest_X, file = "harvest_X_resultsFeb1.rds")





# Run harvest model only with sites that have harvest 
# Set up some required arrays
load("Feb27Covariates_for_sites_w_harvest_only.Rdata")
load("Feb27harvestedsitesonly_dets_array.RData")


# Set up some arrays to run the model 
# Add na.rm = TRUE to the inits function (otherwise most of the intial values will be NA)
inits <- function() { 
  list(z = apply(y, c(1, 2), max, na.rm = TRUE))
}

# Need to make a different survey count array, because of how your data is structured 
nsurv <- array(NA, dim = c(dim(y)[1], dim(y)[2], dim(y)[3]))
for(i in 1:dim(y)[1]){
  for(j in 1:dim(y)[2]){
    s <- which(!is.na(y[i, j, ]))
    nsurv[i, j, 1:length(s)] <- s
  }
}

# Get the actual number of surveys in each year at each point
J <- do.call(rbind, lapply(1:dim(y)[1], function(i){
  sapply(1:dim(y)[2], function(j){
    length(which(!is.na(y[i, j, ])))
  })
}))

# Add covariates on psi 
x.psi <- cbind(rep(1, nrow(first_year_covariates)), first_year_covariates) 

# Create x.phi and x.gamma arrays for distance to edge covariates 
x.phi <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3] + 1))
for(i in 1:dim(yearly_covariates_array)[2]){
  t <- cbind(rep(1, dim(yearly_covariates_array)[1]), yearly_covariates_array[, i, ])
  x.phi[, i, ] <- t
}

x.gamma <- x.phi

# Run this if you are adding random year effect as intercept 
x.phi.nointercept <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
x.gamma.nointercept <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
for(i in 1:dim(yearly_covariates_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi.nointercept[, i, ] <- yearly_covariates_array[, i, ]
  x.gamma.nointercept[, i, ] <- yearly_covariates_array[, i, ]
}

# Add covariates on p
# Adding an intercept
ncovariates_with_intercept <- dim(detection_covariates_array)[4] + 1

# Initialize x.p with an additional dimension for the intercept
x.p <- array(NA, dim = c(dim(detection_covariates_array)[1], dim(detection_covariates_array)[2], 
                         dim(detection_covariates_array)[3], ncovariates_with_intercept))

# Populate x.p
for (i in 1:dim(x.p)[1]) {
  for (k in 1:dim(x.p)[2]) {
    for (j in 1:dim(x.p)[3]) {
      # Inserting intercept as the first covariate
      x.p[i, k, j, 1] <- 1  # Intercept
      # Fill in other covariates
      x.p[i, k, j, 2:ncovariates_with_intercept] <- detection_covariates_array[i, k, j, ]
    }
  }
}

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "alpha.phi", "alpha.gamma", "phi", "gamma", "psi", "N")


# MCMC settings
ni <- 20000
nt <- 1
nb <- 10000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi.nointercept, 
                 nbeta.phi = dim(x.phi.nointercept)[3], x.gamma = x.gamma.nointercept, nbeta.gamma = dim(x.gamma.nointercept)[3], x.p = x.p, nbeta.p = dim(x.p)[4])

system.time({
  harvestsitesonly_outRE <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                               model.file = "DistancetoEdge_RE.txt", n.chains = nc, 
                               n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(harvestsitesonly_outRE)
saveRDS(harvestsitesonly_out, "Results/HARVOUT_sites_w_out_harvest_removed.rds")






# Get harvest related covariates to test the harvest*age interaction 
yearly_covariates <- read.csv("Data/Distancetoedge_noOldHARVEST.csv")
sites_w_harvest <- unique(yearly_covariates$SS)
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

# Now get stand variables for initial year 
firstyearcovs <- read.csv("Data/Nov28CLcovariates1993only.csv")
firstyearcovs <- firstyearcovs[c("SS", "WhiteSpruce", "whitespruce.s", "Age", "age.s", "whitespruce.s.2")]
firstyearcovs <- firstyearcovs[firstyearcovs$SS %in% sites_w_harvest, ]
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
detcovs <- detcovs[detcovs$SS %in% sites_w_harvest, ]
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


head(detection_covariates_array)


save(detection_covariates_array, yearly_covariates_array, first_year_covariates, sites_w_harvest, file = "Feb27Covariates_for_sites_w_harvest_only.Rdata")


# prepare y matrix
# Filter dets to include only the rows that are in sites_w_harvest
library(reshape2)
library(abind)
library(dplyr)
library(tidyverse)
dets_filtered <- dets[dets$SS %in% sites_w_harvest, ]

# Organize the dataframe into an array for running in JAGS 

# Step 0: Create a unique site_id if it doesn't exist
# It's assumed that the 'SS' column contains a unique identifier for each site.
if (!"site_id" %in% names(dets_filtered)) {
  dets_filtered$site_id <- as.numeric(as.factor(dets_filtered$SS))
}

# Extract the unique detection columns that match the pattern X{year}_{visit}
detection_columns <- grep("^X[0-9]+_[1-4]$", names(dets_filtered), value = TRUE)

# Step 1: Create a dataframe to extract year and visit information
years_visits_df <- do.call(rbind, strsplit(detection_columns, "_"))
years <- as.numeric(gsub("^X", "", years_visits_df[, 1]))
visits <- as.numeric(years_visits_df[, 2])

# Check if there are any NAs introduced
any(is.na(years))
any(is.na(visits))

# Step 2: Melt the dataframe to long format
dets_long <- melt(dets_filtered, id.vars = "site_id", measure.vars = detection_columns)

# Ensure that the years and visits are matched correctly
dets_long$year <- as.numeric(gsub("^X", "", gsub("_.*", "", dets_long$variable)))
dets_long$visit <- as.numeric(gsub(".*_", "", dets_long$variable))

# Check the unique values to ensure there are no NAs
unique(dets_long$year)
unique(dets_long$visit)

# Step 3: Cast the molten data into an array with the correct dimensions (114 sites x 25 years x 4 visits)
dets_array <- acast(dets_long, site_id ~ year ~ visit, value.var = "value")

# Ensure that the array has the correct dimensions
expected_dims <- c(90, 25, 4) # 114 sites, 25 years, 4 visits

# If the array does not have the expected dimensions, we will adjust it
actual_dims <- dim(dets_array)
if (is.null(actual_dims) || !all(actual_dims == expected_dims)) {
  # Create an array filled with NA values
  dets_array <- array(NA, dim = expected_dims)
  
  # Fill the array with the existing data
  for (i in seq_along(dets_long$site_id)) {
    site <- dets_long$site_id[i]
    year <- dets_long$year[i]
    visit <- dets_long$visit[i]
    value <- dets_long$value[i]
    # Ensure indices are within the bounds
    if (site <= expected_dims[1] && year <= expected_dims[2] && visit <= expected_dims[3]) {
      dets_array[site, year, visit] <- value
    }
  }
}
str(dets_array)
save(y, dets, sites_w_harvest, file = "Feb27harvestedsitesonly_dets_array.RData")

y <- dets_array