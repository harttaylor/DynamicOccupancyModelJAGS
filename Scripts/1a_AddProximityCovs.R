# Add proximity covariates 
# Load dets array to get 'sites_to_kep' object
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/COLEXT model/Conventionalway")
load("dets_array.Rdata")
yearly_covariates <- read.csv("yearly_covsdist_interaction_19Jan2024.csv")
# Select relevant covariates along with site names (SS column) and year
selected_covariates <- yearly_covariates[c("SS", "YEAR", "NEAR.DIST.conventional.seismic", 
                                           "NEAR.DIST.unimproved.road", 
                                           "NEAR.DIST.pipeline",
                                           "harv", "age")]

unscale <- function(scaled, min, max) {
  (scaled * (max - min)) + min
}

# Assuming you have the mean values of your beta parameters
# For example: mean_beta_phi, mean_beta_gamma

# Unscale the covariates
unscaled_covs <- yearly_covs
unscaled_covs$NEAR.DIST.conventional.seismic <- unscale(yearly_covs$NEAR.DIST.conventional.seismic, min_values["NEAR.DIST.conventional.seismic"], max_values["NEAR.DIST.conventional.seismic"])
unscaled_covs$NEAR.DIST.unimproved.road <- unscale(yearly_covs$NEAR.DIST.unimproved.road, min_values["NEAR.DIST.unimproved.road"], max_values["NEAR.DIST.unimproved.road"])
unscaled_covs$NEAR.DIST.harvest <- unscale(yearly_covs$NEAR.DIST.harvest, min_values["NEAR.DIST.harvest"], max_values["NEAR.DIST.harvest"])
unscaled_covs$NEAR.DIST.pipeline <- unscale(yearly_covs$NEAR.DIST.pipeline, min_values["NEAR.DIST.pipeline"], max_values["NEAR.DIST.pipeline"])
unscaled_covs$MEANAGE.565.harvest <- unscale(yearly_covs$MEANAGE.565.harvest, min_values["MEANAGE.565.harvest"], max_values["MEANAGE.565.harvest"])

yearly_covariates <- read.csv("Nov28CLcovariatesyearly.csv")
yearly_covariates <- yearly_covariates[c("SS", "YEAR", "NEAR.DIST.conventional.seismic", 
                                           "NEAR.DIST.unimproved.road", 
                                           "NEAR.DIST.pipeline", "NEAR.DIST.harvest",
                                           "MEANAGE.565.harvest")]

yearly_covariates <- unscaled_covs

yearly_covariates$harv <- ifelse(yearly_covariates$NEAR.DIST.harvest >0, yearly_covariates$NEAR.DIST.harvest + 1, yearly_covariates$NEAR.DIST.harvest)
yearly_covariates$age <- ifelse(yearly_covariates$MEANAGE.565.harvest > 30, 0, yearly_covariates$MEANAGE.565.harvest)




# Identify the columns to scale (exclude 'SS' and 'YEAR' columns)
covariate_columns_to_scale <- names(selected_covariates)[!names(selected_covariates) %in% c("SS", "YEAR")]

# Scale covariates from 0 to 1
max_values <- apply(selected_covariates[covariate_columns_to_scale], 2, max)
min_values <- apply(selected_covariates[covariate_columns_to_scale], 2, min)

scaled_covariates <- as.data.frame(lapply(covariate_columns_to_scale, function(column_name) {
  (selected_covariates[[column_name]] - min_values[column_name]) / (max_values[column_name] - min_values[column_name])
}))

# Retain original column names for the scaled covariates
names(scaled_covariates) <- covariate_columns_to_scale

# Combine the unscaled 'SS' and 'YEAR' columns with the scaled covariates
final_covariates <- cbind(selected_covariates[c("SS", "YEAR")], scaled_covariates)

# Extract treatment from SS column
#covariates$treatment <- substr(covariates$SS, 4, 4)
#covariates$treatment <- gsub("C", "control", covariates$treatment)
#covariates$treatment <- gsub("F", "fragmented", covariates$treatment)
#covariates$treatment <- gsub("R", "riparian", covariates$treatment)

# Combine scaled covariates with site names and treatment
#yearly_covariates <- cbind(selected_covariates[c("SS", "YEAR")], 
#                           scaled_covariates, treatment = covariates$treatment)


# Filter 'final_covariates' to keep only rows where the site name is in 'sites_to_keep'
yearly_covariates <- yearly_covariates[yearly_covariates$SS %in% sites_to_keep, ]
yearly_covariates <- yearly_covariates[yearly_covariates$YEAR != 2004,]
yearly_covariates$harvest_interaction <- yearly_covariates$harv * yearly_covariates$age
write.csv(yearly_covariates, "harvestages.csv")
# These are the final covariates where when there is both pipe and road, pipe is set to 1 (1km away), harvest distance is +1 so that no values are zero 
# and when age of harvest is 102 it is set  to zero so that there is no effect 
#write.csv(yearly_covariates, "yearly_covsdist_interaction_24Jan2024.csv")



yearly_covariates <- read.csv("yearly_covsdist_interaction_19Jan2024.csv")
# Yearly covariates (human footprint and treatment)as a matrix (format for jags model) 
# Assuming 'yearly_covariates' is your data frame
# Extract unique sites and years
sites <- unique(yearly_covariates$SS)
years <- unique(yearly_covariates$YEAR)

# Convert 'treatment' to binary dummy variables
# treatment_2 is 1 for 'fragmented' and 0 otherwise
#yearly_covariates$treatment_2 <- as.numeric(yearly_covariates$treatment == "fragmented")
# treatment_3 is 1 for 'riparian' and 0 otherwise
#yearly_covariates$treatment_3 <- as.numeric(yearly_covariates$treatment == "riparian")

# Make an array for harvest variables 
num_covariates <- 2  # 5 original covariates + 2 treatment dummies

# Initialize the array with NA values
harvest_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("MEANAGE.565.harvest",
                                      "harvest_interaction")]
      harvest_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } else {
      # Print for diagnostic purposes
      print(paste("No data for site:", sites[i], "year:", years[j]))
      harvest_covariates_array[i, j, ] <- rep(NA, num_covariates)
    }
  }
}

# Make an array for non-harvest variables  
num_covariates <- 4 

# Initialize the array with NA values
nonharvest_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.conventional.seismic", 
                                      "NEAR.DIST.unimproved.road", 
                                      "NEAR.DIST.pipeline", "NEAR.DIST.harvest")]
      nonharvest_covariates_array[i, j, ] <- as.numeric(covariate_data)
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

save(detection_covariates_array, yearly_covariates_array, first_year_covariates, sites_to_keep, max_values, min_values, file = "allCLcovs24Jan2024.Rdata")







# Convert 'treatment' to a factor and then to numeric
#yearly_covariates$treatment_numeric <- as.numeric(as.factor(yearly_covariates$treatment))

# Initialize the array with NA values
#yearly_covariates_array <- array(NA, dim = c(length(sites), length(years), 6))

#for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.conventional.seismic", "NEAR.DIST.unimproved.road", "NEAR.DIST.harvest", "NEAR.DIST.pipeline",
                                      "MEANAGE.565.harvest")]
      treatment_numeric_data <- first_row$treatment_numeric
      yearly_covariates_array[i, j, ] <- c(as.numeric(covariate_data), treatment_numeric_data)
    } else {
      # Print for diagnostic purposes
      print(paste("No data for site:", sites[i], "year:", years[j]))
      yearly_covariates_array[i, j, ] <- rep(NA, 6)
    }
  }
}



head(yearly_covariates_array)
# Check dimensions of the covariates array
print(dim(yearly_covariates_array))

# Keep track of what numnber corresponds to which treatment in the array
treatment_levels <- levels(treatment_data)
treatment_mapping <- setNames(seq_along(treatment_levels), treatment_levels)

# Check a small sample to see if it looks correct
print(head(yearly_covariates_array[1,,]))
# Okay everything looks good, try running the model 


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
