# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

load("covariatearraysFeb16.Rdata")
detection_history <- readRDS("Data/detection_history_array.rds")
y <- detection_history
detection_covariates_array <- readRDS("Data/detection_covariates_arrayV2.rds")

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

# Initialize a 2-dimensional matrix for harvest distance 
harvest_distances_matrix <- matrix(NA, nrow = length(sites), ncol = length(years))

# Assign row and column names to the matrix for clarity
rownames(harvest_distances_matrix) <- sites
colnames(harvest_distances_matrix) <- years

# Fill the matrix with harvest distances for each site-year combination
for (site in sites) {
  for (year in years) {
    # Extract the corresponding harvest distance value
    covariate_value <- harvest_covariates$NEAR.DIST.harvest[harvest_covariates$SS == site & harvest_covariates$YEAR == year]
    
    # Check if there is exactly one value for each site-year combination
    if (length(covariate_value) == 1) {
      harvest_distances_matrix[site, as.character(year)] <- covariate_value
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

params <- c("beta.psi", "delta.phi", "beta.phi", "delta.gamma", "beta.gamma", "beta.p",  "alpha.phi", "alpha.gamma", "N", "z", "muZ")


# MCMC settings
ni <- 100
nt <- 1
nb <- 50
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.p = x.p, nbeta.p = dim(x.p)[4], 
                 harvest = harvest, harvest_age = harvest_age_matrix, harvest_distance = harvest_distances_matrix)


system.time({
  out_harvRE <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                     model.file = "HarvestModel_V2.txt", n.chains = nc, 
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_harvRE)


saveRDS(out_harvRE, "Results/harvest_RE_24000.rds")





# Run model for harvest WITHOUT the interaction effect and no random year effects 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ")


# MCMC settings
ni <- 100
nt <- 1
nb <- 50
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

