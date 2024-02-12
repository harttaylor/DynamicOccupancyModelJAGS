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
                                          "NEAR.DIST.harvest", "MEANAGE.565.harvest")]

harvest_covariates$NEAR.DIST.harvest <- harvest_covariates$NEAR.DIST.harvest / max(harvest_covariates$NEAR.DIST.harvest)
harvest_covariates$MEANAGE.565.harvest <- harvest_covariates$MEANAGE.565.harvest / max(harvest_covariates$MEANAGE.565.harvest)
harvest_covariates$harvest_interaction <- (harvest_covariates$NEAR.DIST.harvest)*(harvest_covariates$MEANAGE.565.harvest)

# Make an array for the interaction harvest model  
# Get unique sites and years
sites <- unique(harvest_covariates$SS)
years <- unique(harvest_covariates$YEAR)

num_covariates <- 2

# Initialize the array with NA values
interaction_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- harvest_covariates[harvest_covariates$SS == sites[i] & harvest_covariates$YEAR == years[j], ]

    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.harvest", "harvest_interaction")]
      interaction_array[i, j, ] <- as.numeric(covariate_data)
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
x.phi.harvest <- array(NA, dim = c(dim(interaction_array)[1], dim(interaction_array)[2], dim(interaction_array)[3]))
x.gamma.harvest <- array(NA, dim = c(dim(interaction_array)[1], dim(interaction_array)[2], dim(interaction_array)[3]))
for(i in 1:dim(interaction_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi.harvest[, i, ] <- interaction_array[, i, ]
  x.gamma.harvest[, i, ] <- interaction_array[, i, ]
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


# Run model for harvest with the interaction effect and no random year effects and tell model that when there is no harvest for a site x year 
# combination that harvest age wont have variability (priors are set to 0 variability when there is no harvest(2's))
#alpha is intercept now and delta is coefficient for the age which priors vary depending on whether harvest is 1 or 2 at site 

params <- c("beta.psi", "alpha.phi", "delta.phi", "beta.phi", "alpha.gamma", "delta.gamma", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ")


# MCMC settings
ni <- 24000
nt <- 1
nb <- 12000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi.harvest = x.phi.harvest, 
                 nbeta.phi = dim(x.phi.harvest)[3], x.gamma.harvest = x.gamma.harvest, nbeta.gamma = dim(x.gamma.harvest)[3], x.p = x.p, nbeta.p = dim(x.p)[4], 
                 harvest = harvest, harvest_age = harvest_age_matrix)


system.time({
  out_harvest_interaction <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                                  model.file = "Harvest_Model.txt", n.chains = nc, 
                                  n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 
print(out_harvest_interaction)

saveRDS(out_harvest_interaction, "Results/harvest_interaction.rds")
read