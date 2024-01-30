# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/COLEXT model/Conventionalway")
library(jagsUI)
load("Jan24DynoccModel.RData")
# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array

# Get the covariate arrays 
load("Data/allCLcovs24Jan2024.Rdata")


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

# Create x.phi.harvest and x.gamma.harvest arrays from already prepared yearly covariate arrays 
# For harvest covariates 
x.phi.harvest <- array(NA, dim = c(dim(harvest_covariates_array)[1], dim(harvest_covariates_array)[2], dim(harvest_covariates_array)[3] + 1))
for(i in 1:dim(harvest_covariates_array)[2]){
  t <- cbind(rep(1, dim(harvest_covariates_array)[1]), harvest_covariates_array[, i, ])
  x.phi.harvest[, i, ] <- t
}

x.gamma.harvest <- x.phi.harvest


# Or instead of abova, don't add an intercept, using yearly random effect as intercept instead 
x.phi.harvest.nointercept <- array(NA, dim = c(dim(harvest_covariates_array)[1], dim(harvest_covariates_array)[2], dim(harvest_covariates_array)[3]))
x.gamma.harvest.nointercept <- array(NA, dim = c(dim(harvest_covariates_array)[1], dim(harvest_covariates_array)[2], dim(harvest_covariates_array)[3]))
for(i in 1:dim(harvest_covariates_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi.harvest.nointercept[, i, ] <- harvest_covariates_array[, i, ]
  x.gamma.harvest.nointercept[, i, ] <- harvest_covariates_array[, i, ]
}



# Create x.phi and x.gamma arrays for non harvest covariates 
x.phi <- array(NA, dim = c(dim(nonharvest_covariates_array)[1], dim(nonharvest_covariates_array)[2], dim(nonharvest_covariates_array)[3] + 1))
for(i in 1:dim(nonharvest_covariates_array)[2]){
  t <- cbind(rep(1, dim(nonharvest_covariates_array)[1]), nonharvest_covariates_array[, i, ])
  x.phi[, i, ] <- t
}

x.gamma <- x.phi

# With no intercept here too
x.phi.nointercept <- array(NA, dim = c(dim(nonharvest_covariates_array)[1], dim(nonharvest_covariates_array)[2], dim(nonharvest_covariates_array)[3]))
x.gamma.nointercept <- array(NA, dim = c(dim(nonharvest_covariates_array)[1], dim(nonharvest_covariates_array)[2], dim(nonharvest_covariates_array)[3]))
for(i in 1:dim(nonharvest_covariates_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi.nointercept[, i, ] <- nonharvest_covariates_array[, i, ]
  x.gamma.nointercept[, i, ] <- nonharvest_covariates_array[, i, ]
}

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

params <- c("beta.psi", "beta.phi", "delta.phi", "beta.gamma", "delta.gamma", "beta.p",  "alpha.phi", "alpha.gamma", "phi", "gamma", "N", "z", "muZ")
#params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "alpha.phi", "alpha.gamma", "psi", "phi", "gamma", "N", "z", "muZ")


# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), 
                 x.phi = x.phi.nointercept, x.phi.harvest = x.phi.harvest.nointercept, ndelta.phi = dim(x.phi.nointercept)[3], nbeta.phi = dim(x.phi.harvest.nointercept)[3], 
                 x.gamma.harvest = x.gamma.harvest.nointercept, x.gamma = x.gamma.nointercept, ndelta.gamma = dim(x.gamma.nointercept)[3], nbeta.gamma = dim(x.gamma.harvest.nointercept)[3],
                 x.p = x.p, nbeta.p = dim(x.p)[4], harvest = harvest)

#out_cov_harv2 has distance to harvest depending on the harvest state 
#out_cov_harv3 has distance to harvest as not depending on the harvest state 
system.time({
  out_cov_randomyeareffects <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                       model.file = "cl_model_cov_p2_ancova_most_complicated.txt", n.chains = nc, 
                       n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 
print(out_cov_randomyeareffects)
