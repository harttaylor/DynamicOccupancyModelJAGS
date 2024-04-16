
# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

# Set up some required arrays

load("covariatearraysFeb19.Rdata")


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
# Format distances to edges depending on what distance relationship you are testing 
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
# Add log distances to test the log relationship 
yearly_covariates$log.SEIS <- log(distance_covariates$NEAR.DIST.conventional.seismic + 1)
yearly_covariates$log.ROAD <- log(distance_covariates$NEAR.DIST.unimproved.road + 1)
yearly_covariates$log.PIPE <- log(distance_covariates$NEAR.DIST.pipeline + 1)
yearly_covariates$log.HARV <- log(distance_covariates$NEAR.DIST.harvest + 1)

# Inverse relationships
yearly_covariates$inv.SEIS <- 1 / (distance_covariates$NEAR.DIST.conventional.seismic + 1)
yearly_covariates$inv.ROAD <- 1 / (distance_covariates$NEAR.DIST.unimproved.road + 1)
yearly_covariates$inv.PIPE <- 1 / (distance_covariates$NEAR.DIST.pipeline + 1)
yearly_covariates$inv.HARV <- 1 / (distance_covariates$NEAR.DIST.harvest + 1)

# Square root relationships
yearly_covariates$sqrt.SEIS <- sqrt(distance_covariates$NEAR.DIST.conventional.seismic + 1)
yearly_covariates$sqrt.ROAD <- sqrt(distance_covariates$NEAR.DIST.unimproved.road + 1)
yearly_covariates$sqrt.PIPE <- sqrt(distance_covariates$NEAR.DIST.pipeline + 1)
yearly_covariates$sqrt.HARV <- sqrt(distance_covariates$NEAR.DIST.harvest + 1)


# Make an array for distance variables 
# Extract unique sites and years
sites <- unique(yearly_covariates$SS)
years <- unique(yearly_covariates$YEAR)

# Change as needed when adding in the quadratic distances or log distances 
num_covariates <- 4

# Initialize the array with NA values
yearly_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("inv.SEIS", "inv.ROAD", "inv.PIPE", "inv.HARV")]
      yearly_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } 
  }
}

#"log.SEIS", "log.ROAD", "log.PIPE", "log.HARV"
#"inv.SEIS", "inv.ROAD", "inv.PIPE", "inv.HARV"
#"sqrt.SEIS", "sqrt.ROAD", "sqrt.PIPE", "sqrt.HARV"
#"NEAR.DIST.conventional.seismic", "NEAR.DIST.unimproved.road", "NEAR.DIST.pipeline","NEAR.DIST.harvest"

# Create x.phi and x.gamma arrays for distance to edge covariates 
x.phi <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3] + 1))
for(i in 1:dim(yearly_covariates_array)[2]){
  t <- cbind(rep(1, dim(yearly_covariates_array)[1]), yearly_covariates_array[, i, ])
  x.phi[, i, ] <- t
}

x.gamma <- x.phi

# Run this if you are adding random year effect as intercept 
x.phi <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
x.gamma <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
for(i in 1:dim(yearly_covariates_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi[, i, ] <- yearly_covariates_array[, i, ]
  x.gamma[, i, ] <- yearly_covariates_array[, i, ]
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

# Run distance to edge model with no random year effect 
# But with a log effect 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "alpha.gamma", "alpha.phi", "psi", "N", "z", "muZ", "tau")


# MCMC settings
ni <- 100
nt <- 1
nb <- 50
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  out_edge <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                            model.file = "DistancetoEdgeModel.txt", n.chains = nc, 
                            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_edge)
saveRDS(RElog_edge, file = "Results/QUADEDGE_resultsFeb12.rds")

RElog_edge <- readRDS("Results/EDGE_resultsFeb3.rds")



