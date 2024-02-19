# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array

load("covariatearraysFeb16.Rdata")

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
# yearly_covariates$log.SEIS <- log(distance_covariates$NEAR.DIST.conventional.seismic + 1)
# yearly_covariates$log.ROAD <- log(distance_covariates$NEAR.DIST.unimproved.road + 1)
# yearly_covariates$log.PIPE <- log(distance_covariates$NEAR.DIST.pipeline + 1)
# yearly_covariates$log.HARV <- log(distance_covariates$NEAR.DIST.harvest + 1)

# try with quadratic distances 
# yearly_covariates$seis.2 <- (yearly_covariates$NEAR.DIST.conventional.seismic)^2
# yearly_covariates$road.2 <- (yearly_covariates$NEAR.DIST.unimproved.road)^2
# yearly_covariates$pipe.2 <- (yearly_covariates$NEAR.DIST.pipeline)^2
# yearly_covariates$harv.2 <- (yearly_covariates$NEAR.DIST.harvest)^2


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

# Create x.phi and x.gamma arrays for distance to edge covariates 
x.phi <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3] + 1))
for(i in 1:dim(yearly_covariates_array)[2]){
  t <- cbind(rep(1, dim(yearly_covariates_array)[1]), yearly_covariates_array[, i, ])
  x.phi[, i, ] <- t
}

x.gamma <- x.phi

# Run this if you are adding random year effect as intercept 
# x.phi.nointercept <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
# x.gamma.nointercept <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3]))
# for(i in 1:dim(yearly_covariates_array)[2]){
# Directly use all covariates without adding an intercept column
#  x.phi.nointercept[, i, ] <- yearly_covariates_array[, i, ]
#  x.gamma.nointercept[, i, ] <- yearly_covariates_array[, i, ]
#}


# Read and preprocess the detection covariates
detcovs <- read.csv("Data/detcovs.csv")
detcovs <- detcovs[detcovs$YYYY != "2004",]
detcovs <- detcovs[detcovs$SS %in% sites_to_keep, ]
detcovsready <- select(detcovs, c("SS", "ROUND", "YYYY", "jday", "tssr"))

# Extract unique sites, years, and set parameters
unique_sites <- unique(detcovsready$SS)
unique_years <- sort(unique(detcovsready$YYYY))
nsite <- length(unique_sites)
nyear <- length(unique_years)
nvisits <- 4 # Assuming this is your maximum number of visits
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
      # Subset data for the specific visit
      visit_data <- site_year_data[site_year_data$ROUND == visit, ]
      
      if (nrow(visit_data) == 1) {
        # Assign values when there's exactly one row of data for the visit
        detection_covariates_array[site, year, visit, 1] <- visit_data$jday
        detection_covariates_array[site, year, visit, 2] <- visit_data$tssr
      } else {
        # Keep NAs if there are zero or multiple rows for the visit
        detection_covariates_array[site, year, visit, 1:2] <- NA
      }
    }
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
# Adding an intercept
ncovariates_with_intercept <- dim(detection_covariates_array)[4] + 1

# Initialize x.p with dimensions: sites, years, maximum visits, covariates (including intercept)
x.p <- array(NA, dim = c(nsite, nyear, nvisits, ncovariates_with_intercept))

# Populate x.p
for (i in 1:nsite) {
  for (k in 1:nyear) {
    for (j in 1:nvisits) {
      if (!is.na(detection_covariates_array[i, k, j, 1])) {  # Check if the covariate data is not NA
        x.p[i, k, j, 1] <- 1  # Intercept
        x.p[i, k, j, 2:ncovariates_with_intercept] <- detection_covariates_array[i, k, j, ]
      } else {
        x.p[i, k, j, 1:ncovariates_with_intercept] <- NA
      }
    }
  }
}
head(x.p)

# Create the site x year indicator matrix
# indicator of whether the species was ever detected at that site in that year, used in the likelihood calculation in jags model 
ind = apply(y, c(1, 2), max, na.rm = TRUE)

# Run distance to edge model with no random year effect 
# But with a log effect 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ", "lprob.y")


# MCMC settings
ni <- 100
nt <- 1
nb <- 50
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], ind = ind)


system.time({
  out_lik <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                   model.file = "cl_model_cov_lik.txt", n.chains = nc, 
                   n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_lik)
