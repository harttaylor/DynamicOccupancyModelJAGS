# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)
library(tidyr)
library(dplyr)

load("covariatearraysFeb16.Rdata")
detection_history <- readRDS("Data/detection_history_array.rds")
y <- detection_history
detection_covariates_array <- readRDS("Data/detection_covariates_arrayV2.rds")


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
num_covariates <- 2

# Initialize the array with NA values
yearly_covariates_array <- array(NA, dim = c(length(sites), length(years), num_covariates))

for (i in 1:length(sites)) {
  for (j in 1:length(years)) {
    covs_rows <- yearly_covariates[yearly_covariates$SS == sites[i] & yearly_covariates$YEAR == years[j], ]
    
    if (nrow(covs_rows) > 0) {
      first_row <- covs_rows[1, ]
      covariate_data <- first_row[, c("NEAR.DIST.harvest", "MEANAGE.565.harvest")]
      yearly_covariates_array[i, j, ] <- as.numeric(covariate_data)
    } 
  }
}

#"harvest_interaction"

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

# Checks to make sure the NAs/visits match 
head(x.p[, 2, ,])
head(y[, 2, ])

head(x.p[12, , ,])
head(y[12, , ])


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
  if (yearly_covariates$MEANAGE.565.harvest[row] < 40) {
    harvest[site_index, year_index] <- 1  # Set to 1 if harvested
  }
}

sum(harvest == 1) #2172
sum(harvest == 2) #678
# Create the site x year indicator matrix

# indicator of whether the species was ever detected at that site in that year, used in the likelihood calculation in jags model 
ind = apply(y, c(1, 2), max, na.rm = TRUE)



params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "l.score", "y.prob", "N", "score.year", "hmean.turn", "z", "muZ")


# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], harvest = harvest, ind = ind)


system.time({
  harvest_no_X <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                      model.file = "harvestmodel_V3.txt", n.chains = nc, 
                      n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(harvest_no_X)

saveRDS(harvest_no_X, file = "Results/Apr15Harvest_noX_Results.rds")

params <- c("beta.psi", "beta.phi", "beta.gamma", "alpha.phi", "alpha.gamma", "beta.p", "l.score", "y.prob", "N", "score.year", "hmean.turn", "z", "muZ")


win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi.nointercept, 
                 nbeta.phi = dim(x.phi.nointercept)[3], x.gamma = x.gamma.nointercept, nbeta.gamma = dim(x.gamma.nointercept)[3], x.p = x.p, nbeta.p = dim(x.p)[4], harvest = harvest, ind = ind)


system.time({
  harvest_RE <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                      model.file = "harvestmodelRE_V3.txt", n.chains = nc, 
                      n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 

print(harvest_RE)

saveRDS(harvest_RE, file = "Results/Harvest model/Apr15REharvest.rds")



library(loo)
lp <- harvest_RE$sims.list$score.year
lp1 <- lp[, 2:dim(lp)[2]]
waic(lp1)
#Computed from 30000 by 24 log-likelihood matrix

#           Estimate    SE
#elpd_waic  -4155.9 196.9
#p_waic       268.6  16.2
#waic        8311.8 393.8

#24 (100.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.

loo(lp1)
#Computed from 30000 by 24 log-likelihood matrix

#Estimate    SE
#elpd_loo  -4150.9 196.7
#p_loo       263.6  14.9
#looic      8301.8 393.5
#------
#  Monte Carlo SE of elpd_loo is NA.

#Pareto k diagnostic values:
#  Count Pct.    Min. n_eff
#(-Inf, 0.5]   (good)      0     0.0%   <NA>      
#  (0.5, 0.7]   (ok)        0     0.0%   <NA>      
#  (0.7, 1]   (bad)       4    16.7%   43        
#(1, Inf)   (very bad) 20    83.3%   5         
#See help('pareto-k-diagnostic') for details.
#Warning messages:
#  1: Relative effective sample sizes ('r_eff' argument) not specified.
#For models fit with MCMC, the reported PSIS effective sample sizes and 
#MCSE estimates will be over-optimistic. 
#2: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

lpy <- harvest_RE$sims.list$y.prob
lpy1 <- lpy[, , 2:dim(lpy)[3]]

str(lpy1)

d <- matrix(NA, dim(lpy1)[2], dim(lpy1)[3])
for(k in 1:dim(lpy1)[3]){
  for(i in 1:dim(lpy1)[2]){
    d[i, k] <- log(mean(lpy1[1:dim(lpy1)[1], i, k]))
  }
}

d1 <- sum(colSums(d))

p <- array(NA, dim = dim(lpy1))
v <- matrix(NA, dim(lpy1)[2], dim(lpy1)[3])
for(k in 1:dim(lpy1)[3]){
  for(i in 1:dim(lpy1)[2]){
    for(s in 1:dim(lpy1)[1]){
      p[s, i, k] <- log(lpy1[s, i, k])
    }
    v[i, k] <- var(p[, i, k])
  }
}

v1 <- sum(colSums(v))

w <- -2*d1 + 2*v1
print(w) 




# 8301.708 for harvest interaction model 
# 8307.112 for no interaction harvest model 
