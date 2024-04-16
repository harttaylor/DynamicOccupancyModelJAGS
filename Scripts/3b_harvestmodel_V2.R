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


# Prepare harvest age matrix 
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




# Prepare harvest interaction matrix 
harvest_covariates <- yearly_covariates[c("SS", "YEAR",
                                          "NEAR.DIST.harvest", "MEANAGE.565.harvest")]

harvest_covariates$NEAR.DIST.harvest <- harvest_covariates$NEAR.DIST.harvest / max(harvest_covariates$NEAR.DIST.harvest)
harvest_covariates$MEANAGE.565.harvest <- harvest_covariates$MEANAGE.565.harvest / max(harvest_covariates$MEANAGE.565.harvest)
harvest_covariates$harvest_interaction <- (harvest_covariates$NEAR.DIST.harvest)*(harvest_covariates$MEANAGE.565.harvest)

# Get unique sites and years
sites <- unique(harvest_covariates$SS)
years <- unique(harvest_covariates$YEAR)

# Initialize a 2-dimensional matrix for harvest distance 
harvest_interaction_matrix <- matrix(NA, nrow = length(sites), ncol = length(years))

# Assign row and column names to the matrix for clarity
rownames(harvest_interaction_matrix) <- sites
colnames(harvest_interaction_matrix) <- years

# Fill the matrix with harvest distances for each site-year combination
for (site in sites) {
  for (year in years) {
    # Extract the corresponding harvest distance value
    covariate_value <- harvest_covariates$harvest_interaction[harvest_covariates$SS == site & harvest_covariates$YEAR == year]
    
    # Check if there is exactly one value for each site-year combination
    if (length(covariate_value) == 1) {
      harvest_interaction_matrix[site, as.character(year)] <- covariate_value
    }
  }
}



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

sum(harvest == 1)

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
# alpha is intercept now and delta is coefficient for the age which priors vary depending on whether harvest is 1 or 2 at site 
# No interaction 
params <- c("beta.psi", "delta.phi", "beta.phi", "delta.gamma", "beta.gamma", "beta.p",  "alpha.phi", "alpha.gamma", "y.prob", "score.year", "N", "z", "muZ")


# MCMC settings
ni <- 20000
nt <- 1
nb <- 10000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.p = x.p, nbeta.p = dim(x.p)[4], 
                 harvest = harvest, harvest_age = harvest_age_matrix, harvest_distance = harvest_distances_matrix, ind = ind)


system.time({
  smallout_harvlik <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                       model.file = "HarvestModel_V2.txt", n.chains = nc, 
                       n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(smallout_harvlik)


saveRDS(smallout_harvlik, "Results/Apr08harvest_lik_20000.rds")

# With harvest age interaction 
params <- c("beta.psi", "delta.phi", "beta.phi", "delta.gamma", "beta.gamma", "delta.phi.interaction", 
            "delta.gamma.interaction", "beta.p",  "alpha.phi", "alpha.gamma", "score.year", "y.prob", "N", "z", "muZ")


# MCMC settings
ni <- 20000
nt <- 1
nb <- 10000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.p = x.p, nbeta.p = dim(x.p)[4], 
                 harvest = harvest, harvest_age = harvest_age_matrix, harvest_distance = harvest_distances_matrix, harvest_interaction = harvest_interaction_matrix, ind = ind)


system.time({
  mout_harvXlik <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                     model.file = "HarvestModel_V2Interaction.txt", n.chains = nc, 
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(mout_harvXlik)


saveRDS(tout_harvXlik, "Results/Apr08harvest_Xlik_20000.rds")

sout_harvXlik <- readRDS("Results/Apr08harvest_Xlik_20000.rds")

Mar11harvest_lik_20000 <- readRDS("Results/Mar11harvest_lik_20000.rds")
Mar11harvest_Xlik_20000 <- readRDS("Results/Mar11harvest_Xlik_20000.rds")
print(Mar11harvest_Xlik_20000)







library(loo)
lp <- sout_harvXlik$sims.list$score.year
lp1 <- lp[, 2:dim(lp)[2]]
waic(lp1)
#Computed from 30000 by 24 log-likelihood matrix

#Estimate    SE
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

lpy <- sout_harvXlik$sims.list$y.prob
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
print(w) # 8301.708 for harvest interaction model 
# 8307.112 for no interaction harvest model 
