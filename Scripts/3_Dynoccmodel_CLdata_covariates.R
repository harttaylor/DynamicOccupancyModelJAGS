
# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array

# Get the covariate arrays 
load("Data/allCLcovs24Jan2024.Rdata")

#length(which(is.na(dets_array)))#1042

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

# Create x.phi and x.gamma arrays
x.phi <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3] + 1))
x.gamma <- array(NA, dim = c(dim(yearly_covariates_array)[1], dim(yearly_covariates_array)[2], dim(yearly_covariates_array)[3] + 1))
for(i in 1:dim(yearly_covariates_array)[2]){
  # Directly use all covariates without adding an intercept column
  x.phi[, i, ] <- yearly_covariates_array[, i, ]
  x.gamma[, i, ] <- yearly_covariates_array[, i, ]
}

# Get the dimensions from yearly_covariates_array
dim1 = dim(yearly_covariates_array)[1]
dim2 = dim(yearly_covariates_array)[2]

# Initialize x.phi and x.gamma arrays with the new dimensions
x.phi <- array(NA, dim = c(dim1, dim2, 3))
x.gamma <- array(NA, dim = c(dim1, dim2, 3))

# Fill in the arrays
for (i in 1:dim1) {
  for (j in 1:dim2) {
    x.phi[i, j, 1] <- 1  # Intercept
    x.phi[i, j, 2] <- yearly_covariates_array[i, j, 4]  # 4th element
    x.phi[i, j, 3] <- yearly_covariates_array[i, j, 5]  # 5th element
    
    x.gamma[i, j, 1] <- 1  # Intercept
    x.gamma[i, j, 2] <- yearly_covariates_array[i, j, 4]  # 4th element
    x.gamma[i, j, 3] <- yearly_covariates_array[i, j, 5]  # 5th element
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


params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N")
#params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "alpha.phi", "alpha.gamma", "psi", "phi", "gamma", "N", "z", "muZ")


# MCMC settings
ni <- 120
nt <- 1
nb <- 60
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], ind = ind, 
                 harvest = harvest)
win.data <- list(
  y = y,
  nsite = dim(y)[1],
  nyear = dim(y)[2],
  nsurv = nsurv,
  J = J,
  x.psi = x.psi,
  nbeta.psi = ncol(x.psi),
  x.phi = x.phi,  # using the array with only the 5th element
  nbeta.phi = 3,  # only one beta coefficient for phi
  x.gamma = x.gamma,  # using the array with only the 5th element
  nbeta.gamma = 3,  # only one beta coefficient for gamma
  x.p = x.p,
  nbeta.p = dim(x.p)[4],
  harvest = harvest
)

system.time({
  out_cov_harv <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                            model.file = "cl_model_cov_p2_ancova.txt", n.chains = nc, 
                            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


system.time({
  out_cov_dist12001 <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                       model.file = "cl_model_notrt.txt", n.chains = nc, 
                       n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 

print(out_cov_dist12001)

print(out_cov_dist12000)
system.time({
  out_cov_dist <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                  model.file = "cl_model_cov_p2_re.txt", n.chains = nc, 
                  n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) # with 12000 iterations took 26mins with z 

print(out_cov_dist) 


saveRDS(out_cov_dist, file = "model_resultsJan18.rds")
save.image()

# Model with harvestage*harvestdist interaction 
win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, tr = tr, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], ind = ind)

system.time({
  out_cov_dist_interaction30000 <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                          model.file = "cl_model_cov_p2_re.txt", n.chains = nc, 
                          n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) # took 83 mins 

print(out_cov_dist_interaction30000)
out_cov_dist_interaction3000$sims.list$lprob.y


# Model with harvestage*harvestdist interaction but no treatment 
# MCMC settings
params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ")

ni <- 12000
nt <- 1
nb <- 6000
nc <- 3
win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], ind = ind)

system.time({
  out_cov_interact_notrt <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                                        model.file = "cl_model_notrt.txt", n.chains = nc, 
                                        n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 

print(out_cov_interact_notrt)

# Model with just distance (no harvestage*harvestdist interaction and no treatment)
# MCMC settings
ni <- 120
nt <- 1
nb <- 60
nc <- 3
win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4], ind = ind)

system.time({
  out_cov_interact_notrt <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                                 model.file = "cl_model_notrt.txt", n.chains = nc, 
                                 n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 

print(out_cov_interact_notrt)

# define a mappign from JAGS parameter names to more descriptive labels 
param_descriptions <- c("beta.psi[1]" = "Beta.Psi.Intercept", 
                        "beta.psi[2]" = "Beta.Psi.Percent.Conifer", 
                        "beta.psi[3]" = "Beta.Psi.Peco.Squared",
                        "beta.psi[4]" = "Beta.Psi.Stand.Age",
                        "beta.phi[1]" = "Beta.Phi.Intercept",
                        "beta.phi[2]" = "Beta.Phi.Dist.Seismis.Line",
                        "beta.phi[3]" = "Beta.Phi.Dist.Road",
                        "beta.phi[4]" = "Beta.Phi.Dist.Harvest",
                        "beta.phi[5]" = "Beta.Phi.Dist.Pipeline",
                        "beta.phi[6]" = "Beta.Phi.Harvest.Age",
                        "beta.phi[7]" = "Beta.Phi.Treatment.Fragment",
                        "beta.phi[8]" = "Beta.Phi.Treatment.Riparian",
                        "beta.gamma[1]" = "Beta.Gamma.Intercept",
                        "beta.gamma[2]" = "Beta.Gamma.Dist.Seis",
                        "beta.gamma[3]" = "Beta.Gamma.Dist.Road",
                        "beta.gamma[4]" = "Beta.Gamma.Dist.Harvest",
                        "beta.gamma[5]" = "Beta.Gamma.Dist.Pipeline",
                        "beta.gamma[6]" = "Beta.Gamma.Harvest.Age",
                        "beta.gamma[7]" = "Beta.Gamma.Treatment.Fragment",
                        "beta.gamma[8]" = "Beta.Gamma.Treatment.Riparian",
                        "beta.p[1]" = "Beta.P.Intercept", 
                        "beta.p[2]" = "Beta.P.Julian.Day", 
                        "beta.p[3]" = "Beta.P.TSSR")





##---- Diagnostic checks----
# Perform diagnostic checks on MCMC model output
library(coda)

jagsUI::traceplot(out_cov_dist12001)


# List of parameters
params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N")

# Extract the samples list from out_cov
samples_list <- out_cov$sims.list

# Loop through each parameter and perform diagnostics
for (param in params) {
  # Convert the samples to an mcmc object
  mcmc_param <- mcmc(as.matrix(samples_list[[param]]))
  
  # Print the summary
  print(paste("Summary for", param))
  print(summary(mcmc_param))
  
  # Trace plot for visual inspection of convergence
  traceplot(mcmc_param, main=paste("Traceplot for", param))
  
  # Density plot for posterior distribution
  densplot(mcmc_param, main=paste("Density Plot for", param))
  
  # Autocorrelation plot
  autocorr.plot(mcmc_param, main=paste("Autocorrelation for", param))
  
  # Calculating Effective Sample Size (ESS)
  ess_param <- effectiveSize(mcmc_param)
  print(paste("Effective Sample Size for", param, ":", ess_param))
  
  # Check for convergence using Gelman-Rubin diagnostic (requires multiple chains)
  if (is.mcmc.list(mcmc_param) && dim(mcmc_param)[1] > 1) {
    gelman_diag <- gelman.diag(mcmc_param)
    print(paste("Gelman Diagnostic for", param))
    print(gelman_diag)
    
    gelman_plot <- gelman.plot(mcmc_param, main=paste("Gelman Plot for", param))
    print(gelman_plot)
  }
}
