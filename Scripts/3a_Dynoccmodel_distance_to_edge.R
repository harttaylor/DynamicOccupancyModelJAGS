
# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)

# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array

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


# Create x.phi and x.gamma arrays for LOG distance to edge covariates 
#log.x.phi <- array(NA, dim = c(dim(log_dist_array)[1], dim(log_dist_array)[2], dim(log_dist_array)[3] + 1))
#for(i in 1:dim(log_dist_array)[2]){
 # t <- cbind(rep(1, dim(log_dist_array)[1]), log_dist_array[, i, ])
  #log.x.phi[, i, ] <- t
#}

#log.x.gamma <- log.x.phi


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


# Run distance to edge model with no random year effect 
# But with a log effect 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "phi", "gamma", "psi", "N", "z", "muZ")


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


print(out_cov_edge3)



saveRDS(out_cov_edge, file = "Results/EDGE_resultsFeb3.rds")

out_edge <- readRDS("Results/EDGE_resultsFeb3.rds")

print(out_edge)
# Run distance to edge model WITH the random year effect 

params <- c("beta.psi", "beta.phi", "beta.gamma", "beta.p", "alpha.phi", "alpha.gamma", "psi", "phi", "gamma", "N", "z", "muZ")


# MCMC settings
ni <- 12000
nt <- 1
nb <- 6000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi.nointercept, 
                 nbeta.phi = dim(x.phi.nointercept)[3], x.gamma = x.gamma.nointercept, nbeta.gamma = dim(x.gamma.nointercept)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  out_cov_RE <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                       model.file = "DistancetoEdge_RE.txt", n.chains = nc, 
                       n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(out_cov_RE)

save

saveRDS(out_cov_RE, "Results/EDGE_RE_model12000.rds")
out_edge_RE <- readRDS("Results/EDGE_RE_model_resultsJan31.rds")


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
