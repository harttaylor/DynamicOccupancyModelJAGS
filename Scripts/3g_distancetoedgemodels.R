# Run all distance to edge models with 30000 iterations and no random effects 
library(jagsUI)

# Model 1: Quad distance to edge 

load("Quad_Jagsmodeldata.Rdata")

# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  quad_edge <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                     model.file = "DistancetoEdgeModel.txt", n.chains = nc, 
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(quad_edge)
saveRDS(quad_edge, file = "Results/QUADEDGE_30000resultsFeb20.rds")



# Model 2: Log distance to edge 

load("Log_Jagsmodeldata.Rdata")

# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  log_edge <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                    model.file = "DistancetoEdgeModel.txt", n.chains = nc, 
                    n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(log_edge)
saveRDS(log_edge, file = "Results/LOGEDGE_30000resultsFeb20.rds")



# Model 3: Linear distance to edge 

load("Linear_Jagsmodeldata.Rdata")

# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  linear_edge <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                   model.file = "DistancetoEdgeModel.txt", n.chains = nc, 
                   n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(linear_edge)
saveRDS(linear_edge, file = "Results/LINEAREDGE_30000resultsFeb20.rds")

