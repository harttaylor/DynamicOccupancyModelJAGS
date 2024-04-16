# Model 2: log distances with Random effect 
library(jagsUI)

load("LOG_REJagsmodeldata.Rdata")


# MCMC settings
ni <- 30000
nt <- 1
nb <- 15000
nc <- 3

win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J, x.psi = x.psi, nbeta.psi = ncol(x.psi), x.phi = x.phi, 
                 nbeta.phi = dim(x.phi)[3], x.gamma = x.gamma, nbeta.gamma = dim(x.gamma)[3], x.p = x.p, nbeta.p = dim(x.p)[4])


system.time({
  RElog_edge <- jags(data = win.data, inits = inits, parameters.to.save = params, 
                      model.file = "DistancetoEdge_RE.txt", n.chains = nc, 
                      n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
}) 


print(RElog_edge)
saveRDS(RElog_edge, file = "Results/LOGEDGE_30000resultsFeb20.rds")
