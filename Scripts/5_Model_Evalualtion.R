

save(win.data, inits, params, out_cov, file = "Model eval/out_cov_files.Rdata")


library(jagsUI)
library(ggplot2)
library(ROCR)
library(reshape2)


post <- out_cov_dist_interaction30000$sims.list
n.sav <- dim(post$z)[1] 
nsite <- win.data$nsite

psi.pred <- array(dim=c(nsite, n.sav))
Z.est <- array(dim=c(nsite, n.sav))
year = 2

post$

AUC <- rep(NA, n.sav)
fpr <- array(dim=c(n.sav, nsite + 1))
tpr <- array(dim=c(n.sav, nsite + 1))
for(i in 1:n.sav){
  psi.vals <- post$muZ[i, , 2]
  z.vals <- post$z[i, , 2]
  pred <- prediction(psi.vals, factor(z.vals, levels = c("0", "1")))
  perf <- performance(pred, "auc")
  AUC[i] <- perf@y.values[[1]]
  perf <- performance(pred, "tpr","fpr")
  fpr[i, ] <- perf@x.values[[1]]
  tpr[i, ] <- perf@y.values[[1]]
}


fprm <- melt(fpr, varnames=c("iter", "site"))
tprm <- melt(tpr, varnames = c("iter", "site"))
ROC1 <- data.frame(fpr = fprm$value,
                   tpr = tprm$value,
                   iter = rep(fprm$iter, 2))


p1 <- ggplot(ROC1, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=0.05, color="blue") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()
print(p1)

ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=1, color="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()


mean(AUC) # 0.8953861 for out_cov_dist_interaction30000













post <- out_cov_interact_notrt$sims.list
n.sav <- dim(post$z)[1] 
nsite <- win.data$nsite

psi.pred <- array(dim=c(nsite, n.sav))
Z.est <- array(dim=c(nsite, n.sav))
year = 2

  
  AUC <- rep(NA, n.sav)
fpr <- array(dim=c(n.sav, nsite + 1))
tpr <- array(dim=c(n.sav, nsite + 1))
for(i in 1:n.sav){
  psi.vals <- post$muZ[i, , 2]
  z.vals <- post$z[i, , 2]
  pred <- prediction(psi.vals, factor(z.vals, levels = c("0", "1")))
  perf <- performance(pred, "auc")
  AUC[i] <- perf@y.values[[1]]
  perf <- performance(pred, "tpr","fpr")
  fpr[i, ] <- perf@x.values[[1]]
  tpr[i, ] <- perf@y.values[[1]]
}


fprm <- melt(fpr, varnames=c("iter", "site"))
tprm <- melt(tpr, varnames = c("iter", "site"))
ROC1 <- data.frame(fpr = fprm$value,
                   tpr = tprm$value,
                   iter = rep(fprm$iter, 2))


p2 <- ggplot(ROC1, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=0.05, color="blue") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()

ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=1, color="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()


mean(AUC) # 0.8979226




post <- out_cov_distnoINT$sims.list
n.sav <- dim(post$z)[1] 
nsite <- win.data$nsite

psi.pred <- array(dim=c(nsite, n.sav))
Z.est <- array(dim=c(nsite, n.sav))
year = 2


AUC <- rep(NA, n.sav)
fpr <- array(dim=c(n.sav, nsite + 1))
tpr <- array(dim=c(n.sav, nsite + 1))
for(i in 1:n.sav){
  psi.vals <- post$muZ[i, , 2]
  z.vals <- post$z[i, , 2]
  pred <- prediction(psi.vals, factor(z.vals, levels = c("0", "1")))
  perf <- performance(pred, "auc")
  AUC[i] <- perf@y.values[[1]]
  perf <- performance(pred, "tpr","fpr")
  fpr[i, ] <- perf@x.values[[1]]
  tpr[i, ] <- perf@y.values[[1]]
}


fprm <- melt(fpr, varnames=c("iter", "site"))
tprm <- melt(tpr, varnames = c("iter", "site"))
ROC1 <- data.frame(fpr = fprm$value,
                   tpr = tprm$value,
                   iter = rep(fprm$iter, 2))


p3 <- ggplot(ROC1, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=0.05, color="blue") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()

ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=1, color="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()


mean(AUC) # 0.8953197 for distnoINT





# Model validation using loo (Leave-one-out cross validation)
# The WAIC is a fully Bayesian criterion for model selection and is particularly useful for comparing different models. 
# It's based on the log pointwise predictive density (lppd) and penalizes for model complexity.
library(jagsUI)
library(loo)

#out_cov is model output 

# Extract log-likelihoods (lprob.y) from model output
# It should be a 3D array with dim corresponding to mcmc iterations, chains, and data points (observations)
log_lik_values <- out_cov_dist_interaction3000$sims.list$lprob.y
dim(log_lik_values)

# Reshape lprob.y for WAIC calculation, loo package requires the log likelihoods to be in a 2D matrix where each row represents 
# an observation and each column represents a different mcmc sample
n_obs <- 114 * 25  # Total number of site-year combinations
n_mcmc_samples <- 4500  # Total number of MCMC samples

log_lik_matrix <- matrix(NA, nrow = n_obs, ncol = n_mcmc_samples)

# Reshaping the array into a matrix
for (m in 1:n_mcmc_samples) {
  # Flatten site and year dimensions into a single dimension for each MCMC sample
  log_lik_matrix[, m] <- as.vector(log_lik_values[m, , ])
}

# Assuming 'log_lik_matrix' is already created
n_chains <- 3
n_iterations_per_chain <- 1500  # Post-burn-in iterations

# Calculate relative effective sample sizes
r_eff <- relative_eff(log_lik_matrix, chain_id = rep(1:n_chains, each = n_iterations_per_chain))

# Now calculate WAIC with r_eff
waic_result <- loo(log_lik_matrix)
print(waic_result)





n_chains <- 3
n_mcmc_samples_per_chain <- 1500  # MCMC samples per chain after burn-in

# Check if the number of columns in log_lik_matrix matches total MCMC samples
if(ncol(log_lik_matrix) == n_chains * n_mcmc_samples_per_chain) {
  # Create chain_id vector
  chain_id <- rep(1:n_chains, each = n_mcmc_samples_per_chain)
  
  # Calculate relative effective sample sizes
  r_eff <- relative_eff(log_lik_matrix, chain_id = chain_id)
  
  # Calculate WAIC
  waic_result <- loo(log_lik_matrix, r_eff = r_eff)
  print(waic_result)
} else {
  stop("Mismatch between the number of columns in log_lik_matrix and total MCMC samples")
}






