# Read in model output results 

harvest_interaction <- readRDS("Results/harvest_interaction.rds")
out_harv <- readRDS("Results/harvest_nointeraction.rds")
out_edge <- readRDS("Results/EDGE_resultsFeb3.rds")
out_edge_RE <- readRDS("Results/EDGE_RE_model12000.rds")

save(win.data, inits, params, out_cov_edge, file = "Results/Model Eval/out_edge24000.Rdata")

# Load required packages 
library(jagsUI)
library(ggplot2)
library(ROCR)
library(reshape2)


# AUC for distance to edge model with no random year effect 
post <- out_cov_edge$sims.list
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


p1 <- ggplot(ROC1, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=0.05, color="blue") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()
print(p1)
ggsave("Results/Model Eval/ROC_LOGEDGE.png")

ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=1, color="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()
ggsave("Results/Model Eval/AUC_LOGEDGE.png")

mean(AUC) # 0.890187 for linear dist to edge 
mean(AUC) # 0.8883817 for log dist to edge 
# Only a very small change in AUC when comparing the distance to edge random effect of year vs no random effect of year models (Adding the random effect 
# imrpoves AUC score by 0.014). DIC for Edge_RE model=9884.689, DIC for Edge model= 9909.905, so DIC scores say the no RE model is better 
# easier to interpret results using the model without the random effect for year. Possible explanations:
# 1. Limited Variation Across Years: If the impact of the year on colonization and persistence probabilities is relatively uniform across the years studied, 
# the random effect for year may not capture much additional variation.
# 2. Model Complexity: Adding a random effect increases the complexity of the model. If the true underlying process is not that complex (i.e., the year effect is not a significant factor), the simpler 
# model without the random year effect might be more robust and easier to interpret. In such cases, Occam's razor applies: the simplest explanation (or model) is often preferable. Practical Significance 
# vs. Statistical Significance: A small increase in AUC, while statistically significant, may not be practically significant. The added complexity of including a random effect might not justify 
# the minimal improvement in model performance
# 3. Influence of Other Covariates: The effect of year might be confounded or overshadowed by other covariates in your model. If other variables have a stronger
# influence on colonization and persistence probabilities, the year's effect might be relatively minor.



# AUC for distance to edge model with the random year effect 
post <- out_cov_RE$sims.list
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


p1 <- ggplot(ROC1, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=0.05, color="blue") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()
print(p1)
ggsave("Results/Model Eval/ROC_EDGE_RE.png")

ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x=fpr, y=tpr, group=iter)) +
  geom_line(alpha=1, color="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()
ggsave("Results/Model Eval/AUC_EDGE_RE.png")

mean(AUC) # 0.9053925 for out_cov_RE at 12000 iterations 



#AUC for harvest model no interaction 
post <- out_harvlik$sims.list
n.sav <- dim(post$z)[1]  # Number of saved iterations
nsite <- win.data$nsite   # Number of sites
nyear <- win.data$nyear   # Number of years

# Choose a specific year for analysis
year <- 2

# Initialize arrays
psi.pred <- array(dim = c(nsite, n.sav))
Z.est <- array(dim = c(nsite, n.sav))

# Initialize AUC and lists to store FPR and TPR values for each iteration
AUC <- rep(NA, n.sav)
fpr_list <- vector("list", n.sav)
tpr_list <- vector("list", n.sav)

# Loop through each iteration
for (i in 1:n.sav) {
  psi.vals <- post$muZ[i, , year]
  z.vals <- post$z[i, , year]
  
  # Prediction and performance
  pred <- prediction(psi.vals, factor(z.vals, levels = c("0", "1")))
  perf_auc <- performance(pred, "auc")
  AUC[i] <- perf_auc@y.values[[1]]
  
  perf_roc <- performance(pred, "tpr", "fpr")
  fpr_list[[i]] <- perf_roc@x.values[[1]]
  tpr_list[[i]] <- perf_roc@y.values[[1]]
}

# Processing FPR and TPR values for plotting or analysis (if needed)
# Example: Averaging FPR and TPR values across all iterations
# (you can modify this part according to your specific analysis or plotting requirements)
avg_fpr <- sapply(fpr_list, function(x) mean(x, na.rm = TRUE))
avg_tpr <- sapply(tpr_list, function(x) mean(x, na.rm = TRUE))

# Plotting the averaged ROC curve
ggplot() +
  geom_line(aes(x = avg_fpr, y = avg_tpr), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()

# Optional: Sampling for a cleaner plot
ROC2 <- ROC1[sample(nrow(ROC1), size = 1000), ]
ggplot(ROC2, aes(x = fpr, y = tpr, group = iter)) +
  geom_line(alpha = 1, color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylab("True positive rate") +
  xlab("False positive rate") + theme_bw()

# Calculate average AUC
avg_auc <- mean(AUC, na.rm = TRUE) #0.8737484 for harvest no INT 
print(avg_auc)
# 0.8887167 for harvest no INT, RE 

mean(AUC) #0.8734311 for harvest interaction model 
# 0.8887622 for harvest Interaction, with RE

# So the AUC for the interaction vs. no interaction model are basicall ythe same. DIC for interaction model is quite a bit better (9577.506) 
# vs. the no interaction harvest model is 9616.455. 



# For the Random effect of year models the DIC for interaction model is 9605.348 with 24000 iterations 
# And the no interaction RE model is 9670.074 with 24000 iterations 










# Model validation using loo (Leave-one-out cross validation)
# The WAIC is a fully Bayesian criterion for model selection and is particularly useful for comparing different models. 
# It's based on the log pointwise predictive density (lppd) and penalizes for model complexity.
library(jagsUI)
library(loo)

# Extract log-likelihoods (lprob.y) from model output
# It should be a 3D array with dim corresponding to mcmc iterations, chains, and data points (observations)
log_lik_values <- out_harvlik$sims.list$l.score
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
n_iterations_per_chain <- 10000  # Post-burn-in iterations

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







