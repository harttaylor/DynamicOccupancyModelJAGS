# Make plots to show results of JAGS model 
# Plot variability in persistence and colonization together with changes in the number of sites occupied 
# Set up graphical parameters for plotting 
# op <- par(mfrow = c(3,1), mar=c(4,4,1,1) +0.1, pch=4, lwd=2,las=1)
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/COLEXT model/Conventionalway")
# Load necessary library
library(ggplot2)
library(gridExtra)
install.packages("tidybayes")
library(tidybayes)

# Load data 
out_cov <- readRDS("model_resultsJan15.rds")

# Make plots for Psi 
# Define the logistic function
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Model output for beta.psi[4] (stand age)
beta_psi_4_mean <- 4.657
beta_psi_4_sd <- 2.445

# Create a sequence for scaled stand age (0 to 1)
scaled_age <- seq(0, 1, length.out = 100)

# Calculate the logit of psi using the beta.psi[3] coefficient for the scaled age
logit_psi <- beta_psi_4_mean * scaled_age

# Apply logistic transformation
psi_estimated <- logistic(logit_psi)

# Calculate confidence intervals
upper_confidence <- logistic((beta_psi_4_mean + beta_psi_4_sd) * scaled_age)
lower_confidence <- logistic((beta_psi_4_mean - beta_psi_4_sd) * scaled_age)

# Create a data frame for plotting
plot_data <- data.frame(scaled_age, psi_estimated, upper_confidence, lower_confidence)

# Plotting
ggplot(plot_data, aes(x = scaled_age, y = psi_estimated)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower_confidence, ymax = upper_confidence), alpha = 0.2, fill = "lightblue") +
  xlab("Scaled Stand Age") +
  ylab("Initial Occupancy Probability (Psi)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Removes grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(color = "black"))
ggsave("Results/StandAgePsi.png")

# Psi and Percent Conifer 
# Model output for beta.psi[2] (percent conifer)
beta_psi_conifer_mean <- -0.057
beta_psi_conifer_sd <- 0.073

# Create a sequence for percent conifer (0 to 53.08333)
percent_conifer <- seq(0, 53.08333, length.out = 100)

# Calculate the logit of psi using the beta.psi[2] coefficient for percent conifer
logit_psi <- beta_psi_conifer_mean * percent_conifer

# Apply logistic transformation
psi_estimated <- logistic(logit_psi)

# Calculate confidence intervals
upper_confidence <- logistic((beta_psi_conifer_mean + beta_psi_conifer_sd) * percent_conifer)
lower_confidence <- logistic((beta_psi_conifer_mean - beta_psi_conifer_sd) * percent_conifer)

# Create a data frame for plotting
plot_data <- data.frame(percent_conifer, psi_estimated, upper_confidence, lower_confidence)

# Plotting
ggplot(plot_data, aes(x = percent_conifer, y = psi_estimated)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower_confidence, ymax = upper_confidence), alpha = 0.2, fill = "lightblue") +
  xlab("Percent Conifer") +
  ylab("Initial Occupancy Probability (Psi)") +
  theme_classic()

ggsave("Results/PercentConiferPsi.png")


# Make panel with both intitial occupancy predictors 
# Function to create a plot for a given beta coefficient and predictor range
create_plot <- function(beta_mean, beta_sd, predictor, predictor_name) {
  logit_psi <- beta_mean * predictor
  psi_estimated <- logistic(logit_psi)
  upper_confidence <- logistic((beta_mean + beta_sd) * predictor)
  lower_confidence <- logistic((beta_mean - beta_sd) * predictor)
  
  plot_data <- data.frame(predictor, psi_estimated, upper_confidence, lower_confidence)
  
  ggplot(plot_data, aes(x = predictor, y = psi_estimated)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_confidence, ymax = upper_confidence), alpha = 0.2, fill = "lightblue") +
    xlab(predictor_name) +
    ylab("Initial Occupancy Probability (Psi)") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          axis.line = element_line(color = "black"))
}

# Stand age plot
beta_psi_3_mean <- 5.415
beta_psi_3_sd <- 1.809
scaled_age <- seq(0, 1, length.out = 100)
plot_age <- create_plot(beta_psi_3_mean, beta_psi_3_sd, scaled_age, "Scaled Stand Age (0-1)")

# Percent conifer plot
beta_psi_conifer_mean <- -0.052
beta_psi_conifer_sd <- 0.021
percent_conifer <- seq(0, 53.08333, length.out = 100)
plot_conifer <- create_plot(beta_psi_conifer_mean, beta_psi_conifer_sd, percent_conifer, "Percent Conifer")

# Combine the plots into a grid object
combined_plot <- grid.arrange(plot_age, plot_conifer, ncol = 2)
# Draw the plot and save it
library(grid)
png(filename = "combined_plot.png", width = 800, height = 400)
grid.draw(combined_plot)
dev.off()


# plot the relationship between the quadratic term of peco and psi 
# Coefficient for quadratic term of percent conifer
beta_psi_conifer2_mean <- -0.595
beta_psi_conifer2_sd <- 4.304

# Logistic transformation function
logistic <- function(x) 1 / (1 + exp(-x))

# Create a sequence for percent conifer (0 to 53.08333)
percent_conifer <- seq(0, 53.08333, length.out = 100)
percent_conifer_squared <- percent_conifer^2

# Calculate the logit of psi using only the quadratic term coefficient
logit_psi <- beta_psi_conifer2_mean * percent_conifer_squared

# Apply logistic transformation
psi_estimated <- logistic(logit_psi)

# Calculate confidence intervals for the quadratic term
upper_confidence <- logistic((beta_psi_conifer2_mean + beta_psi_conifer2_sd) * percent_conifer_squared)
lower_confidence <- logistic((beta_psi_conifer2_mean - beta_psi_conifer2_sd) * percent_conifer_squared)

# Create a data frame for plotting
plot_data <- data.frame(percent_conifer, psi_estimated, upper_confidence, lower_confidence)

# Plotting
ggplot(plot_data, aes(x = percent_conifer, y = psi_estimated)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower_confidence, ymax = upper_confidence), alpha = 0.2, fill = "lightblue") +
  xlab("Percent Conifer") +
  ylab("Estimated Psi (Quadratic Relationship)") +
  ggtitle("Quadratic Relationship between Psi and Percent Conifer") +
  theme_classic()

ggsave("Results/PercentConiferPsi_QuadraticOnly.png")


# Plot phi yearly 
# Extract the phi matrix from your model output
phi_matrix <- out_cov_dist_interaction30000$mean$phi[, 1:24]  # Adjust the indices if necessary
upper_phi <- out_cov_dist_interaction30000$q97.5$phi[, 1:24]
lower_phi <- out_cov_dist_interaction30000$q2.5$phi[, 1:24]

# Calculate the mean phi for each year across all sites
mean_phi_per_year <- apply(phi_matrix, 2, mean)

# Years
years <- 1:24  # Adjust if your study years are labeled differently

phi_plot <- ggplot(data = data.frame(years, mean_phi_per_year), aes(x = years, y = mean_phi_per_year)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean Persistence Probability (Phi)") +
 # ggtitle("Mean Persistence Probability Over 24 Years") +
  theme_classic()


ggsave("Results/YearlyPhi.png")


# Now do yearly estimates of gamma 
# Extract the gamma matrix from your model output
gamma_matrix <- out_cov_dist_interaction30000$mean$gamma[, 1:24]  # Adjust the indices if necessary

# Calculate the mean gamma for each year across all sites
mean_gamma_per_year <- apply(gamma_matrix, 2, mean)

# Years
years <- 1:24 

gamma_plot <- ggplot(data = data.frame(years, mean_gamma_per_year), aes(x = years, y = mean_gamma_per_year)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean Yearly Colonization Probability (Gamma)") +
  #ggtitle("Mean Yearly Colonization Probability Over 24 Years") +
  theme_classic() 

combined_plot <- grid.arrange(phi_plot, gamma_plot, ncol = 2)
combined_plot <- phi_plot + gamma_plot
ggsave("Results/CombinedPhiGamma.png", combined_plot, width = 10, height = 5)



##---- Plot phi and gamma yearly on the same graph----
library(ggplot2)

# Extract the phi matrix from your model output
phi_matrix <- out_cov_dist12001$mean$phi[, 1:24]

# Calculate the mean phi for each year across all sites
mean_phi_per_year <- apply(phi_matrix, 2, mean)

# Extract the gamma matrix from your model output
gamma_matrix <- out_cov_dist12001$mean$gamma[, 1:24]

# Calculate the mean gamma for each year across all sites
mean_gamma_per_year <- apply(gamma_matrix, 2, mean)

# Years
years <- 2:25  # Adjust if your study years are labeled differently

# Create a data frame for phi and gamma
data_df <- data.frame(years, mean_phi_per_year, mean_gamma_per_year, mean)

# Create the combined plot
combined_plot <- ggplot(data = data_df, aes(x = years)) +
  geom_line(aes(y = mean_phi_per_year, color = "Persistence"), size = 1) +
  geom_line(aes(y = mean_gamma_per_year, color = "Colonization"), size = 1) +
  xlab("Year") +
  ylab("Mean Probability") +
  scale_y_continuous(n.breaks = 10) + 
  scale_color_manual(values = c("Persistence" = "blue", "Colonization" = "red")) +
  theme_classic() +
  labs(color = "Parameter")

# Save the combined plot
ggsave("Results/CombinedPhiGamma.png", combined_plot, width = 10, height = 5)

# Now do yearly estimates of z, occupancy probability 
# Extract the gamma matrix from your model output
occu_matrix <- out_cov_dist12001$mean$z[, 1:25]  # Adjust the indices if necessary
print(dim(out_cov$mean$z))

# Calculate the mean gamma for each year across all sites
mean_gamma_per_year <- apply(occu_matrix, 2, mean)

# Years
years <- 1:25 

ggplot(data = data.frame(years, mean_gamma_per_year), aes(x = years, y = mean_gamma_per_year)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean Yearly Occupancy Probability (Psi)") +
  #ggtitle("Mean Yearly Colonization Probability Over 25 Years") +
  theme_classic() 




# Assuming you have extracted the z matrix similarly
z_matrix <- out_cov_dist12001$mean$z[, 1:25]
z_lower <- out_cov_dist12001$q2.5$z[, 1:25]
z_upper <- out_cov_dist12001$q97.5$z[, 1:25]
mean_z_per_year <- apply(z_matrix, 2, mean)

# Extract the phi matrix from your model output
phi_matrix <- out_cov_dist12001$mean$phi[, 1:24]
phi_lower <- out_cov_dist12001$q2.5$phi[, 1:24]
phi_upper <- out_cov_dist12001$q97.5$phi[, 1:24]
mean_phi_per_year <- apply(phi_matrix, 2, mean)

# Extract the gamma matrix from your model output
gamma_matrix <- out_cov_dist12001$mean$gamma[, 1:24]
gamma_lower <- out_cov_dist12001$q2.5$gamma[, 1:24]
gamma_upper <- out_cov_dist12001$q97.5$gamma[, 1:24]
mean_gamma_per_year <- apply(gamma_matrix, 2, mean)

# Calculate yearly averages for the lower and upper CI bounds
mean_phi_lower <- apply(phi_lower, 2, mean)
mean_phi_upper <- apply(phi_upper, 2, mean)
mean_gamma_lower <- apply(gamma_lower, 2, mean)
mean_gamma_upper <- apply(gamma_upper, 2, mean)
mean_z_lower <- apply(z_lower, 2, mean)
mean_z_upper <- apply(z_upper, 2, mean)

# Adjust the lengths of the mean CI vectors
mean_phi_lower <- c(NA, mean_phi_lower)  # Prepend NA
mean_phi_upper <- c(NA, mean_phi_upper)  # Prepend NA
mean_gamma_lower <- c(NA, mean_gamma_lower)  # Prepend NA
mean_gamma_upper <- c(NA, mean_gamma_upper)  # Prepend NA
mean_phi <- c(NA, mean_phi_per_year)
mean_gamma <- c(NA, mean_gamma_per_year)
# Years vector covering 1 to 25
years <- 1:25

# Create a data frame for plotting
data_df <- data.frame(years, mean_phi, mean_phi_lower, mean_phi_upper, 
                      mean_gamma, mean_gamma_lower, mean_gamma_upper, 
                      mean_z_per_year, mean_z_lower, mean_z_upper)

library(viridis)
# Create the combined plot with adjusted colors and alpha levels
combined_plot <- ggplot(data = data_df, aes(x = years)) +
  # Plot the widest CIs first as the background
  geom_ribbon(aes(ymin = mean_z_lower, ymax = mean_z_upper), fill = "grey80", alpha = 0.3) +
  geom_ribbon(aes(ymin = mean_phi_lower, ymax = mean_phi_upper), fill = "grey70", alpha = 0.3) +
  geom_ribbon(aes(ymin = mean_gamma_lower, ymax = mean_gamma_upper), fill = "grey60", alpha = 0.3) +
  
  # Now plot the lines, with more prominent colors
  geom_line(aes(y = mean_phi, color = "Persistence"), size = 1.2) +
  geom_line(aes(y = mean_gamma, color = "Colonization"), size = 1.2) +
  geom_line(aes(y = mean_z_per_year, color = "Occupancy"), size = 1.2) +
  
  scale_color_viridis_d(option = "D", begin = 0.3, end = 0.7) +
  xlab("Year") +
  ylab("Mean Probability") +
  theme_classic() +
  labs(color = "Parameter") +
  # Adding a guide for the fills, if you want to include it in the legend
  guides(fill = guide_legend(title = "95% CI"))

# Print the plot
print(combined_plot)


# Create a plot for occupancy with the y-axis scaled from 0 to 1
occupancy_plot <- ggplot(data = data_df, aes(x = years)) +
  geom_ribbon(aes(ymin = mean_z_lower, ymax = mean_z_upper), fill = "orange", alpha = 0.2) +
  geom_line(aes(y = mean_z_per_year, color = "Occupancy"), size = 1.2) +
  scale_color_manual(values = c("Occupancy" = "orange")) +
  expand_limits(y = 0:1) +
  labs(x = "Year", y = "Mean Occupancy Probability", color = "Parameter") +
  theme_classic()

# Create a plot for colonization and persistence with the y-axis scaled from 0 to 1
col_pers_plot <- ggplot(data = data_df, aes(x = years)) +
  geom_ribbon(aes(ymin = mean_phi_lower, ymax = mean_phi_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = mean_phi, color = "Persistence"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_gamma_lower, ymax = mean_gamma_upper), fill = "purple", alpha = 0.2) +
  geom_line(aes(y = mean_gamma, color = "Colonization"), size = 1.2) +
  scale_color_manual(values = c("Persistence" = "blue", "Colonization" = "purple")) +
  expand_limits(y = 0:1) +
  labs(x = "Year", y = "Mean Probability", color = "Parameter") +
  theme_classic()

# Print the plots
print(occupancy_plot)
print(col_pers_plot)


# Make a plot for the number of sites occupied each year 
# Assuming 'out_cov' is your JAGS model output
mean_occupied_sites <- out_cov$mean$N
sd_occupied_sites <- out_cov$sd$N
years <- 1:25

# Map 'years' to actual years (1993 to 2018)
actual_years <- 1993 + (years - 1)

# Calculating the range for uncertainty using SD
sd_upper <- mean_occupied_sites + sd_occupied_sites
sd_lower <- mean_occupied_sites - sd_occupied_sites

# Create a data frame for plotting
plot_data <- data.frame(years = actual_years, mean_occupied_sites, sd_upper, sd_lower)

# Plotting with actual years
ggplot(plot_data, aes(x = years, y = mean_occupied_sites)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = sd_lower, ymax = sd_upper), alpha = 0.4, fill = "#A9A9A9") +
  scale_x_continuous(breaks = seq(1993, 2018, 5), labels = seq(1993, 2018, 5)) +
  xlab("Year") +
  ylab("Number of Occupied Sites") +
  theme_classic()
ggsave("Results/OccupiedSitesPerYear.png")


# Plotting compelling human footprint relationships 
# Look for coefficients where the 95% credible interval (2.5% to 97.5% range) does not overlap with zero. 
# This suggests that there's a high probability that the true effect is either positive or negative, rather than zero.
# In output, beta coefficients like beta.phi[2], beta.phi[3], beta.phi[4], beta.gamma[2], and beta.gamma[7] show intervals not overlapping zero, 
# indicating potential significance.

# Larger effect sizes (the mean of the coefficient) can be more compelling, especially if they have relatively smaller standard deviations, 
# indicating precision in the estimate. For example, beta.psi[4] has a large mean (4.565) and a relatively smaller standard deviation compared to its magnitude.

# have to get unscaled covariates 
# Function to unscale covariates
yearly_covs <- read.csv("yearly_covs13Jan2024.csv")

unscale <- function(scaled, min, max) {
  (scaled * (max - min)) + min
}

# Assuming you have the mean values of your beta parameters
# For example: mean_beta_phi, mean_beta_gamma

# Unscale the covariates
unscaled_covs <- yearly_covs
unscaled_covs$NEAR.DIST.conventional.seismic <- unscale(yearly_covs$NEAR.DIST.conventional.seismic, min_values["NEAR.DIST.conventional.seismic"], max_values["NEAR.DIST.conventional.seismic"])
unscaled_covs$NEAR.DIST.unimproved.road <- unscale(yearly_covs$NEAR.DIST.unimproved.road, min_values["NEAR.DIST.unimproved.road"], max_values["NEAR.DIST.unimproved.road"])
unscaled_covs$NEAR.DIST.harvest <- unscale(yearly_covs$NEAR.DIST.harvest, min_values["NEAR.DIST.harvest"], max_values["NEAR.DIST.harvest"])
unscaled_covs$NEAR.DIST.pipeline <- unscale(yearly_covs$NEAR.DIST.pipeline, min_values["NEAR.DIST.pipeline"], max_values["NEAR.DIST.pipeline"])
unscaled_covs$MEANAGE.565.harvest <- unscale(yearly_covs$MEANAGE.565.harvest, min_values["MEANAGE.565.harvest"], max_values["MEANAGE.565.harvest"])



# Phi 
# Distance to seismic (Beta.phi[2])
print(out_cov)
intercept <- -0.541
slope <-  1.340 #mean beta seis 
ci_low_slope <- 0.014
ci_high_slope <- 2.959
dist_to_seis <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_phi <- 1 / (1 + exp(-(intercept + slope * dist_to_seis)))
lower_phi <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_seis)))
upper_phi <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_seis)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_seis, 
                        Phi = estimated_phi, 
                        Lower = lower_phi, 
                        Upper = upper_phi)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Phi)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )
ggsave("Results/PhiDistSeis.png")


# Assuming out_cov contains your model output
intercept <- out_cov$mean$beta.phi[1]
slope_seis <- out_cov$mean$beta.phi[2]
lower_slope_seis <- out_cov$q2.5$beta.phi[2]
upper_slope_seis <- out_cov$q97.5$beta.phi[2]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in c(1, 3:7)){  # Adjust all columns except the second (Distance to Seismic Line)
  x_seismic[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_seismic <- inv.logit(intercept + slope_seis * x_seismic[, 2])
lower_pred_seismic <- inv.logit(intercept + lower_slope_seis * x_seismic[, 2])
upper_pred_seismic <- inv.logit(intercept + upper_slope_seis * x_seismic[, 2])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToSeismic = x_seismic[, 2],
  Phi = mean_pred_seismic,
  LowerCI = lower_pred_seismic,
  UpperCI = upper_pred_seismic
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToSeismic, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PhiDistSeis.png", width = 8, height = 6)

slope_seis <- out_cov$mean$beta.phi[1]
lower_slope_seis <- out_cov$q2.5$beta.phi[1]
upper_slope_seis <- out_cov$q97.5$beta.phi[1]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in 2:length(x_phi[1,])){  # Adjust all columns except the first (Distance to Seismic Line)
  x_seismic[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_seismic <- inv.logit(slope_seis * x_seismic[, 1])
lower_pred_seismic <- inv.logit(lower_slope_seis * x_seismic[, 1])
upper_pred_seismic <- inv.logit(upper_slope_seis * x_seismic[, 1])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToSeismic = x_seismic[, 1],
  Phi = mean_pred_seismic,
  LowerCI = lower_pred_seismic,
  UpperCI = upper_pred_seismic
)

# Plotting with ggplot2
library(ggplot2)
ggplot(plot_data, aes(x = DistanceToSeismic, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PhiDistSeis.png", width = 8, height = 6)

# ROAD 
# Assuming out_cov contains your model output
intercept <- out_cov$mean$beta.phi[1]
slope_road <- out_cov$mean$beta.phi[3]
lower_slope_road <- out_cov$q2.5$beta.phi[3]
upper_slope_road <- out_cov$q97.5$beta.phi[3]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_phi
for(i in c(1:2, 4:7)){  # Adjust all columns except the third (Distance to Road)
  x_road[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_road <- inv.logit(intercept + slope_road * x_road[, 3])
lower_pred_road <- inv.logit(intercept + lower_slope_road * x_road[, 3])
upper_pred_road <- inv.logit(intercept + upper_slope_road * x_road[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToRoad = x_road[, 3],
  Phi = mean_pred_road,
  LowerCI = lower_pred_road,
  UpperCI = upper_pred_road
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToRoad, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  xlab("Distance to Road (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )
# Do Gamma and Seis 
# Using the mean and CI from your model output for beta.gamma[2]
intercept <- -0.385
slope <- 1.848 #mean beta seis 
ci_low_slope <- 0.658
ci_high_slope <- 2.989
dist_to_seis <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_gamma <- 1 / (1 + exp(-(intercept + slope * dist_to_seis)))
lower_gamma <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_seis)))
upper_gamma <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_seis)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_seis, 
                        Gamma = estimated_gamma, 
                        Lower = lower_gamma, 
                        Upper = upper_gamma)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Gamma)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )
ggsave("Results/GammaDistSeis.png")


# All HF 
library(dplyr)
library(tidybayes)
library(tidyverse)
library(ggplot2)
# New model output data
data <- data.frame(
  Parameter = c("Distance to Seismic line", "Distance to Road", "Distance to Harvest", 
                "Distance to Pipeline", "Harvest Age", "HarvestAge*DistHarvest"),
  Type = rep(c("Phi", "Gamma"), each = 6),
  Value = c(out_cov_dist_interaction30000$mean$beta.phi[1], out_cov_dist_interaction30000$mean$beta.phi[2], 
            out_cov_dist_interaction30000$mean$beta.phi[3], out_cov_dist_interaction30000$mean$beta.phi[4], 
            out_cov_dist_interaction30000$mean$beta.phi[5], out_cov_dist_interaction30000$mean$beta.phi[6],
            out_cov_dist_interaction30000$mean$beta.gamma[1], out_cov_dist_interaction30000$mean$beta.gamma[2], 
            out_cov_dist_interaction30000$mean$beta.gamma[3], out_cov_dist_interaction30000$mean$beta.gamma[4],
            out_cov_dist_interaction30000$mean$beta.gamma[5], out_cov_dist_interaction30000$mean$beta.gamma[6]),
  LowCI = c(out_cov_dist_interaction30000$q2.5$beta.phi[1], out_cov_dist_interaction30000$q2.5$beta.phi[2], 
            out_cov_dist_interaction30000$q2.5$beta.phi[3], out_cov_dist_interaction30000$q2.5$beta.phi[4], 
            out_cov_dist_interaction30000$q2.5$beta.phi[5], out_cov_dist_interaction30000$q2.5$beta.phi[6],
            out_cov_dist_interaction30000$q2.5$beta.gamma[1], out_cov_dist_interaction30000$q2.5$beta.gamma[2], 
            out_cov_dist_interaction30000$q2.5$beta.gamma[3], out_cov_dist_interaction30000$q2.5$beta.gamma[4],
            out_cov_dist_interaction30000$q2.5$beta.gamma[5], out_cov_dist_interaction30000$q2.5$beta.gamma[6]),
  HighCI = c(out_cov_dist_interaction30000$q97.5$beta.phi[1], out_cov_dist_interaction30000$q97.5$beta.phi[2], 
             out_cov_dist_interaction30000$q97.5$beta.phi[3], out_cov_dist_interaction30000$q97.5$beta.phi[4], 
             out_cov_dist_interaction30000$q97.5$beta.phi[5], out_cov_dist_interaction30000$q97.5$beta.phi[6],
             out_cov_dist_interaction30000$q97.5$beta.gamma[1], out_cov_dist_interaction30000$q97.5$beta.gamma[2], 
             out_cov_dist_interaction30000$q97.5$beta.gamma[3], out_cov_dist_interaction30000$q97.5$beta.gamma[4],
             out_cov_dist_interaction30000$q97.5$beta.gamma[5], out_cov_dist_interaction30000$q97.5$beta.gamma[6])
)

# Adjust the size and position of points and error bars
dodge <- position_dodge(width = 0.25)
custom_labels <- c("Phi" = "Probability of Persistence", "Gamma" = "Probability of Colonization")

ggplot(data, aes(x = Parameter, y = Value, color = Type)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_errorbar(aes(ymin = LowCI, ymax = HighCI), position = position_dodge(width = 0.25), width = 0, size = 1) +
  scale_color_manual(values = c("Phi" = "red", "Gamma" = "blue"), labels = custom_labels) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  coord_flip() +
  labs(x = "", y = "Posterior Values", color = NULL) + # Remove the 'Parameters' label and legend title
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom", # Move legend to bottom
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = rel(1.0)),
    axis.text.x = element_text(color = "black", size = rel(1.2)), # Adjust the size of the legend text if necessary
    axis.text.y = element_text(color = "black", size = rel(1.2))
  ) +
  guides(color = guide_legend(title.position = "bottom")) # Ensure the legend title is positioned correctly

# If you want to save the plot, uncomment the next line
ggsave("Results/human_footprint_effects_plot.png", width = 10, height = 8)



# Data frame with the correct values for the treatments
trt_data <- data.frame(
  Treatment = factor(c("Control", "Riparian", "Fragment"), levels = c("Control", "Riparian", "Fragment")),
  Median = c(-0.397, -0.363, -0.026), # Median values
  Q25 = c(-2.060140, -2.012926, -1.652895), # 25th percentile
  Q75 = c(1.210531, 1.316863, 1.655694),    # 75th percentile
  CI_Lower = c(-4.467, -4.371, -4.014),      # 2.5% CI values
  CI_Upper = c(4.639, 4.595, 4.996)          # 97.5% CI values
)

# Create a box plot with the provided values and custom colors
p <- ggplot(trt_data, aes(x = Treatment, y = Median)) +
  geom_boxplot(aes(lower = Q25, middle = Median, upper = Q75, ymin = CI_Lower, ymax = CI_Upper), 
               stat = "identity", 
               fill = c("pink", "light blue", "light green")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Treatment", y = "Posterior Values") +
  theme_classic() +
  theme(legend.position = "none")

# Print the plot
print(p)



# Do Harvest and Phi 
# Using the mean and CI from your model output for beta.gamma[2]
intercept <- -0.541
slope <-  3.016 #mean beta harvest dist 
ci_low_slope <- 1.070
ci_high_slope <-  5.094
dist_to_harv <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_phi <- 1 / (1 + exp(-(intercept + slope * dist_to_harv)))
lower_phi <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_harv)))
upper_phi <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_harv)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_harv, 
                        Phi = estimated_phi, 
                        Lower = lower_phi, 
                        Upper = upper_phi)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Phi)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Distance to Harvest (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() 
ggsave("Results/PhiDistHarv.png")


# Do Road and Phi 
print(out_cov)
# Using the mean and CI from your model output for beta.gamma[2]
intercept <- -0.541
slope <-  1.257 #mean beta harvest dist 
ci_low_slope <- 0.502
ci_high_slope <-  1.995
dist_to_road <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_phi <- 1 / (1 + exp(-(intercept + slope * dist_to_road)))
lower_phi <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_road)))
upper_phi <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_road)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_road, 
                        Phi = estimated_phi, 
                        Lower = lower_phi, 
                        Upper = upper_phi)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Phi)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Distance to Road (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() 
ggsave("Results/PhiDistRoad.png")


# Do Pipe and Phi 
print(out_cov)
# Using the mean and CI from your model output for beta.gamma[2]
intercept <- -0.541
slope <-  -0.937 #mean beta harvest dist 
ci_low_slope <- -1.720
ci_high_slope <-  -0.192
dist_to_pipe <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_phi <- 1 / (1 + exp(-(intercept + slope * dist_to_pipe)))
lower_phi <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_pipe)))
upper_phi <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_pipe)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_pipe, 
                        Phi = estimated_phi, 
                        Lower = lower_phi, 
                        Upper = upper_phi)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Phi)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Distance to Pipeline (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() 
ggsave("Results/PhiDistPipe.png")


# Do Harvest Age and Phi 
print(out_cov)
range(unscaled_covs$MEANAGE.565.harvest)
# Using the mean and CI from your model output for beta.gamma[2]
intercept <- -0.541
slope <-  -1.394 #mean beta harvest dist 
ci_low_slope <- -2.433
ci_high_slope <-  -0.378
dist_to_pipe <- seq(0, 100, length.out = 10000)

# Calculate estimated probabilities
estimated_phi <- 1 / (1 + exp(-(intercept + slope * dist_to_pipe)))
lower_phi <- 1 / (1 + exp(-(intercept + ci_low_slope * dist_to_pipe)))
upper_phi <- 1 / (1 + exp(-(intercept + ci_high_slope * dist_to_pipe)))

# Data frame for plotting
plot_data <- data.frame(Distance = dist_to_pipe, 
                        Phi = estimated_phi, 
                        Lower = lower_phi, 
                        Upper = upper_phi)

# Plotting
ggplot(plot_data, aes(x = Distance, y = Phi)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line() +
  xlab("Harvest Age") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() 
ggsave("Results/PhiHarvestAge.png")



# Plot the treatment effect and phi 
treatment_effects <- data.frame(
  Treatment = c("Fragmented", "Riparian"),
  Logit_Effect = c(1.132, 1.499),
  LowerCI_Logit = c(0.390, 0.499),
  UpperCI_Logit = c(1.878, 2.566)
)

# Back-transform from logit to probability scale
treatment_effects$Effect <- exp(treatment_effects$Logit_Effect) / (1 + exp(treatment_effects$Logit_Effect))
treatment_effects$LowerCI <- exp(treatment_effects$LowerCI_Logit) / (1 + exp(treatment_effects$LowerCI_Logit))
treatment_effects$UpperCI <- exp(treatment_effects$UpperCI_Logit) / (1 + exp(treatment_effects$UpperCI_Logit))

# Plotting
ggplot(treatment_effects, aes(x = Treatment, y = Effect)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(width = 0.1)) +
  ylab("Estimated Probability of Effect") +
  xlab("Treatment") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )
