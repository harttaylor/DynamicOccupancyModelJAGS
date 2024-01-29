# Create predictions (hold means constant for all beta variables but the one you want to plot) to plot effects 
# of covariates on parameters 
library(boot)
library(ggplot2)
library(dplyr)


# Simpler plots 

beta.phi <- out_cov$mean$beta.phi
lower.beta.phi <- out_cov$q2.5$beta.phi
upper.beta.phi <- out_cov$q97.5$beta.phi

x_phi <- win.data$x.phi[, 2, ] # this is for year 2 only but wont change the relationship because the beta is the same for all years 

# This line calculates predicted values using all original covariates (x_phi) and the coefficients (beta.phi). 
# The %*% operator performs matrix multiplication.

pred1 <- x_phi %*% beta.phi


x.road <- x_phi 

# selects the 3 covariate in the list which is distance to road to allow values to vary, and all other covariates are held at their mean to 
# isolate the effect of the oen of interest 
# The loop iterates over certain columns of x.road (all except the third, which represents "distance to road") 
# and replaces their values with the mean of those columns. This "holds constant" these covariates at their mean values.
for(x in c(1,2,4:7)){
  x.road[, x] <- mean(x_phi[, x])
}

pred2 <- x.road %*% beta.phi # This line calculates new predicted values using the adjusted covariates (x.road) and the coefficients (beta.phi). 

plot(x.road[, 3], inv.logit(pred2))

# Basic scatter plot of the original data points
# png("plot_distance_to_road.png", width = 800, height = 600)
plot(x.road[, 3], inv.logit(pred2), 
     xlab = "Scaled Distance to Road (m)", ylab = "Probability of Persistence")

# Add a loess smoothed trendline
lines(smooth.spline(x.road[, 3], inv.logit(pred2)), col = "blue", lwd = 1)


# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in c(1, 3:7)){  # Adjust all columns except the second (Distance to Seismic Line)
  x_seismic[, i] <- mean(x_phi[, i])
}

# Calculate new predictions
pred_seismic <- x_seismic %*% beta.phi

# Basic scatter plot of the original data points
#png("plot_distance_to_seismic_line.png", width = 800, height = 600)
plot(x_seismic[, 2], inv.logit(pred_seismic), 
     xlab = "Scaled Distance to Seismic Line (m)", ylab = "Probability of Persistence")

# Add a loess smoothed trendline
lines(smooth.spline(x_seismic[, 2], inv.logit(pred_seismic)), col = "green", lwd = 1)



# For harvest 
x_harvest <- x_phi
for(i in c(1:3, 5:7)){  # Adjust all columns except the fourth (Distance to Harvest)
  x_harvest[, i] <- mean(x_phi[, i])
}

# Calculate new predictions
pred_harvest <- x_harvest %*% beta.phi

# Basic scatter plot of the original data points
# png("plot_distance_to_harvest.png", width = 800, height = 600)
plot(x_harvest[, 4], inv.logit(pred_harvest), 
     xlab = "Scaled Distance to Harvest (m)", ylab = "Probability of Persistence")

# Add a loess smoothed trendline
lines(smooth.spline(x_harvest[, 4], inv.logit(pred_harvest)), col = "red", lwd = 1)






# Nicer plots 
# Psi 
# Stand Age 
intercept_psi <- out_cov_harv2$mean$beta.psi[1]
slope_standage <- out_cov_harv2$mean$beta.psi[4]
lower_slope_standage <-out_cov_harv2$q2.5$beta.psi[4]
upper_slope_standage <-out_cov_harv2$q97.5$beta.psi[4]

# Hold over psi covs constant at their means 
x_psi <- win.data$x.psi[ , ]
x_standage <- x_psi
for(i in c(1:3)){
  x_standage[, i] <- mean(x_psi[, i])
}

mean_pred_standage <- inv.logit(intercept_psi + slope_standage * x_standage[, 4])
lower_pred_standage <- inv.logit(intercept_psi + lower_slope_standage * x_standage[, 4])
upper_pred_standage <- inv.logit(intercept_psi + upper_slope_standage * x_standage[, 4])

# Data frame for plotting
plot_data <- data.frame(
  StandAge = x_standage[, 4],
  Psi = mean_pred_standage,
  LowerCI = lower_pred_standage,
  UpperCI = upper_pred_standage
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = StandAge, y = Psi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.2) +
  geom_line(color = "darkgreen") +
  xlab("Stand Age") +
  ylab("Estimated Probability of initial occupancy (Psi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PsiStandAge.png", width = 8, height = 6)


# Percent Conifer 
intercept_psi <- out_cov_harv2$mean$beta.psi[1]
slope_percentconifer <- out_cov_harv2$mean$beta.psi[2]
lower_slope_percentconifer <-out_cov_harv2$q2.5$beta.psi[2]
upper_slope_percentconifer <-out_cov_harv2$q97.5$beta.psi[2]

# Hold over psi covs constant at their means 
x_psi <- win.data$x.psi[ , ]
x_percentconifer <- x_psi
for(i in c(1, 3:4)){
  x_percentconifer[, i] <- mean(x_psi[, i])
}

mean_pred_percentconifer <- inv.logit(intercept_psi + slope_percentconifer * x_percentconifer[, 2])
lower_pred_percentconifer <- inv.logit(intercept_psi + lower_slope_percentconifer * x_percentconifer[, 2])
upper_pred_percentconifer <- inv.logit(intercept_psi + upper_slope_percentconifer * x_percentconifer[, 2])

# Data frame for plotting
plot_data <- data.frame(
  PercentConifer = x_percentconifer[, 2],
  Psi = mean_pred_perconifer,
  LowerCI = lower_pred_percentconifer,
  UpperCI = upper_pred_percentconifer
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = PercentConifer, y = Psi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "brown", alpha = 0.2) +
  geom_line(color = "brown") +
  xlab("Percent Conifer") +
  ylab("Estimated Probability of initial occupancy (Psi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PsiPeCO.png", width = 8, height = 6)




# Phi 
# Assuming out_cov contains your model output
slope_seis <- out_cov_dist_interaction30000$mean$beta.phi[1]
lower_slope_seis <- out_cov_dist_interaction30000$q2.5$beta.phi[1]
upper_slope_seis <- out_cov_dist_interaction30000$q97.5$beta.phi[1]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in c(2:6)){  # Adjust all columns except the second (Distance to Seismic Line)
  x_seismic[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_seismic <- inv.logit(intercept + slope_seis * x_seismic[, 1])
lower_pred_seismic <- inv.logit(intercept + lower_slope_seis * x_seismic[, 1])
upper_pred_seismic <- inv.logit(intercept + upper_slope_seis * x_seismic[, 1])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToSeismic = x_seismic[, 1],
  Phi = mean_pred_seismic,
  LowerCI = lower_pred_seismic,
  UpperCI = upper_pred_seismic
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToSeismic, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.2) +
  geom_line(color = "darkgreen") +
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
slope_road <- out_cov_dist_interaction30000$mean$beta.phi[2]
lower_slope_road <- out_cov_dist_interaction30000$q2.5$beta.phi[2]
upper_slope_road <- out_cov_dist_interaction30000$q97.5$beta.phi[2]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_phi
for(i in c(1, 3:6)){  # Adjust all columns except the third (Distance to Road)
  x_road[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_road <- inv.logit(intercept + slope_road * x_road[, 2])
lower_pred_road <- inv.logit(intercept + lower_slope_road * x_road[, 2])
upper_pred_road <- inv.logit(intercept + upper_slope_road * x_road[, 2])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToRoad = x_road[, 2],
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



# PIPELINE
# Assuming out_cov contains your model output
slope_pipeline <- out_cov_dist_interaction30000$mean$beta.phi[4]
lower_slope_pipeline <- out_cov_dist_interaction30000$q2.5$beta.phi[4]
upper_slope_pipeline <- out_cov_dist_interaction30000$q97.5$beta.phi[4]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Pipeline"
# Holding all other covariates constant at their mean values
x_pipeline <- x_phi
for(i in c(1:3, 5:6)){  # Adjust all columns except the fifth (Distance to Pipeline)
  x_pipeline[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_pipeline <- inv.logit(intercept + slope_pipeline * x_pipeline[, 4])
lower_pred_pipeline <- inv.logit(intercept + lower_slope_pipeline * x_pipeline[, 4])
upper_pred_pipeline <- inv.logit(intercept + upper_slope_pipeline * x_pipeline[, 4])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToPipeline = x_pipeline[, 4],
  Phi = mean_pred_pipeline,
  LowerCI = lower_pred_pipeline,
  UpperCI = upper_pred_pipeline
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToPipeline, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "black", alpha = 0.2) +
  geom_line(color = "black") +
  xlab("Distance to Pipeline (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PhiDistPipeline.png", width = 8, height = 6)



# HARVEST DISTANCE 
# Assuming out_cov contains your model output
#intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- out_cov_dist_interaction30000$mean$beta.phi[3]
lower_slope_harvest <- out_cov_dist_interaction30000$q2.5$beta.phi[3]
upper_slope_harvest <- out_cov_dist_interaction30000$q97.5$beta.phi[3]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_phi
for(i in c(1:2, 4:6)){  # Adjust all columns except the fourth (Distance to Harvest)
  x_harvest[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_harvest <- inv.logit(intercept + slope_harvest * x_harvest[, 3])
lower_pred_harvest <- inv.logit(intercept + lower_slope_harvest * x_harvest[, 3])
upper_pred_harvest <- inv.logit(intercept + upper_slope_harvest * x_harvest[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 3],
  Phi = mean_pred_harvest,
  LowerCI = lower_pred_harvest,
  UpperCI = upper_pred_harvest
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "red", alpha = 0.2) +
  geom_line(color = "red") +
  xlab("Distance to Harvest (m)") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )


# Save the plot
ggsave("Results/PhiDistHarvest.png", width = 8, height = 6)



# HARVEST AGE  
# Assuming out_cov contains your model output
intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- out_cov$mean$beta.phi[6]
lower_slope_harvest <- out_cov$q2.5$beta.phi[6]
upper_slope_harvest <- out_cov$q97.5$beta.phi[6]

# Assuming x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Harvest Age"
# Holding all other covariates constant at their mean values
x_harvest <- x_phi
for(i in c(1:5, 7)){  # Adjust all columns except the sixth (Harvest Age)
  x_harvest[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_harvest <- inv.logit(intercept + slope_harvest * x_harvest[, 4])
lower_pred_harvest <- inv.logit(intercept + lower_slope_harvest * x_harvest[, 4])
upper_pred_harvest <- inv.logit(intercept + upper_slope_harvest * x_harvest[, 4])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 4],
  Phi = mean_pred_harvest,
  LowerCI = lower_pred_harvest,
  UpperCI = upper_pred_harvest
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  xlab("Harvest Age") +
  ylab("Estimated Probability of Persistence (Phi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )


# Save the plot
ggsave("Results/PhiAgeHarvest.png", width = 8, height = 6)









# GAMMAAAAAAAAAAAAAAAAAAAA
# ROAD 
# Assuming out_cov contains your model output
gslope_road <- out_cov_dist_interaction30000$mean$beta.gamma[2]
glower_slope_road <- out_cov_dist_interaction30000$q2.5$beta.gamma[2]
gupper_slope_road <- out_cov_dist_interaction30000$q97.5$beta.gamma[2]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_gamma
for(i in c(1, 3:6)){  # Adjust all columns except the third (Distance to Road)
  x_road[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
gmean_pred_road <- inv.logit(intercept + gslope_road * x_road[, 2])
glower_pred_road <- inv.logit(intercept + glower_slope_road * x_road[, 2])
gupper_pred_road <- inv.logit(intercept + gupper_slope_road * x_road[, 2])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToRoad = x_road[, 2],
  Gamma = gmean_pred_road,
  LowerCI = glower_pred_road,
  UpperCI = gupper_pred_road
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToRoad, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  xlab("Distance to Road (m)") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )



# HARVEST DISTANCE 
# Assuming out_cov contains your model output
#intercept <- out_cov$mean$beta.phi[1]
gslope_harvest <- out_cov_dist_interaction30000$mean$beta.gamma[3]
glower_slope_harvest <- out_cov_dist_interaction30000$q2.5$beta.gamma[3]
gupper_slope_harvest <- out_cov_dist_interaction30000$q97.5$beta.gamma[3]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_gamma
for(i in c(1:2, 4:6)){  # Adjust all columns except the fourth (Distance to Harvest)
  x_harvest[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
gmean_pred_harvest <- inv.logit(intercept + gslope_harvest * x_harvest[, 3])
glower_pred_harvest <- inv.logit(intercept + glower_slope_harvest * x_harvest[, 3])
gupper_pred_harvest <- inv.logit(intercept + gupper_slope_harvest * x_harvest[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 3],
  Gamma = gmean_pred_harvest,
  LowerCI = glower_pred_harvest,
  UpperCI = gupper_pred_harvest
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "red", alpha = 0.2) +
  geom_line(color = "red") +
  xlab("Distance to Harvest (m)") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )


# Save the plot
ggsave("Results/PhiDistHarvest.png", width = 8, height = 6)



# HARVEST AGE  
# Assuming out_cov contains your model output
#intercept <- out_cov_dist_interaction30000$mean$beta.gamma[5]
slope_harvest <- out_cov_dist_interaction30000$mean$beta.gamma[5]
lower_slope_harvest <- out_cov_dist_interaction30000$q2.5$beta.gamma[5]
upper_slope_harvest <- out_cov_dist_interaction30000$q97.5$beta.gamma[5]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Harvest Age"
# Holding all other covariates constant at their mean values
x_harvest <- x_gamma
for(i in c(1:4, 6)){  # Adjust all columns except the sixth (Harvest Age)
  x_harvest[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_harvest <- inv.logit(intercept + slope_harvest * x_harvest[, 5])
lower_pred_harvest <- inv.logit(intercept + lower_slope_harvest * x_harvest[, 5])
upper_pred_harvest <- inv.logit(intercept + upper_slope_harvest * x_harvest[, 5])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 5],
  Gamma = mean_pred_harvest,
  LowerCI = lower_pred_harvest,
  UpperCI = upper_pred_harvest
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkorange", alpha = 0.2) +
  geom_line(color = "black") +
  xlab("Harvest Age") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )


# Save the plot
ggsave("Results/GamAgeHarvest.png", width = 8, height = 6)






# HARVEST INTERACTION  
# Assuming out_cov contains your model output
#intercept <- out_cov_dist_interaction30000$mean$beta.gamma[5]
# Example model coefficients
intercept <- -2.868 # average of mean values of alpha gamma 
slope_age <- out_cov_dist12001$mean$beta.gamma[4]
slope_distance <- out_cov_dist12001$mean$beta.gamma[3] # Adjust as per your model
slope_interaction <- out_cov_dist12001$mean$beta.gamma[5] # Adjust as per your model

# Define two specific harvest ages
ages <- c(0, 1)
# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Harvest Age"
# Holding all other covariates constant at their mean values
x_harvest <- x_gamma
for(i in c(1:2, 4:5)){  # Adjust all columns except the sixth (Harvest Age)
  x_harvest[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions for harvest age 0
Gamma_age_0 <- inv.logit(intercept + 
                           slope_age * 0 + 
                           slope_distance * x_harvest[, 3] + 
                           slope_interaction * 0 * x_harvest[, 3])

# Calculate new predictions for harvest age 25
Gamma_age_25 <- inv.logit(intercept + 
                            slope_age * 0.25 + 
                            slope_distance * x_harvest[, 3] + 
                            slope_interaction * 0.25 * x_harvest[, 3])
Gamma_age_5 <- inv.logit(intercept + 
                            slope_age * 0.5 + 
                            slope_distance * x_harvest[, 3] + 
                            slope_interaction * 0.5 * x_harvest[, 3])
Gamma_age_75 <- inv.logit(intercept + 
                            slope_age * 0.75 + 
                            slope_distance * x_harvest[, 3] + 
                            slope_interaction * 0.75 * x_harvest[, 3])
# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = rep(x_harvest[, 3], 4),
  Gamma = c(Gamma_age_0, Gamma_age_25, Gamma_age_5, Gamma_age_75),
  HarvestAge = factor(rep(c("0 years", "7.5 years", "15 years", "22.5 years"), each = length(x_harvest[, 3])))
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma, color = HarvestAge)) +
  geom_line() +
  xlab("Distance to Harvest") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  ) +
  scale_color_manual(values = c("black", "blue", "red", "orange"))  # Adjust colors as needed

# Save the plot
ggsave("Results/HarvestAgeDistanceInteractionPlot.png", width = 10, height = 6)




