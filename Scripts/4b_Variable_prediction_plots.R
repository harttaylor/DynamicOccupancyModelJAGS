# Create predictions (hold means constant for all beta variables but the one you want to plot) to plot effects 
# of covariates on parameters 
library(boot)
library(ggplot2)
library(dplyr)

out_cov_edge <- readRDS("Results/EDGE_resultsFeb3.rds")
#out_edge_RE <- readRDS("Results/EDGE_RE_model12000.rds")

# Psi 
# Stand Age 
intercept_psi <- out_cov_edge$mean$beta.psi[1]
slope_standage <- out_cov_edge$mean$beta.psi[4]
lower_slope_standage <-out_cov_edge$q25$beta.psi[4]
upper_slope_standage <-out_cov_edge$q75$beta.psi[4]

# Hold other psi covs constant at their means 
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
ggsave("Results/PsiStandAge50percentquartiles.png", width = 8, height = 6)


# Percent Conifer 
intercept_psi <- out_cov_edge$mean$beta.psi[1]
slope_percentconifer <- out_cov_edge$mean$beta.psi[2]
lower_slope_percentconifer <-out_cov_edge$q2.5$beta.psi[2]
upper_slope_percentconifer <-out_cov_edge$q97.5$beta.psi[2]

# Hold other psi covs constant at their means 
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
  Psi = mean_pred_percentconifer,
  LowerCI = lower_pred_percentconifer ,
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
ggsave("Results/PsiPECO.png", width = 8, height = 6)




# Phi 
intercept <- out_cov_edge$mean$beta.phi[1]
slope_seis <- out_cov_edge$mean$beta.phi[2]
lower_slope_seis <- out_cov_edge$q2.5$beta.phi[2]
upper_slope_seis <- out_cov_edge$q97.5$beta.phi[2]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in c(1, 3:5)){  # Adjust all columns except the second (Distance to Seismic Line)
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
intercept <- out_cov_edge$mean$beta.phi[1]
slope_road <- out_cov_edge$mean$beta.phi[3]
lower_slope_road <- out_cov_edge$q2.5$beta.phi[3]
upper_slope_road <- out_cov_edge$q97.5$beta.phi[3]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_phi
for(i in c(1:2, 4:5)){  # Adjust all columns except the third (Distance to Road)
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

# Save the plot
ggsave("Results/PhiDistRoad.png", width = 8, height = 6)

# PIPELINE
intercept <- out_cov_edge$mean$beta.phi[1]
slope_pipeline <- out_cov_edge$mean$beta.phi[4]
lower_slope_pipeline <- out_cov_edge$q2.5$beta.phi[4]
upper_slope_pipeline <- out_cov_edge$q97.5$beta.phi[4]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Pipeline"
# Holding all other covariates constant at their mean values
x_pipeline <- x_phi
for(i in c(1:3, 5)){  # Adjust all columns except the fifth (Distance to Pipeline)
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
#intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- out_cov_edge$mean$beta.phi[5]
lower_slope_harvest <- out_cov_edge$q2.5$beta.phi[5]
upper_slope_harvest <- out_cov_edge$q97.5$beta.phi[5]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_phi
for(i in c(1:4)){  # Adjust all columns except the fourth (Distance to Harvest)
  x_harvest[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_harvest <- inv.logit(intercept + slope_harvest * x_harvest[, 5])
lower_pred_harvest <- inv.logit(intercept + lower_slope_harvest * x_harvest[, 5])
upper_pred_harvest <- inv.logit(intercept + upper_slope_harvest * x_harvest[, 5])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 5],
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
intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- out_cov$mean$beta.phi[6]
lower_slope_harvest <- out_cov$q2.5$beta.phi[6]
upper_slope_harvest <- out_cov$q97.5$beta.phi[6]

# x_phi is your covariate matrix
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

# SEISMIC LINE
intercept <- out_cov_edge$mean$beta.gamma[1]
slope_seis <- out_cov_edge$mean$beta.gamma[2]
lower_slope_seis <- out_cov_edge$q2.5$beta.gamma[2]
upper_slope_seis <- out_cov_edge$q97.5$beta.gamma[2]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_seis <- x_gamma
for(i in c(1, 3:5)){  # Adjust all columns except the third (Distance to seis)
  x_seis[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_seis <- inv.logit(intercept + slope_seis * x_seis[, 2])
lower_pred_seis <- inv.logit(intercept + lower_slope_seis * x_seis[, 2])
upper_pred_seis <- inv.logit(intercept + upper_slope_seis * x_seis[, 2])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToSeis = x_seis[, 2],
  Gamma = mean_pred_seis,
  LowerCI = lower_pred_seis,
  UpperCI = upper_pred_seis
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToSeis, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.2) +
  geom_line(color = "darkgreen") +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/GammaSeis.png", width = 8, height = 6)


# ROAD 
intercept <- out_cov_edge$mean$beta.gamma[1]
slope_road <- out_cov_edge$mean$beta.gamma[3]
lower_slope_road <- out_cov_edge$q2.5$beta.gamma[3]
upper_slope_road <- out_cov_edge$q97.5$beta.gamma[3]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_gamma
for(i in c(1:2, 4:5)){  # Adjust all columns except the third (Distance to Road)
  x_road[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_road <- inv.logit(intercept + slope_road * x_road[, 3])
lower_pred_road <- inv.logit(intercept + lower_slope_road * x_road[, 3])
upper_pred_road <- inv.logit(intercept + upper_slope_road * x_road[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToRoad = x_road[, 3],
  Gamma = mean_pred_road,
  LowerCI = lower_pred_road,
  UpperCI = upper_pred_road
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

# Save the plot
ggsave("Results/GammaRoad.png", width = 8, height = 6)



# PIPE
intercept <- out_cov_edge$mean$beta.gamma[1]
slope_pipe <- out_cov_edge$mean$beta.gamma[4]
lower_slope_pipe <- out_cov_edge$q2.5$beta.gamma[4]
upper_slope_pipe <- out_cov_edge$q97.5$beta.gamma[4]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_pipe <- x_gamma
for(i in c(1:3, 5)){  # Adjust all columns except the third (Distance to Road)
  x_pipe[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_pipe <- inv.logit(intercept + slope_pipe * x_pipe[, 4])
lower_pred_pipe <- inv.logit(intercept + lower_slope_pipe * x_pipe[, 4])
upper_pred_pipe <- inv.logit(intercept + upper_slope_pipe * x_pipe[, 4])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToPipe = x_pipe[, 4],
  Gamma = mean_pred_pipe,
  LowerCI = lower_pred_pipe,
  UpperCI = upper_pred_pipe
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToPipe, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "black", alpha = 0.2) +
  geom_line(color = "black") +
  xlab("Distance to Seismic Line (m)") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/GammaPipe.png", width = 8, height = 6)

# HARVEST DISTANCE 
#intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- out_cov_edge$mean$beta.gamma[5]
lower_slope_harvest <- out_cov_edge$q2.5$beta.gamma[5]
upper_slope_harvest <- out_cov_edge$q97.5$beta.gamma[5]

# x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_gamma
for(i in c(1:4)){  # Adjust all columns except the fourth (Distance to Harvest)
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
ggsave("Results/GammaDistHarvest.png", width = 8, height = 6)



# HARVEST AGE 
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



