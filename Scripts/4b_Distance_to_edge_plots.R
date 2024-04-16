# Create predictions (hold means constant for all beta variables but the one you want to plot) to plot effects 
# of covariates on parameters 
library(boot)
library(ggplot2)
library(dplyr)
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)
library(extrafont)
font_import()
loadfonts(device = "win")


# Psi 
# Stand Age 
intercept_psi <- logedgeRE$mean$beta.psi[1]
slope_standage <- logedgeRE$mean$beta.psi[4]
lower_slope_standage <-logedgeRE$q2.5$beta.psi[4]
upper_slope_standage <-logedgeRE$q97.5$beta.psi[4]

# Hold other psi covs constant at their means 
x_psi <- win.data$x.psi[ , ]
x_standage <- x_psi
for(i in c(1:3, )){
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
plot_data$StandAge <- plot_data$StandAge * 100
# Plotting with ggplot2
ggplot(plot_data, aes(x = StandAge, y = Psi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.1) +
  geom_line(color = "darkgreen") +
  xlab("Stand Age") +
  ylab("Estimated Probability of initial occupancy (Psi)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("Results/PsiStandAge50percentquartiles.png", width = 8, height = 6)




# Phi 
intercept <- -1.56
intercept <- logedgeRE$mean$alpha.phi[2]
slope_seis <- logedgeRE$mean$beta.phi[1]
lower_slope_seis <- logedgeRE$q2.5$beta.phi[1]
upper_slope_seis <- logedgeRE$q97.5$beta.phi[1]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Seismic Line"
# Holding all other covariates constant at their mean values
x_seismic <- x_phi
for(i in c(2:4)){  # Adjust all columns except the second (Distance to Seismic Line)
  x_seismic[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
# Linear distances 
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

plot_data$DistanceToSeismic <- exp(plot_data$DistanceToSeismic) -1
# Plot for Seismic
plot_seismic <- ggplot(plot_data, aes(x = DistanceToSeismic, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.08) +
  geom_line(color = "darkgreen", linewidth = 1) +
  xlab("Distance to Seismic Line (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  )
 # annotate("text", x = min(plot_data$DistanceToSeismic), y = max(plot_data$Phi), label = "", hjust = -0.2, vjust = -6, size = 10) 
 #+
  #scale_y_continuous(limits = c(0.5, 1)) # Set the limits here as appropriate for your data

print(plot_seismic)

# Save the plot
ggsave("Results/Mar7PhiDistSeis.png", width = 8, height = 6)



# ROAD 
slope_road <- logedgeRE$mean$beta.phi[2]
lower_slope_road <- logedgeRE$q2.5$beta.phi[2]
upper_slope_road <- logedgeRE$q97.5$beta.phi[2]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_phi
for(i in c(1, 3:4)){  # Adjust all columns except the third (Distance to Road)
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

plot_data$DistanceToRoad <- exp(plot_data$DistanceToRoad) -1
# Plotting with ggplot2
plot_road <- ggplot(plot_data, aes(x = DistanceToRoad, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.08) +
  geom_line(color = "blue", linewidth = 1) +
  xlab("Distance to Road (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  ) +
  scale_x_continuous(limits = c(44, 1000))  # Set x-axis limits

print(plot_road)
# Save the plot
ggsave("Results/Mar7PhiDistRoad.png", width = 8, height = 6)


# PIPELINE
slope_pipeline <- logedgeRE$mean$beta.phi[3]
lower_slope_pipeline <- logedgeRE$q2.5$beta.phi[3]
upper_slope_pipeline <- logedgeRE$q97.5$beta.phi[3]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Pipeline"
# Holding all other covariates constant at their mean values
x_pipeline <- x_phi
for(i in c(1:2, 4)){  # Adjust all columns except the fifth (Distance to Pipeline)
  x_pipeline[, i] <- mean(x_phi[, i])
}

# Calculate new predictions and credible intervals
mean_pred_pipeline <- inv.logit(intercept + slope_pipeline * x_pipeline[, 3])
lower_pred_pipeline <- inv.logit(intercept + lower_slope_pipeline * x_pipeline[, 3])
upper_pred_pipeline <- inv.logit(intercept + upper_slope_pipeline * x_pipeline[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToPipeline = x_pipeline[, 3],
  Phi = mean_pred_pipeline,
  LowerCI = lower_pred_pipeline,
  UpperCI = upper_pred_pipeline
)

plot_data$DistanceToPipeline <- exp(plot_data$DistanceToPipeline)
# Plotting with ggplot2
plot_pipeline <- ggplot(plot_data, aes(x = DistanceToPipeline, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "black", alpha = 0.08) +
  geom_line(color = "black", linewidth = 1) +
  xlab("Distance to Pipeline (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  ) +
  scale_x_continuous(limits = c(14, 1000))  # Set x-axis limits

print(plot_pipeline)
# Save the plot
ggsave("Results/Mar7PhiDistPipeline.png", width = 8, height = 6)



# HARVEST DISTANCE 
#intercept <- out_cov$mean$beta.phi[1]
slope_harvest <- logedgeRE$mean$beta.phi[4]
lower_slope_harvest <- logedgeRE$q2.5$beta.phi[4]
upper_slope_harvest <- logedgeRE$q97.5$beta.phi[4]

# x_phi is your covariate matrix
x_phi <- win.data$x.phi[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_phi
for(i in c(1:3)){  # Adjust all columns except the fourth (Distance to Harvest)
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

plot_data$DistanceToHarvest <- exp(plot_data$DistanceToHarvest)
# Plotting with ggplot2
plot_harvest <- ggplot(plot_data, aes(x = DistanceToHarvest, y = Phi)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "red", alpha = 0.08) +
  geom_line(color = "red", linewidth = 1) +
  xlab("Distance to Harvest (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  ) +
  annotate("text", x = min(plot_data$DistanceToHarvest), y = max(plot_data$Phi), label = "", hjust = -0.2, vjust = -6, size = 10)
print(plot_harvest)


# Save the plot
ggsave("Results/Mar7PhiDistHarvest.png", width = 8, height = 6)


# create a panel that displays all four footprint plots horizontally
library(patchwork)

# Combine the plots in a 2x2 grid
combined_plot <- plot_seismic + plot_road + 
  plot_pipeline + plot_harvest + 
  plot_layout(ncol = 2, nrow = 2)
print(combined_plot)
# Save the combined plot
ggsave("Results/Edge model/April15CombinedPhiPlots_2x2.png", combined_plot, width = 16, height = 12)




# GAMMAAAAAAAAAAAAAAAAAAAA

# SEISMIC LINE
intercept <- -1.87
intercept <- logedgeRE$mean$alpha.gamma[13]
slope_seis <- logedgeRE$mean$beta.gamma[1]
lower_slope_seis <- logedgeRE$q2.5$beta.gamma[1]
upper_slope_seis <- logedgeRE$q97.5$beta.gamma[1]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_seis <- x_gamma
for(i in c(2:4)){  # Adjust all columns except the third (Distance to seis)
  x_seis[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_seis <- inv.logit(intercept + slope_seis * x_seis[, 1])
lower_pred_seis <- inv.logit(intercept + lower_slope_seis * x_seis[, 1])
upper_pred_seis <- inv.logit(intercept + upper_slope_seis * x_seis[, 1])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToSeismic = x_seis[, 1],
  Gamma = mean_pred_seis,
  LowerCI = lower_pred_seis,
  UpperCI = upper_pred_seis
)

plot_data$DistanceToSeismic <- exp(plot_data$DistanceToSeismic)
# Plotting with ggplot2
plot_seismic <- ggplot(plot_data, aes(x = DistanceToSeismic, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "darkgreen", alpha = 0.08) +
  geom_line(color = "darkgreen", linewidth = 1) +
  xlab("Distance to Seismic Line (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  )
print(plot_seismic)
# Save the plot
ggsave("Results/GammaSeis.png", width = 8, height = 6)



# ROAD 
slope_road <- logedgeRE$mean$beta.gamma[2]
lower_slope_road <- logedgeRE$q2.5$beta.gamma[2]
upper_slope_road <- logedgeRE$q97.5$beta.gamma[2]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_road <- x_gamma
for(i in c(1, 3:4)){  # Adjust all columns except the third (Distance to Road)
  x_road[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_road <- inv.logit(intercept + slope_road * x_road[, 2])
lower_pred_road <- inv.logit(intercept + lower_slope_road * x_road[, 2])
upper_pred_road <- inv.logit(intercept + upper_slope_road * x_road[, 2])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToRoad = x_road[, 2],
  Gamma = mean_pred_road,
  LowerCI = lower_pred_road,
  UpperCI = upper_pred_road
)

plot_data$DistanceToRoad <- exp(plot_data$DistanceToRoad)
# Plotting with ggplot2
plot_road <- ggplot(plot_data, aes(x = DistanceToRoad, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "blue", alpha = 0.08) +
  geom_line(color = "blue", linewidth = 1) +
  xlab("Distance to Road (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  ) +
  scale_x_continuous(limits = c(44, 1000))  # Set x-axis limits

print(plot_road)
# Save the plot
ggsave("Results/GammaRoad.png", width = 8, height = 6)



# PIPE
slope_pipe <- logedgeRE$mean$beta.gamma[3]
lower_slope_pipe <- logedgeRE$q2.5$beta.gamma[3]
upper_slope_pipe <- logedgeRE$q97.5$beta.gamma[3]

# Assuming x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Road"
# Holding all other covariates constant at their mean values
x_pipe <- x_gamma
for(i in c(1:2, 4)){  # Adjust all columns except the third (Distance to Road)
  x_pipe[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_pipe <- inv.logit(intercept + slope_pipe * x_pipe[, 3])
lower_pred_pipe <- inv.logit(intercept + lower_slope_pipe * x_pipe[, 3])
upper_pred_pipe <- inv.logit(intercept + upper_slope_pipe * x_pipe[, 3])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToPipe = x_pipe[, 3],
  Gamma = mean_pred_pipe,
  LowerCI = lower_pred_pipe,
  UpperCI = upper_pred_pipe
)

plot_data$DistanceToPipe <- exp(plot_data$DistanceToPipe)
# Plotting with ggplot2
plot_pipeline <- ggplot(plot_data, aes(x = DistanceToPipe, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "black", alpha = 0.08) +
  geom_line(color = "black", linewidth = 1) +
  xlab("Distance to Pipeline (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  ) +
  scale_x_continuous(limits = c(14, 1000))  # Set x-axis limits
print(plot_pipeline)
# Save the plot
ggsave("Results/GammaPipe.png", width = 8, height = 6)

# HARVEST DISTANCE 
slope_harvest <- logedgeRE$mean$beta.gamma[4]
lower_slope_harvest <- logedgeRE$q2.5$beta.gamma[4]
upper_slope_harvest <- logedgeRE$q97.5$beta.gamma[4]

# x_phi is your covariate matrix
x_gamma <- win.data$x.gamma[, 2, ] # For year 2 only

# Adjust x_phi to isolate the effect of "Distance to Harvest"
# Holding all other covariates constant at their mean values
x_harvest <- x_gamma
for(i in c(1:3)){  # Adjust all columns except the fourth (Distance to Harvest)
  x_harvest[, i] <- mean(x_gamma[, i])
}

# Calculate new predictions and credible intervals
mean_pred_harvest <- inv.logit(intercept + slope_harvest * x_harvest[, 4])
lower_pred_harvest <- inv.logit(intercept + lower_slope_harvest * x_harvest[, 4])
upper_pred_harvest <- inv.logit(intercept + upper_slope_harvest * x_harvest[, 4])

# Data frame for plotting
plot_data <- data.frame(
  DistanceToHarvest = x_harvest[, 4],
  Gamma = mean_pred_harvest,
  LowerCI = lower_pred_harvest,
  UpperCI = upper_pred_harvest
)

plot_data$DistanceToHarvest <- exp(plot_data$DistanceToHarvest)

# Plotting with ggplot2
plot_harvest <- ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "red", alpha = 0.08) +
  geom_line(color = "red", linewidth = 1) +
  xlab("Distance to Harvest (m)") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 20), # Adjust font size as needed
    axis.title = element_text(size = 20), # Adjust font size as needed
    axis.text = element_text(size = 18) # Adjust font size as needed
  )
print(plot_harvest)

# Save the plot
ggsave("Results/GammaDistHarvest.png", width = 8, height = 6)


# put all together ina  panel 
# Combine the plots in a 2x2 grid
combined_plot <- plot_seismic + plot_road + 
  plot_pipeline + plot_harvest + 
  plot_layout(ncol = 2, nrow = 2)
print(combined_plot)
# Save the combined plot
ggsave("Results/Harvest model/Apr15CombinedGAMMAPlots_2x2.png", combined_plot, width = 16, height = 12)






# Percent Conifer 
intercept_psi <- out_edge$mean$beta.psi[1]
slope_percentconifer <- out_edge$mean$beta.psi[2]
lower_slope_percentconifer <-out_edge$q2.5$beta.psi[2]
upper_slope_percentconifer <-out_edge$q97.5$beta.psi[2]

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







