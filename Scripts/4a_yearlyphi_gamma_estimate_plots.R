# Make plots to show results of JAGS model 
# Plot variability in persistence and colonization together with changes in the number of sites occupied 

setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS/")

# Load necessary library
library(ggplot2)
library(gridExtra)
library(tidybayes)

# Load data 
out_cov <- readRDS("Results/EDGE_resultsFeb3.rds")


# Plot phi yearly 
# Extract the phi matrix from your model output
phi_matrix <- out_cov_edge$mean$phi[, 1:24]  # Adjust the indices if necessary
upper_phi <- out_cov_edge$q97.5$phi[, 1:24]
lower_phi <- out_cov_edge$q2.5$phi[, 1:24]

# Calculate the mean phi for each year across all sites
mean_phi_per_year <- apply(phi_matrix, 2, mean)

# Years
years <- 1:24  # Adjust if your study years are labeled differently

ggplot(data = data.frame(years, mean_phi_per_year), aes(x = years, y = mean_phi_per_year)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean Persistence Probability (Phi)") +
 # ggtitle("Mean Persistence Probability Over 24 Years") +
  theme_classic()


ggsave("Results/YearlyPhi.png")


# Now do yearly estimates of gamma 
# Extract the gamma matrix from your model output
gamma_matrix <- out_cov_edge$mean$gamma[, 1:24]
gamma_lower <- out_cov_edge$q2.5$gamma[, 1:24]
gamma_upper <- out_cov_edge$q97.5$gamma[, 1:24]

# Calculate the mean gamma for each year across all sites
mean_gamma_per_year <- apply(gamma_matrix, 2, mean)

# Years
years <- 1:24 

ggplot(data = data.frame(years, mean_gamma_per_year), aes(x = years, y = mean_gamma_per_year)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean Yearly Colonization Probability (Gamma)") +
  #ggtitle("Mean Yearly Colonization Probability Over 24 Years") +
  theme_classic() 

ggsave("Results/YearlyGamma.png")


# Plot phi and gamma on the same graph
# Extract the phi matrix from your model output
phi_matrix <- out_cov_edge$mean$phi[, 1:24]
phi_lower <- out_cov_edge$q2.5$phi[, 1:24]
phi_upper <- out_cov_edge$q97.5$phi[, 1:24]
mean_phi <- apply(phi_matrix, 2, mean)

# Extract the gamma matrix from your model output
gamma_matrix <- out_cov_edge$mean$gamma[, 1:24]
gamma_lower <- out_cov_edge$q2.5$gamma[, 1:24]
gamma_upper <- out_cov_edge$q97.5$gamma[, 1:24]
mean_gamma <- apply(gamma_matrix, 2, mean)

# Calculate yearly averages for the lower and upper CI bounds
mean_phi_lower <- apply(phi_lower, 2, mean)
mean_phi_upper <- apply(phi_upper, 2, mean)
mean_gamma_lower <- apply(gamma_lower, 2, mean)
mean_gamma_upper <- apply(gamma_upper, 2, mean)

# Years vector covering 1 to 25
years <- 1:24
actual_years <- 1994 + (years - 1)

# Create a combined data frame for plotting
data_df <- data.frame(
  years = actual_years,
  mean_phi, mean_phi_lower, mean_phi_upper,
  mean_gamma, mean_gamma_lower, mean_gamma_upper
)

# Adjust the lengths of the mean CI vectors
# only need to do this if you are plotting occu and col/persist on same graph to standardize the years 
#mean_phi_lower <- c(NA, mean_phi_lower)  # Prepend NA
#mean_phi_upper <- c(NA, mean_phi_upper)  # Prepend NA
#mean_gamma_lower <- c(NA, mean_gamma_lower)  # Prepend NA
#mean_gamma_upper <- c(NA, mean_gamma_upper)  # Prepend NA
#mean_phi <- c(NA, mean_phi_per_year)
#mean_gamma <- c(NA, mean_gamma_per_year)



ggplot(data_df, aes(x = years)) +
  # Colonization Probability (phi) with ribbon
  geom_ribbon(aes(ymin = mean_phi_lower, ymax = mean_phi_upper, fill = "Persistence"), alpha = 0.2) +
  geom_line(aes(y = mean_phi, color = "Persistence")) +
  
  # Extinction Probability (gamma) with ribbon
  geom_ribbon(aes(ymin = mean_gamma_lower, ymax = mean_gamma_upper, fill = "Colonization"), alpha = 0.2) +
  geom_line(aes(y = mean_gamma, color = "Colonization")) +
  
  # Labels and theme
  labs(x = "Year", y = "Probability",
       fill = "Probability Type", color = "Probability Type") +
  scale_fill_manual(values = c("Persistence" = "purple", "Colonization" = "blue")) +
  scale_color_manual(values = c("Persistence" = "purple", "Colonization" = "blue")) +
  scale_x_continuous(breaks = seq(1994, 2018, 4), labels = seq(1994, 2018, 4)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1)) + 
  theme_classic() +
  theme(
    legend.title = element_blank(), # Removes the legend title
    legend.position = c(0.9, 0.6),
    text = element_text(size = 14), # Increases the font size for all text elements
    axis.text.y = element_text(size = 12) # Customizes y-axis text
  )

ggsave("Results/colonization_extinction_trends.png", width = 12, height = 6)


# Create a plot for occupancy with the y-axis scaled from 0 to 1
# Assuming you have extracted the z matrix similarly
z_matrix <- out_cov_edge$mean$z[, 1:25]
z_lower <- out_cov_edge$q2.5$z[, 1:25]
z_upper <- out_cov_edge$q97.5$z[, 1:25]
mean_z_per_year <- apply(z_matrix, 2, mean)
mean_z_lower <- apply(z_lower, 2, mean)
mean_z_upper <- apply(z_upper, 2, mean)

# Years vector covering 1 to 25
years <- 1:25
actual_years <- 1993 + (years - 1)

# Create a data frame for plotting
data_df <- data.frame(years = actual_years, mean_z_per_year, mean_z_lower, mean_z_upper)

occupancy_plot <- ggplot(data = data_df, aes(x = years)) +
  geom_ribbon(aes(ymin = mean_z_lower, ymax = mean_z_upper), fill = "orange", alpha = 0.2) +
  geom_line(aes(y = mean_z_per_year), color = "dark orange", size = 1) +
  scale_x_continuous(breaks = seq(1993, 2018, 5), labels = seq(1993, 2018, 5)) +
  #expand_limits(y = 0:1) +
  labs(x = "Year", y = "Mean Occupancy Probability") +
  theme_classic() +
  theme(
    text = element_text(size = 14), # Increases the font size for all text elements
    axis.text.y = element_text(size = 12) # Customizes y-axis text
  )
print(occupancy_plot)
ggsave("Results/occupancy.png")


# Make a plot for the number of sites occupied each year 
mean_N_per_year <- out_cov_edge$mean$N
N_lower <- out_cov_edge$q2.5$N
N_upper <- out_cov_edge$q97.5$N
years <- 1:25
# Map 'years' to actual years (1993 to 2018)
actual_years <- 1993 + (years - 1)

# Create a data frame for plotting
data_df <- data.frame(years = actual_years, mean_N_per_year, N_lower, N_upper)

occupancy_plot <- ggplot(data = data_df, aes(x = years)) +
  geom_ribbon(aes(ymin = N_lower, ymax = N_upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(y = mean_N_per_year), color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(1993, 2018, 5), labels = seq(1993, 2018, 5)) +
  labs(x = "Year", y = "Number of Occupied Sites") +
  theme_classic() +
  theme(
    text = element_text(size = 14), # Increases the font size for all text elements
    axis.text.y = element_text(size = 12) # Customizes y-axis text
  )

print(occupancy_plot)
ggsave("Results/NumberofOccupiedSites.png")


# Plotting compelling human footprint relationships 
# Look for coefficients where the 95% credible interval (2.5% to 97.5% range) does not overlap with zero. 
# This suggests that there's a high probability that the true effect is either positive or negative, rather than zero.
# In output, beta coefficients like beta.phi[2], beta.phi[3], beta.phi[4], beta.gamma[2], and beta.gamma[7] show intervals not overlapping zero, 
# indicating potential significance.

# Larger effect sizes (the mean of the coefficient) can be more compelling, especially if they have relatively smaller standard deviations, 
# indicating precision in the estimate. For example, beta.psi[4] has a large mean (4.565) and a relatively smaller standard deviation compared to its magnitude.
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
