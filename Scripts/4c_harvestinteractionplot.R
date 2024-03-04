# HARVEST INTERACTION  
# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)
library(ggplot2)
palette.colors(palette = "Tableau 10")

harvest_interaction <- readRDS("Results/harvest_interactionRE24000.rds")
out_harv <- readRDS("Results/harvest_nointeraction.rds")

print(harvest_interaction)
print(out_harv)

# First make separate plots for with (1) and without harvest (2) showing relationship with harvest age and distance 
# Gamma 
# Model coefficients from output
intercept_with_harvest <- harvest_interaction$mean$alpha.gamma[2]
slope_age_with_harvest <- harvest_interaction$mean$delta.gamma[1]
slope_distance_with_harvest <- harvest_interaction$mean$beta.gamma[1,1]
slope_interaction_with_harvest <- harvest_interaction$mean$beta.gamma[1,2]

# Define a range for harvest distance
distance_harvest_seq <- seq(0, 1, length.out = 10000)


# Example harvest ages (transformed) for visualization
example_harvest_ages_transformed <- c(0.1, 0.3, 0.8)  # Transformed values
example_harvest_ages_original <- example_harvest_ages_transformed * 30  # Original age values

# Initialize an empty data frame for plotting
plot_data <- data.frame()

# Loop through each transformed harvest age and calculate predictions
for (i in seq_along(example_harvest_ages_transformed)) {
  harvest_age_transformed <- example_harvest_ages_transformed[i]
  harvest_age_original <- example_harvest_ages_original[i]
  
  # Calculate predicted values (probabilities) using the model coefficients and logistic function
  logit_gamma <- intercept_with_harvest +
    slope_age_with_harvest * harvest_age_transformed +
    slope_distance_with_harvest * distance_harvest_seq +
    slope_interaction_with_harvest * harvest_age_transformed * distance_harvest_seq
  predicted_gamma <- exp(logit_gamma) / (1 + exp(logit_gamma))
  
  # Combine data into a single data frame for plotting
  plot_data <- rbind(plot_data, data.frame(
    DistanceToHarvest = distance_harvest_seq,
    PredictedGamma = predicted_gamma,
    HarvestAgeLabel = as.factor(harvest_age_original)  # Use original age for labels
  ))
}

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759")
# Plot
ggplot(plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Colonization (Gamma)") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
   theme(axis.title = element_text(color = "black")) 



# Plotting WITHOUT harvest 
intercept_no_harvest <- harvest_interaction$mean$alpha.gamma[2]
slope_age_no_harvest <- harvest_interaction$mean$delta.gamma[2]
slope_distance_no_harvest <- harvest_interaction$mean$beta.gamma[2,1] 
slope_interaction_no_harvest <- harvest_interaction$mean$beta.gamma[2,2] 

# Calculate predicted values for the no-harvest scenario
 logit_gamma_no_harvest <- intercept_no_harvest +
   slope_distance_no_harvest * distance_harvest_seq +
  slope_interaction_no_harvest * 1 * distance_harvest_seq  # Assuming one age/no harvest scenario
predicted_gamma_no_harvest <- exp(logit_gamma_no_harvest) / (1 + exp(logit_gamma_no_harvest))


no_harvest_data <- data.frame(
  DistanceToHarvest = distance_harvest_seq,
  PredictedGamma = predicted_gamma_no_harvest,
  HarvestAgeLabel = factor(rep("No Harvest", length(distance_harvest_seq)))
)

# Combine with the existing plot data
combined_plot_data <- rbind(plot_data, no_harvest_data)

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759", "No Harvest" = "#59A14F")
# Plot with both harvest and no-harvest scenarios
ggplot(combined_plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Colonization (Gamma)") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette, breaks = c("3", "9", "24", "No Harvest")) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black"),
    legend.position = "right"
  )

# Save the plot
ggsave("Results/Feb21GammaHarvestInteractionPlot.png", width = 10, height = 6)



# Phi
# Model coefficients from output
intercept_with_harvest <- harvest_interaction$mean$alpha.phi[2]
slope_age_with_harvest <- harvest_interaction$mean$delta.phi[1]
slope_distance_with_harvest <- harvest_interaction$mean$beta.phi[1,1]
slope_interaction_with_harvest <- harvest_interaction$mean$beta.phi[1,2]

# Define a range for harvest distance
distance_harvest_seq <- seq(0, 1, length.out = 10000)


# Example harvest ages (transformed) for visualization
example_harvest_ages_transformed <- c(0.1, 0.3, 0.8)  # Transformed values
example_harvest_ages_original <- example_harvest_ages_transformed * 30  # Original age values

# Initialize an empty data frame for plotting
plot_data <- data.frame()

# Loop through each transformed harvest age and calculate predictions
for (i in seq_along(example_harvest_ages_transformed)) {
  harvest_age_transformed <- example_harvest_ages_transformed[i]
  harvest_age_original <- example_harvest_ages_original[i]
  
  # Calculate predicted values (probabilities) using the model coefficients and logistic function
  logit_gamma <- intercept_with_harvest +
    slope_age_with_harvest * harvest_age_transformed +
    slope_distance_with_harvest * distance_harvest_seq +
    slope_interaction_with_harvest * harvest_age_transformed * distance_harvest_seq
  predicted_gamma <- exp(logit_gamma) / (1 + exp(logit_gamma))
  
  # Combine data into a single data frame for plotting
  plot_data <- rbind(plot_data, data.frame(
    DistanceToHarvest = distance_harvest_seq,
    PredictedGamma = predicted_gamma,
    HarvestAgeLabel = as.factor(harvest_age_original)  # Use original age for labels
  ))
}

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759")
# Plot
ggplot(plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Persistence") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
  theme(axis.title = element_text(color = "black")) 



# Plotting WITHOUT harvest 
intercept_no_harvest <- harvest_interaction$mean$alpha.phi[2]
slope_age_no_harvest <- harvest_interaction$mean$delta.phi[2]
slope_distance_no_harvest <- harvest_interaction$mean$beta.phi[2,1] 
slope_interaction_no_harvest <- harvest_interaction$mean$beta.phi[2,2] 

# Calculate predicted values for the no-harvest scenario
logit_gamma_no_harvest <- intercept_no_harvest +
  slope_distance_no_harvest * distance_harvest_seq +
  slope_interaction_no_harvest * 1 * distance_harvest_seq  # Assuming one age/no harvest scenario
predicted_gamma_no_harvest <- exp(logit_gamma_no_harvest) / (1 + exp(logit_gamma_no_harvest))

# Add the no-harvest scenario to the plot_data
no_harvest_data <- data.frame(
  DistanceToHarvest = distance_harvest_seq,
  PredictedGamma = predicted_gamma_no_harvest,
  HarvestAgeLabel = factor(rep("No Harvest", length(distance_harvest_seq)))
)

# Combine with the existing plot data
combined_plot_data <- rbind(plot_data, no_harvest_data)

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759", "No Harvest" = "#59A14F")
# Plot with both harvest and no-harvest scenarios
ggplot(combined_plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Persistence") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette, breaks = c("3", "9", "24", "No Harvest")) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black"),
    legend.position = "right"
  )

# Save the plot
ggsave("Results/Feb21PhiHarvestInteractionPlot.png", width = 10, height = 6)

# Coefficients from your model output for 'with harvest' scenario
intercept_with_harvest <- -2.251  # Placeholder, use out_cov_harv2$beta.gamma[1,1] from your model output
slope_distance_with_harvest <- 2.061  # beta.gamma[2,1]
slope_interaction_with_harvest <- 0.928  # beta.gamma[4,1]

# Coefficients from your model output for 'no harvest' scenario
intercept_no_harvest <- 5.062  # beta.gamma[1,2]
slope_distance_no_harvest <- 4.409  # beta.gamma[2,2]

# Define a range for distance to harvest
distance_to_harvest <- seq(0, 1, length.out = 100)

# Define the age categories for 'with harvest' scenario
age_categories <- c("0-5 years", "5-15 years", "15-20 years")
age_values <- c(2.5, 10, 17.5)  # Midpoints of the age categories

# Function to calculate the inverse logit (probability)
inv.logit <- function(logit) {
  return(1 / (1 + exp(-logit)))
}

# Create a data frame for plotting for 'with harvest'
plot_data_with_harvest <- expand.grid(Age = age_values, DistanceToHarvest = distance_to_harvest, Harvest = "With")
plot_data_with_harvest$Gamma <- with(plot_data_with_harvest, inv.logit(intercept_with_harvest + slope_distance_with_harvest * DistanceToHarvest + slope_interaction_with_harvest * Age * DistanceToHarvest))

# Create a data frame for plotting for 'no harvest'
plot_data_no_harvest <- expand.grid(Age = NA, DistanceToHarvest = distance_to_harvest, Harvest = "Without")
plot_data_no_harvest$Gamma <- with(plot_data_no_harvest, inv.logit(intercept_no_harvest + slope_distance_no_harvest * DistanceToHarvest))

# Ensure 'Age' column is a factor with correct levels
plot_data_with_harvest$Age <- factor(plot_data_with_harvest$Age, levels = c(2.5, 10, 17.5))
plot_data_no_harvest$Age <- factor(NA, levels = c(2.5, 10, 17.5))  # Set to NA for no harvest
plot_data_no_harvest$Harvest <- 'Without'

# Combine the two datasets
plot_data <- rbind(plot_data_with_harvest, plot_data_no_harvest)

# Set correct labels for 'Age' column
plot_data$AgeLabel <- ifelse(is.na(plot_data$Age), "Without", 
                             ifelse(plot_data$Age == 2.5, "0-5 years",
                                    ifelse(plot_data$Age == 10, "5-15 years", "15-20 years")))

# Plotting with ggplot2
gg <- ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma, color = AgeLabel)) +
  geom_line() +
  scale_color_manual(values = c("Without" = "darkgreen", "0-5 years" = "red", "5-15 years" = "orange", "15-20 years" = "yellow")) +
  xlab("Distance to Harvest") +
  ylab("Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  guides(color = guide_legend(title = "Scenario"))

# Print the plot
print(gg)

# Save the plot
ggsave("HarvestAgeDistanceInteractionPlot.png", plot = gg, width = 8, height = 5, dpi = 300)




# Make plot to show harvest age effects for only sites with harvest (unharvested sites removed) 

# Phi
# Model coefficients from output
harvest_interaction <- readRDS("Results/HARVOUT_sites_w_out_harvest_removed.rds")

intercept_with_harvest <- harvest_interaction$mean$beta.phi[1]
slope_age_with_harvest <- harvest_interaction$mean$beta.phi[3]
slope_distance_with_harvest <- harvest_interaction$mean$beta.phi[2]
slope_interaction_with_harvest <- harvest_interaction$mean$beta.phi[4]

# Define a range for harvest distance
distance_harvest_seq <- seq(0, 1, length.out = 10000)


# Example harvest ages (transformed) for visualization
example_harvest_ages_transformed <- c(0.1, 0.3, 0.8)  # Transformed values
example_harvest_ages_original <- example_harvest_ages_transformed * 30  # Original age values

# Initialize an empty data frame for plotting
plot_data <- data.frame()

# Loop through each transformed harvest age and calculate predictions
for (i in seq_along(example_harvest_ages_transformed)) {
  harvest_age_transformed <- example_harvest_ages_transformed[i]
  harvest_age_original <- example_harvest_ages_original[i]
  
  # Calculate predicted values (probabilities) using the model coefficients and logistic function
  logit_gamma <- intercept_with_harvest +
    slope_age_with_harvest * harvest_age_transformed +
    slope_distance_with_harvest * distance_harvest_seq +
    slope_interaction_with_harvest * harvest_age_transformed * distance_harvest_seq
  predicted_gamma <- exp(logit_gamma) / (1 + exp(logit_gamma))
  
  # Combine data into a single data frame for plotting
  plot_data <- rbind(plot_data, data.frame(
    DistanceToHarvest = distance_harvest_seq,
    PredictedGamma = predicted_gamma,
    HarvestAgeLabel = as.factor(harvest_age_original)  # Use original age for labels
  ))
}

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759")
# Plot
ggplot(plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Persistence") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
  theme(axis.title = element_text(color = "black")) 



# Gamma 
# Model coefficients from output
intercept_with_harvest <- harvest_interaction$mean$beta.gamma[1]
slope_age_with_harvest <- harvest_interaction$mean$beta.gamma[3]
slope_distance_with_harvest <- harvest_interaction$mean$beta.gamma[2]
slope_interaction_with_harvest <- harvest_interaction$mean$beta.gamma[4]

# Define a range for harvest distance
distance_harvest_seq <- seq(0, 1, length.out = 10000)


# Example harvest ages (transformed) for visualization
example_harvest_ages_transformed <- c(0.1, 0.3, 0.8)  # Transformed values
example_harvest_ages_original <- example_harvest_ages_transformed * 30  # Original age values

# Initialize an empty data frame for plotting
plot_data <- data.frame()

# Loop through each transformed harvest age and calculate predictions
for (i in seq_along(example_harvest_ages_transformed)) {
  harvest_age_transformed <- example_harvest_ages_transformed[i]
  harvest_age_original <- example_harvest_ages_original[i]
  
  # Calculate predicted values (probabilities) using the model coefficients and logistic function
  logit_gamma <- intercept_with_harvest +
    slope_age_with_harvest * harvest_age_transformed +
    slope_distance_with_harvest * distance_harvest_seq +
    slope_interaction_with_harvest * harvest_age_transformed * distance_harvest_seq
  predicted_gamma <- exp(logit_gamma) / (1 + exp(logit_gamma))
  
  # Combine data into a single data frame for plotting
  plot_data <- rbind(plot_data, data.frame(
    DistanceToHarvest = distance_harvest_seq,
    PredictedGamma = predicted_gamma,
    HarvestAgeLabel = as.factor(harvest_age_original)  # Use original age for labels
  ))
}

color_palette <- c("3" = "#EDC948", "9" = "#F28E2B", "24" = "#E15759")
# Plot
ggplot(plot_data, aes(x = DistanceToHarvest, y = PredictedGamma, color = HarvestAgeLabel)) +
  geom_line() +
  scale_y_continuous("Probability of Colonization (Gamma)") +
  scale_x_continuous("Distance to Harvest") +
  scale_color_manual(values = color_palette) +
  labs(color = "Harvest Age (Years)") +
  theme_classic() +
  theme(axis.title = element_text(color = "black")) 









library(ggplot2)

# Coefficients from your model output for 'with harvest' scenario
intercept_with_harvest <- -2.251
slope_distance_with_harvest <- 2.061
slope_interaction_with_harvest <- 0.928

# Coefficients from your model output for 'no harvest' scenario
intercept_no_harvest <- 5.062
slope_distance_no_harvest <- 4.409

# Define a range for distance to harvest
distance_to_harvest <- seq(0, 1, length.out = 100)

# Define the age values for 'with harvest' scenario
age_values <- c(2.5, 10, 17.5)  # Midpoints of the age categories

# Function to calculate the inverse logit (probability)
inv.logit <- function(logit) {
  return(1 / (1 + exp(-logit)))
}

# Create a data frame for plotting for 'with harvest'
plot_data_with_harvest <- expand.grid(Age = age_values, DistanceToHarvest = distance_to_harvest)
plot_data_with_harvest$Gamma <- with(plot_data_with_harvest, inv.logit(intercept_with_harvest + slope_distance_with_harvest * DistanceToHarvest + slope_interaction_with_harvest * Age * DistanceToHarvest))

# Add a Scenario column to identify the age group
plot_data_with_harvest$Scenario <- rep(c("0-5 years", "5-15 years", "15-20 years"), each = length(distance_to_harvest))

# Create a data frame for plotting for 'no harvest'
plot_data_no_harvest <- data.frame(DistanceToHarvest = distance_to_harvest, Gamma = inv.logit(intercept_no_harvest + slope_distance_no_harvest * distance_to_harvest), Scenario = "Without")

# Combine the two datasets
plot_data <- rbind(plot_data_with_harvest, plot_data_no_harvest)

# Plotting with ggplot2
gg <- ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma, color = Scenario, group = Scenario)) +
  geom_line() +
  scale_color_manual(values = c("0-5 years" = "red", "5-15 years" = "orange", "15-20 years" = "yellow", "Without" = "darkgreen")) +
  xlab("Distance to Harvest") +
  ylab("Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  guides(color = guide_legend(title = "Scenario"))

# Print the plot
print(gg)

# Save the plot
ggsave("HarvestAgeDistanceInteractionPlot.png", plot = gg, width = 8, height = 5, dpi = 300)

