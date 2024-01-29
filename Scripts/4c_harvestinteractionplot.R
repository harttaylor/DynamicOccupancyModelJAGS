# HARVEST INTERACTION  

library(ggplot2)

# Example model coefficients from your output
intercept_with_harvest <- -2.251  #out_cov_harv2$mean$beta.gamma[1,1]
slope_distance_with_harvest <- 2.061  # beta.gamma[2,1]
slope_interaction_with_harvest <- 0.928  # beta.gamma[4,1]

intercept_no_harvest <- 5.062  # beta.gamma[1,2]
slope_distance_no_harvest <- 4.409  # beta.gamma[2,2]
slope_interaction_no_harvest <- -7.377  # beta.gamma[4,2]

# Define a range for harvest age and distance to harvest
distance_to_harvest <- seq(0, 1, length.out = 100)

# Function to calculate the inverse logit (probability)
inv.logit <- function(logit) {
  return(1 / (1 + exp(-logit)))
}

# Create a data frame for plotting
plot_data <- expand.grid(DistanceToHarvest = distance_to_harvest, Harvest = c("With", "Without"))
plot_data$Gamma <- with(plot_data, ifelse(Harvest == "With",
                                          inv.logit(intercept_with_harvest + slope_distance_with_harvest * DistanceToHarvest + slope_interaction_with_harvest * DistanceToHarvest),
                                          inv.logit(intercept_no_harvest + slope_distance_no_harvest * DistanceToHarvest + slope_interaction_no_harvest * DistanceToHarvest)))

# Plotting with ggplot2
ggplot(plot_data, aes(x = DistanceToHarvest, y = Gamma, color = Harvest)) +
  geom_line() +
  xlab("Distance to Harvest") +
  ylab("Estimated Probability of Colonization (Gamma)") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(color = "black")
  ) +
  scale_color_manual(values = c("blue", "red"))  # Adjust colors as needed

# Save the plot
ggsave("HarvestAgeDistanceInteractionPlot.png", width = 10, height = 6)




# try showing without harvets slope vs harvest slop with age at 3 different values 

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

