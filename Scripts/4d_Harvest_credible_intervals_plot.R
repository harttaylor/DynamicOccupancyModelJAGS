

# Define the data frame
data <- data.frame(
  Parameter = c("Alpha Phi (Harvest Present)", "Alpha Phi (No Harvest)", 
                "Delta Phi (Harvest Present)", "Delta Phi (No Harvest)",
                "Beta Phi (Distance, Harvest)", "Beta Phi (Distance, No Harvest)", 
                "Beta Phi (Interaction, Harvest)", "Beta Phi (Interaction, No Harvest)", 
                "Alpha Gamma (Harvest Present)", "Alpha Gamma (No Harvest)", 
                "Delta Gamma (Harvest Present)", "Delta Gamma (No Harvest)", 
                "Beta Gamma (Distance, Harvest)", "Beta Gamma (Distance, No Harvest)", 
                "Beta Gamma (Interaction, Harvest)", "Beta Gamma (Interaction, No Harvest)"),
  Mean = c(1.569, 0.242, 1.179, 0.000, 0.977, 0.639, 0.114, 1.696, 
           -2.090, 0.417, -4.156, -0.002, 1.743, 1.999, 1.736, -4.231),
  CI.Lower = c(0.875, -1.599, -3.740, -0.196, -1.141, -13.793, -4.913, -3.839, 
               -2.772, -1.373, -8.895, -0.195, -0.350, -12.407, -7.073, -13.049),
  CI.Upper = c(2.273, 2.089, 6.280, 0.195, 3.200, 15.210, 6.664, 5.931, 
               -1.446, 2.222, 0.584, 0.195, 3.822, 16.091, 10.780, 3.764)
)

# Manually specify the colors to be used for harvest present and absent
manual_colors <- c("blue", "darkgreen")

# Create the plot
plot <- ggplot(data, aes(x=Parameter, y=Mean)) + 
  geom_point(aes(color=ifelse(grepl("No Harvest", Parameter), manual_colors[2], manual_colors[1]))) +
  geom_errorbar(aes(ymin=CI.Lower, ymax=CI.Upper, 
                    color=ifelse(grepl("No Harvest", Parameter), manual_colors[2], manual_colors[1])), width=0.2) +
  coord_flip() + 
  theme_classic() +
  xlab("95% Credible Interval") + 
  ylab("Parameter") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_color_identity(guide='legend', 
                       breaks=manual_colors, 
                       labels=c("Harvest Present", "No Harvest")) +
  guides(color=guide_legend(title="Harvest Status"))

# Print the plot
print(plot)

# Load required libraries
library(ggplot2)
library(dplyr)

# Define the data frame
data <- data.frame(
  Parameter = c("Alpha Phi (Harvest Present)", "Alpha Phi (No Harvest)", 
                "Delta Phi (Harvest Present)", "Delta Phi (No Harvest)",
                "Beta Phi (Distance, Harvest)", "Beta Phi (Distance, No Harvest)", 
                "Beta Phi (Interaction, Harvest)", "Beta Phi (Interaction, No Harvest)", 
                "Alpha Gamma (Harvest Present)", "Alpha Gamma (No Harvest)", 
                "Delta Gamma (Harvest Present)", "Delta Gamma (No Harvest)", 
                "Beta Gamma (Distance, Harvest)", "Beta Gamma (Distance, No Harvest)", 
                "Beta Gamma (Interaction, Harvest)", "Beta Gamma (Interaction, No Harvest)"),
  Mean = c(1.569, 0.242, 1.179, 0.000, 0.977, 0.639, 0.114, 1.696, 
           -2.090, 0.417, -4.156, -0.002, 1.743, 1.999, 1.736, -4.231),
  CI.Lower = c(0.875, -1.599, -3.740, -0.196, -1.141, -13.793, -4.913, -3.839, 
               -2.772, -1.373, -8.895, -0.195, -0.350, -12.407, -7.073, -13.049),
  CI.Upper = c(2.273, 2.089, 6.280, 0.195, 3.200, 15.210, 6.664, 5.931, 
               -1.446, 2.222, 0.584, 0.195, 3.822, 16.091, 10.780, 3.764)
)

# Manually specify the colors to be used for harvest present and absent
manual_colors <- c("blue", "darkgreen")

# Filter data for phi and gamma parameters
phi_data <- filter(data, grepl("Phi", Parameter))
gamma_data <- filter(data, grepl("Gamma", Parameter))

# Function to create plot
create_plot <- function(data_to_plot) {
  ggplot(data_to_plot, aes(x=Parameter, y=Mean)) + 
    geom_point(aes(color=ifelse(grepl("No Harvest", Parameter), manual_colors[2], manual_colors[1]))) +
    geom_errorbar(aes(ymin=CI.Lower, ymax=CI.Upper, 
                      color=ifelse(grepl("No Harvest", Parameter), manual_colors[2], manual_colors[1])), width=0.2) +
    coord_flip() + 
    theme_classic() +
    xlab("95% Credible Interval") + 
    ylab("Parameter") +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    scale_color_identity(guide='legend', 
                         breaks=manual_colors, 
                         labels=c("Harvest Present", "No Harvest")) +
    guides(color=guide_legend(title="Harvest Status"))
}

# Create phi plot
phi_plot <- create_plot(phi_data)
print(phi_plot)

# Create gamma plot
gamma_plot <- create_plot(gamma_data)
print(gamma_plot)


# Define the data frame
data <- data.frame(
  Parameter = c("Intercept", "Intercept", 
                "Harvest Age*Distance to Harvest", "Harvest Age*Distance to Harvest",
                "Harvest Age", "Harvest Age", 
                "Distance to Harvest", "Distance to Harvest",
                "Intercept", "Intercept", 
                "Harvest Age*Distance to Harvest", "Harvest Age*Distance to Harvest",
                "Harvest Age", "Harvest Age", 
                "Distance to Harvest", "Distance to Harvest"),
  Mean = c(1.569, 0.242, 1.179, 0.000, 0.977, 0.639, 0.114, 1.696, 
           -2.090, 0.417, -4.156, -0.002, 1.743, 1.999, 1.736, -4.231),
  CI.Lower = c(0.875, -1.599, -3.740, -0.196, -1.141, -13.793, -4.913, -3.839, 
               -2.772, -1.373, -8.895, -0.195, -0.350, -12.407, -7.073, -13.049),
  CI.Upper = c(2.273, 2.089, 6.280, 0.195, 3.200, 15.210, 6.664, 5.931, 
               -1.446, 2.222, 0.584, 0.195, 3.822, 16.091, 10.780, 3.764),
  HarvestStatus = factor(c("Harvest Present", "No Harvest", "Harvest Present", "No Harvest", 
                           "Harvest Present", "No Harvest", "Harvest Present", "No Harvest", 
                           "Harvest Present", "No Harvest", "Harvest Present", "No Harvest", 
                           "Harvest Present", "No Harvest", "Harvest Present", "No Harvest")),
  Type = c("phi", "phi", "phi", "phi", "phi", "phi", "phi", "phi", 
           "gamma", "gamma", "gamma", "gamma", "gamma", "gamma", "gamma", "gamma")
)

# Manually specify the colors to be used for harvest present and absent
manual_colors <- c("Harvest Present"="blue", "No Harvest"="darkgreen")

# Dodge position for not overlapping bars
dodge <- position_dodge(width=0.3)

# Create the plot
plot <- ggplot(data, aes(x=Parameter, y=Mean, color=HarvestStatus)) + 
  geom_point(position=dodge, size=3) + # Change size for visibility
  geom_errorbar(aes(ymin=CI.Lower, ymax=CI.Upper), width=0.2, position=dodge) +
  coord_flip() + 
  theme_classic() +
  xlab("") + 
  ylab("95% Credible Interval") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_color_manual(values=manual_colors) +
  guides(color=guide_legend(title="Harvest Status")) +
  facet_wrap(~Type, scales = "free", labeller = as_labeller(c(phi = "Probability of Persistence", gamma = "Probability of Colonization"))) +
  theme(legend.position = c(0.5, 0.9), legend.justification = c(1, 1))

print(plot)
