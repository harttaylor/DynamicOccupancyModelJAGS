
# Set up working environment 
setwd("C:/Users/hartt/Documents/Chapter 1/BayesianAnalysis/DynamicOccupancyModelJAGS")
library(jagsUI)
library(ggplot2)

# Set up some required arrays
load("Data/dets_array.RData")
y <- dets_array
head(y)

# Add na.rm = TRUE to the inits function (otherwise most of the intial values will be NA)
inits <- function() { 
  list(z = apply(y, c(1, 2), max, na.rm = TRUE))
}

# Need to make a different survey count array, because of how your data is structured 
nsurv <- array(NA, dim = c(dim(y)[1], dim(y)[2], dim(y)[3]))
for(i in 1:dim(y)[1]){
  for(j in 1:dim(y)[2]){
    s <- which(!is.na(y[i, j, ]))
    nsurv[i, j, 1:length(s)] <- s
  }
}

# Get the actual number of surveys in each year at each point
J <- do.call(rbind, lapply(1:dim(y)[1], function(i){
  sapply(1:dim(y)[2], function(j){
    length(which(!is.na(y[i, j, ])))
  })
}))


win.data <- list(y = y, nsite = dim(y)[1], nyear = dim(y)[2], nsurv = nsurv, J = J)

# Parameters monitored
params <- c("psi1", "phi", "gamma", "n.occ", "growthr", "turnover")

# MCMC settings
ni <- 1000
nt <- 1
nb <- 500
nc <- 3

# Call JAGS from R and run the model 
system.time({
  out <- jags(data = win.data, inits = inits, parameters.to.save = params, 
              model.file = "cl_model_basic.txt", n.chains = nc, 
              n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
})

print(out)

# Make quick plots
df1 <- data.frame(Year = 1994:2017, Gamma = out$mean$gamma)
ggplot(dat = df1, aes(Year, y = Gamma)) + geom_line() + geom_smooth(method = lm) + theme_classic(base_size = 12)

df2 <- data.frame(Year = 1994:2017, Phi = out$mean$phi)
ggplot(data = df2, aes(x = Year, y = Phi)) + geom_line() + geom_smooth(method = lm) + theme_classic(base_size = 12)

df3 <- data.frame(Year = 1993: 2017, Psi = out$mean$psi1)
ggplot(data = df3, aes(x = Year, y = Psi)) + geom_line() + geom_smooth(method = lm) + theme_classic(base_size = 12)



# Make nice plots 
df1 <- data.frame(Year = 1994:2017, Gamma = out$mean$gamma)
df2 <- data.frame(Year = 1994:2017, Phi = out$mean$phi)

# Combine the two data frames for plotting
combined_df <- merge(df1, df2, by = "Year")


# Updated plot code with minor gridlines and increased line weight
map_plot <- ggplot() +
  geom_line(data = df1, aes(x = Year, y = Gamma, color = "Colonization"), size = 1.2) +
  geom_smooth(data = df1, aes(x = Year, y = Gamma, color = "Colonization"), method = "lm", fill = "#1B9E77", alpha = 0.2, size = 0.5) +
  geom_line(data = df2, aes(x = Year, y = Phi, color = "Persistence"), size = 1.2) +
  geom_smooth(data = df2, aes(x = Year, y = Phi, color = "Persistence"), method = "lm", fill = "#D95F02", alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("Colonization" = "#1B9E77", "Persistence" = "#D95F02")) +
  scale_x_continuous(name = "Year", breaks = seq(1994, 2017, by = 5)) +
  scale_y_continuous(name = "Probability", breaks = seq(0.1, 1, by = 0.1)) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(color = "")

print(map_plot)
# Save the plot at a high resolution
ggsave("Results/PhiGammaTrends.png", map_plot, dpi = 300, width = 8, height = 6)

# occupancy 
df3 <- data.frame(Year = 1993:2017, Psi = out$mean$psi1)

# Create the plot
occupancy_plot <- ggplot(data = df3, aes(x = Year, y = Psi)) +
  geom_line(color = "#377eb8", size = 1.2) +  # Choose a color that's clear and vision-impaired friendly
  geom_smooth(method = "lm", color = "#377eb8", fill = "#377eb8", alpha = 0.2, size = 0.5) +
  scale_x_continuous(name = "Year", breaks = seq(1993, 2017, by = 5)) +  # Adjust x-axis breaks if needed
  scale_y_continuous(name = "Occupancy Probability", breaks = seq(0, 1, by = 0.1)) +  # Adjust y-axis breaks if needed
  theme_classic(base_size = 12) 

print(occupancy_plot)
# Save the plot at a high resolution
ggsave("Results/occupancy_plot.png", occupancy_plot, dpi = 300, width = 8, height = 6)

