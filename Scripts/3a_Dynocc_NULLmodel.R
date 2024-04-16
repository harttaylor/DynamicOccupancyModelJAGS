
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
params <- c("psi1", "phi", "gamma", "n.occ", "growthr", "turnover", "z")

# MCMC settings
ni <- 1000
nt <- 1
nb <- 500
nc <- 3

# Call JAGS from R and run the model 
system.time({
  out <- jags(data = win.data, inits = inits, parameters.to.save = params, 
              model.file = "cl_model_NULL.txt", n.chains = nc, 
              n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
})

mean(out$mean$turnover) #0.1372772
print(min(out$mean$turnover)) #0.03769864
print(max(out$mean$turnover)) #0.2496167

mean_z <- as.data.frame(out$mean$z)
write.csv(mean_z, "Data/MeanOccupancyProbability.csv")

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
  theme_classic(base_size = 16) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) +
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
  theme_classic(base_size = 22) +
  theme(text = element_text(family = "Times New Roman"))

print(occupancy_plot)
# Save the plot at a high resolution
ggsave("Results/occupancy_plot.png", occupancy_plot, dpi = 300, width = 8, height = 6)






# Plot turnover rates 
# Assuming 'out_tau_edge$mean$tau' is your model output
# Extract the mean turnover rates for each year
print(summary(out_tau_edge$mean$tau))
mean(out_tau_edge$mean$tau[,1])

mean_turnover <- c(mean(out_edge$mean$tau[,1]), 
                   mean(out_edge$mean$tau[,2]), 
                   mean(out_edge$mean$tau[,3]),
                   mean(out_edge$mean$tau[,4]), 
                   mean(out_edge$mean$tau[,5]), 
                   mean(out_edge$mean$tau[,6]), 
                   mean(out_edge$mean$tau[,7]), 
                   mean(out_edge$mean$tau[,8]), 
                   mean(out_edge$mean$tau[,9]),
                   mean(out_edge$mean$tau[,10]), 
                   mean(out_edge$mean$tau[,11]), 
                   mean(out_edge$mean$tau[,12]),
                   mean(out_edge$mean$tau[,13]), 
                   mean(out_edge$mean$tau[,14]), 
                   mean(out_edge$mean$tau[,15]),
                   mean(out_edge$mean$tau[,16]), 
                   mean(out_edge$mean$tau[,17]), 
                   mean(out_edge$mean$tau[,18]), 
                   mean(out_edge$mean$tau[,19]), 
                   mean(out_edge$mean$tau[,20]), 
                   mean(out_edge$mean$tau[,21]),
                   mean(out_edge$mean$tau[,22]), 
                   mean(out_edge$mean$tau[,23]), 
                   mean(out_edge$mean$tau[,24]))

# Create a data frame for plotting
years <- 1:24  # Adjust if your study years are named differently
turnover_data <- data.frame(Year = years, MeanTurnover = mean_turnover)

# Plot
ggplot(turnover_data, aes(x = Year, y = MeanTurnover)) +
  geom_line(color = "#377eb8", size = 1) +  # Choose a color that's clear and vision-impaired friendly
  geom_smooth(method = "lm", color = "#377eb8", fill = "#377eb8", alpha = 0.2, size = 0.5) +
  geom_point(color = "#377eb8", size = 2) +  # Optional: adds points to each year's mean turnover rate
  labs(title = "Changes in Turnover Over 25 Years",
       x = "Year",
       y = "Mean Turnover") +
  theme_classic()

ggsave("Results/YearlyTurnover.png")


# Convert the matrix to a long format for use with ggplot
turnover_data <- reshape2::melt(out_edge$mean$tau)

# Name the columns appropriately
names(turnover_data) <- c("Site", "Year", "TurnoverRate")
write.csv(turnover_data, "Data/turnover_data.csv")
# Plotting the heatmap
ggplot(turnover_data, aes(x = Year, y = Site, fill = TurnoverRate)) +
  geom_tile() +  # Use geom_tile for heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = median(turnover_data$TurnoverRate, na.rm = TRUE), 
                       name = "Turnover Rate") +
  labs(title = "Heatmap of Turnover Rate Across Sites and Years",
       x = "Year",
       y = "Site") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if needed
ggsave("Results/Heatmap_Turnover.png")
# In a population with high turnover, species are frequently going locally extinct and are being replaced by other species colonizing the area. 
# This could be due to environmental variability, habitat fragmentation, or other disturbances.
# A high turnover might suggest instability in the local population. This could be driven by environmental factors (like resource availability or habitat quality), 
# social factors (such as competition or predation), or anthropogenic factors (like urban development or environmental pollution).
# Reflection of Habitat Quality: In wildlife populations, high turnover might reflect changes or fluctuations in habitat quality. 
# Good-quality habitats tend to have lower turnover rates as individuals are more likely to stay and reproduce, whereas in poorer quality habitats, 
# individuals might leave in search of better conditions, or there might be higher mortality rates.



# Create heatmap on actual study area map 
# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(ggmap)
library(grid)

# Read your data (adjust the file paths as necessary)
coords_data <- read.csv("Data/latslons.csv")
turnover_data <- read.csv("Data/turnover_data.csv")
occupancy_data <- read.csv("Data/MeanOccupancyProbability.csv")


# Convert the occupancy probabilities to a binary occupied/unoccupied using the 0.5 cutoff
occupancy_binary <- occupancy_data %>%
  mutate(across(-1, ~ as.integer(. > 0.5)))  # Convert probabilities to 0 or 1 based on cutoff

# Calculate turnover as the number of changes in occupancy status between consecutive years
occupancy_turnover <- occupancy_binary %>%
  rowwise() %>%
  mutate(Turnover = sum(abs(diff(c_across(-1)))))

# count the number of years occupied for each site
occupancy_turnover$YearsOccupied <- rowSums(occupancy_binary[,-1])

# Determine the sites that are always occupied, never occupied, or have high turnover
occupancy_turnover <- occupancy_turnover %>%
  mutate(StableOccupied = case_when(
    Turnover > 3 ~ "High Turnover",
    Turnover <= 3 & YearsOccupied <= 5 ~ "Low Occupancy",
    Turnover <= 3 & YearsOccupied >= 17 ~ "High Occupancy",
    TRUE ~ "High Turnover"  # For other cases, you can label them as "Variable" or choose another label
  ))

# Normalize the turnover data to be between 0 and 1
max_turnover <- max(occupancy_turnover$Turnover)
occupancy_turnover <- occupancy_turnover %>%
  mutate(NormalizedTurnover = Turnover / 25)

#write.csv(coordinates_turnover, "Data/Occu_Turnover.csv")
#coordinates_turnover <- read.csv("Data/Occu_Turnover.csv")
max(occupancy_turnover$NormalizedTurnover)
min(occupancy_turnover$NormalizedTurnover)
mean(occupancy_turnover$NormalizedTurnover)
# Now merge this turnover data with your spatial coordinates data
coordinates <- st_as_sf(coords_data, coords = c('X', 'Y'), crs = 4326)
coordinates_turnover <- coordinates %>%
  left_join(occupancy_turnover, by = "Site")


seismic_lines_clipped <- readRDS("Human footprint extraction/seismic_lines_clipped.rds")
pipelines_clipped <- readRDS("Human footprint extraction/pipelines_clipped.rds")
roads_clipped <- readRDS("Human footprint extraction/roads_clipped.rds")
harvest_clipped <- readRDS("Human footprint extraction/harvest_clipped.rds")

# Calculate the bounding box
xmin <- min(coords_data$X)
xmax <- max(coords_data$X)
ymin <- min(coords_data$Y)
ymax <- max(coords_data$Y)

# Manually expand the bounding box
expand_factor <- 0.2  # Adjust this factor as needed
xrange <- xmax - xmin
yrange <- ymax - ymin
expanded_bbox <- c(xmin - xrange * expand_factor, ymin - yrange * expand_factor, 
                   xmax + xrange * expand_factor, ymax + yrange * expand_factor)

# Convert expanded_bbox to a named vector
bbox_named <- setNames(expanded_bbox, c("left", "bottom", "right", "top"))

# Transform all spatial data to WGS 84
crs_wgs84 <- st_crs(4326)
coordinates <- st_transform(coordinates, crs_wgs84)
seismic_lines_clipped <- st_transform(seismic_lines_clipped, crs_wgs84)
pipelines_clipped <- st_transform(pipelines_clipped, crs_wgs84)
roads_clipped <- st_transform(roads_clipped, crs_wgs84)
harvest_clipped <- st_transform(harvest_clipped, crs_wgs84)

# Fetch the terrain map image
terrain_map <- get_map(location = bbox_named, source = "google", maptype = "terrain", crop = FALSE)
terrain_raster <- rasterGrob(as.raster(terrain_map), interpolate = TRUE)

# Plot with the terrain map as the background
map_plot <- ggplot() +
  annotation_custom(terrain_raster, xmin = bbox_named["left"], xmax = bbox_named["right"], ymin = bbox_named["bottom"], ymax = bbox_named["top"]) +
  geom_sf(data = seismic_lines_clipped, color = "gray", size = 0.1) +
  geom_sf(data = harvest_clipped, color = "grey", fill = "white", size = 1) +
  geom_sf(data = pipelines_clipped, color = "gray", size = 1) +
  geom_sf(data = roads_clipped, color = "gray", size = 1) +
  geom_sf(data = coordinates_turnover, aes(color = NormalizedTurnover, shape = StableOccupied), size = 2.7) +
  scale_color_gradient(low = "blue", high = "red", name = "Turnover Rate") +
  scale_shape_manual(values = c("High Occupancy" = 16, "Low Occupancy" = 17, "High Turnover" = 15), 
                     name = "Occupancy Status") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = TRUE) +
  labs(title = '',
       x = 'Longitude', 
       y = 'Latitude') +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        text = element_text(size = 16))
print(map_plot)
ggsave("Results/Feb22HeatmapTurnover.png", dpi = 300)






# Calculate just turnover 
# Calculate the average turnover for each site
average_turnover <- turnover_data %>%
  group_by(Site) %>%
  summarize(AverageTurnover = mean(TurnoverRate, na.rm = TRUE))

# Merge the average turnover data with the coordinates data
merged_data <- merge(coords_data, average_turnover, by = "Site")

# Save the merged data to a new CSV file
# write.csv(merged_data, "Data/average_site_turnover.csv", row.names = FALSE)

# Convert to a simple features (sf) object
coordinates <- st_as_sf(merged_data, coords = c('X', 'Y'), crs = 4326)

# Load points data with treatment column
points_data <- read.csv("Data/latslons.csv")


seismic_lines_clipped <- readRDS("Human footprint extraction/seismic_lines_clipped.rds")
pipelines_clipped <- readRDS("Human footprint extraction/pipelines_clipped.rds")
roads_clipped <- readRDS("Human footprint extraction/roads_clipped.rds")
harvest_clipped <- readRDS("Human footprint extraction/harvest_clipped.rds")

# Calculate the bounding box
xmin <- min(points_data$X)
xmax <- max(points_data$X)
ymin <- min(points_data$Y)
ymax <- max(points_data$Y)

# Manually expand the bounding box
expand_factor <- 0.2  # Adjust this factor as needed
xrange <- xmax - xmin
yrange <- ymax - ymin
expanded_bbox <- c(xmin - xrange * expand_factor, ymin - yrange * expand_factor, 
                   xmax + xrange * expand_factor, ymax + yrange * expand_factor)

# Convert expanded_bbox to a named vector
bbox_named <- setNames(expanded_bbox, c("left", "bottom", "right", "top"))

# Transform all spatial data to WGS 84
crs_wgs84 <- st_crs(4326)
coordinates <- st_transform(coordinates, crs_wgs84)
seismic_lines_clipped <- st_transform(seismic_lines_clipped, crs_wgs84)
pipelines_clipped <- st_transform(pipelines_clipped, crs_wgs84)
roads_clipped <- st_transform(roads_clipped, crs_wgs84)
harvest_clipped <- st_transform(harvest_clipped, crs_wgs84)

# Fetch the terrain map image
terrain_map <- get_map(location = bbox_named, source = "google", maptype = "terrain", crop = FALSE)
terrain_raster <- rasterGrob(as.raster(terrain_map), interpolate = TRUE)

# Plot with the terrain map as the background
map_plot <- ggplot() +
  annotation_custom(terrain_raster, xmin = bbox_named["left"], xmax = bbox_named["right"], ymin = bbox_named["bottom"], ymax = bbox_named["top"]) +
  geom_sf(data = seismic_lines_clipped, color = "gray", size = 0.1) +
  geom_sf(data = harvest_clipped, color = "grey", fill = "white", size = 1) +
  geom_sf(data = pipelines_clipped, color = "gray", size = 1) +
  geom_sf(data = roads_clipped, color = "gray", size = 1) +
  geom_sf(data = coordinates, aes(color = AverageTurnover), size = 3) +
  scale_color_viridis_c(option = "C", direction = -1, name = "Average\nTurnover") +
  coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  labs(title = '',
       x = 'Longitude', 
       y = 'Latitude') +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.5, "cm"))
print(map_plot)
ggsave("Results/HeatmapTurnover.png", dpi = 300)
