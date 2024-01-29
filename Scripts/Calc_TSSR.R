# Calculate tssr (time since sunrise)
library(lubridate)
#install.packages("suntools")
library(suntools)
detcovs <- read.csv("CLpoints_covariates.csv")

# Convert dates to Julian day  
DD <- with(detcovs, paste0(YYYY, "-", MM, "-", DD, " ", HR, ":", MIN, ":00"))
DD <- strptime(DD, "%Y-%m-%e %H:%M:%S")
## Julian day
detcovs$julian_day <- yday(DD) # this is kept as original
detcovs$jday <- yday(DD)/365
summary(detcovs$jday)

# Calculate time since sunrise 
Coor <- as.matrix(cbind(as.numeric(detcovs$longitude),as.numeric(detcovs$latitude)))
JL <- as.POSIXct(DD)
subset <- rowSums(is.na(Coor))==0 & !is.na(JL)
sr <- sunriset(Coor[subset,], JL[subset], direction="sunrise", POSIXct.out=FALSE) * 24
detcovs$srise <- NA
detcovs$srise[subset] <- sr
# Assuming detcovs$srise is a list of numeric values
detcovs$srise <- sapply(detcovs$srise, function(x) ifelse(is.numeric(x), x, NA))
detcovs$start_time <- detcovs$HR + detcovs$MIN/60
detcovs$start_time <- as.numeric(detcovs$start_time) #to be sure 
detcovs$tssr <- detcovs$start_time - detcovs$srise
head(detcovs)


write.csv(detcovs, "detcovs.csv")

