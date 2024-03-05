options (java.parameters = "-Xmx1024m" )
#library(XLConnect)
library(dplyr)
library(tidyr)
setwd("C:/Users/hartt/Documents/Chapter 1")

#1. Get point counts from uncut forests

spp<-read.csv(file="0_data/raw/RQST_LIONEL_93_18_CL_PTCOUNT50m1ha.csv", header=TRUE)   
#pulls Calling Lake point counts into R  
#212676 obs. of  9 variables

#For this analysis, we want to use all the years of data, but not all 
#of the sites. Also, we are using birds within 100 m of point counts, but at the 1-ha
#sites, we are only using birds occurring within the original boundaries of those
#sites (50 m)

spp_CLCON<-spp[which(substring(spp$PKEY,1,5)=="CL:C-"),]
#filters out control point counts from Calling Lake point counts 
spp_CLISO<-spp[which(substring(spp$PKEY,1,5)=="CL:F-"),]
#filters out forest fragment point counts from Calling Lake point counts 
spp_CLRIP<-spp[which(substring(spp$PKEY,1,5)=="CL:R-"),]
#filters out riparian site point counts from Calling Lake point counts 

spp_cc<-rbind(spp_CLCON,spp_CLISO,spp_CLRIP)
#163917 obs. of  9 variables

spp_cc<-spp_cc[which(spp_cc$DURATION==1),]
#filters out species detected during 5-minute point counts
#163899 obs. of  9 variables



#Each observation in this data frame is the number of a species 
#within a given distance interval (0-50 m, 50-100 m, 100-Inf) and
#a given duration interval within a point count visit to each station.

#A station visit can have multiple observations.

#The column WTABUND (weighted abundance) upweights the number counted 
#according to behavioral observations suggestive of local breeding. The
#highest weight would be an abundance of 2 for an observation (1 male+
#1 female). Observations can also be downweighted if they do not represent
#significant evidence of use.
#So birds that were observed before or after point counts or as flyovers
#or flying through without "using" the point count have already been 
#treated as having "zero" abundance. Any waterfowl or waders within the processed
#data files for this study should probably then be treated as "present"
#since flyovers will have been eliminated.

#2. remove birds > 100 m distant
spp_cc<-spp_cc[!spp_cc$DISTANCE>2,]
#removes observations >100 m away because it's likely such individuals have
#been counted at an adjacent station
#DISTANCE==1 = 0-50 m
#DISTANCE==2 = 50-100 m
#DISTANCE==3 = 100 m - unlimited distance
nrow(spp_cc)
#151346 obs.


#3. merge point count data with variables affecting detectability.
pk<-read.csv(file="0_data/raw/RQST_LIONEL_93_18_CL_PKEY.csv", header=TRUE)
#pulls Calling Lake point count data (date, time of morning, observer) into R  

merge1<-merge(pk, spp_cc, by = c("PCODE","PKEY"))
#a left-handed join: by default, the only observations that will be merged
#are those observations with PKEY values in both data frames 
# Filter the merge1 dataframe for only the sites in sites_to_keep
merge1_filtered <- merge1[merge1$SS %in% sites_to_keep, ]

# Check the structure of the filtered dataframe
str(merge1_filtered)
#variables from "pk" in left hand columns followed by variables from "spp_cc"
#the same variables from both data frames, e.g. PCODE, are renamed, e.g. PCODE.x and PCODE.y
#unless they are the variables used in merging, e.g. PKEY

merge1_filtered$VISIT<-paste0(merge1_filtered$SS,"_",merge1_filtered$YYYY,"_",merge1_filtered$ROUND)
#creates a variable for station-visit
write.csv(merge1_filtered, file = "0_data/processed/1_merge1.csv")

#4. calculate total abundance per species per site visit in each cutblock 
merge1<-read.csv("0_data/processed/1_merge1.csv", header=TRUE)
merge1$ABUND<-is.atomic(merge1$WT_ABUND)
tapply.spp<-tapply(merge1$ABUND, list(merge1$VISIT, merge1$SPECIES), sum, na.rm=TRUE)
tapply.spp<-data.frame(tapply.spp)
tapply.spp$VISIT<-row.names(tapply.spp)
write.csv(tapply.spp, file = "0_data/processed/2_tapply.spp.csv")  
#in the past, I manually created "3_spp_count" from "2_tapply.spp". Automate this
#with R.
spp_count<-tapply.spp%>%
  mutate_at(vars(ALFL:YWAR), ~replace(., is.na(.), 0))
write.csv(spp_count, file = "0_data/processed/3_spp_count.csv")  

#5. merge summarized bird count data per visit and visit-specific environmental 
#covariates in "merge1" by the variable VISIT
merge1<-read.csv("0_data/processed/1_merge1.csv", header=TRUE)
birdcount<-read.csv("0_data/processed/3_spp_count.csv", header=TRUE)
VISIT<-merge1$VISIT
SITE<-merge1$SITE
SS<-merge1$SS
YYYY<-merge1$YYYY
YEAR<-merge1$YYYY-1993
ROUND<-merge1$ROUND
JULIAN<-merge1$JULIAN
HR<-merge1$HR
MIN<-merge1$MIN
TIME<-merge1$HR+(merge1$MIN/60)

newdf<-data.frame(VISIT,SITE,SS,YYYY,YEAR,ROUND,JULIAN,HR,MIN,TIME)
#create "newdf" to have a smaller number of covariates to add
#to bird counts. Right now, "merge1" and "newdf" have multiple rows
#per site visit because the bird data have not been summarized
#in these files.
nrow(newdf)#151295 observations
#you need to whittle the data frame down to 1 row per visit,
#otherwise you will have many false duplicate observations when 
#you add the summarized bird data.
newdf<-unique(newdf)
nrow(newdf)#25498 observations


merge2<-merge(newdf, birdcount, by = c("VISIT"), all.x=TRUE)  
#a left outer join. Visits in "newdf" missing data from "birdcount"
#will still have observations in the merged file, but missing bird data
#(if there are any) will be "NA"
str(merge2)
nrow(merge2)#25498 observations
write.csv(merge2, "0_data/processed/4_merge2.csv") #output check


#The purpose of this script is to reformat count and observation covariate
#data into a form suitable for incorporating into an occupancy modelling data
#frame in R's unmarked package. Data are converted from long format with one 
#row per station visit and one column per variable to wide format with one
#spreadsheet per variable, one row per station, and one column per visit.

#Once data are recast in wide format, a separate script is necessary for calculating
#spatial weights for each station, to account for the autocorrelation in detection
#and occupancy that almost certainly exists among stations within the same sites.

#output needs to be "recast so that counts associated with separate visits to same point count are put in separate columns

merge2.yearround<-read.csv(file="0_data/processed/4_merge2.csv", header=TRUE)      
levels(as.factor(merge2.yearround$ROUND))
#deal with weird values for round: find explanations; correct or if necessary,
#discard data
merge2.yearround$ROUND[merge2.yearround$ROUND==9]<-2
merge2.yearround$ROUND[merge2.yearround$ROUND=="c"]<-2
merge2.yearround$ROUND[merge2.yearround$ROUND==""]<-3
merge2.yearround$ROUND[merge2.yearround$ROUND==650]<-3
merge2.yearround<-merge2.yearround[!merge2.yearround$ROUND=="2b",]
merge2.yearround<-merge2.yearround[!merge2.yearround$ROUND==6,]


merge2.yearround$yearround<-paste0(merge2.yearround$YEAR,"_",merge2.yearround$ROUND)

merge2.yearround$BTNW<-ifelse(merge2.yearround$BTNW>1,1,merge2.yearround$BTNW)
head(merge2.yearround)


names = c("BTNW")#run one or more species

for(i in names){ 
  spp<-merge2.yearround[,c(i)]  #Species as variable
  
  recasted<-tapply(spp, list(merge2.yearround$SS, merge2.yearround$yearround), max, na.rm=TRUE)
  write.csv(recasted, paste0("0_data/processed/5_recast.",i,".csv"))
  
  recasted<-read.csv(paste0("0_data/processed/5_recast.",i,".csv"), header=TRUE)
  names(recasted)
  #The number of visits within a given year varies among stations and across years.
  #Earlier years in the study had more visits per station on average (~5) than
  #recent years in the study. From 2016-2018, we'd be satisfied if we got 3 visits
  #per station. To run multi-season models in "unmarked" in R, you will need to
  #specify the potential number of visits per year and it has to be the same
  #number for all years. If you use a larger number of visits per year (e.g. 4-5), you will
  #need to add columns of NA values (missing visits) for some of the years. A
  #minimum of 3-4 visits is recommended.
  recasted$SS<-recasted$X
  recasted$X15_4<-NA
  recasted$X24_4<-NA #In the 24th year of the study no station had more than 3
  #visits. Assuming that we are using 4 visits per year, an additional column
  #needs to be created. You'll notice that some stations with less than 4 visits
  #in a given year will have NA values for the missing visit. Keep those NA's!
  #Also keep the NA's for the same visits for your visit-specific covariates!
  
  rearranged <-recasted[,c("SS","X0_1","X0_2","X0_3","X0_4",
                           "X1_1","X1_2","X1_3","X1_4",
                           "X2_1","X2_2","X2_3","X2_4",
                           "X3_1","X3_2","X3_3","X3_4",
                           "X4_1","X4_2","X4_3","X4_4",
                           "X5_1","X5_2","X5_3","X5_4",
                           "X6_1","X6_2","X6_3","X6_4",
                           "X7_1","X7_2","X7_3","X7_4",
                           "X8_1","X8_2","X8_3","X8_4",
                           "X9_1","X9_2","X9_3","X9_4",
                           "X10_1","X10_2","X10_3","X10_4",
                           "X11_1","X11_2","X11_3","X11_4",
                           "X12_1","X12_2","X12_3","X12_4",
                           "X13_1","X13_2","X13_3","X13_4",
                           "X14_1","X14_2","X14_3","X14_4",
                           "X15_1","X15_2","X15_3","X15_4",
                           "X16_1","X16_2","X16_3","X16_4",
                           "X17_1","X17_2","X17_3","X17_4",
                           "X18_1","X18_2","X18_3","X18_4",
                           "X19_1","X19_2","X19_3","X19_4",
                           "X20_1","X20_2","X20_3","X20_4",
                           "X21_1","X21_2","X21_3","X21_4",
                           "X22_1","X22_2","X22_3","X22_4",
                           "X23_1","X23_2","X23_3","X23_4",
                           "X24_1","X24_2","X24_3","X24_4",
                           "X25_1","X25_2","X25_3","X25_4")]
  #reorders recast columns for each visit in correct chronological order
  write.csv(rearranged, paste0("0_data/processed/6_rearranged.",i,".csv"))  #  
}

#NOTE: If you examine the data frames, you will find that in year 12 (X11_1
#through X11_4) and 16 (X15_1 through X15_4) some of the riparian point counts
#don't have any visits in those years. You will have to decide if you want 
#to remove these sites from your analysis or skip those years from all sites
#in the analysis.

#This decision should be made before you create the spatial weight matrix
#and the multi-season occupancy modelling data frame.

#Due to the small number of data frames for which data has to be removed, I am
#removing stations and years (just 2011) manually. You could add commands before 
#recasting the data to remove stations and years automatically, but for now, recasting
#all station and year data will force you to examine the results for stations missing
#an entire year's worth of data, since this will vary with the point counts that 
#are available for analysis. What you need to know is that the in "rearranged" files,
#there should be 219 instead of the original 233 stations and year 2011 is removed.

recast.julian<-tapply(merge2.yearround$JULIAN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.julian, "0_data/processed/7_recast.julian.csv")

recast.julian<-read.csv("0_data/processed/7_recast.julian.csv", header=TRUE)
names(recast.julian)
recast.julian$SS<-recast.julian$X
recast.julian$X15_4<-NA
recast.julian$X24_4<-NA
rearranged.julian <-recast.julian[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                     "X1_1","X1_2","X1_3","X1_4",
                                     "X2_1","X2_2","X2_3","X2_4",
                                     "X3_1","X3_2","X3_3","X3_4",
                                     "X4_1","X4_2","X4_3","X4_4",
                                     "X5_1","X5_2","X5_3","X5_4",
                                     "X6_1","X6_2","X6_3","X6_4",
                                     "X7_1","X7_2","X7_3","X7_4",
                                     "X8_1","X8_2","X8_3","X8_4",
                                     "X9_1","X9_2","X9_3","X9_4",
                                     "X10_1","X10_2","X10_3","X10_4",
                                     "X11_1","X11_2","X11_3","X11_4",
                                     "X12_1","X12_2","X12_3","X12_4",
                                     "X13_1","X13_2","X13_3","X13_4",
                                     "X14_1","X14_2","X14_3","X14_4",
                                     "X15_1","X15_2","X15_3","X15_4",
                                     "X16_1","X16_2","X16_3","X16_4",
                                     "X17_1","X17_2","X17_3","X17_4",
                                     "X18_1","X18_2","X18_3","X18_4",
                                     "X19_1","X19_2","X19_3","X19_4",
                                     "X20_1","X20_2","X20_3","X20_4",
                                     "X21_1","X21_2","X21_3","X21_4",
                                     "X22_1","X22_2","X22_3","X22_4",
                                     "X23_1","X23_2","X23_3","X23_4",
                                     "X24_1","X24_2","X24_3","X24_4",
                                     "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.julian, paste0("0_data/processed/8_rearranged.julian.csv"))  #  

meanjulian<- mean(merge2.yearround$JULIAN)
merge2.yearround$J.CEN<-scale(merge2.yearround$JULIAN, center=TRUE, scale=FALSE)
merge2.yearround$J.CEN2<-merge2.yearround$J.CEN^2

recast.jcen<-tapply(merge2.yearround$J.CEN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.jcen, "0_data/processed/9_recast.jcen.csv")

recast.jcen<-read.csv("0_data/processed/9_recast.jcen.csv", header=TRUE)
names(recast.jcen)
recast.jcen$SS<-recast.julian$X
recast.jcen$X15_4<-NA
recast.jcen$X24_4<-NA
rearranged.jcen <-recast.jcen[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                 "X1_1","X1_2","X1_3","X1_4",
                                 "X2_1","X2_2","X2_3","X2_4",
                                 "X3_1","X3_2","X3_3","X3_4",
                                 "X4_1","X4_2","X4_3","X4_4",
                                 "X5_1","X5_2","X5_3","X5_4",
                                 "X6_1","X6_2","X6_3","X6_4",
                                 "X7_1","X7_2","X7_3","X7_4",
                                 "X8_1","X8_2","X8_3","X8_4",
                                 "X9_1","X9_2","X9_3","X9_4",
                                 "X10_1","X10_2","X10_3","X10_4",
                                 "X11_1","X11_2","X11_3","X11_4",
                                 "X12_1","X12_2","X12_3","X12_4",
                                 "X13_1","X13_2","X13_3","X13_4",
                                 "X14_1","X14_2","X14_3","X14_4",
                                 "X15_1","X15_2","X15_3","X15_4",
                                 "X16_1","X16_2","X16_3","X16_4",
                                 "X17_1","X17_2","X17_3","X17_4",
                                 "X18_1","X18_2","X18_3","X18_4",
                                 "X19_1","X19_2","X19_3","X19_4",
                                 "X20_1","X20_2","X20_3","X20_4",
                                 "X21_1","X21_2","X21_3","X21_4",
                                 "X22_1","X22_2","X22_3","X22_4",
                                 "X23_1","X23_2","X23_3","X23_4",
                                 "X24_1","X24_2","X24_3","X24_4",
                                 "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.jcen, paste0("0_data/processed/10_rearranged.jcen.csv"))  #  


recast.jcen2<-tapply(merge2.yearround$J.CEN2, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.jcen2, "0_data/processed/11_recast.jcen2.csv")

recast.jcen2<-read.csv("0_data/processed/11_recast.jcen2.csv", header=TRUE)
names(recast.jcen2)
recast.jcen2$SS<-recast.julian$X
recast.jcen2$X15_4<-NA
recast.jcen2$X24_4<-NA
rearranged.jcen2 <-recast.jcen2[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                   "X1_1","X1_2","X1_3","X1_4",
                                   "X2_1","X2_2","X2_3","X2_4",
                                   "X3_1","X3_2","X3_3","X3_4",
                                   "X4_1","X4_2","X4_3","X4_4",
                                   "X5_1","X5_2","X5_3","X5_4",
                                   "X6_1","X6_2","X6_3","X6_4",
                                   "X7_1","X7_2","X7_3","X7_4",
                                   "X8_1","X8_2","X8_3","X8_4",
                                   "X9_1","X9_2","X9_3","X9_4",
                                   "X10_1","X10_2","X10_3","X10_4",
                                   "X11_1","X11_2","X11_3","X11_4",
                                   "X12_1","X12_2","X12_3","X12_4",
                                   "X13_1","X13_2","X13_3","X13_4",
                                   "X14_1","X14_2","X14_3","X14_4",
                                   "X15_1","X15_2","X15_3","X15_4",
                                   "X16_1","X16_2","X16_3","X16_4",
                                   "X17_1","X17_2","X17_3","X17_4",
                                   "X18_1","X18_2","X18_3","X18_4",
                                   "X19_1","X19_2","X19_3","X19_4",
                                   "X20_1","X20_2","X20_3","X20_4",
                                   "X21_1","X21_2","X21_3","X21_4",
                                   "X22_1","X22_2","X22_3","X22_4",
                                   "X23_1","X23_2","X23_3","X23_4",
                                   "X24_1","X24_2","X24_3","X24_4",
                                   "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.jcen2, paste0("0_data/processed/12_rearranged.jcen2.csv"))  #  


recast.time<-tapply(merge2.yearround$TIME, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.time, "0_data/processed/13_recast.time.csv")

recast.time<-read.csv("0_data/processed/13_recast.time.csv", header=TRUE)
names(recast.time)
recast.time$SS<-recast.julian$X
recast.time$X15_4<-NA
recast.time$X24_4<-NA
rearranged.time <-recast.time[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                 "X1_1","X1_2","X1_3","X1_4",
                                 "X2_1","X2_2","X2_3","X2_4",
                                 "X3_1","X3_2","X3_3","X3_4",
                                 "X4_1","X4_2","X4_3","X4_4",
                                 "X5_1","X5_2","X5_3","X5_4",
                                 "X6_1","X6_2","X6_3","X6_4",
                                 "X7_1","X7_2","X7_3","X7_4",
                                 "X8_1","X8_2","X8_3","X8_4",
                                 "X9_1","X9_2","X9_3","X9_4",
                                 "X10_1","X10_2","X10_3","X10_4",
                                 "X11_1","X11_2","X11_3","X11_4",
                                 "X12_1","X12_2","X12_3","X12_4",
                                 "X13_1","X13_2","X13_3","X13_4",
                                 "X14_1","X14_2","X14_3","X14_4",
                                 "X15_1","X15_2","X15_3","X15_4",
                                 "X16_1","X16_2","X16_3","X16_4",
                                 "X17_1","X17_2","X17_3","X17_4",
                                 "X18_1","X18_2","X18_3","X18_4",
                                 "X19_1","X19_2","X19_3","X19_4",
                                 "X20_1","X20_2","X20_3","X20_4",
                                 "X21_1","X21_2","X21_3","X21_4",
                                 "X22_1","X22_2","X22_3","X22_4",
                                 "X23_1","X23_2","X23_3","X23_4",
                                 "X24_1","X24_2","X24_3","X24_4",
                                 "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.time, paste0("0_data/processed/14_rearranged.time.csv"))  #  



merge2.yearround$T.CEN<-scale(merge2.yearround$TIME, center=TRUE, scale=FALSE)
merge2.yearround$T.CEN2<-merge2.yearround$T.CEN^2

recast.tcen<-tapply(merge2.yearround$T.CEN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.tcen, "0_data/processed/15_recast.tcen.csv")

recast.tcen<-read.csv("0_data/processed/15_recast.tcen.csv", header=TRUE)
names(recast.tcen)
recast.tcen$SS<-recast.julian$X
recast.tcen$X15_4<-NA
recast.tcen$X24_4<-NA
rearranged.tcen <-recast.tcen[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                 "X1_1","X1_2","X1_3","X1_4",
                                 "X2_1","X2_2","X2_3","X2_4",
                                 "X3_1","X3_2","X3_3","X3_4",
                                 "X4_1","X4_2","X4_3","X4_4",
                                 "X5_1","X5_2","X5_3","X5_4",
                                 "X6_1","X6_2","X6_3","X6_4",
                                 "X7_1","X7_2","X7_3","X7_4",
                                 "X8_1","X8_2","X8_3","X8_4",
                                 "X9_1","X9_2","X9_3","X9_4",
                                 "X10_1","X10_2","X10_3","X10_4",
                                 "X11_1","X11_2","X11_3","X11_4",
                                 "X12_1","X12_2","X12_3","X12_4",
                                 "X13_1","X13_2","X13_3","X13_4",
                                 "X14_1","X14_2","X14_3","X14_4",
                                 "X15_1","X15_2","X15_3","X15_4",
                                 "X16_1","X16_2","X16_3","X16_4",
                                 "X17_1","X17_2","X17_3","X17_4",
                                 "X18_1","X18_2","X18_3","X18_4",
                                 "X19_1","X19_2","X19_3","X19_4",
                                 "X20_1","X20_2","X20_3","X20_4",
                                 "X21_1","X21_2","X21_3","X21_4",
                                 "X22_1","X22_2","X22_3","X22_4",
                                 "X23_1","X23_2","X23_3","X23_4",
                                 "X24_1","X24_2","X24_3","X24_4",
                                 "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.tcen, paste0("0_data/processed/16_rearranged.tcen.csv"))  #  

recast.tcen2<-tapply(merge2.yearround$T.CEN2, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
write.csv(recast.tcen2, "0_data/processed/17_recast.tcen2.csv")

recast.tcen2<-read.csv("0_data/processed/17_recast.tcen2.csv", header=TRUE)
names(recast.tcen2)
recast.tcen2$SS<-recast.julian$X
recast.tcen2$X15_4<-NA
recast.tcen2$X24_4<-NA
rearranged.tcen2 <-recast.tcen2[,c("SS","X0_1","X0_2","X0_3","X0_4",
                                   "X1_1","X1_2","X1_3","X1_4",
                                   "X2_1","X2_2","X2_3","X2_4",
                                   "X3_1","X3_2","X3_3","X3_4",
                                   "X4_1","X4_2","X4_3","X4_4",
                                   "X5_1","X5_2","X5_3","X5_4",
                                   "X6_1","X6_2","X6_3","X6_4",
                                   "X7_1","X7_2","X7_3","X7_4",
                                   "X8_1","X8_2","X8_3","X8_4",
                                   "X9_1","X9_2","X9_3","X9_4",
                                   "X10_1","X10_2","X10_3","X10_4",
                                   "X11_1","X11_2","X11_3","X11_4",
                                   "X12_1","X12_2","X12_3","X12_4",
                                   "X13_1","X13_2","X13_3","X13_4",
                                   "X14_1","X14_2","X14_3","X14_4",
                                   "X15_1","X15_2","X15_3","X15_4",
                                   "X16_1","X16_2","X16_3","X16_4",
                                   "X17_1","X17_2","X17_3","X17_4",
                                   "X18_1","X18_2","X18_3","X18_4",
                                   "X19_1","X19_2","X19_3","X19_4",
                                   "X20_1","X20_2","X20_3","X20_4",
                                   "X21_1","X21_2","X21_3","X21_4",
                                   "X22_1","X22_2","X22_3","X22_4",
                                   "X23_1","X23_2","X23_3","X23_4",
                                   "X24_1","X24_2","X24_3","X24_4",
                                   "X25_1","X25_2","X25_3","X25_4")]
#reorders recast columns for each visit in correct chronological order
write.csv(rearranged.tcen2, paste0("0_data/processed/18_rearranged.tcen2.csv"))  #  

