######################## Lake fires vs. all fires ###############################################
# Date: 4-30-18
# updated: 7-25-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(rgeos)

#### input data ####
# All LAGOS GIS data in same coordinate system
states_shp <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#lakes_4ha <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")

lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

# MTBS polygon data (area burned) (Monitoring Trends in Burn Severity)
MTBS_polygon <- shapefile("C:/Ian_GIS/MTBS/mtbs_perims_1984_2015_DD_20170815_LOWER48.shp")
MTBS_polygon <- spTransform(MTBS_polygon, CRSobj = crs(states_shp))

#### define constants ####
first_year = 1984
last_year = 2015

#### load functions ####
source("C:/R_code/FeelTheBurn/functions/number_fires_by_lake.R")
source("C:/R_code/FeelTheBurn/functions/zones_that_burned_given_year.R")

############################### Main program ######################################
# list of burned watersheds by lagoslakeid
# if already run/saved, can read in burned lagoslakeids
burned_watersheds <- read.csv("C:/Ian_GIS/FeelTheBurn/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)
year_seq <- seq(first_year, last_year, 1)
Buff1500m_all <- gBuffer(lakes_4ha_all, byid=T, width=1500)
BurnedBuff1500m <- subset(Buff1500m_all, lagoslakei %in% burned_watersheds)

# calculate number of fires by year in lake watersheds
#nFiresLake_annual <- data.frame(Year=seq(first_year,last_year,1),nTotalFires=NA)

nFiresLake_annual <- data.frame(matrix(NA, nrow = length(year_seq), ncol = 2))
nFiresLake_annual[,1] = year_seq
colnames(nFiresLake_annual) <- c("Year","nLakeFires")

for (i in 1:length(year_seq)){
  burn_year <- subset(MTBS_polygon, Year==year_seq[i])
  burn_area_zone <- suppressWarnings(raster::intersect(burn_year, BurnedBuff1500m))
  nFires_givenyear <- length(unique(burn_area_zone@data$Fire_ID))
  nFiresLake_annual[i,2] <- nFires_givenyear
  burn_year=NULL
  nFires_givenyear=NULL
}

# calculate number of total fires by year in continental USA
all_fires_annual <- MTBS_polygon@data %>% 
  count(Year)

nFiresLake_annual$AllFires <- all_fires_annual[,2]
colnames(nFiresLake_annual) <- c("Year","nLakeFires","nAllFires")

# correlation between lake fires and all fires
cor(nFiresLake_annual$nLakeFires, nFiresLake_annual$nAllFires$n)
plot(nFiresLake_annual$nLakeFires ~ nFiresLake_annual$nAllFires$n, xlab='All Fires', 
     ylab='Fires in Lake Watersheds', pch=20)
legend('topleft',legend=paste0("r = ",round(cor(nFiresLake_annual$nLakeFires, nFiresLake_annual$nAllFires$n),3)), bty='n')

# are lake fires equally common as all fires?
lake_all_fires <- as.data.frame(rbind(c(year_seq),c(nFiresLake_annual$nLakeFires), c(nFiresLake_annual$nAllFires$n)))
colnames(lake_all_fires) <- year_seq

# basic plot of lake fires vs. all fires
barplot(as.matrix(lake_all_fires[2:3,]), col=c('black','gold'), beside=T, las=2, ylim=c(0,2000))
legend('topleft',legend=c('Lake fires','All fires'), lwd=c(3,3), col=c('black','gold'), bty='n')

# test if lake fires vs. all fires are from same distribution
chisq_fire <- chisq.test(lake_all_fires[2:3,])
chisq_fire

plot(chisq_fire$observed ~ chisq_fire$expected, pch=20, xlab='observed',ylab='expected',
     xlim=c(0,1800), ylim=c(0,1800), las=1)
