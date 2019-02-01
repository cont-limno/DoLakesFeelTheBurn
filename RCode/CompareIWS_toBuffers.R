######################## LAGOS US watershed fire severity #######################################
# Date: 2-1-19
# updated: 
# Author: Ian McCullough, immccull@gmail.com
#################################################################################################

setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

#### R libraries ####
library(LAGOSNE)
library(raster)
library(rgeos)

#### input data ####

# All LAGOS GIS data in same coordinate system
#states_shp <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp")
#lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

#states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_all))

# read in burned lagoslakeids
burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

#### define constants ####
first_year = 1984 #in MTBS dataset
last_year = 2015

#### load functions ####
# function to buffer lakes, erasing lake area
source("RCode/functions/Buffer_erase_lagoslakeid.R")
# function to tabulate area of different burn severity classes in MTBS dataset for each lake buffer
source("RCode/functions/tabulate_burn_severity.R")

########################## Main program ########################
Buff1500m_all <- gBuffer(lakes_4ha_all, byid=T, width=1500)

Buff1500m_all_burned <- subset(Buff1500m_all, lagoslakei %in% burned_watersheds)
Buff1500m_all_burned_area <- gArea(Buff1500m_all_burned, byid=T)
buff_df <- data.frame(lagoslakeid=Buff1500m_all_burned@data$lagoslakei, buff_ha=(Buff1500m_all_burned_area/10000))

# get LAGOS watersheds for NE states
dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects
iws_ha <- data.frame(lagoslakeid=dt$iws$lagoslakeid, iws_ha=dt$iws$iws_ha)

buff_iws_df <- merge(buff_df, iws_ha, all.x=F, by='lagoslakeid')

# should play around with axis limits so can see data better
plot(buff_iws_df$buff_ha ~ buff_iws_df$iws_ha, pch=20, xlim=c(0,5000), ylim=c(0,5000),
     xlab='Watershed area (ha)', ylab='Buffer area (ha)')
abline(0,1)
