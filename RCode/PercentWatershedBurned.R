######################## Calculating % burned in US lake watersheds #################################
# Date: 3-12-18
# updated: 4-30-19
# Author: Ian McCullough, immccull@gmail.com
#####################################################################################################

##### R libraries ######
library(raster)
#library(rgdal)

##### input data ######
# set working directory
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

# LAGOS GIS data
# See: https://lagoslakes.org/products/data-products/ for updates
# this is a point version of the polygon shapefile in the repository (used ArcGIS to convert to points)
lakes_4ha_pts <- shapefile("GIS/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_v0.2_pts.shp")

##### D-fine constants #####
first_year = 1984
last_year = 2015

#### D-fine functions ####
source("RCode/functions/percent_watershed_burned_cumulative.R")

############################################ Main program #####################################################
# get list of burned watersheds (1500m buffer) by lagoslakeid
burned_lagoslakeid <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

# calculate % watershed burned by lagoslakeid using pre-calculated data
output_list <- list() #create blank list to be filled by loop
for (i in 1:length(burned_lagoslakeid)){
  xx <- percent_watershed_burned_cumulative(lagoslakeid=burned_lagoslakeid[i])
  output_list[[i]] <- xx
  xx <- NULL
}

pct_burned_lake <- do.call(rbind, output_list) #data frame with lakes as rows, percent burned as columns

### join pct_burned_lake to lake pts shapefile for mapping in ArcGIS
pct_burned_shp <- merge(lakes_4ha_pts, pct_burned_lake, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

#dsnname = paste0("C:/Ian_GIS/FeelTheBurn/percent_burned_cum")
#layername = "percent_burned_cum"
#writeOGR(pct_burned_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

