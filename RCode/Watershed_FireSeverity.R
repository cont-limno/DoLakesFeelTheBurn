######################## LAGOS US watershed fire severity #######################################
# Date: 4-9-18
# updated: 4-3-19
# Author: Ian McCullough, immccull@gmail.com
#################################################################################################

setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

#### R libraries ####
library(LAGOSNE)
library(raster)
library(rgeos)

#### input data ####

# All LAGOS GIS data in same coordinate system
# See: https://lagoslakes.org/products/data-products/
lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

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

# rasters of MTBS data (burn severity rasters by year)
year_seq <- seq(first_year, last_year, 1)
name_vec = paste0(rep("MTBS_BS_",length(year_seq)), year_seq)

# read in unprocessed MTBS severity rasters
# Downloaded Jan 2018 from https://www.mtbs.gov/
for (i in 1:length(year_seq)){
  xrast = raster(paste0("C:/Ian_GIS/MTBS/BurnSeverityMosaics/composite_data/MTBS_BSmosaics/",year_seq[i],"/mtbs_CONUS_",year_seq[i],".tif"))
  assign(name_vec[i], xrast)
  xrast=NULL
}
rast_list = Filter(function(x) is(x, "RasterLayer"), mget(ls())) # make sure don't have other rasters in environment
#MTBS_stack <- stack(rast_list) #stack doesn't work due to differing extents, but this is OK...don't need stack

# with help from:
# http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
output_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = 13))

### double loop, through years and lagoslakeids
# this loop is much slower for watersheds that actually have fire (flies through others)
# NOTE: when using polygons, those seem to include class 1 (unburned to low severity) in total area burned
# This can make the area burned estimates different than those from polygons for a given lake
# Presumably, this lowest severity class shouldn't have a large effect on the lakes
#burned_watersheds=burned_watersheds[1:2] # first had tested on first two lagoslakeids
for (j in 1:length(burned_watersheds)){
  output_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = 13))
  buffer_to_use <- buffer_erase_lagoslakeid(lagoslakeid=burned_watersheds[j], lake_shp=lakes_4ha_all, buffer_width_m=1500)
  buffer_area_ha <- gArea(buffer_to_use)/10000
  for (i in 1:length(year_seq)){
    extracted <- extract(rast_list[[i]], buffer_to_use, method='simple')
    tab <- tabulate_burn_severity(extracted=extracted, lagoslakeid=burned_watersheds[j], year=year_seq[i], cell_size_sqm=900, buffer_area_ha=buffer_area_ha)
    output_df[i,] <- tab
    extracted <- NULL
    tab <- NULL
  }
  colnames(output_df) <- c('Year','unburned_low_ha','unburned_low_pct','low_severity_ha','low_severity_pct',
                           'moderate_severity_ha','moderate_severity_pct','high_severity_ha','high_severity_pct',
                           'increased_greenness_ha','increased_greenness_pct','Total_ha','Total_pct')
  write.csv(output_df, paste0("ExportedData/lake_fire_history_severity/buffer1500m/buffer1500m_burn_severity_",burned_watersheds[j],".csv"))  
  output_df <- NULL
}