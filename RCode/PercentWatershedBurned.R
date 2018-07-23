######################## Calculating % burned in US lake watersheds #################################
# Date: 3-12-18
# updated: 7-11-18
# Author: Ian McCullough, immccull@gmail.com
#####################################################################################################

##### R libraries ######
library(raster)
#library(rgdal)

##### input data ######
# LAGOS GIS data
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")

##### D-fine constants #####
first_year = 1984
last_year = 2015

##### set working directory #####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

############################################ Main program #####################################################
# get list of burned watersheds (1500m buffer) by lagoslakeid
burned_lagoslakeid <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

# calculate % watershed burned by lagoslakeid using pre-calculated data
percent_watershed_burned_cumulative <- function(lagoslakeid){
  #lagoslakeid: lake ID to use
  fire_history <- read.csv(paste0(getwd(),"/ExportedData/lake_fire_history/buffer1500m/buffer1500m_fire_",
                                  lagoslakeid,".csv"))
  total_pct_cum <- max(fire_history$ZoneTotalBurned_pct_cum)
  WF_pct_cum <- suppressWarnings(max(fire_history$wildfire_pct_cum))
  Rx_pct_cum <- suppressWarnings(max(fire_history$Rxfire_pct_cum))
  WLFU_pct_cum <- suppressWarnings(max(fire_history$WLFU_pct_cum))
  Unknown_pct_cum <- suppressWarnings(max(fire_history$unknown_fire_pct_cum))
  output <- data.frame(lagoslakeid=lagoslakeid, total_pct_cum=total_pct_cum, WF_pct_cum=WF_pct_cum, Rx_pct_cum=Rx_pct_cum,
                       WLFU_pct_cum=WLFU_pct_cum, Unknown_pct_cum=Unknown_pct_cum)
  return(output)
}

output_list <- list() #create blank list to be filled by loop
for (i in 1:length(burned_lagoslakeid)){
  xx <- percent_watershed_burned_cumulative(lagoslakeid=burned_lagoslakeid[i])
  output_list[[i]] <- xx
  xx <- NULL
}

pct_burned_lake <- do.call(rbind, output_list) #data frame with lakes as rows, percent burned as columns

### join pct_burned_lake to lake pts shapefile for mapping
pct_burned_shp <- merge(lakes_4ha_pts, pct_burned_lake, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

#dsnname = paste0("C:/Ian_GIS/FeelTheBurn/percent_burned_cum")
#layername = "percent_burned_cum"
#writeOGR(pct_burned_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

