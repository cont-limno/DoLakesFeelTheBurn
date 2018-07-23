######################## Classifying US lakes by Landfire #######################################
# Date: 1-17-18
# updated: 7-11-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)
library(reshape)
library(ggplot2)
library(dplyr)
#library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

# All LAGOS GIS data in same coordinate system
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")

######################## Classifying lakes by fire regime group ##########################
# read in fire regime zonal stats from ArcGIS (zonal stats as a table by lagoslakeid using 1500m buffers around 4ha and larger US lakes)
# using LANDFIRE Fire Regime Groups: https://www.landfire.gov/frg.php
# LF 1.4.0: rasters were downloaded in chunks and mosaicked to new raster in ArcGIS using maximum value as overlap rule
FRG_zonal <- read.csv("ExportedData/LANDFIRE/FRG_1500m_lagoslakeid.csv")
lakes_4ha_pts_FRG <- merge(lakes_4ha_pts, FRG_zonal, by.x='lagoslakei', by.y='LAGOSLAKEI', all.x=F)

#writeOGR(lakes_4ha_pts_FRG, dsn='C:/Ian_GIS/FeelTheBurn/LANDFIRE/Lakes_by_FireRegime', layer='lakes_4ha_pts_FRG', 
#         overwrite_layer = T, driver='ESRI Shapefile')

hist(lakes_4ha_pts_FRG@data$MAJORITY, breaks=c(1,2,3,4,5,111,112,131,132,133))

# stats on lakes in different regime groups
FRG_subset <- subset(lakes_4ha_pts_FRG@data, MAJORITY %in% c(1,2,3,4,5))
hist(FRG_subset$MAJORITY, breaks=c(0,1,2,3,4,5))

FRG_subset$FRG_fac <- NA
FRG_subset$FRG_fac <- cut(FRG_subset$MAJORITY, breaks=c(0,1,2,3,4,5), labels=c('FRG1','FRG2','FRG3','FRG4','FRG5'))

# Number of lakes by FRG
FRG_lake_counts <- FRG_subset %>% 
  group_by(FRG_fac) %>%
  summarise(no_rows = length(FRG_fac))
barplot(FRG_lake_counts$no_rows, names.arg=FRG_lake_counts$FRG_fac)

# get burned subset of all lakes wFire Regime
FRG_subset_burned <- subset(FRG_subset, lagoslakei %in% burned_watersheds)
FRG_lake_counts_burned <- FRG_subset_burned %>% 
  group_by(FRG_fac) %>%
  summarise(no_rows = length(FRG_fac))

FRG_lake_counts_burned$AllLakes <- FRG_lake_counts$no_rows
FRG_lake_counts_burned$PctLakesBurned <- FRG_lake_counts_burned$no_rows/FRG_lake_counts_burned$AllLakes

test <- as.data.frame(t(FRG_lake_counts_burned))
colnames(test) <- c('FRG1','FRG2','FRG3','FRG4','FRG5')
test <- test[-1,]
test <- test[-3,]

par(mar=c(5,5,5,1))
#lty.o <- par("lty") #get rid of default black borders on stacks 
#par(lty = 0) 
barplot(as.matrix(test), col=c('firebrick','tan'), las=2)#, yaxt='n')
#axis(side=2, at=seq(0,5e6,1e6))
legend('topright', legend=c('Burned','All lakes'), pch=c(15,15),
       col=c('firebrick','tan'))
title('MTBS, 1984-2015')

# plot just % of each FRG burned?
test2 <- as.data.frame(t(FRG_lake_counts_burned))
colnames(test2) <- c('FRG1','FRG2','FRG3','FRG4','FRG5')
test2 <- test2[4,]

par(mar=c(5,5,5,1))
#lty.o <- par("lty") #get rid of default black borders on stacks 
#par(lty = 0) 
barplot(as.matrix(test2), col=c('firebrick'), las=2)#, yaxt='n')
#axis(side=2, at=seq(0,5e6,1e6))
#legend('topright', legend=c('Burned','All lakes'), pch=c(15,15),
#       col=c('firebrick','tan'))
title('Proportion of lakes burned by Fire Regime Group')


########################### Characterizing lakes by mean fire return interval ############################
# similar to previous chunk, based on LANDFIRE fire return interval: https://www.landfire.gov/mfri.php
# read in fire regime zonal stats from ArcGIS (zonal stats as a table by lagoslakeid using 1500m buffers around 4ha and larger US lakes)
# LF 1.4.0: rasters were downloaded in chunks and mosaicked to new raster in ArcGIS using maximum value as overlap rule
# reclassified based on median years
MFRI_zonal <- read.csv("ExportedData/LANDFIRE/lagoslakeid_MFRI_1500m.csv")
lakes_4ha_pts_MFRI <- merge(lakes_4ha_pts, MFRI_zonal, by.x='lagoslakei', by.y='LAGOSLAKEI', all.x=F)

#writeOGR(lakes_4ha_pts_MFRI, dsn='C:/Ian_GIS/FeelTheBurn/LANDFIRE/Lakes_by_MFRI', layer='lakes_4ha_pts_MFRI', 
#         overwrite_layer = T, driver='ESRI Shapefile')

#### calculate expected vs. observed wildfire to infer "fire deficit"
# subset only lakes with wildfire
wildfire_lakes <- subset(lakes_nFires, Wildfire > 0)
wildfire_lakes <- wildfire_lakes@data$lagoslakei
fire_deficit_df <- lakes_4ha_pts_MFRI@data
fire_deficit_df <- subset(fire_deficit_df, lagoslakei %in% wildfire_lakes)

# get cumulative percent burned
# first read in data for percent burned and subset to only lakes that had wildfire
percent_cum_burned <- shapefile("C:/Ian_GIS/FeelTheBurn/percent_burned_cum_FRG/percent_burned_cum_FRG.shp")
percent_cum_burned_WF <- subset(percent_cum_burned, lagoslk %in% wildfire_lakes)
percent_cum_burned_WF_df <- data.frame(lagoslakeid=percent_cum_burned_WF@data$lagoslk, WF_pct=percent_cum_burned_WF@data$WF_pct_)

# create necessary columns and perform calculation
# observed pct burned: WF burn area/buffer area
# expected pct burned: given time period (yrs) and MFRI (yrs), how much of buffer area would expected to be burned? 
# expected pct burned = time period (yrs)/MFRI
# deficit = (observed pct burned/expected percent burned) -1, such that a large positive number indicates much more burning than expected and a large negative number is much less burning than expected

fire_deficit_df <- merge(fire_deficit_df, percent_cum_burned_WF_df, by.x='lagoslakei', by.y='lagoslakeid',all.x=F)
fire_deficit_df$MFRI <- fire_deficit_df$MAJORITY #create MFRI column to minimize confusion
fire_deficit_df$ExpectedPctWF <- (last_year - first_year)/fire_deficit_df$MFRI
fire_deficit_df$DeficitPct <- (fire_deficit_df$WF_pct/fire_deficit_df$ExpectedPctWF) - 1
fire_deficit_df_formerge <- fire_deficit_df[,c(1,44,45,46,47)]
fire_deficit_lakes <- merge(lakes_4ha_pts, fire_deficit_df_formerge, by='lagoslakei',all.x=F) 

#writeOGR(fire_deficit_lakes, dsn='C:/Ian_GIS/FeelTheBurn/fire_deficit_WF', layer='fire_deficit_WF_4ha_pts', 
#         overwrite_layer = T, driver='ESRI Shapefile')

# above was just wildfire...how about considering all fires when calculating deficit?
fire_deficit_df_all <- lakes_4ha_pts_MFRI@data
fire_deficit_df_all <- subset(fire_deficit_df_all, lagoslakei %in% burned_watersheds)

# get cumulative percent burned
# first read in data for percent burned and subset to only lakes that had wildfire
percent_cum_burned_all_fire <- subset(percent_cum_burned, lagoslk %in% burned_watersheds)
percent_cum_burned_all_fire_df <- data.frame(lagoslakeid=percent_cum_burned_all_fire@data$lagoslk, Total_pct=percent_cum_burned_all_fire@data$ttl_pc_)

fire_deficit_df_all <- merge(fire_deficit_df_all, percent_cum_burned_all_fire_df, by.x='lagoslakei', by.y='lagoslakeid',all.x=F)
fire_deficit_df_all$MFRI <- fire_deficit_df_all$MAJORITY #create MFRI column to minimize confusion
fire_deficit_df_all$ExpectedPctFire <- (last_year - first_year)/fire_deficit_df_all$MFRI
fire_deficit_df_all$DeficitPct <- (fire_deficit_df_all$Total_pct/fire_deficit_df_all$ExpectedPctFire) - 1
fire_deficit_df_all_formerge <- fire_deficit_df_all[,c(1,44,45,46,47)]
fire_deficit_lakes_all <- merge(lakes_4ha_pts, fire_deficit_df_all_formerge, by='lagoslakei',all.x=F) 

#writeOGR(fire_deficit_lakes_all, dsn='C:/Ian_GIS/FeelTheBurn/fire_deficit_WF', layer='fire_deficit_totalfire_4ha_pts', 
#         overwrite_layer = T, driver='ESRI Shapefile')

# what proportion of lakes have a fire deficit that burned? (neg number means burned less than expected)
nrow(subset(fire_deficit_df_all_formerge, DeficitPct<0))/nrow(fire_deficit_df_all_formerge)

# what proportion of lakes have a fire deficit?
nrow(subset(fire_deficit_df_all_formerge, DeficitPct>=0))/nrow(lakes_4ha_pts@data)

# what proportion of lakes burned more than expected?
nrow(subset(fire_deficit_df_all_formerge, DeficitPct>1))/nrow(lakes_4ha_pts@data)
