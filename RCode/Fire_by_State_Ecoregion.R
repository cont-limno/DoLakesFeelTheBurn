############### Mapping, analyzing watershed fire by US state and Bailey's provinces ######################
# Date: 4-11-18
# updated: 4-3-19
# Author: Ian McCullough, immccull@gmail.com
###########################################################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(rgeos)
library(Hmisc)
library(tmap)
library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")
# All LAGOS GIS data in same coordinate system
# See: https://lagoslakes.org/products/data-products/
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")

# other GIS data
states_shp <- shapefile("GIS/US_states/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_pts)) #reproject to LAGOS projection

# Bailey's ecoregions (provinces) (US Forest Service 1994)
# downloaded Apr 2019: https://www.sciencebase.gov/catalog/item/54244abde4b037b608f9e23d 
baileys <- shapefile("GIS/baileys_ecoregions/eco_us_province_dissolved.shp")
baileys <- spTransform(baileys, CRSobj = crs(states_shp))

# read in burned lagoslakeids
burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)
csv_list <- list.files("ExportedData/lake_fire_history_severity/buffer1500m", pattern='.csv', full.names=T)
area_burned_list <- list.files("ExportedData/lake_fire_history/buffer1500m", pattern='.csv', full.names=T)

# burned lagoslakeids for wildfire and prescribed fire
burned_watersheds_WF <- read.csv("ExportedData/Burned1500mBuffs_WF.csv")[,2] #reads 2nd column (lagoslakeid)
burned_watersheds_Rx <- read.csv("ExportedData/Burned1500mBuffs_Rx.csv")[,2]

#### define constants ####
first_year = 1984
last_year = 2015
year_seq <- seq(first_year, last_year, 1)

#################################### Main program ####################################
# count number of lake fires of each severity class by lake
output_df <- data.frame(matrix(NA, nrow = length(csv_list), ncol = 3))
colnames(output_df) <- c('Low','Moderate','High')
for (i in 1:length(csv_list)){
  fire_hist <- read.csv(csv_list[i])
  fire_hist[,1] <- NULL #remove useless index column
  nLow <- nrow(subset(fire_hist, low_severity_ha > 0))
  nMod <- nrow(subset(fire_hist, moderate_severity_ha > 0))
  nHigh <- nrow(subset(fire_hist, high_severity_ha > 0))
  output_df[i,1] <- nLow
  output_df[i,2] <- nMod
  output_df[i,3] <-nHigh
  nLow <- NULL
  nMod <- NULL
  nHigh <- NULL
}

# get rownames for output_df created above
test <- unPaste(csv_list, sep='ty_')[2]
test <- unPaste(test[[1]], sep='.csv')
temp_lagoslakeids <- as.numeric(test[[1]])
rownames(output_df) <- temp_lagoslakeids #numeric string of burned lagoslakeids

# merge to lake points for mapping/subsetting later
output_df$lagoslakei <- rownames(output_df)
burned_lakes <- merge(lakes_4ha_pts, output_df, by='lagoslakei', all.x=F)
burned_lakes_LS <- subset(burned_lakes, Low > 0)
burned_lakes_MS <- subset(burned_lakes, Moderate > 0)
burned_lakes_HS <- subset(burned_lakes, High > 0)

burned_lakes_WF <- subset(burned_lakes, lagoslakei %in% burned_watersheds_WF)
burned_lakes_Rx <- subset(burned_lakes, lagoslakei %in% burned_watersheds_Rx)

#### get fire histories by severity class (not specific to regions)
# get high-severity fire histories
HS_df <- data.frame(matrix(NA, ncol = length(csv_list), nrow = length(year_seq)))
for (i in 1:length(csv_list)){
  fire_hist <- read.csv(csv_list[i])
  HS_df[,i] <- fire_hist$high_severity_pct
  fire_hist <- NULL
}
colnames(HS_df) <- burned_watersheds
rownames(HS_df) <- year_seq

# get moderate-severity fire histories
MS_df <- data.frame(matrix(NA, ncol = length(csv_list), nrow = length(year_seq)))
for (i in 1:length(csv_list)){
  fire_hist <- read.csv(csv_list[i])
  MS_df[,i] <- fire_hist$moderate_severity_pct
  fire_hist <- NULL
}
colnames(MS_df) <- burned_watersheds
rownames(MS_df) <- year_seq

# get low-severity fire histories
LS_df <- data.frame(matrix(NA, ncol = length(csv_list), nrow = length(year_seq)))
for (i in 1:length(csv_list)){
  fire_hist <- read.csv(csv_list[i])
  LS_df[,i] <- fire_hist$low_severity_pct
  fire_hist <- NULL
}
colnames(LS_df) <- burned_watersheds
rownames(LS_df) <- year_seq

# get total area burned (wildfire) fire histories
WFarea_df <- data.frame(matrix(NA, ncol = length(csv_list), nrow = length(year_seq)))
for (i in 1:length(area_burned_list)){
  fire_hist <- read.csv(area_burned_list[i])
  WFarea_df[,i] <- fire_hist$wildfire_ha #not pct
  fire_hist <- NULL
}
colnames(WFarea_df) <- burned_watersheds
rownames(WFarea_df) <- year_seq

# get total area burned at high-severity
HSarea_df <- data.frame(matrix(NA, ncol = length(csv_list), nrow = length(year_seq)))
for (i in 1:length(area_burned_list)){
  fire_hist <- read.csv(csv_list[i])
  HSarea_df[,i] <- fire_hist$high_severity_pct
  fire_hist <- NULL
}
colnames(HSarea_df) <- burned_watersheds
rownames(HSarea_df) <- year_seq

######################## Regional analysis #############################
# helps to have data frame of lake IDs
lakes_4ha_pts@data$rowID <- rownames(lakes_4ha_pts@data)
rowid_lagos_df <- data.frame(rowID=lakes_4ha_pts@data$rowID, lagoslakeid=lakes_4ha_pts$lagoslakei)

######### Bailey's ecoregions #########
baileys_names <- baileys@data$PROVINCE
# first need number of lakes per ecoregion
# subset points that fall in each ecoregion polygon
# sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
baileys_lagoslakeid <- sp::over(lakes_4ha_pts, baileys, returnList = F)
baileys_lagoslakeid$joinID <- rownames(baileys_lagoslakeid)
baileys_lagoslakeid <- merge(baileys_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
#baileys_lagoslakeid <- baileys_ecoregion_lagoslakeid[,3:4]
# get rid of factor; would cause problems later
baileys_lagoslakeid$lagoslakeid <- as.numeric(levels(baileys_lagoslakeid$lagoslakeid))[baileys_lagoslakeid$lagoslakeid]

# number of burned lakes by ecoregion
baileys_burned <- colSums(gContains(baileys, burned_lakes, byid = T))
setNames(baileys_burned, baileys@data$PROVINCE)

baileys_burned_WF <- colSums(gContains(baileys, burned_lakes_WF, byid = T))
setNames(baileys_burned_WF, baileys@data$PROVINCE)

baileys_burned_Rx <- colSums(gContains(baileys, burned_lakes_Rx, byid = T))
setNames(baileys_burned_Rx, baileys@data$PROVINCE)

baileys_burned_DF <- data.frame(baileys_province=baileys@data$PROVINCE, BurnedLakes = baileys_burned, BurnedLakes_WF=baileys_burned_WF,
                                BurnedLakes_Rx=baileys_burned_Rx)

# proportion of burned lakes by ecoregion (out of total lakes in each ecoregion)
# count number of rows (lagoslakeids, therefore lakes )per unique ecoregion
lake_countz_baileys <- baileys_lagoslakeid %>%
  group_by(PROVINCE) %>%
  tally()
colnames(lake_countz_baileys) <- c("baileys_province","nLakes")
lake_countz_baileys <- merge(lake_countz_baileys, baileys_burned_DF, by="baileys_province",all.x=F)
lake_countz_baileys$PropBurned <- lake_countz_baileys$BurnedLakes/lake_countz_baileys$nLakes
lake_countz_baileys$PropBurned_WF <- lake_countz_baileys$BurnedLakes_WF/lake_countz_baileys$nLakes
lake_countz_baileys$PropBurned_Rx <- lake_countz_baileys$BurnedLakes_Rx/lake_countz_baileys$nLakes
#write.csv(lake_countz_baileys, "ExportedData/baileys_provinces_burned_lakes.csv")

lake_countz_baileys_shp <- merge(baileys, lake_countz_baileys, by.x='PROVINCE', by.y='baileys_province')
tm_shape(lake_countz_baileys_shp)+
  tm_fill('PropBurned', style='fixed', title='Proportion of Lakes Burned',
          breaks=c(0,0.05,0.1,0.2,0.4,0.6, Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

#writeOGR(lake_countz_baileys_shp, dsn='GIS/baileys_ecoregions', layer='baileys_provinces_propBurned', 
#         overwrite_layer = T, driver='ESRI Shapefile')

######### By US State (lower 48) #########
states_names <- states_shp@data$NAME
# first need number of lakes per state
# subset points that fall in each state polygon
# sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
states_lagoslakeid <- sp::over(lakes_4ha_pts, states_shp, returnList = F)
states_lagoslakeid$joinID <- rownames(states_lagoslakeid)
states_lagoslakeid <- merge(states_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
#states_lagoslakeid <- states_lagoslakeid[,3:4]
# get rid of factor; would cause problems later
states_lagoslakeid$lagoslakeid <- as.numeric(levels(states_lagoslakeid$lagoslakeid))[states_lagoslakeid$lagoslakeid]

# number of burned lakes by state
states_burned <- colSums(gContains(states_shp, burned_lakes, byid = T))
setNames(states_burned, states_shp@data$NAME)

states_burned_WF <- colSums(gContains(states_shp, burned_lakes_WF, byid = T))
setNames(states_burned_WF, states_shp@data$NAME)

states_burned_Rx <- colSums(gContains(states_shp, burned_lakes_Rx, byid = T))
setNames(states_burned_Rx, states_shp@data$NAME)

states_burned_DF <- data.frame(BurnedLakes = states_burned, BurnedLakes_WF = states_burned_WF,
                               BurnedLakes_Rx = states_burned_Rx, state=states_shp@data$NAME)

# proportion of burned lakes by state (out of total lakes in each state)
# count number of rows (lagoslakeids, therefore lakes )per unique state
lake_countz_states <- states_lagoslakeid %>%
  group_by(NAME) %>%
  tally()
colnames(lake_countz_states) <- c("state","nLakes")
lake_countz_states <- merge(lake_countz_states, states_burned_DF, by="state", all.x=F)
lake_countz_states$PropBurned <- lake_countz_states$BurnedLakes/lake_countz_states$nLakes
lake_countz_states$PropBurned_WF <- lake_countz_states$BurnedLakes_WF/lake_countz_states$nLakes
lake_countz_states$PropBurned_Rx <- lake_countz_states$BurnedLakes_Rx/lake_countz_states$nLakes
lake_countz_states$OverallRank <- rank(-lake_countz_states$PropBurned)
lake_countz_states$WildfireRank <- rank(-lake_countz_states$PropBurned_WF)
lake_countz_states$RxRank <- rank(-lake_countz_states$PropBurned_Rx)

#write.csv(lake_countz_states, "ExportedData/states_burned_lakes.csv")

state_burned_shp <- merge(states_shp, lake_countz_states, by.x='NAME',by.y='state', all.x=F)

#writeOGR(state_burned_shp, dsn='GIS/US_states', layer='state_prop_lakes_burned', 
#         overwrite_layer = T, driver='ESRI Shapefile')

