######################## Mapping, analyzing watershed fire severity #######################################
# Date: 4-11-18
# updated: 3-27-19
# Author: Ian McCullough, immccull@gmail.com
###########################################################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(Hmisc)
library(rgeos)
library(tmap)
library(mblm)
library(Kendall)
library(rgdal)

# library(LAGOSNE)
# library(rgdal)
# library(mblm)
# library(reshape2)

#### input data ####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")
# All LAGOS GIS data in same coordinate system
# may not be on EDI by time submit...REVISIT before submitting
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
# HUC 4 watersheds
#hu4_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/HU4/HU4.shp")

# other GIS data
states_shp <- shapefile("GIS/US_states/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_pts)) #reproject to LAGOS projection

# Level 3 ecoregions
#ecoregions <- shapefile(paste0(getwd(),"/GIS/level3Ecoregions/us_eco_l3_dissolved.shp")) #(manually dissolved in ArcGIS by US ecoregion name)
#ecoregions <- spTransform(ecoregions, CRSobj = crs(states_shp))

# Ecoregions from National Aquatic Resource Survey (NARS)
NARS_ecoregions <- shapefile("GIS/NARS_ecoregions/NARS_ecoregions.shp")
NARS_ecoregions <- spTransform(NARS_ecoregions, CRSobj = crs(states_shp))

# Bailey's ecoregions
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

######### NARS ecoregions #########
NARS_ecoregion_names <- NARS_ecoregions@data$WSA9_NAME
# first need number of lakes per ecoregion
# subset points that fall in each ecoregion polygon
# sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
NARS_ecoregion_lagoslakeid <- sp::over(lakes_4ha_pts, NARS_ecoregions, returnList = F)
NARS_ecoregion_lagoslakeid$joinID <- rownames(NARS_ecoregion_lagoslakeid)
NARS_ecoregion_lagoslakeid <- merge(NARS_ecoregion_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
#NARS_ecoregion_lagoslakeid <- NARS_ecoregion_lagoslakeid[,3:4]
# get rid of factor; would cause problems later
NARS_ecoregion_lagoslakeid$lagoslakeid <- as.numeric(levels(NARS_ecoregion_lagoslakeid$lagoslakeid))[NARS_ecoregion_lagoslakeid$lagoslakeid]

# number of burned lakes by ecoregion
NARS_ecoregion_burned <- colSums(gContains(NARS_ecoregions, burned_lakes, byid = T))
setNames(NARS_ecoregion_burned, NARS_ecoregions@data$WSA9_NAME)

NARS_ecoregion_burned_WF <- colSums(gContains(NARS_ecoregions, burned_lakes_WF, byid = T))
setNames(NARS_ecoregion_burned_WF, NARS_ecoregions@data$WSA9_NAME)

NARS_ecoregion_burned_Rx <- colSums(gContains(NARS_ecoregions, burned_lakes_Rx, byid = T))
setNames(NARS_ecoregion_burned_Rx, NARS_ecoregions@data$WSA9_NAME)

NARS_ecoregion_burned_DF <- data.frame(NARS_ecoregion=NARS_ecoregions@data$WSA9_NAME, BurnedLakes = NARS_ecoregion_burned, BurnedLakes_WF=NARS_ecoregion_burned_WF,
                                       BurnedLakes_Rx=NARS_ecoregion_burned_Rx)

# proportion of burned lakes by ecoregion (out of total lakes in each ecoregion)
# count number of rows (lagoslakeids, therefore lakes )per unique ecoregion
lake_countz_NARS_ecoregion <- NARS_ecoregion_lagoslakeid %>%
  group_by(WSA9_NAME) %>%
  tally()
colnames(lake_countz_NARS_ecoregion) <- c("NARS_ecoregion","nLakes")
lake_countz_NARS_ecoregion <- merge(lake_countz_NARS_ecoregion, NARS_ecoregion_burned_DF, by="NARS_ecoregion", all.x=F)
lake_countz_NARS_ecoregion$PropBurned <- lake_countz_NARS_ecoregion$BurnedLakes/lake_countz_NARS_ecoregion$nLakes
lake_countz_NARS_ecoregion$PropBurned_WF <- lake_countz_NARS_ecoregion$BurnedLakes_WF/lake_countz_NARS_ecoregion$nLakes
lake_countz_NARS_ecoregion$PropBurned_Rx <- lake_countz_NARS_ecoregion$BurnedLakes_Rx/lake_countz_NARS_ecoregion$nLakes
#write.csv(lake_countz_NARS_ecoregion, "ExportedData/NARS_ecoregions_burned_lakes.csv")

lake_countz_NARS_ecoregion_shp <- merge(NARS_ecoregions, lake_countz_NARS_ecoregion, by.x='WSA9_NAME', by.y='NARS_ecoregion')
tm_shape(lake_countz_NARS_ecoregion_shp)+
  tm_fill('PropBurned', style='fixed', title='Proportion of Lakes Burned',
          breaks=c(0,0.01,0.03,0.05,0.08,0.1, Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

#writeOGR(lake_countz_NARS_ecoregion_shp, dsn='GIS/NARS_ecoregions/NARS_ecoregions_propBurned', layer='NARS_ecoregions_propBurned', 
#         overwrite_layer = T, driver='ESRI Shapefile')

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

####### US Level 3 ecoregions ###########
# ecoregion_names <- ecoregions@data$US_L3NAME
# # first need number of lakes per ecoregion
# # subset points that fall in each ecoregion polygon
# # sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
# ecoreg_lagoslakeid <- sp::over(lakes_4ha_pts, ecoregions, returnList = F)
# ecoreg_lagoslakeid$joinID <- rownames(ecoreg_lagoslakeid)
# ecoreg_lagoslakeid <- merge(ecoreg_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
# ecoreg_lagoslakeid <- ecoreg_lagoslakeid[,3:4]
# # get rid of factor; would cause problems later
# ecoreg_lagoslakeid$lagoslakeid <- as.numeric(levels(ecoreg_lagoslakeid$lagoslakeid))[ecoreg_lagoslakeid$lagoslakeid] 
# 
# # number of burned lakes by ecoregion
# burned_lakes_LS <- subset(burned_lakes, Low > 0)
# burned_lakes_MS <- subset(burned_lakes, Moderate > 0)
# burned_lakes_HS <- subset(burned_lakes, High > 0)
# 
# ecoreg_LS <- colSums(gContains(ecoregions, burned_lakes_LS, byid = T))
# setNames(ecoreg_LS, ecoregions@data$US_L3NAME) 
# Ecoreg_counts_LS <- data.frame(LS_Count = ecoreg_LS, L3Ecoreg=ecoregions@data$US_L3NAME)
# 
# ecoreg_MS <- colSums(gContains(ecoregions, burned_lakes_MS, byid = T))
# setNames(ecoreg_MS, ecoregions@data$US_L3NAME) 
# Ecoreg_counts_MS <- data.frame(MS_Count = ecoreg_MS, L3Ecoreg=ecoregions@data$US_L3NAME)
# 
# ecoreg_HS <- colSums(gContains(ecoregions, burned_lakes_HS, byid = T))
# setNames(ecoreg_HS, ecoregions@data$US_L3NAME) 
# Ecoreg_counts_HS <- data.frame(HS_Count = ecoreg_HS, L3Ecoreg=ecoregions@data$US_L3NAME)
# 
# Ecoreg_counts <- data.frame(Ecoregion=Ecoreg_counts_LS$L3Ecoreg, LS_counts=Ecoreg_counts_LS$LS_Count,
#                             MS_counts=Ecoreg_counts_MS$MS_Count, HS_counts=Ecoreg_counts_HS$HS_Count)
# 
# ####### proportion of lakes burned (by severity class) by ecoregion ###########
# # count number of rows (lagoslakeids, therefore lakes )per unique ecoregion
# lake_countz_ecoreg <- ecoreg_lagoslakeid %>% 
#   group_by(US_L3NAME) %>%
#   tally()
# colnames(lake_countz_ecoreg) <- c("US_L3NAME","nLakes")
# lake_countz_ecoreg <- merge(lake_countz_ecoreg, Ecoreg_counts, by.x='US_L3NAME', by.y='Ecoregion')
# # using total counts of lakes by ecoregion and number of lakes burned by severity class
# # calculate proportion of lakes that experienced each severity class by ecoregion
# lake_countz_ecoreg$PctLakes_LS <- round(lake_countz_ecoreg$LS_counts/lake_countz_ecoreg$nLakes,4)
# lake_countz_ecoreg$PctLakes_MS <- round(lake_countz_ecoreg$MS_counts/lake_countz_ecoreg$nLakes,4)
# lake_countz_ecoreg$PctLakes_HS <- round(lake_countz_ecoreg$HS_counts/lake_countz_ecoreg$nLakes,4)
# 
# #merge lake counts and proportions by ecoregion to ecoregion shapefile
# Ecoreg_countz_shp <- merge(ecoregions, lake_countz_ecoreg, by='US_L3NAME')
# tm_shape(Ecoreg_countz_shp)+
#   tm_fill('PctLakes_HS', style='fixed', title='Proportion of Lakes Burned (HS)',
#           breaks=c(0,0.1,0.2,0.3,0.4,0.5, Inf), textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(Ecoreg_countz_shp)+
#   tm_fill('PctLakes_MS', style='fixed', title='Proportion of Lakes Burned (MS)',
#           breaks=c(0,0.1,0.2,0.3,0.4,0.5, Inf), textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(Ecoreg_countz_shp)+
#   tm_fill('PctLakes_LS', style='fixed', title='Proportion of Lakes Burned (LS)',
#           breaks=c(0,0.1,0.2,0.3,0.4,0.5, Inf), textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# ##### show trend/lack of in high-severity fire by ecoregion through time #######
# # loop through ecoregion names to create data frame with # of lakes with HS fire by ecoregion (rows=years)
# # uses HS, MS and LS fire histories by lagoslakeid generated above
# ecoregion_names <- unique(ecoreg_lagoslakeid$US_L3NAME)
# ecoregion_names <- ecoregion_names[!is.na(ecoregion_names)]#get rid of NA
# ecoregion_HS_df <- data.frame(matrix(NA, ncol = length(ecoregion_names), nrow = length(year_seq)))
# colnames(ecoregion_HS_df) <- ecoregion_names
# rownames(ecoregion_HS_df) <- year_seq
# for (i in 1:length(ecoregion_names)){
#   ecoreg_test <- subset(ecoreg_lagoslakeid, US_L3NAME == ecoregion_names[i])
#   ecoreg_test_lakes <- unique(ecoreg_test$lagoslakeid)
#   HS_df_test <- as.data.frame(t(HS_df))
#   HS_df_test$lagoslakeid <- rownames(HS_df_test)
#   HS_df_test <- subset(HS_df_test, lagoslakeid %in% ecoreg_test_lakes)
#   # building in conditional statement for instances when no HS fire occurs in given ecoregion
#   if (nrow(HS_df_test) > 0) {
#     ecoreg_HS_total <- colSums(HS_df_test != 0, na.rm=T)[1:length(year_seq)]
#   }
#   else {
#     ecoreg_HS_total <- rep(0, length(year_seq))
#   }
#   ecoregion_HS_df[,i] <- ecoreg_HS_total 
#   ecoreg_test <- NULL
#   HS_df_test <- NULL
#   ecoreg_HS_total <- NULL
# }
# ecoregion_HS_df$Year <- year_seq
# 
# ## loop through ecoregion names to create data frame with # of lakes with MS fire by ecoregion (rows=years)
# ecoregion_MS_df <- data.frame(matrix(NA, ncol = length(ecoregion_names), nrow = length(year_seq)))
# colnames(ecoregion_MS_df) <- ecoregion_names
# rownames(ecoregion_MS_df) <- year_seq
# for (i in 1:length(ecoregion_names)){
#   ecoreg_test <- subset(ecoreg_lagoslakeid, US_L3NAME == ecoregion_names[i])
#   ecoreg_test_lakes <- unique(ecoreg_test$lagoslakeid)
#   MS_df_test <- as.data.frame(t(MS_df))
#   MS_df_test$lagoslakeid <- rownames(MS_df_test)
#   MS_df_test <- subset(MS_df_test, lagoslakeid %in% ecoreg_test_lakes)
#   # building in conditional statement for instances when no MS fire occurs in given ecoregion
#   if (nrow(MS_df_test) > 0) {
#     ecoreg_MS_total <- colSums(MS_df_test != 0, na.rm=T)[1:length(year_seq)]
#   }
#   else {
#     ecoreg_MS_total <- rep(0, length(year_seq))
#   }
#   ecoregion_MS_df[,i] <- ecoreg_MS_total 
#   ecoreg_test <- NULL
#   MS_df_test <- NULL
#   ecoreg_MS_total <- NULL
# }
# ecoregion_MS_df$Year <- year_seq
# 
# ## loop through ecoregion names to create data frame with # of lakes with LS fire by ecoregion (rows=years)
# ecoregion_LS_df <- data.frame(matrix(NA, ncol = length(ecoregion_names), nrow = length(year_seq)))
# colnames(ecoregion_LS_df) <- ecoregion_names
# rownames(ecoregion_LS_df) <- year_seq
# for (i in 1:length(ecoregion_names)){
#   ecoreg_test <- subset(ecoreg_lagoslakeid, US_L3NAME == ecoregion_names[i])
#   ecoreg_test_lakes <- unique(ecoreg_test$lagoslakeid)
#   LS_df_test <- as.data.frame(t(LS_df))
#   LS_df_test$lagoslakeid <- rownames(LS_df_test)
#   LS_df_test <- subset(LS_df_test, lagoslakeid %in% ecoreg_test_lakes)
#   # building in conditional statement for instances when no LS fire occurs in given ecoregion
#   if (nrow(LS_df_test) > 0) {
#     ecoreg_LS_total <- colSums(LS_df_test != 0, na.rm=T)[1:length(year_seq)]
#   }
#   else {
#     ecoreg_LS_total <- rep(0, length(year_seq))
#   }
#   ecoregion_LS_df[,i] <- ecoreg_LS_total 
#   ecoreg_test <- NULL
#   LS_df_test <- NULL
#   ecoreg_LS_total <- NULL
# }
# ecoregion_LS_df$Year <- year_seq
# 
# #### Same analysis, but on proportion of lakes in each ecoregion by severity group
# PropLakes_HS_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(ecoregion_names)))
# for (i in 1:length(ecoregion_names)){
#   ecoregion_given <- ecoregion_names[i]
#   nLakes <- subset(lake_countz_ecoreg, US_L3NAME == ecoregion_given)[,2]
#   ecoregion_HS_given <- subset(ecoregion_HS_df, select=ecoregion_given)
#   PropLakes <- ecoregion_HS_given[,1]/nLakes
#   PropLakes_HS_df[,i] <- PropLakes
# }
# colnames(PropLakes_HS_df) <- ecoregion_names
# PropLakes_HS_df$Year <- year_seq # create year column
# 
# PropLakes_MS_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(ecoregion_names)))
# for (i in 1:length(ecoregion_names)){
#   ecoregion_given <- ecoregion_names[i]
#   nLakes <- subset(lake_countz_ecoreg, US_L3NAME == ecoregion_given)[,2]
#   ecoregion_MS_given <- subset(ecoregion_MS_df, select=ecoregion_given)
#   PropLakes <- ecoregion_MS_given[,1]/nLakes
#   PropLakes_MS_df[,i] <- PropLakes
# }
# colnames(PropLakes_MS_df) <- ecoregion_names
# PropLakes_MS_df$Year <- year_seq # create year column
# 
# PropLakes_LS_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(ecoregion_names)))
# for (i in 1:length(ecoregion_names)){
#   ecoregion_given <- ecoregion_names[i]
#   nLakes <- subset(lake_countz_ecoreg, US_L3NAME == ecoregion_given)[,2]
#   ecoregion_LS_given <- subset(ecoregion_LS_df, select=ecoregion_given)
#   PropLakes <- ecoregion_LS_given[,1]/nLakes
#   PropLakes_LS_df[,i] <- PropLakes
# }
# colnames(PropLakes_LS_df) <- ecoregion_names
# PropLakes_LS_df$Year <- year_seq # create year column

######### Fire trends in hexagonal regions across US ###########
# create hexagonal grid for USA
# need to buffer states so portions of USA edges aren't cut off
states_buffer <- gBuffer(states_shp, byid=F, width=200000) #byID=F: acts as dissolving states into single polygon rather than acting on individual states

# first create point grid, which then gets converted to hexagons
hex_points <- spsample(states_buffer, type='hexagonal', cellsize=250000, nsig=2, offset=c(0.5,0.5)) #orig set at 300000
hex_points #view how many points were made

hex_grid <- HexPoints2SpatialPolygons(hex_points, dx=250000) #create hexagon polygons from points

dev.off()
plot(states_shp)
plot(hex_grid, add=T)
plot(hex_points, add=T)

# need data frame of hexagon center coordinates so can convert ot spatial polygons data frame
hex_points_df <- as.data.frame(coordinates(hex_grid))
hex_points_df$ID <- rownames(hex_points_df)
colnames(hex_points_df) <- c('XCor','YCor', 'ID')

# convert spatial polygons to spatial polygons data frame
hex_grid <- SpatialPolygonsDataFrame(Sr=hex_grid, data=hex_points_df, match.ID = T)

# count number of lakes with fire by hexagon
hex_names <- hex_grid@data$ID
# first need number of lakes per ecoregion
#subset points that fall in each ecoregion polygon
# sp::over doesn't retain attribute data from points, so use data frame to join those data back later based on rowid
hex_lagoslakeid <- sp::over(lakes_4ha_pts, hex_grid, returnList = F)
hex_lagoslakeid$joinID <- rownames(hex_lagoslakeid)
hex_lagoslakeid <- merge(hex_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
hex_lagoslakeid <- hex_lagoslakeid[,4:5]
# get rid of factor; would cause problems later
hex_lagoslakeid$lagoslakeid <- as.numeric(levels(hex_lagoslakeid$lagoslakeid))[hex_lagoslakeid$lagoslakeid] 

####### proportion of lakes burned (by severity class) by hexagon ###########
hex_LS <- colSums(gContains(hex_grid, burned_lakes_LS, byid = T))
setNames(hex_LS, hex_grid@data$ID) 
hex_counts_LS <- data.frame(LS_Count = hex_LS, ID=hex_grid@data$ID)

hex_MS <- colSums(gContains(hex_grid, burned_lakes_MS, byid = T))
setNames(hex_MS, hex_grid@data$ID) 
hex_counts_MS <- data.frame(MS_Count = hex_MS, ID=hex_grid@data$ID)

hex_HS <- colSums(gContains(hex_grid, burned_lakes_HS, byid = T))
setNames(hex_HS, hex_grid@data$ID) 
hex_counts_HS <- data.frame(HS_Count = hex_HS, ID=hex_grid@data$ID)

hex_counts <- data.frame(ID=hex_counts_LS$ID, LS_counts=hex_counts_LS$LS_Count,
                         MS_counts=hex_counts_MS$MS_Count, HS_counts=hex_counts_HS$HS_Count)
# count number of rows (lagoslakeids, therefore lakes )per unique hexagon
lake_countz_hex <- hex_lagoslakeid %>% 
  group_by(ID) %>%
  tally()
colnames(lake_countz_hex) <- c("ID","nLakes")
countz_hex <- merge(lake_countz_hex, hex_counts, by='ID')
# using total counts of lakes by ecoregion and number of lakes burned by severity class
# calculate proportion of lakes that experienced each severity class by ecoregion
countz_hex$PctLakes_LS <- round(countz_hex$LS_counts/countz_hex$nLakes,4)
countz_hex$PctLakes_MS <- round(countz_hex$MS_counts/countz_hex$nLakes,4)
countz_hex$PctLakes_HS <- round(countz_hex$HS_counts/countz_hex$nLakes,4)

# map by hexagon
hex_counts_shp <- merge(hex_grid, countz_hex, by='ID')
tm_shape(hex_counts_shp)+
  tm_fill('PctLakes_LS', style='fixed', title='Prop of Burned Lakes',
          breaks=c(0,0.1,0.2,0.3,0.4,Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

##### show trend/lack of in high-severity fire by hexagon through time #######
# loop through hexagon names to create data frame with # of lakes with HS fire by hexagon (rows=years)
# uses HS, MS and LS fire histories by lagoslakeid generated above
hexagon_names <- unique(lake_countz_hex$ID)
hexagon_names <- hexagon_names[!is.na(hexagon_names)]#get rid of NA
hexagon_HS_df <- data.frame(matrix(NA, ncol = length(hexagon_names), nrow = length(year_seq)))
colnames(hexagon_HS_df) <- hexagon_names
rownames(hexagon_HS_df) <- year_seq
for (i in 1:length(hexagon_names)){
  hex_test <- subset(hex_lagoslakeid, ID == hexagon_names[i])
  hex_test_lakes <- unique(hex_test$lagoslakeid)
  HS_df_test <- as.data.frame(t(HS_df))
  HS_df_test$lagoslakeid <- rownames(HS_df_test)
  HS_df_test <- subset(HS_df_test, lagoslakeid %in% hex_test_lakes)
  # building in conditional statement for instances when no HS fire occurs in given hexagon
  if (nrow(HS_df_test) > 0) {
    hex_HS_total <- colSums(HS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hex_HS_total <- rep(0, length(year_seq))
  }
  hexagon_HS_df[,i] <- hex_HS_total 
  hex_test <- NULL
  HS_df_test <- NULL
  hex_HS_total <- NULL
}
hexagon_HS_df$Year <- year_seq

## export shapefile of lakes (points) with 1 or more high-severity fire
HS_points <- as.data.frame(t(HS_df))
HS_points$Total <- rowSums(HS_points)
HS_points$lagoslakeid <- rownames(HS_points)
HS_points <- subset(HS_points, Total > 0)
HS_points_shp <- merge(lakes_4ha_pts, HS_points, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
#plot(states_shp)
#plot(HS_points_shp, add=T, col='red', pch=20)
#writeOGR(HS_points_shp, dsn='C:/Ian_GIS/FeelTheBurn/HS_fire_lakes', layer='HS_fire_lakes', 
#         overwrite_layer = T, driver='ESRI Shapefile')

## loop through hexagon names to create data frame with # of lakes with MS fire by hexagon (rows=years)
hexagon_MS_df <- data.frame(matrix(NA, ncol = length(hexagon_names), nrow = length(year_seq)))
colnames(hexagon_MS_df) <- hexagon_names
rownames(hexagon_MS_df) <- year_seq
for (i in 1:length(hexagon_names)){
  hex_test <- subset(hex_lagoslakeid, ID == hexagon_names[i])
  hex_test_lakes <- unique(hex_test$lagoslakeid)
  MS_df_test <- as.data.frame(t(MS_df))
  MS_df_test$lagoslakeid <- rownames(MS_df_test)
  MS_df_test <- subset(MS_df_test, lagoslakeid %in% hex_test_lakes)
  # building in conditional statement for instances when no MS fire occurs in given hexagon
  if (nrow(MS_df_test) > 0) {
    hex_MS_total <- colSums(MS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hex_MS_total <- rep(0, length(year_seq))
  }
  hexagon_MS_df[,i] <- hex_MS_total 
  hex_test <- NULL
  MS_df_test <- NULL
  hex_MS_total <- NULL
}
hexagon_MS_df$Year <- year_seq

## loop through hexagon names to create data frame with # of lakes with LS fire by hexagon (rows=years)
hexagon_LS_df <- data.frame(matrix(NA, ncol = length(hexagon_names), nrow = length(year_seq)))
colnames(hexagon_LS_df) <- hexagon_names
rownames(hexagon_LS_df) <- year_seq
for (i in 1:length(hexagon_names)){
  hex_test <- subset(hex_lagoslakeid, ID == hexagon_names[i])
  hex_test_lakes <- unique(hex_test$lagoslakeid)
  LS_df_test <- as.data.frame(t(LS_df))
  LS_df_test$lagoslakeid <- rownames(LS_df_test)
  LS_df_test <- subset(LS_df_test, lagoslakeid %in% hex_test_lakes)
  # building in conditional statement for instances when no LS fire occurs in given hexagon
  if (nrow(LS_df_test) > 0) {
    hex_LS_total <- colSums(LS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hex_LS_total <- rep(0, length(year_seq))
  }
  hexagon_LS_df[,i] <- hex_LS_total 
  hex_test <- NULL
  LS_df_test <- NULL
  hex_LS_total <- NULL
}
hexagon_LS_df$Year <- year_seq

#### Same analysis, but on proportion of lakes in each hexagon by severity group
PropLakes_HS_hex_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hexagon_names)))
for (i in 1:length(hexagon_names)){
  hexagon_given <- hexagon_names[i]
  nLakes <- subset(lake_countz_hex, ID == hexagon_given)[,2]
  hexagon_HS_given <- subset(hexagon_HS_df, select=hexagon_given)
  PropLakes <- hexagon_HS_given[,1]/as.numeric(nLakes) #need as.numeric to get plain number from tibble
  PropLakes_HS_hex_df[,i] <- PropLakes
}
colnames(PropLakes_HS_hex_df) <- hexagon_names
PropLakes_HS_hex_df$Year <- year_seq # create year column

PropLakes_MS_hex_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hexagon_names)))
for (i in 1:length(hexagon_names)){
  hexagon_given <- hexagon_names[i]
  nLakes <- subset(lake_countz_hex, ID == hexagon_given)[,2]
  hexagon_MS_given <- subset(hexagon_MS_df, select=hexagon_given)
  PropLakes <- hexagon_MS_given[,1]/as.numeric(nLakes)
  PropLakes_MS_hex_df[,i] <- PropLakes
}
colnames(PropLakes_MS_hex_df) <- hexagon_names
PropLakes_MS_hex_df$Year <- year_seq # create year column

PropLakes_LS_hex_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hexagon_names)))
for (i in 1:length(hexagon_names)){
  hexagon_given <- hexagon_names[i]
  nLakes <- subset(lake_countz_hex, ID == hexagon_given)[,2]
  hexagon_LS_given <- subset(hexagon_LS_df, select=hexagon_given)
  PropLakes <- hexagon_LS_given[,1]/as.numeric(nLakes)
  PropLakes_LS_hex_df[,i] <- PropLakes
}
colnames(PropLakes_LS_hex_df) <- hexagon_names
PropLakes_LS_hex_df$Year <- year_seq # create year column

# Use Theil Sen slope for detecting hexagons with significant increases in proportion of lakes experiencing
# high severity fire
x=PropLakes_HS_hex_df$Year
theilSenProp_df <- data.frame(matrix(NA, nrow = length(hexagon_names), ncol = 3))
for (i in 1:length(hexagon_names)){
  y=PropLakes_HS_hex_df[,i]
  mumble_model <- mblm(y ~ x)
  theilSenProp_df[i,1] <- mumble_model$coefficients[1] #intercept
  theilSenProp_df[i,2] <- mumble_model$coefficients[2] #slope
  MK <- MannKendall(y)
  theilSenProp_df[i,3] <- MK$sl #pvalue
  mumble_model <- NULL
  MK <- NULL
  y <- NULL
}
theilSenProp_df$ID <- hexagon_names
colnames(theilSenProp_df)[1:3] <- c('Intercept','Slope','pval')
# if slope is 0, then have no fires, so make NA
theilSenProp_df$pval <- ifelse(theilSenProp_df$Slope==0, NA, theilSenProp_df$pval)

# join theil sen data frame to hexagon shapefile for mapping
theilSenProp_shp <- merge(hex_grid, theilSenProp_df, by='ID')

tm_shape(theilSenProp_shp)+
  tm_fill('pval', style='fixed', title='Pval',
          breaks=c(0,0.00001,0.00002,0.00003,0.0004,0.0005, Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# export to map in ArcGIS
#writeOGR(theilSenProp_shp, dsn='C:/Ian_GIS/FeelTheBurn/hexagons', layer='theilSenPropHS_shp2', 
#         overwrite_layer = T, driver='ESRI Shapefile')


###### Changing wildfire area burned in lake watersheds? (by hexagon) #######
hexagon_WFarea_df <- data.frame(matrix(NA, ncol = length(hexagon_names), nrow = length(year_seq)))
colnames(hexagon_WFarea_df) <- hexagon_names
rownames(hexagon_WFarea_df) <- year_seq
for (i in 1:length(hexagon_names)){
  hex_test <- subset(hex_lagoslakeid, ID == hexagon_names[i])
  hex_test_lakes <- unique(hex_test$lagoslakeid)
  WF_df_test <- as.data.frame(t(WFarea_df))
  WF_df_test$lagoslakeid <- rownames(WF_df_test)
  WF_df_test <- subset(WF_df_test, lagoslakeid %in% hex_test_lakes)
  # building in conditional statement for instances when no HS fire occurs in given hexagon
  if (nrow(WF_df_test) > 0) {
    hex_WF_total <- colSums(WF_df_test[,1:length(year_seq)], na.rm=T)
  }
  else {
    hex_WF_total <- rep(0, length(year_seq))
  }
  hexagon_WFarea_df[,i] <- hex_WF_total 
  hex_test <- NULL
  WF_df_test <- NULL
  hex_WF_total <- NULL
}
hexagon_WFarea_df$Year <- year_seq

# Theil sen analysis
x=hexagon_WFarea_df$Year
theilSenProp_WF_df <- data.frame(matrix(NA, nrow = length(hexagon_names), ncol = 3))
for (i in 1:length(hexagon_names)){
  y=hexagon_WFarea_df[,i]
  mumble_model <- mblm(y ~ x)
  theilSenProp_WF_df[i,1] <- mumble_model$coefficients[1] #intercept
  theilSenProp_WF_df[i,2] <- mumble_model$coefficients[2] #slope
  MK <- MannKendall(y)
  theilSenProp_WF_df[i,3] <- MK$sl #pvalue
  mumble_model <- NULL
  MK <- NULL
  y <- NULL
}
theilSenProp_WF_df$ID <- hexagon_names
colnames(theilSenProp_WF_df)[1:3] <- c('Intercept','Slope','pval')
# if slope is 0, then have no fires, so make NA
theilSenProp_WF_df$pval <- ifelse(theilSenProp_WF_df$Slope==0, NA, theilSenProp_WF_df$pval)

# join theil sen data frame to hexagon shapefile for mapping
theilSenPropWF_shp <- merge(hex_grid, theilSenProp_WF_df, by='ID')

tm_shape(theilSenPropWF_shp)+
  tm_fill('pval', style='fixed', title='Pval',
          breaks=c(0,0.001,0.05,0.1,0.25,0.5, Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# export to map in ArcGIS
#writeOGR(theilSenPropWF_shp, dsn='C:/Ian_GIS/FeelTheBurn/hexagons', layer='theilSenPropWF_ha', 
#         overwrite_layer = T, driver='ESRI Shapefile')

####### Changing area burned at high severity in lake watersheds within hexagons? #########
hexagon_HSarea_df <- data.frame(matrix(NA, ncol = length(hexagon_names), nrow = length(year_seq)))
colnames(hexagon_HSarea_df) <- hexagon_names
rownames(hexagon_HSarea_df) <- year_seq
for (i in 1:length(hexagon_names)){
  hex_test <- subset(hex_lagoslakeid, ID == hexagon_names[i])
  hex_test_lakes <- unique(hex_test$lagoslakeid)
  HS_df_test <- as.data.frame(t(HSarea_df))
  HS_df_test$lagoslakeid <- rownames(HS_df_test)
  HS_df_test <- subset(HS_df_test, lagoslakeid %in% hex_test_lakes)
  # building in conditional statement for instances when no HS fire occurs in given hexagon
  if (nrow(HS_df_test) > 0) {
    hex_HS_total <- colSums(HS_df_test[,1:length(year_seq)], na.rm=T)
  }
  else {
    hex_HS_total <- rep(0, length(year_seq))
  }
  hexagon_HSarea_df[,i] <- hex_HS_total 
  hex_test <- NULL
  HS_df_test <- NULL
  hex_HS_total <- NULL
}
hexagon_HSarea_df$Year <- year_seq

# Theil sen analysis
x=hexagon_HSarea_df$Year
theilSenProp_HS_df <- data.frame(matrix(NA, nrow = length(hexagon_names), ncol = 3))
for (i in 1:length(hexagon_names)){
  y=hexagon_HSarea_df[,i]
  mumble_model <- mblm(y ~ x)
  theilSenProp_HS_df[i,1] <- mumble_model$coefficients[1] #intercept
  theilSenProp_HS_df[i,2] <- mumble_model$coefficients[2] #slope
  MK <- MannKendall(y)
  theilSenProp_HS_df[i,3] <- MK$sl #pvalue
  mumble_model <- NULL
  MK <- NULL
  y <- NULL
}
theilSenProp_HS_df$ID <- hexagon_names
colnames(theilSenProp_HS_df)[1:3] <- c('Intercept','Slope','pval')
# if slope is 0, then have no fires, so make NA
theilSenProp_HS_df$pval <- ifelse(theilSenProp_HS_df$Slope==0, NA, theilSenProp_HS_df$pval)

# join theil sen data frame to hexagon shapefile for mapping
theilSenPropHS_shp <- merge(hex_grid, theilSenProp_HS_df, by='ID')

tm_shape(theilSenPropHS_shp)+
  tm_fill('pval', style='fixed', title='Pval',
          breaks=c(0,0.001,0.05,0.1,0.25,0.5, Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# export to map in ArcGIS
#writeOGR(theilSenPropHS_shp, dsn='C:/Ian_GIS/FeelTheBurn/hexagons', layer='theilSenPropHS_area', 
#         overwrite_layer = T, driver='ESRI Shapefile')

######### Fire trends in HU4 watersheds across US ###########
# count number of lakes with fire by hu4 watershed
hu4_names <- hu4_shp@data$ZoneID
# first need number of lakes per ecoregion
#subset points that fall in each ecoregion polygon
# sp::over doesn't retain attribute data from points, so use data frame to join those data back later based on rowid
hu4_lagoslakeid <- sp::over(lakes_4ha_pts, hu4_shp, returnList = F)
hu4_lagoslakeid$joinID <- rownames(hu4_lagoslakeid)
hu4_lagoslakeid <- merge(hu4_lagoslakeid, rowid_lagos_df, by.x='joinID', by.y='rowID')
hu4_lagoslakeid <- hu4_lagoslakeid[,14:15]
# get rid of factor; would cause problems later
hu4_lagoslakeid$lagoslakeid <- as.numeric(levels(hu4_lagoslakeid$lagoslakeid))[hu4_lagoslakeid$lagoslakeid] 

hu4_LS <- colSums(gContains(hu4_shp, burned_lakes_LS, byid = T))
setNames(hu4_LS, hu4_shp@data$ZoneID) 
hu4_counts_LS <- data.frame(LS_Count = hu4_LS, ZoneID=hu4_shp@data$ZoneID)

hu4_MS <- colSums(gContains(hu4_shp, burned_lakes_MS, byid = T))
setNames(hu4_MS, hu4_shp@data$ZoneID) 
hu4_counts_MS <- data.frame(MS_Count = hu4_MS, ZoneID=hu4_shp@data$ZoneID)

hu4_HS <- colSums(gContains(hu4_shp, burned_lakes_HS, byid = T))
setNames(hu4_HS, hu4_shp@data$ZoneID) 
hu4_counts_HS <- data.frame(HS_Count = hu4_HS, ZoneID=hu4_shp@data$ZoneID)

hu4_counts <- data.frame(ZoneID=hu4_counts_LS$ZoneID, LS_counts=hu4_counts_LS$LS_Count,
                         MS_counts=hu4_counts_MS$MS_Count, HS_counts=hu4_counts_HS$HS_Count)
hu4_counts_shp <- merge(hu4_shp, hu4_counts, by='ZoneID')

# count number of rows (lagoslakeids, therefore lakes )per unique hu4agon
lake_countz_hu4 <- hu4_lagoslakeid %>% 
  group_by(ZoneID) %>%
  tally()
colnames(lake_countz_hu4) <- c("ZoneID","nLakes")
countz_hu4 <- merge(lake_countz_hu4, hu4_counts, by='ZoneID')
# using total counts of lakes by ecoregion and number of lakes burned by severity class
# calculate proportion of lakes that experienced each severity class by ecoregion
countz_hu4$PctLakes_LS <- round(countz_hu4$LS_counts/countz_hu4$nLakes,4)
countz_hu4$PctLakes_MS <- round(countz_hu4$MS_counts/countz_hu4$nLakes,4)
countz_hu4$PctLakes_HS <- round(countz_hu4$HS_counts/countz_hu4$nLakes,4)

# map by hu4
countz_hu4_shp <- merge(hu4_shp, countz_hu4, by='ZoneID')
tm_shape(countz_hu4_shp)+
  tm_fill('PctLakes_LS', style='fixed', title='Prop of Burned Lakes',
          breaks=c(0,0.025,0.05,0.10,Inf), textNA = 'NA', colorNA = 'gray')+
  tm_borders()

##### show trend/lack of in high-severity fire by hu4 through time #######
# loop through hu4 names to create data frame with # of lakes with HS fire by hu4 (rows=years)
# uses HS, MS and LS fire histories by lagoslakeid generated above
hu4_names <- unique(lake_countz_hu4$ZoneID)
hu4_names <- hu4_names[!is.na(hu4_names)]#get rid of NA
hu4_HS_df <- data.frame(matrix(NA, ncol = length(hu4_names), nrow = length(year_seq)))
colnames(hu4_HS_df) <- hu4_names
rownames(hu4_HS_df) <- year_seq
for (i in 1:length(hu4_names)){
  hu4_test <- subset(hu4_lagoslakeid, ZoneID == hu4_names[i])
  hu4_test_lakes <- unique(hu4_test$lagoslakeid)
  HS_df_test <- as.data.frame(t(HS_df))
  HS_df_test$lagoslakeid <- rownames(HS_df_test)
  HS_df_test <- subset(HS_df_test, lagoslakeid %in% hu4_test_lakes)
  # building in conditional statement for instances when no HS fire occurs in given hu4
  if (nrow(HS_df_test) > 0) {
    hu4_HS_total <- colSums(HS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hu4_HS_total <- rep(0, length(year_seq))
  }
  hu4_HS_df[,i] <- hu4_HS_total 
  hu4_test <- NULL
  HS_df_test <- NULL
  hu4_HS_total <- NULL
}
hu4_HS_df$Year <- year_seq

## loop through hu4 names to create data frame with # of lakes with MS fire by hu4 (rows=years)
hu4_MS_df <- data.frame(matrix(NA, ncol = length(hu4_names), nrow = length(year_seq)))
colnames(hu4_MS_df) <- hu4_names
rownames(hu4_MS_df) <- year_seq
for (i in 1:length(hu4_names)){
  hu4_test <- subset(hu4_lagoslakeid, ZoneID == hu4_names[i])
  hu4_test_lakes <- unique(hu4_test$lagoslakeid)
  MS_df_test <- as.data.frame(t(MS_df))
  MS_df_test$lagoslakeid <- rownames(MS_df_test)
  MS_df_test <- subset(MS_df_test, lagoslakeid %in% hu4_test_lakes)
  # building in conditional statement for instances when no MS fire occurs in given hu4
  if (nrow(MS_df_test) > 0) {
    hu4_MS_total <- colSums(MS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hu4_MS_total <- rep(0, length(year_seq))
  }
  hu4_MS_df[,i] <- hu4_MS_total 
  hu4_test <- NULL
  MS_df_test <- NULL
  hu4_MS_total <- NULL
}
hu4_MS_df$Year <- year_seq

## loop through hu4 names to create data frame with # of lakes with LS fire by hu4 (rows=years)
hu4_LS_df <- data.frame(matrix(NA, ncol = length(hu4_names), nrow = length(year_seq)))
colnames(hu4_LS_df) <- hu4_names
rownames(hu4_LS_df) <- year_seq
for (i in 1:length(hu4_names)){
  hu4_test <- subset(hu4_lagoslakeid, ZoneID == hu4_names[i])
  hu4_test_lakes <- unique(hu4_test$lagoslakeid)
  LS_df_test <- as.data.frame(t(LS_df))
  LS_df_test$lagoslakeid <- rownames(LS_df_test)
  LS_df_test <- subset(LS_df_test, lagoslakeid %in% hu4_test_lakes)
  # building in conditional statement for instances when no LS fire occurs in given hu4
  if (nrow(LS_df_test) > 0) {
    hu4_LS_total <- colSums(LS_df_test != 0, na.rm=T)[1:length(year_seq)]
  }
  else {
    hu4_LS_total <- rep(0, length(year_seq))
  }
  hu4_LS_df[,i] <- hu4_LS_total 
  hu4_test <- NULL
  LS_df_test <- NULL
  hu4_LS_total <- NULL
}
hu4_LS_df$Year <- year_seq

#### Same analysis, but on proportion of lakes in each hu4 by severity group
PropLakes_HS_hu4_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hu4_names)))
for (i in 1:length(hu4_names)){
  hu4_given <- hu4_names[i]
  nLakes <- subset(lake_countz_hu4, ZoneID == hu4_given)[,2]
  hu4_HS_given <- subset(hu4_HS_df, select=hu4_given)
  PropLakes <- hu4_HS_given[,1]/nLakes
  PropLakes_HS_hu4_df[,i] <- PropLakes
}
colnames(PropLakes_HS_hu4_df) <- hu4_names
PropLakes_HS_hu4_df$Year <- year_seq # create year column

PropLakes_MS_hu4_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hu4_names)))
for (i in 1:length(hu4_names)){
  hu4_given <- hu4_names[i]
  nLakes <- subset(lake_countz_hu4, ZoneID == hu4_given)[,2]
  hu4_MS_given <- subset(hu4_MS_df, select=hu4_given)
  PropLakes <- hu4_MS_given[,1]/nLakes
  PropLakes_MS_hu4_df[,i] <- PropLakes
}
colnames(PropLakes_MS_hu4_df) <- hu4_names
PropLakes_MS_hu4_df$Year <- year_seq # create year column

PropLakes_LS_hu4_df <- data.frame(matrix(NA, nrow = length(year_seq), ncol = length(hu4_names)))
for (i in 1:length(hu4_names)){
  hu4_given <- hu4_names[i]
  nLakes <- subset(lake_countz_hu4, ZoneID == hu4_given)[,2]
  hu4_LS_given <- subset(hu4_LS_df, select=hu4_given)
  PropLakes <- hu4_LS_given[,1]/nLakes
  PropLakes_LS_hu4_df[,i] <- PropLakes
}
colnames(PropLakes_LS_hu4_df) <- hu4_names
PropLakes_LS_hu4_df$Year <- year_seq # create year column
