## function to calculate area burned through time in lake watersheds
# uses intersection between fire and watershed polygons
# watershed polygons can be substituted by lake buffer polygons
# lake polygons erased from watershed polygons
# updated 2-28-18

library(raster)
library(rgeos)
library(dplyr)

lagoslakeid_fire_history_watershed <- function(burn_polygons, lagoslakeid, zone_shp, lake_shp, first_year, last_year){
  #burn_polygons: MTBS polygons
  #lagoslakeid: individual lake ID in LAGOS
  #zone_shp: polygons of zones in which to calculate area burned; could be watersheds or buffers around lakes, must have lagoslakeid
  #lake shp: polygons of lakes, must have lagoslakeid
  #first_year: year 1 of fire history in MTBS
  #last_year: last year of fire history in MTBS
  #function ASSUMES all inputs in same coordinate system
  zone_of_interest <- subset(zone_shp, lagoslakei == lagoslakeid)
  zone_of_interest <- gBuffer(zone_of_interest, width=0, byid=T)# guard against invalid geometries (check with gIsValid())
  lake_of_interest <- subset(lake_shp, lagoslakei == lagoslakeid)
  lake_of_interest <- gBuffer(lake_of_interest, width=0, byid=T) #but why have overlap when there's only one? Weird shape not digitized well?
  zone_erased <- raster::erase(zone_of_interest, lake_of_interest) #remove lake itself from zone
  zone_area_ha <- gArea(zone_erased)/10000 #divide by 10000 to get hectares
  # make calculations for all fire types
  totalburn_in_zone <- suppressWarnings(raster::intersect(zone_erased, burn_polygons)) #get fires in zone of interest
  totalburn_in_zone <- totalburn_in_zone[order(totalburn_in_zone@data$Year),] #sort Year column oldest to newest
  areas = gArea(totalburn_in_zone, byid=T) #calculate area of each intersected iws/fire
  totalburn_in_zone@data$Area <- areas/10000 #crs uses meters, so must convert to hectares
  year_df <- data.frame(Year = seq(first_year, last_year, 1))
  annual_totals <- aggregate(as.numeric(totalburn_in_zone@data$Area) ~ totalburn_in_zone@data$Year, FUN=sum, na.rm=T)
  colnames(annual_totals) <- c('Year','ZoneTotalBurned_ha')
  #annual_totals$IgnitionDate <- as.character(totalburn_in_zone@data$ig_date)
  annual_totalburn <- aggregate(as.numeric(totalburn_in_zone@data$Acres)*0.404686 ~ totalburn_in_zone@data$Year, FUN=sum, na.rm=T)#converting acres to hectares
  annual_totals$WholeFire_ha <- annual_totalburn[,2]#second column is the area column calculated in previous line
  annual_totals$nTotalFires <- aggregate(totalburn_in_zone@data$Area ~ totalburn_in_zone@data$Year, FUN=length)[,2]
  #annual_totals$WholeFire_pct <- annual_totals$WholeFire_ha/zone_area_ha
  lake_fire_history = full_join(year_df, annual_totals, by='Year')
  lake_fire_history[is.na(lake_fire_history)] <- 0 #replace NAs with 0 (to indicate 0 burned area)
  lake_fire_history$ZoneTotalBurned_pct <- (lake_fire_history$ZoneTotalBurned_ha/zone_area_ha)# divide by area of selected watershed, converted to ha
  lake_fire_history$ZoneTotalBurned_ha_cum <- cumsum(ifelse(is.na(lake_fire_history$ZoneTotalBurned_ha), 0, lake_fire_history$ZoneTotalBurned_ha)) + lake_fire_history$ZoneTotalBurned_ha*0
  lake_fire_history$ZoneTotalBurned_pct_cum <- cumsum(ifelse(is.na(lake_fire_history$ZoneTotalBurned_pct), 0, lake_fire_history$ZoneTotalBurned_pct)) + lake_fire_history$ZoneTotalBurned_pct*0
  lake_fire_history <- lake_fire_history[,c(1,2,5,6,7,3,4)]
  
  # calculations for wildfire only
  # returns NA columns if no fires found
  wildfire <- subset(burn_polygons, FireType=='Wildfire')
  wildfire_in_zone <- suppressWarnings(raster::intersect(zone_erased, wildfire)) #get fires in zone of interest
  if (is.null(wildfire_in_zone) == TRUE) {
    lake_fire_history_WF <- data.frame(Year=year_df$Year, wildfire_ha=NA, wildfire_pct=NA, wildfire_ha_cum=NA,
                                       wildfire_pct_cum=NA, wildfire_IgDate=NA, Wholewildfire_ha=NA, nWildfires=NA)
  } else {
    wildfire_in_zone <- wildfire_in_zone[order(wildfire_in_zone@data$Year),] #sort Year column oldest to newest
    areas <- gArea(wildfire_in_zone, byid=T) #calculate area of each intersected iws/fire
    wildfire_in_zone@data$Area <- areas/10000 #crs uses meters, so must convert to hectares
    annual_totals_WF <- aggregate(as.numeric(wildfire_in_zone@data$Area) ~ wildfire_in_zone@data$Year, FUN=sum, na.rm=T)
    colnames(annual_totals_WF) <- c('Year','wildfire_ha')
    # create data frame with only one row per year; taking first fire date of each year
    tump <- wildfire_in_zone@data[!duplicated(wildfire_in_zone@data$Year),]
    annual_totals_WF$wildfire_IgDate <- as.character(tump$ig_date)
    annual_totals_WF$Wholewildfire_ha <- aggregate(as.numeric(wildfire_in_zone@data$Acres) *0.404686 ~wildfire_in_zone@data$Year, FUN=sum, na.rm=T)[,2] #convert original fire acreage to hectares
    annual_totals_WF$nWildfires <- aggregate(wildfire_in_zone@data$Area ~ wildfire_in_zone@data$Year, FUN=length)[,2]
    #annual_totals_WF$Wholewildfire_pct <- annual_totals_WF$wildfire_ha/zone_area_ha
    lake_fire_history_WF <- full_join(year_df, annual_totals_WF, by='Year')
    lake_fire_history_WF[is.na(lake_fire_history_WF)] <- 0 #replace NAs with 0 (to indicate 0 burned area)
    lake_fire_history_WF$wildfire_pct <- (lake_fire_history_WF$wildfire_ha/zone_area_ha)# divide by area of selected watershed, converted to ha
    lake_fire_history_WF$wildfire_ha_cum <- cumsum(ifelse(is.na(lake_fire_history_WF$wildfire_ha), 0, lake_fire_history_WF$wildfire_ha)) + lake_fire_history_WF$wildfire_ha*0
    lake_fire_history_WF$wildfire_pct_cum <- cumsum(ifelse(is.na(lake_fire_history_WF$wildfire_pct), 0, lake_fire_history_WF$wildfire_pct)) + lake_fire_history_WF$wildfire_pct*0
    lake_fire_history_WF <- lake_fire_history_WF[,c(1,2,6,7,8,3,4,5)]
  }
  
  # calculations for prescribed (Rx) burns only
  # returns NA columns if no fires found
  Rxfire <- subset(burn_polygons, FireType=='Prescribed Fire')
  Rxfire_in_zone <- suppressWarnings(raster::intersect(zone_erased, Rxfire)) #get fires in zone of interest
  if (is.null(Rxfire_in_zone) == TRUE) {
    lake_fire_history_Rx <- data.frame(Year=year_df$Year, Rxfire_ha=NA, Rxfire_pct=NA, Rxfire_ha_cum=NA,
                                       Rxfire_pct_cum=NA, Rxfire_IgDate=NA, WholeRxfire_ha=NA, nRxfires=NA)
  } else {
  Rxfire_in_zone <- Rxfire_in_zone[order(Rxfire_in_zone@data$Year),] #sort Year column oldest to newest
  areas <- gArea(Rxfire_in_zone, byid=T) #calculate area of each intersected iws/fire
  Rxfire_in_zone@data$Area <- areas/10000 #crs uses meters, so must convert to hectares
  annual_totals_Rx <- aggregate(as.numeric(Rxfire_in_zone@data$Area) ~ Rxfire_in_zone@data$Year, FUN=sum, na.rm=T)
  colnames(annual_totals_Rx) <- c('Year','Rxfire_ha')
  # create data frame with only one row per year; taking first fire date of each year
  tump <- Rxfire_in_zone@data[!duplicated(Rxfire_in_zone@data$Year),]
  annual_totals_Rx$Rxfire_IgDate <- as.character(tump$ig_date)
  annual_totals_Rx$WholeRxfire_ha <- aggregate(as.numeric(Rxfire_in_zone@data$Acres) *0.404686 ~Rxfire_in_zone@data$Year, FUN=sum, na.rm=T)[,2] #convert original fire acreage to hectares
  annual_totals_Rx$nRxfires <- aggregate(Rxfire_in_zone@data$Area ~ Rxfire_in_zone@data$Year, FUN=length)[,2]
  #annual_totals_Rx$WholeRxfire_pct <- annual_totals_Rx$Rxfire_ha/zone_area_ha
  lake_fire_history_Rx <- full_join(year_df, annual_totals_Rx, by='Year')
  lake_fire_history_Rx[is.na(lake_fire_history_Rx)] <- 0 #replace NAs with 0 (to indicate 0 burned area)
  lake_fire_history_Rx$Rxfire_pct <- (lake_fire_history_Rx$Rxfire_ha/zone_area_ha)# divide by area of selected watershed, converted to ha
  lake_fire_history_Rx$Rxfire_ha_cum <- cumsum(ifelse(is.na(lake_fire_history_Rx$Rxfire_ha), 0, lake_fire_history_Rx$Rxfire_ha)) + lake_fire_history_Rx$Rxfire_ha*0
  lake_fire_history_Rx$Rxfire_pct_cum <- cumsum(ifelse(is.na(lake_fire_history_Rx$Rxfire_pct), 0, lake_fire_history_Rx$Rxfire_pct)) + lake_fire_history_Rx$Rxfire_pct*0
  lake_fire_history_Rx <- lake_fire_history_Rx[,c(1,2,6,7,8,3,4,5)]
  }
  
  # calculations for wildland fire use only (i.e., managed wildfire)
  # returns NA columns if no fires found
  WLFU <- subset(burn_polygons, FireType=='Wildland Fire Use')
  WLFU_in_zone <- suppressWarnings(raster::intersect(zone_erased, WLFU)) #get fires in zone of interest
  if (is.null(WLFU_in_zone) == TRUE) {
    lake_fire_history_WLFU <- data.frame(Year=year_df$Year, WLFU_ha=NA, WLFU_pct=NA, WLFU_ha_cum=NA,
                                         WLFU_pct_cum=NA, WLFU_IgDate=NA, WholeWLFU_ha=NA, nWLFUfires=NA)
  } else {
    WLFU_in_zone <- WLFU_in_zone[order(WLFU_in_zone@data$Year),] #sort Year column oldest to newest
    areas <- gArea(WLFU_in_zone, byid=T) #calculate area of each intersected iws/fire
    WLFU_in_zone@data$Area <- areas/10000 #crs uses meters, so must convert to hectares
    annual_totals_WLFU <- aggregate(as.numeric(WLFU_in_zone@data$Area) ~ WLFU_in_zone@data$Year, FUN=sum, na.rm=T)
    colnames(annual_totals_WLFU) <- c('Year','WLFU_ha')
    # create data frame with only one row per year; taking first fire date of each year
    tump <- WLFU_in_zone@data[!duplicated(WLFU_in_zone@data$Year),]
    annual_totals_WLFU$WLFU_IgDate <- as.character(tump$ig_date)
    annual_totals_WLFU$WholeWLFU_ha <- aggregate(as.numeric(WLFU_in_zone@data$Acres) *0.404686 ~WLFU_in_zone@data$Year, FUN=sum, na.rm=T)[,2] #convert original fire acreage to hectares
    annual_totals_WLFU$nWLFUfires <- aggregate(WLFU_in_zone@data$Area ~ WLFU_in_zone@data$Year, FUN=length)[,2]
    #annual_totals_WLFU$WholeWLFU_pct <- annual_totals_WLFU$WLFU_ha/zone_area_ha
    lake_fire_history_WLFU <- full_join(year_df, annual_totals_WLFU, by='Year')
    lake_fire_history_WLFU[is.na(lake_fire_history_WLFU)] <- 0 #replace NAs with 0 (to indicate 0 burned area)
    lake_fire_history_WLFU$WLFU_pct <- (lake_fire_history_WLFU$WLFU_ha/zone_area_ha)# divide by area of selected watershed, converted to ha
    lake_fire_history_WLFU$WLFU_ha_cum <- cumsum(ifelse(is.na(lake_fire_history_WLFU$WLFU_ha), 0, lake_fire_history_WLFU$WLFU_ha)) + lake_fire_history_WLFU$WLFU_ha*0
    lake_fire_history_WLFU$WLFU_pct_cum <- cumsum(ifelse(is.na(lake_fire_history_WLFU$WLFU_pct), 0, lake_fire_history_WLFU$WLFU_pct)) + lake_fire_history_WLFU$WLFU_pct*0
    lake_fire_history_WLFU <- lake_fire_history_WLFU[,c(1,2,6,7,8,3,4,5)]
  }
  
  # calculations for unknown only (i.e., type of fire not known)
  # returns NA columns if no fires found
  unknown_fire <- subset(burn_polygons, FireType=='Unknown')
  unknown_fire_in_zone <- suppressWarnings(raster::intersect(zone_erased, unknown_fire)) #get fires in zone of interest
  if (is.null(unknown_fire_in_zone) == TRUE) {
    lake_fire_history_unknown_fire <- data.frame(Year=year_df$Year, unknown_fire_ha=NA, unknown_fire_pct=NA, unknown_fire_ha_cum=NA,
                                                 unknown_fire_pct_cum=NA, unknown_fire_IgDate=NA, Wholeunknown_fire_ha=NA, nUnknownfires=NA)
  } else {
    unknown_fire_in_zone <- unknown_fire_in_zone[order(unknown_fire_in_zone@data$Year),] #sort Year column oldest to newest
    areas <- gArea(unknown_fire_in_zone, byid=T) #calculate area of each intersected iws/fire
    unknown_fire_in_zone@data$Area <- areas/10000 #crs uses meters, so must convert to hectares
    annual_totals_unknown_fire <- aggregate(as.numeric(unknown_fire_in_zone@data$Area) ~ unknown_fire_in_zone@data$Year, FUN=sum, na.rm=T)
    colnames(annual_totals_unknown_fire) <- c('Year','unknown_fire_ha')
    # create data frame with only one row per year; taking first fire date of each year
    tump <- unknown_fire_in_zone@data[!duplicated(unknown_fire_in_zone@data$Year),]
    annual_totals_unknown_fire$unknown_fire_IgDate <- as.character(tump$ig_date)
    annual_totals_unknown_fire$Wholeunknown_fire_ha <- aggregate(as.numeric(unknown_fire_in_zone@data$Acres) *0.404686 ~unknown_fire_in_zone@data$Year, FUN=sum, na.rm=T)[,2] #convert original fire acreage to hectares
    annual_totals_unknown_fire$nUnknownfires <- aggregate(unknown_fire_in_zone@data$Area ~ unknown_fire_in_zone@data$Year, FUN=length)[,2]
    #annual_totals_unknown_fire$Wholeunknown_fire_pct <- annual_totals_unknown_fire$unknown_fire_ha/zone_area_ha
    lake_fire_history_unknown_fire <- full_join(year_df, annual_totals_unknown_fire, by='Year')
    lake_fire_history_unknown_fire[is.na(lake_fire_history_unknown_fire)] <- 0 #replace NAs with 0 (to indicate 0 burned area)
    lake_fire_history_unknown_fire$unknown_fire_pct <- (lake_fire_history_unknown_fire$unknown_fire_ha/zone_area_ha)# divide by area of selected watershed, converted to ha
    lake_fire_history_unknown_fire$unknown_fire_ha_cum <- cumsum(ifelse(is.na(lake_fire_history_unknown_fire$unknown_fire_ha), 0, lake_fire_history_unknown_fire$unknown_fire_ha)) + lake_fire_history_unknown_fire$unknown_fire_ha*0
    lake_fire_history_unknown_fire$unknown_fire_pct_cum <- cumsum(ifelse(is.na(lake_fire_history_unknown_fire$unknown_fire_pct), 0, lake_fire_history_unknown_fire$unknown_fire_pct)) + lake_fire_history_unknown_fire$unknown_fire_pct*0
    lake_fire_history_unknown_fire <- lake_fire_history_unknown_fire[,c(1,2,6,7,8,3,4,5)]
  }
  lake_fire_history_combined <- cbind.data.frame(lake_fire_history, lake_fire_history_WF, lake_fire_history_Rx, lake_fire_history_WLFU, lake_fire_history_unknown_fire)
  lake_fire_history_combined[,c(8,16,24,32)] <- NULL #get rid of duplicate Year columns
  return(lake_fire_history_combined)
}