### function that determines how many lakes (by lagoslakeid) that have experienced different fire types
# in MTBS data (wildfire, prescribed, wildland fire use, unknown)
# uses intersection of fire polygons and lake watersheds/buffer areas

library(raster)

zones_that_burned_given_year_fire_type <- function(burn_polygons, zone_shp, year){
  #burn_polygons: MTBS polygons
  #zone_shp: polygons of zones in which to calculate area burned; could be watersheds or buffers around lakes
  #function ASSUMES all inputs in same coordinate system
  # Fire_ID column: each fire has state abbrev as prefix; can subset based on that 
  burn_year <- subset(burn_polygons, Year==year)
  burn_area_zone <- suppressWarnings(raster::intersect(zone_shp, burn_year))
  unique_lakeIDs <- burn_area_zone@data[!duplicated(burn_area_zone@data$lagoslakei),]
  unique_lakeIDS_WF <- nrow(subset(unique_lakeIDs, FireType=='Wildfire'))
  unique_lakeIDS_Rx <- nrow(subset(unique_lakeIDs, FireType=='Prescribed Fire'))
  unique_lakeIDS_WLFU <- nrow(subset(unique_lakeIDs, FireType=='Wildland Fire Use'))
  unique_lakeIDS_Unknown <- nrow(subset(unique_lakeIDs, FireType=='Unknown'))
  output <- data.frame(Year=year,WF_count=unique_lakeIDS_WF, Rx_count=unique_lakeIDS_Rx, WLFU_count=unique_lakeIDS_WLFU,
                       Unknown_count=unique_lakeIDS_Unknown)
  output$TotalCount <- output$WF_count + output$Rx_count + output$WLFU_count + output$Unknown_count
  return(output)
}