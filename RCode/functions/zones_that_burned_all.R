### function that determines which lakes (by lagoslakeid) that have experienced fire
# uses intersection of fire polygons and lake watersheds/buffer areas

library(raster)

zones_that_burned_all = function(burn_polygons, zone_shp){
  #burn_polygons: MTBS polygons
  #zone_shp: polygons of zones in which to calculate area burned; could be watersheds or buffers around lakes
  #function ASSUMES all inputs in same coordinate system
  # Fire_ID column: each fire has state abbrev as prefix; can subset based on that 
  burn_area_zone = suppressWarnings(raster::intersect(zone_shp, burn_polygons))
  unique_lakeIDs = burn_area_zone@data[!duplicated(burn_area_zone@data$lagoslakei),]
  unique_lakeIDS = unique(unique_lakeIDs$lagoslakei)
  return(unique_lakeIDs)
}