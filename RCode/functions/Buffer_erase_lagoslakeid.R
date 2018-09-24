library(raster)
library(rgeos)
# create function to buffer lakes, erasing lake area
buffer_erase_lagoslakeid <- function(lagoslakeid, lake_shp, buffer_width_m){
  #lagoslakeid: unique ID for each LAGOS lake
  #lake_shp: polygon spatial data frame for lakes
  #buffer_width_m: buffer distance around lakes, meters
  lake_sub <- subset(lake_shp, lagoslakei == lagoslakeid) #subset lake of interest
  lake_buffer <- gBuffer(lake_sub, byid=T, width=buffer_width_m)
  lake_sub <- gBuffer(lake_sub, byid=T, width=0) #guard against invalid geometries
  lake_buffer <- gBuffer(lake_buffer, byid=T, width=0)
  lake_sub <- spTransform(lake_sub, CRSobj = crs(MTBS_stack)) #set to same projection as MTBS (easier than other way around)
  lake_buffer <- spTransform(lake_buffer, CRSobj = crs(MTBS_stack))
  buffer_erased <- raster::erase(lake_buffer, lake_sub) #remove lake itself from buffer zone
  return(buffer_erased)
}