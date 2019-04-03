### temporal trends in area burned by fire type
# Calculates annual wildfire, prescribed, wildland fire use, unknown type and total area burned
# from pre-calculated fire histories (output of lagoslakeid_fire_history_watershed.R)

annual_area_burn_firetype <- function(first_year,last_year,lagoslakeid){
  #first_year: first year of time series
  #last_year: last year of time series
  #lagoslakeid: unique ID for each lake
  lake_fire_history <- read.csv(paste0(getwd(),"/ExportedData/lake_fire_history/buffer1500m/buffer1500m_fire_",burned_watersheds[i],".csv"))
  lake_fire_history <- subset(lake_fire_history, Year>=first_year & Year<=last_year)
  output <- data.frame(WF_ha=lake_fire_history$wildfire_ha, Rx_ha=lake_fire_history$Rxfire_ha,
                       WLFU_ha=lake_fire_history$WLFU_ha, Unknown_ha=lake_fire_history$unknown_fire_ha,
                       Total_ha=lake_fire_history$ZoneTotalBurned_ha)  
  return(output)
}
