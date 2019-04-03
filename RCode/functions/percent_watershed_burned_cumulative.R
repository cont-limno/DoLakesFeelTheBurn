# calculate % watershed burned by lagoslakeid using pre-calculated data
# designed to be looped through fire history files in ExportedData directory

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