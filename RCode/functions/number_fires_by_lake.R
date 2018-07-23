##### counts number of fires by lake (lagoslakeid) based on pre-calculated lake-specific
# fire histories

number_fires_by_lake <- function(lagoslakeid, first_year, last_year, directory_path){
  #lagoslakeid: unique ID of each lake in LAGOS
  #first_year: of study period
  #last_year: of study period
  #directory_path: folder containing fire histories
  history_data <- read.csv(paste0(directory_path, '/buffer1500m_fire_',lagoslakeid,'.csv')) 
  history_data[,1] <- NULL #remove useless index column
  history_data <- subset(history_data, Year >= first_year & Year <= last_year) #subset by years of interest
  nFires_all <- sum(history_data$nTotalFires, na.rm=T)
  nFires_WF <- sum(history_data$nWildfires, na.rm=T)
  nRxfires <- sum(history_data$nRxfires, na.rm=T)
  nWLFU <- sum(history_data$nWLFUfires, na.rm=T)
  nUnknown <- sum(history_data$nUnknownfires, na.rm=T)
  nFires <- data.frame(Total=nFires_all, Wildfire=nFires_WF, Rxfires=nRxfires, WLFU=nWLFU, Unknown=nUnknown)
  return(nFires)
}
