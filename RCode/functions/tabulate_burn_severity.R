# create function that returns table of hectares and percent of each severity class
# includes 0s for years without burning
# MTBS burn severity has 6 classes
# 1: unburned to low severity
# 2: low severity; veg mortality 0-20%
# 3: moderate severity; veg mortality 20-70%
# 4: high severity; veg mortality > 70%
# 5: increased greenness; post-fire veg response since image from previous year
# 6: non-processing area; masked out clouds, cloud shadows, waterbodies, Landsat 7 scan line corrector failure
# see: Eidenshink, J., Schwind, B., Brewer, K., Zhu, Z., Quayle, B., & Howard, S. (2007). A project for monitoring trends in burn severity. 
# Fire Ecology 3 (1): 3-21. Fire Ecology Special Issue Vol, 3, 4.
# Or: Odion, D. C., et al. (2014). Examining historical and current mixed-severity fire regimes in 
# ponderosa pine and mixed-conifer forests of western North America. PloS one, 9(2), e87852.
tabulate_burn_severity <- function(extracted, lagoslakeid, year, cell_size_sqm, buffer_area_ha) {
  #extracted: pre-extracted cells from buffer zones
  #lagoslakeid: lake of interest
  #year of interest
  #cell_size_sqm: cell area in square meters
  #buffer_area_ha: buffer area around lake in hectares; do gArea on the output of buffer_erase_lagoslakeid
  #converts from number of cells to hectares and percent of buffer area
  dat <- data.frame(Var1=seq(1,6,1)) #start with empty data frame of all possible values 1-6
  freq <- as.data.frame(table(extracted[[seq(extracted)]]))
  #if actually extracted data, will have a data frame with actual rows in it
  if (nrow(freq) > 0) {
    dat <- merge(dat,freq,by='Var1',all.x=T) #merges with extracted data, nonmatches are NA
    dat$Freq[is.na(dat$Freq)] <- 0 #convert NAs to 0 (indicating none of that class)
    #dat$lagoslakeid <- lagoslakeid
    dat <- as.data.frame(t(dat))[2,]
    colnames(dat) <- paste0('Class',seq(1,6,1))
    dat$Year <- year #create year column
    dat <- dat[,c(7,1,2,3,4,5)] #reorder columns, drop class 6
    dat$Class1_ha <- (dat$Class1*cell_size_sqm)/10000 #multiply number of cells by cell size (sq m), divide by 10000 to get hectares
    dat$Class1_pct <- dat$Class1_ha/buffer_area_ha #divide hectares by buffer area (ha) to get %
    dat$Class2_ha <- (dat$Class2*cell_size_sqm)/10000
    dat$Class2_pct <- dat$Class2_ha/buffer_area_ha
    dat$Class3_ha <- (dat$Class3*cell_size_sqm)/10000
    dat$Class3_pct <- dat$Class3_ha/buffer_area_ha
    dat$Class4_ha <- (dat$Class4*cell_size_sqm)/10000
    dat$Class4_pct <- dat$Class4_ha/buffer_area_ha
    dat$Class5_ha <- (dat$Class5*cell_size_sqm)/10000
    dat$Class5_pct <- dat$Class5_ha/buffer_area_ha
    dat <- dat[,c(1,7:16)]
    #if extracted no data, will have a data frame with no rows in it
    #therefore, create data frame with all 0s except in year column
  } else {
    dat <- data.frame(Year=year,Class1_ha=0,Class1_pct=0,Class2_ha=0,Class2_pct=0,Class3_ha=0,
                      Class3_pct=0,Class4_ha=0,Class4_pct=0,Class5_ha=0,Class5_pct=0)
    #dat$Year = year
  }
  #rename columns intuitively and calculate total hectares and percent burned
  colnames(dat) <- c('Year','unburned_low_ha','unburned_low_pct','low_severity_ha','low_severity_pct',
                     'moderate_severity_ha','moderate_severity_pct','high_severity_ha','high_severity_pct',
                     'increased_greenness_ha','increased_greenness_pct')
  dat$Total_ha <- dat$low_severity_ha + dat$moderate_severity_ha + dat$high_severity_ha
  dat$Total_pct <- dat$low_severity_pct + dat$moderate_severity_pct + dat$high_severity_pct
  return(dat)
}
