######################## Comparing buffer, watershed and lake size ##############################
# Date: 2-1-19
# updated: 3-27-19
# Author: Ian McCullough, immccull@gmail.com
#################################################################################################

setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

#### R libraries ####
library(LAGOSNE)
library(raster)
library(rgeos)

#### input data ####

# All LAGOS GIS data in same coordinate system
#states_shp <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp")
#lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")
lakes_4ha_all_US <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

#states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_all))

# read in burned lagoslakeids
#burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

#### define constants ####
#first_year = 1984 #in MTBS dataset
#last_year = 2015

########################## Main program ########################
#lakes_4ha_sub <- subset(lakes_4ha_all, lagoslakei %in% burned_watersheds)
#Buff1500m_all_burned <- gBuffer(lakes_4ha_sub, byid=T, width=1500)
#Buff1500m_all_burned <- gBuffer(lakes_4ha_all, byid=T, width=1500)
Buff1500m_all <- gBuffer(lakes_4ha_all, byid=T, width=1500)
Buff1500m_all_area <- gArea(Buff1500m_all, byid=T)
buff_df <- data.frame(lagoslakeid=Buff1500m_all@data$lagoslakei, buff_ha=(Buff1500m_all_area/10000))

Buff1500m_all_US <- gBuffer(lakes_4ha_all_US, byid=T, width=1500) #slow!!

# get LAGOS watersheds for NE states
dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects
iws_ha <- data.frame(lagoslakeid=dt$iws$lagoslakeid, iws_ha=dt$iws$iws_ha, lakearea_ha=dt$iws$iws_lakeareaha)

# get master data frame for plotting/correlation analysis
buff_iws_df <- merge(buff_df, iws_ha, all.x=F, by='lagoslakeid')
buff_iws_df$buff_ha_minuslake <- buff_iws_df$buff_ha - buff_iws_df$lakearea_ha
buff_iws_df$iws_ha_minuslake <- buff_iws_df$iws_ha - buff_iws_df$lakearea_ha

cor(buff_iws_df[,2:6], method='pearson')
summary(buff_iws_df[,2:6])

#jpeg('C:/Ian_GIS/FeelTheBurn/Paper1/Figures/buff_iws_lake_area.jpeg',width = 6,height = 4,units = 'in',res=300) 
  par(mfrow=c(1,2))
  # buffer size vs. iws size
  plot(buff_iws_df$buff_ha ~ buff_iws_df$iws_ha, pch=20, xlim=c(0,20000), ylim=c(0,20000),
      xlab='Watershed area (ha)', ylab='Buffer area (ha)', main='LAGOS NE, 51000 lakes')
  abline(0,1)
  cor_plot <- round(cor(buff_iws_df$buff_ha, buff_iws_df$iws_ha, method='pearson'),2)
  legend('topleft', paste0("r = ", cor_plot), bty='n')

  # buffer size vs. lake size
  plot(buff_iws_df$buff_ha ~ buff_iws_df$lakearea_ha, pch=20, xlim=c(), ylim=c(),
     xlab='Lake area (ha)', ylab='Buffer area (ha)', main='LAGOS NE, 51000 lakes')
  abline(0,1)
  cor_plot2 <- round(cor(buff_iws_df$buff_ha, buff_iws_df$lakearea_ha, method='pearson'),2)
  legend('topleft', paste0("r = ", cor_plot2), bty='n')
#dev.off()

## If subtract lake size from buffers and iws
par(mfrow=c(1,2))
# buffer size vs. iws size
plot(buff_iws_df$buff_ha_minuslake ~ buff_iws_df$iws_ha_minuslake, pch=20, xlim=c(0,20000), ylim=c(0,20000),
      xlab='Watershed area (ha)', ylab='Buffer area (ha)', main='LAGOS NE, 51000 lakes')
abline(0,1)
cor_plot3 <- round(cor(buff_iws_df$buff_ha_minuslake, buff_iws_df$iws_ha_minuslake, method='pearson'),2)
legend('topleft', paste0("r = ", cor_plot3), bty='n')
  
# buffer size vs. lake size
plot(buff_iws_df$buff_ha_minuslake ~ buff_iws_df$lakearea_ha, pch=20, xlim=c(), ylim=c(),
      xlab='Lake area (ha)', ylab='Buffer area (ha)', main='LAGOS NE, 51000 lakes')
abline(0,1)
cor_plot4 <- round(cor(buff_iws_df$buff_ha_minuslake, buff_iws_df$lakearea_ha, method='pearson'),2)
legend('topleft', paste0("r = ", cor_plot4), bty='n')
  
# Frequency distributions
par(mfrow=c(1,2))
hist(buff_iws_df$buff_ha, breaks=seq(0,250000,100), xlim=c(0,10000), main='1500m buffer areas', xlab='Hectares',
     ylim=c(0,20000))
mtext(side=3, 'LAGOS-NE, 51000 lakes')
hist(buff_iws_df$iws_ha, main='watershed areas', xlim=c(0,10000), breaks=seq(0,3300000,100), xlab='Hectares',
     ylim=c(0,20000))
mtext(side=3, 'LAGOS-NE, 51000 lakes')

## all US lakes
Buff1500m_all_area_US <- gArea(Buff1500m_all_US, byid=T)
buff_allUS_df <- data.frame(lagoslakeid=Buff1500m_all_US@data$lagoslakei, lakearea_ha=Buff1500m_all_US@data$Hectares,
                            buff_ha=Buff1500m_all_area_US)  
cor(buff_allUS_df$buff_ha, buff_allUS_df$lakearea_ha, method='pearson', use='pairwise.complete.obs')

# buffer size vs. lake size
par(mfrow=c(1,1))
plot(buff_allUS_df$buff_ha ~ buff_allUS_df$lakearea_ha, pch=20, xlim=c(), ylim=c(),
     xlab='Lake area (ha)', ylab='Buffer area (ha)', main='LAGOS US, 137000 lakes')
#abline(0,1)
cor_plot2 <- round(cor(buff_allUS_df$buff_ha, buff_allUS_df$lakearea_ha, method='pearson', use='pairwise.complete.obs'),2)
legend('topleft', paste0("r = ", cor_plot2), bty='n')
