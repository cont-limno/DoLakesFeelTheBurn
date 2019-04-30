######################## Fire area burned in lake watersheds of USA ############################
# Date: 1-17-18
# updated: 4-4-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)

#### input data ####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

# All LAGOS GIS data in same coordinate system
# download from repository and unzip
# See: https://lagoslakes.org/products/data-products/ for updates
lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

# MTBS polygon data (Monitoring Trends in Burn Severity) (downloaded Jan 2018: https://www.mtbs.gov/; May 2017 data release)
MTBS_polygon <- shapefile("C:/Ian_GIS/MTBS/mtbs_perims_1984_2015_DD_20170815_LOWER48.shp")
MTBS_polygon <- spTransform(MTBS_polygon, crs(lakes_4ha_all)) #reproject to be same as LAGOS GIS data

#### define constants ####
first_year = 1984 #in MTBS dataset
last_year = 2015

#### load functions ####
source("RCode/functions/zones_that_burned_given_year_fire_type.R") # function that determines how many lakes (by lagoslakeid) that have experienced different fire types
source("RCode/functions/lagoslakeid_fire_history_watershed.R") # function to calculate area burned through time in lake watersheds
source("RCode/functions/zones_that_burned_all.R") #function that determines which lakes (by lagoslakeid) that have experienced fire
source("RCode/functions/annual_area_burn_firetype.R") #calculate area burn by fire type from output of lagoslakeid_fire_history_watershed.R

##################################### Main program ########################################
# need to check for self intersection; GIS functions fail otherwise
# 1500 m buffer to all lakes
Buff1500m_all <- gBuffer(lakes_4ha_all, byid=T, width=1500) #slow!!
MTBS_polygon <- gBuffer(MTBS_polygon, byid=T, width=0) #gIsValid(MTBS_polygon) returned FALSE, so apply 0 m buffer
MTBS_polygon_WF <- subset(MTBS_polygon, FireType =='Wildfire')
MTBS_polygon_Rx <- subset(MTBS_polygon, FireType =='Prescribed Fire')

#### Identify watersheds with fire some time between first and last year ####
# will take many hours, but can save output for use later
# burned_watersheds <- zones_that_burned_all(burn_polygons = MTBS_polygon, zone_shp=Buff1500m_all)
# burned_watersheds_WF <- zones_that_burned_all(burn_polygons = MTBS_polygon_WF, zone_shp=Buff1500m_all)
# burned_watersheds_Rx <- zones_that_burned_all(burn_polygons = MTBS_polygon_Rx, zone_shp=Buff1500m_all)
# 
# burned_watersheds_WF_exp <- burned_watersheds_WF$lagoslakei
# burned_watersheds_Rx_exp <- burned_watersheds_Rx$lagoslakei
# write.csv(burned_watersheds_WF_exp, "ExportedData/Burned1500mBuffs_WF.csv")
# write.csv(burned_watersheds_Rx_exp, "ExportedData/Burned1500mBuffs_Rx.csv")
# write.csv(burned_watersheds, "ExportedData/Burned1500mBuffs.csv")

# if already run/saved, can read in burned lagoslakeids
burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

#### Calculate fire history for all lakes that experienced fire, saving as lagoslakeid-specific csv ####
# seems to take about 10 seconds per lake
# this code works, but already run so commented out

lagoslakeid_vec <- burned_watersheds #if have saved burned lagoslakeids
# lagoslakeid_vec <- lagoslakeid_vec[! lagoslakeid_vec %in% c(219340, 219342)] #manually remove rogue lagoslakeids that shouldn't have been included
# for (i in 1:length(lagoslakeid_vec)){
#   xx <- lagoslakeid_fire_history_watershed(burn_polygons = MTBS_polygon, lagoslakeid = lagoslakeid_vec[i], zone_shp = Buff1500m_all,
#                                            lake_shp = lakes_4ha_all, first_year = first_year, last_year = last_year)
#   outpath <- paste0("ExportedData/lake_fire_history/buffer1500m/","buffer1500m_fire_",lagoslakeid_vec[i],".csv")
#   write.csv(xx, outpath)
#   xx=NULL
# }

#### Count lakes with different fire types by year ####
burn_by_year <- list()
year_vec <- seq(first_year, last_year, 1)

# returns list of lagoslakeids that burned, looping through years
for (i in 1:length(year_vec)) {
  yeer <- zones_that_burned_given_year_fire_type(MTBS_polygon, Buff1500m_all, year_vec[i])
  burn_by_year[[i]] <- yeer
  yeer <- NULL
}

nFires_year <- do.call(rbind.data.frame, burn_by_year)

### temporal trends in area burned by fire type
area_burn_list <- list()
for (i in 1:length(burned_watersheds)){
  xx <- annual_area_burn_firetype(first_year,last_year,burned_watersheds[i])
  area_burn_list[[i]] <- xx
  xx <- NULL
}

annual_area_burn_df <- do.call(cbind, area_burn_list)
annual_WF_df <- annual_area_burn_df[,c(seq(1,ncol(annual_area_burn_df),5))] #subset by WF columns only
annual_Rx_df <- annual_area_burn_df[,c(seq(2,ncol(annual_area_burn_df),5))] #subset by Rx columns only
annual_WLFU_df <- annual_area_burn_df[,c(seq(3,ncol(annual_area_burn_df),5))] #subset by Rx columns only
annual_unknown_df <- annual_area_burn_df[,c(seq(4,ncol(annual_area_burn_df),5))] #subset by Rx columns only

annual_area_burn_type_df <- data.frame(Year=year_vec, WF_ha=rowSums(annual_WF_df, na.rm=T),Rx_ha=rowSums(annual_Rx_df, na.rm=T),
                                       WLFU_ha=rowSums(annual_WLFU_df, na.rm=T),Unknown_ha=rowSums(annual_unknown_df, na.rm=T))
annual_area_burn_type_df$Year <- as.factor(year_vec)
colnames(annual_area_burn_type_df) <- c('Year','Wildfire','Prescribed','WLFU','Unknown')

### Create stacked bar plot showing time series of the number of watersheds that
# experienced different fire types (wildfire, prescribed, wildland fire use, unknown)

# stacked bar with number of fires (in lake watersheds) line
fire_stacked <- as.data.frame(t(annual_area_burn_type_df))
colnames(fire_stacked) <- year_vec
fire_stacked <- fire_stacked[-1,]#delete "Year" row (first row)
hmm <- list() #create temporary list so can convert all factors columns to numeric
hmm <- lapply(fire_stacked, function(x) as.numeric(as.character(x)))
fire_stacked <- as.data.frame(do.call(cbind, hmm))

#### add severity to same plot as area burned
burn_severity <- read.csv('ExportedData/fire_area_severity_year.csv') #pre-calculated
colnames(burn_severity) <- c('class',year_vec)

#png('C:/Ian_GIS/FeelTheBurn/Paper1/Figures/StackedBar_Type_Severity.png',width = 7.5,height = 4.75,units = 'in',res=300)
par(mfrow=c(2,1))
par(mar=c(1,5,2,5)) #bot,left,top,right
lty.o <- par("lty") #get rid of default black borders on stacks 
par(lty = 0) 
barplot(as.matrix(fire_stacked), col=c('firebrick3','dodgerblue','slateblue3','gray'), xlab='', ylab='', 
        las=2, yaxt='n', ylim=c(0,9e5), xaxt='n')
axis(side=2, at=seq(0,9e5,1e5), labels = c(seq(0,900,100)), las=1, cex.axis=0.75)
legend('topleft', legend=c('Wildfire','Prescribed','Wildland Fire Use','Unknown'), pch=c(15,15,15,15),
       col=c('firebrick3','dodgerblue','slateblue3','gray'), bty='n')
#title('Continental US fire types, MTBS')
par(new=T)
par(lty=1)
plot(TotalCount ~ Year, data=nFires_year, type='l', xlab='', ylab='', col='black', ylim=c(0,1800),axes=F, lwd=2, yaxs='i')
axis(side=4, at=seq(0,1800,300),labels=seq(0,1800,300), las=1, cex.axis=0.75)
mtext(side=4,'Number of fires (all types)', line=3)

test <- cbind.data.frame(burn_severity[,1], burn_severity[,2:33]*-1)

par(mar=c(4,5,0,5))
par(lty = 0) 
barplot(as.matrix(test)[,2:33], col=c('tan','gold','orange','saddlebrown'), xlab='', ylab='', 
        las=2, ylim=c(-9e5,0), yaxt='n', cex.names=0.75)
axis(side=2, at=seq(-9e5,0,1e5), labels = c(900,800,700,600,500,400,300,200,100,0), las=1, cex.axis=0.75)
#axis(side=1, at=seq(1,32,2), labels=seq(1984,2014,2), cex.axis=0.75)
legend('bottomleft', legend=c('Unburned to Low','Low','Moderate','High'), pch=c(15,15,15,15),
       col=c('tan','gold','orange','saddlebrown'), bty='n')
#title('Area burned in lake watersheds by fire severity class, MTBS')
#text(x=-1,y=0, "Hectares (thousands)", srt=90)
mtext("Hectares (thousands)", line=2.5, side=2, at=c(0,0))
#dev.off()
