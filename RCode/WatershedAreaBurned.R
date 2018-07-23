######################## Fire in lake watersheds of USA ########################################
# Date: 1-17-18
# updated: 7-11-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)
library(reshape)
library(ggplot2)
#library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

# All LAGOS GIS data in same coordinate system
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
lakes_4ha_all <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_v0.2.shp")

# MTBS polygon data (Monitoring Trends in Burn Severity)
MTBS_polygon <- shapefile("C:/Ian_GIS/MTBS/mtbs_perims_1984_2015_DD_20170815_LOWER48.shp")
MTBS_polygon <- spTransform(MTBS_polygon, crs(lakes_4ha_pts)) #reproject MTBS data to be same as LAGOS GIS data

# state polygons
states_shp <- shapefile(paste0(getwd(),"/GIS/US_States/lower48.shp"))
states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_pts)) #

state_df <- lakes_4ha_pts@data[,c('lagoslakei','STATE')] #create data frame of states and lagoslakeids
colnames(state_df) <- c('lagoslakeid','STATE')
state_df$StateFac <- as.factor(state_df$STATE)

#### define constants ####
first_year = 1984 #in MTBS dataset
last_year = 2015

#### load functions ####
source("RCode/functions/zones_that_burned_given_year_fire_type.R")
source("RCode/functions/lagoslakeid_fire_history_watershed.R")
source("Rcode/functions/number_fires_by_lake.R")

##################################### Main program ########################################
# need to check for self intersection; GIS functions fail otherwise
# 1500 m buffer to all lakes
Buff1500m_all <- gBuffer(lakes_4ha_all, byid=T, width=1500) #slow!!
MTBS_polygon <- gBuffer(MTBS_polygon, byid=T, width=0) #gIsValid(MTBS_polygon) returned FALSE, so apply 0 m buffer

# identify watersheds with fire some time between first and last year
# will take many hours
#burned_watersheds <- zones_that_burned_all(burn_polygons = MTBS_polygon, zone_shp=Buff1500m_all)

# if already run/saved, can read in burned lagoslakeids
burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

# calculate fire history for all lakes that experienced fire, saving as lagoslakeid-specific csv
# seems to take about 10 seconds per lake
# this code works, but already run so commented out

lagoslakeid_vec <- burned_watersheds #if have saved burned lagoslakeids
# lagoslakeid_vec <- lagoslakeid_vec[! lagoslakeid_vec %in% c(219340, 219342)] #manually remove rogue lagoslakeids that shouldn't have been included
# for (i in 1:length(lagoslakeid_vec)){
#   xx <- lagoslakeid_fire_history_watershed(burn_polygons = MTBS_polygon, lagoslakeid = lagoslakeid_vec[i], zone_shp = Buff1500m_all,
#                                            lake_shp = lakes_4ha_all, first_year = first_year, last_year = last_year)
#   outpath <- paste0("C:/Ian_GIS/FeelTheBurn/lake_fire_history/buffer1500m/","buffer1500m_fire_",lagoslakeid_vec[i],".csv")
#   write.csv(xx, outpath)
#   xx=NULL
# }


## map burned watersheds by year
burn_by_year <- list()
year_vec <- seq(first_year, last_year, 1)

# returns list of lagoslakeids that burned
for (i in 1:length(year_vec)) {
  yeer <- zones_that_burned_given_year_fire_type(MTBS_polygon, Buff1500m_all, year_vec[i])
  burn_by_year[[i]] <- yeer
  yeer <- NULL
}

nFires_year <- do.call(rbind.data.frame, burn_by_year)
test <- nFires_year[,1:5]
test$Year <- as.factor(test$Year)
colnames(test) <- c('Year','Wildfire','Prescribed','WLFU','Unknown')#by default, ggplot uses colnames
melted <- melt(test, id='Year')

# Create stacked area plot showing time series of the number of watersheds that
# experienced different fire types (wildfire, prescribed, wildland fire use, unknown)
years_plot <- rep(year_vec, 4)
ggplot(melted, aes(x=years_plot, y=value, fill=variable))+
  geom_area() + scale_fill_manual(values=c('firebrick','gold','navy','gray')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x='Year', y='Number of watersheds with fire') +
  scale_x_discrete(limits=c(year_vec), position = 'bottom') +
  scale_y_continuous(breaks=seq(0,1800,300))+
  #scale_y_discrete(limits=seq(0,1800,300))+
  theme(axis.text.x=element_text(angle=70, hjust=1))+
  labs(title='MTBS, 1984-2015')
  
##### map exposure of lakes to fire by number of fires experienced by each lake ######
# from calculated fire histories
nFires_by_lake <- data.frame(lagoslakeid=lagoslakeid_vec, Total=NA, Wildfire=NA, Rxfires=NA, WLFU=NA, Unknown=NA)
# loop trips if lagoslakeids are included in vec but don't have fire histories in file system
for (i in 1:length(lagoslakeid_vec)){
  xx <- number_fires_by_lake(lagoslakeid = lagoslakeid_vec[i], first_year = first_year, last_year=last_year,
                             directory_path = paste0(getwd(),"/ExportedData/lake_fire_history/buffer1500m"))
  nFires_by_lake[i,2:6] <- xx
  xx <- NULL
}

lakes_nFires <- merge(lakes_4ha_pts, nFires_by_lake, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

# create data frame for stubborn ggplot
x_df <- as.data.frame(coordinates(lakes_nFires)[,1:2])
colnames(x_df) <- c('xCor', 'yCor')
x_df$Total <- lakes_nFires@data$Total
x_df$lagoslakeid <- lakes_nFires@data$lagoslakei

nFire.point <- ggplot(x_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=x_df$Total), size=2) +
  ggtitle('Number of Fires')
nFire.point$labels$colour = 'n Fires' # change legend title
nFire.point + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  scale_color_gradient(low='lightgoldenrod', high='firebrick')

# where are there lots of fires
nFires_cutoff <- 5
nFire.point2 <- ggplot(subset(x_df, Total >= nFires_cutoff), aes(x=xCor,y=yCor))+
  geom_point(aes(colour=subset(x_df, Total >= nFires_cutoff)$Total), size=2) +
  ggtitle('Number of Fires')
nFire.point2$labels$colour = 'n Fires' # change legend title
nFire.point2 + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  scale_color_gradient(low='lightgoldenrod', high='firebrick')

hist(nFires_by_lake$Total, main='Number of fires by lake', xlab='Number of fires', las=1,
     xlim=c(0,20), freq=F)

#writeOGR(lakes_nFires, dsn='C:/Ian_GIS/FeelTheBurn/nFires_by_lake', layer='nFires_by_lake', 
#         overwrite_layer = T, driver='ESRI Shapefile')

## trends in fire size in lake watersheds?
fire_size_list <- list()
for (i in 1:length(burned_watersheds)){
  lake_fire_history <- read.csv(paste0(getwd(),"/ExportedData/lake_fire_history/buffer1500m/buffer1500m_fire_",burned_watersheds[i],".csv"))
  allfire_size <- data.frame(lake_fire_history$ZoneTotalBurned_ha)#$WholeFire_ha
  colnames(allfire_size) <- burned_watersheds[i]
  rownames(allfire_size) <- lake_fire_history$Year
  fire_size_list[[i]] <- allfire_size
}

fire_size_df <- do.call(cbind, fire_size_list)
fire_size_df_line <- data.frame(Year=as.numeric(rownames(fire_size_df)), mean_ha=rowMeans(fire_size_df, na.rm=T)) 
plot(mean_ha ~ Year, fire_size_df_line, type='l', col='firebrick', lwd=2, 
     ylab='hectares', main='Mean area burned in lake watersheds')
lm_fire_size <- lm(mean_ha ~ Year, fire_size_df_line)
p.val <- round(anova(lm_fire_size)$'Pr(>F)'[1], digits=3)
corcoef <- round(cor(fire_size_df_line$mean_ha, fire_size_df_line$Year),3)
legend('topleft', legend=paste0('p = ',p.val), bty = "n")
legend('topright', legend = paste0('r = ', corcoef), bty='n')
abline(lm_fire_size)

fire_size_df_2plot <- cbind(data.frame(Year=as.factor(year_vec)), fire_size_df)
fire_size_df_2plot <- melt(fire_size_df_2plot, id="Year")
boxplot(fire_size_df_2plot$value ~ fire_size_df_2plot$Year, outline=F)
test <- subset(fire_size_df_2plot, value>0)
boxplot(test$value ~ test$Year, outline=F, ylim=c(0,2000), ylab='hectares', las=3,
        main='Area burned in lake watersheds')#, whisklty=0, staplelty=0)
summary(lm(test$value~as.numeric(test$Year)))

### temporal trends in area burned by fire type
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
colnames(annual_area_burn_type_df) <- c('Year','Wildfire','Prescribed','WLFU','Unknown')#by default, ggplot uses colnames
melted2 <- melt(annual_area_burn_type_df, id='Year')

# Create stacked bar plot showing time series of the number of watersheds that
# experienced different fire types (wildfire, prescribed, wildland fire use, unknown)

# stacked bar with number of fires (in lake watersheds) line
fire_stacked <- as.data.frame(t(annual_area_burn_type_df))
colnames(fire_stacked) <- year_vec
fire_stacked <- fire_stacked[-1,]#delete "Year" row (first row)
hmm <- list() #create temporary list so can convert all factors columns to numeric
hmm <- lapply(fire_stacked, function(x) as.numeric(as.character(x)))
fire_stacked <- as.data.frame(do.call(cbind, hmm))

#### add severity to same plot as area burned
# calculated and saved to disk in "MappingAnalyzingFireSeverity.R" ("fire_area_stacked" data frame)
burn_severity <- read.csv('ExportedData/fire_area_severity_year.csv')
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
#title('Conterminous USA fire types, MTBS')
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
