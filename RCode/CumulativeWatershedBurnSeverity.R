######################## cumulative watershed burn severity ###############################################
# Date: 4-11-18
# updated: 7-24-18
# Author: Ian McCullough, immccull@gmail.com
###########################################################################################################

#### R libraries ####
library(ggplot2)
library(reshape2)

setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

######## Main program ##########

# if already run/saved, can read in burned lagoslakeids
burned_watersheds <- read.csv("ExportedData/Burned1500mBuffs.csv")[,2] #reads 2nd column (lagoslakeid)

output_df <- data.frame(lagoslakeid=NA,cumul_UBL_pct=NA,cumul_LS_pct=NA,cumul_MS_pct=NA,cumul_HS_pct=NA,cumul_IG_pct=NA)
for (i in 1:length(burned_watersheds)){
  fire_hist <- read.csv(paste0("ExportedData/lake_fire_history_severity/buffer1500m/buffer1500m_burn_severity_",burned_watersheds[i],".csv"))
  output_df[i,1] <- burned_watersheds[i]
  output_df[i,2] <- sum(fire_hist$unburned_low_pct, na.rm=T)   
  output_df[i,3] <- sum(fire_hist$low_severity_pct, na.rm=T)
  output_df[i,4] <- sum(fire_hist$moderate_severity_pct, na.rm=T)
  output_df[i,5] <- sum(fire_hist$high_severity_pct, na.rm=T)
  output_df[i,6] <- sum(fire_hist$increased_greenness_pct, na.rm=T)
}

par(lty = 1)
hist(output_df$cumul_LS_pct, xlab='Cumulative Prop Burned', main='Low')
hist(output_df$cumul_MS_pct, xlab='Cumulative Prop Burned', main='Moderate')
hist(output_df$cumul_HS_pct, xlab='Cumulative Prop Burned', main='High')

# subset to < 1 so can plot on common axes
LS_sub <- subset(output_df, cumul_LS_pct <= 1)
MS_sub <- subset(output_df, cumul_MS_pct <= 1)
HS_sub <- subset(output_df, cumul_HS_pct <= 1)

common_breaks <- seq(0,1,0.05)
hist(LS_sub$cumul_LS_pct, xlab='Cumulative Prop Burned', main='Low Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks)
hist(MS_sub$cumul_MS_pct, xlab='Cumulative Prop Burned', main='Moderate Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks)
hist(HS_sub$cumul_HS_pct, xlab='Cumulative Prop Burned', main='High Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks)

# overlapping histogram (diff severities on one plot)
hist(LS_sub$cumul_LS_pct, xlab='Cumulative Prop Burned', main='Low Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks, col=rgb(1,0,0,0.5))
hist(MS_sub$cumul_MS_pct, xlab='Cumulative Prop Burned', main='Moderate Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks, add=T, col=rgb(0,0,1,0.5))
hist(HS_sub$cumul_HS_pct, xlab='Cumulative Prop Burned', main='High Severity', xlim=c(0,1), 
     ylim=c(0,8800), breaks=common_breaks, add=T, col=rgb(0,1,1,0.5))

# overlapping histogram with ggplot
fire_melts_data <- melt(output_df, id='lagoslakeid')
fire_melts_data <- subset(fire_melts_data, variable %in% c('cumul_LS_pct','cumul_MS_pct','cumul_HS_pct'))#,'cumul_UBL_pct'))
colnames(fire_melts_data) <- c('lagoslakeid','SeverityClass','PropBurned')
#png("C:/Ian_GIS/FeelTheBurn/Paper1/Figures/SeverityClassHistogramm.png",width = 6,height = 4,units = 'in',res = 300)
ggplot(fire_melts_data, aes(x=PropBurned, fill = SeverityClass)) + geom_histogram(alpha = 0.5, position = 'identity', bins=40) +
  labs(x='Cumulative Proportion Burned', y='Number of Lakes') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_continuous(limits = c(0, 4), breaks=seq(0,4,0.5)) +
  scale_y_continuous(limits = c(0, 600), breaks=seq(0,600,100)) +
  theme(legend.position = c(0.87,0.8), legend.background = element_rect(color='black')) +
  scale_fill_manual(values=c('gray','gold','firebrick4'), labels=c('Low','Moderate','High'), name='Severity Class')
#dev.off()

