library(tidyverse)
library(lubridate)
library(readr)
library(jagsUI)
library(MCMCpack)
library(arm)

setwd("C:/Users/FWL/Documents/DoLakesFeelTheBurn")

#load data
dat <- read_csv("ExportedData/Noah/hexagon_WildfireArea_ha.csv",col_types = cols(.default="d",Year="i"))
dat <- dat[,-1]

sink("model.txt")
cat("
    model {
    
    # Priors
    a ~ dnorm(0, 0.001)
    b ~ dnorm(0, 0.001)
    tau ~ dgamma( 0.001 , 0.001 )
    
    # Likelihood: 
    for (i in 1:n){
    y[i] ~ dt(mu[i], tau, tdf)               # Distribution for random part
    mu[i] <- a + b * x[i]          # Linear predictor
    } # i
    
    # Derived quantities
    udf ~ dunif(0,1)
    tdf <- 1 - tdfGain * log(1-udf) # tdf in [1,Inf).
    } # end model
    ",fill = TRUE)
sink()

#------------------------------------------------------------------------------
# Loop through Hexegons

slope_trend <- data.frame(region=character(),trend=logical(),slope_prob=numeric(),rhat=numeric())
slope_trend$region = as.character(slope_trend$region)

for(i in 1:(ncol(dat)-1)) {
  #------------------------------------------------------------------------------
  # THE DATA.
  n = nrow(dat)
  x = dat$Year
  xName="year"
  y = dat[[i]]
  yName=names(dat)[i]
  
  # Specify data, as a list.
  tdfGain = 1 # 1 for low-baised tdf, 100 for high-biased tdf
  datalist = list(
    x = x-min(x) ,
    y = log(y+1) , #natural log transform area (+1 for zero data), asin(sqrt) for proportion
    n = n,
    tdfGain = tdfGain
  )
  
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  
  inits <- function (){
    list (a=rnorm(1), b=rnorm(1), tau=runif(1), udf=0.95 )
  }
  #------------------------------------------------------------------------------
  # run model
  
  # Parameters monitored
  parameters <- c("a","b","tau","tdf")
  
  
  # MCMC settings
  ni <- 30000
  nt <- 5
  nb <- 5000
  nc <- 3
  
  out1 <- jags(datalist, inits, parameters, "Model.txt", n.chains = nc, 
               n.thin = nt, n.iter = ni, n.burnin = nb, parallel = T)    
  BugsOut <- out1$summary
  
  #------------------------------------------------------------------------------
  # Calculate slope probabilities
  
  slopes <- out1$sims.list$b
  # sign of slope
  slopeSign <- out1$mean$b > 0     
  # calc
  slopeProbs <- numeric()
  slopeProbs <- mean(slopes > 0)
  # if(slopeSign > 0){
  #     slopeProbs <- mean(slopes > 0)
  # } else {
  #     slopeProbs <- mean(slopes < 0)
  # }
  
  out.stats = matrix(nrow = 1,ncol = 4,data = NA)
  out.stats[1] <- as.character(yName)
  out.stats[2] <- slopeSign
  out.stats[3] <- round(slopeProbs,digits = 4)
  out.stats[4] <-round(BugsOut[2,8],3)
  slope_trend[i,] <- out.stats
  
}
write_csv(x = slope_trend,path = "data/wildfire_area_probs.csv")