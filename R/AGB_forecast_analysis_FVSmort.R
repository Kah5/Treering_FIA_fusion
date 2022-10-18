library(ggplot2)
library(reshape2)
library(tidyverse)
library(rgdal)

# reading in the available forecasts (1-184 I believe)

get_biomass_ests <- function(plot, mort.scheme, scenario){
  
  load(paste0("forecasts_AGB/biomass_data_FVSmort/plot2AGB_",mort.scheme,".", as.character(plot),".",scenario, ".Rdata"))
  #load("biomass_data_FVSmort/plot2AGB_DIDD.2449653010690.rcp26.Rdata")
  # objects
  # out, AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
  # AGB.foliage, NPP.foliage, 
  # AGB.stembark, NPP.stembark,
  # AGB.stemwood, NPP.stemwood,
  # AGB.branchdead, NPP.branchdead,
  # AGB.branchlive, NPP.branchlive,
  i <- 1
  mplot <- 1
  nt <- ncol(NPP[i,,])
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  mAGB.dead <- sAGB.dead <- mAGB <- sAGB <- mAGB.stemwood <- mAGB.stembark <- mAGB.branchlive <- mAGB.branchdead <- mAGB.foliage<- sAGB.stemwood <- sAGB.stembark <- sAGB.branchlive <- sAGB.branchdead <- sAGB.foliage<- matrix(NA, mplot, nt)
  mNPP.dead<- sNPP.dead <- mNPP <- sNPP <- mNPP.stemwood <-  mNPP.stembark <-  mNPP.branchlive <-  mNPP.branchdead<- mNPP.foliage <-  sNPP.stemwood <-  sNPP.stembark <-  sNPP.branchlive <-  sNPP.branchdead <-  sNPP.foliage<- matrix(NA, mplot,nt)
  lowAGB <- lowAGB.stemwood <- lowAGB.stembark <- lowAGB.branchlive <- lowAGB.branchdead <- lowAGB.foliage<- hiAGB <- hiAGB.stemwood <- hiAGB.stembark <- hiAGB.branchlive <- hiAGB.branchdead <- hiAGB.foliage <- lowAGB.dead<- hiAGB.dead  <- matrix(NA, mplot, nt)
  hiNPP <- hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead <- hiNPP.foliage <- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
  
  
  
  mNPP[i, ] <- apply( NPP[i, , ], 2, median, na.rm = TRUE)
  sNPP[i, ] <- apply(NPP[i, , ], 2, sd, na.rm = TRUE)
  lowNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB[i, ] <- apply(AGB[i, , ], 2, median, na.rm = TRUE)
  sAGB[i, ] <- apply(AGB[i, , ], 2, sd, na.rm = TRUE)
  lowAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  
  # branch dead
  mNPP.branchdead[i, ] <- apply( NPP.branchdead[i, , ], 2, median, na.rm = TRUE)
  sNPP.branchdead[i, ] <- apply(NPP.branchdead[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, median, na.rm = TRUE)
  sAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, sd, na.rm = TRUE)
  lowAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # foliage
  mNPP.foliage[i, ] <- apply( NPP.foliage[i, , ] , 2, median, na.rm = TRUE)
  sNPP.foliage[i, ] <- apply(NPP.foliage[i, , ] , 2, sd, na.rm = TRUE)
  lowNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, mean, na.rm = TRUE)
  sAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # stembark
  mNPP.stembark[i, ] <- apply( NPP.stembark[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stembark[i, ] <- apply(NPP.stembark[i, , ] , 2, sd, na.rm = TRUE)
  lowNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, median, na.rm = TRUE)
  sAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # branchlive
  mNPP.branchlive[i, ] <- apply( NPP.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sNPP.branchlive[i, ] <- apply(NPP.branchlive[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  # stemwood
  mNPP.stemwood[i, ] <- apply( NPP.stemwood[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stemwood[i, ] <- apply(NPP.stemwood[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ], 2, median, na.rm = TRUE)
  sAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  lowAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  # dead trees:
  # stemwood
  mNPP.dead[i, ] <- apply( NPP.dead[i, , ] , 2, median, na.rm = TRUE)
  sNPP.dead[i, ] <- apply(NPP.dead[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.dead[i, ] <- apply(AGB.dead[i, , ], 2, median, na.rm = TRUE)
  sAGB.dead[i, ] <- apply(AGB.dead[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  lowAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  up  <- hiNPP
  low <- lowNPP
  
  up.deadstem  <- hiNPP.dead
  low.deadstem <- lowNPP.dead
  
  up.stemwood  <- hiNPP.stemwood
  low.stemwood <- lowNPP.stemwood
  
  up.stembark  <- hiNPP.stembark
  low.stembark <- lowNPP.stembark
  
  up.branchlive  <- hiNPP.branchlive
  low.branchlive  <- hiNPP.branchlive
  
  up.branchdead  <- hiNPP.branchdead
  low.branchdead  <- hiNPP.branchdead
  
  up.foliage  <- hiNPP.foliage
  low.foliage   <- lowNPP.foliage
  
  # plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
  # lines(yrvec[-1], up)
  # lines(yrvec[-1], low)
  upA  <- hiAGB
  lowA <- lowAGB
  
  upA.deadstem  <- hiAGB.dead
  lowA.deadstem <- lowAGB.dead
  
  upA.stemwood  <- hiAGB.stemwood
  lowA.stemwood <- lowAGB.stemwood
  
  upA.stembark  <- hiAGB.stembark
  lowA.stembark <- lowAGB.stembark
  
  upA.branchlive  <- hiAGB.branchlive
  lowA.branchlive  <- lowAGB.branchlive
  
  upA.branchdead  <- hiAGB.branchdead
  lowA.branchdead  <- lowAGB.branchdead
  
  upA.foliage  <- hiAGB.foliage
  lowA.foliage   <- lowAGB.foliage
  
  # plot(yrvec, mAGB[i, ], ylim = range(c(upA, lowA)), ylab = "Mg/ha", xlab = "year")
  # lines(yrvec, upA)
  # lines(yrvec, lowA)
  # }
  #grDevices::dev.off()
  
  # make nicer plots for each plot:
  i <- 1
  # calculate upper and lower bounds
  up  <- hiNPP
  low <- lowNPP
  
  upA  <- hiAGB
  lowA <- lowAGB
  
  total.plot <- data.frame(plot = plot, 
                           mort.scheme = mort.scheme, 
                           rcp = scenario,
                           year = yrvec[2:length(low.stemwood)], 
                           mAGB = mAGB[i,2:length(low.stemwood)], 
                           mAGB.stemwood = mAGB.stemwood[i,2:length(low.stemwood)],
                           mAGB.stembark = mAGB.stembark[i,2:length(low.stemwood)],
                           mAGB.branchlive = mAGB.branchlive[i,2:length(low.stemwood)],
                           mAGB.branchdead = mAGB.branchdead[i,2:length(low.stemwood)],
                           mAGB.foliage = mAGB.foliage[i,2:length(low.stemwood)],
                           mAGB.dead  = mAGB.dead[i,2:length(low.stemwood)],
                           
                           upA = upA[2:length(low.stemwood)], 
                           lowA = lowA[2:length(low.stemwood)], 
                           upA.stemwood = upA.stemwood[2:length(low.stemwood)],
                           upA.stembark = upA.stembark[2:length(low.stemwood)],
                           upA.branchlive = upA.branchlive[2:length(low.stemwood)],
                           upA.branchdead = upA.branchdead[2:length(low.stemwood)],
                           upA.foliage = upA.foliage[2:length(low.stemwood)],
                           upA.dead = upA.deadstem[2:length(low.stemwood)], 
                           
                           lowA.stemwood = lowA.stemwood[2:length(low.stemwood)],
                           lowA.stembark = lowA.stembark[2:length(low.stemwood)],
                           lowA.branchlive = lowA.branchlive[2:length(low.stemwood)],
                           lowA.branchdead = lowA.branchdead[2:length(low.stemwood)],
                           lowA.foliage = lowA.foliage[2:length(low.stemwood)],
                           lowA.dead = lowA.deadstem[2:length(low.stemwood)], 
                           
                           mNPP = mNPP[i,2:length(low.stemwood)], 
                           mNPP.stemwood = mNPP.stemwood[2:length(low.stemwood)],
                           mNPP.stembark =mNPP.stembark[2:length(low.stemwood)],
                           mNPP.branchlive =mNPP.branchlive[2:length(low.stemwood)],
                           mNPP.branchdead = mNPP.branchdead[2:length(low.stemwood)],
                           mNPP.foliage = mNPP.foliage[2:length(low.stemwood)],  
                           mNPP.dead = mNPP.dead[2:length(low.stemwood)], 
                           
                           up = up[2:length(low.stemwood)], 
                           low = low[2:length(low.stemwood)], 
                           
                           up.dead = up.deadstem[2:length(low.stemwood)], 
                           low.dead = low.deadstem[2:length(low.stemwood)],
                           
                           up.stemwood = up.stemwood[2:length(low.stemwood)],
                           up.stembark = up.stembark[2:length(low.stemwood)],
                           up.branchlive = up.branchlive[2:length(low.stemwood)],
                           up.branchdead =  up.branchdead[2:length(low.stemwood)],
                           up.foliage =up.foliage[2:length(low.stemwood)],
                           
                           low.stemwood = low.stemwood[2:length(low.stemwood)],
                           low.stembark = low.stembark[2:length(low.stemwood)],
                           low.branchlive = low.branchlive[2:length(low.stemwood)],
                           low.branchdead = low.branchdead[2:length(low.stemwood)],
                           low.foliage = low.foliage[2:length(low.stemwood)])
  
  
  total.plot
}


get_biomass_ests_ncc <- function(plot, mort.scheme, scenario){
  
  load(paste0("forecasts_AGB/biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  
  # objects
  # out, AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
  # AGB.foliage, NPP.foliage, 
  # AGB.stembark, NPP.stembark,
  # AGB.stemwood, NPP.stemwood,
  # AGB.branchdead, NPP.branchdead,
  # AGB.branchlive, NPP.branchlive,
  i <- 1
  mplot <- 1
  nt <- ncol(NPP[i,,])
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  mAGB.dead <- sAGB.dead <- mAGB <- sAGB <- mAGB.stemwood <- mAGB.stembark <- mAGB.branchlive <- mAGB.branchdead <- mAGB.foliage<- sAGB.stemwood <- sAGB.stembark <- sAGB.branchlive <- sAGB.branchdead <- sAGB.foliage<- matrix(NA, mplot, nt)
  mNPP.dead<- sNPP.dead <- mNPP <- sNPP <- mNPP.stemwood <-  mNPP.stembark <-  mNPP.branchlive <-  mNPP.branchdead<- mNPP.foliage <-  sNPP.stemwood <-  sNPP.stembark <-  sNPP.branchlive <-  sNPP.branchdead <-  sNPP.foliage<- matrix(NA, mplot,nt)
  lowAGB <- lowAGB.stemwood <- lowAGB.stembark <- lowAGB.branchlive <- lowAGB.branchdead <- lowAGB.foliage<- hiAGB <- hiAGB.stemwood <- hiAGB.stembark <- hiAGB.branchlive <- hiAGB.branchdead <- hiAGB.foliage <- lowAGB.dead<- hiAGB.dead  <- matrix(NA, mplot, nt)
  hiNPP <- hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead <- hiNPP.foliage <- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
  
  
  
  mNPP[i, ] <- apply( NPP[i, , ], 2, median, na.rm = TRUE)
  sNPP[i, ] <- apply(NPP[i, , ], 2, sd, na.rm = TRUE)
  lowNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB[i, ] <- apply(AGB[i, , ], 2, median, na.rm = TRUE)
  sAGB[i, ] <- apply(AGB[i, , ], 2, sd, na.rm = TRUE)
  lowAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  
  # branch dead
  mNPP.branchdead[i, ] <- apply( NPP.branchdead[i, , ], 2, median, na.rm = TRUE)
  sNPP.branchdead[i, ] <- apply(NPP.branchdead[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, median, na.rm = TRUE)
  sAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, sd, na.rm = TRUE)
  lowAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # foliage
  mNPP.foliage[i, ] <- apply( NPP.foliage[i, , ] , 2, median, na.rm = TRUE)
  sNPP.foliage[i, ] <- apply(NPP.foliage[i, , ] , 2, sd, na.rm = TRUE)
  lowNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, mean, na.rm = TRUE)
  sAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # stembark
  mNPP.stembark[i, ] <- apply( NPP.stembark[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stembark[i, ] <- apply(NPP.stembark[i, , ] , 2, sd, na.rm = TRUE)
  lowNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, median, na.rm = TRUE)
  sAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  # branchlive
  mNPP.branchlive[i, ] <- apply( NPP.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sNPP.branchlive[i, ] <- apply(NPP.branchlive[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  lowAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  # stemwood
  mNPP.stemwood[i, ] <- apply( NPP.stemwood[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stemwood[i, ] <- apply(NPP.stemwood[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ], 2, median, na.rm = TRUE)
  sAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  lowAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  # dead trees:
  # stemwood
  mNPP.dead[i, ] <- apply( NPP.dead[i, , ] , 2, median, na.rm = TRUE)
  sNPP.dead[i, ] <- apply(NPP.dead[i, , ], 2, sd, na.rm = TRUE)
  lowNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  mAGB.dead[i, ] <- apply(AGB.dead[i, , ], 2, median, na.rm = TRUE)
  sAGB.dead[i, ] <- apply(AGB.dead[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  lowAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
  hiAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
  
  
  up  <- hiNPP
  low <- lowNPP
  
  up.deadstem  <- hiNPP.dead
  low.deadstem <- lowNPP.dead
  
  up.stemwood  <- hiNPP.stemwood
  low.stemwood <- lowNPP.stemwood
  
  up.stembark  <- hiNPP.stembark
  low.stembark <- lowNPP.stembark
  
  up.branchlive  <- hiNPP.branchlive
  low.branchlive  <- hiNPP.branchlive
  
  up.branchdead  <- hiNPP.branchdead
  low.branchdead  <- hiNPP.branchdead
  
  up.foliage  <- hiNPP.foliage
  low.foliage   <- lowNPP.foliage
  
  # plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
  # lines(yrvec[-1], up)
  # lines(yrvec[-1], low)
  upA  <- hiAGB
  lowA <- lowAGB
  
  upA.deadstem  <- hiAGB.dead
  lowA.deadstem <- lowAGB.dead
  
  upA.stemwood  <- hiAGB.stemwood
  lowA.stemwood <- lowAGB.stemwood
  
  upA.stembark  <- hiAGB.stembark
  lowA.stembark <- lowAGB.stembark
  
  upA.branchlive  <- hiAGB.branchlive
  lowA.branchlive  <- lowAGB.branchlive
  
  upA.branchdead  <- hiAGB.branchdead
  lowA.branchdead  <- lowAGB.branchdead
  
  upA.foliage  <- hiAGB.foliage
  lowA.foliage   <- lowAGB.foliage
  
  # plot(yrvec, mAGB[i, ], ylim = range(c(upA, lowA)), ylab = "Mg/ha", xlab = "year")
  # lines(yrvec, upA)
  # lines(yrvec, lowA)
  # }
  #grDevices::dev.off()
  
  # make nicer plots for each plot:
  i <- 1
  # calculate upper and lower bounds
  up  <- hiNPP
  low <- lowNPP
  
  upA  <- hiAGB
  lowA <- lowAGB
  
  total.plot <- data.frame(plot = plot, 
                           mort.scheme = mort.scheme, 
                           rcp = paste("no climate change", scenario),
                           year = yrvec[2:length(low.stemwood)], 
                           mAGB = mAGB[i,2:length(low.stemwood)], 
                           mAGB.stemwood = mAGB.stemwood[i,2:length(low.stemwood)],
                           mAGB.stembark = mAGB.stembark[i,2:length(low.stemwood)],
                           mAGB.branchlive = mAGB.branchlive[i,2:length(low.stemwood)],
                           mAGB.branchdead = mAGB.branchdead[i,2:length(low.stemwood)],
                           mAGB.foliage = mAGB.foliage[i,2:length(low.stemwood)],
                           mAGB.dead  = mAGB.dead[i,2:length(low.stemwood)],
                           
                           upA = upA[2:length(low.stemwood)], 
                           lowA = lowA[2:length(low.stemwood)], 
                           upA.stemwood = upA.stemwood[2:length(low.stemwood)],
                           upA.stembark = upA.stembark[2:length(low.stemwood)],
                           upA.branchlive = upA.branchlive[2:length(low.stemwood)],
                           upA.branchdead = upA.branchdead[2:length(low.stemwood)],
                           upA.foliage = upA.foliage[2:length(low.stemwood)],
                           upA.dead = upA.deadstem[2:length(low.stemwood)], 
                           
                           lowA.stemwood = lowA.stemwood[2:length(low.stemwood)],
                           lowA.stembark = lowA.stembark[2:length(low.stemwood)],
                           lowA.branchlive = lowA.branchlive[2:length(low.stemwood)],
                           lowA.branchdead = lowA.branchdead[2:length(low.stemwood)],
                           lowA.foliage = lowA.foliage[2:length(low.stemwood)],
                           lowA.dead = lowA.deadstem[2:length(low.stemwood)], 
                           
                           mNPP = mNPP[i,2:length(low.stemwood)], 
                           mNPP.stemwood = mNPP.stemwood[2:length(low.stemwood)],
                           mNPP.stembark =mNPP.stembark[2:length(low.stemwood)],
                           mNPP.branchlive =mNPP.branchlive[2:length(low.stemwood)],
                           mNPP.branchdead = mNPP.branchdead[2:length(low.stemwood)],
                           mNPP.foliage = mNPP.foliage[2:length(low.stemwood)],  
                           mNPP.dead = mNPP.dead[2:length(low.stemwood)], 
                           
                           up = up[2:length(low.stemwood)], 
                           low = low[2:length(low.stemwood)], 
                           
                           up.dead = up.deadstem[2:length(low.stemwood)], 
                           low.dead = low.deadstem[2:length(low.stemwood)],
                           
                           up.stemwood = up.stemwood[2:length(low.stemwood)],
                           up.stembark = up.stembark[2:length(low.stemwood)],
                           up.branchlive = up.branchlive[2:length(low.stemwood)],
                           up.branchdead =  up.branchdead[2:length(low.stemwood)],
                           up.foliage =up.foliage[2:length(low.stemwood)],
                           
                           low.stemwood = low.stemwood[2:length(low.stemwood)],
                           low.stembark = low.stembark[2:length(low.stemwood)],
                           low.branchlive = low.branchlive[2:length(low.stemwood)],
                           low.branchdead = low.branchdead[2:length(low.stemwood)],
                           low.foliage = low.foliage[2:length(low.stemwood)])
  
  
  total.plot
}


# list files and get all the plot numbers:

filenames <- list.files(path = "forecasts_AGB/biomass_data_FVSmort26all/", pattern = "rcp26")

# get the plot numbers that were run
plotnos <- do.call(rbind, stringr::str_split(filenames, pattern = "\\."))[,2]
unique(plotnos)


get_biomass_ests(plot = unique(plotnos)[1], mort.scheme = "DIDD",scenario = "rcp26")

# check that 151 plots were also run for the no climate change scenario
filenames.nocc <- list.files(path = "forecasts_AGB/biomass_data_nocc", pattern = "rcp26")
plotnos.nocc <- do.call(rbind, stringr::str_split(filenames, pattern = "\\."))[,2]
unique(plotnos.nocc)


# -------------------------------------------------------------------------------
# read in forecasts for all scenarios and mortality conditions
# -------------------------------------------------------------------------------
# RCP8.5
normort.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp85")
DIonly.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp85")
DDonly.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp85")
DIDD.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp85")

# RCP 60:
normort.AGB.60 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp60")
DIonly.AGB.60 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp60")
DDonly.AGB.60 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp60")
DIDD.AGB.60 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp60")


# RCP 4.5:
normort.AGB.45 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp45")
DIonly.AGB.45 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp45")
DDonly.AGB.45 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp45")
DIDD.AGB.45 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp45")


# RCP 2.6:
normort.AGB.26 <- lapply(unique(plotnos), FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp26")
DIonly.AGB.26 <- lapply(unique(plotnos), FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp26")
DDonly.AGB.26 <- lapply(unique(plotnos), FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp26")
DIDD.AGB.26 <- lapply(unique(plotnos), FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp26")



# no climate change scenarios:
nocc.nomort.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests_ncc, mort.scheme = "nomort", scenario = "rcp26")
nocc.DIonly.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests_ncc, mort.scheme = "DIonly", scenario = "rcp26")
nocc.DDonly.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests_ncc, mort.scheme = "DDonly", scenario = "rcp26")
nocc.DIDD.AGB <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests_ncc, mort.scheme = "DIDD", scenario = "rcp26")

normort.AGB.df <- do.call(rbind, normort.AGB)
DIonly.AGB.df <- do.call(rbind, DIonly.AGB)
DDonly.AGB.df <- do.call(rbind, DDonly.AGB)
DIDD.AGB.df <- do.call(rbind, DIDD.AGB)

# rcp60:
normort.AGB.60.df <- do.call(rbind, normort.AGB.60)
DIonly.AGB.60.df <- do.call(rbind, DIonly.AGB.60)
DDonly.AGB.60.df <- do.call(rbind, DDonly.AGB.60)
DIDD.AGB.60.df <- do.call(rbind, DIDD.AGB.60)

# rcp45
normort.AGB.45.df <- do.call(rbind, normort.AGB.45)
DIonly.AGB.45.df <- do.call(rbind, DIonly.AGB.45)
DDonly.AGB.45.df <- do.call(rbind, DDonly.AGB.45)
DIDD.AGB.45.df <- do.call(rbind, DIDD.AGB.45)

# rcp26
normort.AGB.26.df <- do.call(rbind, normort.AGB.26)
DIonly.AGB.26.df <- do.call(rbind, DIonly.AGB.26)
DDonly.AGB.26.df <- do.call(rbind, DDonly.AGB.26)
DIDD.AGB.26.df <- do.call(rbind, DIDD.AGB.26)


nocc.nomort.AGB.df <- do.call(rbind, nocc.nomort.AGB)
nocc.DIonly.AGB.df <- do.call(rbind, nocc.DIonly.AGB)
nocc.DDonly.AGB.df <- do.call(rbind, nocc.DDonly.AGB )
nocc.DIDD.AGB.df <- do.call(rbind, nocc.DIDD.AGB)


# -------------------------------------------------------------------------------
# combine all forecasts
# -------------------------------------------------------------------------------
all10plots <- rbind(normort.AGB.df, DIonly.AGB.df, DDonly.AGB.df, DIDD.AGB.df, 
                    normort.AGB.60.df, DIonly.AGB.60.df, DDonly.AGB.60.df, DIDD.AGB.60.df, 
                    normort.AGB.45.df, DIonly.AGB.45.df, DDonly.AGB.45.df, DIDD.AGB.45.df, 
                    normort.AGB.26.df, DIonly.AGB.26.df, DDonly.AGB.26.df, DIDD.AGB.26.df, 
                    nocc.nomort.AGB.df, nocc.DIonly.AGB.df, nocc.DDonly.AGB.df, nocc.DIDD.AGB.df)
all10plots <- DIDD.AGB.26.df

ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
  summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)



added.up.tenplot.summary <- ten.plot.summary %>% group_by(mort.scheme, rcp, year) %>% 
  mutate(Total.biomass.high =  upA.stemwood + upA.stembark + upA.foliage + upA.branchlive + upA.branchdead + upA.dead, 
         Total.biomass.low =  lowA.stemwood + lowA.stembark + lowA.foliage + lowA.branchlive + lowA.branchdead + lowA.dead, 
         A.stemwood.high = Total.biomass.high,
         A.stemwood.low = Total.biomass.high - lowA.stemwood, 
         A.livebranch.high = A.stemwood.low, 
         A.livebranch.low = Total.biomass.high - lowA.stemwood - lowA.branchlive,
         A.stembark.high = A.livebranch.low, 
         A.stembark.low = Total.biomass.high - (lowA.stemwood + lowA.branchlive + lowA.stembark), 
         A.foliage.high = A.stembark.low, 
         A.foliage.low = Total.biomass.high - (lowA.stemwood + lowA.branchlive + lowA.stembark + lowA.foliage), 
         A.branchdead.high = A.foliage.low,
         A.branchdead.low = Total.biomass.high - (lowA.stemwood + lowA.branchlive + lowA.stembark + lowA.foliage + lowA.branchdead), 
         A.dead.high = A.branchdead.low, 
         A.dead.low = Total.biomass.high - (lowA.stemwood + lowA.branchlive + lowA.stembark + lowA.foliage + lowA.branchdead + low.dead) #- lowA.stemwood - lowA.branchlive - lowA.stembark - lowA.foliage - lowA.branchdead - lowA.dead
  )

# -------------------------------------------------------------------------------
#  make plots of total biomass across the region
# -------------------------------------------------------------------------------
AGB.line <- ggplot()+geom_line(data = ten.plot.summary, aes(year, mAGB, group = mort.scheme, color = mort.scheme))+theme_bw()+ylab("Total AGB for 351 plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp)
NPP.line <- ggplot(ten.plot.summary, aes(year, mNPP, group = mort.scheme, color = mort.scheme))+geom_line()+theme_bw()+ylab("Total NPP for 351  plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp)

png(height = 7, width = 10, units = "in", res = 150, "forecasts_AGB/output/example351plots.total.biomass.png")
cowplot::plot_grid(AGB.line, NPP.line, ncol = 1, align = "hv")
dev.off()


b.plot <- ggplot()+
  #geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.stemwood, ymax = upA.stemwood, fill = "stem wood"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.branchlive, ymax = upA.branchlive, fill = "live branch"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.stembark, ymax = upA.stembark, fill = "stem bark"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.foliage, ymax = upA.foliage, fill = "foliage"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.branchdead, ymax = upA.branchdead, fill = "dead branch"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA.dead, ymax = upA.dead, fill = "dead stem"))+
  
  theme_bw()+facet_grid(cols =  vars(rcp), rows = vars(mort.scheme))+
  ylab(paste("ten plot total biomass \n  (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black" ))

b.plot

png(height = 10, width = 12, units = "in", res = 150, "forecasts_AGB/output/Total_biomass_kaye_151_plots_allrcps_nocc.png")
b.plot
dev.off()



b.plot.sum <- ggplot()+
  #geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.stemwood.low, ymax = A.stemwood.high, fill = "stem wood"))+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.livebranch.low, ymax = A.livebranch.high, fill = "live branch"))+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.stembark.low, ymax = A.stembark.high, fill = "stem bark"))+#facet_grid(cols =  vars(rcp), rows = vars(mort.scheme))+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.foliage.low, ymax = A.foliage.high, fill = "foliage"))+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.branchdead.low, ymax = A.branchdead.high, fill = "dead branch"))+
  geom_ribbon(data = added.up.tenplot.summary, aes(x = year, ymin = A.dead.low, ymax = A.dead.high, fill = "dead stem"))+
  
  theme_bw()+facet_grid(cols =  vars(rcp), rows = vars(mort.scheme))+
  ylab(paste("ten plot total biomass \n  (Mg/ha)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black" ))

b.plot.sum
# something is wrong with this plot

tota.plot.trunk <- ten.plot.summary %>% filter(year <= 2098)
yhi <- max(  tota.plot.trunk$up.stemwood, na.rm = TRUE) + 1
ylow <- min(  tota.plot.trunk$low.branchdead, na.rm=TRUE) - 1

b.flux <- ggplot()+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.stemwood, ymax = up.stemwood, fill = "stem wood"))+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.branchlive, ymax = up.branchlive, fill = "live branch"))+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.stembark, ymax = up.stembark, fill = "stem bark"))+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.foliage, ymax = up.foliage, fill = "foliage"))+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.branchdead, ymax = up.branchdead, fill = "dead branch"))+
  geom_ribbon(data = tota.plot.trunk, aes(x = year, ymin = low.dead, ymax = up.dead, fill = "dead stem"))+
  
  theme_bw()+facet_grid(cols =  vars(rcp), rows = vars(mort.scheme), scales = "free_y")+#xlim(2001,2018)+ylim(ylow, yhi)+
  ylab(paste("Total biomass increment, \n RCP 8.5, ten plots (Mg/ha)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1",  "dead stem" = "black" ))

b.flux
# save thes summaries:

png(height = 10, width = 12, units = "in", res = 150, "biomass_AGB/output/NPP_biomass_kaye_ten_plots_allrcps_nocc.png")
b.flux
dev.off()

# -------------------------------------------------------------------------------
#  make plots of big tree vs small across the region
# -------------------------------------------------------------------------------
#big tree vs small tree carbon in forecasts
get_tree_levelC_ests_FVS <- function(plot, mort.scheme, scenario, nocc = FALSE){
  cat(paste("reading data from", plot))
  if(nocc == FALSE){
    load(paste0("forecasts_AGB/biomass_data_FVSmort26all/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }else{
    load(paste0("biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }
  # objects
  # out, out.dead AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
  # AGB.foliage, NPP.foliage, 
  # AGB.stembark, NPP.stembark,
  # AGB.stemwood, NPP.stemwood,
  # AGB.branchdead, NPP.branchdead,
  # AGB.branchlive, NPP.branchlive,
  
  # lets say large trees > 30 cm dbh and small trees are < 30 cm dbh
  
  # out
  # dim(biomass.dead)
  # AGB
  # AGB.dead
  # 
  # second value is the median
  #diam.live[,1:10, 1]
  #diam.dead[,1:10, 1]
  
  diam.live.melt <- melt(diam.live[2,,])
  diam.live.melt$size_class <- ifelse(diam.live.melt$value<= 0.1, NA, 
                                      ifelse(diam.live.melt$value <= 30, "small tree", "big tree"))
  colnames(diam.live.melt) <- c("tree", "time", "DBH", "size_class")
  
  total.biomass.bytree <- biomass.stembark + biomass.branchlive + biomass.branchdead + biomass.foliage + biomass.stemwood
  
  # scale up to the stand
  total.biomass.bytree.stand <- total.biomass.bytree*tpa.live 
  
  total.biomass.bytree.melt <- melt(total.biomass.bytree.stand[2,,])
  colnames(total.biomass.bytree.melt) <- c("tree", "time", "AGB")
  
  diam.biomass.df <- left_join(diam.live.melt, total.biomass.bytree.melt, by = c("tree", "time"))
  
  tpa.live
  
  head(diam.biomass.df )
  
  ggplot(diam.biomass.df, aes(x = time, y = AGB, color = size_class, group = tree))+geom_line()
  
  ggplot(na.omit(diam.biomass.df), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
    ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
    scale_fill_manual(name = 'Size class', 
                      values =c("big tree"="#e66101","small tree"="#5e3c99"))
  
  diam.biomass.df$plot <- plot
  diam.biomass.df$mort.scheme <- mort.scheme
  if(nocc == FALSE){
    diam.biomass.df$scenario <- scenario
  }else{
    diam.biomass.df$scenario <- paste0("nocc", ".", scenario)
  }
  
  diam.biomass.df
  #e66101
  #5e3c99
}


# read in and get the tree level estimates
# RCP 2.6
btst.AGB.DIDD.26 <- lapply(unique(plotnos), FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIDD", scenario = "rcp26")})
btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
btst.AGB.DIDD.26.df
plot = "2873938010690"


unique(plotnos)[1:264] %in% "2668869010690"
plot <- c("2611722010690","2616304010690")
 idx <-!unique(plotnos) %in% plot

btst.AGB.nomort.26<- lapply(unique(plotnos), FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "nomort", scenario = "rcp26")})
btst.AGB.nomort.26.df <- do.call(rbind, btst.AGB.nomort.26)

btst.AGB.DDonly.26<- lapply(unique(plotnos), FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DDonly", scenario = "rcp26")})
btst.AGB.DDonly.26.df <- do.call(rbind, btst.AGB.DDonly.26)

btst.AGB.DIonly.26<- lapply(unique(plotnos)[idx], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIonly", scenario = "rcp26")})
btst.AGB.DIonly.26.df <- do.call(rbind, btst.AGB.DIonly.26)

# RCP 4.5
btst.AGB.DIDD.45 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45")})
btst.AGB.DIDD.45.df <- do.call(rbind, btst.AGB.DIDD.45)

btst.AGB.nomort.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp45")})
btst.AGB.nomort.45.df <- do.call(rbind, btst.AGB.nomort.45)

btst.AGB.DDonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp45")})
btst.AGB.DDonly.45.df <- do.call(rbind, btst.AGB.DDonly.45)

btst.AGB.DIonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp45")})
btst.AGB.DIonly.45.df <- do.call(rbind, btst.AGB.DIonly.45)

# RCP 6.0
btst.AGB.DIDD.60 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60")})
btst.AGB.DIDD.60.df <- do.call(rbind, btst.AGB.DIDD.60)

btst.AGB.nomort.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp60")})
btst.AGB.nomort.60.df <- do.call(rbind, btst.AGB.nomort.60)

btst.AGB.DDonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp60")})
btst.AGB.DDonly.60.df <- do.call(rbind, btst.AGB.DDonly.60)

btst.AGB.DIonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp60")})
btst.AGB.DIonly.60.df <- do.call(rbind, btst.AGB.DIonly.60)

# RCP 8.5
btst.AGB.DIDD.85 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85")})
btst.AGB.DIDD.85.df <- do.call(rbind, btst.AGB.DIDD.85)

btst.AGB.nomort.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp85")})
btst.AGB.nomort.85.df <- do.call(rbind, btst.AGB.nomort.85)

btst.AGB.DDonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp85")})
btst.AGB.DDonly.85.df <- do.call(rbind, btst.AGB.DDonly.85)

btst.AGB.DIonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp85")})
btst.AGB.DIonly.85.df <- do.call(rbind, btst.AGB.DIonly.85)

# no climate change scenario:
# read in and get the tree level estimates
# RCP 2.6
btst.AGB.DIDD.nocc <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DIDD.nocc.df <- do.call(rbind, btst.AGB.DIDD.nocc)
btst.AGB.DIDD.nocc.df

btst.AGB.nomort.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", nocc = TRUE)})
btst.AGB.nomort.nocc.df <- do.call(rbind, btst.AGB.nomort.nocc)

btst.AGB.DDonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DDonly.nocc.df <- do.call(rbind, btst.AGB.DDonly.nocc)

btst.AGB.DIonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DIonly.nocc.df <- do.call(rbind, btst.AGB.DIonly.nocc)


# combine all the tree-level datasets together:
allplots.treeC <- rbind(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df, 
                        btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
                        btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
                        btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df, 
                        btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)
saveRDS(allplots.treeC, "outputs/allplots.treeC.RDS")

rm(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df, 
   btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
   btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
   btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df, 
   btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)

rm(btst.AGB.nomort.26 , btst.AGB.DIonly.26 , btst.AGB.DDonly.26 , btst.AGB.DIDD.26 , 
   btst.AGB.nomort.45 , btst.AGB.DIonly.45 , btst.AGB.DDonly.45 , btst.AGB.DIDD.45 ,
   btst.AGB.nomort.60 , btst.AGB.DIonly.60 , btst.AGB.DDonly.60 , btst.AGB.DIDD.60 , 
   btst.AGB.nomort.85 , btst.AGB.DIonly.85 , btst.AGB.DDonly.85 , btst.AGB.DIDD.85 , 
   btst.AGB.nomort.nocc , btst.AGB.DIonly.nocc , btst.AGB.DDonly.nocc , btst.AGB.DIDD.nocc )
#
# ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
#   summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)

allplots.treeC <- rbind(btst.AGB.DIDD.26.df, btst.AGB.nomort.26.df,  btst.AGB.DDonly.26.df)

png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_all_539_plots.png")
ggplot(na.omit(allplots.treeC), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (kg/acre)")+theme_bw()+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

# example for how single plots may vary quite alot!
png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_one_plot.png")
ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[1]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_two_plot.png")

ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[2]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_three_plot.png")

ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[3]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))

dev.off()


allplots.treeC.totals <- na.omit(allplots.treeC) %>% group_by(plot, mort.scheme, scenario, size_class, time) %>%
  summarise(total.AGB.size = sum(AGB, na.rm=TRUE), 
            avg.diam.size = median(DBH, na.rm = TRUE), 
            ntrees = n())

# get proportion of total biomass in large vs small trees:
plotC.totals.spread <- allplots.treeC.totals %>% select(-ntrees, -avg.diam.size) %>% group_by(plot, mort.scheme, scenario, time) %>%
  spread(size_class, value = total.AGB.size)

plotC.totals.prop <- plotC.totals.spread %>% group_by(plot, mort.scheme, scenario, time) %>%
  mutate(total.AGB = `big tree` + `small tree`,
         prop.big = `big tree`/total.AGB, 
         prop.small = `small tree`/total.AGB) 


whole.regionC <- plotC.totals.prop %>% group_by(mort.scheme, scenario, time) %>% mutate(big.dominated = ifelse(prop.big > 0.65, 1, 0), 
                                                                                        small.dominated = ifelse(prop.small > 0.65, 1, 0), 
                                                                                        mixed.size = ifelse(prop.small <= 0.65 & prop.small >=0.35, 1, 0))%>%
  summarise(prop.big.all = mean(prop.big, na.rm =TRUE), 
            prop.small.all = mean(prop.small, na.rm =TRUE), 
            sum.AGB = sum(total.AGB, na.rm =TRUE),
            nplots.big = sum(big.dominated, na.rm = TRUE), 
            nplots.small = sum(small.dominated, na.rm = TRUE), 
            nplots.mixed = sum(mixed.size, na.rm = TRUE))

png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_dominated_539plots.png")

ggplot()+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.big, color = "big tree dominated"))+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.small, color= "small tree dominated"))+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.mixed, color= "mixed dominated"))+
  scale_color_manual(name = 'Size class', 
                     values =c("big tree dominated"="#e66101","small tree dominated"="#5e3c99", "mixed dominated" = "black"))+
  facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))+
  
  ylab(paste("number of plots dominated by big vs small trees"))+    xlab("Year")+
  
  theme_bw()+theme(panel.grid = element_blank())

dev.off()
# plot the number of plots dominated by small vs large trees:
png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_dominated_prop_biomass_539plots.png")

ggplot()+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=prop.big.all, ymax=prop.big.all + prop.small.all, fill = "small tree"))+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=0, ymax=prop.big.all, fill = "big tree"))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+
  facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))+
  
  ylab(paste("% of total biomass in each size class"))+    xlab("Year")+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())
dev.off()


on <- plotC.totals.prop %>% filter(plot %in% "2447353010690")

ggplot()+
  geom_ribbon(data = on, aes(x=time,ymin=0, ymax=prop.big), fill = "#e66101")+
  geom_ribbon(data = on, aes(x=time,ymin=prop.big, ymax=prop.big + prop.small), fill = "#5e3c99")+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+
  facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))+
  
  ylab(paste("% of total biomass in each size class"))+    xlab("Year")+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())


ggplot(plotC.totals.prop, aes(prop.big))+geom_histogram()
ggplot(plotC.totals.prop, aes(prop.big))+geom_histogram()+geom_density()+facet_wrap(~time)

# how to classify these different trajectories?

plotC.totals.prop %>% group_by(plot, mort.scheme, scenario, time) %>% select(-total.AGB, -prop.small, )

# simple classifcation scheme based on prop large trees:
emissions.norm <- BBmisc::normalize(emissions, method="standardize")
clust.pam <- tsclust(emissions.norm, type="partitional", k=2L:17L, distance="dtw", centroid="pam")

# dtwclust to see how many different trajectories there are in the DIDD:







# read in all the diameters from dbh.pred (out) and dbh.dead (out.dead)

# set a size threshold for big trees vs small trees

# plot proportion of biomass in live big tree, live small tree, dead big tree, dead small tree

# by the rcp and mortalti scenarios






# -------------------------------------------------------------------------------
#  make plots of big tree vs small tree mortality
# -------------------------------------------------------------------------------
get_tree_diam_live_dead_ests <- function(plot, mort.scheme, scenario, nocc = FALSE){
  if(nocc == FALSE){
    load(paste0("forecasts_AGB/biomass_data_FVSmort26all//plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }else{
    load(paste0("forecasts_AGB/biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }

  # get live diameters
  diam.live.melt <- melt(diam.live[2,,])
  diam.live.melt$size_class <- ifelse(diam.live.melt$value<= 0.1, NA, 
                                      ifelse(diam.live.melt$value <= 30, "small tree", "big tree"))
  colnames(diam.live.melt) <- c("tree", "time", "DBH", "size_class")
  
  # need to multiply by the tpa as wel
  tpa.live.melt <- melt(tpa.live[2,,])
  #tpa.live.melt$size_class <- ifelse(tpa.live.melt$value<= 0.1, NA, 
   #                                   ifelse(tpa.live.melt$value <= 30, "small tree", "big tree"))
  colnames(tpa.live.melt) <- c("tree", "time", "TPA")
  
  
  tpa.dead.melt <- melt(tpa.dead[2,,])
  colnames(tpa.dead.melt) <- c("tree", "time", "TPA")
  
 
 
  
  # get dead diameters
  diam.dead.melt <- melt(diam.live[2,,])
  diam.dead.melt$size_class <- ifelse(diam.dead.melt$value<= 0.1, NA, 
                                      ifelse(diam.dead.melt$value <= 30, "small tree", "big tree"))
  colnames(diam.dead.melt) <- c("tree", "time", "DBH", "size_class")
  
  summary(diam.dead.melt)
  
  diam.dead.melt$status <- ifelse(diam.dead.melt$DBH == 0, "live", "dead")
  diam.live.melt$status <- ifelse(diam.live.melt$DBH == 0.1, "dead", "live")
  
  
  diam.dead.melt <- left_join(diam.dead.melt, tpa.dead.melt, by = c("tree", "time"))
  diam.live.melt <- left_join(diam.live.melt, tpa.live.melt, by = c("tree", "time"))
  
  #diam.dead.melt$status <- ifelse(diam.dead.melt$DBH == 0, "live", "all dead")
  #diam.live.melt$status <- ifelse(diam.live.melt$DBH == 0.1, "all dead", "live")
  
  
  #combine into one big DF
  diam.dead.melt$df <- "dead"
  diam.live.melt$df <- "live"
  diams <- rbind(diam.live.melt, diam.dead.melt)
  
  diams$plot <- plot
  diams$mort.scheme <- mort.scheme
  if(nocc == FALSE){
    diams$scenario <- scenario
  }else{
    diams$scenario <- paste0("nocc", ".", scenario)
  }
  
  diams
 
}
plot <- unique(plotnos)[1]
# read in and get the tree level estimates
# RCP 2.6
btst.DIAMS.DIDD.26 <- lapply(unique(plotnos), FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26")})
btst.DIAMS.DIDD.26.df <- do.call(rbind, btst.DIAMS.DIDD.26)
btst.DIAMS.DIDD.26.df

btst.DIAMS.nomort.26<- lapply(unique(plotnos), FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26")})
 btst.DIAMS.nomort.26.df <- do.call(rbind, btst.DIAMS.nomort.26)
# 
 btst.DIAMS.DDonly.26<- lapply(unique(plotnos), FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26")})
 btst.DIAMS.DDonly.26.df <- do.call(rbind, btst.DIAMS.DDonly.26)
# 
 btst.DIAMS.DIonly.26<- lapply(unique(plotnos), FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26")})
 btst.DIAMS.DIonly.26.df <- do.call(rbind, btst.DIAMS.DIonly.26)
# 
# # RCP 4.5
# btst.DIAMS.DIDD.45 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45")})
# btst.DIAMS.DIDD.45.df <- do.call(rbind, btst.DIAMS.DIDD.45)
# 
# btst.DIAMS.nomort.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp45")})
# btst.DIAMS.nomort.45.df <- do.call(rbind, btst.DIAMS.nomort.45)
# 
# btst.DIAMS.DDonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp45")})
# btst.DIAMS.DDonly.45.df <- do.call(rbind, btst.DIAMS.DDonly.45)
# 
# btst.DIAMS.DIonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp45")})
# btst.DIAMS.DIonly.45.df <- do.call(rbind, btst.DIAMS.DIonly.45)
# 
# # RCP 6.0
# btst.DIAMS.DIDD.60 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60")})
# btst.DIAMS.DIDD.60.df <- do.call(rbind, btst.DIAMS.DIDD.60)
# 
# btst.DIAMS.nomort.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp60")})
# btst.DIAMS.nomort.60.df <- do.call(rbind, btst.DIAMS.nomort.60)
# 
# btst.DIAMS.DDonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp60")})
# btst.DIAMS.DDonly.60.df <- do.call(rbind, btst.DIAMS.DDonly.60)
# 
# btst.DIAMS.DIonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp60")})
# btst.DIAMS.DIonly.60.df <- do.call(rbind, btst.DIAMS.DIonly.60)
# 
# # RCP 8.5
# btst.DIAMS.DIDD.85 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85")})
# btst.DIAMS.DIDD.85.df <- do.call(rbind, btst.DIAMS.DIDD.85)
# 
# btst.DIAMS.nomort.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp85")})
# btst.DIAMS.nomort.85.df <- do.call(rbind, btst.DIAMS.nomort.85)
# 
# btst.DIAMS.DDonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp85")})
# btst.DIAMS.DDonly.85.df <- do.call(rbind, btst.DIAMS.DDonly.85)
# 
# btst.DIAMS.DIonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp85")})
# btst.DIAMS.DIonly.85.df <- do.call(rbind, btst.DIAMS.DIonly.85)
# 
# # no climate change scenario:
# # read in and get the tree level estimates
# # RCP 2.6
# btst.DIAMS.DIDD.nocc <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", nocc = TRUE)})
# btst.DIAMS.DIDD.nocc.df <- do.call(rbind, btst.DIAMS.DIDD.nocc)
# btst.DIAMS.DIDD.nocc.df
# 
# btst.DIAMS.nomort.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", nocc = TRUE)})
# btst.DIAMS.nomort.nocc.df <- do.call(rbind, btst.DIAMS.nomort.nocc)
# 
# btst.DIAMS.DDonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", nocc = TRUE)})
# btst.DIAMS.DDonly.nocc.df <- do.call(rbind, btst.DIAMS.DDonly.nocc)
# 
# btst.DIAMS.DIonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", nocc = TRUE)})
# btst.DIAMS.DIonly.nocc.df <- do.call(rbind, btst.DIAMS.DIonly.nocc)
# 
# 
# 
# # checking to see if the mort trees are being labeled properly:
# ggplot(btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]), aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# 
# filtered.test <- btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]) %>% filter(status == df)
# ggplot(filtered.test, aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# # combine all the tree-level datasets together:
# allplots.treeDIAM <- rbind(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df, 
#                         btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
#                         btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df, 
#                         btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df, 
#                         btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
# 
# saveRDS(allplots.treeDIAM, "outputs/allplots.treeDiam.forecast.RDS")
# rm(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df, 
#    btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
#    btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df, 
#    btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df, 
#    btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
# rm(btst.DIAMS.nomort.26 , btst.DIAMS.DIonly.26 , btst.DIAMS.DDonly.26 , btst.DIAMS.DIDD.26 , 
#    btst.DIAMS.nomort.45 , btst.DIAMS.DIonly.45 , btst.DIAMS.DDonly.45 , btst.DIAMS.DIDD.45 ,
#    btst.DIAMS.nomort.60 , btst.DIAMS.DIonly.60 , btst.DIAMS.DDonly.60 , btst.DIAMS.DIDD.60 , 
#    btst.DIAMS.nomort.85 , btst.DIAMS.DIonly.85 , btst.DIAMS.DDonly.85 , btst.DIAMS.DIDD.85 , 
#    btst.DIAMS.nomort.nocc , btst.DIAMS.DIonly.nocc , btst.DIAMS.DDonly.nocc , btst.DIAMS.DIDD.nocc )
# #
# #

# since we have the data on more for DDID and rcp26:

allplots.treeDIAM <- rbind(btst.DIAMS.DIDD.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.nomort.26.df)
# this gets rid of trees labeled as dead in the live df and vice versa
allplots.treeDIAMsubset <- allplots.treeDIAM #%>% filter(status == df)

region.ndead <- allplots.treeDIAMsubset %>% group_by(df,  mort.scheme, scenario, time) %>% summarise(ntree = sum(TPA))
ggplot(region.ndead, aes(x = time, y = ntree, color = df))+geom_point()+facet_wrap(~mort.scheme)

# summarize the dead tree mortality rate by plot, and compare to the results of FIA data analyses
# 
plot.ndead <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time) %>% summarise(ntree = sum(TPA))
plot.ndead.spread <- plot.ndead %>% ungroup()  %>% group_by(plot, mort.scheme, scenario, time) %>% spread(df, value = ntree)
plot.prop.dead <- plot.ndead.spread %>% group_by(plot, mort.scheme, scenario, time) %>% mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))


# crazy line plot
ggplot(plot.prop.dead, aes(x = time, y = prop.dead, group = plot))+geom_line()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

# proportion of dead trees in 2018 :
ggplot(plot.prop.dead %>% filter(time == 17), aes(x =  prop.dead))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

# plot size distribution of living and dead trees in 2018
plot.mort.rate2001.2020 <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time) %>% filter(time == 17)


# calculate a 10 year mortality rate, from 2001-2010 and from 2011-2020 to compare:
all.trees.2001.2010 <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time) %>% filter(time %in% 1:10)%>% 
  summarise(ntree = sum(TPA))%>%ungroup()  %>% 
  group_by(plot, mort.scheme, scenario, time) %>% spread(df, value = ntree)%>% group_by(plot, mort.scheme, scenario, time) %>% 
  mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))# gets # dead for each plot in each year


all.trees.2011.2020 <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time) %>% filter(time %in% 11:20)%>%
  summarise(ntree = sum(TPA))%>%ungroup()  %>% 
  group_by(plot, mort.scheme, scenario, time) %>% spread(df, value = ntree)%>% group_by(plot, mort.scheme, scenario, time) %>% 
  mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))# gets # dead for each plot in each year

# calculate mortality rate for each plot, but wait to join with another variable
# for the first time period mortality rate is just the proportion at time 10
plot.mort.rate.2001.2010 <- all.trees.2001.2010 %>% group_by(plot, mort.scheme, scenario) %>% filter(time == 10)%>% mutate(mortality.rate = prop.dead/10) %>% dplyr::select(-time)

# but for the second time period its the # of newly dead trees at time 20
plot.mort.rate.2011.2020 <- all.trees.2011.2020 %>% group_by(plot, mort.scheme, scenario) %>% filter(time == 20)%>% mutate(mortality.rate = prop.dead/10) %>% dplyr::select(-time)

colnames(plot.mort.rate.2011.2020) <-c("PLT_CN", "mort.scheme", "scenario",  "dead_2001_2020", "live_2001_2020", "prop.dead_2001_2020", "mortality.rate_2001_2020") 
colnames(plot.mort.rate.2001.2010) <-c("PLT_CN", "mort.scheme", "scenario",  "dead_2010", "live_2010", "prop.dead_2010", "mortality.rate_2010") 

plot.mort.rate2001.2020 <- left_join(plot.mort.rate.2001.2010, plot.mort.rate.2011.2020)

plot.mort.rate2001.2020 <- plot.mort.rate2001.2020 %>% group_by(PLT_CN, mort.scheme, scenario) %>% mutate(dead_2010_2020 = dead_2001_2020 - dead_2010, 
                                                                               live_2010_2020 = live_2001_2020, 
                                                                               prop.dead_2010_2020 = dead_2010_2020/(dead_2010_2020+live_2010_2020),
                                                                               mortality.rate_2010_2020 = prop.dead_2010_2020/10)
# plot-level mortality ranges from 0-5% mortality in from 2001-2010
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010, y =mortality.rate_2010_2020))+geom_point()

# plot-level mortality ranges from 0-5% mortality in from 2001-2010
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010))+geom_histogram()
# and the range is about the same from 2011-2020
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010_2020))+geom_histogram()

# get bins of SDI and DBH:
#TREE_remeas <- TREE_remeas %>% mutate(SDIbin=cut(SDIs_static, breaks=c(0,50,100, 150,200,250,300,350,400, 450,Inf), labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", 

# get SDI information and plot
#                                                                                                      "300-350", "350-400", "400-450", ">450")))

fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))

PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE

# get the previous survey for each tree: 
TREE_remeas <- subset(TREE, !is.na(PREVDIA))
TREE_remeas <- subset(TREE_remeas, STATUSCD == 1 | STATUSCD == 2) 

# Look up previous AGB
### look up previous AGB, PLT_CN, and CONDID
TREE_remeas$PREV_DRYBIO_AG <- TREE$DRYBIO_AG[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
TREE_remeas$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
TREE_remeas$PREV_CONDID <- TREE$CONDID[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]

# Subset records with previous AGB found
TREE_remeas <- subset(TREE_remeas, !is.na(PREV_DRYBIO_AG))

# for growth analysis
TREE_remeas$DRYBIO_AG_DIFF <- TREE_remeas$DRYBIO_AG - TREE_remeas$PREV_DRYBIO_AG
TREE_remeas$DIA_DIFF <- TREE_remeas$DIA - TREE_remeas$PREVDIA

# basal area increment
TREE_remeas$BAt1 <- ((TREE_remeas$PREVDIA/2)^2)*3.14159
TREE_remeas$BAt2 <- ((TREE_remeas$DIA/2)^2)*3.14159
TREE_remeas$BA_DIFF <- TREE_remeas$BAt2 - TREE_remeas$BAt1

# Read in plot data and get coordinates and previous measurement year
#plots <- read.csv(paste(data.path,"PLOT_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)

TREE_remeas$LAT <- PLOT$LAT[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$LON <- PLOT$LON[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$ELEV <- PLOT$ELEV[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$PREV_MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PREV_PLT_CN, PLOT$CN)]

# Calculate census interval
TREE_remeas$CENSUS_INTERVAL <- TREE_remeas$MEASYEAR - TREE_remeas$PREV_MEASYEAR

colnames(TREE_remeas)

# zero if no change, 
# 2 if the tree died in the census interval
TREE_remeas$STATUSCD_CHANGE <- ifelse(TREE_remeas$PREV_STATUS_CD == TREE_remeas$STATUSCD, 0, 
                                      ifelse(TREE_remeas$PREV_STATUS_CD == 1 & TREE_remeas$STATUSCD == 2, 2, NA))

# match up PLOT and COND data
PLOT$DSTRBYR1 <- COND$DSTRBYR1[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBYR2 <- COND$DSTRBYR2[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBYR3 <- COND$DSTRBYR3[match(PLOT$CN, COND$PLT_CN)]


PLOT$DSTRBCD1 <- COND$DSTRBCD1[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBCD2 <- COND$DSTRBCD2[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBCD3 <- COND$DSTRBCD3[match(PLOT$CN, COND$PLT_CN)]

# Match up the tree and plot data
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]


TREE$DSTRBCD1 <- PLOT$DSTRBCD1[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBCD2 <- PLOT$DSTRBCD2[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBCD3 <- PLOT$DSTRBCD3[match(TREE$PLT_CN, PLOT$CN)]

TREE$DSTRBYR1 <- PLOT$DSTRBYR1[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBYR2 <- PLOT$DSTRBYR2[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBYR3 <- PLOT$DSTRBYR3[match(TREE$PLT_CN, PLOT$CN)]


unique(TREE$MORTCD)
unique(TREE$STATECD)

# TREE_remeas %>% dplyr::filter(SPCD %in% "122" & STATUSCD %in% c(1, 2)) %>% group_by(STATUSCD_CHANGE) %>% summarise(median.dbh = median(DIA, na.rm = TRUE), 
#                                                                                                                    median.ht = median(HT, na.rm = TRUE))
# 
# ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(DIA))+facet_wrap(~STATUSCD_CHANGE)
# 
# ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(HT))+facet_wrap(~STATUSCD_CHANGE)

# goal: make plots of the distribution of tree diameters that died with groups of diameter, heights, subplot SDIs, and SI
# compare these to biomass estimates
# get a static estimate of SDI, which includes the dead trees:

static_SDI_pltcn <- TREE %>% ungroup() %>%  filter(DIA > 1) %>%
  group_by(PLT_CN, STATECD, PLOT, COUNTYCD,  MEASYR) %>%
  summarise(ntrees_static = n(),
            TPA_static =sum(TPA_UNADJ), 
            Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/ntrees_static), 
            SDIdq_static = ((Dq_static/10)^1.6)*TPA_static, #calculate SDI (Summation Method) on the subplot:
            SDIs_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE))#, ## calculate SDI (Quadratic mean diameter) on the subplot:


hist(static_SDI_pltcn$SDIs_static) 

TREE$SDIs_static <- static_SDI_pltcn$SDIs_static[match(TREE$PLT_CN, static_SDI_pltcn$PLT_CN)]
# join up the SDI static with the forecasts:
# this is the most recent SDI in the dataset, not the SDI estimated by the forecast
allplots.treeDIAMsubset$SDIs_static <- static_SDI_pltcn$SDIs_static[match(allplots.treeDIAMsubset$plot, static_SDI_pltcn$PLT_CN)]

allplots.treeDIAMsubset <- allplots.treeDIAMsubset %>% mutate(SDIbin=cut(SDIs_static, breaks=c(0,135, 270,450, Inf), labels=c("0-135","136-270","270-450", ">450")))

allplots.treeDIAMsubset$DIA <- allplots.treeDIAMsubset$DBH/2.54
allplots.treeDIAMsubset <- allplots.treeDIAMsubset %>% mutate(DIAbin=cut(DIA, breaks=c(0,5,10, 15,20,25,30,35,40, 45,Inf), labels=c("0-5","5-10","10-15", "15-20", "20-25", "25-30", 
                                                                                                            "30-35", "35-40", "40-45", ">45")))
prop.dead.2020 <- allplots.treeDIAMsubset %>% group_by(SDIbin, DIAbin, df) %>% filter(time %in% 1:20) %>% summarise(ntree = sum(TPA)) %>% 
  ungroup() %>% group_by (SDIbin, DIAbin) %>% spread(`ntree`, key = df) %>% mutate(prop.dead = `dead`/(`dead`+`live`)) %>% mutate(prop.dead = ifelse(is.na(prop.dead), 0, prop.dead),
                                                                                                                           mort.rate = prop.dead/20)

png(height = 4, width = 6, units = "in", res = 150, "outputs/scatter_mort_rate_by_dia_sdi_lines_forecast_26_DDID.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes(x = DIAbin, y = mort.rate, color = SDIbin, group = SDIbin))+
  geom_point()+geom_line()+theme_bw()+ylab("proportion of trees dead")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "outputs/boxplot_mort_rate_by_dia_forecast_26_DDID.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = mort.rate, x = DIAbin))+
  geom_boxplot()+theme_bw()+ylab("proportion of trees dead")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "outputs/boxplot_mort_rate_by_sdi_forecast_26_DDID.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = mort.rate, x = SDIbin))+
  geom_boxplot()+theme_bw()+ylab("proportion of trees dead")+xlab("SDI Class")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "outputs/tile_prop_mort_by_dia_sdi_forecast_26_DDID.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin) & !is.na(SDIbin)), aes( x = DIAbin, y = SDIbin, fill = mort.rate))+
  geom_raster()+scale_fill_gradientn(colors = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))

dev.off()


# We cant compare to the number of dead for these same plots over this time period because there is not linkage of annual and periodic
TREE.sameplts <- TREE_REMEAS %>% filter(PREV_PLT_CN %in% unique(plotnos))
length(unique(TREE.sameplts$PLT_CN))

# but we have some data on these trees/plots that could be useful:
TREE.sameplts <- TREE %>% filter(PLT_CN %in% unique(plotnos))
length(unique(TREE.sameplts$PLT_CN))

head(TREE.sameplts)

PLOT.sameplts <- PLOT %>% filter(CN %in% unique(plotnos))
colnames(plot.mort.rate2001.2020)[1] <- "CN"
PLOT.sameplts$CN <- as.character(PLOT.sameplts$CN)
plot.mort.rate2001.2020$CN <- as.character(plot.mort.rate2001.2020$CN)
forecast.2018 <- left_join(PLOT.sameplts, plot.mort.rate2001.2020, by = "CN")

prop.dead.2018.plot <- forecast.2018 


####################################################################################
# Summary by ecoregion
####################################################################################
# read in the ecoregion shapefile:
####################################################################################

# library(rgdal)
# library(ggspatial)
# library(raster)
# library(cowplot)
# library(egg)
# library(rnaturalearth)
# library(sf)
# library(hddtools)


eco.regions <- read_sf( "us_eco_l3/us_eco_l3.shp")
# plot the llevel 3 ecoregions (takes awhile)
# eco.regions %>% 
#   ggplot() +
#   geom_sf() +
#   theme_bw()

# get SDI;
prop.dead.2018.plot$SDIstatic <- static_SDI_pltcn$SDIs_static[match(prop.dead.2018.plot$CN, static_SDI_pltcn$PLT_CN)]
st_crs(eco.regions)
# use lat and long to get ecoregions:
PLOT.sameplts_sf <- st_as_sf(prop.dead.2018.plot, coords = c("LON", "LAT"))
str(PLOT.sameplts_sf)
st_crs(PLOT.sameplts_sf) <- 4326
PLOT.sameplts_sf <- st_transform(PLOT.sameplts_sf, st_crs(eco.regions))

#-124.79,49.38, 24.41, -101
bbox <- st_as_sf(as(raster::extent(-124.79, -101, 24.41, 49.38), "SpatialPolygons"))
st_crs(bbox) <- 4326
bbox <- st_transform(bbox, st_crs(eco.regions))

eco_crop <- st_crop(eco.regions, bbox)


#PLOT_intersects <- st_intersects(PLOT.sameplts_sf, eco.regions)


#eco.regions_sel_sf <-eco.regions[PLOT_intersects[[1]],]

# plot

# png(height = 6, width = 11, units = "in", res = 200, "CONUS_ECOREGIONS_WITH_THE_151_FORECASTED_PLOTS.png")
# plot(st_geometry(eco.regions), border="#aaaaaa", main="CONUS ECOREGIONS WITH THE 151 forecasted plots")
# plot(st_geometry(eco.regions_sel_sf), add=T, col="red")
# plot(st_geometry(eco.regions_sel_sf), add=T, lwd = 2)
# dev.off()

# TREE_remeas_sf <- st_as_sf(TREE_remeas, coords = c("PLOT_LON", "PLOT_LAT"))
# #str(PLOT.sameplts_sf)
# st_crs(TREE_remeas_sf) <- 4326
# 
# TREE_remeas_sf <- st_transform(TREE_remeas_sf, st_crs(eco_crop))
# 
# # 
# Do an spatial join to link the prop.dead plot level data to the ecoregion data
ecojoin_j <- st_join(eco_crop, PLOT.sameplts_sf )
ecojoin_summary <- ecojoin_j %>% dplyr::select(-NA_L2CODE, -NA_L2NAME,-L2_KEY, -NA_L1CODE, -NA_L1NAME, -L1_KEY) %>% group_by(US_L3CODE, US_L3NAME, L3_KEY, Shape_Leng, Shape_Area, mort.scheme) %>% summarise(avg_prop_dead = median(prop.dead_2010, na.rm = TRUE), 
                                                                                                                                                                                          avg_mort_rate = median(mortality.rate_2010, na.rm = TRUE),
                                                                                                                                                                                          total_dead = sum(dead_2010, na.rm = TRUE), 
                                                                                                                                                                                          total_living = sum(live_2010, na.rm =TRUE), 
                                                                                                                                                                                          prop_dead_ecoregion = ifelse(total_dead == 0, 0, total_dead/(total_dead + total_living)), 
                                                                                                                                                                                          mort_rate_ecoregion = prop_dead_ecoregion/10,
                                                                                                                                                                                          #SDI = mean(SDIs_static, na.rm =TRUE),
                                                                                                                                                                                          
                                                                                                                                                                                          avg_prop_dead_2020 = median(prop.dead_2010_2020, na.rm = TRUE), 
                                                                                                                                                                                          avg_mort_rate_2020 = median(mortality.rate_2010_2020, na.rm = TRUE),
                                                                                                                                                                                          total_dead_2020 = sum(dead_2010_2020, na.rm = TRUE), 
                                                                                                                                                                                          total_living_2020 = sum(live_2010_2020, na.rm =TRUE), 
                                                                                                                                                                                          prop_dead_ecoregion_2020 = ifelse(total_dead_2020 == 0, 0, total_dead_2020/(total_dead_2020 + total_living_2020)), 
                                                                                                                                                                                          mort_rate_ecoregion_2020 =  prop_dead_ecoregion_2020/10)
png(height = 8, width = 18, units = "in", res = 200, "CONUS_forecast_average_plot_mort_rate_2010_rcp26_DDID.png")
ggplot() + 
  geom_sf(data = ecojoin_summary, aes(fill = avg_mort_rate)) +
  scale_fill_gradientn(colours = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))+facet_wrap(~mort.scheme)
dev.off()

png(height = 10, width = 10, units = "in", res = 200, "CONUS_forecast_average_ecoregion_mort_rate_2010_2020_rcp26_DDID.png")
ggplot() + 
  geom_sf(data =  ecojoin_summary, aes(fill = mort_rate_ecoregion)) +
  scale_fill_gradientn(colours = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))+
  geom_sf(data = PLOT.sameplts_sf, aes(color = SDIstatic))+facet_wrap(~mort.scheme)
dev.off()

# png(height = 8, width = 6, units = "in", res = 200, "CONUS_ecoregions_of_IW.png")
# ggplot() + 
#   geom_sf(data = eco_crop, aes(fill = US_L3NAME)) 
# dev.off()




#------------------------------------------------------------------------------------
# get torching and crowning indices from the diameter distributions
#------------------------------------------------------------------------------------
get_torch_crown_indices_FORECASTS <- function(plt){
  
  ex.plt <- TREE %>% filter(PLT_CN %in% plt)
  ex.plt$HT_m <- ex.plt$HT*0.304
  ex.plt$TPH <- ex.plt$TPA_UNADJ*(1/0.404686)
  ex.plt$DBH_m <-ex.plt$DIA*2.54 
  ex.plt$BA <- (pi*(ex.plt$DBH_m/2)^2)/10000 # should be in m2
  
  forecast.plt <- allplots.treeDIAMsubset %>% filter(plot %in% plt)
  
  forecast.plt$TPH <- forecast.plt$TPA*(1/0.404686)
  forecast.plt$BA <- (pi*(forecast.plt$DBH/2)^2)/10000 # should be in m2
  
  plt.characteristics <- forecast.plt %>% filter(df %in% "live")%>%group_by (plot, mort.scheme, scenario, time) %>% summarise(ba = sum(BA, na.rm =TRUE), 
                                                                    #ht = mean(HT_m, na.rm = TRUE), 
                                                                    tph = sum(TPH, na.rm = TRUE))
  
  # calculate height from the FIA survey data
  survey.characteristics <- ex.plt  %>% summarise(ba = sum(BA, na.rm =TRUE), 
                                      ht = mean(HT_m, na.rm = TRUE), 
                                      tph = sum(TPH, na.rm = TRUE))
  plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
  for(i in 1:length(plt.characteristics$plot)){
    plt.CrownFuel[i,] = as.matrix(canFuel(ba = plt.characteristics[i,]$ba, ht = survey.characteristics$ht, tph = plt.characteristics[i,]$tph, type = "pp")) # pp is ponderosa pine:
  }
  colnames(plt.CrownFuel) <- c("cfl", "cbd", "cbh")
  
  exampCrownFuel = data.frame(
    CBD = plt.CrownFuel[,"cbd"], # Canopy bulk density
    FMC = rep(100,length(plt.CrownFuel[,"cbd"])),  # foliar moisture content..assuming 100%?
    CBH = plt.CrownFuel[,"cbh"], # Canopy base height
    CFL = plt.CrownFuel[,"cfl"] # canopy fuel load
  )
  
  
  # Choose an example fuel model and fuel mositure scenario
  # need to do some research on what this is/means
  
  
  exampSurfFuel = fuelModels['A10',]
  
  
  # assume very dry dead fuel loads:
  #fuelMoisture['D1L1',]
  
  #exampFuelMoisture = fuelMoisture['D1L1',]
  # get slope from COND data
  #ex.plot.data <-PLOT %>% filter(CN %in% "2447353010690") 
  ex.COND.data <-COND %>% filter(PLT_CN %in% plt) 
  
  
  exampEnviro = data.frame(
    slope = ex.COND.data$SLOPE,
    windspeed = 40, # windspeed (at 10m, open)
    direction = 0, # direction of wind, from uphill
    waf = 0.2 # Wind adjustment factor
  )
  
  
  # now lets calculate the TI and CI
  #plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
  ex.2 <-TI.CI.list <- list()
  
  for(i in 1:length(plt.characteristics$plot)){
    if(exampCrownFuel[i,"CBD"] == 0 | exampCrownFuel[i,"CFL"] == 0){
      ex.2[[i]] <- ex.2[[i-1]]
      ex.2[[i]]$fireBehavior$`Torching Index [m/min]` <- 0
      ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]` <- 0
    }else{
    ex.2[[i]] = rothermel(exampSurfFuel, exampFuelMoisture, exampCrownFuel[i,], exampEnviro)
    }
    
    TI.CI.list[[i]] <- data.frame(TI =  ex.2[[i]]$fireBehavior$`Torching Index [m/min]`, 
                           CI =  ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]`, 
                           PLT_CN = plt, 
                           mort.scheme = plt.characteristics[i,]$mort.scheme, 
                           scenario = plt.characteristics[i,]$scenario, 
                           time= plt.characteristics[i,]$time)
    
  }
  #ex.2 = rothermel(exampSurfFuel, exampFuelMoisture, exampCrownFuel, exampEnviro)
  # ex.2$fireBehavior$`Torching Index [m/min]`
  # ex.2$fireBehavior$`Crowning Index [km/hr]`
  # 
  # 
 
  
  TI.CI.df <- do.call(rbind, TI.CI.list)
  TI.CI.df
}

a <- get_torch_crown_indices_FORECASTS(plotnos[170])

# get it for all the plotnos:
# this takes some time on my computer...
# i switched to a for loop to see where the lapply function broke down...we got a warning about a nonnumeric SI
for(i in 170:length(unique(plotnos))){
TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(unique(plotnos)[i])
}


TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)

# lets join plot these over time for each plot:

ggplot(TI.CI.FORECASTS, aes(x = time, y = TI, color = PLT_CN))+geom_line()+facet_wrap(~mort.scheme)
