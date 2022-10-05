# reading in the available forecasts (1-184 I believe)

get_biomass_ests <- function(plot, mort.scheme, scenario){
  
  load(paste0("forecasts_AGB/biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  
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

plot <- unique(plots)[1]

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

get_biomass_ests(plot = "2447353010690", mort.scheme = "DDonly",scenario = "rcp85")

# list files and get all the plot numbers:

filenames <- list.files(path = "forecasts_AGB/biomass_data", pattern = "rcp85")

# get the plot numbers that were run
plotnos <- do.call(rbind, stringr::str_split(filenames, pattern = "\\."))[,2]
unique(plotnos)

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
normort.AGB.26 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp26")
DIonly.AGB.26 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp26")
DDonly.AGB.26 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp26")
DIDD.AGB.26 <- lapply(unique(plotnos)[1:151], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp26")



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
AGB.line <- ggplot()+geom_line(data = ten.plot.summary, aes(year, mAGB, group = mort.scheme, color = mort.scheme))+theme_bw()+ylab("Total AGB for 10 plots \n (Mg/ha), RCP 8.5")+facet_wrap(~rcp)
NPP.line <- ggplot(ten.plot.summary, aes(year, mNPP, group = mort.scheme, color = mort.scheme))+geom_line()+theme_bw()+ylab("Total NPP for 10  plots \n (Mg/ha), RCP 8.5")+facet_wrap(~rcp)

png(height = 7, width = 10, units = "in", res = 150, "forecasts_AGB/output/allrcps.example151plots.total.biomass.png")
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
  ylab(paste("ten plot total biomass \n  (Mg/ha)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black" ))

b.plot

png(height = 10, width = 12, units = "in", res = 150, "forecasts_AGB/output/Total_biomass_kaye_ten_plots_allrcps_nocc.png")
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
get_tree_levelC_ests <- function(plot, mort.scheme, scenario, nocc = FALSE){
  if(nocc == FALSE){
    load(paste0("forecasts_AGB/biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }else{
    load(paste0("forecasts_AGB/biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
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
  
  total.biomass.bytree.melt <- melt(total.biomass.bytree[2,,])
  colnames(total.biomass.bytree.melt) <- c("tree", "time", "AGB")
  
  diam.biomass.df <- left_join(diam.live.melt, total.biomass.bytree.melt, by = c("tree", "time"))
  
  
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
btst.AGB.DIDD.26 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26")})
btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
btst.AGB.DIDD.26.df

btst.AGB.nomort.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26")})
btst.AGB.nomort.26.df <- do.call(rbind, btst.AGB.nomort.26)

btst.AGB.DDonly.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26")})
btst.AGB.DDonly.26.df <- do.call(rbind, btst.AGB.DDonly.26)

btst.AGB.DIonly.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26")})
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


# ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
#   summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)


png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_all_50_plots.png")
ggplot(na.omit(allplots.treeC), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
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

png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_dominated_nplots.png")

ggplot()+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.big), color = "big tree dominated")+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.small), color= "small tree dominated")+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.mixed), color= "mixed dominated")+
  scale_color_manual(name = 'Size class', 
                     values =c("big tree dominated"="#e66101","small tree dominated"="#5e3c99", "mixed dominated" = "black"))+
  facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))+
  
  ylab(paste("number of plots dominated by big vs small trees"))+    xlab("Year")+
  
  theme_bw()+theme(panel.grid = element_blank())

dev.off()
# plot the number of plots dominated by small vs large trees:
png(height = 10, width = 10, units = "in", res = 150, "forecasts_AGB/output/big_tree_vs_small_tree_dominated_prop_biomass.png")

ggplot()+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=prop.big.all, ymax=prop.big.all + prop.small.all), fill = "#5e3c99")+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=0, ymax=prop.big.all), fill = "#e66101")+
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
    load(paste0("forecasts_AGB/biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }else{
    load(paste0("forecasts_AGB/biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }

  # get live diameters
  diam.live.melt <- melt(diam.live[2,,])
  diam.live.melt$size_class <- ifelse(diam.live.melt$value<= 0.1, NA, 
                                      ifelse(diam.live.melt$value <= 30, "small tree", "big tree"))
  colnames(diam.live.melt) <- c("tree", "time", "DBH", "size_class")
  
  # get dead diameters
  diam.dead.melt <- melt(diam.dead[2,,])
  diam.dead.melt$size_class <- ifelse(diam.dead.melt$value<= 0.1, NA, 
                                      ifelse(diam.dead.melt$value <= 30, "small tree", "big tree"))
  colnames(diam.dead.melt) <- c("tree", "time", "DBH", "size_class")
  
  summary(diam.dead.melt)
  
  diam.dead.melt$status <- ifelse(diam.dead.melt$DBH == 0, "live", "dead")
  diam.live.melt$status <- ifelse(diam.live.melt$DBH == 0.1, "dead", "live")
  
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

# read in and get the tree level estimates
# RCP 2.6
btst.DIAMS.DIDD.26 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26")})
btst.DIAMS.DIDD.26.df <- do.call(rbind, btst.DIAMS.DIDD.26)
btst.DIAMS.DIDD.26.df

btst.DIAMS.nomort.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26")})
btst.DIAMS.nomort.26.df <- do.call(rbind, btst.DIAMS.nomort.26)

btst.DIAMS.DDonly.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26")})
btst.DIAMS.DDonly.26.df <- do.call(rbind, btst.DIAMS.DDonly.26)

btst.DIAMS.DIonly.26<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26")})
btst.DIAMS.DIonly.26.df <- do.call(rbind, btst.DIAMS.DIonly.26)

# RCP 4.5
btst.DIAMS.DIDD.45 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45")})
btst.DIAMS.DIDD.45.df <- do.call(rbind, btst.DIAMS.DIDD.45)

btst.DIAMS.nomort.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp45")})
btst.DIAMS.nomort.45.df <- do.call(rbind, btst.DIAMS.nomort.45)

btst.DIAMS.DDonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp45")})
btst.DIAMS.DDonly.45.df <- do.call(rbind, btst.DIAMS.DDonly.45)

btst.DIAMS.DIonly.45<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp45")})
btst.DIAMS.DIonly.45.df <- do.call(rbind, btst.DIAMS.DIonly.45)

# RCP 6.0
btst.DIAMS.DIDD.60 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60")})
btst.DIAMS.DIDD.60.df <- do.call(rbind, btst.DIAMS.DIDD.60)

btst.DIAMS.nomort.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp60")})
btst.DIAMS.nomort.60.df <- do.call(rbind, btst.DIAMS.nomort.60)

btst.DIAMS.DDonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp60")})
btst.DIAMS.DDonly.60.df <- do.call(rbind, btst.DIAMS.DDonly.60)

btst.DIAMS.DIonly.60<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp60")})
btst.DIAMS.DIonly.60.df <- do.call(rbind, btst.DIAMS.DIonly.60)

# RCP 8.5
btst.DIAMS.DIDD.85 <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85")})
btst.DIAMS.DIDD.85.df <- do.call(rbind, btst.DIAMS.DIDD.85)

btst.DIAMS.nomort.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp85")})
btst.DIAMS.nomort.85.df <- do.call(rbind, btst.DIAMS.nomort.85)

btst.DIAMS.DDonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp85")})
btst.DIAMS.DDonly.85.df <- do.call(rbind, btst.DIAMS.DDonly.85)

btst.DIAMS.DIonly.85<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp85")})
btst.DIAMS.DIonly.85.df <- do.call(rbind, btst.DIAMS.DIonly.85)

# no climate change scenario:
# read in and get the tree level estimates
# RCP 2.6
btst.DIAMS.DIDD.nocc <- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", nocc = TRUE)})
btst.DIAMS.DIDD.nocc.df <- do.call(rbind, btst.DIAMS.DIDD.nocc)
btst.DIAMS.DIDD.nocc.df

btst.DIAMS.nomort.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", nocc = TRUE)})
btst.DIAMS.nomort.nocc.df <- do.call(rbind, btst.DIAMS.nomort.nocc)

btst.DIAMS.DDonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", nocc = TRUE)})
btst.DIAMS.DDonly.nocc.df <- do.call(rbind, btst.DIAMS.DDonly.nocc)

btst.DIAMS.DIonly.nocc<- lapply(unique(plotnos)[1:151], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", nocc = TRUE)})
btst.DIAMS.DIonly.nocc.df <- do.call(rbind, btst.DIAMS.DIonly.nocc)



# checking to see if the mort trees are being labeled properly:
ggplot(btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]), aes(x = time, y = DBH, group = tree, color = df))+geom_point()


filtered.test <- btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]) %>% filter(status == df)
ggplot(filtered.test, aes(x = time, y = DBH, group = tree, color = df))+geom_point()

# combine all the tree-level datasets together:
allplots.treeDIAM <- rbind(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df, 
                        btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
                        btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df, 
                        btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df, 
                        btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)

# this gets rid of trees labeled as dead in the live df and vice versa
allplots.treeDIAMsubset <- allplots.treeDIAM %>% filter(status == df)

region.ndead <- allplots.treeDIAMsubset %>% group_by(df,  mort.scheme, scenario, time, status) %>% summarise(ntree = n())
ggplot(region.ndead, aes(x = time, y = ntree, color = status))+geom_point()+facet_wrap(~mort.scheme)

# summarize the dead tree mortality rate by plot, and compare to the results of FIA data analyses
# 
plot.ndead <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time, status) %>% summarise(ntree = n())
plot.ndead.spread <- plot.ndead %>% ungroup() %>% select(-df) %>% group_by(plot, mort.scheme, scenario, time) %>% spread(status, value = ntree)
plot.prop.dead <- plot.ndead.spread %>% group_by(plot, mort.scheme, scenario, time) %>% mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))


# crazy line plot
ggplot(plot.prop.dead, aes(x = time, y = prop.dead, group = plot))+geom_line()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

# proportion of dead trees in 2018 :
ggplot(plot.prop.dead %>% filter(time == 17), aes(x =  prop.dead))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

# plot size distribution of dead trees in 2018

all.trees.2018 <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time, status) %>% filter(time == 17)
ggplot(all.trees.2018, aes(x =  DBH, fill = status))+geom_histogram(position = "identity", alpha = 0.7)+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

