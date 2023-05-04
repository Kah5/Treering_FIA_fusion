library(viridis)
library(here)
library(tidyverse)
#Parse apart the SDI and climate change effects sensitivity effects on AGB
parse_biomass_ests <- function(plot, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, rcp, cc.scenario = "doubleCC" ){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
   cat("less than 2 trees on the first plot")
  }else{
    if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))){
     cat("no existing future climate data") 
    }else{
    # newTREE <- TREE %>% dplyr::filter (PREV_PLT_CN %in% plot)
    # newTREE$PREVDIA
    # STATUSCD_change <- newTREE %>% group_by(STATUSCD,  PREV_STATUS_CD) %>% 
    #   mutate(dead.class = ifelse(STATUSCD == 1 & PREV_STATUS_CD == 1, "live", 
    #                              ifelse(STATUSCD == 2 & PREV_STATUS_CD == 1, "died in inventory", 
    #                                     ifelse(STATUSCD == 2 & PREV_STATUS_CD == 2, "died before first survey",
    #                                            ifelse(STATUSCD == 1 & is.na(PREV_STATUS_CD) == "TRUE","ingrowth", 
    #                                                   ifelse(STATUSCD == 3 & PREV_STATUS_CD == 1,"cut/removed in inventory", 
    #                                                          ifelse(STATUSCD == 3 & PREV_STATUS_CD == 3, "cut/removed before first survey", "ingrowth")))))))
    # newTREE$INV_Period <- newTREE$MEASYR - newTREE$PREV_MEASYR
    # INVperiod <- mean(newTREE$INV_Period, na.rm =TRUE)
    # STATUSCD_change$PREVDIA*2.54
    
    
    
    #STATUSCD_change$DRYBIO_AG
    oldTREE$DRYBIO_AG
    
    # dead.diams <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% 
    #   dplyr::select(CN, dead.class, PREVDIA, TPAMORT_UNADJ, TPA_UNADJ, PREV_TPA_UNADJ, AGENTCD) %>% mutate(PREVDBH = PREVDIA*2.54)
    # #rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
    # mort.scheme <- "DIonly"
    
      if(cc.scenario == "doubleCC"){
    
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      #load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
    }
    
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
    hiNPP <-hiNPP.foliage <-  hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
    
    
    
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
                             rcp = rcp,
                             cc.scenario = cc.scenario,
                             parse = "full",
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
                             
                             #up.dead = up.dead[2:length(low.stemwood)], 
                             #low.dead = low.dead[2:length(low.stemwood)],
                             
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
    
    # obs.biomass <- data.frame(plot = c(plot,plot), 
    #                           DRYBIO_AG_lb_ha = c(sum(oldTREE$DRYBIO_AG*oldTREE$TPA_UNADJ), sum(STATUSCD_change$DRYBIO_AG*STATUSCD_change$TPA_UNADJ, na.rm =TRUE)),
    #                           year = c(unique(oldTREE$MEASYR), unique(STATUSCD_change$MEASYR)) )
    # 
    # # convert lbs to kg
    
    #obs.biomass$DRYBIO_AG_kg_ha <-  obs.biomass$DRYBIO_AG_lb_ha/2.205
    
   # total.plot.obs <- left_join(total.plot, obs.biomass, by = c("plot", "year"))
    #total.plot. <- total.plot
    
    cat("reading in no SDI results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_noSDI/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".noSDI.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_noSDI/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".noSDI.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
    }
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
    hiNPP <-hiNPP.foliage <-  hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
    
    
    
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
    
    total.plot.nosdi <- data.frame(plot = plot, 
                                   mort.scheme = mort.scheme, 
                                   rcp = rcp,
                                   cc.scenario = cc.scenario,
                                   parse = "no SDI",
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
                                   
                                   #up.dead = up.dead[2:length(low.stemwood)], 
                                   #low.dead = low.dead[2:length(low.stemwood)],
                                   
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
    
    # obs.biomass <- data.frame(plot = c(plot,plot), 
    #                           DRYBIO_AG_lb_ha = c(sum(oldTREE$DRYBIO_AG*oldTREE$TPA_UNADJ), sum(STATUSCD_change$DRYBIO_AG*STATUSCD_change$TPA_UNADJ, na.rm =TRUE)),
    #                           year = c(unique(oldTREE$MEASYR), unique(STATUSCD_change$MEASYR)) )
    # 
    # # convert lbs to kg
    # 
    # obs.biomass$DRYBIO_AG_kg_ha <-  obs.biomass$DRYBIO_AG_lb_ha/2.205
    # 
    #total.plot.obs.nosdi <- left_join(total.plot.nosdi, obs.biomass, by = c("plot", "year"))
    
    cat("reading in no CC results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_noCC/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".detrendedCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_noCC/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".detrendedCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
    }
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
    hiNPP <-hiNPP.foliage <-  hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
    
    
    
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
    
    total.plot.noprecip <- data.frame(plot = plot, 
                                      mort.scheme = mort.scheme, 
                                      rcp = rcp,
                                      cc.scenario = cc.scenario,
                                      parse = "no climate change",
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
                                      
                                      #up.dead = up.dead[2:length(low.stemwood)], 
                                      #low.dead = low.dead[2:length(low.stemwood)],
                                      
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
    
    # obs.biomass <- data.frame(plot = c(plot,plot),
    #                           DRYBIO_AG_lb_ha = c(sum(oldTREE$DRYBIO_AG*oldTREE$TPA_UNADJ), sum(STATUSCD_change$DRYBIO_AG*STATUSCD_change$TPA_UNADJ, na.rm =TRUE)),
    #                           year = c(unique(oldTREE$MEASYR), unique(STATUSCD_change$MEASYR)) )
    # 
    # # convert lbs to kg
    
    #obs.biomass$DRYBIO_AG_kg_ha <-  obs.biomass$DRYBIO_AG_lb_ha/2.205
    
    
    total.plot.obs.noprecip <- total.plot.noprecip
    
   
   
    
    
    total.plot.obs.all <- rbind(total.plot, total.plot.nosdi, total.plot.noprecip)
    total.plot.obs.all
    
    
  }
  }
}
plots <- unique(cov.data.regional$PLT_CN)
plot <- unique(plots)[2]
mort.scheme = "DIonly"
SDI.ratio.DD = 0.8
cc.scenario = "singleCC"

unique(plots) %in% 2584218010690
DIDD.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC" )})
DIDD.rcp85.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC" )})
DIDD.rcp45.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC" )})
DIDD.rcp60.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC" )})





# get the no Density Dependent mortality forecasts too:
DIonly.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC" )})
DIonly.rcp85.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC" )})
DIonly.rcp45.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC" )})
DIonly.rcp60.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC" )})

# get the Density Dependent mortality forecasts too:
DDonly.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC" )})
DDonly.rcp85.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC" )})
DDonly.rcp45.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC" )})
DDonly.rcp60.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC" )})

unique(plots) %in% 3081205010690

DDonly.parse.df <- do.call(rbind, DDonly.parse.list)
DDonly.rcp45.parse.df <- do.call(rbind, DDonly.rcp45.parse.list)
DDonly.rcp60.parse.df <- do.call(rbind, DDonly.rcp60.parse.list)
DDonly.rcp85.parse.df <- do.call(rbind, DDonly.rcp85.parse.list)
parse.DD.mort <- rbind( DDonly.parse.df, DDonly.rcp45.parse.df, DDonly.rcp60.parse.df,DDonly.rcp85.parse.df)
saveRDS(parse.DD.mort, "outputs/parse.DIDD.mort.RDS")
parse.DD.mort <- readRDS( "outputs/parse.DD.mort.RDS")


#DDonly.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests(plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})
#DIonly.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})
#nomort.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests(plot = x, mort.scheme = "nomort",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})


DIDD.parse.df <- do.call(rbind, DIDD.parse.list)
DIDD.rcp45.parse.df <- do.call(rbind, DIDD.rcp45.parse.list)
DIDD.rcp60.parse.df <- do.call(rbind, DIDD.rcp60.parse.list)
DIDD.rcp85.parse.df <- do.call(rbind, DIDD.rcp85.parse.list)

parse.DIDD.mort <- rbind( DIDD.parse.df, DIDD.rcp45.parse.df, DIDD.rcp60.parse.df, DIDD.rcp85.parse.df)
saveRDS(parse.DIDD.mort, "outputs/parse.DIDD.mort.RDS")


DIonly.parse.df <- do.call(rbind, DIonly.parse.list)
DIonly.rcp45.parse.df <- do.call(rbind, DIonly.rcp45.parse.list)
DIonly.rcp60.parse.df <- do.call(rbind, DIonly.rcp60.parse.list)
DIonly.rcp85.parse.df <- do.call(rbind, DIonly.rcp85.parse.list)


parse.all.mort <- rbind(DIDD.parse.df, DIDD.rcp45.parse.df, DIDD.rcp60.parse.df,DIDD.rcp85.parse.df,
  DIonly.parse.df, DIonly.rcp45.parse.df, DIonly.rcp60.parse.df,DIonly.rcp85.parse.df)

saveRDS(parse.all.mort, "outputs/parse.all.mortDI_DIDD.RDS")
parse.all.mort <- readRDS("outputs/parse.DIDD.mort.RDS")
parse.all.mort$plot <- as.character(parse.all.mort$plot)
#parse.DD.mort$plot <- as.character(parse.DD.mort$plot)

#parse.all.mort <- rbind(parse.all.mort, parse.DD.mort)
# subtract the scenarios from the full scenario for the mean AGB:
AGB.parse.dCC <- parse.all.mort %>% select(plot, rcp, mort.scheme, year, parse, mAGB) %>% group_by(plot, mort.scheme, year, parse) %>%
  spread(parse, mAGB) %>% mutate(climatechangediff = full - `no climate change`, 
                                 #tmaxdiff = full - `no tmax`, 
                                 SDIdiff = full - `no SDI`, 
                                 climatechangediff.pct = ((full - `no climate change`)/full)*100, 
                                 #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
                                 SDIdiff.pct = ((full - `no SDI`)/full)*100)

AGB.parse.dCC.summary <- AGB.parse.dCC %>% ungroup() %>% group_by(rcp, mort.scheme, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE),)

ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = climatechangediff.median, color = mort.scheme))+geom_line()+
  geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))

# ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = tmaxdiff.median, color = mort.scheme))+geom_line()+
#   geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = tmaxdiff.median - tmaxdiff.sd, ymax = tmaxdiff.median + tmaxdiff.sd,  fill = mort.scheme))+
#   facet_wrap(~mort.scheme)
# 
# ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = SDIdiff.median, color = mort.scheme))+geom_line()+
#   geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = SDIdiff.median - SDIdiff.sd, ymax = SDIdiff.median + SDIdiff.sd,  fill = mort.scheme))+
#   facet_wrap(~mort.scheme)
#------------------------- Parse DI and DD mortality contributions--------------------------------------
parse_mortality_ests <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.8, rcp, cc.scenario = "doubleCC", parse ){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    
    if(parse == "full"){
      fn <- paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".", parse,".Rdata")
    }else{
      if(parse == "noSDI"){
        fn <- paste0("biomass_dataFIAperiodic_noSDI/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
           }else{
        
             fn <-  paste0("biomass_dataFIAperiodic_noCC/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
        
      }
    }
    
    
    
    if(!file.exists(fn)){
      cat("no existing future climate data") 
    }else{
   
      load(fn)
      
      
      
      # get a TPA out:
      #tpa.m$newVar3 <- rep(4:102, each = 100*ni)
      
      tpa.di.m <- tpa.di %>% filter(time %in% 1:99)
      tpa.di.m$id <- paste0("x[",tpa.di.m$treeno, ",", tpa.di.m$time,"]")
      
      TPA.DI.ordered <- tpa.di.m$id
      
      tpa.di.m.time <- tpa.di.m %>% dplyr::select(id, TPA) %>%  spread(key = id, value = TPA)
      
      
      # for dd
      tpa.dd.m <- tpa.dd %>% filter(time %in% 1:99)
      colnames(tpa.dd.m)[3] <- "TPAdd"
      
      tpa.di.m <- tpa.di %>% filter(time %in% 1:99)
      colnames(tpa.di.m)[3] <- "TPAdi"
      
      di.dd.tpa <- left_join(tpa.di.m, tpa.dd.m)
      
      tpa.full.m <- reshape2::melt(tpa.live)
      colnames(tpa.full.m) <- c("quantile", "treeno", "time", "TPAfull")
      tpa.full.m  <- tpa.full.m %>% filter(quantile == 2) %>% select(-quantile)
      full.di.dd.tpa <- left_join(di.dd.tpa, tpa.full.m)
      
      tpa.dead.m <- reshape2::melt(tpa.dead)
      colnames(tpa.dead.m) <- c("quantile", "treeno", "time", "TPAdead")
      tpa.dead.m <- tpa.dead.m %>% filter(quantile == 2) %>% select(-quantile)
      
      all.tpa <- left_join(full.di.dd.tpa, tpa.dead.m)
      all.tpa$PLT_CN <- as.character(plot)
      all.tpa
      
      i <- 1
      mplot <- 1
      nt <- ncol(NPP[i,,])
      
      # sequentially add up:
      # branchdead, then foliage, then stembark, then branchlive, then stemwood
      mAGB.dead <- sAGB.dead <- mAGB.dead.dd <- mAGB.dead.di <- sAGB.dead.dd <- sAGB.dead.di<- matrix(NA, mplot, nt)
      #mNPP.dead <- sNPP.dead  <- matrix(NA, mplot,nt)
      lowAGB.dead <- hiAGB.dead  <- lowAGB.dead.di <- hiAGB.dead.di <-  lowAGB.dead.dd <- hiAGB.dead.dd  <-matrix(NA, mplot, nt)
      #lowNPP.dead <- hiNPP.dead  <-matrix(NA, mplot,nt)
      AGB.dead.dd <- AGB.dead.di <- AGB.dead <-  array(NA, c(mplot, nrep, nt))
      
      
      # need to create a matrix for tpadead.dd & tpadead.di
      tpa.dead.dd  <- tpa.dd.m %>% ungroup() %>% group_by(time) %>% spread(time, TPAdd) %>% select(`1`:`99`)
      tpa.dead.di  <- tpa.di.m %>% ungroup() %>% group_by(time) %>% spread(time, TPAdi) %>% select(`1`:`99`)
    
      nrep    <- 3
      for (g in seq_len(nrep)) {
  
        j <- 1
      AGB.dead[j, g, ] <- apply(biomass.dead[g,,]*tpa.dead[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
      AGB.dead.di[j, g, ] <- apply(biomass.dead[g,,]*tpa.dead.di[,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
      AGB.dead.dd[j, g, ] <- apply(biomass.dead[g,,]*tpa.dead.dd[,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
      
      }
     
      
      # dead trees:

      mAGB.dead[i, ] <- apply(AGB.dead[i, , ], 2, median, na.rm = TRUE)
      sAGB.dead[i, ] <- apply(AGB.dead[i, , ]  , 2, sd, na.rm = TRUE)
      
      lowAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
      hiAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
      
      # parse into dead from DI vs dead from DD
      
      mAGB.dead.di[i, ] <- apply(AGB.dead.di[i, , ], 2, median, na.rm = TRUE)
      sAGB.dead.di[i, ] <- apply(AGB.dead.di[i, , ]  , 2, sd, na.rm = TRUE)
      
      lowAGB.dead.di[i,]<- apply(AGB.dead.di[i, , ], 2, quantile, na.rm = TRUE, 0.025)
      hiAGB.dead.di[i,] <- apply(AGB.dead.di[i, , ], 2, quantile, na.rm = TRUE, 0.975)
      
      
      mAGB.dead.dd[i, ] <- apply(AGB.dead.dd[i, , ], 2, median, na.rm = TRUE)
      sAGB.dead.dd[i, ] <- apply(AGB.dead.dd[i, , ]  , 2, sd, na.rm = TRUE)
      
      lowAGB.dead.dd[i,]<- apply(AGB.dead.dd[i, , ], 2, quantile, na.rm = TRUE, 0.025)
      hiAGB.dead.dd[i,] <- apply(AGB.dead.dd[i, , ], 2, quantile, na.rm = TRUE, 0.975)
      
      # get total dead: 
      tpa.dead.dd.sum <- colSums(tpa.dead.dd)
      tpa.dead.di.sum <- colSums(tpa.dead.di)
      
      
      yrvec <- 2001:2099
      # okay now add all to a dataframe:
      total.plot <- data.frame(plot = plot, 
                               mort.scheme = mort.scheme, 
                               rcp = rcp,
                               cc.scenario = cc.scenario,
                               parse = parse,
                               year = yrvec, 
                               tpa.dead.dd =  tpa.dead.dd.sum, 
                               tpa.dead.di = tpa.dead.di.sum , 
                              
                      
                               mAGB.dead  = mAGB.dead[i,],
                               mAGB.dead.dd = mAGB.dead.dd[i,], 
                               mAGB.dead.di = mAGB.dead.di[i,], 
                               
                               lowAGB.dead = lowAGB.dead[i,], 
                               lowAGB.dead.dd = lowAGB.dead.dd[i,], 
                               lowAGB.dead.di = lowAGB.dead.di[i,], 
                               
                               hiAGB.dead = hiAGB.dead[i,], 
                               hiAGB.dead.dd = hiAGB.dead.dd[i,], 
                               hiAGB.dead.di = hiAGB.dead.di[i,])
      
      total.plot
    }
    
  }
}

plot = '2567520010690'
mort.26.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "full" )})
mort.26 <- do.call(rbind, mort.26.list)

mort.45.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "full" )})
mort.45.test <- do.call(rbind, mort.45.list)

mort.60.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "full" )})
mort.60.test <- do.call(rbind, mort.60.list)

mort.85.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "full" )})
mort.85.test <- do.call(rbind, mort.85.list)

mort.full.parse <- rbind(mort.26, mort.45.test, mort.60.test, mort.85.test)


# get it for noCC:

mort.test.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.test.noCC <- do.call(rbind, mort.test.list.noCC)

mort.45.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.45.test.noCC <- do.call(rbind, mort.45.list.noCC)

mort.60.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.60.test.noCC <- do.call(rbind, mort.60.list.noCC)

mort.85.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.85.test.noCC <- do.call(rbind, mort.85.list.noCC)


mort.full.parse.noCC <- rbind(mort.test.noCC, mort.45.test.noCC, mort.60.test.noCC, mort.85.test.noCC)

# get it for noSDI:

mort.test.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "noSDI" )})
mort.test.noSDI <- do.call(rbind, mort.test.list.noSDI)

mort.45.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "noSDI" )})
mort.45.test.noSDI <- do.call(rbind, mort.45.list.noSDI)

mort.60.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "noSDI" )})
mort.60.test.noSDI <- do.call(rbind, mort.60.list.noSDI)

mort.85.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "noSDI" )})
mort.85.test.noSDI <- do.call(rbind, mort.85.list.noSDI)


mort.full.parse.noSDI <- rbind(mort.test.noSDI, mort.45.test.noSDI, mort.60.test.noSDI, mort.85.test.noSDI)

mort.all.parse <- rbind(mort.full.parse, mort.full.parse.noSDI, mort.full.parse.noCC)

# save as RDS:
saveRDS(mort.all.parse, here("outputs/", "all.plot.mort.C.RDS"))
mort.all.parse <- readRDS( here("outputs/", "all.plot.mort.C.RDS"))

# create function to scale biomass to C and convert to Tg?
# Cfraction
C.convert.deadwood <- function(x, C.frac = 0.4822){(x*C.frac)/1000000}
C.convert.deadwood(mort.all.parse$mAGB.dead, C.frac = 0.4822)

# get general summary of the total mortality in terms of C for each mortality type
mort.test <- mort.all.parse %>% group_by(plot, mort.scheme, rcp, year, parse) %>%
                                summarise(across(c(mAGB.dead:hiAGB.dead.di), function(x){C.convert.deadwood(x)})) %>% 
                                ungroup() %>% # sum across all the PLT_CNs
                                group_by(rcp, mort.scheme, year, parse) %>%
                                summarise(across(c(mAGB.dead:hiAGB.dead.di), sum))

#mort.test.m <- reshape2::melt(mort.test, id.vars = c("rcp", "mort.scheme", "year", "parse"))

ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = "Density Independent"))+
  geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = "Density Dependent"))+
  facet_grid(cols = vars(parse), rows = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C)")

ggsave(height = 6, width = 8, units = "in", here("outputs/", "Dead_Carbon_by_DI_DD_total_parse_periodic.png"))

# reorient it so it looks closer to the parse plots

ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.di, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
 # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C)")+scale_fill_manual( name = "Scenario",
                                                    values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))

  

ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.di, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C) \n Density Independent Mortality")+scale_fill_manual( name = "Scenario",
                                                     values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))

ggsave(height = 3, width = 8, units = "in", here("outputs/", "Dead_Carbon_by_DI_total_parse_periodic.png"))


ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.dd, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C) \n Density Dependent Mortality")+scale_fill_manual( name = "Scenario",
                                                     values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","detrendedCC"= "#d95f02", "noSDI"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))


ggsave(height = 3, width = 8, units = "in", here("outputs/", "Dead_Carbon_by_DD_total_parse_periodic.png"))

#--------------get the number & sizes of dead trees by mortality type and plot over time--------------------------

parse = full
parse_mortality_size <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.8, rcp, cc.scenario = "singleCC", parse ){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    
    if(parse == "full"){
      fn <- paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".", parse,".Rdata")
    }else{
      if(parse == "noSDI"){
        fn <- paste0("biomass_dataFIAperiodic_noSDI/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
      }else{
        
        fn <-  paste0("biomass_dataFIAperiodic_noCC/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
        
      }
    }
    
    
    
    if(!file.exists(fn)){
      cat("no existing future climate data") 
    }else{
      
      load(fn)
      
      
      
      # get a TPA out:
      #tpa.m$newVar3 <- rep(4:102, each = 100*ni)
      
      tpa.di.m <- tpa.di %>% filter(time %in% 1:99)
      tpa.di.m$id <- paste0("x[",tpa.di.m$treeno, ",", tpa.di.m$time,"]")
      
      TPA.DI.ordered <- tpa.di.m$id
      
      tpa.di.m.time <- tpa.di.m %>% dplyr::select(id, TPA) %>%  spread(key = id, value = TPA)
      
      
      # for dd
      tpa.dd.m <- tpa.dd %>% filter(time %in% 1:99)
      colnames(tpa.dd.m)[3] <- "TPAdd"
      
      tpa.di.m <- tpa.di %>% filter(time %in% 1:99)
      colnames(tpa.di.m)[3] <- "TPAdi"
      
      di.dd.tpa <- left_join(tpa.di.m, tpa.dd.m)
      
      tpa.full.m <- reshape2::melt(tpa.live)
      colnames(tpa.full.m) <- c("quantile", "treeno", "time", "TPAfull")
      tpa.full.m  <- tpa.full.m %>% filter(quantile == 2) %>% select(-quantile)
      full.di.dd.tpa <- left_join(di.dd.tpa, tpa.full.m)
      
      tpa.dead.m <- reshape2::melt(tpa.dead)
      colnames(tpa.dead.m) <- c("quantile", "treeno", "time", "TPAdead")
      tpa.dead.m <- tpa.dead.m %>% filter(quantile == 2) %>% select(-quantile)
      
      all.tpa <- left_join(full.di.dd.tpa, tpa.dead.m)
      all.tpa$PLT_CN <- as.character(plot)
      #all.tpa
      
      i <- 1
      mplot <- 1
      nt <- ncol(NPP[i,,])
      
      
      # median
      
      tree.size.m <- reshape2::melt(diam.dead[1,,])
      colnames(tree.size.m ) <- c("treeno", "time", "diameter")
      
      full.dead.diameter <- left_join(full.di.dd.tpa, tree.size.m)
      
      full.dead.diameter$PLT_CN <- plot
      full.dead.diameter$parse <- parse
      full.dead.diameter$rcp <- rcp
      full.dead.diameter$mort.scheme <- mort.scheme
      
      
      full.dead.diameter
    }
    
  }
}

mort.dbh.26.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "full" )})
mort.dbh.26 <- do.call(rbind, mort.dbh.26.list)

mort.dbh.45.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "full" )})
mort.dbh.45.test <- do.call(rbind, mort.dbh.45.list)

mort.dbh.60.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "full" )})
mort.dbh.60.test <- do.call(rbind, mort.dbh.60.list)

mort.dbh.85.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "full" )})
mort.dbh.85.test <- do.call(rbind, mort.dbh.85.list)

mort.dbh.full.parse <- rbind(mort.dbh.26, mort.dbh.45.test, mort.dbh.60.test, mort.dbh.85.test)


# get it for noCC:

mort.dbh.test.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.dbh.test.noCC <- do.call(rbind, mort.dbh.test.list.noCC)

mort.dbh.45.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.dbh.45.test.noCC <- do.call(rbind, mort.dbh.45.list.noCC)

mort.dbh.60.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.dbh.60.test.noCC <- do.call(rbind, mort.dbh.60.list.noCC)

mort.dbh.85.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "detrendedCC" )})
mort.dbh.85.test.noCC <- do.call(rbind, mort.dbh.85.list.noCC)


mort.dbh.full.parse.noCC <- rbind(mort.dbh.test.noCC, mort.dbh.45.test.noCC, mort.dbh.60.test.noCC, mort.dbh.85.test.noCC)

# get it for noSDI:

mort.dbh.test.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC", parse = "noSDI" )})
mort.dbh.test.noSDI <- do.call(rbind, mort.dbh.test.list.noSDI)

mort.dbh.45.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC", parse = "noSDI" )})
mort.dbh.45.test.noSDI <- do.call(rbind, mort.dbh.45.list.noSDI)

mort.dbh.60.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC", parse = "noSDI" )})
mort.dbh.60.test.noSDI <- do.call(rbind, mort.dbh.60.list.noSDI)

mort.dbh.85.list.noSDI <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC", parse = "noSDI" )})
mort.dbh.85.test.noSDI <- do.call(rbind, mort.dbh.85.list.noSDI)


mort.dbh.full.parse.noSDI <- rbind(mort.dbh.test.noSDI, mort.dbh.45.test.noSDI, mort.dbh.60.test.noSDI, mort.dbh.85.test.noSDI)

mort.dbh.all.parse <- rbind(mort.dbh.full.parse, mort.dbh.full.parse.noSDI, mort.dbh.full.parse.noCC)

# save as RDS:
saveRDS(mort.dbh.all.parse, here("outputs/", "all.plot.mort.dbh.N.RDS"))


# generate plots of dead by size class
mort.dbh.all.parse <- mort.dbh.all.parse  %>%
                                          mutate(dbh.class = cut(diameter, breaks=c(0, 10, 20, 30, 120)))

n.mort.dbh.class <- mort.dbh.all.parse %>% group_by(mort.scheme, rcp, time, parse, dbh.class) %>%
                        summarise(n.mort.dd = sum(TPAdd, na.rm = TRUE), 
                                  n.mort.di = sum(TPAdi, na.rm = TRUE), 
                                  n.total = sum(TPAfull, na.rm = TRUE))

parse.names <- data.frame(parse = c("full", "detrendedCC", "noSDI"), 
                          scenario = c("full", "no climate change", "no SDI effect on growth"))

n.mort.dbh.class <- left_join(n.mort.dbh.class, parse.names)

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, color = dbh.class))+geom_line()+
  facet_grid(rows = vars(parse), cols = vars(rcp))

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, color = scenario))+geom_line()+
  facet_grid(rows = vars(dbh.class), cols = vars(rcp))

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.di, color = dbh.class))+geom_line()+
  facet_grid(rows = vars(parse), cols = vars(rcp))

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.di, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density independent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_by_DBH_DI_parse_periodic.png"))


ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density dependent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_by_DBH_DI_parse_periodic.png"))


ggplot(n.mort.dbh.class, aes(x = time, y = n.total, color = dbh.class))+geom_line()+
  facet_grid(rows = vars(parse), cols = vars(rcp))

#------------------------get regional differences for the Components-----------------------------------
# sum up across plots, then take parse differences:
get_component_diffs <- function(component, parse.all.mort){
  
  parse.difference.df  <- parse.all.mort %>% group_by(mort.scheme, parse, rcp, year) %>% 
    summarise(across(c(mAGB:low.foliage), function(x){(x*0.5)/1000000})) %>%
    summarise(across(c(mAGB:low.foliage), sum)) %>% # sum across all the plots
    select(rcp, mort.scheme, year, parse, UQ(sym(component))) %>% 
    group_by( mort.scheme, year, parse) %>%
    spread(parse, UQ(sym(component))) %>% 
    
    mutate(climatechangediff = full - `no climate change`, 
                                  #tmaxdiff = full - `no tmax`, 
                                  SDIdiff = full - `no SDI`, 
                                  climatechangediff.pct = ((full - `no climate change`)/full)*100, 
                                  #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
                                  SDIdiff.pct = ((full - `no SDI`)/full)*100)
  parse.difference.df 
  
}


# get parse differences by plots, then sum differences:
get_component_plot_diffs <- function(component, parse.all.mort){
  
  parse.difference.df  <- parse.all.mort %>% group_by(plot, mort.scheme, parse, rcp, year) %>% 
    summarise(across(c(mAGB:low.foliage), function(x){(x*0.5)/1000000})) %>% 
    ungroup() %>% # sum across all the plots
    select(rcp, plot, mort.scheme, year, parse, UQ(sym(component))) %>% 
    group_by(plot, rcp, mort.scheme, year, parse) %>%
    spread(parse, UQ(sym(component))) %>% 
    
    mutate(climatechangediff = full - `no climate change`, 
           #tmaxdiff = full - `no tmax`, 
           SDIdiff = full - `no SDI`, 
           climatechangediff.pct = ((full - `no climate change`)/full)*100, 
           #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
           SDIdiff.pct = ((full - `no SDI`)/full)*100) %>%
    ungroup() %>% select(rcp, mort.scheme, year, climatechangediff, SDIdiff) %>% 
    group_by(rcp, mort.scheme, year) %>%
    summarise(across(c(climatechangediff :SDIdiff), sum)) 
  parse.difference.df 
  
}

# for the total C
upA.parse.plot <- get_component_plot_diffs(component = "upA", parse.all.mort)
lowA.parse.plot <- get_component_plot_diffs("lowA", parse.all.mort)

upA.parse.m <- reshape2::melt(upA.parse.plot, id.vars = c("rcp", "mort.scheme", "year"))
lowA.parse.m <- reshape2::melt(lowA.parse.plot, id.vars = c("rcp", "mort.scheme", "year"))
colnames(upA.parse.m)[5] <- "upA"
colnames(lowA.parse.m)[5] <- "lowA"

CI.A.parse <- left_join(upA.parse.m, lowA.parse.m)
parse.differences <- CI.A.parse %>% filter(variable %in% c("climatechangediff", "SDIdiff"))

parse.differences$year <- as.numeric(parse.differences$year)

parse.differences

# ggplot()+geom_line(data = parse.differences , aes(x = year, y = lowA,  color = variable))+facet_wrap(~rcp)
# ggplot()+geom_line(data = parse.differences , aes(x = year, y = upA,  color = variable))+facet_wrap(~rcp)

# why wont this plot?
ggplot()+geom_ribbon(data = parse.differences , aes(x = year, ymin = lowA, ymax = upA, fill = variable))+facet_wrap(~rcp)


# notes: based on this I don't think we should be removing the SDI effect entirely---
# SDI + climate change scenario?
# is this because SDI x X or SDI x climate interactions? can we remove just the time varying effect of SDI?
# for NPP
# get_component_diffs("up", parse.all.mort)
# get_component_diffs("low", parse.all.mort)



#parse.all.mort <- parse.DIDD.mort # use only DIDD runs
colnames(parse.all.mort)[1] <- "PLT_CN"


# plot each of the summed forecasts for the region:

C.convert.livewood <- function(x, C.frac = 0.501){(x*C.frac)/1000000}
C.convert.livewood(mort.all.parse$mAGB.dead, C.frac = 0.501)

AGB.parse.totals  <- parse.all.mort %>% #select(PLT_CN, rcp, mort.scheme, year, parse, mAGB) %>% 
  group_by(PLT_CN, mort.scheme,rcp, year, parse) %>%
  
  #spread(parse, mAGB) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){C.convert.livewood(x)})) %>% 
  ungroup() %>% # sum across all the PLT_CNs
    group_by(rcp, mort.scheme, year, parse) %>%
  summarise(across(c(mAGB:low.foliage), sum)) 


# More about the parse scenarios:
#1. No SDI = 0SDI in the growth equation
#2. DIonly, No SDI = 0SDI in the growth equation, no density dependent mortality (parse mortality + growth effect of SDI on Carbon)
#3. no Climate change, DIDD-- parse climate effect on Carbon, including all mortality
#4. no Climate change, DIDD-- parse only climate effect on Carbon, excluding density dependent mortality (do we need this)

#5. Full ==full forecast scenario

# rename the AGB.parse.totals and filter
AGB.parse.totals$parse.new <- ifelse(AGB.parse.totals$parse %in% c("no SDI") & AGB.parse.totals$mort.scheme %in% "DIonly", "no SDI growth & mortality", 
                                     ifelse(AGB.parse.totals$parse %in% c("no SDI") & AGB.parse.totals$mort.scheme %in% "DIDD", "no SDI growth", 
                                            ifelse(AGB.parse.totals$parse %in% c("no climate change") & AGB.parse.totals$mort.scheme %in% "DDonly", "no climate change, DDonly",
                                                   ifelse(AGB.parse.totals$parse %in% c("no climate change") & AGB.parse.totals$mort.scheme %in% "DIonly", "no climate change, DIonly" , 
                                                          ifelse(AGB.parse.totals$parse %in% c("full") & AGB.parse.totals$mort.scheme %in% "DIonly", "full, DIonly",AGB.parse.totals$parse)))))

AGB.parse.totals$parse.type <- ifelse(AGB.parse.totals$mort.scheme %in% "DIonly" & AGB.parse.totals$parse.new %in% "no SDI growth & mortality", "in", 
                                     ifelse(AGB.parse.totals$mort.scheme %in% "DIDD", "in", 
                                            ifelse(AGB.parse.totals$parse.new %in% "no climate change, DDonly", "in",
                                                   ifelse(AGB.parse.totals$parse.new %in% "no climate change, DIonly", "in", 
                                                          ifelse(AGB.parse.totals$parse.new %in% "full, DIonly","in","out")))))


new.AGB.parse.totals <- AGB.parse.totals %>% filter(parse.type %in% "in")                              
# plot the totals
parse.Carbon.totals <- ggplot()+geom_ribbon(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002), aes(x = year, ymin = lowA, ymax = upA, fill = parse.new), alpha = 0.5)+
  geom_line(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, y = mAGB, color = parse.new))+
  
  facet_wrap(~rcp, ncol = 4)+ ylab( "Total Carbon in PIPO FIA stands\n (Tg C)") + theme_bw(base_size = 14)+theme(panel.grid = element_blank())+
  scale_fill_manual( name = "Scenario",
                     values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))

  

parse.Carbon.flux.totals <- ggplot()+geom_ribbon(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, ymin = low, ymax = up, fill = parse.new), alpha = 0.5)+
  geom_line(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, y = mNPP, color = parse.new))+
  
  facet_wrap(~rcp, ncol = 4)+theme_bw(base_size = 14) + ylab( "Carbon difference \n (Tg C)")+theme(panel.grid = element_blank())+
  scale_fill_manual( name = "Scenario",
                     values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                     values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))



Carbon.legend <- cowplot::get_legend(parse.Carbon.flux.totals)

png(height = 7, width = 12, units = "in", res = 300, "outputs/Carbon_density_regional_NPP_total_parse_periodic.png")
cowplot::plot_grid(cowplot::plot_grid(parse.Carbon.totals+theme(legend.position = "none", axis.text.x = element_text(hjust = 1, angle = 45)), 
                                      parse.Carbon.flux.totals  +theme(legend.position = "none", axis.text.x = element_text(hjust = 1, angle = 45)), 
                                      ncol = 1, align = "hv"), Carbon.legend, ncol = 2, rel_widths = c(0.65, 0.1))
dev.off()


# plot dead carbon totals by parse scenarios:

parse.deadCarbon.totals <- ggplot()+geom_ribbon(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002), aes(x = year, ymin = lowA.dead, ymax = upA.dead, fill = parse.new), alpha = 0.5)+
  geom_line(data = new.AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, y = mAGB.dead, color = parse.new))+
  
  facet_wrap(~rcp, ncol = 4)+ ylab( "Dead Carbon \n (Tg C)") + theme_bw(base_size = 14)+theme(panel.grid = element_blank())+
  scale_fill_manual( name = "Scenario",
                     values = c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  scale_color_manual( name = "Scenario",
                      values = c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))

# without SDI based mortality, DI-dependent mortality increases unchecked?
png(height = 3.5, width = 12, units = "in", res = 300, "outputs/Dead_Carbon_density_regional_total_parse_periodic.png")
parse.deadCarbon.totals
dev.off()

parse.dead.Carbon.flux.totals <- ggplot()+#geom_ribbon(data = AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, ymin = low.dead, ymax = up.dead, fill = parse), alpha = 0.5)+
  geom_line(data = AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, y = mNPP.dead, color = parse))+
  
  facet_wrap(~rcp, ncol = 4)+theme_bw(base_size = 14) + ylab( "Carbon Density Flux \n (Tg C/ha)")+theme(panel.grid = element_blank())+
  #scale_fill_manual( name = "Scenario",
   
  #                  values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI"="#7570b3"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI"="#7570b3", "no climate change, DDonly" = "black"))





# separate out by high and low SDI
SDI.plt.unscaled <- SDI.mat.PLT.subp %>% group_by(PLT_CN) %>% summarise(SDI = sum(`2001`, na.rm=TRUE))
SDI.plt.unscaled$SDI.bin <- ifelse(SDI.plt.unscaled$SDI >= 203, "> 203", 
                                   ifelse(SDI.plt.unscaled$SDI < 203 & SDI.plt.unscaled$SDI >= 133, "133 - 203",
                                          ifelse(SDI.plt.unscaled$SDI < 203 & SDI.plt.unscaled$SDI >= 85, "85 - 133", "<85")))
colnames(parse.all.mort)[1] <- "PLT_CN"
SDI.plt.unscaled$PLT_CN <- as.character(SDI.plt.unscaled$PLT_CN)


# rename the AGB.parse.totals and filter
parse.all.mort$parse.new <- ifelse(parse.all.mort$parse %in% c("no SDI") & parse.all.mort$mort.scheme %in% "DIonly", "no SDI growth & mortality", 
                                     ifelse(parse.all.mort$parse %in% c("no SDI") & parse.all.mort$mort.scheme %in% "DIDD", "no SDI growth", parse.all.mort$parse))

parse.all.mort$parse.type <- ifelse(parse.all.mort$mort.scheme %in% "DIonly" & parse.all.mort$parse.new %in% "no SDI growth & mortality", "in", 
                                      ifelse(parse.all.mort$mort.scheme %in% "DIDD", "in", "out"))


new.parse.all.mort <- parse.all.mort %>% filter(parse.type %in% "in")  


AGB.parse.all.mort.SDI <- left_join(new.parse.all.mort, SDI.plt.unscaled)

# plot the totals across space:
plot.by.plot.totals  <- AGB.parse.all.mort.SDI %>% 
  group_by(PLT_CN, mort.scheme, parse.new, rcp, year, SDI.bin, SDI) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){(x*0.5)/1000000})) %>% 
  mutate(AGB.median = sum(mAGB.stemwood:mAGB.foliage, na.rm = TRUE)) 

plot.by.plot.parse <- plot.by.plot.totals %>% ungroup() %>% 
  group_by(PLT_CN, LAT, LON, parse.new, rcp, year) %>%
  spread(parse.new, AGB.median) %>% 
  mutate(climatechangediff = full - `no climate change`, 
         #tmaxdiff = full - `no tmax`, 
         SDIdiff = full - `no SDI growth`, 
         SDImortdiff = full - `no SDI growth & mortality`, 
         climatechangediff.pct = ((full - `no climate change`)/full)*100, 
         #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
         SDIdiff.pct = ((full - `no SDI growth`)/full)*100, 
         SDImortdiff.pct = ((full - `no SDI growth & mortality`)/full)*100)



# plot up the median contributions for set time points across the map
# merge with lat and lon values:
plot.by.plot.totals$LAT <- PLOT[match(plot.by.plot.totals$PLT_CN, PLOT$CN),]$LAT
plot.by.plot.totals$LON <- PLOT[match(plot.by.plot.totals$PLT_CN, PLOT$CN),]$LON


ggplot(data = plot.by.plot.totals %>% filter(year %in% c(2050)), aes(x = LON, y = LAT, color = mAGB))+
  geom_point()+
  facet_grid(cols = vars(rcp), rows = vars(parse.new)) 

ggplot(data = plot.by.plot.totals %>% filter(year %in% c(2050)), aes(x = SDI, y = mAGB))+
  geom_point()+
  facet_grid(cols = vars(rcp), rows = vars(parse.new)) 


# We want biomass differences by plots for total AGB:
plot.by.plot.parse.median <- AGB.parse.all.mort.SDI %>% 
  group_by(PLT_CN, mort.scheme, parse.new, rcp, year, SDI.bin, SDI) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){(x*0.5)/1000000})) %>% 
  mutate(AGB.median = sum(mAGB.stemwood:mAGB.foliage, na.rm = TRUE))%>% 
  select(rcp, PLT_CN, mort.scheme, year, parse.new, SDI.bin, SDI, AGB.median) %>% ungroup()%>%
  group_by(PLT_CN, rcp, year, parse.new, SDI, SDI.bin) %>%
  spread(parse.new, AGB.median) %>% 
  mutate(climatechangediff = full - `no climate change`, 
         #tmaxdiff = full - `no tmax`, 
         SDIdiff = full - `no SDI growth`, 
         SDImortdiff = full - `no SDI growth & mortality`, 
         climatechangediff.pct = ((full - `no climate change`)/full)*100, 
         #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
         SDIdiff.pct = ((full - `no SDI growth`)/full)*100, 
         SDImortdiff.pct = ((full - `no SDI growth & mortality`)/full)*100)


ggplot(data = plot.by.plot.parse.median, aes(x = year, y = climatechangediff, group = PLT_CN))+geom_point()+
  facet_wrap(~rcp) 

# plot up the median contributions against SDI for set time points
ggplot(data = plot.by.plot.parse.median %>% filter(year %in% c(2025, 2050, 2075, 2099)), aes(x = SDI, y = climatechangediff, color = year))+
  geom_point()+
  facet_wrap(~rcp) 

ggplot(data = plot.by.plot.parse.median %>% filter(year %in% c(2025, 2050, 2075, 2099)), aes(x = SDI, y = `no SDI growth & mortality`, color = year))+
  geom_point()+
  facet_wrap(~rcp) 

ggplot(data = plot.by.plot.parse.median %>% filter(year %in% c(2025, 2050, 2075, 2099)), aes(x = SDI, y = `no SDI growth`, color = year))+
  geom_point()+
  facet_grid(cols = vars(rcp), rows = vars(year)) 

# plot up the median contributions for set time points across the map
# merge with lat and lon values:
plot.by.plot.parse.median$LAT <- PLOT[match(plot.by.plot.parse.median$PLT_CN, PLOT$CN),]$LAT
plot.by.plot.parse.median$LON <- PLOT[match(plot.by.plot.parse.median$PLT_CN, PLOT$CN),]$LON

ggplot(data = plot.by.plot.parse.median %>% filter(year %in% c(2025, 2050, 2075, 2099)), aes(x = LON, y = LAT, color = `no SDI growth`))+
  geom_point()+
  facet_grid(cols = vars(rcp), rows = vars(year)) 



#get component plto not working anymore because of UQ() and sym() language...
get_component_plot_sdi_diffs <- function(component, AGB.parse.all.mort.SDI){
  
  parse.difference.df  <- AGB.parse.all.mort.SDI %>% group_by(PLT_CN, mort.scheme, parse.new, rcp, year, SDI,SDI.bin) %>% 
    summarise(across(c(mAGB:low.foliage), function(x){(x*0.5)/1000000})) %>% 
    ungroup() %>% # sum across all the PLT_CNs
    select(rcp, PLT_CN, mort.scheme, year, parse.new, SDI.bin, SDI, !!rlang::sym(component)) %>% 
    group_by(PLT_CN, rcp, mort.scheme, year, parse.new, SDI, SDI.bin) %>%
    spread(parse.new, !!rlang::sym(component)) %>% 
    
    mutate(climatechangediff = full - `no climate change`, 
           #tmaxdiff = full - `no tmax`, 
           SDIdiff = full - `no SDI`, 
           climatechangediff.pct = ((full - `no climate change`)/full)*100, 
           #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
           SDIdiff.pct = ((full - `no SDI`)/full)*100) %>%
    ungroup() %>% select(rcp, mort.scheme, year, SDI.bin, SDI, climatechangediff, SDIdiff) %>% 
    group_by(rcp, mort.scheme, year, SDI.bin) %>%
    summarise(across(c(climatechangediff:SDIdiff), sum)) 
  parse.difference.df 
  
}

# for the total C
upA.parse.plot <- get_component_plot_sdi_diffs(component = "upA", AGB.parse.all.mort.SDI)
lowA.parse.plot <- get_component_plot_sdi_diffs("lowA", AGB.parse.all.mort.SDI)

upA.parse.m <- reshape2::melt(upA.parse.plot, id.vars = c("rcp", "mort.scheme", "year", "SDI.bin"))
lowA.parse.m <- reshape2::melt(lowA.parse.plot, id.vars = c("rcp", "mort.scheme", "year","SDI.bin"))
colnames(upA.parse.m)[6] <- "upA"
colnames(lowA.parse.m)[6] <- "lowA"

CI.A.parse <- left_join(upA.parse.m, lowA.parse.m)
parse.differences <- CI.A.parse %>% filter(variable %in% c("climatechangediff", "SDIdiff"))

parse.differences$year <- as.numeric(parse.differences$year)

parse.differences

# ggplot()+geom_line(data = parse.differences , aes(x = year, y = lowA,  color = variable))+facet_wrap(~rcp)
# ggplot()+geom_line(data = parse.differences , aes(x = year, y = upA,  color = variable))+facet_wrap(~rcp)

# why wont this plot?
ggplot()+geom_ribbon(data = parse.differences , aes(x = year, ymin = lowA, ymax = upA, fill = variable))+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))





#---------------------------- merge with SDI ----------------------------------
SDI.plt.unscaled <- SDI.mat.PLT.subp %>% group_by(PLT_CN) %>% summarise(SDI = sum(`2001`, na.rm=TRUE))
SDI.plt.unscaled$SDI.bin <- ifelse(SDI.plt.unscaled$SDI >= 203, "> 203", 
                                   ifelse(SDI.plt.unscaled$SDI < 203 & SDI.plt.unscaled$SDI >= 133, "133 - 203",
                                          ifelse(SDI.plt.unscaled$SDI < 203 & SDI.plt.unscaled$SDI >= 85, "85 - 133", "<85")))
colnames(AGB.parse.dCC)[1] <- "PLT_CN"
SDI.plt.unscaled$PLT_CN <- as.character(SDI.plt.unscaled$PLT_CN)
AGB.parse.dCC.SDI <- left_join(AGB.parse.dCC, SDI.plt.unscaled)

AGB.parse.dCC.summary.SDI  <- AGB.parse.dCC.SDI %>% ungroup() %>% group_by(rcp, mort.scheme, SDI.bin, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            SDIdiff.lo = quantile(SDIdiff, 0.025),
            SDIdiff.hi = quantile(SDIdiff, 0.975),
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.lo = quantile(climatechangediff, 0.025),
            climatechangediff.hi = quantile(climatechangediff, 0.975),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.lo = quantile(tmaxdiff, 0.025),
            # tmaxdiff.hi = quantile(tmaxdiff, 0.975),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE))


AGB.parse.dCC.summary.SDI$SDI.bin <- factor(AGB.parse.dCC.summary.SDI$SDI.bin, levels = c("<85", "85 - 133", "133 - 203", "> 203"))

ggplot(data = AGB.parse.dCC.summary.SDI, aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.dCC.summary.SDI, aes(x = year, ymin = SDIdiff.median - SDIdiff.sd, ymax = SDIdiff.median + SDIdiff.sd,  fill = mort.scheme))+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))

png(height = 6, width = 7, units = "in", res = 100, "outputs/SDI.parse.AGB.FIAperiodic.doubleCC.bySDI.png")
ggplot(data = na.omit(AGB.parse.dCC.summary.SDI), aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.dCC.summary.SDI, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank())
dev.off()

png(height = 6, width = 7, units = "in", res = 100, "outputs/climate_changes.parse.AGB.FIAperiodic.doubleCC.bySDI.png")
ggplot(data = AGB.parse.dCC.summary.SDI, aes(x = year, y = climatechangediff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.dCC.summary.SDI, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank())
dev.off()

# png(height = 6, width = 7, units = "in", res = 100, "outputs/tmax.parse.AGB.FIAperiodic.doubleCC.bySDI.png")
# ggplot(data = AGB.parse.dCC.summary.SDI, aes(x = year, y = tmaxdiff.median))+geom_line()+
#   geom_ribbon(data = AGB.parse.dCC.summary.SDI, aes(x = year, ymin = tmaxdiff.lo, ymax = tmaxdiff.hi,  fill = mort.scheme),alpha = 0.5)+
#   facet_grid(rows = vars(mort.scheme), cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank())
# dev.off()

# summaries for the single CC runs
AGB.parse <- parse.all.mort %>% select(plot,rcp, mort.scheme, year, parse, mAGB) %>% group_by(plot, mort.scheme, year, parse) %>%
  spread(parse, mAGB) %>% mutate(climatechangediff = full - `no climate change`, 
                                 #tmaxdiff = full - `no tmax`, 
                                 SDIdiff = full - `no SDI`, 
                                 climatechangediff.pct = ((full - `no climate change`)/full)*100, 
                                 #tmaxdiff.pct = ((full - `no tmax`)/full)*100, 
                                 SDIdiff.pct = ((full - `no SDI`)/full)*100)

AGB.parse.summary <- AGB.parse %>% ungroup() %>% group_by(rcp,mort.scheme, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            #tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            #tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            #tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            #tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE),)



ggplot(data = AGB.parse.summary, aes(x = year, y = climatechangediff.median, color = mort.scheme))+geom_line()+
  geom_ribbon(data = AGB.parse.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_wrap(~rcp)


# ggplot(data = AGB.parse.summary, aes(x = year, y = tmaxdiff.median, color = mort.scheme))+geom_line()+
#   geom_ribbon(data = AGB.parse.summary, aes(x = year, ymin = tmaxdiff.median - tmaxdiff.sd, ymax = tmaxdiff.median + tmaxdiff.sd,  fill = mort.scheme))+
#   facet_wrap(~mort.scheme)

ggplot(data = AGB.parse.summary, aes(x = year, y = SDIdiff.median, color = mort.scheme))+geom_line()+
  geom_ribbon(data = AGB.parse.summary, aes(x = year, ymin = SDIdiff.median - SDIdiff.sd, ymax = SDIdiff.median + SDIdiff.sd,  fill = mort.scheme))+
  facet_wrap(~rcp)


#---------------------------- merge with SDI ----------------------------------
SDI.plt.unscaled <- SDI.mat.PLT.subp %>% group_by(PLT_CN) %>% summarise(SDI = sum(`2001`, na.rm=TRUE))
SDI.plt.unscaled$SDI.bin <- ifelse(SDI.plt.unscaled$SDI >= 400, "> 400", 
                                   ifelse(SDI.plt.unscaled$SDI < 400 & SDI.plt.unscaled$SDI >= 250, "250 - 400",
                                          ifelse(SDI.plt.unscaled$SDI < 250 & SDI.plt.unscaled$SDI >= 150, "150 - 250", ">150")))
colnames(AGB.parse)[1] <- "PLT_CN"
SDI.plt.unscaled$PLT_CN <- as.character(SDI.plt.unscaled$PLT_CN)
AGB.parse.SDI <- left_join(AGB.parse, SDI.plt.unscaled)

AGB.parse.summary.SDI  <- AGB.parse.SDI %>% ungroup() %>% group_by(rcp,mort.scheme, SDI.bin, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            SDIdiff.lo = quantile(SDIdiff, 0.025),
            SDIdiff.hi = quantile(SDIdiff, 0.975),
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.lo = quantile(climatechangediff, 0.025),
            climatechangediff.hi = quantile(climatechangediff, 0.975),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.lo = quantile(tmaxdiff, 0.025),
            # tmaxdiff.hi = quantile(tmaxdiff, 0.975),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE))
AGB.parse.summary.SDI$SDI.bin <- factor(AGB.parse.summary.SDI$SDI.bin, levels = c(">150", "150 - 250", "250 - 400", "> 400"))

ggplot(data = AGB.parse.summary.SDI, aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.SDI, aes(x = year, ymin = SDIdiff.median - SDIdiff.sd, ymax = SDIdiff.median + SDIdiff.sd,  fill = mort.scheme))+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))


png(height = 6, width = 7, units = "in", res = 100, "outputs/SDI.parse.AGB.FIAperiodic.singleCC.bySDI.png")
ggplot(data = AGB.parse.summary.SDI, aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.SDI, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))+ theme_bw()
dev.off()

png(height = 6, width = 7, units = "in", res = 100, "outputs/no_climate_change.parse.AGB.FIAperiodic.singleCC.bySDI.png")
ggplot(data = AGB.parse.summary.SDI, aes(x = year, y = climatechangediff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.SDI, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows = vars(rcp), cols = vars(SDI.bin))+ theme_bw()
dev.off()



SDI.parse <- ggplot(data = AGB.parse.summary.SDI, aes(x = year, y = SDIdiff.median))+geom_line()+
                    geom_ribbon(data = AGB.parse.summary.SDI, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
                    facet_grid(rows =vars(rcp), cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
                    ylab("Difference between no SDI \n and full projections")


CC.parse <- ggplot(data = AGB.parse.summary.SDI, aes(x = year, y = climatechangediff.median))+geom_line()+
                    geom_ribbon(data = AGB.parse.summary.SDI, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
                    facet_grid(rows =vars(rcp),cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
                    ylab("Difference between no climate \n change and full projections")

png(height = 10, width = 8, units = "in", res = 200, "outputs/no_cc_no_SDI_absolute_difference_plots.png")
cowplot::plot_grid(SDI.parse, CC.parse, align = "hv", ncol = 1)
dev.off()

#-----------------------------------------------------------
# next plot out by MAP and MAT
#-----------------------------------------------------------
cov.data.unique <- unique(cov.data.regional %>% select(PLT_CN, STATECD, COUNTYCD, PLOT, SUBP, SICOND, STDAGE, MAT, MAP, PLOTSTATE))
cov.data.unique$MAP.bin <- ifelse(cov.data.unique$MAP >= 0.659801, "> 0.659801", 
                                   ifelse(cov.data.unique$MAP < 0.659801 & cov.data.unique$MAP >= -0.225419, "0.659801 - -0.225419",
                                          ifelse(cov.data.unique$MAP < -0.225419 & cov.data.unique$MAP >= -0.722995, "-0.225419 - -0.722995", "<-0.722995")))

cov.data.unique$MAT.bin <- ifelse(cov.data.unique$MAT >= 0.569027, "> 0.569027", 
                                  ifelse(cov.data.unique$MAT < 0.569027 & cov.data.unique$MAT >= -0.006959, "0.569027 - -0.006959",
                                         ifelse(cov.data.unique$MAT < -0.006959 & cov.data.unique$MAT >= -0.579155, "-0.006959 - -0.579155", "<-0.579155")))

cov.data.unique$PLT_CN <- as.character(cov.data.unique$PLT_CN)
AGB.parse.dCC.MAP.MAT <- left_join(AGB.parse.dCC.SDI, cov.data.unique)

# get the summary for MAP:
AGB.parse.summary.MAP  <- AGB.parse.dCC.MAP.MAT %>% ungroup() %>% group_by(rcp, mort.scheme, MAP.bin, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            SDIdiff.lo = quantile(SDIdiff, 0.025),
            SDIdiff.hi = quantile(SDIdiff, 0.975),
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.lo = quantile(climatechangediff, 0.025),
            climatechangediff.hi = quantile(climatechangediff, 0.975),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.lo = quantile(tmaxdiff, 0.025),
            # tmaxdiff.hi = quantile(tmaxdiff, 0.975),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE))
AGB.parse.summary.MAP$MAP.bin <- factor(AGB.parse.summary.MAP$MAP.bin, levels = c("<-0.722995", "-0.225419 - -0.722995", "0.659801 - -0.225419", "> 0.659801"))


ggplot(data = AGB.parse.summary.MAP, aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.MAP, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp), cols = vars(MAP.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no SDI \n and full projections")


ggplot(data = AGB.parse.summary.MAP, aes(x = year, y = climatechangediff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.MAP, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp), cols = vars(MAP.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no climate \n change and full projections")


# get the summary for MAT:
AGB.parse.summary.MAT  <- AGB.parse.dCC.MAP.MAT %>% ungroup() %>% group_by(rcp, mort.scheme, MAT.bin, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            SDIdiff.lo = quantile(SDIdiff, 0.025),
            SDIdiff.hi = quantile(SDIdiff, 0.975),
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.lo = quantile(climatechangediff, 0.025),
            climatechangediff.hi = quantile(climatechangediff, 0.975),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.lo = quantile(tmaxdiff, 0.025),
            # tmaxdiff.hi = quantile(tmaxdiff, 0.975),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE))
AGB.parse.summary.MAT$MAT.bin <- factor(AGB.parse.summary.MAT$MAT.bin, levels = c("<-0.579155", "-0.006959 - -0.579155", "0.569027 - -0.006959", "> 0.569027"))

ggplot(data = AGB.parse.summary.MAT, aes(x = year, y = SDIdiff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.MAT, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp), cols = vars(MAT.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no SDI \n and full projections")


ggplot(data = AGB.parse.summary.MAT, aes(x = year, y = climatechangediff.median))+geom_line()+
  geom_ribbon(data = AGB.parse.summary.MAT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp), cols = vars(MAT.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no climate \n change and full projections")




#---------------------------------------------------------------------------
# Make some individual difference plots
#---------------------------------------------------------------------------

# get the summary for MAT:
AGB.parse.summary.PLT  <- AGB.parse.dCC.MAP.MAT %>% ungroup() %>% group_by(rcp, mort.scheme, PLT_CN,SDI.bin, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            SDIdiff.median = median(SDIdiff, na.rm =TRUE),
            SDIdiff.sd = sd(SDIdiff, na.rm =TRUE),
            SDIdiff.lo = quantile(SDIdiff, 0.025),
            SDIdiff.hi = quantile(SDIdiff, 0.975),
            climatechangediff.pct.median = median(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.pct.sd = sd(climatechangediff.pct, na.rm =TRUE),
            climatechangediff.lo = quantile(climatechangediff, 0.025),
            climatechangediff.hi = quantile(climatechangediff, 0.975),
            # tmaxdiff.pct.median = median(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.pct.sd = sd(tmaxdiff.pct, na.rm =TRUE),
            # tmaxdiff.lo = quantile(tmaxdiff, 0.025),
            # tmaxdiff.hi = quantile(tmaxdiff, 0.975),
            SDIdiff.pct.median = median(SDIdiff.pct, na.rm =TRUE),
            SDIdiff.pct.sd = sd(SDIdiff.pct, na.rm =TRUE))


ggplot(data = AGB.parse.summary.PLT, aes(x = year, y = SDIdiff.median, group = PLT_CN))+geom_line()+
 # geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = SDIdiff.lo, ymax = SDIdiff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp), cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no SDI \n and full projections")


ggplot(data = AGB.parse.summary.PLT, aes(x = year, y = climatechangediff.median, group = PLT_CN))+geom_line()+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows =vars(rcp),cols = vars(SDI.bin))+theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no climate \n change and full projections")


ggplot(data = AGB.parse.summary.PLT, aes(x = year, y = climatechangediff.median, group = PLT_CN))+geom_line(color = "darkgrey")+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("Difference between no climate \n change and full projections")+facet_wrap(~rcp)


#----------------------------------------------------------------------------
# look at a variety of covariates: sdi, site index, stdage, elevation, lat/lon
#----------------------------------------------------------------------------
cov.AGB.parse.PLT <- left_join(AGB.parse.summary.PLT, cov.data.unique)
cov.AGB.parse.PLT <- left_join(PLOT, cov.AGB.parse.PLT )

cov.AGB.parse.PLT
# plot up trajectories by STDAGE
ggplot(data = cov.AGB.parse.PLT %>% filter(!is.na(rcp)), aes(x = year, y = climatechangediff.median, group = PLT_CN, color = STDAGE))+geom_line()+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  facet_grid(rows=vars(rcp),cols = vars(STATECD))+theme_bw()+theme(panel.grid = element_blank())+
  ylab("Difference between no climate \n change and full projections")+ scale_color_viridis(option = "D")



ggplot(data = cov.AGB.parse.PLT %>% filter(!is.na(rcp)), aes(x = year, y = climatechangediff.median, group = PLT_CN, color = STDAGE))+geom_line()+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  theme_bw()+theme(panel.grid = element_blank())+facet_grid(rows=vars(rcp))+
  ylab("Difference between no climate \n change and full projections")+ scale_color_viridis(option = "D")



# by ELEV, 

ggplot(data = cov.AGB.parse.PLT %>% filter(!is.na(rcp)), aes(x = year, y = climatechangediff.median, group = PLT_CN, color = ELEV))+geom_line()+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  theme_bw()+theme(panel.grid = element_blank())+facet_grid(cols=vars(rcp))+
  ylab("Difference between no climate \n change and full projections")+ scale_color_viridis(option = "D")


# by state

# by MAP & MAT


# plot up all trajectories:

ggplot(data = parse.all.mort,  aes(x = year, y = mAGB, group = plot))+geom_line()+
  #geom_ribbon(data = AGB.parse.summary.PLT, aes(x = year, ymin = climatechangediff.lo, ymax = climatechangediff.hi,  fill = mort.scheme),alpha = 0.5)+
  theme_bw()+theme(panel.grid = element_blank(), legend.position = "none")+
  ylab("full projections by plot")+facet_grid(cols= vars(parse), rows = vars(rcp))


# get whether biomass is increasing, decreasing, or staying the same over time. 

biomass.trajectory.means <- parse.all.mort %>% select(plot, parse,rcp, mAGB, year)%>% group_by(plot,rcp, parse) %>% filter(year %in% c(2002, 2098))%>%
  spread(year, value = mAGB) %>% mutate(difference.century = `2098`-`2002`)
# biomass decreasing over time on the whole
sum(biomass.trajectory.means$difference.century)
hist(biomass.trajectory.means$difference.century)

# join up with plot tables to map out:
colnames(biomass.trajectory.means)[1] <- "CN" 
PLOT$CN <- as.character(PLOT$CN)

biomass.plot.trajectory <- left_join(biomass.trajectory.means, PLOT)

ggplot(biomass.plot.trajectory, aes(x = LON, y = LAT, color = difference.century))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(rcp),cols = vars(parse))+  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0)
  
ggplot(biomass.plot.trajectory, aes(x = ELEV, y = difference.century))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed", color = "red")+
  facet_grid(rows = vars(rcp),cols = vars(parse))#+  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0)

ggplot(biomass.plot.trajectory, aes(x = LON, y = difference.century))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed", color = "red")+
  facet_grid(rows = vars(rcp),cols = vars(parse))#+  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0)

ggplot(biomass.plot.trajectory, aes(x = LAT, y = difference.century))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed", color = "red")+
  facet_grid(rows = vars(rcp),cols = vars(parse))#+  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0)

COND$CN <- as.character(COND$PLT_CN)
biomass.plot.trajectory.cond <- left_join(biomass.plot.trajectory, COND)
ggplot(biomass.plot.trajectory.cond, aes(x = SICOND, y = difference.century))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed", color = "red")+
  facet_grid(rows = vars(rcp),cols = vars(parse))#+  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0)


#----------------------------------------------------------------------------
# MAP out in space
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
# Plots for % differences between parses
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
# Plots for differences and % differences in dead mAGB vs reduced mAGB due to reduced growth
#----------------------------------------------------------------------------

