library(viridis)
#Parse apart the SDI and climate change effects sensitivity effects on AGB
parse_biomass_ests <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, rcp, cc.scenario = "doubleCC" ){
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
    # mort.scheme <- "DIDD"
    
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
mort.scheme = "DIDD"
SDI.ratio.DD = 0.8
cc.scenario = "singleCC"

unique(plots) %in% 2584218010690
DIDD.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp26", cc.scenario = "singleCC" )})
DIDD.rcp85.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp85", cc.scenario = "singleCC" )})
DIDD.rcp45.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp45", cc.scenario = "singleCC" )})
DIDD.rcp60.parse.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.8, rcp = "rcp60", cc.scenario = "singleCC" )})

#DDonly.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests(plot = x, mort.scheme = "DDonly",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})
#DIonly.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIonly",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})
#nomort.parse.list <- lapply(unique(plots)[1:43],FUN = function(x){parse_biomass_ests(plot = x, mort.scheme = "nomort",  SDI.ratio.DD = 0.7, cc.scenario = "singleCC" )})


DIDD.parse.df <- do.call(rbind, DIDD.parse.list)
DIDD.rcp45.parse.df <- do.call(rbind, DIDD.rcp45.parse.list)
DIDD.rcp60.parse.df <- do.call(rbind, DIDD.rcp60.parse.list)
DIDD.rcp85.parse.df <- do.call(rbind, DIDD.rcp85.parse.list)

parse.all.mort <- rbind(DIDD.parse.df, DIDD.rcp45.parse.df, DIDD.rcp60.parse.df,DIDD.rcp85.parse.df)


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
  facet_wrap(~rcp)

# ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = tmaxdiff.median, color = mort.scheme))+geom_line()+
#   geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = tmaxdiff.median - tmaxdiff.sd, ymax = tmaxdiff.median + tmaxdiff.sd,  fill = mort.scheme))+
#   facet_wrap(~mort.scheme)
# 
# ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = SDIdiff.median, color = mort.scheme))+geom_line()+
#   geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = SDIdiff.median - SDIdiff.sd, ymax = SDIdiff.median + SDIdiff.sd,  fill = mort.scheme))+
#   facet_wrap(~mort.scheme)

#------------------------get differences for the CI-----------------------------------
get_component_diffs <- function(component, parse.all.mort){
  
  parse.difference.df  <- parse.all.mort %>% group_by(mort.scheme, parse, rcp, year) %>% 
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

# for the total AGB
get_component_diffs("upA", parse.all.mort)
get_component_diffs("lowA", parse.all.mort)

# for NPP
get_component_diffs("up", parse.all.mort)
get_component_diffs("low", parse.all.mort)





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

