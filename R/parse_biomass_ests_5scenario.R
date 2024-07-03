library(viridis)
library(here)
library(tidyverse)

data <- readRDS("data/regional_pipo_jags_formatted_data.RDS")
cov.data.regional <- data$cov.data.regional
plots <- unique(cov.data.regional$PLT_CN)
plot <- unique(plots)[2]
mort.scheme = "DIDD"
SDI.ratio.DD = 0.6
cc.scenario = "singleCC"
db <- readRDS("data/InWeUS_FIAdb.rds")
TREE <- db$TREE
PLOT <- db$PLOT %>% rename(`PLT_CN` = "CN")
rm(db)
#Parse apart the SDI and climate change effects sensitivity effects on AGB
parse_biomass_ests <- function(plot, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, rcp, cc.scenario = "doubleCC", scale.mort.prob = 1 ){
  
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  rm(total.plot.obs.all)
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  
  if(nrow(oldTREE) <=1){
   cat("less than 2 trees on the first plot")
  }else{
    if(!file.exists(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))){
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
    
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    
        }else{
      
          load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
       
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

#######################################################################
# Density Dependent ramping scenario  
    cat("reading in DD.ramp results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".DD.ramp.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".DD.ramp.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
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
    
    total.plot.DD.ramp <- data.frame(plot = plot, 
                                   mort.scheme = mort.scheme, 
                                   rcp = rcp,
                                   cc.scenario = cc.scenario,
                                   parse = "DD.ramp",
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
#######################################################################
# Growth Dependent mortality scaled by 10
 
    cat("reading in GD by 10 results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".GT.10.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".GT.10.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
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
    
    total.plot.GD.10 <- data.frame(plot = plot, 
                                   mort.scheme = mort.scheme, 
                                   rcp = rcp,
                                   cc.scenario = cc.scenario,
                                   parse = "GD.10",
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
    
#######################################################################
# Growth Dependent mortality scaled by 20
    cat("reading in GD by 20 results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".GT.20.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".GT.20.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
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
    
    total.plot.GD.20 <- data.frame(plot = plot, 
                                   mort.scheme = mort.scheme, 
                                   rcp = rcp,
                                   cc.scenario = cc.scenario,
                                   parse = "GD.20",
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
    

#######################################################################
# No climate Change scenario
    cat("reading in no CC results")
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".noCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".noCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
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
    
    total.plot.notemperature <- data.frame(plot = plot, 
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
    
    

    
   
   
##########################################################################
# combine all together
    
    total.plot.obs.all <- rbind(total.plot, 
                                total.plot.DD.ramp,
                                total.plot.GD.10, 
                                total.plot.GD.20,
                                total.plot.notemperature )
    total.plot.obs.all
    
    
  }
  }
 
}


unique(plots) %in% 2469188010690

plt.parse.18 <- parse_biomass_ests (plot = unique(plots)[1], mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", scale.mort.prob = 1 )
plot <- "2584218010690"
# read in forecasts with the 60% threshold for SDI based mortality
DIDD.parse.list.60 <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", scale.mort.prob = 1 )})
DIDD.parse.df.60 <- do.call(rbind, DIDD.parse.list.60)

saveRDS(DIDD.parse.df.60, "outputs/temporary.DIDD.parse.df.60.rds")
DIDD.parse.df.60 <- readRDS( "outputs/temporary.DIDD.parse.df.60.rds")

# DIDD.rcp85.parse.list.60 <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC" , scale.mort.prob = 1)})
# DIDD.rcp45.parse.list.60 <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC" , scale.mort.prob = 1)})
# DIDD.rcp60.parse.list.60 <- lapply(unique(plots)[1:675],FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC" , scale.mort.prob = 1)})
#DIDD.parse.df.60.ncol <- do.call(ncol, DIDD.parse.list.60)



# DIDD.rcp45.parse.df.60 <- do.call(rbind, DIDD.rcp45.parse.list.60)
# DIDD.rcp60.parse.df.60 <- do.call(rbind, DIDD.rcp60.parse.list.60)
# DIDD.rcp85.parse.df.60 <- do.call(rbind, DIDD.rcp85.parse.list.60)
unique(DIDD.parse.df.60$plot)

rm(future.clim.subset.26, future.clim.subset.45, future.clim.subset.60, future.clim.subset.85)
# check forecasts vs. tp.ratio
db <- readRDS("data/InWeUS_FIAdb.rds")
db$TREE %>% filter(PLT_CN %in% unique(plots))%>% distinct(PLT_CN)
tp.ratio <- db$TREE %>% filter(PLT_CN %in% unique(plots)) %>% 
  group_by(PLT_CN, SPCD == 122) %>% summarise(`n()` = sum(TPA_UNADJ, na.rm =TRUE)) %>% 
  spread(`SPCD == 122`, `n()`) %>%
  mutate(PIPO = ifelse(is.na(`TRUE`), 0, `TRUE`), 
         NonPIPO = ifelse(is.na(`FALSE`), 0, `FALSE`))%>% 
  mutate(PIPO.ratio = PIPO/(PIPO +NonPIPO)) %>% 
  mutate(stand.cat = ifelse(PIPO.ratio < 0.5, "less than 50% PIPO", 
                            ifelse(PIPO.ratio >= 0.5 & PIPO.ratio < 0.75, "50-75% PIPO",  ">75% PIPO")))
hist(tp.ratio$PIPO.ratio)

AGB.plt <- db$TREE %>% filter(PLT_CN %in% unique(plots)) %>%group_by(PLT_CN) %>%  summarise(
            CARBON_AG_MG = sum(CARBON_AG*TPA_UNADJ/2205), 
            BIOMASS_AG_MG =  sum((CARBON_AG*TPA_UNADJ/2205)*2))


hist(AGB.plt$CARBON_AG_MG)
hist(AGB.plt$BIOMASS_AG_MG)

tp.ratio$plot <- as.character(tp.ratio$PLT_CN)
AGB.plt$plot <- as.character(tp.ratio$PLT_CN)
as.character(unique(tp.ratio$PLT_CN)) %in% as.character(DIDD.parse.df.60$plot)
unique(as.character(DIDD.parse.df.60$plot)) %in% as.character(unique(tp.ratio$PLT_CN))
tp.ratio %>% filter(PLT_CN %in% "2560687010690") 

DIDD.parse.df.60$plot <- as.character(DIDD.parse.df.60$plot)
parse.ratio.agb <- left_join(DIDD.parse.df.60, tp.ratio)
parse.observed.agb <- left_join(DIDD.parse.df.60, AGB.plt)
parse.observed.agb



# do the same plots but color by ratio of PIPO
parse.obs.agb.ration <- left_join(parse.observed.agb, tp.ratio)

ggplot()+geom_point(data = parse.obs.agb.ration %>% filter(year %in% 2098 & parse %in% "full"), aes(x = BIOMASS_AG_MG, y = mAGB/1000, color = stand.cat))+
  geom_errorbar(data = parse.obs.agb.ration %>% filter(year %in% 2098& parse %in% "full"), aes(x = BIOMASS_AG_MG, ymin = lowA/1000, ymax = upA/1000, color = stand.cat))+facet_wrap(~stand.cat, scales = "free_y")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ylab("Total Forecasted plot AGB (MG/acre) in 2098")+xlab("Total AGB in live stem and bark (MG/acre) in FIADB")
ggsave(height = 5, width = 6, units = "in", "outputs/RCP2.6_PLOT_ALL_AGB_Pred_observed_AGB_2098_by_stand_cat_fixed.png")


# there are no more full CC plots that are suspiciously high:
high.prediction.plots <- parse.obs.agb.ration %>% filter(year %in% 2098 & parse %in% "full" & (mAGB/1000) > 250)
#saveRDS(high.prediction.plots, "outputs/suspiciously_high_prediction_plots_post_fix3MSB.rds")

# there are 16 plots where the mean predicticed AGB is greater than 300 Mg/ha
unique(high.prediction.plots$plot)[1]
high.prediction.plots$mAGB/1000

# plot trajectories over time
ggplot(parse.ratio.agb %>% filter(parse == "full"), aes(x = year, y = mAGB/1000, color = stand.cat, group = plot))+geom_line()+facet_wrap(~parse)
ggsave(height = 5, width = 6, units = "in", "outputs/RCP2.6_PLOT_ALL_AGB_Pred_agb_trajectories_stand_cat_fixed.png")

unique(parse.ratio.agb$plot)
# plot ending biomass across space
PLOT.ll <- db$PLOT %>% select(LAT, LON, CN) %>% rename("PLT_CN" = "CN")

parse.ratio.agb.ll <- left_join(parse.ratio.agb , PLOT.ll)


# get the states:
# make a map of all of these:
library(mapdata)
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona", "utah", "new mexico", "colorado","idaho", "wyoming", "montana", "nevada", 
                                            "california", "oregon", "washington", "texas", "kansas", 
                                            "nebraska", "north dakota", "south dakota", "oklahoma") )
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")
parse.ratio.agb.ll.2098 <- parse.ratio.agb.ll %>% filter(year == 2098)
hist(parse.ratio.agb.ll.2098$mAGB)
summary(parse.ratio.agb.ll.2098$mAGB)

parse.ratio.agb.ll.2098 <- parse.ratio.agb.ll.2098 %>% mutate(mAGB.Mg = mAGB/1000)%>% mutate(mAGB.class = 
                                                                ifelse(mAGB.Mg < 50, "<50", 
                                                                        ifelse(mAGB.Mg >=50 & mAGB.Mg < 150, "50-150", 
                                                              ifelse(mAGB.Mg >=150 & mAGB.Mg <=250, "150-250",">240"))))

ggplot()+
  geom_polygon(data = states, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = mexico, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = canada, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white")+
  geom_point(data = parse.ratio.agb.ll.2098, aes(x = LON, y = LAT, color = mAGB.class))+
  facet_wrap(~parse)+theme_bw()+
  coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())
ggsave(height = 15, width = 10, "outputs/AGB_2098_map_by_parse_fixed.png")


ggplot()+
  geom_polygon(data = states, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = mexico, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = canada, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white")+
  geom_point(data = parse.ratio.agb.ll.2098 %>% filter(parse %in% "full"), aes(x = LON, y = LAT, color = stand.cat))+
  facet_wrap(~parse)+theme_bw()+
  coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())
ggsave(height = 15, width = 10, "outputs/stand_category_map_fixed.png")

ggplot(data = parse.ratio.agb.ll.2098 %>% filter(parse %in% "full"), aes(x = stand.cat, y = mAGB.Mg))+geom_violin()

#----------------------------------------------------------------------------------
# get the predicted diameters of all the trees over time
#----------------------------------------------------------------------------------


nrow(PLOT %>% filter(PLT_CN %in% unique(DIDD.parse.df.60$plot)))
nrow(TREE %>% filter(PLT_CN %in% unique(DIDD.parse.df.60$plot)) %>% distinct(PLT_CN))

unique(DIDD.parse.df.60$plot)

#parse.ratio.agb
parse.DIDD.mort.60 <- rbind( DIDD.parse.df.60)#, 
                            # DIDD.rcp45.parse.df.60, 
                             #DIDD.rcp60.parse.df.60, 
                             #DIDD.rcp85.parse.df.60)
#saveRDS( DIDD.parse.df.60, "outputs/parse.DIDD.mort.60SDIthreshold_1.RDS")

saveRDS(parse.DIDD.mort.60, "outputs/parse.DIDD.mort.60SDIthreshold_1.RDS")
parse.DIDD.mort.60 <- readRDS("outputs/parse.DIDD.mort.60SDIthreshold_1.RDS")

parse.all.mort <- DIDD.parse.df.60
parse.all.mort <- parse.DIDD.mort.60  #DIDD.parse.df.60 ##DIDD.parse.df.60  #<- 
parse.all.mort$plot <- as.character(parse.all.mort$plot)

# subtract the scenarios from the full scenario for the mean AGB:
AGB.parse.dCC <- parse.all.mort %>% select(plot, rcp, mort.scheme, year, parse, mAGB) %>% 
  group_by(plot, mort.scheme, year, parse) %>%
  spread(parse, mAGB) %>% mutate(climatechangediff = full - `no climate change`,
                                 cc.GD.10 = GD.10 - `no climate change`, 
                                 cc.GD.20 = GD.20 - `no climate change`, 
                                 cc.DD.ramp = DD.ramp - `no climate change`)

AGB.parse.dCC.summary <- AGB.parse.dCC %>% ungroup() %>% group_by(rcp, mort.scheme, year) %>% 
  summarise(climatechangediff.median = median(climatechangediff, na.rm =TRUE),
            climatechangediff.sd = sd(climatechangediff, na.rm =TRUE),
            
            cc.GD.10diff.median = median(cc.GD.10, na.rm =TRUE),
            cc.GD.10diff.sd = sd(cc.GD.10, na.rm =TRUE),
            # tmaxdiff.median = median(tmaxdiff, na.rm =TRUE),
            # tmaxdiff.sd = sd(tmaxdiff, na.rm =TRUE),
            cc.GD.20diff.median = median(cc.GD.20, na.rm =TRUE),
            cc.GD.20diff.sd = sd(cc.GD.20, na.rm =TRUE),
            
            cc.DD.ramp.diff.median = median(cc.DD.ramp, na.rm =TRUE),
            cc.DD.ramp.diff.sd = sd(cc.DD.ramp, na.rm =TRUE))

ggplot(data = AGB.parse.dCC.summary, aes(x = year, y = climatechangediff.median, color = mort.scheme))+geom_line()+
  geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))


ggplot(data = AGB.parse.dCC, aes(x = year, y = climatechangediff, color = mort.scheme, group = plot))+geom_line()+
  #geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))


ggplot(data = AGB.parse.dCC, aes(x = year, y = cc.DD.ramp, color = mort.scheme, group = plot))+geom_line()+
  #geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))

ggplot(data = AGB.parse.dCC, aes(x = year, y = cc.GD.10, color = mort.scheme, group = plot))+geom_line()+
  #geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))

ggplot(data = AGB.parse.dCC, aes(x = year, y = cc.GD.20, color = mort.scheme, group = plot))+geom_line()+
  #geom_ribbon(data = AGB.parse.dCC.summary, aes(x = year, ymin = climatechangediff.median - climatechangediff.sd, ymax = climatechangediff.median + climatechangediff.sd,  fill = mort.scheme))+
  facet_grid(cols = vars(rcp), rows = vars(mort.scheme))


###########################################################################################################################################
#------------------------- Parse DI and DD mortality contributions for 60% max SDI threshold--------------------------------------
parse_mortality_ests <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.6, rcp, cc.scenario = "doubleCC", parse , scale.mort.prob = 1.1){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    
    if(parse == "full" | parse == "DD.ramp" | parse == "GD.10" | parse == "GD.20"){
      fn <- paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".", parse,".Rdata")
    }else{
      if(parse == "noSDI"){
        fn <- paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
      }else{
        
        fn <-  paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
        
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
      
      tpa.di.m.time <- tpa.di.m %>% dplyr::select(id, TPADI) %>%  spread(key = id, value = TPADI)
      
      
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
      nrep    <- 3
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


# get parsing for full climate change scenario
mort.26.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "full" , scale.mort.prob = 1)})
#for(i in 1:675){
 # mort.26.list[[i]] <- parse_mortality_ests (plot = unique(plots)[i], mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "full" , scale.mort.prob = 1)
#}
mort.26 <- do.call(rbind, mort.26.list)
saveRDS(mort.26, "outputs/parse.mort.26.60.tempfile.full.rds")
rm(mort.26, mort.26.list)

# mort.45.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "full"  , scale.mort.prob = 1)})
# mort.45.test <- do.call(rbind, mort.45.list)
# saveRDS(mort.45.test, "outputs/parse.mort.45.60.tempfile.full.rds")
# rm(mort.45.test, mort.45.list)
# 
# 
# mort.60.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "full", scale.mort.prob = 1)})
# mort.60.test <- do.call(rbind, mort.60.list)
# saveRDS(mort.60.test, "outputs/parse.mort.60.60.tempfile.full.rds")
# rm(mort.60.test, mort.60.list)

# mort.85.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "full" , scale.mort.prob = 1)})
# mort.85.test <- do.call(rbind, mort.85.list)
# saveRDS(mort.85.test, "outputs/parse.mort.85.60.tempfile.full.rds")
# rm(mort.85.list)

mort.26 <- readRDS("outputs/parse.mort.26.60.tempfile.full.rds")
#mort.45.test <- readRDS("outputs/parse.mort.45.60.tempfile.full.rds")
#mort.60.test <- readRDS("outputs/parse.mort.60.60.tempfile.full.rds")
mort.full.parse <- rbind(mort.26)#, 
                         #mort.45.test, 
                         #mort.60.test, 
                        # mort.85.test)
#mort.full.parse <- mort.26
saveRDS(mort.full.parse, "outputs/mort.full.parse.all.rds")
rm(mort.full.parse, mort.26, mort.45.test, mort.60.test, mort.85.test)
unique(plots) %in% 3172745010690
plot = 3172745010690
# get it for noCC

mort.test.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "noCC" , scale.mort.prob = 1)})
mort.test.noCC <- do.call(rbind, mort.test.list.noCC)

# mort.45.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "noCC" , scale.mort.prob = 1)})
# mort.45.test.noCC <- do.call(rbind, mort.45.list.noCC)
# 
# mort.60.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "noCC" , scale.mort.prob = 1)})
# mort.60.test.noCC <- do.call(rbind, mort.60.list.noCC)
# 

# mort.85.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "noCC" , scale.mort.prob = 1)})
# mort.85.test.noCC <- do.call(rbind, mort.85.list.noCC)

#mort.full.parse.noCC <- mort.test.noCC
mort.full.parse.noCC <- rbind(mort.test.noCC)#, 
                              #mort.45.test.noCC, 
                              #mort.60.test.noCC, 
                             # mort.85.test.noCC)
saveRDS(mort.full.parse.noCC, "outputs/mort.full.pars.noCC.60.rds")
rm(mort.full.parse.noCC, 
   mort.26.list.noCC, mort.26.test.noCC, 
   mort.45.list.noCC, mort.45.test.noCC, 
   mort.60.list.noCC, mort.60.test.noCC, 
   mort.85.list.noCC, mort.85.test.noCC)

########
# get it for DDramp:


mort.test.list.DD.ramp <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "DD.ramp" , scale.mort.prob = 1)})
mort.test.DD.ramp <- do.call(rbind, mort.test.list.DD.ramp)

# mort.45.list.DD.ramp <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "DD.ramp" , scale.mort.prob = 1)})
# mort.45.test.DD.ramp <- do.call(rbind, mort.45.list.DD.ramp)
# 
# mort.60.list.DD.ramp <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "DD.ramp" , scale.mort.prob = 1)})
# mort.60.test.DD.ramp <- do.call(rbind, mort.60.list.DD.ramp)

# mort.85.list.DD.ramp <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "DD.ramp" , scale.mort.prob = 1)})
# mort.85.test.DD.ramp <- do.call(rbind, mort.85.list.DD.ramp)

#mort.full.parse.DD.ramp <- mort.test.DD.ramp
mort.full.parse.DD.ramp <- rbind(mort.test.DD.ramp)#, 
                                 #mort.45.test.DD.ramp, 
                                 #mort.60.test.DD.ramp, 
                                 #mort.85.test.DD.ramp)
saveRDS(mort.full.parse.DD.ramp, "outputs/mort.full.parse.DD.ramp.60.rds")
rm( 
   mort.26.list.DD.ramp, mort.26.test.DD.ramp, 
   mort.45.list.DD.ramp, mort.45.test.DD.ramp, 
   mort.60.list.DD.ramp, mort.60.test.DD.ramp, 
   mort.85.list.DD.ramp, mort.85.test.DD.ramp)
#

########
# get it for GD 10 mortality:


mort.test.list.GT.10 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "GD.10" , scale.mort.prob = 1)})
mort.test.GT.10 <- do.call(rbind, mort.test.list.GT.10)

# mort.45.list.GT.10 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "GT.10" , scale.mort.prob = 1)})
# mort.45.test.GT.10 <- do.call(rbind, mort.45.list.GT.10)
# 
# mort.60.list.GT.10 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "GT.10" , scale.mort.prob = 1)})
# mort.60.test.GT.10 <- do.call(rbind, mort.60.list.GT.10)

# mort.85.list.GT.10 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "GT.10" , scale.mort.prob = 1)})
# mort.85.test.GT.10 <- do.call(rbind, mort.85.list.GT.10)

mort.full.parse.GT.10 <- mort.test.GT.10
mort.full.parse.GT.10 <- rbind(mort.test.GT.10)#,
                               #mort.45.test.GT.10, 
                               #mort.60.test.GT.10, 
                               #mort.85.test.GT.10)
saveRDS(mort.full.parse.GT.10, "outputs/mort.full.parse.GT.10.60.rds")
rm( 
  mort.26.list.GT.10, mort.26.test.GT.10, 
  mort.45.list.GT.10, mort.45.test.GT.10, 
  mort.60.list.GT.10, mort.60.test.GT.10, 
  mort.85.list.GT.10, mort.85.test.GT.10)
#

########
# get it for GD 20 mortality:


mort.test.list.GT.20 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "GD.20" , scale.mort.prob = 1)})
mort.test.GT.20 <- do.call(rbind, mort.test.list.GT.20)

# mort.45.list.GT.20 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "GT.20" , scale.mort.prob = 1)})
# mort.45.test.GT.20 <- do.call(rbind, mort.45.list.GT.20)
# 
# mort.60.list.GT.20 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "GT.20" , scale.mort.prob = 1)})
# mort.60.test.GT.20 <- do.call(rbind, mort.60.list.GT.20)

# mort.85.list.GT.20 <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "GT.20" , scale.mort.prob = 1)})
# mort.85.test.GT.20 <- do.call(rbind, mort.85.list.GT.20)

#mort.full.parse.GT.20 <- mort.test.GT.20
mort.full.parse.GT.20 <- rbind(mort.test.GT.20)#, 
                               #mort.45.test.GT.20, 
                               #mort.60.test.GT.20, 
                               #mort.85.test.GT.20)
saveRDS(mort.full.parse.GT.20, "outputs/mort.full.parse.GT.20.60.rds")
rm( 
  mort.26.list.GT.20, mort.26.test.GT.20, 
  mort.45.list.GT.20, mort.45.test.GT.20, 
  mort.60.list.GT.20, mort.60.test.GT.20, 
  mort.85.list.GT.20, mort.85.test.GT.20)
#



mort.full.parse <- readRDS("outputs/mort.full.parse.all.rds")
mort.full.parse.noCC <- readRDS("outputs/mort.full.pars.noCC.60.rds")
mort.full.parse.DD.ramp <- readRDS("outputs/mort.full.parse.DD.ramp.60.rds")
mort.full.parse.GT.10 <- readRDS("outputs/mort.full.parse.GT.10.60.rds")
mort.full.parse.GT.20 <- readRDS("outputs/mort.full.parse.GT.20.60.rds")
mort.all.parse.60 <- rbind(mort.full.parse, 
                           mort.full.parse.DD.ramp, 
                           mort.full.parse.GT.10, 
                           mort.full.parse.GT.20,
                           mort.full.parse.noCC)

# for just rcp 26
#mort.all.parse.60 <- rbind(mort.26, mort.test.noCC, mort.test.DD.ramp)
# save as RDS:
saveRDS(mort.all.parse.60, "outputs/all.plot.mort.C.60SDIthresh_1.RDS")
mort.all.parse.60 <- readRDS( "outputs/all.plot.mort.C.60SDIthresh_1.RDS")

# create function to scale biomass to C and convert to Tg?
# Cfraction
C.convert.deadwood <- function(x, C.frac = 0.4822){(x*C.frac)/1000000}
C.convert.deadwood(mort.all.parse.60$mAGB.dead, C.frac = 0.4822)

# get general summary of the total mortality in terms of C for each mortality type
mort.test <- mort.all.parse.60 %>% group_by(plot, mort.scheme, rcp, year, parse) %>%
  summarise(across(c(mAGB.dead:hiAGB.dead.di), function(x){C.convert.deadwood(x)})) %>% 
  ungroup() %>% # sum across all the PLT_CNs
  group_by(rcp, mort.scheme, year, parse) %>%
  summarise(across(c(mAGB.dead:hiAGB.dead.di), sum))

summary(mort.test$mAGB.dead.di)

#mort.test.m <- reshape2::melt(mort.test, id.vars = c("rcp", "mort.scheme", "year", "parse"))

ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = "Density Independent"))+
  geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = "Density Dependent"))+
  facet_grid(cols = vars(parse), rows = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C)")

ggsave(height = 6, width = 10, units = "in", here("outputs/", "Dead_Carbon_by_DI_DD_total_parse_periodic_60MaxSDIthresh_1_test.png"))

# reorient it so it looks closer to the parse plots

ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.di, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C)")+scale_fill_manual( name = "Scenario",
                                                     values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))



ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.di, ymax = hiAGB.dead.di, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.di, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C) \n Density Independent Mortality")+scale_fill_manual( name = "Scenario",
                                                                                      values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))

ggsave(height = 3, width = 8, units = "in", here("outputs/", "Dead_Carbon_by_DI_total_parse_periodic_60MaxSDIthresh_1.1.png"))


ggplot()+geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  geom_line(data = mort.test, aes(x = year, y = mAGB.dead.dd, color = parse), alpha = 0.7)+
  
  #geom_ribbon(data = mort.test, aes(x = year, ymin = lowAGB.dead.dd, ymax = hiAGB.dead.dd, fill = parse), alpha = 0.7)+
  facet_grid(cols = vars(rcp))+theme_bw()+#theme(panel.grid = element_blank())+
  # scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Dead Wood carbon (Tg C) \n Density Dependent Mortality")+scale_fill_manual( name = "Scenario",
                                                                                    values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","noCC"= "#d95f02", "DD.ramp"="#7570b3", "GD.10" = "grey", "GD.20" = "black"))


ggsave(height = 3, width = 8, units = "in", here("outputs/", "Dead_Carbon_by_DD_total_parse_periodic_60MaxSDIthresh_1.1.png"))

#-------------- get the number and size for all dead trees for the SDI 60% threshold runs

parse_mortality_size <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.8, rcp, cc.scenario = "singleCC", parse,  scale.mort.prob = 1){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    
    if(parse == "full"){
      fn <- paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".", parse,".Rdata")
    }else{
      if(parse == "noSDI"){
        fn <- paste0("biomass_dataFIAperiodic_noSDI_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
      }else{
        
        fn <-  paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".", parse,".Rdata")
        
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
      
      tpa.di.m.time <- tpa.di.m %>% dplyr::select(id, TPADI) %>%  spread(key = id, value = TPADI)
      
      
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



mort.dbh.26.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "full" )})
mort.dbh.26 <- do.call(rbind, mort.dbh.26.list)

# mort.dbh.45.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "full" )})
# mort.dbh.45.test <- do.call(rbind, mort.dbh.45.list)
# 
# mort.dbh.60.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "full" )})
# mort.dbh.60.test <- do.call(rbind, mort.dbh.60.list)

# mort.dbh.85.list <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "full" )})
# mort.dbh.85.test <- do.call(rbind, mort.dbh.85.list)

mort.dbh.full.parse <- rbind(mort.dbh.26) #, 
                             #mort.dbh.45.test, 
                             #mort.dbh.60.test, 
                             #mort.dbh.85.test)


# get it for noCC:

mort.dbh.test.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", parse = "noCC" )})
mort.dbh.test.noCC <- do.call(rbind, mort.dbh.test.list.noCC)

# mort.dbh.45.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp45", cc.scenario = "singleCC", parse = "noCC" )})
# mort.dbh.45.test.noCC <- do.call(rbind, mort.dbh.45.list.noCC)
# 
# mort.dbh.60.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp60", cc.scenario = "singleCC", parse = "noCC" )})
# mort.dbh.60.test.noCC <- do.call(rbind, mort.dbh.60.list.noCC)

# mort.dbh.85.list.noCC <- lapply(unique(plots)[1:675],FUN = function(x){parse_mortality_size (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp85", cc.scenario = "singleCC", parse = "noCC" )})
# mort.dbh.85.test.noCC <- do.call(rbind, mort.dbh.85.list.noCC)


mort.dbh.full.parse.noCC <- rbind(mort.dbh.test.noCC )#, 
                                  #mort.dbh.45.test.noCC, 
                                  #mort.dbh.60.test.noCC, 
                                  #mort.dbh.85.test.noCC)


mort.dbh.all.parse <- rbind(mort.dbh.full.parse, 
                            
                            mort.dbh.full.parse.noCC)

# save as RDS:
saveRDS(mort.dbh.all.parse, here("outputs/", "all.plot.mort.dbh.N.60SDIthresh.RDS"))
mort.dbh.all.parse <- readRDS(here("outputs/", "all.plot.mort.dbh.N.60SDIthresh.RDS"))


# generate plots of dead by size class
mort.dbh.all.parse <- mort.dbh.all.parse  %>%
  mutate(dbh.class = cut(diameter, breaks=c(0, 10, 20, 30,50, 75, 120)))

n.mort.dbh.class <- mort.dbh.all.parse %>% group_by(mort.scheme, rcp, time, parse, dbh.class) %>%
  summarise(n.mort.dd = sum(TPAdd, na.rm = TRUE), 
            n.mort.di = sum(TPAdi, na.rm = TRUE), 
            n.total = sum(TPAfull, na.rm = TRUE))

pct.mort.dbh.class <- n.mort.dbh.class  %>% group_by(mort.scheme, rcp, time, parse) %>% mutate(total.trees.dead.dd = sum(n.mort.dd), 
                                                                                               total.trees.dead.di = sum(n.mort.di))%>%
  ungroup()%>% group_by(mort.scheme, rcp, time, parse, dbh.class) %>% mutate(pct.mort.dd = n.mort.dd/total.trees.dead.dd, 
                                                                             pct.mort.di =  n.mort.di/total.trees.dead.di)


parse.names <- data.frame(parse = c("full", "noCC"), 
                          scenario = c("full", "no climate change"))



n.mort.dbh.class <- left_join(n.mort.dbh.class, parse.names)

pct.mort.dbh.class <- left_join(pct.mort.dbh.class, parse.names)

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, color = dbh.class))+geom_line()+
  facet_grid(rows = vars(parse), cols = vars(rcp))



ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, color = scenario))+geom_line()+
  facet_grid(rows = vars(dbh.class), cols = vars(rcp))

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.di, color = dbh.class))+geom_line()+
  facet_grid(rows = vars(parse), cols = vars(rcp))

ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.di, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density independent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_by_DBH_DI_parse_periodic_60SDIthresh.png"))


ggplot(n.mort.dbh.class, aes(x = time, y = n.mort.dd, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density dependent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_by_DBH_DI_parse_periodic_60SDIthresh.png"))


ggplot(pct.mort.dbh.class, aes(x = time, y = pct.mort.di, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density independent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_pct_by_DBH_DI_parse_periodic_60SDIthresh.png"))


ggplot(pct.mort.dbh.class, aes(x = time, y = pct.mort.dd, col = dbh.class, fill = dbh.class))+geom_bar(stat = "identity")+
  facet_grid(rows = vars(scenario), cols = vars(rcp))+theme_bw()+ylab("# density dependent mortalities")
ggsave(height = 8, width = 8, units = "in", here("outputs/", "Dead_trees_pct_by_DBH_DD_parse_periodic_60SDIthresh.png"))

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
C.convert.livewood(parse.all.mort$mAGB.dead, C.frac = 0.501)

AGB.parse.totals  <- parse.all.mort %>% #select(PLT_CN, rcp, mort.scheme, year, parse, mAGB) %>% 
  group_by(PLT_CN, mort.scheme,rcp, year, parse) %>%
  
  #spread(parse, mAGB) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){C.convert.livewood(x)})) %>% 
  ungroup() %>% # sum across all the PLT_CNs
    group_by(rcp, mort.scheme, year, parse) %>%
  summarise(across(c(mAGB:low.foliage), sum)) # sum across all the PLT_CNs then divide by # plots to convert to per acre


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

png(height = 7, width = 12, units = "in", res = 300, "outputs/Carbon_density_regional_NPP_total_parse_periodic_26.png")
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
png(height = 3.5, width = 12, units = "in", res = 300, "outputs/Dead_Carbon_density_regional_total_parse_periodic_26.png")
parse.deadCarbon.totals
dev.off()

parse.dead.Carbon.flux.totals <- ggplot()+#geom_ribbon(data = AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, ymin = low.dead, ymax = up.dead, fill = parse), alpha = 0.5)+
  geom_line(data = AGB.parse.totals %>% filter(!year %in% 2001:2002) , aes(x = year, y = mNPP.dead, color = parse))+
  
  facet_wrap(~rcp, ncol = 4)+theme_bw(base_size = 14) + ylab( "Carbon Density Flux \n (Tg C/ha)")+theme(panel.grid = element_blank())+
  #scale_fill_manual( name = "Scenario",
   
  #                  values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI"="#7570b3"))+
  scale_color_manual( name = "Scenario",
                      values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI"="#7570b3", "no climate change, DDonly" = "black"))



#----------------------------------------------------------------------
# make plots where we subtract no-climate change from the full scenario
#----------------------------------------------------------------------

# calculate the total decline over time?
# how much is due to CC and how much is due to everything else. 



AGB.parse.totals.m <- reshape2::melt(AGB.parse.totals, id.vars = c("rcp", "mort.scheme", "year", "parse"))
AGB.parse.totals.m$value <- as.numeric(AGB.parse.totals.m$value)
AGB.parse.spread <- AGB.parse.totals.m %>% group_by(rcp, mort.scheme, year, variable) %>% 
  pivot_wider(names_from = year, values_from = value) %>% group_by(rcp, mort.scheme, parse, variable) %>%
  select(rcp, mort.scheme, parse, variable, `2002`, `2098`)%>% 
  # rename( "end.2098" = `2098`,
  #                                                                                    "start.2002"= `2002`)%>%
   mutate(DELTA = `2098` - `2002`) %>%
  ungroup() %>% select(-`2002`, -`2098`) %>% group_by(rcp, mort.scheme, parse, variable) %>%
  pivot_wider(names_from = parse, values_from= DELTA) %>% 
                           mutate(full.cc.diff =   `no climate change` - full , # effect of climate change
                                  full.no.SDI.diff =  full - `no SDI` ,  # not really the effect of SDI
                                  full.remainder =  abs(full) - `full.cc.diff` )  # full
View(AGB.parse.spread)
colnames(AGB.parse.spread)[3] <- "component"



# this needs work right now

AGB.parse.spread.melt <- reshape2::melt(AGB.parse.spread, id.vars = c("rcp", "mort.scheme", "component") )
colnames(AGB.parse.spread.melt)[4] <- "parse"

AGB.parse.spread2 <- AGB.parse.spread.melt %>% group_by(rcp, mort.scheme, parse) %>% 
  pivot_wider(names_from = component, values_from = value)
unique(AGB.parse.spread2$parse)

parse.Carbon.totals.diff <- ggplot()+ #geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+ 
  geom_ribbon(data = AGB.parse.spread2 %>% filter(!year %in% 2001:2002 & parse %in% c("full.cc.diff", "full.no.SDI.diff",  "full.remainder")), aes(x = year, ymin = lowA, ymax = upA, fill = parse), alpha = 0.5)+
  geom_line(data = AGB.parse.spread2 %>% filter(!year %in% 2001:2002 & parse %in% c("full.cc.diff", "full.no.SDI.diff", "full.remainder")) , aes(x = year, y = mAGB, color = parse))+
  
  facet_wrap(~rcp, ncol = 4)+ ylab( "Total Carbon in PIPO FIA stands\n (Tg C)") + theme_bw(base_size = 14)+theme(panel.grid = element_blank()) 
 
parse.Carbon.totals.diff  




 # scale_fill_manual( name = "Scenario",
  #                    values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))+
  # scale_color_manual( name = "Scenario",
  #                     values =c("full"="#1b9e77","no climate change"= "#d95f02", "no SDI growth"="#7570b3", "no SDI growth & mortality" = "grey", "no climate change, DDonly" = "black", "no climate change, DIonly" = "red", "full, DIonly" = "goldenrod"))
  # 

#----------------------------------------------------------------------
# separate out by high and low SDI
#----------------------------------------------------------------------

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

