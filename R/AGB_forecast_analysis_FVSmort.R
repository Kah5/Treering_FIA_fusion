library(ggplot2)
library(reshape2)
library(tidyverse)
library(rgdal)
library(firebehavioR)

# reading in the available forecasts (1-184 I believe)

get_biomass_ests <- function(plot, mort.scheme, scenario, SDI.ratio.DD, cc.scenario){
  
  cat(paste0("getting pred vs obs for ", as.character(plot)))
  
  if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))==TRUE){
    cat("no forecast")
  }else{
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{

    cat (paste("reading in forecasts from plot ", plot))
    load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
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
  }
}

get_biomass_ests(plot = 3169377010690, mort.scheme = "DIDD", scenario = "rcp26", SDI.ratio.DD= 0.8, cc.scenario = "singleCC")
plot = 3169377010690

# -------------------------------------------------------------------------------
# read in forecasts for all scenarios and mortality conditions
# -------------------------------------------------------------------------------
# RCP2.6
# so far only have DIDD for rcp2.6 done, and only 43 plots
# normort.AGB <- lapply(unique(plots)[1:10], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
# DIonly.AGB <- lapply(unique(plots)[1:417], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
# DDonly.AGB <- lapply(unique(plots)[1:417], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
DIDD.AGB <- lapply(unique(plots)[1:650], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
DIDD.AGB.45 <- lapply(unique(plots)[1:650], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
DIDD.AGB.60 <- lapply(unique(plots)[1:650], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
DIDD.AGB.85 <- lapply(unique(plots)[1:650], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85", SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})


# normort.AGB.df <- do.call(rbind, normort.AGB)
# DIonly.AGB.df <- do.call(rbind, DIonly.AGB)
# DDonly.AGB.df <- do.call(rbind, DDonly.AGB)
DIDD.AGB.df <- do.call(rbind, DIDD.AGB)
DIDD.AGB.45.df <- do.call(rbind, DIDD.AGB.45)
DIDD.AGB.60.df <- do.call(rbind, DIDD.AGB.60)
DIDD.AGB.85.df <- do.call(rbind, DIDD.AGB.85)



# -------------------------------------------------------------------------------
# combine all forecasts
# -------------------------------------------------------------------------------
all10plots <- rbind(DIDD.AGB.df, DIDD.AGB.45.df, DIDD.AGB.60.df, DIDD.AGB.85.df) #, 
#all10plots <- DIDD.AGB.df#, 

#normort.AGB.60.df, DIonly.AGB.60.df, DDonly.AGB.60.df, DIDD.AGB.60.df, 
#normort.AGB.45.df, DIonly.AGB.45.df, DDonly.AGB.45.df, DIDD.AGB.45.df, 
#normort.AGB.26.df, DIonly.AGB.26.df, DDonly.AGB.26.df, DIDD.AGB.26.df) #, 
#nocc.nomort.AGB.df, nocc.DIonly.AGB.df, nocc.DDonly.AGB.df, nocc.DIDD.AGB.df)
#all.AGB.26.nomort <- rbind(nocc.nomort.AGB.df, nocc.DDonly.AGB.df, nocc.DIDD.AGB.df, nocc.DIonly.AGB.df)

saveRDS(all10plots, "all.AGB.fiaperiodic_singleCC_0.8_full.RDS")

# -------------------------------------------------------------------------------
# read in forecasts for all scenarios and mortality conditions (double cc)
# -------------------------------------------------------------------------------
### Don't have any of the double cc scenarios done yet
# RCP2.6
# normort.AGB <- lapply(unique(plots)[1:417], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# DIonly.AGB <- lapply(unique(plots)[1:417], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# DDonly.AGB <- lapply(unique(plots)[1:417], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
DIDD.AGB <- lapply(unique(plots)[1:43], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", SDI.ratio.DD = 0.8, cc.scenario = "doubleCC")})


normort.AGB.df <- do.call(rbind, normort.AGB)
DIonly.AGB.df <- do.call(rbind, DIonly.AGB)
DDonly.AGB.df <- do.call(rbind, DDonly.AGB)
DIDD.AGB.df <- do.call(rbind, DIDD.AGB)



# -------------------------------------------------------------------------------
# combine all forecasts 
# -------------------------------------------------------------------------------
all10plots <- rbind(normort.AGB.df, DIonly.AGB.df, DDonly.AGB.df, DIDD.AGB.df) #, 
#normort.AGB.60.df, DIonly.AGB.60.df, DDonly.AGB.60.df, DIDD.AGB.60.df, 
#normort.AGB.45.df, DIonly.AGB.45.df, DDonly.AGB.45.df, DIDD.AGB.45.df, 
#normort.AGB.26.df, DIonly.AGB.26.df, DDonly.AGB.26.df, DIDD.AGB.26.df) #, 
#nocc.nomort.AGB.df, nocc.DIonly.AGB.df, nocc.DDonly.AGB.df, nocc.DIDD.AGB.df)
#all.AGB.26.nomort <- rbind(nocc.nomort.AGB.df, nocc.DDonly.AGB.df, nocc.DIDD.AGB.df, nocc.DIonly.AGB.df)

saveRDS(all10plots, "all.AGB.fiaperiodic_doubleCC_12.30.22tempfile.RDS")


#all10plot <- readRDS( "all.AGB.fiaperiodic_singleCC_12.30.22tempfile.RDS")
# all10plot.nocc<- rbind(all.AGB.26.nomort, all10plot)
# saveRDS(all10plot.nocc, "all.AGB.10.26.22tempfile.RDS")
# all10plot.nocc <- readRDS("all.AGB.10.26.22tempfile.RDS")

ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
  summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)

# -------------------------------------------------------------------------------
#  make plots of total biomass across the region
# -------------------------------------------------------------------------------
AGB.line <- ggplot()+geom_line(data = ten.plot.summary, aes(year, mAGB, group = mort.scheme, color = mort.scheme))+theme_bw()+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)+ylim(0,1.7e7)
NPP.line <- ggplot(ten.plot.summary, aes(year, mNPP, group = mort.scheme, color = mort.scheme))+geom_line()+theme_bw()+ylab("Total NPP for all  plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)+ylim(-6e5,2e5)

png(height = 7, width = 10, units = "in", res = 150, "outputs/allplotsFIAperiodic.total.biomass.singleCC.png")
cowplot::plot_grid(AGB.line, NPP.line, ncol = 1, align = "hv")
dev.off()


# -------------------------------------------------------------------------------
#  make plots of big tree vs small across the region
# -------------------------------------------------------------------------------
#big tree vs small tree carbon in forecasts
get_tree_levelC_ests_FVS <- function(plot, mort.scheme, scenario, nocc = FALSE, SDI.ratio.DD = 0.7, cc.scenario = "doubleCC"){
  cat(paste("reading data from", plot))
  cat(paste0("getting pred vs obs for ", as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
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
    #oldTREE$DRYBIO_AG
    
    # dead.diams <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% 
    #   dplyr::select(CN, dead.class, PREVDIA, TPAMORT_UNADJ, TPA_UNADJ, PREV_TPA_UNADJ, AGENTCD) %>% mutate(PREVDBH = PREVDIA*2.54)
    # #rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
    cat (paste("reading in forecasts from plot ", plot))
    
    
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".",  cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
    }#load("biomass_
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
}


# -------------------------------------------------------------------------------
# read in everything for single CC
# -------------------------------------------------------------------------------

# read in and get the tree level estimates
# RCP 2.6
btst.AGB.DIDD.26 <- lapply(unique(plots)[1:43], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
btst.AGB.DIDD.26.df


# btst.AGB.nomort.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "nomort", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
# btst.AGB.nomort.26.df <- do.call(rbind, btst.AGB.nomort.26)
# 
# btst.AGB.DDonly.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DDonly", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
# btst.AGB.DDonly.26.df <- do.call(rbind, btst.AGB.DDonly.26)
# 
# 
# btst.AGB.DIonly.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIonly", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
# btst.AGB.DIonly.26.df <- do.call(rbind, btst.AGB.DIonly.26)


# combine all the tree-level datasets together:
allplots.treeC <- rbind(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df)#, 
allplots.treeC <- btst.AGB.DIDD.26.df
#btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
#btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
#btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df) #, 
#btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)
saveRDS(allplots.treeC, "outputs/allplots.FIAperiodic.treeC1.20.23.RDS")


# -------------------------------------------------------------------------------
# read in everything for double CC
# -------------------------------------------------------------------------------

# # read in and get the tree level estimates
# # RCP 2.6
# btst.AGB.DIDD.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
# btst.AGB.DIDD.26.df
# 
# 
# btst.AGB.nomort.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "nomort", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# btst.AGB.nomort.26.df <- do.call(rbind, btst.AGB.nomort.26)
# 
# btst.AGB.DDonly.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DDonly", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# btst.AGB.DDonly.26.df <- do.call(rbind, btst.AGB.DDonly.26)
# 
# 
# btst.AGB.DIonly.26 <- lapply(unique(plots)[1:417], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIonly", scenario = "rcp26",SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
# btst.AGB.DIonly.26.df <- do.call(rbind, btst.AGB.DIonly.26)
# 
# 
# # combine all the tree-level datasets together:
# allplots.treeC <- rbind(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df)#, 
# #btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
# #btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
# #btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df) #, 
# #btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)
# saveRDS(allplots.treeC, "outputs/allplots.FIAperiodic.treeC12.30.22_doubleCC.RDS")
# 

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

#allplots.treeC <- rbind(btst.AGB.DIDD.26.df, btst.AGB.nomort.26.df,  btst.AGB.DDonly.26.df)

png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_all_run_plotsFIAperiodic_singleCC_43plots.png")
ggplot(na.omit(allplots.treeC), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (kg/acre)")+theme_bw()+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

# example for how single plots may vary quite alot!
png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_one_plot_FIAperiodic_singleCC.png")
ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[1]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_two_plotFIAperiodic_singleCC.png")

ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[2]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_three_plotFIAperiodic_singleCC.png")

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

png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_dominated_FIAperiodic_singleCC.png")

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
png(height = 10, width = 10, units = "in", res = 150, "outputs/big_tree_vs_small_tree_dominated_prop_biomass_FIAperiodic_singleCC.png")

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




ggplot(plotC.totals.prop, aes(prop.big))+geom_histogram()
ggplot(plotC.totals.prop, aes(prop.big))+geom_histogram()+geom_density()+facet_wrap(~time)

# how to classify these different trajectories?

plotC.totals.prop %>% group_by(plot, mort.scheme, scenario, time) %>% select(-total.AGB, -prop.small, )




# -------------------------------------------------------------------------------
#  make plots of big tree vs small tree mortality
# -------------------------------------------------------------------------------
get_tree_diam_live_dead_ests <- function(plot, mort.scheme, scenario, SDI.ratio.DD = 0.8,nocc = FALSE, cc.scenario){
  cat(paste0("reading in plot "), plot)
  
  if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))==TRUE){
    cat("no forecast")
  }else{
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
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
    # 
    # 
    # 
    # STATUSCD_change$DRYBIO_AG
    # oldTREE$DRYBIO_AG
    
    # dead.diams <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% 
    #   dplyr::select(CN, dead.class, PREVDIA, TPAMORT_UNADJ, TPA_UNADJ, PREV_TPA_UNADJ, AGENTCD) %>% mutate(PREVDBH = PREVDIA*2.54)
    # #rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
    cat (paste("reading in forecasts from plot ", plot))
    
    
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    }else{
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot,".",scenario,".", SDI.ratio.DD, ".",  cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      
    }#load("biomass_data_FVSmort/plot2AGB_DIDD.2449653010690.rcp26.Rdata")
    # objects
    # out, AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
    # AGB.foliage, NPP.foliage, 
    # AGB.stembark, NPP.stembark,
    # AGB.stemwood, NPP.stemwood,
    # AGB.branchdead, NPP.branchdead,
    # AGB.branchlive
    
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
  }
}


# -------------------------------------------------------------------------------
# for single CC
# -------------------------------------------------------------------------------

# read in and get the tree level estimates
# RCP 2.6
btst.DIAMS.DIDD.26 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
btst.DIAMS.DIDD.26.df <- do.call(rbind, btst.DIAMS.DIDD.26)

# RCP 4.5
btst.DIAMS.DIDD.45 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
btst.DIAMS.DIDD.45.df <- do.call(rbind, btst.DIAMS.DIDD.45)

# RCP 6.0
btst.DIAMS.DIDD.60 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
btst.DIAMS.DIDD.60.df <- do.call(rbind, btst.DIAMS.DIDD.60)

# RCP 8.5
btst.DIAMS.DIDD.85 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
btst.DIAMS.DIDD.85.df <- do.call(rbind, btst.DIAMS.DIDD.85)


#rcp26.DIAMS <- btst.DIAMS.DIDD.26.df
rcp26.DIAMS <-  rbind(btst.DIAMS.DIDD.26.df, btst.DIAMS.DIDD.45.df, btst.DIAMS.DIDD.60.df, btst.DIAMS.DIDD.85.df)
saveRDS(rcp26.DIAMS, "periodicFIA.btst.DIAMS.allrcps.RDS")

# # checking to see if the mort trees are being labeled properly:
# ggplot(btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]), aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# 
# filtered.test <- btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]) %>% filter(status == df)
# ggplot(filtered.test, aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# # combine all the tree-level datasets together:
allplots.treeDIAM  <- btst.DIAMS.DIDD.26.df
#allplots.treeDIAM <- rbind(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df )#,
#btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
#btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df,
#btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df) #,
#btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#nocc.allplots.treeeDIAM <- rbind(btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#saveRDS(nocc.allplots.treeeDIAM, "outputs/allplots.treeDiam.forecast.nocc.RDS")

saveRDS(rcp26.DIAMS , "outputs/allplots.treeDiam.forecast.periodicFIA.full.RDS")



# -------------------------------------------------------------------------------
# for double CC
# -------------------------------------------------------------------------------

# # read in and get the tree level estimates
# # RCP 2.6
# btst.DIAMS.DIDD.26 <- lapply(unique(plots)[1:43], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", cc.scenario = "doubleCC")})
# btst.DIAMS.DIDD.26.df <- do.call(rbind, btst.DIAMS.DIDD.26)
# btst.DIAMS.DIDD.26.df
# 
# btst.DIAMS.nomort.26<- lapply(unique(plots)[1:417], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", cc.scenario = "doubleCC")})
# btst.DIAMS.nomort.26.df <- do.call(rbind, btst.DIAMS.nomort.26)
# # 
# btst.DIAMS.DDonly.26<- lapply(unique(plots)[1:417], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", cc.scenario = "doubleCC")})
# btst.DIAMS.DDonly.26.df <- do.call(rbind, btst.DIAMS.DDonly.26)
# # 
# btst.DIAMS.DIonly.26<- lapply(unique(plots)[1:417], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", cc.scenario = "doubleCC")})
# btst.DIAMS.DIonly.26.df <- do.call(rbind, btst.DIAMS.DIonly.26)
# # 
# 
# rcp26.DIAMS <-  rbind(btst.DIAMS.DIDD.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.nomort.26.df)
# saveRDS(rcp26.DIAMS, "annaulFIA.btst.DIAMS_doubleCC.RDS")

# # checking to see if the mort trees are being labeled properly:
# ggplot(btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]), aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# 
# filtered.test <- btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]) %>% filter(status == df)
# ggplot(filtered.test, aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# # combine all the tree-level datasets together:
#allplots.treeDIAM <- rbind(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df )#,
#btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
#btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df,
#btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df) #,
#btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#nocc.allplots.treeeDIAM <- rbind(btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#saveRDS(nocc.allplots.treeeDIAM, "outputs/allplots.treeDiam.forecast.nocc.RDS")

#saveRDS(allplots.treeDIAM, "outputs/allplots.treeDiam.forecast.periodicFIA_doubleCC.RDS")



rm(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df,
   btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
   btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df,
   btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df) #,
#btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
rm(btst.DIAMS.nomort.26 , btst.DIAMS.DIonly.26 , btst.DIAMS.DDonly.26 , btst.DIAMS.DIDD.26 ,
   btst.DIAMS.nomort.45 , btst.DIAMS.DIonly.45 , btst.DIAMS.DDonly.45 , btst.DIAMS.DIDD.45 ,
   btst.DIAMS.nomort.60 , btst.DIAMS.DIonly.60 , btst.DIAMS.DDonly.60 , btst.DIAMS.DIDD.60 ,
   btst.DIAMS.nomort.85 , btst.DIAMS.DIonly.85 , btst.DIAMS.DDonly.85 , btst.DIAMS.DIDD.85) #,
#btst.DIAMS.nomort.nocc , btst.DIAMS.DIonly.nocc , btst.DIAMS.DDonly.nocc , btst.DIAMS.DIDD.nocc )
#
#
allplots.treeDIAMsubset <- allplots.treeDIAM

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
saveRDS(all.trees.2001.2010, "all.trees.2001.2010.FIAperiodic_singleCC.rds")
all.trees.2001.2010 <- readRDS("all.trees.2001.2010.FIAperiodic_singleCC.rds")

all.trees.2011.2020 <- allplots.treeDIAMsubset %>% group_by(df,plot,  mort.scheme, scenario, time) %>% filter(time %in% 11:20)%>%
  summarise(ntree = sum(TPA))%>%ungroup()  %>% 
  group_by(plot, mort.scheme, scenario, time) %>% spread(df, value = ntree)%>% group_by(plot, mort.scheme, scenario, time) %>% 
  mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))# gets # dead for each plot in each year
saveRDS(all.trees.2011.2020, "all.trees.2011.2020.FIAperiodic_singleCC.rds")
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
# 
# fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))
# 
# PLOT <- fiadb$PLOT
# SUBPLOT <- fiadb$SUBPLOT
# STATECD <- fiadb$STATECD
# COND <- fiadb$COND
# TREE <- fiadb$TREE
# rm(fiadb)
# 
# # get the previous survey for each tree: 
# TREE_remeas <- subset(TREE, !is.na(PREVDIA))
# TREE_remeas <- subset(TREE_remeas, STATUSCD == 1 | STATUSCD == 2) 
# 
# # Look up previous AGB
# ### look up previous AGB, PLT_CN, and CONDID
# TREE_remeas$PREV_DRYBIO_AG <- TREE$DRYBIO_AG[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
# TREE_remeas$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
# TREE_remeas$PREV_CONDID <- TREE$CONDID[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
# 
# # Subset records with previous AGB found
# TREE_remeas <- subset(TREE_remeas, !is.na(PREV_DRYBIO_AG))
# 
# # for growth analysis
# TREE_remeas$DRYBIO_AG_DIFF <- TREE_remeas$DRYBIO_AG - TREE_remeas$PREV_DRYBIO_AG
# TREE_remeas$DIA_DIFF <- TREE_remeas$DIA - TREE_remeas$PREVDIA
# 
# # basal area increment
# TREE_remeas$BAt1 <- ((TREE_remeas$PREVDIA/2)^2)*3.14159
# TREE_remeas$BAt2 <- ((TREE_remeas$DIA/2)^2)*3.14159
# TREE_remeas$BA_DIFF <- TREE_remeas$BAt2 - TREE_remeas$BAt1
# 
# # Read in plot data and get coordinates and previous measurement year
# #plots <- read.csv(paste(data.path,"PLOT_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)
# 
# TREE_remeas$LAT <- PLOT$LAT[match(TREE_remeas$PLT_CN, PLOT$CN)]
# TREE_remeas$LON <- PLOT$LON[match(TREE_remeas$PLT_CN, PLOT$CN)]
# TREE_remeas$ELEV <- PLOT$ELEV[match(TREE_remeas$PLT_CN, PLOT$CN)]
# TREE_remeas$MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PLT_CN, PLOT$CN)]
# TREE_remeas$PREV_MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PREV_PLT_CN, PLOT$CN)]
# 
# # Calculate census interval
# TREE_remeas$CENSUS_INTERVAL <- TREE_remeas$MEASYEAR - TREE_remeas$PREV_MEASYEAR
# 
# colnames(TREE_remeas)
# 
# # zero if no change, 
# # 2 if the tree died in the census interval
# TREE_remeas$STATUSCD_CHANGE <- ifelse(TREE_remeas$PREV_STATUS_CD == TREE_remeas$STATUSCD, 0, 
#                                       ifelse(TREE_remeas$PREV_STATUS_CD == 1 & TREE_remeas$STATUSCD == 2, 2, NA))
# 
# # match up PLOT and COND data
# PLOT$DSTRBYR1 <- COND$DSTRBYR1[match(PLOT$CN, COND$PLT_CN)]
# PLOT$DSTRBYR2 <- COND$DSTRBYR2[match(PLOT$CN, COND$PLT_CN)]
# PLOT$DSTRBYR3 <- COND$DSTRBYR3[match(PLOT$CN, COND$PLT_CN)]
# 
# 
# PLOT$DSTRBCD1 <- COND$DSTRBCD1[match(PLOT$CN, COND$PLT_CN)]
# PLOT$DSTRBCD2 <- COND$DSTRBCD2[match(PLOT$CN, COND$PLT_CN)]
# PLOT$DSTRBCD3 <- COND$DSTRBCD3[match(PLOT$CN, COND$PLT_CN)]
# 
# # Match up the tree and plot data
# TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
# TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
# TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
# TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]
# 
# 
# TREE$DSTRBCD1 <- PLOT$DSTRBCD1[match(TREE$PLT_CN, PLOT$CN)]
# TREE$DSTRBCD2 <- PLOT$DSTRBCD2[match(TREE$PLT_CN, PLOT$CN)]
# TREE$DSTRBCD3 <- PLOT$DSTRBCD3[match(TREE$PLT_CN, PLOT$CN)]
# 
# TREE$DSTRBYR1 <- PLOT$DSTRBYR1[match(TREE$PLT_CN, PLOT$CN)]
# TREE$DSTRBYR2 <- PLOT$DSTRBYR2[match(TREE$PLT_CN, PLOT$CN)]
# TREE$DSTRBYR3 <- PLOT$DSTRBYR3[match(TREE$PLT_CN, PLOT$CN)]
# 
# 
# unique(TREE$MORTCD)
# unique(TREE$STATECD)
# 
# # TREE_remeas %>% dplyr::filter(SPCD %in% "122" & STATUSCD %in% c(1, 2)) %>% group_by(STATUSCD_CHANGE) %>% summarise(median.dbh = median(DIA, na.rm = TRUE), 
# #                                                                                                                    median.ht = median(HT, na.rm = TRUE))
# # 
# # ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(DIA))+facet_wrap(~STATUSCD_CHANGE)
# # 
# # ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(HT))+facet_wrap(~STATUSCD_CHANGE)
# 
# # goal: make plots of the distribution of tree diameters that died with groups of diameter, heights, subplot SDIs, and SI
# # compare these to biomass estimates
# # get a static estimate of SDI, which includes the dead trees:

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
prop.dead.2020 <- allplots.treeDIAMsubset %>% group_by(SDIbin, DIAbin, df, mort.scheme, scenario) %>% filter(time %in% 1:20) %>% summarise(ntree = sum(TPA)) %>% 
  ungroup() %>% group_by (SDIbin, DIAbin, mort.scheme, scenario) %>% spread(`ntree`, key = df) %>% mutate(prop.dead = `dead`/(`dead`+`live`)) %>% mutate(prop.dead = ifelse(is.na(prop.dead), 0, prop.dead),
                                                                                                                                                         mort.rate = prop.dead/20)

prop.dead.2020$total = prop.dead.2020$dead + prop.dead.2020$live

png(height = 10, width = 10, units = "in", res = 150, "outputs/scatter_mort_rate_by_dia_sdi_lines_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes(x = DIAbin, y = mort.rate, color = SDIbin, group = SDIbin))+
  geom_point()+geom_line()+theme_bw()+ylab("mortality rate")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


png(height = 10, width = 10, units = "in", res = 150, "outputs/scatter_total_by_dia_sdi_lines_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes(x = DIAbin, y = total, color = SDIbin, group = SDIbin))+
  geom_point()+geom_line()+theme_bw()+ylab("total number of trees (live and dead)")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/boxplot_totals_rate_by_dia_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = total, x = DIAbin))+
  geom_boxplot()+theme_bw()+ylab("total number of trees (live and dead)")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


png(height = 10, width = 10, units = "in", res = 150, "outputs/boxplot_mort_rate_by_dia_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = mort.rate, x = DIAbin))+
  geom_boxplot()+theme_bw()+ylab("mortality rate")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/boxplot_mort_rate_by_sdi_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = mort.rate, x = SDIbin))+
  geom_boxplot()+theme_bw()+ylab("mortality rate")+xlab("SDI Class")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/boxplot_total_by_sdi_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin)), aes( y = total, x = SDIbin))+
  geom_boxplot()+theme_bw()+ylab("total number of trees (live and dead)")+xlab("SDI Class")+theme(panel.grid = element_blank())+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/tile_prop_mort_by_dia_sdi_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin) & !is.na(SDIbin)), aes( x = DIAbin, y = SDIbin, fill = mort.rate))+
  geom_raster()+scale_fill_gradientn(colors = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

png(height = 10, width = 10, units = "in", res = 150, "outputs/tile_total_by_dia_sdi_forecast_all_singleCC.png")
ggplot(prop.dead.2020 %>% filter(!is.na(DIAbin) & !is.na(SDIbin)), aes( x = DIAbin, y = SDIbin, fill = total))+
  geom_raster()+scale_fill_gradientn(colors = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))+
  facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()


# We cant compare to the number of dead for these same plots over this time period because there is not linkage of periodic and periodic                                                                                                                                                                                      mort_rate_ecoregion_2020 =  prop_dead_ecoregion_2020/10)
#------------------------------------------------------------------------------------
# get torching and crowning indices from the diameter distributions
#------------------------------------------------------------------------------------
# get the aboveground woody biomass component:
all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8_full.RDS")
all.woody.agb.26 <- all10plot %>% group_by(plot, mort.scheme, rcp) %>% arrange(plot, mort.scheme, rcp, year) %>% 
  #group_by(plot, mort.scheme, rcp, year) %>% 
  #mutate(lag.value = lag(mAGB.dead)) #%>% 
  mutate(dead.lagged = dplyr::lag(mAGB.dead, n = 5,  order_by=year))%>% group_by(plot, mort.scheme, rcp, year)%>% #select(mAGB.stemwood, mAGB.stembark, mAGB.branchdead, mAGB.branchlive) %>%
  summarise(woody.biomass = mAGB.stemwood + mAGB.stembark + mAGB.branchlive, 
            dead = mAGB.dead,
            dead.lag = dead.lagged)



time.df <- data.frame(year = unique(all.woody.agb.26$year),
                      time = 1:length(unique(all.woody.agb.26$year)))

colnames(all.woody.agb.26)[3] <- "scenario"

all.woody.agb.26 <- left_join(all.woody.agb.26, time.df)

unique(all.woody.agb.26$scenario)
rm(all10plot.nocc)

# to estimate ht from DIameters, generate a simple relationship:
pipo.tree <- TREE %>% filter(SPCD %in% "122")

glm.ht.dia <- lm(data = pipo.tree, HT ~ DIA)
summary(glm.ht.dia)

glm.ht.dia$coefficients

rm(fiadb)
# get the diameter distributions of each plot
forecast.plt  <- readRDS("outputs/allplots.treeDiam.forecast.periodicFIA.full.RDS")
#forecast.plt <- allplots.treeDIAM
rm(allplots.treeDIAM)
rm(rcp26.DIAMS)
rm(SUBPLOT)
rm(TREE_remeas)
rm(NPP.line)
rm(allplots.treeDIAMsubset)
#forecast.plt <- allplots.treeDIAMsubset #%>% filter(plot %in% plt)

forecast.plt$TPH <- forecast.plt$TPA*(1/0.404686)
forecast.plt$BA <- ((pi*(forecast.plt$DBH/2)^2)*forecast.plt$TPH)*(1/10000) # convert from sq cm to sq m and multiply by # trees/ha # tree BAshould be in m2/ha
forecast.plt$HT <- (forecast.plt$DBH/2.54)*glm.ht.dia$coefficients[2] + glm.ht.dia$coefficients[1]
forecast.plt$HT_m <- forecast.plt$HT*0.304
forecast.plt$HT_m <- ifelse(forecast.plt$HT_m > 62, 62, forecast.plt$HT_m)

# select some years to assess fire risk in:
# lets say year 1 (2002), 24 (2025), 49 2050, 74, 97 2098
plt.characteristics.all <- forecast.plt %>% filter(df %in% "live")%>% #filter(time %in% c(1, 24, 49,74,97))%>% 
  group_by (plot, mort.scheme, scenario, time) %>% summarise(ba = sum(BA, na.rm =TRUE), 
                                                             ht = mean(HT_m, na.rm = TRUE), 
                                                             tph = sum(TPH, na.rm = TRUE))
plt.characteristics.all$ba <- ifelse(plt.characteristics.all$ba >= quantile(plt.characteristics.all$ba, 0.975),quantile(plt.characteristics.all$ba, 0.975), plt.characteristics.all$ba)

#all.woody.agb.26 <- all.woody.agb.26 #%>%  filter(time %in% c(1, 24, 49,74,97))
all.woody.agb.26$woody.biomass <- ifelse(all.woody.agb.26$woody.biomass >= quantile(all.woody.agb.26$woody.biomass, c(0.999)), quantile(all.woody.agb.26$woody.biomass, c(0.999)), all.woody.agb.26$woody.biomass)
all.woody.agb.26$dead <- ifelse(all.woody.agb.26$dead >= quantile(all.woody.agb.26$dead, c(0.999)), quantile(all.woody.agb.26$dead, c(0.999)), all.woody.agb.26$dead)


summary(all.woody.agb.26$woody.biomass/1000)
summary(all.woody.agb.26$dead*0.001)
summary(plt.characteristics.all$ba)

library(firebehavioR)
data(fuelModels, fuelMoisture)
exampSurfFuel = fuelModels['TU1',]

fuelMoisture['D1L1',]

exampFuelMoisture = fuelMoisture['D1L1',]

# naw.az <- terra::rast("nawfd_arizona.tif")
# plot(naw.az)
# rm(naw.az)
# fuelModels
# read in the summary stats from the MTRI fuels product:
# ideally down the road I will draw samples and propagate the uncertainty through

NAM.means <- read.csv("data/NAmWildlandFuelsDatabase_evt_groups_2022-12-30.csv")
pipo.fuels <- NAM.means %>% filter(evt_group_name %in% "Ponderosa Pine Forest| Woodland and Savanna" )

colnames(pipo.fuels)


plt <- "31367645010690"

get_torch_crown_indices_FORECASTS <- function(plt, fuelmodel){
  cat(paste0("getting pred vs obs for ", as.character(plt)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    # ex.plt <- TREE %>% filter(PLT_CN %in% plt)
    # ex.plt$HT_m <- ex.plt$HT*0.304
    # ex.plt$TPH <- ex.plt$TPA_UNADJ*(1/0.404686)
    # ex.plt$DBH_m <-ex.plt$DIA*2.54 
    # ex.plt$BA <- (pi*(ex.plt$DBH_m/2)^2)/10000 # should be in m2
    
    # forecast.plt <- allplots.treeDIAMsubset %>% filter(plot %in% plt)
    # 
    # forecast.plt$TPH <- forecast.plt$TPA*(1/0.404686)
    # forecast.plt$BA <- (pi*(forecast.plt$DBH/2)^2)/10000 # should be in m2
    
    plt.characters <- plt.characteristics.all %>% filter(plot %in% plt) %>% group_by(mort.scheme, scenario, time)
    
    plt.woody.agb <- all.woody.agb.26  %>% filter(plot %in% plt)%>% group_by(mort.scheme, scenario, time )
    
    plt.characteristics <- left_join(plt.characters, plt.woody.agb , by = c("plot", "mort.scheme","scenario","time"))
    
    plt.characteristics$dead.lag <- ifelse(is.na(plt.characteristics$dead.lag), 0, plt.characteristics$dead.lag)
    # calculate height from the FIA survey data
    # survey.characteristics <- ex.plt  %>% summarise(ba = sum(BA, na.rm =TRUE), 
    #                                     ht = mean(HT_m, na.rm = TRUE), 
    #                                     tph = sum(TPH, na.rm = TRUE))
    plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
    
    
    if(nrow(plt.characteristics) <=1){
      cat("no plot forecasts")
    }else{
      
      cat(paste("calculating crown fuels for plot", plt))
      for(i in 1:length(plt.characteristics$plot)){
        #cat(paste("year", i))
        plt.CrownFuel[i,] = as.matrix(canFuel(ba = plt.characteristics[i,]$ba, ht = plt.characteristics[i,]$ht, tph = plt.characteristics[i,]$tph, type = "pp")) # pp is ponderosa pine:
      }
      colnames(plt.CrownFuel) <- c("cfl", "cbd", "cbh")
      
      exampCrownFuel = data.frame(
        CBD = plt.CrownFuel[,"cbd"], # Canopy bulk density
        FMC = rep(80,length(plt.CrownFuel[,"cbd"])),  # foliar moisture content..assuming 100%?
        CBH = plt.CrownFuel[,"cbh"], # Canopy base height
        CFL = plt.CrownFuel[,"cfl"] # canopy fuel load
      )
      
      
      # Choose an example fuel model and fuel mositure scenario
      # need to do some research on what this is/means
      #all.woody.agb.26  %>% filter(plot %in% plt)
      repno <- length(plt.characteristics$woody.biomass)
      
      #exampSurfFuel = fuelModels['TU5',]
      exampSurfFuel = fuelModels[fuelmodel,]
      # could also just run with TL 5 or 9 (more fuel than TU model) and TU models--TL model should have lower TI indices
      
      pipoSurfFuel <- data.frame(fuelModelType = rep("S", repno),
                                 loadLitter = rep( pipo.fuels$fl_litter_mg_p_ha_q50, repno),
                                 load1hr = rep( exampSurfFuel$load1hr, repno), 
                                 load10hr =rep( exampSurfFuel$load10hr, repno), 
                                 
                                 # load1hr = ifelse(plt.characteristics$dead*0.001 ==0 |
                                 #                    is.na(plt.characteristics$dead), pipo.fuels$fl_1hr_mg_p_ha_q25, 
                                 #                  ((plt.characteristics$dead*0.001))*0.10),
                                 # load10hr = ifelse(plt.characteristics$dead*0.001 == 0|
                                 #                     is.na(plt.characteristics$dead),  pipo.fuels$fl_10hr_mg_p_ha_q25, 
                                 #                   ((plt.characteristics$dead*0.001))*0.30),
                                 load100hr = ifelse(plt.characteristics$dead.lag*0.001 == 0|
                                                      is.na(plt.characteristics$dead.lag),   exampSurfFuel$load100hr,
                                                    (plt.characteristics$dead.lag*0.001)), 
                                 # put all the dead in the 100hr fuels since most will be larger trees
                                 # with a ~5-10 year lag to mimic real dead trees 
                                 
                                 loadLiveHerb = rep( pipo.fuels$fl_herb_mg_p_ha_mean, repno), 
                                 loadLiveWoody = rep( pipo.fuels$fl_shrub_mg_p_ha_mean, repno), 
                                 #loadLiveWoody = ifelse(is.na(plt.characteristics$woody.biomass*0.001),0, (plt.characteristics$woody.biomass*0.001)), # convert kg/acre to Mg/ha
                                 savLitter = rep( exampSurfFuel$savLitter, repno),   
                                 sav1hr = rep( exampSurfFuel$sav1hr, repno),   
                                 sav10hr = rep( exampSurfFuel$sav10hr, repno),   
                                 sav100hr = rep( exampSurfFuel$sav100hr,  repno),  
                                 savLiveHerb = rep( exampSurfFuel$savLiveHerb,  repno),  
                                 savLiveWoody = rep( exampSurfFuel$savLiveWoody,  repno),  
                                 
                                 fuelBedDepth = rep( pipo.fuels$fl_duff_mg_p_ha_mean, repno), 
                                 mxDead = rep( exampSurfFuel$mxDead, repno), 
                                 heat = rep( exampSurfFuel$heat,  repno), 
                                 description = rep( exampSurfFuel$description, repno), 
                                 source = rep( exampSurfFuel$source, repno))
      
      
      #pipoSurfFuel$load1hr <- ifelse(pipoSurfFuel$load1hr > pipo.fuels$fl_1hr_mg_p_ha_max, pipo.fuels$fl_1hr_mg_p_ha_max, pipoSurfFuel$load1hr)
      #pipoSurfFuel$load10hr <- ifelse(pipoSurfFuel$load10hr > pipo.fuels$fl_10hr_mg_p_ha_max, pipo.fuels$fl_10hr_mg_p_ha_max, pipoSurfFuel$load10hr)
      #pipoSurfFuel$load100hr <- ifelse(pipoSurfFuel$load100hr > pipo.fuels$fl_100hr_mg_p_ha_max, pipo.fuels$fl_100hr_mg_p_ha_max, pipoSurfFuel$load100hr)
      #pipoSurfFuel$loadLiveWoody <- ifelse(pipoSurfFuel$loadLiveWoody > pipo.fuels$fl_tree_mg_p_ha_max, pipo.fuels$fl_tree_mg_p_ha_max, pipoSurfFuel$loadLiveWoody)
      
      
      # assume very dry dead fuel loads:
      #fuelMoisture['D1L1',]
      
      #exampFuelMoisture = fuelMoisture['D1L1',]
      # get slope from COND data
      #ex.plot.data <-PLOT %>% filter(CN %in% "2447353010690") 
      ex.COND.data <- COND %>% filter(PLT_CN %in% plt) 
      waffull <- vector()
      for(i in 1:length(plt.characteristics$ht)){
        waffull[i] <- waf(pipo.fuels$fl_duff_mg_p_ha_mean, plt.characteristics[i,]$ht, sheltered = "y")
      }
      
      if(nrow(ex.COND.data)==0){
        exampEnviro = data.frame(
          slope = rep(0, length(plt.characteristics$ht)),
          windspeed = rep(40, length(plt.characteristics$ht)), # windspeed (at 10m, open)
          direction = rep(0,length(plt.characteristics$ht)), # direction of wind, from uphill
          waf = waffull # Wind adjustment factor
        )
      }else{
        
        exampEnviro = data.frame(
          slope = rep(ex.COND.data$SLOPE, length(plt.characteristics$ht)),
          windspeed = rep(40, length(plt.characteristics$ht)), # windspeed (at 10m, open)
          direction = rep(0,length(plt.characteristics$ht)), # direction of wind, from uphill
          waf = waffull # Wind adjustment factor
        )
      }
      
      # now lets calculate the TI and CI
      #plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
      ex.2 <-TI.CI.list <- list()
      
      for(i in 1:length(plt.characteristics$plot)){
        if(exampCrownFuel[i,"CBD"] == 0 | exampCrownFuel[i,"CFL"] == 0){
          ex.2[[i]] <- ex.2[[i-1]]
          ex.2[[i]]$fireBehavior$`Torching Index [m/min]` <- 0
          ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]` <- 0
        }else{
          
          #surfFuel, moisture, crownFuel, enviro, rosMult = 1, cfbForm = "f", folMoist = "y"
          ex.2[[i]] = rothermel(surfFuel = pipoSurfFuel[i,], moisture = exampFuelMoisture, crownFuel= exampCrownFuel[i,], enviro = exampEnviro[i,], rosMult = 1, cfbForm = "f", folMoist = "y")
        }
        
        # add surface flame length to outputs
        TI.CI.list[[i]] <- data.frame(TI =  ex.2[[i]]$fireBehavior$`Torching Index [m/min]`, 
                                      CI =  ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]`, 
                                      TI_km_hr =  (ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60,
                                      TI_km_hr_trunc =  ifelse((ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60 >= 200, 200, (ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60),
                                      CI_km_hr_trunc =  ifelse(ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]` >= 200, 200, ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]`),
                                      flame_length_m = ex.2[[i]]$fireBehavior$`Flame Length [m]`, # could also save flamelength?
                                      PLT_CN = plt, 
                                      mort.scheme = plt.characteristics[i,]$mort.scheme, 
                                      scenario = plt.characteristics[i,]$scenario, 
                                      time = plt.characteristics[i,]$time, 
                                      fuelmodel = fuelmodel, 
                                      CBD = exampCrownFuel[i,"CBD"], 
                                      CFL = exampCrownFuel[i,"CFL"], 
                                      tph = plt.characteristics[i,]$tph,
                                      ht = plt.characteristics[i,]$ht,
                                      ba = plt.characteristics[i,]$ba,
                                      AGB.live = pipoSurfFuel[i,"loadLiveWoody"], 
                                      load1hr = pipoSurfFuel[i,"load1hr"], 
                                      load10hr = pipoSurfFuel[i,"load10hr"], 
                                      load100hr = pipoSurfFuel[i,"load100hr"])
        
      }
      #ex.2 = rothermel(exampSurfFuel, exampFuelMoisture, exampCrownFuel, exampEnviro)
      # ex.2$fireBehavior$`Torching Index [m/min]`
      # ex.2$fireBehavior$`Crowning Index [km/hr]`
      # 
      # 
      
      
      TI.CI.df <- do.call(rbind, TI.CI.list)
      TI.CI.df
    }
  }
}

a3 <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[1], fuelmodel = "TU5" )
a3
summary(a3$TI_km_hr_trunc)
summary(a3$TI)
#a3 %>% filter(scenario %in% "nocc.rcp26")
ggplot(a3, aes(time, TI_km_hr, color = scenario))+geom_point()+facet_wrap(~mort.scheme)

ggplot(a3, aes(AGB.live, TI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(load100hr, TI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)


ggplot(a3, aes(time, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(AGB.live, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(load100hr, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)

# get it for all the plotnos:
# this takes some time on my computer...
# i switched to a for loop to see where the lapply function broke down...we got a warning about a nonnumeric SI

TI.CI.list <- list()

# run for singleCC and fuel model TU5
for(i in 1:length(unique(plots)[1:43])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TU5")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TU5_5lagdead_43DIDDplots.rds")

# run for singleCC and TL5 or 9 

TI.CI.list <- list()

# run for singleCC and fuel model TL5
for(i in 1:length(unique(plots)[1:43])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TL5")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TL5_5lagdead_43DIDDplots.rds")

# run for singleCC and fuel model TL9
for(i in 1:length(unique(plots)[1:43])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TL9")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TL9_5lagdead_43DIDDplots.rds")


#TI.CI.FORECASTS <- readRDS( "TI.CI.snapshots_with_estimated_dead_litter_fixed_ba_dynamicwoody.rds")

TL5.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TL5_5lagdead_43DIDDplots.rds")
TL9.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TL9_5lagdead_43DIDDplots.rds")
TU5.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TU5_5lagdead_43DIDDplots.rds")

TI.CI.FORECASTS <- rbind(TL5.TI.CI, TL9.TI.CI, TU5.TI.CI)


# define high low hazards--using how its defined here: https://www.fs.usda.gov/rm/pubs/rmrs_p029/rmrs_p029_039_048.pdf
# High-hazard forest conditions were defined as
# having a crowning index less than 25 mph, moderate hazard from 25 to 50
# mph, and low hazard greater than 50 mph.
high.crown.haz <- 25
low.crown.haz <- 50

TI.CI.FORECASTS$crown_hazard <- ifelse(TI.CI.FORECASTS$CI_km_hr_trunc <= high.crown.haz, "high fire hazard",
                                       ifelse(TI.CI.FORECASTS$CI_km_hr_trunc > high.crown.haz & TI.CI.FORECASTS$CI_km_hr_trunc <= low.crown.haz, "moderate fire hazard", "low fire hazard"))

crown_hazard.summary <- TI.CI.FORECASTS %>% group_by( scenario, mort.scheme, fuelmodel,time) %>% mutate(total.plts = n()) %>% ungroup() %>%
  group_by(crown_hazard, scenario, mort.scheme, fuelmodel, time) %>% summarise(nplots = n(), 
                                                                               pct.plts = (nplots/total.plts)*100)

crown_hazard.summary$crown_hazard <- factor(crown_hazard.summary$crown_hazard, levels = c("low fire hazard", "moderate fire hazard", "high fire hazard"))

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_pct_pltsFIAperiodic_singleCC_5lagdead.png")
ggplot()+geom_bar(data = na.omit(crown_hazard.summary), aes(x = time, y = pct.plts, fill = crown_hazard), position="fill", stat="identity")+
  facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+scale_fill_manual(values = c("low fire hazard" = "#2c7bb6", "moderate fire hazard" = "#fdae61" , "high fire hazard" = "#d7191c"), name = "crown hazard")+
  theme_bw(base_size = 12) + ylab("% of plots in each hazard category")
dev.off()

TI.CI.FORECASTS$torch_hazard <- ifelse(TI.CI.FORECASTS$TI_km_hr_trunc <= high.crown.haz, "high fire hazard",
                                       ifelse(TI.CI.FORECASTS$TI_km_hr_trunc > high.crown.haz & TI.CI.FORECASTS$TI_km_hr_trunc <= low.crown.haz, "moderate fire hazard", "low fire hazard"))


ggplot(TI.CI.FORECASTS, aes(x = time, y = TI_km_hr_trunc, color = PLT_CN))+geom_line()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")

ggplot(TI.CI.FORECASTS, aes(x = time, y = CI, color = PLT_CN))+geom_line()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")

# note that TI is in m/min and CI is in km/hr
ggplot(TI.CI.FORECASTS, aes(x = TI_km_hr_trunc))+geom_histogram()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")


torch_hazard.summary <- TI.CI.FORECASTS %>% group_by( scenario, mort.scheme, fuelmodel, time) %>% mutate(total.plts = n()) %>% ungroup() %>%
  group_by(torch_hazard, scenario, mort.scheme, fuelmodel, time) %>% summarise(nplots = n(), 
                                                                               pct.plts = (nplots/total.plts)*100)

torch_hazard.summary$torch_hazard <- factor(torch_hazard.summary$torch_hazard, levels = c("low fire hazard", "moderate fire hazard", "high fire hazard"))

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_TI_periodicByFuelModel_5lagdead.png")
ggplot(TI.CI.FORECASTS, aes(x = TI_km_hr_trunc))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_CI_periodicByFuelModel_5lagdead.png")
ggplot(TI.CI.FORECASTS, aes(x = CI_km_hr_trunc))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_flame_length_periodicByFuelModel_5lagdead.png")
ggplot(TI.CI.FORECASTS, aes(x = flame_length_m))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")#+xlim(0, 3)#+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()


png(height = 6, width = 8, units = "in", res = 250, "outputs/torch_fire_hazard_pct_plts_periodicByFuelModel_5lagdead.png")
ggplot()+geom_bar(data = na.omit(torch_hazard.summary), aes(x = time, y = pct.plts, fill = torch_hazard), position="fill", stat="identity")+
  facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+scale_fill_manual(values = c("low fire hazard" = "#2c7bb6", "moderate fire hazard" = "#fdae61" , "high fire hazard" = "#d7191c"), name = "torch hazard")+
  theme_bw(base_size = 12) + ylab("% of plots in each hazard category")
dev.off()

ggplot(TI.CI.FORECASTS, aes(x = time, y = CI_km_hr_trunc, group = fuelmodel, fill = fuelmodel))+geom_boxplot()+facet_wrap(~mort.scheme, ncol = 4)+
  theme(legend.position = "none")+ylim(0,100)

ggplot(TI.CI.FORECASTS, aes(x = time, y = (TI/1000)*60, group = scenario, fill = scenario))+geom_boxplot()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")+ylim(0,100000)


TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr



TI.CI.summary <- TI.CI.FORECASTS %>% group_by(mort.scheme, scenario, fuelmodel, PLT_CN) %>% summarise(TI.mean = mean(TI, na.rm = TRUE), 
                                                                                                      TI.97.5 =quantile(TI, 0.975, na.rm = TRUE), 
                                                                                                      TI.02.5 = quantile(TI, 0.025, na.rm = TRUE), 
                                                                                                      
                                                                                                      CI.mean = mean(CI, na.rm = TRUE), 
                                                                                                      CI.97.5 =quantile(CI, 0.975, na.rm = TRUE), 
                                                                                                      CI.02.5 = quantile(CI, 0.025, na.rm = TRUE))
# CI gradually increases but TI and flamelength decrease even though load100hr increases
ggplot()+geom_point(data = TI.CI.FORECASTS, aes(x = time, y = load100hr))
ggplot()+geom_point(data = TI.CI.FORECASTS, aes(x = time, y = TI_km_hr))
ggplot()+geom_point(data = TI.CI.FORECASTS, aes(x = time, y = CI))
ggplot()+geom_point(data = TI.CI.FORECASTS, aes(x = time, y = flame_length_m))

#------------------------------------------------------------------------------------
# For DoubleCC:  get torching and crowning indices from the diameter distributions
#------------------------------------------------------------------------------------
# get the aboveground woody biomass component:
all10plot.nocc <- readRDS("all.AGB.fiaperiodic_doubleCC_12.30.22tempfile.RDS")
all.woody.agb.26 <- all10plot.nocc %>% group_by(plot, mort.scheme, rcp) %>% arrange(plot, mort.scheme, rcp, year) %>% 
  #group_by(plot, mort.scheme, rcp, year) %>% 
  #mutate(lag.value = lag(mAGB.dead)) #%>% 
  mutate(dead.lagged = dplyr::lag(mAGB.dead, n = 5,  order_by=year))%>% group_by(plot, mort.scheme, rcp, year)%>% #select(mAGB.stemwood, mAGB.stembark, mAGB.branchdead, mAGB.branchlive) %>%
  summarise(woody.biomass = mAGB.stemwood + mAGB.stembark + mAGB.branchlive, 
            dead = mAGB.dead,
            dead.lag = dead.lagged)



time.df <- data.frame(year = unique(all.woody.agb.26$year),
                      time = 1:length(unique(all.woody.agb.26$year)))

colnames(all.woody.agb.26)[3] <- "scenario"

all.woody.agb.26 <- left_join(all.woody.agb.26, time.df)

unique(all.woody.agb.26$scenario)
rm(all10plot.nocc)

# to estimate ht from DIameters, generate a simple relationship:
pipo.tree <- TREE %>% filter(SPCD %in% "122")

glm.ht.dia <- lm(data = pipo.tree, HT ~ DIA)
summary(glm.ht.dia)

glm.ht.dia$coefficients

rm(fiadb)
# get the diameter distributions of each plot
forecast.plt  <- readRDS("outputs/allplots.treeDiam.forecast.periodicFIA_doubleCC.RDS")
#forecast.plt <- allplots.treeDIAM
rm(allplots.treeDIAM)
rm(SUBPLOT)
rm(TREE_remeas)
rm(NPP.line)
rm(allplots.treeDIAMsubset)
#forecast.plt <- allplots.treeDIAMsubset #%>% filter(plot %in% plt)

forecast.plt$TPH <- forecast.plt$TPA*(1/0.404686)
forecast.plt$BA <- ((pi*(forecast.plt$DBH/2)^2)*forecast.plt$TPH)*(1/10000) # convert from sq cm to sq m and multiply by # trees/ha # tree BAshould be in m2/ha
forecast.plt$HT <- (forecast.plt$DBH/2.54)*glm.ht.dia$coefficients[2] + glm.ht.dia$coefficients[1]
forecast.plt$HT_m <- forecast.plt$HT*0.304
forecast.plt$HT_m <- ifelse(forecast.plt$HT_m > 62, 62, forecast.plt$HT_m)

# select some years to assess fire risk in:
# lets say year 1 (2002), 24 (2025), 49 2050, 74, 97 2098
plt.characteristics.all <- forecast.plt %>% filter(df %in% "live")%>% #filter(time %in% c(1, 24, 49,74,97))%>% 
  group_by (plot, mort.scheme, scenario, time) %>% summarise(ba = sum(BA, na.rm =TRUE), 
                                                             ht = mean(HT_m, na.rm = TRUE), 
                                                             tph = sum(TPH, na.rm = TRUE))
plt.characteristics.all$ba <- ifelse(plt.characteristics.all$ba >= quantile(plt.characteristics.all$ba, 0.975),quantile(plt.characteristics.all$ba, 0.975), plt.characteristics.all$ba)

all.woody.agb.26 <- all.woody.agb.26 #%>%  filter(time %in% c(1, 24, 49,74,97))
all.woody.agb.26$woody.biomass <- ifelse(all.woody.agb.26$woody.biomass >= quantile(all.woody.agb.26$woody.biomass, c(0.999)), quantile(all.woody.agb.26$woody.biomass, c(0.999)), all.woody.agb.26$woody.biomass)
all.woody.agb.26$dead <- ifelse(all.woody.agb.26$dead >= quantile(all.woody.agb.26$dead, c(0.999)), quantile(all.woody.agb.26$dead, c(0.999)), all.woody.agb.26$dead)


summary(all.woody.agb.26$woody.biomass/1000)
summary(all.woody.agb.26$dead*0.001)
summary(plt.characteristics.all$ba)

library(firebehavioR)
data(fuelModels, fuelMoisture)
exampSurfFuel = fuelModels['TU1',]

fuelMoisture['D1L1',]

exampFuelMoisture = fuelMoisture['D1L1',]

# naw.az <- terra::rast("nawfd_arizona.tif")
# plot(naw.az)
# rm(naw.az)
# fuelModels
# read in the summary stats from the MTRI fuels product:
# ideally down the road I will draw samples and propagate the uncertainty through

NAM.means <- read.csv("data/NAmWildlandFuelsDatabase_evt_groups_2022-12-30.csv")
pipo.fuels <- NAM.means %>% filter(evt_group_name %in% "Ponderosa Pine Forest| Woodland and Savanna" )

colnames(pipo.fuels)


plt <- "31367645010690"

get_torch_crown_indices_FORECASTS <- function(plt, fuelmodel){
  cat(paste0("getting pred vs obs for ", as.character(plt)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    # ex.plt <- TREE %>% filter(PLT_CN %in% plt)
    # ex.plt$HT_m <- ex.plt$HT*0.304
    # ex.plt$TPH <- ex.plt$TPA_UNADJ*(1/0.404686)
    # ex.plt$DBH_m <-ex.plt$DIA*2.54 
    # ex.plt$BA <- (pi*(ex.plt$DBH_m/2)^2)/10000 # should be in m2
    
    # forecast.plt <- allplots.treeDIAMsubset %>% filter(plot %in% plt)
    # 
    # forecast.plt$TPH <- forecast.plt$TPA*(1/0.404686)
    # forecast.plt$BA <- (pi*(forecast.plt$DBH/2)^2)/10000 # should be in m2
    
    plt.characters <- plt.characteristics.all %>% filter(plot %in% plt) %>% group_by(mort.scheme, scenario, time)
    
    plt.woody.agb <- all.woody.agb.26  %>% filter(plot %in% plt)%>% group_by(mort.scheme, scenario, time )
    
    plt.characteristics <- left_join(plt.characters, plt.woody.agb , by = c("plot", "mort.scheme","scenario","time"))
    
    plt.characteristics$dead.lag <- ifelse(is.na(plt.characteristics$dead.lag), 0, plt.characteristics$dead.lag)
    # calculate height from the FIA survey data
    # survey.characteristics <- ex.plt  %>% summarise(ba = sum(BA, na.rm =TRUE), 
    #                                     ht = mean(HT_m, na.rm = TRUE), 
    #                                     tph = sum(TPH, na.rm = TRUE))
    plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
    
    
    if(nrow(plt.characteristics) <=1){
      cat("no plot forecasts")
    }else{
      
      cat(paste("calculating crown fuels for plot", plt))
      for(i in 1:length(plt.characteristics$plot)){
        #cat(paste("year", i))
        plt.CrownFuel[i,] = as.matrix(canFuel(ba = plt.characteristics[i,]$ba, ht = plt.characteristics[i,]$ht, tph = plt.characteristics[i,]$tph, type = "pp")) # pp is ponderosa pine:
      }
      colnames(plt.CrownFuel) <- c("cfl", "cbd", "cbh")
      
      exampCrownFuel = data.frame(
        CBD = plt.CrownFuel[,"cbd"], # Canopy bulk density
        FMC = rep(80,length(plt.CrownFuel[,"cbd"])),  # foliar moisture content..assuming 100%?
        CBH = plt.CrownFuel[,"cbh"], # Canopy base height
        CFL = plt.CrownFuel[,"cfl"] # canopy fuel load
      )
      
      
      # Choose an example fuel model and fuel mositure scenario
      # need to do some research on what this is/means
      #all.woody.agb.26  %>% filter(plot %in% plt)
      repno <- length(plt.characteristics$woody.biomass)
      
      #exampSurfFuel = fuelModels['TU5',]
      exampSurfFuel = fuelModels[fuelmodel,]
      # could also just run with TL 5 or 9 (more fuel than TU model) and TU models--TL model should have lower TI indices
      
      pipoSurfFuel <- data.frame(fuelModelType = rep("S", repno),
                                 loadLitter = rep( pipo.fuels$fl_litter_mg_p_ha_q50, repno),
                                 load1hr = rep( exampSurfFuel$load1hr, repno), 
                                 load10hr =rep( exampSurfFuel$load10hr, repno), 
                                 
                                 # load1hr = ifelse(plt.characteristics$dead*0.001 ==0 |
                                 #                    is.na(plt.characteristics$dead), pipo.fuels$fl_1hr_mg_p_ha_q25, 
                                 #                  ((plt.characteristics$dead*0.001))*0.10),
                                 # load10hr = ifelse(plt.characteristics$dead*0.001 == 0|
                                 #                     is.na(plt.characteristics$dead),  pipo.fuels$fl_10hr_mg_p_ha_q25, 
                                 #                   ((plt.characteristics$dead*0.001))*0.30),
                                 load100hr = ifelse(plt.characteristics$dead.lag*0.001 == 0|
                                                      is.na(plt.characteristics$dead.lag),   exampSurfFuel$load100hr,
                                                    (plt.characteristics$dead.lag*0.001)), 
                                 # put all the dead in the 100hr fuels since most will be larger trees
                                 # with a ~5-10 year lag to mimic real dead trees 
                                 
                                 loadLiveHerb = rep( pipo.fuels$fl_herb_mg_p_ha_mean, repno), 
                                 loadLiveWoody = rep( pipo.fuels$fl_shrub_mg_p_ha_mean, repno), 
                                 #loadLiveWoody = ifelse(is.na(plt.characteristics$woody.biomass*0.001),0, (plt.characteristics$woody.biomass*0.001)), # convert kg/acre to Mg/ha
                                 savLitter = rep( exampSurfFuel$savLitter, repno),   
                                 sav1hr = rep( exampSurfFuel$sav1hr, repno),   
                                 sav10hr = rep( exampSurfFuel$sav10hr, repno),   
                                 sav100hr = rep( exampSurfFuel$sav100hr,  repno),  
                                 savLiveHerb = rep( exampSurfFuel$savLiveHerb,  repno),  
                                 savLiveWoody = rep( exampSurfFuel$savLiveWoody,  repno),  
                                 
                                 fuelBedDepth = rep( pipo.fuels$fl_duff_mg_p_ha_mean, repno), 
                                 mxDead = rep( exampSurfFuel$mxDead, repno), 
                                 heat = rep( exampSurfFuel$heat,  repno), 
                                 description = rep( exampSurfFuel$description, repno), 
                                 source = rep( exampSurfFuel$source, repno))
      
      
      #pipoSurfFuel$load1hr <- ifelse(pipoSurfFuel$load1hr > pipo.fuels$fl_1hr_mg_p_ha_max, pipo.fuels$fl_1hr_mg_p_ha_max, pipoSurfFuel$load1hr)
      #pipoSurfFuel$load10hr <- ifelse(pipoSurfFuel$load10hr > pipo.fuels$fl_10hr_mg_p_ha_max, pipo.fuels$fl_10hr_mg_p_ha_max, pipoSurfFuel$load10hr)
      #pipoSurfFuel$load100hr <- ifelse(pipoSurfFuel$load100hr > pipo.fuels$fl_100hr_mg_p_ha_max, pipo.fuels$fl_100hr_mg_p_ha_max, pipoSurfFuel$load100hr)
      #pipoSurfFuel$loadLiveWoody <- ifelse(pipoSurfFuel$loadLiveWoody > pipo.fuels$fl_tree_mg_p_ha_max, pipo.fuels$fl_tree_mg_p_ha_max, pipoSurfFuel$loadLiveWoody)
      
      
      # assume very dry dead fuel loads:
      #fuelMoisture['D1L1',]
      
      #exampFuelMoisture = fuelMoisture['D1L1',]
      # get slope from COND data
      #ex.plot.data <-PLOT %>% filter(CN %in% "2447353010690") 
      ex.COND.data <- COND %>% filter(PLT_CN %in% plt) 
      waffull <- vector()
      for(i in 1:length(plt.characteristics$ht)){
        waffull[i] <- waf(pipo.fuels$fl_duff_mg_p_ha_mean, plt.characteristics[i,]$ht, sheltered = "y")
      }
      
      if(nrow(ex.COND.data)==0){
        exampEnviro = data.frame(
          slope = rep(0, length(plt.characteristics$ht)),
          windspeed = rep(40, length(plt.characteristics$ht)), # windspeed (at 10m, open)
          direction = rep(0,length(plt.characteristics$ht)), # direction of wind, from uphill
          waf = waffull # Wind adjustment factor
        )
      }else{
        
        exampEnviro = data.frame(
          slope = rep(ex.COND.data$SLOPE, length(plt.characteristics$ht)),
          windspeed = rep(40, length(plt.characteristics$ht)), # windspeed (at 10m, open)
          direction = rep(0,length(plt.characteristics$ht)), # direction of wind, from uphill
          waf = waffull # Wind adjustment factor
        )
      }
      
      # now lets calculate the TI and CI
      #plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
      ex.2 <-TI.CI.list <- list()
      
      for(i in 1:length(plt.characteristics$plot)){
        if(exampCrownFuel[i,"CBD"] == 0 | exampCrownFuel[i,"CFL"] == 0){
          ex.2[[i]] <- ex.2[[i-1]]
          ex.2[[i]]$fireBehavior$`Torching Index [m/min]` <- 0
          ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]` <- 0
        }else{
          
          #surfFuel, moisture, crownFuel, enviro, rosMult = 1, cfbForm = "f", folMoist = "y"
          ex.2[[i]] = rothermel(surfFuel = pipoSurfFuel[i,], moisture = exampFuelMoisture, crownFuel= exampCrownFuel[i,], enviro = exampEnviro[i,], rosMult = 1, cfbForm = "f", folMoist = "y")
        }
        
        # add surface flame length to outputs
        TI.CI.list[[i]] <- data.frame(TI =  ex.2[[i]]$fireBehavior$`Torching Index [m/min]`, 
                                      CI =  ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]`, 
                                      TI_km_hr =  (ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60,
                                      TI_km_hr_trunc =  ifelse((ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60 >= 200, 200, (ex.2[[i]]$fireBehavior$`Torching Index [m/min]`*0.001)*60),
                                      CI_km_hr_trunc =  ifelse(ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]` >= 200, 200, ex.2[[i]]$fireBehavior$`Crowning Index [km/hr]`),
                                      flame_length_m = ex.2[[i]]$fireBehavior$`Flame Length [m]`, # could also save flamelength?
                                      PLT_CN = plt, 
                                      mort.scheme = plt.characteristics[i,]$mort.scheme, 
                                      scenario = plt.characteristics[i,]$scenario, 
                                      time = plt.characteristics[i,]$time, 
                                      fuelmodel = fuelmodel, 
                                      CBD = exampCrownFuel[i,"CBD"], 
                                      CFL = exampCrownFuel[i,"CFL"], 
                                      tph = plt.characteristics[i,]$tph,
                                      ht = plt.characteristics[i,]$ht,
                                      ba = plt.characteristics[i,]$ba,
                                      AGB.live = pipoSurfFuel[i,"loadLiveWoody"], 
                                      load1hr = pipoSurfFuel[i,"load1hr"], 
                                      load10hr = pipoSurfFuel[i,"load10hr"], 
                                      load100hr = pipoSurfFuel[i,"load100hr"])
        
      }
      #ex.2 = rothermel(exampSurfFuel, exampFuelMoisture, exampCrownFuel, exampEnviro)
      # ex.2$fireBehavior$`Torching Index [m/min]`
      # ex.2$fireBehavior$`Crowning Index [km/hr]`
      # 
      # 
      
      
      TI.CI.df <- do.call(rbind, TI.CI.list)
      TI.CI.df
    }
  }
}

a3 <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[1], fuelmodel = "TU5" )
a3
summary(a3$TI_km_hr_trunc)
summary(a3$TI)
#a3 %>% filter(scenario %in% "nocc.rcp26")
ggplot(a3, aes(time, TI_km_hr, color = scenario))+geom_point()+facet_wrap(~mort.scheme)

ggplot(a3, aes(AGB.live, TI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(load100hr, TI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)


ggplot(a3, aes(time, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(AGB.live, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)
ggplot(a3, aes(load100hr, CI_km_hr_trunc, color = scenario))+geom_point()+facet_wrap(~mort.scheme)

# get it for all the plotnos:
# this takes some time on my computer...
# i switched to a for loop to see where the lapply function broke down...we got a warning about a nonnumeric SI

TI.CI.list <- list()

# run for singleCC and fuel model TU5
for(i in 1:length(unique(plots)[1:417])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TU5")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TU5_5lagdead_doubleCC.rds")

# run for singleCC and TL5 or 9 

TI.CI.list <- list()

# run for singleCC and fuel model TL5
for(i in 1:length(unique(plots)[1:417])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TL5")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TL5_5lagdead_doubleCC.rds")

# run for singleCC and fuel model TL9
for(i in 1:length(unique(plots)[1:417])){
  TI.CI.list[[i]] <- get_torch_crown_indices_FORECASTS(plt = unique(plots)[i], fuelmodel = "TL9")
}

TI.CI.FORECASTS <- do.call(rbind, TI.CI.list)
#TI.CI.FORECASTS$TI_km_hr <- (TI.CI.FORECASTS$TI/1000)*60 # convert from m/min to km/hr

saveRDS(TI.CI.FORECASTS, "TI.CI.snapshots.FIAperiodic_TL9_5lagdead_doubleCC.rds")


#TI.CI.FORECASTS <- readRDS( "TI.CI.snapshots_with_estimated_dead_litter_fixed_ba_dynamicwoody.rds")

TL5.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TL5_5lagdead_doubleCC.rds")
TL9.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TL9_5lagdead_doubleCC.rds")
TU5.TI.CI <- readRDS("TI.CI.snapshots.FIAperiodic_TU5_5lagdead_doubleCC.rds")

TI.CI.FORECASTS <- rbind(TL5.TI.CI, TL9.TI.CI, TU5.TI.CI)


# define high low hazards--using how its defined here: https://www.fs.usda.gov/rm/pubs/rmrs_p029/rmrs_p029_039_048.pdf
# High-hazard forest conditions were defined as
# having a crowning index less than 25 mph, moderate hazard from 25 to 50
# mph, and low hazard greater than 50 mph.
high.crown.haz <- 25
low.crown.haz <- 50

TI.CI.FORECASTS$crown_hazard <- ifelse(TI.CI.FORECASTS$CI_km_hr_trunc <= high.crown.haz, "high fire hazard",
                                       ifelse(TI.CI.FORECASTS$CI_km_hr_trunc > high.crown.haz & TI.CI.FORECASTS$CI_km_hr_trunc <= low.crown.haz, "moderate fire hazard", "low fire hazard"))

crown_hazard.summary <- TI.CI.FORECASTS %>% group_by( scenario, mort.scheme, fuelmodel,time) %>% mutate(total.plts = n()) %>% ungroup() %>%
  group_by(crown_hazard, scenario, mort.scheme, fuelmodel, time) %>% summarise(nplots = n(), 
                                                                               pct.plts = (nplots/total.plts)*100)

crown_hazard.summary$crown_hazard <- factor(crown_hazard.summary$crown_hazard, levels = c("low fire hazard", "moderate fire hazard", "high fire hazard"))

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_pct_pltsFIAperiodic_singleCC_5lagdead_doubleCC.png")
ggplot()+geom_bar(data = na.omit(crown_hazard.summary), aes(x = time, y = pct.plts, fill = crown_hazard), position="fill", stat="identity")+
  facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+scale_fill_manual(values = c("low fire hazard" = "#2c7bb6", "moderate fire hazard" = "#fdae61" , "high fire hazard" = "#d7191c"), name = "crown hazard")+
  theme_bw(base_size = 12) + ylab("% of plots in each hazard category")
dev.off()

TI.CI.FORECASTS$torch_hazard <- ifelse(TI.CI.FORECASTS$TI_km_hr_trunc <= high.crown.haz, "high fire hazard",
                                       ifelse(TI.CI.FORECASTS$TI_km_hr_trunc > high.crown.haz & TI.CI.FORECASTS$TI_km_hr_trunc <= low.crown.haz, "moderate fire hazard", "low fire hazard"))


ggplot(TI.CI.FORECASTS, aes(x = time, y = TI_km_hr_trunc, color = PLT_CN))+geom_line()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")

ggplot(TI.CI.FORECASTS, aes(x = time, y = CI, color = PLT_CN))+geom_line()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")

# note that TI is in m/min and CI is in km/hr
ggplot(TI.CI.FORECASTS, aes(x = TI_km_hr_trunc))+geom_histogram()+facet_wrap(~mort.scheme)+
  theme(legend.position = "none")


torch_hazard.summary <- TI.CI.FORECASTS %>% group_by( scenario, mort.scheme, fuelmodel, time) %>% mutate(total.plts = n()) %>% ungroup() %>%
  group_by(torch_hazard, scenario, mort.scheme, fuelmodel, time) %>% summarise(nplots = n(), 
                                                                               pct.plts = (nplots/total.plts)*100)

torch_hazard.summary$torch_hazard <- factor(torch_hazard.summary$torch_hazard, levels = c("low fire hazard", "moderate fire hazard", "high fire hazard"))

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_TI_periodicByFuelModel_5lagdead_doubleCC.png")
ggplot(TI.CI.FORECASTS, aes(x = TI_km_hr_trunc))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_CI_periodicByFuelModel_5lagdead_doubleCC.png")
ggplot(TI.CI.FORECASTS, aes(x = CI_km_hr_trunc))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()

png(height = 6, width = 8, units = "in", res = 250, "outputs/crown_fire_hazard_hists_flame_length_periodicByFuelModel_5lagdead_doubleCC.png")
ggplot(TI.CI.FORECASTS, aes(x = flame_length_m))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+
  theme(legend.position = "none")#+xlim(0, 3)#+geom_vline(aes(xintercept = c(high.crown.haz)))+geom_vline(aes(xintercept = c(low.crown.haz)))
dev.off()


png(height = 6, width = 8, units = "in", res = 250, "outputs/torch_fire_hazard_pct_plts_periodicByFuelModel_5lagdead_doubleCC.png")
ggplot()+geom_bar(data = na.omit(torch_hazard.summary), aes(x = time, y = pct.plts, fill = torch_hazard), position="fill", stat="identity")+
  facet_grid(rows = vars(mort.scheme), cols = vars(fuelmodel))+scale_fill_manual(values = c("low fire hazard" = "#2c7bb6", "moderate fire hazard" = "#fdae61" , "high fire hazard" = "#d7191c"), name = "torch hazard")+
  theme_bw(base_size = 12) + ylab("% of plots in each hazard category")
dev.off()

