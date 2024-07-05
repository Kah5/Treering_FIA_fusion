library(ggplot2)
library(reshape2)
library(tidyverse)
library(rgdal)
library(firebehavioR)

# read in data needed to run post processing
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
# reading in the available forecasts (1-184 I believe)
# only gets it for the full scenario
get_biomass_ests <- function(plot, mort.scheme, scenario, SDI.ratio.DD, cc.scenario, scale.mort.prob = 1 ){
  
  cat(paste0("getting pred vs obs for ", as.character(plot)))
  
  if(!file.exists(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))==TRUE){
    cat("no forecast")
  }else{
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{

    cat (paste("reading in forecasts from plot ", plot))
    load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
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
    
    # plot(yrvec, mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
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
    
    plot(yrvec[2:length(low.stemwood)], mAGB[i, 2:length(low.stemwood)], ylim = range(c(upA, lowA)), ylab = "Mg/ha", xlab = "year")
     lines(yrvec[2:length(low.stemwood)], upA[2:length(low.stemwood)])
     lines(yrvec[2:length(low.stemwood)], lowA[2:length(low.stemwood)])
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

get_biomass_ests(plot = 2447353010690, mort.scheme = "DIDD", scenario = "rcp26", SDI.ratio.DD= 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)


# -------------------------------------------------------------------------------
# read in forecasts for all scenarios and mortality conditions, 80%SDI threshold
# -------------------------------------------------------------------------------

# do this for the 60% max SDI threshold scenario
DIDD.AGB.60thresh <- lapply(unique(plots)[1:675], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
DIDD.AGB.45.60thresh <- lapply(unique(plots)[1:675], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45", SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
DIDD.AGB.60.60thresh <- lapply(unique(plots)[1:675], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60", SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
DIDD.AGB.85.60thresh <- lapply(unique(plots)[1:675], FUN = function(x) {get_biomass_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85", SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})



DIDD.AGB.60thresh.df <- do.call(rbind, DIDD.AGB.60thresh)
DIDD.AGB.45.60thresh.df <- do.call(rbind, DIDD.AGB.45.60thresh)
DIDD.AGB.60.60thresh.df <- do.call(rbind, DIDD.AGB.60.60thresh)
DIDD.AGB.85.60thresh.df <- do.call(rbind, DIDD.AGB.85.60thresh)
all10plot <- all10plots <- DIDD.AGB.60thresh.df


# -------------------------------------------------------------------------------
# combine all forecasts
# -------------------------------------------------------------------------------
all10plots <- rbind(DIDD.AGB.60thresh.df, 
                    DIDD.AGB.45.60thresh.df, 
                    DIDD.AGB.60.60thresh.df, 
                    DIDD.AGB.85.60thresh.df) #, 

saveRDS(all10plots, "all.AGB.fiaperiodic_singleCC_60thresh_full_1_scale_mort.RDS")
all10plots <- readRDS("all.AGB.fiaperiodic_singleCC_60thresh_full_1_scale_mort.RDS")


all10plots <- DIDD.AGB.60thresh.df
ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
  summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)
ten.plot.medians <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
  summarise_at(.vars = vars(mAGB:low.foliage), .funs = median, na.rm = TRUE)

# -------------------------------------------------------------------------------
#  make plots of total biomass across the region
# -------------------------------------------------------------------------------
# high AGB plots
highAGBplots <- all10plots %>% filter(mAGB*0.001 > 150 & year == 2040) %>% select(plot) %>% distinct()
highAGBplots
AGB.lines <- ggplot()+geom_line(data = all10plots, aes(year, mAGB*0.001, group = plot), alpha = 0.5)+theme_bw()+facet_wrap(~rcp, ncol = 5)#+
  #geom_ribbon(data = ten.plot.summary, aes(year, ymin = lowA, ymax = upA, group = mort.scheme, fill = mort.scheme))+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
AGB.lines
ggsave(height = 4, width = 6, units = "in", "outputs/allplots_AGB_lines_log_MSBfn.png")


AGB.lines <- ggplot()+geom_line(data = all10plots %>% filter(year < 2050 ), aes(year, mAGB*0.001, group = plot), alpha = 0.5)+theme_bw()+facet_wrap(~rcp, ncol = 5)+
  ylab("Mean plot Aboveground Biomass (Mg/hectare)")+xlab("Year")
#geom_ribbon(data = ten.plot.summary, aes(year, ymin = lowA, ymax = upA, group = mort.scheme, fill = mort.scheme))+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
AGB.lines
ggsave(height = 4, width = 6, units = "in", "outputs/allplots_AGB_lines_log_MSBfn_2000_2050.png")

AGB.lines <- ggplot()+geom_line(data = all10plots %>% filter(year < 2025), aes(year, mAGB*0.001, group = plot), alpha = 0.5)+theme_bw()+facet_wrap(~rcp, ncol = 5)#+
#geom_ribbon(data = ten.plot.summary, aes(year, ymin = lowA, ymax = upA, group = mort.scheme, fill = mort.scheme))+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
AGB.lines
ggsave(height = 4, width = 6, units = "in", "outputs/allplots_AGB_lines_log_MSBfn_2000_2050.png")


# summarise
all10plots
ggplot()+geom_line(data = all10plots, aes(year, mAGB*0.001, group = plot), alpha = 0.5)+
theme_bw()+facet_wrap(~rcp, ncol = 5)#+
#geom_ribbon(data = ten.plot.summary, aes(year, ymin = lowA, ymax = upA, group = mort.scheme, fill = mort.scheme))+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)



all.10.plots.diff <- all10plots %>% filter(year == 2098 | year == 2002 | year == 2050) %>% group_by(plot, rcp, mort.scheme) %>% 
  select(plot, mort.scheme, rcp, year, mAGB) %>% spread(year, mAGB) %>% mutate(diff = `2098`-`2002`,
                                                                               diff.50 = `2050` - `2002`)
summary(all.10.plots.diff$diff)
hist(all.10.plots.diff$diff*0.001)
hist(all.10.plots.diff$diff.50*0.001)
summary(all.10.plots.diff$diff.50*0.001)
ggplot()+geom_histogram(data = all.10.plots.diff, aes(x = diff*0.001))+
  geom_vline(aes(xintercept = median(all.10.plots.diff$diff*0.001)), color = "red", linetype = "dashed")+xlab("AGB (Mg) difference \n 2098-2002")
ggsave(height = 4, width = 6, units = "in", "outputs/allplots_AGBdiff_2002_2098.png")

ggplot()+geom_histogram(data = all.10.plots.diff, aes(x = diff.50*0.001))+
  geom_vline(aes(xintercept = median(all.10.plots.diff$diff.50*0.001)), color = "red", linetype = "dashed")+xlab("AGB (Mg) difference \n 2098-2002")
ggsave(height = 4, width = 6, units = "in", "outputs/allplots_AGBdiff_2002_2050.png")

AGB.line <- ggplot()+geom_line(data = ten.plot.summary, aes(year, mAGB*0.001, group = mort.scheme), color = "forestgreen")+theme_bw()+
  geom_ribbon(data = ten.plot.summary, aes(year, ymin = lowA*0.001, ymax = upA*0.001, group = mort.scheme), alpha = 0.5, fill = "forestgreen")+ylab("Total AGB for all plots \n (Mg/hectare), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
NPP.line <- ggplot(ten.plot.summary, aes(year, mNPP, group = mort.scheme, color = mort.scheme))+geom_line()+theme_bw()+ylab("Total NPP for all  plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(-6e5,2e5)
AGB.line
NPP.line
png(height = 7, width = 10, units = "in", res = 150, "outputs/allplotsFIAperiodic.total.biomass.singleCC_1.full.png")
cowplot::plot_grid(AGB.line, NPP.line, ncol = 1, align = "hv")
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "outputs/allplotsFIAperiodic.total.biomass.singleCC_1.full_AGB.png")
AGB.line
dev.off()

# plot the individual plots with the means
AGB.lines <- ggplot()+geom_line(data = all10plots %>% filter( ! plot %in% highAGBplots$plot), aes(year, mAGB*0.001, group = plot), alpha = 0.5)+theme_bw()+facet_wrap(~rcp, ncol = 5)+
  geom_line(data = ten.plot.medians, aes(year, y = mAGB*0.001, group = mort.scheme), color = "forestgreen", size = 2)+ylab("Total AGB for all plots \n (kg/acre), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
AGB.lines
ggsave(height = 4, width = 6, units = "in", "outputs/Average_plot_AGB_rcp2.6_with_median.png")

AGB.lines <- ggplot()+geom_line(data = all10plots %>% filter(year <= 2052 &  ! plot %in% highAGBplots$plot), aes(year, mAGB*0.001, group = plot), alpha = 0.25, color = "black")+theme_bw()+facet_wrap(~rcp, ncol = 5)+
  geom_line(data = ten.plot.medians %>% filter(year <= 2052), aes(year, y = mAGB*0.001, group = mort.scheme), color = "forestgreen", size = 2)+ylab("Total per plot live AGB \n (Mg/hectoare), RCP 2.6")+facet_wrap(~rcp, ncol = 5)#+ylim(0,1.7e7)
AGB.lines
ggsave(height = 4, width = 6, units = "in", "outputs/Average_plot_AGB_rcp2.6_with_median_2002_2050.png")


# plot out the differences:
PLOT.ll <- PLOT %>% select(PLT_CN, LAT, LON) %>% distinct() %>% rename(`plot` = "PLT_CN")
PLOT.ll$plot <- as.character(PLOT.ll$plot)
all.10.plots.diff$plot <- as.character(all.10.plots.diff$plot)
all.diff.ll <- left_join( all.10.plots.diff , PLOT.ll)
all.diff.ll <- all.diff.ll %>% mutate(`change in live AGB` = diff*0.001, 
                                      `change in live AGB 2001-2050`= diff.50*0.001)
library(mapdata)
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona", "utah", "new mexico", "colorado","idaho", "wyoming", "montana", "nevada", 
                                            "california", "oregon", "washington", "texas", "kansas", 
                                            "nebraska", "north dakota", "south dakota", "oklahoma") )
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

change.map <- ggplot()+
  geom_polygon(data = states, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = mexico, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = canada, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white")+
  geom_point(data = all.diff.ll, aes(x = LON, y = LAT, color = `change in live AGB`))+
  theme_bw()+
  coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+
  scale_color_viridis_c(option = "B")
png(height = 7, width = 10, units = "in", res = 150, "outputs/allplotsChange_2098.png")
change.map
dev.off()


change.map.2050 <- ggplot()+
  geom_polygon(data = states, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = mexico, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = canada, 
               aes(x=long, y=lat, group = group), 
               color = "black", fill = "white")+
  geom_point(data = all.diff.ll, aes(x = LON, y = LAT, color = `change in live AGB 2001-2050`))+
  theme_bw()+
  coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+
  scale_color_viridis_b(option = "B")
png(height = 7, width = 12, units = "in", res = 150, "outputs/allplotsChange_2050.png")
change.map.2050
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
# btst.AGB.DIDD.26 <- lapply(unique(plots)[1:675], FUN = function(x){get_tree_levelC_ests_FVS(plot = x, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.8, cc.scenario = "singleCC")})
# btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
# btst.AGB.DIDD.26.df


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
# allplots.treeC <- rbind(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df)#, 
# allplots.treeC <- btst.AGB.DIDD.26.df
# #btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
# #btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
# #btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df) #, 
# #btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)
# saveRDS(allplots.treeC, "outputs/allplots.FIAperiodic.treeC1.20.23.RDS")


# -------------------------------------------------------------------------------
#  make plots of big tree vs small tree mortality
# -------------------------------------------------------------------------------
plot <- 2487922010690
get_tree_diam_live_dead_ests <- function(plot, mort.scheme, scenario, SDI.ratio.DD = 0.8,nocc = FALSE, cc.scenario, scale.mort.prob = 1){
  cat(paste0("reading in plot "), plot)
  
  if(!file.exists(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))==TRUE){
    cat("no forecast")
  }else{
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    
    # 
    cat (paste("reading in forecasts from plot ", plot))
    
    
    #if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    # }else{
    #   load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot,".",scenario,".", SDI.ratio.DD, ".",  cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
    #   
    # }#load("biomass_data_FVSmort/plot2AGB_DIDD.2449653010690.rcp26.Rdata")
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
    
    
    
    
    # get dead diameters not sure we need to add this!
    # diam.dead.melt <- melt(diam.live[2,,])
    # diam.dead.melt$size_class <- ifelse(diam.dead.melt$value<= 0.1, NA, 
    #                                     ifelse(diam.dead.melt$value <= 30, "small tree", "big tree"))
    # colnames(diam.dead.melt) <- c("tree", "time", "DBH", "size_class")
    # 
    # summary(diam.dead.melt)
    
  #  diam.dead.melt$status <- ifelse(diam.dead.melt$DBH == 0, "live", "dead")
   # diam.live.melt$status <- ifelse(diam.live.melt$DBH == 0.1, "dead", "live")
    colnames(tpa.dead.melt) <- c("tree", "time", "TPAdead")
    # use TPA now to characterize dead vs live
    #diam.dead.melt <- left_join(diam.dead.melt, tpa.dead.melt, by = c("tree", "time"))
    diam.live.melt <- left_join(diam.live.melt, tpa.live.melt, by = c("tree", "time"))
    diam.live.melt <- left_join(diam.live.melt, tpa.dead.melt, by = c("tree", "time"))
    
    #diam.dead.melt$status <- ifelse(diam.dead.melt$TPA == 0, "live", "all dead")
    diam.live.melt$status <- ifelse(diam.live.melt$TPA == 0, "all dead", "live")
    #unique(diam.dead.melt$status)
    ggplot(diam.live.melt, aes(time, DBH, color = TPA == 0))+geom_point()+facet_wrap("status")
    ggplot(diam.live.melt, aes(TPAdead, TPA, color = TPA == 0))+geom_point()+facet_wrap("status")
    #combine into one big DF
    #diam.dead.melt$df <- "dead"
    diam.live.melt$df <- "live"
    #diams <- rbind(diam.live.melt, diam.dead.melt)
    diams <- diam.live.melt
    diams$plot <- plot
    diams$mort.scheme <- mort.scheme
    if(nocc == FALSE){
      diams$scenario <- scenario
    }else{
      diams$scenario <- paste0("nocc", ".", scenario)
    }
   ggplot(diams, aes(x = time, y = DBH, group = tree))+geom_line()
   
    diams
  }
  }
}
get_tree_diam_live_dead_ests(plot = 3635611010690, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)
plot <- 2511127010690
# -------------------------------------------------------------------------------
# for single CC
# -------------------------------------------------------------------------------
unique(plots) %in% 2511127010690
# read in and get the tree level estimates
# RCP 2.6
plot <- unique(plots)[15]
btst.DIAMS.DIDD.26 <- lapply(unique(plots)[1:675], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26",SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
btst.DIAMS.DIDD.26.df <- do.call(rbind, btst.DIAMS.DIDD.26)
saveRDS(btst.DIAMS.DIDD.26.df, "btst.DIAMS.DIDD.26.df.tempfile.RDS")
rm(btst.DIAMS.DIDD.26)
btst.DIAMS.DIDD.26.df <- readRDS( "btst.DIAMS.DIDD.26.df.tempfile.RDS")
live.diams <- btst.DIAMS.DIDD.26.df %>% filter(status %in% "live")
large.dia.plots <- live.diams %>% filter(DBH > 90) %>% select(plot) %>% distinct()
saveRDS(large.dia.plots, "outputs/large.dia.plots.rds")

# validation with live trees in PIPO record
matched.cores <- read.csv("data/PIPOCore_TentativeMatch1.csv")
matched.cores <- matched.cores %>% mutate(DIA_T2 = DIA+INCR, 
                                          DIA_cm_T2 = (DIA+ INCR)*2.54)
matched.cores$CORE_CN <- as.character(matched.cores$CORE_CN)
matching.dia <- left_join(matched.cores, cov.data.regional)
all.diams <- left_join(btst.DIAMS.DIDD.26.df , year.time.match)

p1 <- matching.dia$PLT_CN[1]

year.time.match <- data.frame(time = 1:98, 
                              year = 2002:2099)
cored.trees <- matching.dia %>% filter(PLT_CN %in% p1)
all.diams %>% filter(plot %in% p1 & tree %in% 1:nrow(cored.trees))

cov.data.regional %>% filter(TRE_CN %in% matched.cores$TRE_CN[1])

# calculate mean diameter and QMD for each plot and time
live.diams.plt <- live.diams %>% group_by(time, plot, scenario) %>% 
  summarise(TPA.tot = sum(TPA),
            TPA.tot.dead = sum(TPAdead),
            DBH.tot = sum(DBH, na.rm =TRUE),
            DBH.mean = mean(DBH, na.rm = TRUE), 
            DBH.max = max(DBH, na.rm =TRUE)) %>%
  mutate(QMD.mean = (sqrt(DBH.tot/2.54)^2/TPA.tot))
unique(unique(live.diams.plt %>% filter(QMD.mean > 30) %>% select(plot))$plot)

ggplot(live.diams.plt %>% filter(time == 1), aes(x = TPA.tot, y = DBH.mean))+geom_point()
ggsave(height = 4, width = 6, "outputs/plot_summary_DBHmean_TPA_2002_MSBfix_3.png")
ggplot(live.diams.plt %>% filter(time == 1), aes(x = TPA.tot, y = DBH.max ))+geom_point()
ggsave(height = 4, width = 6, "outputs/plot_summary_DBHmax_TPA_2002_MSBfix_3.png")
ggplot(live.diams.plt %>% filter(time == 49), aes(x = TPA.tot, y = DBH.mean))+geom_point()
ggsave(height = 4, width = 6, "outputs/plot_summary_DBHmean_TPA_2052_MSBfix_3.png")

ggplot(live.diams.plt %>% filter(time < 50), aes(x = TPA.tot, y = QMD.mean))+geom_point()+
  facet_wrap(~time, ncol = 10, scales = "free")+theme(legend.position = "none")
ggsave(height = 5, width = 12, "outputs/plot_summary_QMDmean_TPA_2002_2051_MSBfix_3.png")

ggplot(live.diams.plt %>% filter(time == 49), aes(x = TPA.tot, y = QMD.mean))+geom_point()
ggsave(height = 4, width = 6, "outputs/plot_summary_QMDmean_TPA_2052_MSBfix_3.png")


ggplot(live.diams.plt %>% filter(time <=15), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_first15_MSBfix_3.png")
ggplot(live.diams.plt %>% filter(time <=30 & time >15 ), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_16-30years_MSBfix_3.png")

ggplot(live.diams.plt %>% filter(time <=45 & time >30 ), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_31-45years_MSBfix_3.png")


ggplot(live.diams.plt %>% filter(time <=60 & time >45 ), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_46-60years_MSBfix_3.png")

ggplot(live.diams.plt %>% filter(time <=75 & time >60 ), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_61-75years_MSBfix_3.png")


ggplot(live.diams.plt %>% filter(time <=90 & time >75 ), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_76-90years_MSBfix_3.png")


ggplot(live.diams.plt %>% filter(time >90), aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)+facet_wrap(~time, ncol = 5)
ggsave(height = 8, width = 15, "outputs/plot_summary_DBHmean_TPA_>90years_MSBfix_3.png")

PIPO.plt.summary <- TREE %>% filter(SPCD %in% "122")%>% group_by(PLT_CN) %>% summarise(DBH.mean = mean(DIA*2.54, na.rm =TRUE), 
                                                                   TPA.tot = sum(TPA_UNADJ, na.rm =TRUE))
ggplot(PIPO.plt.summary, aes(x = TPA.tot, y = DBH.mean))+geom_point(alpha = 0.75, shape = 21)
ggsave(height = 4, width = 4, "outputs/plot_summary_DBHmean_TPA_all_annual_MSBfix_3.png")

# these keep failing...may need to separately join these?
# RCP 4.5
btst.DIAMS.DIDD.45 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45",SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
btst.DIAMS.DIDD.45.df <- do.call(rbind, btst.DIAMS.DIDD.45)
saveRDS(btst.DIAMS.DIDD.45.df, "btst.DIAMS.DIDD.45.df.tempfile.RDS")
rm(btst.DIAMS.DIDD.45.df)



# RCP 6.0
btst.DIAMS.DIDD.60 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60",SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
btst.DIAMS.DIDD.60.df <- do.call(rbind, btst.DIAMS.DIDD.60)
saveRDS(btst.DIAMS.DIDD.60.df, "btst.DIAMS.DIDD.60.df.tempfile.RDS")
rm(btst.DIAMS.DIDD.60.df)

# RCP 8.5
btst.DIAMS.DIDD.85 <- lapply(unique(plots)[1:650], FUN = function(x){get_tree_diam_live_dead_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85",SDI.ratio.DD = 0.6, cc.scenario = "singleCC", scale.mort.prob = 1)})
btst.DIAMS.DIDD.85.df <- do.call(rbind, btst.DIAMS.DIDD.85)
saveRDS(btst.DIAMS.DIDD.85.df, "btst.DIAMS.DIDD.85.df.tempfile.RDS")
rm(btst.DIAMS.DIDD.85.df)

btst.DIAMS.DIDD.26.df <- readRDS("btst.DIAMS.DIDD.26.df.tempfile.RDS")
btst.DIAMS.DIDD.45.df <- readRDS("btst.DIAMS.DIDD.45.df.tempfile.RDS")
btst.DIAMS.DIDD.60.df <- readRDS("btst.DIAMS.DIDD.60.df.tempfile.RDS")
btst.DIAMS.DIDD.85.df <- readRDS("btst.DIAMS.DIDD.85.df.tempfile.RDS")

rcp26.DIAMS <- btst.DIAMS.DIDD.26.df
rcp26.DIAMS <-  rbind(btst.DIAMS.DIDD.26.df) #, 
                      # btst.DIAMS.DIDD.45.df, 
                      # btst.DIAMS.DIDD.60.df, 
                      #btst.DIAMS.DIDD.85.df)
saveRDS(rcp26.DIAMS, "periodicFIA.btst.DIAMS.allrcps_1.RDS")

# # checking to see if the mort trees are being labeled properly:
# ggplot(btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]), aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# 
# filtered.test <- btst.DIAMS.DIDD.85.df %>% filter(plot %in% unique(plotnos)[2]) %>% filter(status == df)
# ggplot(filtered.test, aes(x = time, y = DBH, group = tree, color = df))+geom_point()
# 
# # combine all the tree-level datasets together:
#allplots.treeDIAM  <- btst.DIAMS.DIDD.26.df
#allplots.treeDIAM <- rbind(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df )#,
#btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
#btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df,
#btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df) #,
#btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#nocc.allplots.treeeDIAM <- rbind(btst.DIAMS.nomort.nocc.df, btst.DIAMS.DIonly.nocc.df, btst.DIAMS.DDonly.nocc.df, btst.DIAMS.DIDD.nocc.df)
#saveRDS(nocc.allplots.treeeDIAM, "outputs/allplots.treeDiam.forecast.nocc.RDS")

saveRDS(rcp26.DIAMS , "outputs/allplots.treeDiam.forecast.periodicFIA.full_1.RDS")


rm(btst.DIAMS.nomort.26.df, btst.DIAMS.DIonly.26.df, btst.DIAMS.DDonly.26.df, btst.DIAMS.DIDD.26.df,
   btst.DIAMS.nomort.45.df, btst.DIAMS.DIonly.45.df, btst.DIAMS.DDonly.45.df, btst.DIAMS.DIDD.45.df,
   btst.DIAMS.nomort.60.df, btst.DIAMS.DIonly.60.df, btst.DIAMS.DDonly.60.df, btst.DIAMS.DIDD.60.df,
   btst.DIAMS.nomort.85.df, btst.DIAMS.DIonly.85.df, btst.DIAMS.DDonly.85.df, btst.DIAMS.DIDD.85.df) 
rm(btst.DIAMS.nomort.26 , btst.DIAMS.DIonly.26 , btst.DIAMS.DDonly.26 , btst.DIAMS.DIDD.26 ,
   btst.DIAMS.nomort.45 , btst.DIAMS.DIonly.45 , btst.DIAMS.DDonly.45 , btst.DIAMS.DIDD.45 ,
   btst.DIAMS.nomort.60 , btst.DIAMS.DIonly.60 , btst.DIAMS.DDonly.60 , btst.DIAMS.DIDD.60 ,
   btst.DIAMS.nomort.85 , btst.DIAMS.DIonly.85 , btst.DIAMS.DDonly.85 , btst.DIAMS.DIDD.85) 
#

allplots.treeDIAMsubset <- rcp26.DIAMS
# need to redo this somehow?
region.ndead <- allplots.treeDIAMsubset %>% group_by(mort.scheme, scenario, time) %>% summarise(ntree = sum(TPA), 
                                                                                                         ntree.dead = sum(TPAdead))
ggplot()+geom_point(data = region.ndead %>% filter(ntree > 0), aes(x = time, y = ntree), color = "forestgreen")+
  geom_point(data = region.ndead %>% filter(ntree.dead > 0 ), aes(x = time, y = ntree.dead), color = "brown")+
  facet_wrap(~mort.scheme)
ggsave("outputs/ntree_dead_live.png")
# summarize the dead tree mortality rate by plot, and compare to the results of FIA data analyses
# 
plot.ndead <- allplots.treeDIAMsubset %>% group_by(plot,  mort.scheme, scenario, time) %>% summarise(live = sum(TPA), 
                                                                                                     dead = sum(TPAdead))
plot.ndead.spread <- plot.ndead %>% ungroup()  %>% group_by(plot, mort.scheme, scenario, time) #%>% spread(df, value = ntree)
plot.prop.dead <- plot.ndead.spread %>% group_by(plot, mort.scheme, scenario, time) %>% mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)))

plot.prop.dead %>% filter(dead < 0) %>% select(plot) %>% distinct()

# crazy line plot
ggplot(plot.prop.dead, aes(x = time, y = prop.dead, group = plot))+geom_line()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")
ggsave("outputs/prop_dead_by_plot.png")
# proportion of dead trees in 2018 :
ggplot(plot.prop.dead %>% filter(time == 17), aes(x =  prop.dead))+geom_histogram()+facet_grid(rows = vars(mort.scheme), cols = vars(scenario), scales = "free_y")

# plot size distribution of living and dead trees in 2018
plot.mort.rate2001.2020 <- allplots.treeDIAMsubset %>% group_by(plot,  mort.scheme, scenario, time) %>% filter(time == 17)


# calculate a 10 year mortality rate, from 2001-2010 and from 2011-2020 to compare:
all.trees.2001.2010 <- allplots.treeDIAMsubset %>% group_by(plot,  mort.scheme, scenario, time) %>% filter(time %in% 10)%>% 
  summarise(live = sum(TPA), 
            dead = sum(TPAdead))%>% group_by(plot, mort.scheme, scenario, time) %>% 
  mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)),
         avg.dead.rate = prop.dead/10)# gets # dead for each plot in each year
png(height = 4, width = 6, units = "in", res = 200, "outputs/Plot_pct_mortality_forecasted_2001_2010.png")
ggplot()+geom_histogram(data = all.trees.2001.2010, aes(avg.dead.rate*100))+facet_wrap(~scenario)+ggtitle("2001-2010 forecasted plot mortality rates")+
  geom_vline(aes(xintercept = median(all.trees.2001.2010$avg.dead.rate*100)), color = "red", linetype = "dashed")
dev.off()

hist(all.trees.2001.2010$avg.dead.rate*100, main = "2001-2010 forecasted plot mortality rates", xlab = "Plot level average yearly mortality rates (%)")
summary(all.trees.2001.2010$avg.dead.rate*100)

saveRDS(all.trees.2001.2010, "all.trees.2001.2010.FIAperiodic_singleCC_1.full.rds")
all.trees.2001.2010 <- readRDS("all.trees.2001.2010.FIAperiodic_singleCC_1.full.rds")

all.trees.2011.2020 <- allplots.treeDIAMsubset %>% group_by(plot,  mort.scheme, scenario, time) %>% filter(time %in% 20)%>%
  summarise(live = sum(TPA), 
            dead = sum(TPAdead))%>% group_by(plot, mort.scheme, scenario, time) %>% 
  group_by(plot, mort.scheme, scenario, time) %>% 
  mutate(prop.dead = ifelse(is.na(dead), 1, dead/(live + dead)),
         avg.dead.rate = prop.dead/20)# gets # dead for each plot in each year

hist(all.trees.2011.2020$avg.dead.rate*100)
summary(all.trees.2011.2020$avg.dead.rate*100)

all.trees.2011.2020 %>% filter(avg.dead.rate <=0)

png(height = 4, width = 6, units = "in", res = 200, "outputs/Plot_pct_mortality_forecasted_2011_2020.png")
ggplot()+geom_histogram(data = all.trees.2011.2020, aes(avg.dead.rate*100))+facet_wrap(~scenario)+ggtitle("2011-2020 forecasted plot mortality rates")+
  geom_vline(aes(xintercept = median(all.trees.2011.2020$avg.dead.rate*100)), color = "red", linetype = "dashed")

#hist(all.trees.2011.2020$avg.dead.rate*100, main = "2011-2020 forecasted plot mortality rates", xlab = "Plot level average yearly mortality rates (%)")
dev.off()




median(all.trees.2001.2010$avg.dead.rate)*100
median(all.trees.2011.2020$avg.dead.rate)*100
mean(all.trees.2001.2010$avg.dead.rate)*100
mean(all.trees.2011.2020$avg.dead.rate)*100


saveRDS(all.trees.2011.2020, "all.trees.2011.2020.FIAperiodic_singleCC_1.full.rds")
# calculate mortality rate for each plot, but wait to join with another variable
# for the first time period mortality rate is just the proportion at time 10
plot.mort.rate.2001.2010 <- all.trees.2001.2010 %>% group_by(plot, mort.scheme, scenario) %>% filter(time == 10)%>% mutate(mortality.rate = prop.dead/10) %>% dplyr::select(-time)
hist(plot.mort.rate.2001.2010$mortality.rate*100)
mean(plot.mort.rate.2001.2010$mortality.rate)
summary(plot.mort.rate.2001.2010$mortality.rate)
# but for the second time period its the # of newly dead trees at time 20
plot.mort.rate.2011.2020 <- all.trees.2011.2020 %>% group_by(plot, mort.scheme, scenario) %>% filter(time == 20)%>% mutate(mortality.rate = prop.dead/20) %>% dplyr::select(-time)
hist(plot.mort.rate.2011.2020$mortality.rate)

summary(plot.mort.rate.2011.2020$mortality.rate)

# plot-level mortality ranges from 0-5% mortality in from 2001-2010
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010, y =mortality.rate_2010_2020))+geom_point()

# plot-level mortality ranges from 0-5% mortality in from 2001-2010
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010))+geom_histogram()
# and the range is about the same from 2011-2020
ggplot(plot.mort.rate2001.2020, aes(x =  mortality.rate_2010_2020))+geom_histogram()

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
#------------------------------------------------------------------------------------
# Make a pretty figure of total biomass by different components across all the stands in the region
#------------------------------------------------------------------------------------
#all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8_full.RDS")
# all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8.60thresh_full.RDS")
# all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8.60thresh_full_1000DImort.RDS")
# all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8.60thresh_full_100DImort.RDS")
#all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_0.8.60thresh_full_0.9_scale_mort.RDS")
#all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_60thresh_full_1.1_scale_mort.RDS")
all10plot <- readRDS("all.AGB.fiaperiodic_singleCC_60thresh_full_1_scale_mort.RDS")
#all10plot <- all10plots
# get the low and high values and sum across plots?
all.woody.sums <- all10plot %>% group_by(mort.scheme, rcp, year) %>% 
  summarise(across(c(mAGB:low.foliage), mean)) # sum up all plots for each column simply for each year and rcp, and mort.scheme

ggplot(all10plot, aes(x = year, y = mAGB/1000, group = plot))+geom_line()+facet_wrap(~rcp)
ggplot(all10plot, aes(x = year, y = mAGB.dead/1000, group = plot))+geom_line()+facet_wrap(~rcp)
#ggplot(all10plot, aes(x = year, y = mAGB.dead, group = plot))+geom_line()+facet_wrap(~rcp)

# convert on a per acre basis--based on 517 plots in the forecats
n.plt <- all10plot %>% group_by(mort.scheme, rcp, year) %>% summarise(n = n())
unique(n.plt$n)

all.woody.per.acre <- all.woody.sums %>% group_by(mort.scheme, rcp, year) %>%
  summarise(across(c(mAGB:low.foliage), function(x){x/unique(n.plt$n)}))

# convert to Teragrams and Carbon
# convert to g/ha to Tg/ha: 1 Tg = 1000000000 Mg
all.woody.per.acre.TgC <-  all.woody.per.acre %>% group_by(mort.scheme, rcp, year) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){(x*0.501)/100000000})) 

all.woody.per.acre.MgC <-  all.woody.per.acre %>% group_by(mort.scheme, rcp, year) %>% 
  summarise(across(c(mAGB:low.foliage), function(x){(x*0.501)})) # convert to Teragrams and Carbon


# stem.branch = add stemwood + live branch
# branch.wood.bark = stembranch + stembark
# all.live = branch.wood.bark + foliage
# dead.live.branch = all.live + dead branch

all.woody.perha.TgC <- all.woody.per.acre.TgC %>% group_by(mort.scheme, rcp, year) %>% 
  mutate(upA.stem.branch = upA.stemwood + upA.branchlive, 
         lowA.stem.branch = upA.stemwood, 
         
         upA.branch.wood.bark = upA.stem.branch + upA.stembark, 
         lowA.branch.wood.bark = upA.stem.branch, 
         
         upA.all.live = upA.branch.wood.bark + upA.foliage, 
         lowA.all.live = upA.branch.wood.bark, 
         
         upA.deadbranch.live = upA.all.live+ upA.branchdead, 
         lowA.deadbranch.live = upA.all.live) %>% 
       
         # do the same for the nPP:
  # I think that we may need to switch to mins here
         mutate(
         up.stem.branch = up.stemwood + up.branchlive, 
         low.stem.branch = up.stemwood, 
         
         up.branch.wood.bark = up.stem.branch + up.stembark , 
         low.branch.wood.bark = up.stem.branch, 
         
         up.all.live = up.branch.wood.bark + up.foliage, 
         low.all.live = up.branch.wood.bark, 
         
         up.deadbranch.live = up.all.live+ up.branchdead, 
         low.deadbranch.live = up.all.live, 
         ) 

# pretty figure for the regional forecasts:
regional.C.trends <- ggplot()+
  #geom_ribbon(data = total.plot, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
  geom_ribbon(data = all.woody.perha.TgC, aes(x = year, ymin = lowA.stemwood, ymax = upA.stemwood, fill = "stem wood"))+
  geom_ribbon(data = all.woody.perha.TgC, aes(x = year, ymin = lowA.stem.branch, ymax = upA.stem.branch, fill = "live branch"))+
  geom_ribbon(data = all.woody.perha.TgC, aes(x = year, ymin = lowA.branch.wood.bark, ymax = upA.branch.wood.bark, fill = "stem bark"))+
  geom_ribbon(data = all.woody.perha.TgC, aes(x = year, ymin = lowA.all.live, ymax = upA.all.live, fill = "foliage"))+
  
  geom_ribbon(data = all.woody.perha.TgC, aes(x = year, ymin = lowA.deadbranch.live, ymax = upA.deadbranch.live, fill = "dead branch"))+
  #geom_ribbon(data = all.woody.summed.TgC, aes(x = year, ymin = lowA.deadstem, ymax = upA.deadstem, fill = "dead stem"))+
  
  theme_bw(base_size = 14)+
  ylab(paste("Regional Forest Carbon Density \n (Tg Carbon/hectare)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black"))+
  facet_wrap(~rcp, ncol = 4)

# plot NPP:
regional.NPP <- ggplot()+
  #geom_ribbon(data = total.plot, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
  geom_ribbon(data = all.woody.perha.TgC %>% filter(!year %in% 2001:2002), aes(x = year, ymin = low.stemwood, ymax = up.stemwood, fill = "stem wood"))+
  geom_ribbon(data = all.woody.perha.TgC %>% filter(!year %in% 2001:2002), aes(x = year, ymin = low.stem.branch, ymax = up.stem.branch, fill = "live branch"))+
  geom_ribbon(data = all.woody.perha.TgC %>% filter(!year %in% 2001:2002), aes(x = year, ymin = low.branch.wood.bark, ymax = up.branch.wood.bark, fill = "stem bark"))+
  geom_ribbon(data = all.woody.perha.TgC %>% filter(!year %in% 2001:2002), aes(x = year, ymin = low.all.live, ymax = up.all.live, fill = "foliage"))+
  
  geom_ribbon(data = all.woody.perha.TgC%>% filter(!year %in% 2001:2002), aes(x = year, ymin = low.deadbranch.live, ymax = up.deadbranch.live, fill = "dead branch"))+
  #geom_ribbon(data = all.woody.summed.TgC, aes(x = year, ymin = lowA.deadstem, ymax = upA.deadstem, fill = "dead stem"))+
 
  theme_bw(base_size = 14)+xlim(2004, 2099)+
  ylab(paste("Carbon Density Difference \n (Tg Carbon/hectare/year)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual( name = "Component",
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black"))+
  facet_wrap(~rcp, ncol = 4) #+  geom_abline(aes(intercept = 0, slope = 0), color = "salmon", linetype = "dashed")


Carbon.legend <- cowplot::get_legend(regional.NPP)

png(height = 7, width = 12, units = "in", res = 300, "outputs/Carbon_density_regional_NPP_total_fullperiodic_perha_60threshold_1scale.mort10.png")
cowplot::plot_grid(cowplot::plot_grid(regional.C.trends+theme(legend.position = "none", axis.text.x = element_text(hjust = 1, angle = 45)), 
                   regional.NPP+theme(legend.position = "none", axis.text.x = element_text(hjust = 1, angle = 45)), 
                   ncol = 1, align = "hv"), Carbon.legend, ncol = 2, rel_widths = c(0.85, 0.1))
dev.off()


