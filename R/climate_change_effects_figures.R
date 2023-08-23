library(here)
library(tidyverse)
library(rFIA)
library(sf)

# read in periodic data from mortality_parse_figure.R
periodic.data <- readRDS("INWE_FIA_PLO_TREE_COND_POP_STRATUM_ESNT_UNIT.RDS")

#--------------------------------------------------------------------------------
# Do the scaling using expn factors for dead AGB
#--------------------------------------------------------------------------------
mort.all.parse.60 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh.RDS"))
# mort.all.parse.60.1.1 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh_1.1.RDS"))
# mort.all.parse.60.0.9 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh_0.9.RDS"))
head(mort.all.parse.60)
summary(mort.all.parse.60)

scale.mort.expns <- function(mort.df, parse = "full") {
  
  AGB.dead <- mort.df %>% filter(parse == parse) # just get the full scernario
  colnames(AGB.dead)[1]<- "PLT_CN"
  AGB.dead$PLT_CN <- as.numeric(AGB.dead$PLT_CN)
  periodic.data <- periodic.data %>% filter(PLT_CN %in% unique(AGB.dead$PLT_CN))
  
  # scale up mortality in Tg/acre to Tg for each stand, then sum up
  # create function to scale biomass to C and convert to Tg
  # Cfraction
  C.convert.deadwood <- function(x, C.frac = 0.4822){(x*C.frac)} #/1000000}
  C.convert.deadwood(AGB.dead$mAGB.dead, C.frac = 0.4822)
  
  
  plot.periodic <- unique(periodic.data[, c("ECOSUBCD", "ESTN_UNIT_CN", "ESTN_METHOD", "STRATUM_CN", "PLT_CN", "EXPNS")])
  nrow(plot.periodic) # 535 unique plots:
  
  AGB.dead.EXPNS <- left_join(AGB.dead, plot.periodic)
  
  
  # ## Estimate Tree totals
  # tre_bioGrp <- AGB.periodic %>%
  #   filter(EVAL_TYP == 'EXPVOL') %>%
  #   ## Make sure we only have unique observations of plots, trees, etc.
  #   distinct(ECOSUBCD, ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE,year, .keep_all = TRUE) %>%
  #   ## Plot-level estimates first (multiplying by EXPNS here)
  #   group_by(year, ECOSUBCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  #   summarize(bioPlot = mAGB*EXPNS , # out mAGB is in kg (?), assuming acre plto
  #             carbPlot = bioPlot*0.501) %>%  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  #   ## to obtain population totals
  #   group_by(year, ECOSUBCD) %>%
  #   summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
  #             CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))
  # tre_bioGrp 
  
  ## Estimate Area totals by ECOTYPCD
  # area_bioGrp <- AGB.periodic %>%
  #   filter(EVAL_TYP == 'EXPCURR') %>%
  #   ## Make sure we only have unique observations of plots, trees, etc.
  #   distinct(ECOSUBCD, ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  #   ## Plot-level estimates first (multiplying by EXPNS here)
  #   group_by(year, ECOSUBCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  #   summarize(forArea = sum(CONDPROP_UNADJ *  EXPNS, na.rm = TRUE)) %>% # I changed this not sure if its right
  #   ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  #   ## to obtain population totals
  #   group_by(year, ECOSUBCD) %>%
  #   summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))
  # area_bioGrp 
  
  
  
  # get general summary of the total mortality in terms of C for each mortality type
  mort.test <- AGB.dead.EXPNS %>%
    #dplyr::mutate(across(mAGB.dead:hiAGB.dead.di, function(x){x*EXPNS})) %>%
    #group_by(PLT_CN, mort.scheme, rcp, year, parse) %>%
    group_by(year, ECOSUBCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN, mort.scheme, rcp, parse) %>%
    mutate(across(mAGB.dead:hiAGB.dead.di, function(x){x*EXPNS})) %>%
    # mutate(bioPlot = mAGB*EXPNS , # out mAGB is in kg (?), assuming acre plto
    #           carbPlot = bioPlot*0.501) %>%  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
    ## to obtain population totals
    summarise(across(mAGB.dead:hiAGB.dead.di, function(x){C.convert.deadwood(x)})) %>% 
    ungroup() %>% # sum across all the PLT_CNs
    group_by(rcp, mort.scheme, year, parse) %>%
    summarise(across(c(mAGB.dead:hiAGB.dead.di), sum))
  
  
  mort.test
}

full.1 <- scale.mort.expns(mort.df = mort.all.parse.60, parse = "full")
full.1.noCC <- scale.mort.expns(mort.df = mort.all.parse.60, parse = "detrendedCC")

ending.mort.1 <- full.1 %>% filter(parse %in% c("full", "detrendedCC") & year %in% 2098)
#ending.mort.1.1 <- full.1.1 %>% filter(parse %in% "full" & year %in% 2098)
#ending.mort.0.9 <- full.0.9 %>% filter(parse %in% "full" & year %in% 2098)

ending.mort.1$parse <- ifelse(ending.mort.1$parse %in% "detrendedCC", "no climate change", ending.mort.1$parse)

#ending.mort.1$scaled.mortality <- "tuned to historic"
# ending.mort.1.1$scaled.mortality <- "- 10%"
# ending.mort.0.9$scaled.mortality <- "+ 10%"
# 
# ending.mort <- rbind(ending.mort.1, ending.mort.0.9, ending.mort.1.1)

full.low.high <- ending.mort.1 %>% 
  select(rcp, parse, mort.scheme, year, lowAGB.dead, hiAGB.dead, mAGB.dead)
full.low.high.m <- reshape2::melt(full.low.high, id.vars = c("rcp","parse", "mort.scheme","year"))
#colnames(full.low.high.diffs)[6] <- "CI"

# full.low.high.diffs <- full.low.high.m %>% group_by(rcp, mort.scheme, year, scaled.mortality, variable)%>%
#   spread(scaled.mortality, value) %>% mutate(INC.10 = ((`tuned to historic` - `+ 10%`)/`tuned to historic`)*-100, 
#                                              DEC.10 = ((`tuned to historic` - `- 10%`)/`tuned to historic`)*-100,
#                                              HISTORIC = ((`tuned to historic` - `tuned to historic`)/`tuned to historic`)*100) %>% select(rcp, mort.scheme, year, variable, HISTORIC, INC.10, DEC.10)
# colnames(full.low.high.diffs)[4] <- "CI"

# full.low.high.diffs.m <- reshape2::melt(full.low.high.diffs, id.vars = c("rcp", "mort.scheme", "year", "CI"))
# dead.tree.sensitivity <- full.low.high.diffs.m %>% group_by(year, rcp, mort.scheme) %>% spread(CI, value) %>%
#   group_by(year, rcp, mort.scheme, variable) %>% 
#   mutate(low.DEAD = min(lowAGB.dead, hiAGB.dead), 
#          high.DEAD = max(lowAGB.dead, hiAGB.dead), 
#          mDEAD = mean(mAGB.dead))



#--------------------------------------------------------------------------------
# Do the scaling using expn factors for live AGB
#--------------------------------------------------------------------------------
AGB.1 <- readRDS("outputs/parse.DIDD.mort.60SDIthreshold.RDS")
#colnames(AGB)[1] <- "PLT_CN"
AGB.0.9 <- readRDS("outputs/parse.DIDD.mort.60SDIthreshold_0.9.RDS")
AGB.1.1 <- readRDS("outputs/parse.DIDD.mort.60SDIthreshold_1.1.RDS")
AGB = AGB.1
AGB  = AGB.0.9
scale.all.live.expns <- function(AGB, parse = "full") {
  
  AGB.live <- AGB %>% filter(parse == parse) # just get the full scernario
  colnames(AGB.live)[1]<- "PLT_CN"
  AGB.live$PLT_CN <- as.numeric(AGB.live$PLT_CN)
  periodic.data <- periodic.data %>% filter(PLT_CN %in% unique(AGB.live$PLT_CN))
  
  # scale up mortality in Tg/acre to Tg for each stand, then sum up
  # create function to scale biomass to C and convert to Tg
  # Cfraction
  C.convert.livewood <- function(x, C.frac = 0.501){(x*C.frac)} #/1000000}
  #C.convert.livewood(parse.all.mort$mAGB.dead, C.frac = 0.501)
  
  
  plot.periodic <- unique(periodic.data[, c("ECOSUBCD", "ESTN_UNIT_CN", "ESTN_METHOD", "STRATUM_CN", "PLT_CN", "EXPNS")])
  nrow(plot.periodic) # 535 unique plots:
  
  AGB.live.EXPNS <- left_join(AGB.live, plot.periodic)
  
  
  
  # get general summary of the total mortality in terms of C for each mortality type
  live.test <- AGB.live.EXPNS %>%
    #dplyr::mutate(across(mAGB.dead:hiAGB.dead.di, function(x){x*EXPNS})) %>%
    #group_by(PLT_CN, mort.scheme, rcp, year, parse) %>%
    group_by(year, ECOSUBCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN, mort.scheme, rcp, parse) %>%
    mutate(across(mAGB:low.foliage, function(x){x*EXPNS})) %>%
    # mutate(bioPlot = mAGB*EXPNS , # out mAGB is in kg (?), assuming acre plto
    #           carbPlot = bioPlot*0.501) %>%  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
    ## to obtain population totals
    summarise(across(mAGB:lowA.dead, function(x){C.convert.livewood(x)})) %>% 
    ungroup() %>% # sum across all the PLT_CNs
    group_by(rcp, mort.scheme, year, parse) %>%
    summarise(across(c(mAGB:lowA.dead), sum, na.rm = TRUE))
  
  
  live.test
}

full.live.1 <- scale.all.live.expns(AGB = AGB.1, parse = "full")
#full.live.1.nocc <- scale.all.live.expns(AGB = AGB.1, parse = "detrendedCC")
#full.live.1.1 <- scale.all.live.expns(AGB = AGB.1.1, parse = "full")
#full.live.0.9 <- scale.all.live.expns(AGB = AGB.0.9, parse = "full")

# full.live.1$scaled.mortality <- "tuned to historic"
# full.live.1.1$scaled.mortality <- "- 10%"
# full.live.0.9$scaled.mortality <- "+ 10%"

#
full.low.high.live <- full.live.1 %>% filter(parse %in% c("full", "no climate change") & year %in% 2098) %>% 
  select(rcp, parse, mort.scheme, year, lowA, upA, mAGB)
#full.low.high.m <- reshape2::melt(full.low.high, id.vars = c("rcp","parse", "mort.scheme", "scaled.mortality","year"))



## do the math here

# join the dead and live: 
full.low.high.dead.live <- left_join(full.low.high.live, full.low.high)

# low dead values should start at upA and high dead values should be upA + (Range of low and high dead)
full.low.high.dead.live$lowdeadAGBtot <- full.low.high.dead.live$upA
full.low.high.dead.live$highdeadAGBtot <- (full.low.high.dead.live$upA) + (full.low.high.dead.live$hiAGB.dead - full.low.high.dead.live$lowAGB.dead)

#full.low.high.dead.live %>% select(rcp, parse, mort.scheme, year, lowAGB.dead, hiAGB.dead, lowAGBtot, highAGBtot)

df.parse <- data.frame(parse = c("full", "no climate change"), 
                             parse.number = 1:2)

full.low.high.dead.live <- left_join(full.low.high.dead.live, df.parse)





full.low.high.dead.live$mid.dead <- rowMeans(full.low.high.dead.live[,c("lowAGB.dead", "hiAGB.dead")])
full.low.high.dead.live$mAGBtot <- full.low.high.dead.live$mAGB + full.low.high.dead.live$mid.dead
full.low.high.dead.live$lowAGBtot <- full.low.high.dead.live$lowA + full.low.high.dead.live$lowAGB.dead
full.low.high.dead.live$highAGBtot <- full.low.high.dead.live$upA + full.low.high.dead.live$hiAGB.dead

low.high.dead <- full.low.high.dead.live %>% select(rcp, year, parse, lowAGB.dead, hiAGB.dead, mAGB.dead)
low.high.live <- full.low.high.dead.live %>% select(rcp, year, parse, lowAGBtot, highAGBtot, mAGBtot)

low.high.dead$`Carbon Pool` <- "Standing Dead C"
low.high.live$`Carbon Pool` <- "Live Tree C"
colnames(low.high.dead)[5:7] <- c("ci.low",  "ci.hi", "mAGB")
colnames(low.high.live)[5:7] <- c("ci.low",  "ci.hi", "mAGB")

low.high.Cpools <- rbind( low.high.live, low.high.dead)
#low.high.Cpools$parse <- factor(low.high.Cpools$scaled.mortality, levels = c("- 10%",  "tuned to historic", "+ 10%"))

totalC.barplot.parse <- ggplot(data = low.high.Cpools, aes(x = interaction(   rcp, parse), y = (mAGB/1000000000), group = rcp, fill = `Carbon Pool`), alpha = 0.5)+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7)+
  scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
   annotate("text", x = 1:8, y =  10,
            label = rep(c("rcp 2.6", "rcp 4.5", 
                          "rcp 6.0", "rcp 8.5"), 2), 
            angle = 90) +
  annotate("text", c(2.5, 6.5), y = - 10, label = c("climate change", "no climate change"))+
  geom_vline(aes(xintercept = c(4.5)))+
  #geom_vline(aes(xintercept = c(8.5)))+
  ylab("Mean Total Aboveground C (Tg)")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank())
  
totalC.barplot.parse 

ggsave(height = 4, width = 6.5, units = "in", "outputs/mean_total_C_live_dead_cc_vs_noCC.png")

#-----------------------------------------------------
# make a time series figure with the difference 
#-----------------------------------------------------

ending.mort.1 <- full.1 %>% filter(parse %in% c("full", "detrendedCC"))
#ending.mort.1.1 <- full.1.1 %>% filter(parse %in% "full" & year %in% 2098)
#ending.mort.0.9 <- full.0.9 %>% filter(parse %in% "full" & year %in% 2098)

ending.mort.1$parse <- ifelse(ending.mort.1$parse %in% "detrendedCC", "no climate change", ending.mort.1$parse)

#ending.mort.1$scaled.mortality <- "tuned to historic"
# ending.mort.1.1$scaled.mortality <- "- 10%"
# ending.mort.0.9$scaled.mortality <- "+ 10%"
# 
# ending.mort <- rbind(ending.mort.1, ending.mort.0.9, ending.mort.1.1)

full.low.high <- ending.mort.1 %>% 
  select(rcp, parse, mort.scheme, year, lowAGB.dead, hiAGB.dead, mAGB.dead)


full.low.high.live <- full.live.1 %>% filter(parse %in% c("full", "no climate change")) %>% 
  select(rcp, parse, mort.scheme, year, lowA, upA, mAGB)


## do the math here
# get totals
# liveUP + deadUP = totalup
# liveD + deadD = totalDown

# lowDead = totalDOWN
# upDead = totalUp - liveUp
# lowAlive = upDead
# UpAlive = totalup
# note that dead should be on top here

# join the dead and live: 
full.low.high.dead.live <- left_join(full.low.high.live, full.low.high)

# get the totals for up and down
full.low.high.dead.live$totalUP <- full.low.high.dead.live$hiAGB.dead + full.low.high.dead.live$upA
full.low.high.dead.live$totalDOWN <- full.low.high.dead.live$lowAGB.dead + full.low.high.dead.live$lowA

# parse this into total dead and total live

full.low.high.dead.live$lowdeadAGBtot <- full.low.high.dead.live$totalUP - (full.low.high.dead.live$hiAGB.dead - full.low.high.dead.live$lowAGB.dead)
full.low.high.dead.live$highdeadAGBtot <-  full.low.high.dead.live$totalUP 
full.low.high.dead.live$lowAGBtot <- full.low.high.dead.live$highdeadAGBtot
full.low.high.dead.live$upAGBtot <- full.low.high.dead.live$totalDOWN

# get midpoints:
full.low.high.dead.live$mAGB.dead <- rowMeans(full.low.high.dead.live[,c("lowdeadAGBtot", "highdeadAGBtot")])
full.low.high.dead.live$mAGB.live <- rowMeans(full.low.high.dead.live[,c("lowAGBtot", "upAGBtot")])


#full.low.high.dead.live %>% select(rcp, parse, mort.scheme, year, lowAGB.dead, hiAGB.dead, lowAGBtot, highAGBtot)

df.parse <- data.frame(parse = c("full", "no climate change"), 
                       parse.number = 1:2, 
                       parse.name = c("climate change", "no climate change"))

full.low.high.dead.live <- left_join(full.low.high.dead.live, df.parse)






low.high.dead <- full.low.high.dead.live %>% select(rcp, year, parse, lowdeadAGBtot, highdeadAGBtot, mAGB.dead)
low.high.live <- full.low.high.dead.live %>% select(rcp, year, parse, lowAGBtot, upAGBtot, mAGB.live)

low.high.dead$`Carbon Pool` <- "Standing Dead C"
low.high.live$`Carbon Pool` <- "Live Tree C"
colnames(low.high.dead)[5:7] <- c("ci.low",  "ci.hi", "mAGB")
colnames(low.high.live)[5:7] <- c("ci.low",  "ci.hi", "mAGB")

low.high.Cpools <- rbind( low.high.live, low.high.dead)
#low.high.Cpools$parse <- factor(low.high.Cpools$scaled.mortality, levels = c("- 10%",  "tuned to historic", "+ 10%"))

# want a stacked ribbon over time showing the total range of C, colored by live vs standing dead C
ggplot() + 
  geom_ribbon(data = low.high.Cpools %>% filter(rcp %in% "rcp26"), aes(x = year, ymin = ci.low/1000000000, ymax = ci.hi/1000000000, fill = `Carbon Pool`))+
  facet_wrap(~parse)+scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
  ylab("Total Aboveground C (Tg)")+
  xlab("Year")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(height = 4, width = 6, units = "in", "outputs/total_C_climate_change_noclimateChange_toLive_DeadCpools.png")

#----------------------------------------------------------------
# calculate the amount of each pool due to climate change
#----------------------------------------------------------------
# rearrange to take the difference between full - no climate change
# should be negative effect on live tree C and posisitve on standing dead
# Do this for the original values in the data frame not on the summed values, I think
full.low.high.dead.live

low.high.dead.real <- full.low.high.dead.live %>% select(rcp, year, parse, lowAGB.dead, hiAGB.dead)
low.high.live.real <- full.low.high.dead.live %>% select(rcp, year, parse, upA, lowA)

low.high.dead.real$`Carbon Pool` <- "Standing Dead C"
low.high.live.real$`Carbon Pool` <- "Live Tree C"
colnames(low.high.dead.real)[5:6] <- c("ci.low",  "ci.hi")
colnames(low.high.live.real)[5:6] <- c("ci.low",  "ci.hi")

low.high.Cpools.real <- rbind( low.high.live.real, low.high.dead.real)
low.high.Cpools.real.CC <- low.high.Cpools.real %>% filter(parse %in% "full")
low.high.Cpools.real.noCC <- low.high.Cpools.real %>% filter(parse %in% "no climate change")
colnames(low.high.Cpools.real.CC)[5:6] <- c("CC.low", "CC.hi") 
colnames(low.high.Cpools.real.noCC)[5:6] <- c("noCC.low", "noCC.hi") 
full.pools <- left_join(low.high.Cpools.real.CC %>% select(-parse), low.high.Cpools.real.noCC %>% select(-parse))

Full.pool.diff <- full.pools %>% group_by(mort.scheme, rcp, year, `Carbon Pool`)%>% 
  mutate(CC.effect.low = CC.low - noCC.low, 
         CC.effect.hi = CC.hi - noCC.hi, 
         CC.effect.mid = (CC.effect.hi + CC.effect.low)/2)

#ggplot(Full.pool.diff, aes(x = year, ymin = CC.effect.low, ymax = CC.effect.hi, fill = `Carbon Pool`))+geom_ribbon()+facet_wrap(~rcp)
ggplot(data = Full.pool.diff, aes(x = year, y = CC.effect.mid/1000000000, fill = `Carbon Pool`))+geom_bar(stat = "identity", width = 0.9)+
  geom_errorbar(data = Full.pool.diff, aes(x = year, ymin = CC.effect.low/1000000000, ymax = CC.effect.hi/1000000000), width = 0.1, color = "lightgrey")+
  facet_wrap(~rcp, nrow = 4)+scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
  ylab("Effect of Climate Change on Standing Live & Dead C pools (Tg)")+
  xlab("Year")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), 
        panel.grid = element_blank())

ggsave(height = 10, width = 6, units = "in", "outputs/total_contribution_of_climate_change_toLive_DeadCpools.png")


# combine the results for RCP26:
RCP.26.cc.nocc <- ggplot() + 
  geom_ribbon(data = low.high.Cpools %>% filter(rcp %in% "rcp26"), aes(x = year, ymin = ci.low/1000000000, ymax = ci.hi/1000000000, fill = `Carbon Pool`))+
  facet_wrap(~parse)+scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
  ylab("Total Aboveground C (Tg)")+
  xlab("Year")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

CC.contribution.RCP26 <- ggplot(data = Full.pool.diff %>% filter(rcp %in% "rcp26"), aes(x = year, y = CC.effect.mid/1000000000, fill = `Carbon Pool`))+geom_bar(stat = "identity", width = 0.9)+
  geom_errorbar(data = Full.pool.diff %>% filter(rcp %in% "rcp26"), aes(x = year, ymin = CC.effect.low/1000000000, ymax = CC.effect.hi/1000000000), width = 0.1, color = "lightgrey")+
  facet_wrap(~rcp, nrow = 4)+scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
  ylab("Effect of Climate Change on \nStanding Live & Dead C pools (Tg)")+
  xlab("Year")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), 
        panel.grid = element_blank(), legend.position = "bottom")

library(cowplot)
Cpool.legend <- get_legend(CC.contribution.RCP26)
png(height = 4.5, width = 9.2, units = "in", res = 300, "outputs/RCP2.6_CC_effects_combined_figure.png")
plot_grid(plot_grid(RCP.26.cc.nocc + theme(legend.position = "none"), 
          CC.contribution.RCP26 + theme(legend.position = "none"), align = "hv"), 
          Cpool.legend, rel_heights = c(0.9, 0.05), ncol = 1)
dev.off()
