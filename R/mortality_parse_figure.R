library(here)
library(tidyverse)
library(rFIA)
library(sf)

db <- readRDS("data/InWeUS_FIAdb.rds")
db$PLOT <- db$PLOT %>% filter(STATECD %in% c(4, 8, 35, 49, 56, 16, 30))#& CN %in% unique(AGB$CN))
db$POP_EVAL <- rbind(read.csv("data/AZ_POP_EVAL.csv"), read.csv("data/NM_POP_EVAL.csv"), read.csv("data/UT_POP_EVAL.csv"), read.csv("data/CO_POP_EVAL.csv"),  read.csv("data/MT_POP_EVAL.csv"), read.csv("data/ID_POP_EVAL.csv"), read.csv("data/WY_POP_EVAL.csv"))
db$POP_ESTN_UNIT <- rbind(read.csv("data/AZ_POP_ESTN_UNIT.csv"), read.csv("data/NM_POP_ESTN_UNIT.csv"),  read.csv("data/UT_POP_ESTN_UNIT.csv"),  read.csv("data/CO_POP_ESTN_UNIT.csv"), read.csv("data/MT_POP_ESTN_UNIT.csv"),  read.csv("data/ID_POP_ESTN_UNIT.csv"),  read.csv("data/WY_POP_ESTN_UNIT.csv"))
db$POP_PLOT_STRATUM_ASSGN <- rbind(read.csv("data/AZ_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/NM_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/UT_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/CO_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/MT_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/ID_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/WY_POP_PLOT_STRATUM_ASSGN.csv"))
db$POP_EVAL_TYP <- rbind(read.csv("data/AZ_POP_EVAL_TYP.csv"), read.csv("data/NM_POP_EVAL_TYP.csv"), read.csv("data/UT_POP_EVAL_TYP.csv"), read.csv("data/CO_POP_EVAL_TYP.csv"), read.csv("data/MT_POP_EVAL_TYP.csv"), read.csv("data/ID_POP_EVAL_TYP.csv"), read.csv("data/WY_POP_EVAL_TYP.csv"))
db$POP_STRATUM <- rbind(read.csv("data/AZ_POP_STRATUM.csv"), read.csv("data/NM_POP_STRATUM.csv"), read.csv("data/UT_POP_STRATUM.csv"), read.csv("data/CO_POP_STRATUM.csv"), read.csv("data/MT_POP_STRATUM.csv"), read.csv("data/ID_POP_STRATUM.csv"), read.csv("data/WY_POP_STRATUM.csv"))


# make the mortality figure with different + or minus mortality

# scale mortality up by area:
## Select only the columns we need from each table, to keep things slim
PLOT <- select(db$PLOT, CN, MACRO_BREAKPOINT_DIA, ECOSUBCD)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)


PLOT$ECOSUBCD <- str_trim(PLOT$ECOSUBCD)
PLOT$ECOCD <- str_sub(PLOT$ECOSUBCD, 1, -2)

POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)



ids <- db$POP_EVAL %>%
  select('CN', 'END_INVYR', 'EVALID') %>%
  inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>%
  ## Now we filter out everything except current area and 
  ## current volume ids
  filter(EVAL_TYP %in% c('EXPCURR', 'EXPVOL'))

  
db <- clipFIA(db, evalid = ids$EVALID)
## Select only the columns we need from each table, to keep things slim
PLOT <- select(PLOT, CN, MACRO_BREAKPOINT_DIA, ECOCD, ECOSUBCD)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD, FORTYPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)
POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)




# get plot data & save as an object

data <- PLOT %>% #filter 
  ## Add a PLT_CN column for easy joining
  mutate(PLT_CN = CN) %>%
  # ## Join COND & TREE
  left_join(COND, by = 'PLT_CN') %>%
  left_join(TREE, by = c('PLT_CN', 'CONDID')) %>%
  ## Population tables
  left_join(POP_PLOT_STRATUM_ASSGN, by = 'PLT_CN') %>%
  left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
  left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
  left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
  left_join(POP_EVAL_TYP, by = 'EVAL_CN')

data <- data %>%
  mutate(
    ## AREA
    aAdj = case_when(
      ## When NA, stay NA
      is.na(PROP_BASIS) ~ NA_real_,
      ## If the proportion was measured for a macroplot,
      ## use the macroplot value
      PROP_BASIS == 'MACR' ~ as.numeric(ADJ_FACTOR_MACR),
      ## Otherwise, use the subpplot value
      PROP_BASIS == 'SUBP' ~ ADJ_FACTOR_SUBP),
    ## TREE
    tAdj = case_when(
      ## When DIA is na, adjustment is NA
      is.na(DIA) ~ ADJ_FACTOR_SUBP,
      ## When DIA is less than 5", use microplot value
      DIA < 5 ~ ADJ_FACTOR_MICR,
      ## When DIA is greater than 5", use subplot value
      DIA >= 5 ~ ADJ_FACTOR_SUBP
    ))

data <- data %>%
  mutate(YEAR = END_INVYR) %>%
  ## remove any NAs
  filter(!is.na(YEAR))

periodic.data <- data
saveRDS(data, "INWE_FIA_PLO_TREE_COND_POP_STRATUM_ESNT_UNIT.RDS")

rm(data, db, POP_PLOT_STRATUM_ASSGN, TREE, COND, PLOT)


#---------------------------------------------------------------------------------
# 

periodic.data <- readRDS("INWE_FIA_PLO_TREE_COND_POP_STRATUM_ESNT_UNIT.RDS")
mort.all.parse.60 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh.RDS"))
mort.all.parse.60.1.1 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh_1.1.RDS"))
mort.all.parse.60.0.9 <- readRDS( here("outputs/", "all.plot.mort.C.60SDIthresh_0.9.RDS"))

scale.mort.expns <- function(mort.df, parse = "full") {

    AGB.dead <- mort.df %>% filter(parse == parse) # just get the full scernario
    colnames(AGB.dead)[1]<- "PLT_CN"
    AGB.dead$PLT_CN <- as.numeric(AGB.dead$PLT_CN)
    periodic.data <- periodic.data %>% filter(PLT_CN %in% unique(AGB.dead$PLT_CN))
    
    # scale up mortality in Tg/acre to Tg for each stand, then sum up
    # create function to scale biomass to C and convert to Tg
    # Cfraction
    C.convert.deadwood <- function(x, C.frac = 0.4822){(x*C.frac)/1000000000}
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
full.1.1 <- scale.mort.expns(mort.df = mort.all.parse.60.1.1, parse = "full")
full.0.9 <- scale.mort.expns(mort.df = mort.all.parse.60.0.9, parse = "full")




head(full.1)
head(full.0.9)
head(full.1.1)

mort.total.rcp <- ggplot()+geom_ribbon(data = full.1 %>% filter(parse %in% "full"), aes(x = year, ymin = lowAGB.dead, ymax = hiAGB.dead, fill = "Density Independent"))+
  geom_ribbon(data = full.1 %>% filter(parse %in% "full"), aes(x = year, ymin = lowAGB.dead, ymax = lowAGB.dead + hiAGB.dead.dd, fill = "Density Dependent"))+
  facet_wrap(~rcp, ncol = 4)+theme_bw()+#theme(panel.grid = element_blank())+
  scale_fill_manual("Mortality", values = c("Density Dependent" = "#7570b3", "Density Independent" = "#d95f02"))+
  ylab("Cumulative Dead Wood carbon (Tg C)")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

mort.total.rcp
ggsave(height = 4, width = 8, units = "in", "outputs/Total_MORT_DI_DD.PNG")


full.low.high <- full.1 %>% filter(parse %in% c("full", "detrendedCC")) %>% select(rcp, parse, mort.scheme, year, lowAGB.dead, hiAGB.dead, mAGB.dead)
full.low.high.m <- reshape2::melt(full.low.high, id.vars = c("rcp","parse", "mort.scheme", "year"))
full.low.high.diffs <- full.low.high.m %>% group_by(rcp, mort.scheme, year, variable)%>%
  spread(parse, value) %>% mutate(climatechange = ((full - detrendedCC)/full)*100) %>% select(rcp, mort.scheme, year, variable, climatechange)

colnames(full.low.high.diffs)[4] <- "CI"
full.low.high.diffs.m <- reshape2::melt(full.low.high.diffs, id.vars = c("rcp", "mort.scheme", "year", "CI"))
climate.change.dead.tree.effect <- full.low.high.diffs.m %>% group_by(year, rcp, mort.scheme) %>% spread(CI, value) %>%
  mutate(low.DEAD = min(lowAGB.dead, hiAGB.dead), 
         high.DEAD = max(lowAGB.dead, hiAGB.dead))# , 
         #mDEAD = mean(low.DEAD, high.DEAD))
climate.change.dead.tree.effect$mDEAD <- rowMeans(climate.change.dead.tree.effect[,c("low.DEAD", "high.DEAD")])
mort.CC.pct.bar <- ggplot() + 
  geom_bar(data = climate.change.dead.tree.effect %>% filter(year %in% "2098"), aes(x=rcp, y = mDEAD, fill = rcp), size = 5, stat = "identity")+
  geom_errorbar(data = climate.change.dead.tree.effect %>% filter(year %in% "2098"), aes(x=rcp, ymin = low.DEAD, ymax = high.DEAD), size = 0.5, width = 0.1)+
  ylab("% of mortality attributable to climate change")+xlab("Emissions Pathway")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())#+#ylim(0, 30)
mort.CC.pct.bar


mort.CC.pct <- ggplot() + 
  geom_segment(data = climate.change.dead.tree.effect %>% filter(year %in% "2098"), aes(x=rcp, xend = rcp, y = low.DEAD, 
                                                                                         yend = high.DEAD, color = rcp), size = 5)+
  ylab("% of mortality attributable to climate change")+xlab("Emissions Pathway")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())#+#ylim(0, 30)
mort.CC.pct
ggsave(height = 4, width = 4, units = "in", "outputs/PCT_MORT_DUE_TO_CC.PNG")
# ggplot()+geom_errorbar(data = climate.change.dead.tree.effect %>% filter(year %in% "2098"), aes(x = rcp, ymin =low.DEAD, ymax = high.DEAD, color = rcp), width = 0.1)+
#   geom_point(data =  climate.change.dead.tree.effect %>% filter(year %in% "2098"), aes(x = rcp, y = mDEAD, color = rcp))

# now compare the different mortality tuning scenarios:

ending.mort.1 <- full.1 %>% filter(parse %in% "full" & year %in% 2098)
ending.mort.1.1 <- full.1.1 %>% filter(parse %in% "full" & year %in% 2098)
ending.mort.0.9 <- full.0.9 %>% filter(parse %in% "full" & year %in% 2098)

ending.mort.1$scaled.mortality <- "tuned to historic"
ending.mort.1.1$scaled.mortality <- "- 10%"
ending.mort.0.9$scaled.mortality <- "+ 10%"

ending.mort <- rbind(ending.mort.1, ending.mort.0.9, ending.mort.1.1)

full.low.high <- ending.mort %>% filter(parse %in% c("full")) %>% select(rcp, parse, mort.scheme, year,scaled.mortality, lowAGB.dead, hiAGB.dead, mAGB.dead)
full.low.high.m <- reshape2::melt(full.low.high, id.vars = c("rcp","parse", "mort.scheme", "scaled.mortality","year"))
#colnames(full.low.high.diffs)[6] <- "CI"

full.low.high.diffs <- full.low.high.m %>% group_by(rcp, mort.scheme, year, scaled.mortality, variable)%>%
  spread(scaled.mortality, value) %>% mutate(INC.10 = ((`tuned to historic` - `+ 10%`)/`tuned to historic`)*-100, 
                                             DEC.10 = ((`tuned to historic` - `- 10%`)/`tuned to historic`)*-100,
                                             HISTORIC = ((`tuned to historic` - `tuned to historic`)/`tuned to historic`)*100) %>% select(rcp, mort.scheme, year, variable, HISTORIC, INC.10, DEC.10)
colnames(full.low.high.diffs)[4] <- "CI"

full.low.high.diffs.m <- reshape2::melt(full.low.high.diffs, id.vars = c("rcp", "mort.scheme", "year", "CI"))
dead.tree.sensitivity <- full.low.high.diffs.m %>% group_by(year, rcp, mort.scheme) %>% spread(CI, value) %>%
 group_by(year, rcp, mort.scheme, variable) %>% 
  mutate(low.DEAD = min(lowAGB.dead, hiAGB.dead), 
         high.DEAD = max(lowAGB.dead, hiAGB.dead), 
         mDEAD = mean(mAGB.dead))


ggplot()+#geom_point(data = dead.tree.sensitivity, aes(x = variable, y = mAGB.dead, color = rcp),  position= position_dodge(width = 0.9))+
  geom_segment(data = dead.tree.sensitivity, aes(x = rcp, xend = rcp, y = lowAGB.dead, 
                                                 yend = hiAGB.dead, color = variable),  size = 5)

# improve this plot....a 10% increase in mortality leads to a 10+% increase in dead tree C

ggplot()+geom_point(data = ending.mort, aes(x = scaled.mortality, y = mAGB.dead, color = rcp))

ggplot()+geom_point(data = ending.mort, aes(x = scaled.mortality, y = mAGB.dead, color = rcp))+
    geom_errorbar(data = ending.mort, aes(x  = scaled.mortality, ymin = lowAGB.dead, ymax = hiAGB.dead, color = rcp), width = 0.5)+
  facet_wrap(~rcp)

#--------------------------------------------------------------------------------
# Do the scaling for the Full AGB
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
  C.convert.livewood <- function(x, C.frac = 0.501){(x*C.frac)/1000000000}
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
full.live.1.1 <- scale.all.live.expns(AGB = AGB.1.1, parse = "full")
full.live.0.9 <- scale.all.live.expns(AGB = AGB.0.9, parse = "full")

full.live.1$scaled.mortality <- "tuned to historic"
full.live.1.1$scaled.mortality <- "- 10%"
full.live.0.9$scaled.mortality <- "+ 10%"

ending.full.live <- rbind(full.live.1, full.live.0.9, full.live.1.1)

full.low.high.live <- ending.full.live %>% filter(parse %in% c("full") & year %in% 2098) %>% 
  select(rcp, parse, mort.scheme, year,scaled.mortality, lowA, upA, mAGB)
#full.low.high.m <- reshape2::melt(full.low.high, id.vars = c("rcp","parse", "mort.scheme", "scaled.mortality","year"))

## do the math here

# join the dead and live: 
full.low.high.dead.live <- left_join(full.low.high.live, full.low.high)
full.low.high.dead.live$lowdeadAGBtot <- full.low.high.dead.live$upA
full.low.high.dead.live$highdeadAGBtot <- (full.low.high.dead.live$upA) + (full.low.high.dead.live$hiAGB.dead - full.low.high.dead.live$lowAGB.dead)

full.low.high.dead.live %>% select(rcp, parse, mort.scheme, year, scaled.mortality, lowAGB.dead, hiAGB.dead, lowAGBtot, highAGBtot)

df.scaled.mort <- data.frame(scaled.mortality = c("- 10%", "tuned to historic", "+ 10%"), 
                             mort.number = 1:3)

full.low.high.dead.live <- left_join(full.low.high.dead.live, df.scaled.mort)
# ribbon only works for continous vars...
ggplot() + geom_ribbon(data = full.low.high.dead.live, aes(x = mort.number, ymin = lowA, ymax = upA), fill = "forestgreen")+
  geom_ribbon(data = full.low.high.dead.live, aes(x = mort.number, ymin = lowAGB.dead, ymax = hiAGB.dead), fill = "brown")+facet_wrap(~rcp, ncol = 4)+ 
  scale_x_continuous(breaks = 1:3, labels = c("- 10%", "tuned to historic", "+ 10%"))




sens.C.stoage.mort <- ggplot() + geom_ribbon(data = full.low.high.dead.live, aes(x = mort.number, ymin = lowA, ymax = upA, fill = 'Aboveground live tree C'))+
  geom_ribbon(data = full.low.high.dead.live, aes(x = mort.number, ymin = lowdeadAGBtot, ymax = highdeadAGBtot, fill = 'Standing dead tree C'))+facet_wrap(~rcp, ncol = 4)+ 
  scale_x_continuous(breaks = 1:3, labels = c("- 10%", "tuned to historic", "+ 10%"))+ylab("Total Aboveground C storage (Tg C)")+xlab("% change in tree-level mortality probability")+
  theme_bw(base_size = 14)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+scale_fill_manual(name='C pool',
                                                                                                                                     #breaks=c('Linear', 'Quadratic', 'Cubic'),
                                                                                                                                     values=c('Aboveground live tree C'='forestgreen', 
                                                                                                                                              'Standing dead tree C'='brown'))
sens.C.stoage.mort
ggsave(height = 4, width = 8, units = "in", "outputs/SENS_C_STORAGE_2098.PNG")

full.low.high.dead.live$mid.dead <- rowMeans(full.low.high.dead.live[,c("lowAGB.dead", "hiAGB.dead")])


full.low.high.dead.live$mAGBtot <- full.low.high.dead.live$mAGB + full.low.high.dead.live$mid.dead

low.high.dead <- full.low.high.dead.live %>% select(rcp, year, scaled.mortality, lowAGB.dead, hiAGB.dead, mAGB.dead)
low.high.live <- full.low.high.dead.live %>% select(rcp, year, scaled.mortality, lowA, upA, mAGB)

low.high.dead$`Carbon Pool` <- "Standing Dead C"
low.high.live$`Carbon Pool` <- "Live Tree C"
colnames(low.high.dead)[5:7] <- c("ci.low",  "ci.hi", "mAGB")
colnames(low.high.live)[5:7] <- c("ci.low",  "ci.hi", "mAGB")

low.high.Cpools <- rbind( low.high.live, low.high.dead)
low.high.Cpools$scaled.mortality <- factor(low.high.Cpools$scaled.mortality, levels = c("- 10%",  "tuned to historic", "+ 10%"))

totalC.barplot <- ggplot(data = low.high.Cpools, aes(x = interaction(rcp, scaled.mortality), y = mAGB, group = rcp, fill = `Carbon Pool`), alpha = 0.5)+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7)+
  scale_fill_manual(values = c("Live Tree C"= "#018571", "Standing Dead C" = "#a6611a"))+
   annotate("text", x = 1:12, y =  10,
            label = rep(c("rcp 2.6", "rcp 4.5", 
                          "rcp 6.0", "rcp 8.5"), 3), 
            angle = 90) +
  annotate("text", c(2.5, 6.5, 10.5), y = - 5, label = c("- 10%", "tuned to historic", "+ 10%"))+
  geom_vline(aes(xintercept = c(4.5)))+
  geom_vline(aes(xintercept = c(8.5)))+
  ylab("Mean Total Aboveground C (Tg)")+
  theme_bw(base_size = 12)+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank())
  
totalC.barplot 

ggsave(height = 4, width = 6.5, units = "in", "outputs/mean_total_C_live_dead_mort_sens.png")

library(cowplot)
png(height = 8, width = 10, units = "in", res = 200, "outputs/combined_mortality_figures.png")
cowplot::plot_grid(mort.total.rcp+theme(axis.title.x = element_blank()),
                   plot_grid(mort.CC.pct.bar + theme(legend.position= "none")+ylab("% Mortality atttributed \n to climate change"), totalC.barplot, align = "h", ncol = 2, rel_widths = c(0.9, 1.5), 
                             labels = c("B", "C")), labels  = c("A", ""), ncol = 1)

dev.off()
# geom_segment(data = dead.tree.sensitivity, aes(x = rcp, xend = rcp, y = lowAGB.dead, 
#                                                yend = hiAGB.dead, color = variable),  size = 5)
# 
# ggplot() + geom_segment(data = full.low.high.dead.live, aes(x = mort.number, xend = mort.number, y = lowA, yend = upA, group = rcp, color = rcp), color = "forestgreen", size = 5)#+
#   geom_ribbon(data = full.low.high.dead.live, aes(x = mort.number, ymin = lowdeadAGBtot, ymax = highdeadAGBtot), fill = "brown")+facet_wrap(~rcp, ncol = 4)+ 
#   scale_x_continuous(breaks = 1:3, labels = c("- 10%", "tuned to historic", "+ 10%"))+ylab("Total C storage (Tg C)")+xlab("% change in tree-level mortality probability")+
#   theme_bw(base_size = 14)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# ggplot() + geom_point(data = full.low.high.dead.live, aes(x = scaled.mortality, y = upA), color = "forestgreen")+
#   geom_point(data = full.low.high.dead.live, aes(x = scaled.mortality, y =  hiAGB.dead), fill = "brown")+facet_wrap(~rcp)
# 
