library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(here)
library(boot) # for the inv.logit function in R/biomass.sensitivity.periodic.R

# make sure you run Format_TR_data_local.R first

# code to just apply the posterior estimates of the model to the additional trees on these plots (and walk forward)
# some functions from Mike Dietze
parse.MatrixNames <- function(w, pre = "x", numeric = FALSE) {
  w <- sub(pre, "", w)
  w <- sub("[", "", w, fixed = TRUE)
  w <- sub("]", "", w, fixed = TRUE)
  w <- matrix(unlist(strsplit(w, ",")), nrow = length(w), byrow = TRUE)
  if (numeric) {
    class(w) <- "numeric"
  }
  colnames(w) <- c("row", "col")
  return(as.data.frame(w))
} #

#--------------------------------------------------------------------------------------------- 
# Read in the site-tree/tree core data (from the jags model)
#--------------------------------------------------------------------------------------------- 
jags.data <- readRDS("data/regional_pipo_jags_formatted_data.RDS") # see format_PIPO_TR_climate_data.R for the formatting script
cov.data.regional <- jags.data$cov.data.regional
#--------------------------------------------------------------------------------------------- 
# Read in the rest of the FIA tree data
#--------------------------------------------------------------------------------------------- 
#fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))
fiadb <- readRDS("data/InWeUS_FIAdb.rds")
PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE
#TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE$INVYR)
unique(fiadb$PLOT$STATECD)

# Match up the tree and plot data
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]
TREE$PREV_PLT_CN <- PLOT$PREV_PLT_CN[match(TREE$PLT_CN, PLOT$CN)]

#--------------------------------------------------------------------------------------------- 
# Subset by trees in our study plots
#--------------------------------------------------------------------------------------------- 

PLOTS <- PLOT %>% filter(CN %in% unique(cov.data.regional$PLT_CN))
TREEinPLOTS <- TREE %>% filter(PLT_CN %in% unique(cov.data.regional$PLT_CN) & !CN %in% cov.data.regional$TRE_CN)
length(TREEinPLOTS$CN)
# 17968 trees! 

# this gives live trees in PLOTS at the time of coring...
# previously we were filtering by AGENTCD? Not sure why but this eliminated some trees that
# had NA in AGENTCD
TREEinPLOTS <- TREE %>% filter(AGENTCD >= 0 & STATUSCD ==1 & DIA > 1) %>% filter(PLT_CN %in% unique(cov.data.regional$PLT_CN) & !CN %in% cov.data.regional$TRE_CN)
length(TREEinPLOTS$CN)
#TREEinPLOTS <- TREE %>% filter(STATUSCD ==1 & DIA > 1) %>% filter(PLT_CN %in% unique(cov.data.regional$PLT_CN) & !CN %in% cov.data.regional$TRE_CN)
#length(TREEinPLOTS$CN)


# see how many of the pipo plots are mostly pipo
tp.ratio <- TREEinPLOTS %>% group_by(PLT_CN, SPCD == 122) %>% summarise(n()) %>% spread(`SPCD == 122`, `n()`) %>% mutate(PIPO = ifelse(is.na(`TRUE`), 0, `TRUE`), 
                                                                                                                         NonPIPO = ifelse(is.na(`FALSE`), 0, `FALSE`))%>% mutate(PIPO.ratio = PIPO/(PIPO +NonPIPO))%>% filter(PIPO.ratio > 0.60 )
hist(tp.ratio$PIPO.ratio)
saveRDS(tp.ratio, "outputs/PIPO_nonPIPO_plotratios.rds")

TREEinPLOTS %>% dplyr::select(DESIGNCD) %>% distinct()
TPA.designcd.table <- data.frame(DESIGNCD = c(1, 410, 411, 413, 424, 425, 423, 412), #unique(TREEinPLOTS$DESIGNCD),
                                 RADIUS = c("24 ft", 
                                            "Variable", 
                                            "Variable", 
                                            "Variable", 
                                            "Fixed", 
                                            "Fixed", 
                                            "Fixed",
                                            "Variable" ), 
                                 BAF = c(NA, 
                                         40, 
                                         40, 
                                         20,
                                         NA, 
                                         NA, 
                                         NA,
                                         40), 
                                 N.subplots.points = c(4, 
                                                       7,#  maybe 5?
                                                       10, 
                                                       5, 
                                                       4, 
                                                       4, 
                                                       4, 
                                                       5), 
                                 N.microplots = c(4, 
                                                  7, 
                                                  3, 
                                                  5, 
                                                  4,
                                                  4,
                                                  4, 
                                                  3),
                                 MicropplotRadius = c("6.8 ft", 
                                                      "1/300th acre",
                                                      "1/300th acre", 
                                                      "1/300th acre", 
                                                      "1/300th acre", 
                                                      "1/300th acre", 
                                                      "1/300th acre",
                                                      "1/300th acre"), 
                                 TPA.eq = c("1/(N*A)", 
                                            "(BAF/0.005454*DIA^2)/N", 
                                            "(BAF/0.005454*DIA^2)/N",
                                            "(BAF/0.005454*DIA^2)/N",
                                            "1/(N*A)",
                                            "1/(N*A)",
                                            "1/(N*A)",
                                            "(BAF/0.005454*DIA^2)/N"), 
                                 AREA.acr = c(0.0415172, 
                                              NA, 
                                              NA, 
                                              NA, 
                                              0.05,
                                              0.2,
                                              0.1,
                                              NA), 
                                 microplot.AREA.acr = c(0.003334877, 
                                                        NA, 
                                                        NA, 
                                                        NA, 
                                                        0.003333333, 
                                                        0.003333333, 
                                                        0.003333333, 
                                                        NA) )



additional.trees <- TREEinPLOTS %>% group_by(SPCD) %>% summarise(number = n())

png(height = 3, width = 10, units = "in", res = 150, "outputs/barplot_additional_trees.png")
ggplot(additional.trees, aes(x = as.character(SPCD), y = number))+geom_bar(stat = "identity")
dev.off()
# most are PIPO, but second most are gambel oak QUGA, then PSME


additional.trees.by.plot <- TREEinPLOTS %>% group_by(PLT_CN, SPCD) %>% summarise(number = n())
unique(TREEinPLOTS$MEASYR)

match.plots <- unique(TREEinPLOTS$PLT_CN)
# for later use: get the next plots to validate with:
NEWTREEinPLOTS <- TREE %>% filter(PREV_PLT_CN %in% match.plots )
length(NEWTREEinPLOTS$PLT_CN)

# note that we can't match these plots because it is part of the crosswalk that we dont have

#--------------------------------------------------------------------------------------------- 
# get X values from current trees
#--------------------------------------------------------------------------------------------- 
# for now lets just apply the pipo model to all (unrealistic)

dbh.measyr.newtrees <- TREEinPLOTS %>% dplyr::select(CN, PLT_CN, SUBP,MEASYR, DIA) %>% mutate(DIA = DIA*2.54)

# get xdata:

year.df <- expand.grid(Year = 1966:2018, CN = unique(dbh.measyr.newtrees$CN))
cn.df <- unique(dbh.measyr.newtrees[,c("CN", "PLT_CN")])

year.cn.df <- left_join(year.df, cn.df)

dbh.measyr.repeats.full <- left_join(year.cn.df, dbh.measyr.newtrees)

spread.dbh.mat <- dbh.measyr.repeats.full %>% 
  mutate(DIA_measyr = ifelse(Year == MEASYR, DIA, NA)) %>% 
  dplyr::select(-DIA, -MEASYR)  %>% 
  group_by(CN, PLT_CN) %>% tidyr::spread(Year, DIA_measyr)

#spread.dbh.mat <- spread.dbh.mat[!duplicated(spread.dbh.mat$CORE_CN),]

# combine cov.data.regional

head(cov.data.regional)
# can probably index cov.data.regional with the PLT_CN and plotid

plotid.df <- data.frame(PLOTSTATE = unique(cov.data.regional$PLOTSTATE),
                        plotid = 1:length(unique(cov.data.regional$PLOTSTATE)))


cov.data.regional <- left_join(cov.data.regional, plotid.df, by = "PLOTSTATE")
unique.plts <- unique(cov.data.regional[,c("PLT_CN","plotid", "PLOTSTATE", "MAP", "MAT")])
#unique.trees <- unique(cov.data.regional[,c("TRE_CN","PLT_CN","plotid","treeid", "PLOTSTATE", "MAP", "MAT")])


# create a matrix of x values of additonal trees on the plot
x.mat <- merge(unique.plts, spread.dbh.mat, by.x = c("PLT_CN"))
nrow(x.mat)
#m <- 1
# get time series data:
# read in the larger region climate data:
pipo.clim <- read.csv("data/pipo_all_tmean_ppt_v5.csv")

colnames(pipo.clim)

pipo.clim.one <- pipo.clim %>% filter(year %in% 1950)

#ggplot(pipo.clim.one, aes(lon, lat))+geom_point()

pipo.clim$wintP.NovAug <- rowSums(pipo.clim[,c("PPT_8", "PPT_9", "PPT_10", "PPT_11")])
pipo.clim$wintP.wateryr <- rowSums(pipo.clim[,c("PPT_1", "PPT_2", "PPT_3", "PPT_4", "PPT_5", "PPT_6", "PPT_7", "PPT_8", "PPT_9", "PPT_10", "PPT_11", "PPT_12")])
pipo.clim$wintP.NM <- rowSums(pipo.clim[,c("PPT_11", "PPT_12", "PPT_1", "PPT_2", "PPT_3")])
pipo.clim$wintP.JJ <- rowSums(pipo.clim[,c("PPT_6", "PPT_7")])

pipo.clim$tmax.fallspr <- rowMeans(pipo.clim[,c( "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12", "TMAX_1", "TMAX_2", "TMAX_3",  "TMAX_4")])
pipo.clim$tmax.JanA <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8")])
pipo.clim$tmax.MJul <- rowMeans(pipo.clim[,c("TMAX_5", "TMAX_6", "TMAX_7")])
pipo.clim$tmax.AprMayJun <- rowMeans(pipo.clim[,c("TMAX_4","TMAX_5", "TMAX_6")])
pipo.clim$tmax.fall <- rowMeans(pipo.clim[,c("TMAX_9","TMAX_10","TMAX_11")])
pipo.clim$tmax.monsoon <- rowMeans(pipo.clim[,c("TMAX_7","TMAX_8", "TMAX_9")])
pipo.clim$TMAX <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8", "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12")])
pipo.clim$TMEAN <- rowMeans(pipo.clim[,c("TMEAN_1", "TMEAN_2", "TMEAN_3", "TMEAN_4", "TMEAN_5", "TMEAN_6", "TMEAN_7", "TMEAN_8", "TMEAN_9", "TMEAN_10", "TMEAN_11", "TMEAN_12")])

require(dplyr)
pipo.clim %>% dplyr::select(lon, lat, name, year, PPT_1, PPT_2, PPT_3, PPT_4, PPT_5, PPT_6, PPT_7, PPT_8, PPT_9, PPT_10, PPT_11, PPT_12,
                            TMAX_1, TMAX_2, TMAX_3, TMAX_4, TMAX_5, TMAX_6, TMAX_7, TMAX_8, TMAX_9, TMAX_10, TMAX_11, TMAX_12,
                            wintP.NovAug , 
                            wintP.wateryr , wintP.NM ,    wintP.JJ,       tmax.fallspr  , tmax.JanA,     
                            tmax.MJul    ,  tmax.AprMayJun ,tmax.fall,      tmax.monsoon,   TMAX, TMEAN   )

# will need to match the core_CN from the plot
pipo.climate.pltcn <- merge(pipo.clim, cov.data.regional[,c("CORE_CN", "PLT_CN", "STATECD", "COUNTYCD", "plotid", "PLOTSTATE")],by.x = "name", by.y = "CORE_CN")

#-------------------------------------------------------------------------------------
# Read in the climate data
#-------------------------------------------------------------------------------------


get_ordered_climate <- function(x){
  
  clim.subset <- unique(pipo.climate.pltcn[,c("PLT_CN", "lat","lon","year", x)])
  spread.tmax.fall.spr.mat <-  clim.subset  %>% filter(PLT_CN %in% spread.dbh.mat$PLT_CN)%>% 
    dplyr::select(lon, lat, PLT_CN, year, x) %>% 
    group_by(lon, lat, PLT_CN) %>%  tidyr::spread( year, x, drop = T)
  
  
  # now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
  spread.tmax.fall.spr.mat.ordered <-  spread.tmax.fall.spr.mat[order(match(spread.tmax.fall.spr.mat$PLT_CN, spread.dbh.mat[,"PLT_CN"])),]
  climate.mat <- spread.tmax.fall.spr.mat.ordered#[,4:length(spread.tmax.fall.spr.mat.ordered)]
  climate.mat
}

#tmax.fallspr <- get_ordered_climate("Tmax_fallspr")
wintP.wateryr <- get_ordered_climate("wintP.wateryr")
tmax.AprMayJun <- get_ordered_climate("Tmax_AprMayJun")
#tmax.monsoon <- get_ordered_climate("tmax.monsoon")
TMAX <- get_ordered_climate("TMAX")
TMEAN <- get_ordered_climate("TMEAN")
# -------------------------------------------------------------------------------
# read in the SDI time plot level data:
#--------------------------------------------------------------------------------
tv.sdi <- readRDS("data/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v4.RDS")

# get the tv.sdi estimates from just the x.mat plots and subplots?
# reshape tv.sdi:
tv.sdi.spread <- tv.sdi %>% dplyr::select(STATECD, PLOT,SUBP,PLT_CN, Year,SDIs) %>%group_by(STATECD, PLOT,SUBP,PLT_CN) %>% spread(Year, SDIs) %>% dplyr::select(`1966`:`2001`)

SDI.mat.PLT.subp <- left_join(unique(x.mat[,c("PLT_CN", "SUBP")]),tv.sdi.spread)


# Fill in the timeseires:
SDI.mat.PLT.subp
SDI.matrix.plt.subp <- SDI.mat.PLT.subp[,5:length(SDI.mat.PLT.subp)]

# because jags won't take NA values as predictors, we need to give values..so replace NA with the max or min
for(i in 1:nrow(SDI.matrix.plt.subp)){
  for(t in 2:length(SDI.matrix.plt.subp)){
    
    if(is.na(SDI.matrix.plt.subp[i,t])){
      SDI.matrix.plt.subp[i,t] <- SDI.matrix.plt.subp[i,t-1]
    }
    
    if(is.na(SDI.matrix.plt.subp[i,t])){
      SDI.matrix.plt.subp[i,t] <- min(SDI.matrix.plt.subp[i,], na.rm = TRUE)
    }
  }
  # if it is still NA, replace with the smallest SDI value in that row:
  
}


summary(SDI.matrix.plt.subp)
SDI.matrix.plt.subp


# relink to the rest of the dataset:

SDI.mat.PLT.subp[,5:length(SDI.mat.PLT.subp)] <- SDI.matrix.plt.subp



# combine the climate & SDI time data for each plot and standardize:
standardize.mat <- function(x){
  scaled.data <- matrix(NA,nrow = nrow(x), ncol = ncol(x) )
  
  for(i in 1:length(x[,1])){
    x.bar <- mean(as.matrix(x[i,]), na.rm = TRUE)
    s.d. <- sd(as.matrix(x[i,]), na.rm = TRUE)
    scaled.data[i,]<- (x[i,]-x.bar)/s.d.
  }
  
  scaled.data
}

standardize.vector <- function(x){
  x.2 <- apply(x, 2, as.numeric)
  x.bar <- mean(as.vector(x.2), na.rm = TRUE)
  s.d. <- sd(x.2, na.rm = TRUE)
  return((x-x.bar)/s.d.)
}

#time_data$TMAX.scaled <- standardize.mat(TMAX)

#time_data$tmax.fallspr.scaled <- standardize.mat(time_data$tmax.fallspr)
wateryrscaled <- wintP.wateryr
tmaxAprMayJunscaled <- tmax.AprMayJun
SDIscaled <- SDI.mat.PLT.subp # note this is not ordered (it might be but i havent checked)

wateryrscaled[,4:ncol(wateryrscaled)] <- standardize.mat(as.matrix(wintP.wateryr[,4:ncol(wintP.wateryr)]))
tmaxAprMayJunscaled[,4:ncol(tmaxAprMayJunscaled)] <- standardize.mat(as.matrix(tmax.AprMayJun[,4:ncol(tmax.AprMayJun)]))
test <- as.numeric(as.matrix(SDIscaled[,5:ncol(SDIscaled)]))

# Convert the matrix to numeric, forcing non-numeric values to NA
numeric_matrix <- apply(SDIscaled[,5:ncol(SDIscaled)], 2, as.numeric)

SDIscaled[,5:ncol(SDIscaled)] <- standardize.vector(SDI.mat.PLT.subp[,5:ncol(SDI.mat.PLT.subp)])
#SDIscaled <- data.frame(time_data$SDIscaled)
#SDIscaled$PLT_CN <- data$cov.data.regional$PLT_CN
#SDIscaled$SUBP <- data$cov.data.regional$SUBP
#--------------------------------------------------------------------------------------------- 
# Read in the posterior parameter estimates
#--------------------------------------------------------------------------------------------- 
# this model models increment, not diameter...

# READ IN STAN OUTPUT SUMMARY
#STAN.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect
model.params <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/model6.1500.betas.rds")
colnames(model.params)[1] <- "L1"

# read in alphas
alphas <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/model6.1500.alpha_TREES.rds")
colnames(alphas)[1] <- "L1"

output.base.name <- "Regional_model_6"

sigmas <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/model6.1500.sigmas.rds")

mus <- model.params %>% filter(L1 %in% c("mutree","sigma_TREE"))
betas <- model.params %>% filter(!L1%in% c("mutree","sigma_TREE"))
# get year and tree random effects from STAN model
sigma.INC <- sigmas %>% filter(variable %in% "sigma_inc")
sigma.DBH <- sigmas %>% filter(variable %in% "sigma_dbh")

iterate_statespace.inc <- function( x = x.mat[,"x[1,36]"],  betas.all, alpha = 0, beta_YEARid,  SDdbh = 0, SDinc = sigma.INC$median, covariates) {
  
  xscaled <- (x - 31.63)/10.61
  
  
  # pseudocode for now
  tree.growth <- alpha + #beta_YEARid +# sampled from tree level alpha randome effect
    # normal fixed effects
    betas.all$bMAP*covariates$MAP + 
    betas.all$bMAT*covariates$MAT +
    
    # size and SDI fixed
    betas.all$bSDI*covariates$SDI + 
    betas.all$bX*(xscaled) + 
    
    # climate fixed effects
    betas.all$bppt*covariates$ppt + 
    betas.all$btmax*covariates$tmax + 
    
    # MAP interactions
    betas.all$bMAP_MAT*covariates$MAP*covariates$MAT +
    #betas.all$bMAP_SDI*covariates$MAP*covariates$SDI +
    
    betas.all$bMAP_tmax*covariates$MAP*covariates$tmax +
    betas.all$bMAP_ppt*covariates$MAP*covariates$ppt +
    
    # MAT interactions
    #betas.all$bMAT_SDI*covariates$MAT*covariates$SDI+
    betas.all$bMAT_tmax*covariates$MAT*covariates$tmax +
    
    betas.all$bMAT_ppt*covariates$MAT*covariates$ppt +
    
    
    # tmax and precip interactions
    betas.all$btmax_ppt*covariates$tmax*covariates$ppt +
    
    # SDI interactions
    betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
    betas.all$bSDI_ppt*covariates$SDI*covariates$ppt+  
    
    # X interactions
    betas.all$bX_MAP*covariates$MAP*xscaled + 
    betas.all$bX_MAT*covariates$MAT*xscaled + 
    betas.all$bX_ppt*covariates$ppt*xscaled + 
    betas.all$bX_tmax*covariates$tmax*xscaled + 
    betas.all$bX_SDI*covariates$SDI*xscaled 
  
  treegrowth <- rlnorm(n = length(x), tree.growth, SDinc)
  
  treegrowth  <-  ifelse( treegrowth  < 0.001, 0,  treegrowth ) # Assign tree growth to 0 if its below measurable grwoth
  treegrowth  <-  ifelse( treegrowth  >= 2, 2,  treegrowth ) # we shouldn't need this but keeping in
  
  # Stochastic process model
  #incpred <- treegrowth
  
  xpred <- rnorm(length(treegrowth), (treegrowth + x), SDdbh)
  
  
  xpred
  
}
iterate_statespace.incpred <- function( x = x.mat[,"x[1,36]"],  betas.all, alpha = 0, beta_YEARid,  SDdbh = 0, SDinc = sigma.INC$median, covariates) {
  
  xscaled <- (x - 31.63)/10.61
  
  
  tree.growth <- alpha + #beta_YEARid +# sampled from tree level alpha randome effect
    # normal fixed effects
    betas.all$bMAP*covariates$MAP + 
    betas.all$bMAT*covariates$MAT +
    
    # size and SDI fixed
    betas.all$bSDI*covariates$SDI + 
    betas.all$bX*(xscaled) + 
    
    # climate fixed effects
    betas.all$bppt*covariates$ppt + 
    betas.all$btmax*covariates$tmax + 
    
    # MAP interactions
    betas.all$bMAP_MAT*covariates$MAP*covariates$MAT +
    #betas.all$bMAP_SDI*covariates$MAP*covariates$SDI +
    
    betas.all$bMAP_tmax*covariates$MAP*covariates$tmax +
    betas.all$bMAP_ppt*covariates$MAP*covariates$ppt +
    
    # MAT interactions
    #betas.all$bMAT_SDI*covariates$MAT*covariates$SDI+
    betas.all$bMAT_tmax*covariates$MAT*covariates$tmax +
    
    betas.all$bMAT_ppt*covariates$MAT*covariates$ppt +
    
    
    # tmax and precip interactions
    betas.all$btmax_ppt*covariates$tmax*covariates$ppt +
    
    # SDI interactions
    betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
    betas.all$bSDI_ppt*covariates$SDI*covariates$ppt+  
    
    # X interactions
    betas.all$bX_MAP*covariates$MAP*xscaled + 
    betas.all$bX_MAT*covariates$MAT*xscaled + 
    betas.all$bX_ppt*covariates$ppt*xscaled + 
    betas.all$bX_tmax*covariates$tmax*xscaled + 
    betas.all$bX_SDI*covariates$SDI*xscaled
  
  # Stochastic process model
  treegrowth <- rlnorm(n = length(x), tree.growth, SDinc)
  
  treegrowth  <-  ifelse( treegrowth  <= 0.02, 0,  treegrowth ) # Assign tree growth to 0 if its below measurable grwoth
  treegrowth  <-  ifelse( treegrowth  >= 2, 2,  treegrowth ) # we shouldn't need this but keeping in
  
  
  #xpred
  treegrowth
}


cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)

simulate.xvals.from.model.oos <- function(m, nsamps = 100){
  # use the forecast function to forecast forward:
  
  treeids <- cov.data.regional %>% filter(plotid %in% x.mat[m,]$plotid) %>% dplyr::select(treeid)
  if(length(treeids$treeid)>1){
    
    alphatreeid <- vector()
    
    for(i in 1:length(treeids$treeid)){
      alphatreeid[i]<- paste0("alpha_TREE[", treeids[i,], "]")
      
    }
  }else{
    # 
    # sample 
    alphatreeid <- paste0("alpha_TREE[", treeids, "]")
  }
  #model.covs <- substring(colnames(betas), 5)
  
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  #alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  
  # write a function to get the MCMC samples
  
  get_mcmc_samples <- function(x, betas, nsamps){
    
    rnorm(nsamps, mean = as.numeric(betas %>% filter(L1 %in% x) %>% dplyr::select(median)), sd =  as.numeric(betas %>% filter(L1 %in% x) %>% dplyr::select(sd)))
  }
  
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  
  
  # sample from the population mean (mu) for the trees that dont have RE
  
  alpha <- get_mcmc_samples(x = "mutree", betas = mus, nsamps = nsamps)
  
  if(length(alphatreeid)>1){
    
    treealphas <- lapply(alphatreeid, get_mcmc_samples, betas = alphas, nsamps = nsamps)
    treealphas <- do.call(cbind, treealphas)
    colnames(treealphas)<- alphatreeid
    treealphas <- rowMeans(treealphas)
    
  }else{
    treealphas <- get_mcmc_samples(x = alphatreeid, betas = alphas , nsamps = nsamps)
  }
  # 
  
  bMAP <- get_mcmc_samples("betaMAP", betas = betas, nsamps = nsamps)
  bMAT <- get_mcmc_samples("betaMAT", betas = betas, nsamps = nsamps)
  bMAP_MAT <- get_mcmc_samples("betaMAP_MAT", betas = betas, nsamps = nsamps)
  
  bSDI <- get_mcmc_samples("betaSDI", betas = betas, nsamps = nsamps)
  bSDI_ppt <- get_mcmc_samples("betaPrecip_SDI", betas = betas, nsamps = nsamps)
  bSDI_tmax <- get_mcmc_samples("betaTmax_SDI", betas = betas, nsamps = nsamps)
  
  
  
  #MAP interactions:
  bMAP_ppt <- get_mcmc_samples("betaPrecip_MAP", betas = betas, nsamps = nsamps)
  bMAP_tmax <- get_mcmc_samples("betaTmax_MAP", betas = betas, nsamps = nsamps)
  #bMAP_SDI <- get_mcmc_samples("betaSDI_MAP", betas = betas, nsamps = nsamps)
  
  #MAT interactions:
  bMAT_ppt <- get_mcmc_samples("betaPrecip_MAT", betas = betas, nsamps = nsamps)
  bMAT_tmax <- get_mcmc_samples("betaTmax_MAT", betas = betas, nsamps = nsamps)
  #bMAT_SDI <- get_mcmc_samples("betaSDI_MAT", betas = betas, nsamps = nsamps)
  
  
  bX <-  get_mcmc_samples("betaX", betas = betas, nsamps = nsamps)
  
  
  bppt <- get_mcmc_samples("betaPrecip", betas = betas, nsamps = nsamps)
  btmax <- get_mcmc_samples("betaTmax", betas = betas, nsamps = nsamps)
  btmax_ppt <- get_mcmc_samples("betaPrecip_Tmax", betas = betas, nsamps = nsamps)
  
  # X interactions:
  bX_ppt <- get_mcmc_samples("betaX_Precip", betas = betas, nsamps = nsamps)
  bX_tmax <- get_mcmc_samples("betaX_Tmax", betas = betas, nsamps = nsamps)
  bX_MAP<- get_mcmc_samples("betaX_MAP", betas = betas, nsamps = nsamps)
  bX_MAT <- get_mcmc_samples("betaX_MAT", betas = betas, nsamps = nsamps)
  bX_SDI <- get_mcmc_samples("betaX_SDI", betas = betas, nsamps = nsamps)
  
  
  betas.all <- data.frame(  treealphas ,
                            bMAP,
                            bMAT ,
                            bMAP_MAT,
                            bSDI ,
                            bSDI_ppt,
                            bSDI_tmax,
                            
                            #MAP interactions:
                            bMAP_ppt,
                            bMAP_tmax,
                            #bMAP_SDI,
                            #MAT interactions:
                            bMAT_ppt,
                            bMAT_tmax,
                            #bMAT_SDI,
                            
                            bX,
                            bppt,
                            btmax,
                            btmax_ppt, 
                            
                            bX_MAP, 
                            bX_MAT, 
                            bX_tmax, 
                            bX_ppt, 
                            bX_SDI)
  
  
  
  # get PLT_CN
  PLT_CNint <- as.character(x.mat[m,]$PLT_CN)
  SUBPPLT_CNint <- as.character(x.mat[m,]$SUBP)
  #wateryrscaled 
  
  ppt <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2018`)
  
  tmax<- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2018`)
  
  SDI <- SDIscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint & SUBP %in% SUBPPLT_CNint) %>% dplyr::select(`1966`:`2001`)
  
  MAP <- x.mat[m,]$MAP
  MAT <- x.mat[m,]$MAT
  # 
  # if(length(SDI) <=36){
  #   newsdivals <- rep(SDI[36], 17)
  #   names(newsdivals)  <- 2002:2018
  #   SDI.new <- cbind(SDI, newsdivals)
  # }
  # #SICOND <- cov.data[m, ]$SICOND
  #}
  
  covs <- list()
  covs$SDI <- as.matrix(SDI)
  covs$ppt <- as.matrix(ppt)
  covs$tmax <- as.matrix(tmax)
  covs$MAP <- MAP
  covs$MAT <- MAT
  
  time_steps <- length(1966:2001)
  nMCMC <- max(length(betas.all$bSDI), length(x.mat[m,1]))
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  # Make the time series forecasts
  for(t in 1:time_steps){
    if(t < which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if t is less than the measureyr assign NA (fo now)
      dbh.pred <- rep(NA, nMCMC)
      forecast[,t] <- dbh.pred
      inc[,t] <- dbh.pred
    }else{
      if(t == which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if the time step is the measuryr use the measureed DBH
        inc[,t] <- iterate_statespace.incpred(x = x.mat[m,7+t],  betas.all = betas.all, alpha = betas.all$treealphas, SDinc = sigma.INC$median, covariates = data.frame(SDI = covs$SDI[t], 
                                                                                                                                                                        ppt = covs$ppt[t], 
                                                                                                                                                                        tmax = covs$tmax[t], 
                                                                                                                                                                        MAP = covs$MAP,
                                                                                                                                                                        MAT = covs$MAT))
        forecast[,t-1] <- x.mat[m,7+t]
        forecast[,t]<- x.mat[m,7+t]+inc[,t]
        
      }else{
        inc[,t]<- iterate_statespace.incpred(x = forecast[,t-1], betas.all = betas.all, alpha = betas.all$treealphas, SDinc =  sigma.INC$median, covariates = data.frame(SDI = covs$SDI[t], 
                                                                                                                                                                         ppt = covs$ppt[t], 
                                                                                                                                                                         tmax = covs$tmax[t], 
                                                                                                                                                                         MAP = covs$MAP,
                                                                                                                                                                         MAT = covs$MAT))
        forecast[,t] <- forecast[,t-1] + inc[,t]
        
        
      }  }
  }
  
  forecast.med <- apply(forecast, 2, function(x){quantile(x, c( 0.5), na.rm = TRUE)})
  inc.med <- apply(inc, 2, function(x){quantile(x, c( 0.5), na.rm = TRUE)})
  
  colnames(forecast) <- paste0("x[", m,",", 1:36,"]")
  cat(paste("\n", m))
  
  saveRDS(forecast, paste0("xvals_additional_trees_stan/Xvals_tree_",m,".RDS"))
  # forecast.med
  forecast
  
}


#x <- 4203
simulate.xvals.from.model.oos(m = 260, nsamps = 100)

# see how long this will take:
system.time(sims.x.forecast <- lapply(1:20, simulate.xvals.from.model.oos))
#3.8 user time multiplied by ~1500 =95 mintues 

if(file.exists(paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))){
  x.mat2 <- readRDS(paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))
}else{
  sims.x.forecast <- lapply(1:length(unique(x.mat$CN)), simulate.xvals.from.model.oos)
  x.mat2 <- do.call(cbind, sims.x.forecast)
  
  saveRDS(x.mat2, paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))
}
#x.mat2 <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/mortality_future_sensitivity-2022-09-22-21-03-44.7/Xval_noncored.Regional_incifelse_T0.RDS"))

#TREE %>% filter(PREV_TRE_CN %in% as.character(x.mat$CN)) # %in% as.character(validate.diam$TRE_CN)

#--------------------------------------------------------------------------------------------- 
# forecast all trees on the plot from posterior estimates to get X values for 2001-2018, changing SDI values along the way
#--------------------------------------------------------------------------------------------- 

all.trees.in.the.plots <- TREE %>% filter(PLT_CN %in% unique(x.mat$PLT_CN))%>%
  filter( STATUSCD ==1 & DIA > 1) 
length(TREEinPLOTS$CN)

# see how many of the pipo plots are mostly pipo
tp.ratio <- all.trees.in.the.plots %>% group_by(PLT_CN, SPCD == 122) %>% 
  summarise(n()) %>% 
  spread(`SPCD == 122`, `n()`) %>% 
  mutate(PIPO = ifelse(is.na(`TRUE`), 0, `TRUE`), 
         NonPIPO = ifelse(is.na(`FALSE`), 0, `FALSE`)) %>% 
  mutate(PIPO.ratio = PIPO/(PIPO + NonPIPO)) %>% 
  filter(PIPO.ratio > 0.60 )


x.mat2 <- readRDS(paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))

# get the estimated x values for each tree/plot (need to calculate SDI and make forecasts from 2001-2018)
#out.cored <- as.matrix(readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/increment_ifelse_constraint-2022-07-20-15-30-47.9/Xvals_Regional_incifelse_T0.RDS")))
out.cored <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/xcored_model_6_estimates.rds")
#head(out.cored)
#out.cored <- as.matrix(x.mat2)
x.ci      <- apply(out.cored[3000:4500,]  , 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
#    <- as.matrix(Xests) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out.cored), 1, 1) == "x") # grab the state variable columns
# 
# # generate 95% CI of the DBH
ci      <- apply(out.cored[3000:4500, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
mean.pred.cored       <- apply(out.cored[3000:4500, x.cols], 2, mean) # get the var.pred for the last 800 samples
# 
# 
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)



# join the estimated with the forecasts
#out.noncored <- as.matrix(readRDS("data/output/Xvals_noncored_IGFRegional_mvnmu_revCorr_xfixed.RDS"))
# 
# ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols.noncored   <- which(substr(colnames(x.mat2), 1, 1) == "x") # grab the state variable columns
# 
# # generate 95% CI of the DBH
ci.noncored      <- apply(x.mat2[, x.cols.noncored], 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
# #mean.pred.noncored       <- apply(out.noncored[, x.cols.noncored], 2, var) # get the var.pred for the last 800 samples
# #use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
ci.names.noncored <- parse.MatrixNames(colnames(ci.noncored), numeric = TRUE)



#------------------------- match up with the plot information:

# make sure we have tree ids for the noncored trees:
plots <- unique(x.mat$plotid)

saveRDS(x.mat,"outputs/x.mat.RDS")

# select the outdata for the cored and uncored trees:
sel.noncored <- which(ci.names.noncored$row %in% y)

cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)
plot <- cov.data.regional$PLT_CN[1]


all.noncored <- x.mat # x.mat from dbh.spread above
all.noncored$treeid <- 1:length(x.mat$PLT_CN)
plots <- as.character(unique(x.mat$PLT_CN))

# --------------------------------------------------------------------------------------------
# Check for any trees with validation from inventory to 2001
# --------------------------------------------------------------------------------------------
validate.diam <- read.delim("data/PIPOCore_TentativeMatch.csv", sep = ",")
validate.diam<- validate.diam  %>% mutate(DIA_T2 = DIA + INCR) %>% mutate(DIA_cm_T2 = DIA_T2*2.54)
nearTermDIA <- validate.diam %>% filter(CORE_CN %in% cov.data.regional$CORE_CN) %>% filter(MEASYEAR_T2 <= 2001)
farTermDIA <- validate.diam %>% filter(CORE_CN %in% cov.data.regional$CORE_CN) %>% filter(MEASYEAR_T2 > 2001) %>% mutate(DIA_T2 = DIA + INCR)
cov.data.regional$DIA_T2 <- validate.diam[match(cov.data.regional$CORE_CN, validate.diam$CORE_CN),]$DIA_T2
cov.data.regional$DIA_cm_T2 <- validate.diam[match(cov.data.regional$CORE_CN, validate.diam$CORE_CN),]$DIA_cm_T2

cov.data.regional$MEASYEAR_T2 <- validate.diam[match(cov.data.regional$CORE_CN, validate.diam$CORE_CN),]$MEASYEAR_T2

nearTermValidate <- cov.data.regional %>% filter(!is.na(DIA_cm_T2)) %>% filter(MEASYEAR_T2 <= 2001)
plt.num <- as.character(nearTermValidate$PLT_CN[1])

yr.col.df <- data.frame(col = 1:36, 
                        MEASYEAR_T2 = 1966:2001)

#validate.year <- yr.col.df %>% filter(MEASYEAR_T2%in% cored.in.plt$MEASYEAR_T2)

ci.melt <- reshape2::melt(ci, id.vars = rownames(ci))%>% spread(Var1, value)
ci.melt$treeid <- rep(1:1046, 36)
ci.melt$col <- rep(1:36, each = 1046)
ci.years <- left_join(ci.melt, yr.col.df) 
# get the matching melted trees

cov.data.validation <- left_join(cov.data.regional, ci.years)
ggplot()+geom_point(data = cov.data.validation, aes(x = DIA_cm_T2, y = `50%`))+
  geom_errorbar(data = cov.data.validation, aes(x = DIA_cm_T2, ymin = `2.5%`, ymax = `97.5%`))+
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed", color = "red")+theme_bw(base_size = 12)+
  ylab("Predicted held-out diameters (cm) 1990-2001")+
  xlab("Observed held-out diameters (cm)")
ggsave("outputs/validation/OOS_1990_2001_DIAMETERS.png")
saveRDS(cov.data.validation, "outputs/validation/OOS_cov_data_validation_1990_2001.RDS")


#--------------------------------------------------------------------------------------------- 
# forecast from posterior estimates to get X values for the future 
#--------------------------------------------------------------------------------------------- 


# now make a function where you calculate plot level biomass
# devtools::install_github("PecanProject/pecan",subdir="base/logger")
# devtools::install_github("PecanProject/pecan",subdir="modules/allometry")
# 
# 
# library(PEcAn.allometry)
#library(devtools)

# note that these functions are from the PEcAn.allometry package, but I saved them locally because pecan library stopped playing nice here

pfts = list(PIPO = data.frame(spcd=122,acronym='PIPO')) # list our "Pfts--plant functional types" of interest--really list the species
source("R/AllomAve.R")
source("R/read.allom.data.R")
source("R/query.allom.data.R")
source("R/allom.BayesFit.R")
# Run AllomAve for each component in Kaye
kaye_pipo = AllomAve(pfts, components = c(4, 5, 8, 12, 18), ngibbs = 1000,
                     parm = "data/kaye_pipo.csv")

# had to read in the kaye_pipo csv...should just upload to the data
kaye.parm <- read.csv("data/kaye_pipo.csv")


allom.stemwood = load("Allom.PIPO.4.Rdata")
allom.stembark = load("Allom.PIPO.5.Rdata")
allom.branchlive = load("Allom.PIPO.8.Rdata")
allom.branchdead = load("Allom.PIPO.12.Rdata")
allom.foliage = load("Allom.PIPO.18.Rdata")

dbh = 1:50 # vector of DBH values to predict over
#source("data/output/validation.time.dbh.changingsdi.zero.SDIscaled.R")
#validation.time.dbh.changingsdi.zeroinc.SDIscaled( plot = unique(plots)[6])
#lapply(unique(plots)[1:2], validation.time.dbh.changingsdi.zeroinc.SDIscaled)



# we need the list of trees (combined), 
# and the diameter estimates for all the trees:
plot = unique(plots)[22]

# plot = '2482066010690'
# density.dependent = TRUE
# density.independent = TRUE
# rcp <- "rcp26"

#-----------------------------------------------------------------------
# read in future climate, which has been mean corrected and downscaled:
#-----------------------------------------------------------------------

#full.clim <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/future_climate_extraction-2022-08-12-23-22-14.1/full_time_mean_corrected_CMIP5_model_timeseriesIW.RDS"))
full.clim <- readRDS(here("data/full_time_mean_corrected_CMIP5_model_timeseriesIW.RDS"))

#-----------------------------------------------------------------------
# need to scale future climate data on the same scale as the past climate
#-----------------------------------------------------------------------
clim.data <- time_data

# note that each plot is locally scaled, so we will need to apply that to each future timeseries
#x <- plot

library(data.table)

full.clim$ppt.scale <- NA
full.clim$tmax.scale <- NA


future.clim.subset.26 <- full.clim %>% filter(rcp %in% "rcp26")
future.clim.subset.45 <- full.clim %>% filter(rcp %in% "rcp45")
future.clim.subset.60 <- full.clim %>% filter(rcp %in% "rcp60")
future.clim.subset.85 <- full.clim %>% filter(rcp %in% "rcp85")
#future.clim.subset <- future.clim.subset.26 
# we will use this function to set up the future climate
scale.fut.clim.by.plt <- function(x, future.clim.subset){
  cat(x)
  full.clim.plt <-  future.clim.subset %>% filter(PLT_CN == x)#full.clim.dt[PLT_CN %in% plot]
  rowid <- which(cov.data.regional$PLT_CN %in%  x ) # get the row for the climate data
  full.clim.plt$ppt.scale <- ( full.clim.plt$ppt.corrected-mean(as.matrix(clim.data$wateryr[rowid,]), na.rm = TRUE))/sd(as.matrix(clim.data$wateryr[rowid,]), na.rm = TRUE)
  full.clim.plt$tmax.scale <- ( full.clim.plt$tmax.corrected-mean(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE))/sd(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE)
  full.clim.plt
}




# variable.rad.411 <- TREEinPLOTS  %>% filter(DESIGNCD == 411) %>%dplyr::select(PLT_CN)%>% distinct()
# annual.design.plots <- TREEinPLOTS  %>% filter(DESIGNCD == 1) %>%dplyr::select(PLT_CN)%>% distinct()
# TREEinPLOTS  %>% group_by(TPA_UNADJ > 6.02) %>% summarise(n())
# TREEinPLOTS  %>% group_by(DESIGNCD) %>% summarise(n())

# read in model estimated with survival from all trees
m2 <- readRDS("m2_pipo_surv_year.rds") # from mortality_analysis_FVSmrt.R
alpha.mort <- m2$coefficients[1]
b.growth <- m2$coefficients[2]
b.dbh <- m2$coefficients[3]

set.seed(22)
#plot <- variable.rad.411[1,]
# implement mortality stochastically based on scaled SDI of the subplot:
unique(plots)
source("R/plot2AGB_kayeFVS.R")
source("R/get_objects.R")
source("R/plot_level_forecaster.R")


###########################################################################################
# make mean forecasts at the tree-level with only changing climate (i.e. no change in SDI)
###########################################################################################
# predict average growth response across all trees to climate change only ---
#i.e. only the climate change parameters, all other params are set to 0 or their site means
# probably need to try it a few ways

system.time(lapply(X = unique(plots)[1:2],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){predict_avg_growth (plt.num = pltid, #2469918010690 , 
                                                                     
                                                                      scenario = "rcp26", 
                                                                     
                                                                     
                                                                      cov.data.regional.df = cov.data.regional, 
                                                                      TREE.FIA = TREE, 
                                                                      ci.names.df = ci.names, 
                                                                      ci.names.noncored.df = ci.names.noncored, 
                                                                      mean.pred.cored.df = mean.pred.cored,
                                                                      #xmat2 = xmat2, 
                                                                      SDIscaled.matrix = SDIscaled,
                                                                      time_data_list = time_data)}))


system.time(lapply(X = unique(plots)[1:675],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){predict_avg_growth (plt.num = pltid, #2469918010690 , 
                                                             
                                                             scenario = "rcp85", 
                                                             
                                                             
                                                             cov.data.regional.df = cov.data.regional, 
                                                             TREE.FIA = TREE, 
                                                             ci.names.df = ci.names, 
                                                             ci.names.noncored.df = ci.names.noncored, 
                                                             mean.pred.cored.df = mean.pred.cored,
                                                             #xmat2 = xmat2, 
                                                             SDIscaled.matrix = SDIscaled,
                                                             time_data_list = time_data)}))


system.time(lapply(X = unique(plots)[1:675],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){predict_avg_growth (plt.num = pltid, #2469918010690 , 
                                                             
                                                             scenario = "rcp45", 
                                                             
                                                             
                                                             cov.data.regional.df = cov.data.regional, 
                                                             TREE.FIA = TREE, 
                                                             ci.names.df = ci.names, 
                                                             ci.names.noncored.df = ci.names.noncored, 
                                                             mean.pred.cored.df = mean.pred.cored,
                                                             #xmat2 = xmat2, 
                                                             SDIscaled.matrix = SDIscaled,
                                                             time_data_list = time_data)}))


system.time(lapply(X = unique(plots)[1:675],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){predict_avg_growth (plt.num = pltid, #2469918010690 , 
                                                             
                                                             scenario = "rcp60", 
                                                             
                                                             
                                                             cov.data.regional.df = cov.data.regional, 
                                                             TREE.FIA = TREE, 
                                                             ci.names.df = ci.names, 
                                                             ci.names.noncored.df = ci.names.noncored, 
                                                             mean.pred.cored.df = mean.pred.cored,
                                                             #xmat2 = xmat2, 
                                                             SDIscaled.matrix = SDIscaled,
                                                             time_data_list = time_data)}))
###########################################################################################
# Plot average change in growth over time:
plt.number <- unique(plots)[1]
scenario = "rcp60"
get.relative.growth <- function(plt.number, scenario){
  if(file.exists(paste0("no_mortality_sims/Predictions_", plt.number, "_", scenario ,".Rdata"))){
    load(paste0("no_mortality_sims/Predictions_", plt.number, "_", scenario ,".Rdata"))
    
    inc.means <- dbh.inc.index %>% group_by(treeno, time < 20) %>% summarise(mean.inc = mean(INC.median, na.rm =TRUE), 
                                                                SD.inc = sd(INC.median, na.rm = TRUE)) %>% filter(`time < 20` == TRUE) %>%
      select(treeno, mean.inc, SD.inc)
    
    rel.inc.df <- dbh.inc.index %>% left_join(.,inc.means) %>% ungroup() %>% mutate(rel.INC.median = INC.median-mean.inc, 
                                                                      rel.INC.ci.lo = INC.ci.lo - mean.inc, 
                                                                      rel.INC.ci.hi = INC.ci.hi - mean.inc)
    
    # ggplot(dbh.inc.index, aes(x = time, y = INC.median, group = treeno))+geom_line()
    # ggplot(rel.inc.df, aes(x = time, y = rel.INC.median, group = treeno))+geom_line()
  
    rel.inc.df
  }else{
    cat("no existing future climate data") 
  }
}

get.relative.growth(plt.number = unique(plots)[1], scenario = "rcp26")

# for RCP 2.6
relgrowth.list <- lapply(X = unique(plots)[1:675],
                   FUN = function(pltid){get.relative.growth (plt.number = pltid, 
                                                             scenario = "rcp26"
                                                             )})
rel.growth.df <- do.call(rbind, relgrowth.list)
rel.growth.df$scenario <- "rcp26"

# RCP 4.5
relgrowth.list <- lapply(X = unique(plots)[1:675],
                         FUN = function(pltid){get.relative.growth (plt.number = pltid, 
                                                                    scenario = "rcp45"
                         )})
rel.growth.df.45 <- do.call(rbind, relgrowth.list)
rel.growth.df.45$scenario <- "rcp45"

# RCP 6.0
relgrowth.list <- lapply(X = unique(plots)[1:675],
                         FUN = function(pltid){get.relative.growth (plt.number = pltid, 
                                                                    scenario = "rcp60"
                         )})
rel.growth.df.60 <- do.call(rbind, relgrowth.list)
rel.growth.df.60$scenario <- "rcp60"
# RCP 8.5
relgrowth.list <- lapply(X = unique(plots)[1:675],
                         FUN = function(pltid){get.relative.growth (plt.number = pltid, 
                                                                    scenario = "rcp85"
                         )})
rel.growth.df.85 <- do.call(rbind, relgrowth.list) 
rel.growth.df.85$scenario <- "rcp85"

saveRDS(rel.growth.df, "outputs/relative_growth/rcp_2.6_relative_growth_pred.rds")
saveRDS(rel.growth.df.45, "outputs/relative_growth/rcp_4.5_relative_growth_pred.rds")
saveRDS(rel.growth.df.60, "outputs/relative_growth/rcp_6.0_relative_growth_pred.rds")
saveRDS(rel.growth.df.85, "outputs/relative_growth/rcp_8.5_relative_growth_pred.rds")

rm(fiadb, x.mat, x.mat2, numeric_matrix)
# combine together
rel.growth.df <- rbind(rel.growth.df, rel.growth.df.45, rel.growth.df.60, rel.growth.df.85)

# plot up the average % change in growth 
# total average change in growth
rel.growth.summary <- rel.growth.df %>% group_by(time, scenario) %>% summarise(rel.growth.mean = mean(rel.INC.median), 
                                                                     rel.growth.ci.lo = mean(rel.INC.ci.lo), 
                                                                     rel.growth.ci.hi = mean(rel.INC.ci.hi))

# get plot-level averages to plot up
rel.growth.summary.plt <- rel.growth.df %>% group_by(time, scenario, PLT_CN) %>% summarise(rel.growth.mean = mean(rel.INC.median), 
                                                                     rel.growth.ci.lo = mean(rel.INC.ci.lo), 
                                                                     rel.growth.ci.hi = mean(rel.INC.ci.hi))
year.data.frame <- data.frame(time = 1:102, 
                              year = 2001:2102)

rel.growth.summary <- left_join(rel.growth.summary, year.data.frame)
rel.growth.summary.plt <- left_join(rel.growth.summary.plt, year.data.frame)

rel.growth.summary$forecast <- ifelse(rel.growth.summary$year <= 2020, "historical", "forecast")
ggplot(data = rel.growth.summary)+geom_line(aes(x = year, y = rel.growth.mean), color = "black")+
  geom_ribbon(data = rel.growth.summary, aes(x = year, ymin = rel.growth.ci.lo, ymax = rel.growth.ci.hi, group = forecast, fill = forecast), alpha = 0.5)+
  ylab("Mean Relative Change in Tree Growth (cm)")+theme_bw(base_size = 12)+theme(panel.grid = element_blank()) + 
  geom_abline(aes(slope = 0, intercept = 0), linetype = "dashed")+facet_wrap(~scenario, ncol = 4)+
  scale_fill_manual(values = c("historical" = "black", "forecast" = "forestgreen"), name = "")
ggsave(height = 4, width = 12, units = "in", "outputs/average_predicted_growth_declines.png")

rcp.26.rel.growth.change <- ggplot()  +geom_line(data = rel.growth.summary.plt, aes(x = year, y = rel.growth.mean, group = PLT_CN), color = "grey")+
  geom_line(data = rel.growth.summary %>% filter(scenario %in% "rcp26"), aes(x = year, y = rel.growth.mean), color = "darkblue")+
  geom_ribbon(data = rel.growth.summary %>% filter(scenario %in% "rcp26"), aes(x = year, ymin = rel.growth.ci.lo, ymax = rel.growth.ci.hi), alpha = 0.60, fill = "forestgreen")+
  geom_abline(aes(slope = 0, intercept = 0), linetype = "dashed")+
  ylab("Relative Change in Tree Growth")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())#+ylim(-0.17,0.25)
rcp.26.rel.growth.change


rcp.45.rel.growth.change <- ggplot() + geom_abline(aes(slope = 0, intercept = 0), linetype = "dashed")+geom_line(data = rel.growth.summary.plt, aes(x = year, y = rel.growth.mean, group = PLT_CN), color = "grey")+
  geom_line(data = rel.growth.summary %>% filter(scenario %in% "rcp45"), aes(x = year, y = rel.growth.mean), color = "darkblue")+#facet_wrap(~scenario)+
  geom_ribbon(data = rel.growth.summary %>% filter(scenario %in% "rcp45"), aes(x = year, ymin = rel.growth.ci.lo, ymax = rel.growth.ci.hi), alpha = 0.60, fill = "forestgreen")+
  ylab("Relative Change in Tree Growth")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())#+ylim(-0.17,0.07)
rcp.45.rel.growth.change

rcp.60.rel.growth.change <- ggplot() + geom_abline(aes(slope = 0, intercept = 0), linetype = "dashed")+geom_line(data = rel.growth.summary.plt, aes(x = year, y = rel.growth.mean, group = PLT_CN), color = "grey")+
  geom_line(data = rel.growth.summary %>% filter(scenario %in% "rcp60"), aes(x = year, y = rel.growth.mean), color = "darkblue")+#facet_wrap(~scenario)+
  geom_ribbon(data = rel.growth.summary %>% filter(scenario %in% "rcp60"), aes(x = year, ymin = rel.growth.ci.lo, ymax = rel.growth.ci.hi), alpha = 0.60, fill = "forestgreen")+
  ylab("Relative Change in Tree Growth")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(-0.17,0.07)
rcp.60.rel.growth.change

rcp.85.rel.growth.change <- ggplot() + geom_abline(aes(slope = 0, intercept = 0), linetype = "dashed")+geom_line(data = rel.growth.summary.plt, aes(x = year, y = rel.growth.mean, group = PLT_CN), color = "grey")+
  geom_line(data = rel.growth.summary %>% filter(scenario %in% "rcp85"), aes(x = year, y = rel.growth.mean), color = "darkblue")+#facet_wrap(~scenario)+
  geom_ribbon(data = rel.growth.summary %>% filter(scenario %in% "rcp85"), aes(x = year, ymin = rel.growth.ci.lo, ymax = rel.growth.ci.hi), alpha = 0.60, fill = "forestgreen")+
  ylab("Relative Change in Tree Growth")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(-0.17,0.07)
rcp.85.rel.growth.change


cowplot::plot_grid(rcp.26.rel.growth.change, rcp.45.rel.growth.change, rcp.60.rel.growth.change, rcp.26.rel.growth.change, 
          ncol = 4, align = "hv")


###########################################################################################
# make mean forecasts at the tree-level with only changing climate (i.e. no change in SDI)
###########################################################################################
# plot level AGBI trends with climate change only
pfts = list(PIPO = data.frame(spcd=122,acronym='PIPO')) # list our "Pfts--plant functional types" of interest--really list the species
source("R/AllomAve.R")
source("R/read.allom.data.R")
source("R/query.allom.data.R")
source("R/allom.BayesFit.R")
# Run AllomAve for each component in Kaye
kaye_pipo = AllomAve(pfts, components = c(4, 5, 8, 12, 18), ngibbs = 1000,
                     parm = "data/kaye_pipo.csv")

# had to read in the kaye_pipo csv...should just upload to the data
kaye.parm <- read.csv("data/kaye_pipo.csv")


allom.stemwood = load("Allom.PIPO.4.Rdata")
allom.stembark = load("Allom.PIPO.5.Rdata")
allom.branchlive = load("Allom.PIPO.8.Rdata")
allom.branchdead = load("Allom.PIPO.12.Rdata")
allom.foliage = load("Allom.PIPO.18.Rdata")
source("R/plot2AGB_kaye_nomort.R")


colnames()

get.Plot.AGBI <- function(plt.number, scenario){
  if(file.exists(paste0("no_mortality_sims/Predictions_", plt.number, "_", scenario ,".Rdata"))){
    load(paste0("no_mortality_sims/Predictions_", plt.number, "_", scenario ,".Rdata"))
    #dbh.inc.index
    
    out.mean <- t(dbh.inc.index %>% ungroup() %>%  select(DBH.ci.lo, DBH.median, DBH.ci.hi))
    combined <-  dbh.inc.index %>% ungroup() %>% select(treeno, SUBP, CN, TPA_UNADJ) %>% distinct()
    # get a TPA out:
   
    TPAlive <-  t(dbh.inc.index %>% ungroup() %>% select(TPA_UNADJ) )
    total.plot.AGB <- plot2AGB_nomort(combined,
                    out = out.mean,
                    tpa.live = TPAlive,
                    mort.scheme,
                    allom.stats,
                    unit.conv = 1, # no unit converseion
                    plot = plt.number,
                    yrvec = 2001:2098,
                    scenario = scenario)
  }else{
    cat("no existing future climate data") 
    col.names.df <-  c("plot",            "year" ,           "mAGB" ,           "mAGB.stemwood",   "mAGB.stembark",   "mAGB.branchlive",
     "mAGB.branchdead", "mAGB.foliage",    "upA",             "lowA",          "upA.stemwood" ,   "upA.stembark"  ,
     "upA.branchlive" , "upA.branchdead",  "upA.foliage",     "lowA.stemwood",   "lowA.stembark",   "lowA.branchlive",
    "lowA.branchdead", "lowA.foliage",    "mNPP" ,           "mNPP.stemwood" ,  "mNPP.stembark" ,  "mNPP.branchlive",
     "mNPP.branchdead", "mNPP.foliage",    "up"   ,           "low" ,            "up.stemwood",     "up.stembark" ,   
     "up.branchlive", "up.branchdead" ,  "up.foliage",      "low.stemwood",    "low.stembark" ,   "low.branchlive" ,
     "low.branchdead",  "low.foliage")
    
    empty.df <- data.frame(matrix(data = NA, nrow = 1, ncol = 38))
    colnames(empty.df) <- col.names.df
   
    
  }
}

# get the plot AGB and AGBI
single.agb <- get.Plot.AGBI(plt.number = unique(plots)[534], scenario = "rcp26")

ggplot(single.agb, aes(x = year, y = mAGB, group = plot))+geom_line()
# for RCP 2.6
AGBI.list <- lapply(X = unique(plots)[1:675],
                         FUN = function(pltid){get.Plot.AGBI(plt.number = pltid, 
                                                             scenario = "rcp26"
                         )})
AGBI.df <- do.call(rbind, AGBI.list)
AGBI.df$scenario <- "rcp26"
ggplot(AGBI.df %>% filter(plot %in% unique(plots)[1:600]), aes(x = year, y = mAGB, group = plot))+geom_line()

# RCP 4.5
AGBI.45.list <- lapply(X = unique(plots)[1:675],
                    FUN = function(pltid){get.Plot.AGBI(plt.number = pltid, 
                                                        scenario = "rcp45"
                    )})
AGBI.45.df <- do.call(rbind, AGBI.45.list)
AGBI.45.df$scenario <- "rcp45"

# RCP 6.0
AGBI.60.list <- lapply(X = unique(plots)[1:675],
                       FUN = function(pltid){get.Plot.AGBI(plt.number = pltid, 
                                                           scenario = "rcp60"
                       )})
AGBI.60.df <- do.call(rbind, AGBI.60.list)
AGBI.60.df$scenario <- "rcp60"

# RCP 8.5
AGBI.85.list <- lapply(X = unique(plots)[1:675],
                       FUN = function(pltid){get.Plot.AGBI(plt.number = pltid, 
                                                           scenario = "rcp85"
                       )})
AGBI.85.df <- do.call(rbind, AGBI.85.list)
AGBI.85.df$scenario <- "rcp85"


AGBI.all.df <- rbind(AGBI.df, AGBI.45.df, AGBI.60.df, AGBI.85.df)
saveRDS(AGBI.all.df, "outputs/relative_growth/AGBI_all_plots.rds")
ggplot(AGBI.all.df, aes(x = year, y = mAGB, group = plot))+geom_line()+facet_wrap(~scenario)
ggplot(AGBI.all.df, aes(x = year, y = mNPP, group = plot))+geom_line()+facet_wrap(~scenario)


AGBI.all.summary <-
  AGBI.all.df %>% group_by(year, scenario) %>% summarise(mNPP.avg = median(mNPP),
                                                         mNPP.sd = sd(mNPP)) %>%
  
  # these are in kg/acre, need to convert to Mg/Ha
  mutate(
    mNPP.avg.kg_acre = (mNPP.avg / 1000) * 2.471,
    sdNPP.avg.kg_acre = (mNPP.sd / 1000) * 2.471
  ) # divide to convert kg to Mg and multiply to convert to Ha

ggplot()+geom_line(data = AGBI.all.summary, aes(x = year, y = mNPP.avg.kg_acre, color = scenario))+theme_bw(base_size = 12)+
  ylab("Average Aboveground Biomass Increment (Mg/ha)")
ggsave("outputs/AGBI_without_mortality.png")

##############################################################################
# plot level AGBI trends with mortality
##############################################################################

AGB <- readRDS(paste0("outputs/parse.DIDD.mort.60SDIthreshold_1.RDS"))

AGBIw.mort.all.summary <-
  AGB %>% group_by(year, rcp, parse) %>% summarise(mNPP.avg = median(mNPP),
                                                         mNPP.sd = sd(mNPP)) %>%
  
  # these are in kg/acre, need to convert to Mg/Ha
  mutate(
    mNPP.avg.kg_acre = (mNPP.avg / 1000) * 2.471,
    sdNPP.avg.kg_acre = (mNPP.sd / 1000) * 2.471
  ) # divide to convert kg to Mg and multiply to convert to Ha

ggplot()+geom_line(data = AGBIw.mort.all.summary, aes(x = year, y = mNPP.avg.kg_acre, color = rcp))+theme_bw(base_size = 12)+
  ylab("Average Aboveground Biomass Increment (Mg/ha)")+facet_wrap(~parse)
ggsave("outputs/AGBI_with_mortality_all_parse.png")

ggplot()+geom_line(data = AGBIw.mort.all.summary %>% filter(parse %in% "full"), aes(x = year, y = mNPP.avg.kg_acre, color = rcp))+theme_bw(base_size = 12)+
  ylab("Average Aboveground Biomass Increment (Mg/ha)")+facet_wrap(~parse)
ggsave("outputs/AGBI_with_mortality.png")
