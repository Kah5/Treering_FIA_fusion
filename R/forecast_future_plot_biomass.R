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
                                 N.microplots = c(1, 
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
#pipo.clim <- read.csv("data/pipo_all_tmean_ppt_v5.csv")
#pipo.clim <- read.csv(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/pipo_all_tmean_ppt_v5.csv"))
#pipo.clim  <- readRDS("data/pipo.cores.with.downscaled.hydro.ppt.climatev4.rds")
#pipo.clim$cov.data
head(pipo.clim)
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

# #select the covvariate data for the cored and uncored trees:

# select the outdata for the cored and uncored trees:
sel.noncored <- which(ci.names.noncored $row %in% y)

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
source("R/generate_forecast.R")
source("R/biomass.sensitivity.periodic_4scenarios.R")
high.plts <- readRDS("outputs/suspiciously_high_prediction_plots.rds")
# run the function that makes all of the forecasts
# system.time(biomass.sensitivity.periodic( plot = '2533485010690', density.dependent = TRUE, density.independent = TRUE, scenario = "rcp26", SDI.ratio.DD = 0.6, aggressiveCC = FALSE, scale.mort.prob = 0.9))
# system.time(biomass.sensitivity.periodic( plot = '2873938010690', density.dependent = TRUE, density.independent = FALSE, scenario = "rcp26", SDI.ratio.DD = 0.6, aggressiveCC = FALSE, scale.mort.prob = 0.9))

#odd.plots <- readRDS("outputs/suspiciously_high_prediction_plots.rds")
#system.time(biomass.sensitivity.periodic( plot =variable.rad.411[1,], density.dependent = TRUE, density.independent = TRUE, scenario = "rcp45", SDI.ratio.DD = 0.6, aggressiveCC = FALSE, scale.mort.prob = 0.9))
#plot <- unique(odd.plots$plot)[1]

# run all the plots for this scenario and 60% max SDI
remeasured.trees.plts <- cov.data.regional %>% filter(!is.na(DIA_cm_T2) & MEASYEAR_T2 > 2001)
scenario = "rcp26"
#biomass.sensitivity.periodic(plot = unique(odd.plots$plot)[1], density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.6, aggressiveCC = FALSE, scale.mort.prob = 1)
plot <- 3125031010690

plt.num = as.character(remeasured.trees.plts$PLT_CN)[1]#2562224010690, #2562224010690, #as.character(cov.data.regional$PLT_CN[1]), #2487922010690,#2972526010690, #2972148010690, #high.plts$PLT_CN[2] , #2469918010690 , 
density.dependent = TRUE 
density.independent = TRUE 
scenario = "rcp26" 
SDI.ratio.DD = 0.6 
aggressiveCC = FALSE 
scale.mort.prob = 1 
cov.data.regional.df = cov.data.regional 
TREE.FIA = TREE 
ci.names.df = ci.names 
ci.names.noncored.df = ci.names.noncored 
mean.pred.cored.df = mean.pred.cored
#xmat2 = xmat2, 
SDIscaled.matrix = SDIscaled
time_data_list = time_data

#biomass.sensitivity.periodic(plot = unique(odd.plots$plot)[2], density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.6, aggressiveCC = FALSE, scale.mort.prob = 1)
biomass.sensitivity.periodic(plt.num = as.character(remeasured.trees.plts$PLT_CN)[122],#2562224010690, #2562224010690, #as.character(cov.data.regional$PLT_CN[1]), #2487922010690,#2972526010690, #2972148010690, #high.plts$PLT_CN[2] , #2469918010690 , 
                             density.dependent = TRUE, 
                             density.independent = TRUE, 
                             scenario = "rcp26", 
                             SDI.ratio.DD = 0.6, 
                             aggressiveCC = FALSE, 
                             scale.mort.prob = 1, 
                             cov.data.regional.df = cov.data.regional, 
                             TREE.FIA = TREE, 
                             ci.names.df = ci.names, 
                             ci.names.noncored.df = ci.names.noncored, 
                             mean.pred.cored.df = mean.pred.cored,
                             #xmat2 = xmat2, 
                             SDIscaled.matrix = SDIscaled,
                             time_data_list = time_data)
as.character(unique(remeasured.trees.plts$PLT_CN))[1:296] %in% 2484206010690
# run for all of the remeasured trees
system.time(lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[124:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){biomass.sensitivity.periodic(plt.num = pltid, #2469918010690 , 
                                                                  density.dependent = TRUE, 
                                                                  density.independent = TRUE, 
                                                                  scenario = "rcp26", 
                                                                  SDI.ratio.DD = 0.6, 
                                                                  aggressiveCC = FALSE, 
                                                                  scale.mort.prob = 1, 
                                                                  cov.data.regional.df = cov.data.regional, 
                                                                  TREE.FIA = TREE, 
                                                                  ci.names.df = ci.names, 
                                                                  ci.names.noncored.df = ci.names.noncored, 
                                                                  mean.pred.cored.df = mean.pred.cored,
                                                                  #xmat2 = xmat2, 
                                                                  SDIscaled.matrix = SDIscaled,
                                                                  time_data_list = time_data)}))


########################################################################################
# read in the predicted cored values from the SSM plus mortality forecasts
########################################################################################
get_remeas <- function(plot, 
                       mort.scheme, 
                       scenario, 
                       SDI.ratio.DD, 
                       cc.scenario, 
                       scale.mort.prob = 1, 
                       parse = "full" ){
  
  cat(paste0("getting pred vs obs for ", as.character(plot)))
  
  if(!file.exists(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".",parse,".Rdata"))==TRUE){
    cat("no forecast")
  }else{
    oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
    if(nrow(oldTREE) <=1){
      cat("less than 2 trees on the first plot")
    }else{
      
      cat (paste("reading in forecasts from plot ", plot))
      load(paste0("biomass_dataFIAperiodic_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",scenario,".", SDI.ratio.DD, ".", cc.scenario, ".",parse,".Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      cored.remeas
    }
  }
}

cored.reams.list <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[1:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                   FUN = function(pltid){get_remeas(plot = pltid, 
                                                    mort.scheme = "DIDD",
                                                    scenario = "rcp26", 
                                                    SDI.ratio.DD = 0.6, 
                                                    cc.scenario = "singleCC",
                                                    scale.mort.prob = 1,
                                                    parse = "full"
                                                                      )})


cored.remeas.df <- do.call(rbind, cored.reams.list)
cored.remeas.df$parse <- "full"
saveRDS(cored.remeas.df, "outputs/validation/cored.remeas.df.full.rds")

ggplot(data = cored.remeas.df, aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = cored.remeas.df, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")
ggsave("outputs/validation/out_of_sample_full_diamter_plots_full.png")

cored.remeas.df %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                                   RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                                   MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
) 

summary.stats.full <- summary(lm(DIA_cm_T2  ~ predDBH_T2, data = cored.remeas.df))




cored.reams.list.nocc <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[1:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                           FUN = function(pltid){get_remeas(plot = pltid, 
                                                            mort.scheme = "DIDD",
                                                            scenario = "rcp26", 
                                                            SDI.ratio.DD = 0.6, 
                                                            cc.scenario = "singleCC",
                                                            scale.mort.prob = 1,
                                                            parse = "noCC"
                           )})


cored.remeas.df.nocc <- do.call(rbind, cored.reams.list.nocc)
cored.remeas.df.nocc$parse <- "noCC"
saveRDS(cored.remeas.df.nocc, "outputs/validation/cored.remeas.df.noCC.rds")


ggplot(data = cored.remeas.df.nocc, aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = cored.remeas.df.nocc, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")
ggsave("outputs/validation/out_of_sample_full_diamter_plots_noCC.png")


cored.remeas.df.nocc %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                                    RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                                    MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
) 

summary.stats.nocc <- summary(lm(DIA_cm_T2  ~ predDBH_T2, data = cored.remeas.df.nocc))



cored.reams.list.GD.10 <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[1:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                                FUN = function(pltid){get_remeas(plot = pltid, 
                                                                 mort.scheme = "DIDD",
                                                                 scenario = "rcp26", 
                                                                 SDI.ratio.DD = 0.6, 
                                                                 cc.scenario = "singleCC",
                                                                 scale.mort.prob = 1,
                                                                 parse = "GD.10"
                                )})


cored.remeas.df.GD.10 <- do.call(rbind, cored.reams.list.GD.10)
cored.remeas.df.GD.10$parse <- "GD.10"
saveRDS(cored.remeas.df.GD.10, "outputs/validation/cored.remeas.df.GD.10.rds")


ggplot(data = cored.remeas.df.GD.10, aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = cored.remeas.df.GD.10, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")
ggsave("outputs/validation/out_of_sample_full_diamter_plots_GD.10.png")

cored.remeas.df.GD.10 %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                              RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                              MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
                              ) 


summary.stats.GD.10 <- summary(lm(DIA_cm_T2  ~ predDBH_T2, data = cored.remeas.df.GD.10))




cored.reams.list.GD.20 <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[1:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                                 FUN = function(pltid){get_remeas(plot = pltid, 
                                                                  mort.scheme = "DIDD",
                                                                  scenario = "rcp26", 
                                                                  SDI.ratio.DD = 0.6, 
                                                                  cc.scenario = "singleCC",
                                                                  scale.mort.prob = 1,
                                                                  parse = "GD.20"
                                 )})


cored.remeas.df.GD.20 <- do.call(rbind, cored.reams.list.GD.20)
cored.remeas.df.GD.20$parse <- "GD.20"
saveRDS(cored.remeas.df.GD.20, "outputs/validation/cored.remeas.df.GD.20.rds")


ggplot(data = cored.remeas.df.GD.20, aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = cored.remeas.df.GD.20, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")
ggsave("outputs/validation/out_of_sample_full_diamter_plots_GD.20.png")

cored.remeas.df.GD.20 %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                                    RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                                    MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
) 


summary.stats.GD.20 <- summary(lm(DIA_cm_T2  ~ predDBH_T2, data = cored.remeas.df.GD.20))


cored.reams.list.DD.ramp <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN))[1:296],#unique(plots)[534:675],#unique(plots)[!unique(plots) %in% unique(high.plts$plot)][356:612],#[355:612], #unique(high.plts$plot), #unique(plots)[540:650],
                                 FUN = function(pltid){get_remeas(plot = pltid, 
                                                                  mort.scheme = "DIDD",
                                                                  scenario = "rcp26", 
                                                                  SDI.ratio.DD = 0.6, 
                                                                  cc.scenario = "singleCC",
                                                                  scale.mort.prob = 1,
                                                                  parse = "DD.ramp"
                                 )})


cored.remeas.df.DD.ramp <- do.call(rbind, cored.reams.list.DD.ramp)
cored.remeas.df.DD.ramp$parse <- "DD.ramp"
saveRDS(cored.remeas.df.DD.ramp, "outputs/validation/cored.remeas.df.DD.ramp.rds")


ggplot(data = cored.remeas.df.DD.ramp, aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = cored.remeas.df.DD.ramp, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")
ggsave("outputs/validation/out_of_sample_full_diamter_plots_DD.ramp.png")

cored.remeas.df.DD.ramp %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                                    RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                                    MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
) 


summary.stats.DD.ramp <- summary(lm(DIA_cm_T2  ~ predDBH_T2, data = cored.remeas.df.DD.ramp))



## combine here
core.remeas.both <- rbind(cored.remeas.df, cored.remeas.df.nocc, cored.remeas.df.GD.10, 
                          cored.remeas.df.GD.20, cored.remeas.df.DD.ramp)
ggplot(data = core.remeas.both , aes(x = DIA_cm_T2, y = predDBH_T2))+geom_point()+
  geom_errorbar(data = core.remeas.both , aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+
  ylab("Predicted Diameter (cm)")+xlab("Held-out Diameter Observations (cm)")+facet_wrap(~parse)
ggsave("outputs/validation/out_of_sample_full_diamter_plots_all_scenarios.png")

core.remeas.both %>% group_by(treeid) %>% mutate(diff = DIA_cm_T2 - predDBH_T2) %>%
  group_by(parse) %>% summarise(`average difference` = mean (diff)) %>% rename(`scenario` = "parse") |>gt()

core.remeas.both %>% group_by(parse) %>% summarise(MSPE = mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE),
                                   RMSPE = sqrt(mean(( DIA_cm_T2 - predDBH_T2)^2, na.rm =TRUE)),
                                   MAPE = mean(abs( DIA_cm_T2 - predDBH_T2), na.rm = TRUE)
) 
########################################################################################
# generate tree-level forecasts in the near term without mortality or changing SDI
########################################################################################
plt.num = "3153259010690" #as.character(unique(remeasured.trees.plts$PLT_CN))[1]
scenario = "rcp26"
cov.data.regional.df = cov.data.regional
TREE.FIA = TREE
ci.names.df = ci.names
ci.names.noncored.df = ci.names.noncored 
mean.pred.cored.df = mean.pred.cored
SDIscaled.matrix = time_data$SDIscaled

forecast.cored.ssm <- function(plt.num,
                               scenario,
                               cov.data.regional.df,
                               TREE.FIA = TREE,
                               ci.names.df = ci.names,
                               ci.names.noncored.df = ci.names.noncored ,
                               mean.pred.cored.df = mean.pred.cored,
                               SDIscaled.matrix = time_data$SDIscaled){
  
    cored.in.plt <- cov.data.regional.df %>% dplyr::filter (PLT_CN %in% plt.num)
    cored.in.plt <- cored.in.plt[!duplicated(cored.in.plt$TRE_CN),]
    cored.in.plt$TPA_UNADJ <- TREE.FIA[which(TREE.FIA$CN %in% unique(cored.in.plt$TRE_CN)),]$TPA_UNADJ
    x <- cored.in.plt$treeid
    
    
    # plot the posterior predictions of DBH for a single tree:
    sel <- which(ci.names.df$row %in%  x) # use sel to subset the data for the 415th tree
    mean.cored <- mean.pred.cored.df[sel]
    nsamps = 100
    nt <- length(2001:2098)
    ntfull <- length(1998:2098)
    ni <- length(cored.in.plt$CORE_CN)
    # fill in the array with the diameter estimates for 2018:
    dbh.pred <- increment  <- array(NA, dim = c(ni, nsamps, ntfull + 1))
    nsamps1 <- nsamps-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
    
    treeids <- cored.in.plt$treeid
    
    # write a function to get the MCMC samples
    
    get_mcmc_samples <- function(x, betas, nsamps){
      
      rnorm(nsamps, mean = as.numeric(betas %>% filter(L1 %in% x) %>%dplyr::select(median)), sd =  as.numeric(betas %>% filter(L1 %in% x) %>%dplyr::select(sd)))
    }
    
    nsamps <- nsamps
    #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = 1000)
    # for each tree generate a random sample for the tree-level intercept:
    alpha <- matrix(NA, nrow = 100, ncol = ni)
    if(length(treeids)>1){ # if we have more than one cored tree per plot
      alphatreeids <- vector()
      for(i in 1:length(treeids)){
        alphatreeids[i]<- paste0("alpha_TREE[", treeids[i], "]")
        alpha[,i] <- get_mcmc_samples(alphatreeids[i], betas = mus, nsamps = nsamps)
      }
      
      
    }else{ # if there is just one cored tree per plot
      alphatreeids <- paste0("alpha_TREE[", treeids, "]")
      alpha[,1] <- get_mcmc_samples(alphatreeids, betas = alphas, nsamps = nsamps)
    }
    
   
    
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
    
    
    betas.all <- data.frame(  alpha ,
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
    
    
    ##print ("set up SDI")
    # get PLT_CN
    PLT_CNint <- as.character(plt.num)
    
    # get the scaled SDI for the PLT:
    
    # get the unique SUBPLOTS in the plot
    #subplots <- unique(SDIscaled.matrix %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(SUBP))
    
    SDI.PLT.2001 <- SDIscaled.matrix[cored.in.plt$treeid,36]
    
     # get the SDI values
    
    
    ##print("extracting future climate for the plot")
    
    if(scenario %in% "rcp26"){
      clim.fut.scen <- future.clim.subset.26 
    }
    if(scenario %in% "rcp45"){
      clim.fut.scen <- future.clim.subset.45 
    }
    if(scenario %in% "rcp60"){
      clim.fut.scen <- future.clim.subset.60 
    }
    if(scenario %in% "rcp85"){
      clim.fut.scen <- future.clim.subset.85 
    }
    
    
    scale.fut.clim.by.plt <- function(x, future.clim.subset){
      #print(x)
      full.clim.plt <-  future.clim.subset %>% filter(PLT_CN == x)#full.clim.dt[PLT_CN %in% plot]
      rowid <- which(cov.data.regional.df$PLT_CN %in%  x ) # get the row for the climate data
      full.clim.plt$ppt.scale <- ( full.clim.plt$ppt.corrected-mean(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE))/sd(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE)
      full.clim.plt$tmax.scale <- ( full.clim.plt$tmax.corrected-mean(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE))/sd(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE)
      full.clim.plt
    }
    
    fut.clim.scen <- scale.fut.clim.by.plt(x = PLT_CNint, future.clim.subset = clim.fut.scen)
    
    if(nrow(fut.clim.scen)>0){
     
        models <- unique(fut.clim.scen$model) # 21 models
        sample.model <- sample(models, size = length(models), replace= FALSE)
        
        get.ens.df <- function(i){
          
          ens.proj.yr <- fut.clim.scen %>% filter(model %in% sample.model[i])
          ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
          
          
          df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                           tmax = ens.proj.yr$tmax.scale, 
                           model = i, 
                           year = ens.proj.yr$year)
          df$diff.ppt <- NA
          df$diff.tmax <- NA
          
          for(t in 2:82){
            
            df[t,]$diff.ppt <- df[t,]$ppt-df[t-1,]$ppt
            df[t,]$diff.tmax <- df[t,]$tmax - df[t-1,]$tmax
          }
          
          return(df)
        }
        
        ens.samps <- lapply(1:length(models), get.ens.df)
        ens.samps.df <- do.call(rbind, ens.samps)
        
        detrend.samps.df  <- ens.samps.df %>% dplyr::select(diff.ppt, diff.tmax, model, year)# %>%
        colnames(detrend.samps.df) <- c("ppt", "tmax", "model", "year")
        
        
        samps.df  <- ens.samps.df %>% dplyr::select(ppt, tmax, model, year)
        detrend.samps.df  <- detrend.samps.df %>% dplyr::select(ppt, tmax, model, year)
        
        
        ens.samps.df <- samps.df #%>% group_by(year, i) #%>% summarise(ppt.mean = mean(ppt, na.rm =TRUE), 
        #             tmax.mean = mean(tmax, na.rm = TRUE))
        
        #ppt.fut <- ens.samps.df %>% group_by(year,i) %>% dplyr::select(year,i, ppt)  %>% tidyr::spread(key = year, value = ppt)%>% dplyr::select(`2019`:`2098`)
        #tmax.fut <- ens.samps.df %>% dplyr::select(year, i, tmax) %>% tidyr::spread(key = year, value = tmax)%>% dplyr::select(`2019`:`2098`)
        
        ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        
        tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        hist.samps.df <- data.frame(ppt = rep(as.numeric(ppt.hist), length(unique(ens.samps.df$model))), 
                                    tmax = rep(as.numeric(tmax.hist) , length(unique(ens.samps.df$model))), 
                                    model = rep(1:length(unique(ens.samps.df$model)), each = length(as.numeric(ppt.hist))), 
                                    year = rep(2001:2018, length(unique(ens.samps.df$model))))
        
        full.df <- rbind(hist.samps.df, ens.samps.df %>% filter(!year %in% 2018))
        full.df.nodups <- full.df[!duplicated(full.df),]
        #full.df.nodups$rowid <- 1:length(full.df.nodups$ppt)
        full.ens.ppt <- full.df.nodups  %>% dplyr::select( ppt, year, model)%>% group_by(model, year)%>% 
          summarise(ppt.m = mean(ppt, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(ppt.m, year, model) %>%group_by(year)%>%
          spread(year, value = ppt.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
        full.ens.tmax <- full.df.nodups  %>% dplyr::select(tmax, year, model)%>%  group_by(model, year)%>% 
          summarise(tmax.m = mean(tmax, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(tmax.m, year, model) %>%group_by(year)%>%
          spread(year, value = tmax.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
       
        
        
        
        cov.mat <- unique(x.mat %>% filter(PLT_CN %in% plt.num) %>% dplyr::select(PLT_CN, MAP, MAT))#, MAP.scaled, MAT.scaled))
        
        # check the MAP and MAT--I think we can just get from cored.in.plt df
        # this indexing may be off now
        MAP <- cov.mat$MAP
        MAT <- cov.mat$MAT
        
        #print("assembling covariate data ")
        
        covariates <- list()
        covariates$SDI <- SDI.PLT.2001[1]
        covariates$ppt <- as.matrix(full.ens.ppt)
        covariates$tmax <- as.matrix(full.ens.tmax) 
        covariates$MAP <- cored.in.plt$MAP[1]
        covariates$MAT <- cored.in.plt$MAT[1]
        
        # get the diameters:
        
        #yrs <- 31:135
        tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names.df$row == x & ci.names.df$col %in% 33:36)}) #dplyr::select just the years 1994:2010 to match the plot level data:
        i.cored <- do.call(rbind, tree.ind.cored )
        out.cored.plt <-  out.cored[(length(out.cored[,1])-nsamps + 1):length(out.cored[,1]),i.cored] 
        
        all.dbh <- out.cored.plt
        
        all.dbh.ids <- separate(reshape2::melt(colnames(all.dbh)), col =value, into = c("x[", "tree", "year", "]"))
        all.dbh.ids$col.id <- 1:length(all.dbh.ids$tree)
        yr.2001.ids <- as.vector(unlist(all.dbh.ids %>% filter(year == 36) %>% dplyr::select(col.id)))
        
        all.dbh <- all.dbh[,yr.2001.ids]
        
        time_steps <-  length(2001:2098)
        nsamps <- max(length(betas.all$bSDI), length(x.mat[m,1]))
        forecast <- matrix(data = NA, nrow = nsamps, ncol = time_steps)
        
        
        dbh.pred <- increment <- array(NA, dim = c(ni, nsamps, ntfull + 1))
        
        
        
        # set up empty matrices for dbh, increment, TPA projections
        id.ni <- rep(1:ni, each = 1)
        if(is.null(ncol(all.dbh))){
          for(i in 1:length(id.ni)){
          dbh.pred[id.ni[i],,1] <- all.dbh[i]
          }
        }else{
          for(i in 1:length(id.ni)){
            dbh.pred[id.ni[i],,1] <- all.dbh[,i]
          }
        }
        
        # get the increments
        for(i in 1:ni){
          for(t in 2:4){
            increment[i,,t] <- dbh.pred[i,,t] - dbh.pred[i,,t-1]
          }
        }
        
        
        # get the tree diameter means
        dbh.pred.means <- apply(dbh.pred, 1, FUN = mean, na.rm=TRUE)
        
        # Make the time series forecasts for each tree i and time t
        for(i in 1:ni){
         for(t in 1:time_steps){
          
          if(t == 1){ # if t is less than the measureyr assign NA (fo now)
      
            # if the time step is the measuryr use the measureed DBH
            increment[i,,t] <- iterate_statespace.incpred(x = dbh.pred[id.ni[i],,1],  
                                                    betas.all = betas.all, 
                                                    alpha = betas.all[,i], 
                                                    SDinc = sigma.INC$median, 
                                                    covariates = data.frame(SDI = covariates$SDI, 
                                                                           ppt = covariates$ppt[,t], 
                                                                           tmax = covariates$tmax[,t], 
                                                                           MAP = covariates$MAP,
                                                                           MAT = covariates$MAT))
              
            dbh.pred[id.ni[i],,t]<- dbh.pred[id.ni[i],,1]+increment[i,,t]
              
            }else{
              increment[i,,t]<- iterate_statespace.incpred(x = dbh.pred[id.ni[i],,t-1],
                                                   betas.all = betas.all, 
                                                   alpha = betas.all[,i], 
                                                   SDinc = sigma.INC$median, 
                                                   covariates = data.frame(SDI = covariates$SDI, 
                                                                           ppt = covariates$ppt[,t], 
                                                                           tmax = covariates$tmax[,t], 
                                                                           MAP = covariates$MAP,
                                                                           MAT = covariates$MAT))
                                                   
              dbh.pred[id.ni[i],,t] <- dbh.pred[id.ni[i],,t-1]+ increment[i,,t]
              
              forecast <- dbh.pred
          
            }  
           
          }
        
          
        }
        
        # lets output the summary of the posterior predictions
        forecast.summary  <- reshape2::melt(forecast) %>% group_by(Var1, Var3) %>% 
          summarise(median = median(value, na.rm =TRUE), 
                   ci.lo = quantile(value, 0.025, na.rm =TRUE), 
                   ci.hi = quantile(value, 0.975, na.rm =TRUE)) %>%
          rename("index.id" = "Var1", 
                 "year.id" = "Var3")
        
        
        index.df <- data.frame(treeid = cored.in.plt$treeid, 
                   index.id = 1:ni)
        year.df <- data.frame(year = 2002:2098, 
                              year.id = 1:97)
        forecast.summary <- left_join(year.df, forecast.summary) %>% left_join(., index.df)
    }else{
      
       forecast.summary <- data.frame(year = NA, 
                                      year.id = NA, 
                                      index.id = NA, 
                                      median = NA, 
                                      ci.lo = NA, 
                                      ci.hi = NA, 
                                      treeid = NA)
        
    }  
    
    forecast.summary
 }   

# test on one plot
system.time(forecast.cored.ssm(plt.num = as.character(unique(remeasured.trees.plts$PLT_CN))[1],
                   scenario = "rcp26",
                   cov.data.regional.df = cov.data.regional,
                   TREE.FIA = TREE,
                   ci.names.df = ci.names,
                   ci.names.noncored.df = ci.names.noncored ,
                   mean.pred.cored.df = mean.pred.cored,
                   SDIscaled.matrix = time_data$SDIscaled
)   )

cored.tree.ssm.forecast.list <- list()
for(d in 1:20){
cored.tree.ssm.forecast.list[[d]]<- forecast.cored.ssm(plt.num = as.character(unique(remeasured.trees.plts$PLT_CN))[d],
                   scenario = "rcp26",
                   cov.data.regional.df = cov.data.regional,
                   TREE.FIA = TREE,
                   ci.names.df = ci.names,
                   ci.names.noncored.df = ci.names.noncored ,
                   mean.pred.cored.df = mean.pred.cored,
                   SDIscaled.matrix = time_data$SDIscaled
) 
}
# apply the function over all plots that have held out measurements

cored.tree.ssm.forecast.list <- lapply(X = as.character(unique(remeasured.trees.plts$PLT_CN)), 
                                       FUN = function(X){forecast.cored.ssm(plt.num = X,
                                                   scenario = "rcp26",
                                                   cov.data.regional.df = cov.data.regional,
                                                   TREE.FIA = TREE,
                                                   ci.names.df = ci.names,
                                                   ci.names.noncored.df = ci.names.noncored ,
                                                   mean.pred.cored.df = mean.pred.cored,
                                                   SDIscaled.matrix = time_data$SDIscaled
)} )


cored.tree.ssm.forecast.df <- do.call(rbind, cored.tree.ssm.forecast.list)

#cored.tree.ssm.forecast.df <- cored.tree.ssm.forecast.df %>% rename(`MEASYEAR_T2` = year)

ggplot(cored.tree.ssm.forecast.df, aes(x = year, y = median, group = treeid))+geom_line()


pred.obs.ssmonly <- left_join(remeasured.trees.plts, cored.tree.ssm.forecast.df) %>% filter(year == MEASYEAR_T2)

ggplot()+geom_point(data = pred.obs.ssmonly, aes(x = DIA_cm_T2, y = median))+
  geom_errorbar(data = pred.obs.ssmonly, aes(x = DIA_cm_T2, ymin = ci.lo, ymax = ci.hi))+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  ylab("Predicted Diameters (cm)\n SSM-only predictions")+
  xlab("Observed Diameters (cm)")+theme_bw()
ggsave("outputs/validation/SSM_only_predicted_diameters.png")
saveRDS(pred.obs.ssmonly, "outputs/validation/SSM_only_predicted_cored_trees.RDS")

pred.obs.ssmonly %>% summarise(MSPE = mean(( DIA_cm_T2 - median)^2, na.rm =TRUE),
                              RMSPE = sqrt(mean(( DIA_cm_T2 - median)^2, na.rm =TRUE)),
                              MAPE = mean(abs( DIA_cm_T2 - median), na.rm = TRUE)
) 


summary.stats <- summary(lm(DIA_cm_T2  ~ median, data = pred.obs.ssmonly))
saveRDS(pred.obs.ssmonly, "outputs/validation/cored_remeas_ssm_only.RDS")


##########################################################
# Validation for tally trees in these plots 
##########################################################
# get FIADB pulled for the region
db <- readRDS("data/InWeUS_FIAdb.rds")
#db$TREE %>% filter(PLT_CN %in% unique(plots))%>% distinct(PLT_CN)
PLOT <- db$PLOT %>% select(CN, MEASYEAR, PREV_PLT_CN) %>% rename(`PLT_CN` = "CN")
TREE <- db$TREE
rm(db)



##########################################################
# Validation/Check on AGB predictions at the plot-level 
##########################################################



Full.AGB.df <- readRDS("outputs/temporary.full.AGB.df.rds")
Full.AGB.df %>% filter(plot %in% as.character(unique(PLOT$PLT_CN)))
Full.AGB.df %>% filter(plot %in% as.character(unique(PLOT$PREV_PLT_CN)))

# no repeat measurements in the publically available datasets
unique(as.character(cov.data.regional$PLT_CN)) %in% unique(as.character(PLOT$PREV_PLT_CN))
unique(as.character(cov.data.regional$PLT_CN)) %in% unique(as.character(PLOT$CN))

cov.data.regional %>% filter(as.character(PLT_CN) %in% unique(as.character(PLOT$PREV_PLT_CN)))



AGB.plt <- TREE %>% filter(PLT_CN %in% unique(Full.AGB.df$plot)) %>% group_by(PLT_CN, INVYR) %>%  summarise(
  CARBON_AG_MG = sum(CARBON_AG*TPA_UNADJ/2205), 
  BIOMASS_AG_MG =  sum((CARBON_AG*TPA_UNADJ/2205)*2))
AGB.plt <- AGB.plt  %>% left_join(.,PLOT)



hist(AGB.plt$CARBON_AG_MG)
hist(AGB.plt$BIOMASS_AG_MG)

# read in the biomass estimates and compare
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
        load(paste0("biomass_dataFIAperiodic_noCC_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario,".detrendedCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      }else{
        load(paste0("biomass_dataFIAperiodic_noCC_",scale.mort.prob,"/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".",  cc.scenario,".detrendedCC.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
        
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

Full.AGB <- lapply(as.character(unique(remeasured.trees.plts$PLT_CN)),FUN = function(x){parse_biomass_ests (plot = x, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC", scale.mort.prob = 1 )})
Full.AGB.df <- do.call(rbind, Full.AGB)

saveRDS(Full.AGB.df, "outputs/temporary.full.AGB.df.rds")

## 
#AGB.plt$plot <- as.character(tp.ratio$PLT_CN)
Full.AGB.df$plot <- as.character(Full.AGB.df$plot)
AGB.plt$plot <- as.character(AGB.plt$PLT_CN)

parse.observed.agb <- left_join(Full.AGB.df, AGB.plt)
head(parse.observed.agb)

parse.observed.agb
parse.observed.agb %>% filter(year == INVYR)

# plot starting forecast vs FIADB agb:
ggplot()+geom_point(data = parse.observed.agb %>% filter(year == 2002), aes(x = BIOMASS_AG_MG, y = mAGB/1000))+
  geom_errorbar(data = parse.observed.agb %>% filter(year == 2002), aes(x = BIOMASS_AG_MG, ymin = lowA/1000, ymax = upA/1000))+facet_wrap(~parse)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ylim(0, 200)+ylab("Total Forecasted plot AGB (MG/acre) in 2002")+xlab("Total AGB in live stem and bark (MG/acre) in FIADB")
ggsave(height = 5, width = 6, units = "in", "outputs/RCP2.6_PLOT_ALL_AGB_Pred_observed_AGB_fixed.png")

ggplot()+geom_point(data = parse.observed.agb %>% filter(year %in% 2002), aes(x = BIOMASS_AG_MG, y = (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000))+
  geom_errorbar(data = parse.observed.agb %>% filter(year %in% 2002), aes(x = BIOMASS_AG_MG, ymin = (lowA.stemwood + lowA.stembark + lowA.branchlive)/1000, ymax = (upA.stemwood + upA.stembark + upA.branchlive)/1000))+facet_wrap(~parse)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ylim(0, 200)+ylab("Total Forecasted live stem and bark AGB (MG/acre) in 2002")+xlab("Total AGB in live stem and bark (MG/acre) in FIADB")
ggsave(height = 5, width = 6, units = "in", "outputs/RCP2.6_PLOT_STEM_BARK_Pred_observed_AGB_fixed.png")

ggplot()+geom_point(data = parse.observed.agb %>% filter(year %in% 2002 & parse == "full"), aes(x = BIOMASS_AG_MG, y = (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000))+
  geom_errorbar(data = parse.observed.agb %>% filter(year %in% 2002 & parse == "full"), aes(x = BIOMASS_AG_MG, ymin = (lowA.stemwood + lowA.stembark + lowA.branchlive)/1000, ymax = (upA.stemwood + upA.stembark + upA.branchlive)/1000))+#facet_wrap(~parse)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ylim(0, 200)+ylab("Total Forecasted live stem and bark AGB (MG/acre) in 2002")+xlab("Total AGB in live stem and bark (MG/acre) in FIADB")+theme_bw()
ggsave(height = 5, width = 6, units = "in", "outputs/RCP2.6_PLOT_STEM_BARK_Pred_observed_AGB_fixed_full.png")



parse.observed.agb %>% filter(year %in% 2002 & parse == "full") %>% summarise(MSPE = mean(( BIOMASS_AG_MG - (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000)^2, na.rm =TRUE),
                                                                              RMSPE = sqrt(mean(( BIOMASS_AG_MG - (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000)^2, na.rm =TRUE)),
                                                                              MAPE = mean(abs( BIOMASS_AG_MG - (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000), na.rm = TRUE))

AGB.p.o<- parse.observed.agb %>% filter(year %in% 2002 & parse == "full") %>% mutate(pred.median = (mAGB.stemwood + mAGB.stembark + mAGB.branchlive)/1000)

summary.stats <- summary(lm(BIOMASS_AG_MG  ~ pred.median, data = AGB.p.o))

                         