library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
# code to just apply the posterior estimates of the model to the additional trees on these plots (and walk forward)
# some functions:
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
TREEinPLOTS <- TREE %>% filter(AGENTCD >= 0 & STATUSCD ==1 & DIA > 1) %>% filter(PLT_CN %in% unique(cov.data.regional$PLT_CN) & !CN %in% cov.data.regional$TRE_CN)
length(TREEinPLOTS$CN)

additional.trees <- TREEinPLOTS %>% group_by(SPCD) %>% summarise(number = n())

# png(height = 3, width = 10, units = "in", res = 150, "data/output/barplot_additional_trees.png")
# ggplot(additional.trees, aes(x = as.character(SPCD), y = number))+geom_bar(stat = "identity")
# dev.off()
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

dbh.measyr.newtrees <- TREEinPLOTS %>% select(CN, PLT_CN, SUBP,MEASYR, DIA) %>% mutate(DIA = DIA*2.54)

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

x.mat <- merge(unique.plts, spread.dbh.mat, by.x = c("PLT_CN"))
m <- 1
# get time series data:
# read in the larger region climate data:
#pipo.clim <- read.csv("data/pipo_all_tmean_ppt_v5.csv")

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

tmax.fallspr <- get_ordered_climate("tmax.fallspr")
wintP.wateryr <- get_ordered_climate("wintP.wateryr")
tmax.AprMayJun <- get_ordered_climate("tmax.AprMayJun")
tmax.monsoon <- get_ordered_climate("tmax.monsoon")
TMAX <- get_ordered_climate("TMAX")

# -------------------------------------------------------------------------------
# read in the SDI time plot level data:
#--------------------------------------------------------------------------------
tv.sdi <- readRDS("data/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v5.RDS")

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
  x.bar <- mean(as.vector(x), na.rm = TRUE)
  s.d. <- sd(x, na.rm = TRUE)
  return((x-x.bar)/s.d.)
}

#time_data$TMAX.scaled <- standardize.mat(TMAX)

#time_data$tmax.fallspr.scaled <- standardize.mat(time_data$tmax.fallspr)
wateryrscaled <- wintP.wateryr
tmaxAprMayJunscaled <- tmax.AprMayJun
SDIscaled <- SDI.mat.PLT.subp # note this is not ordered (it might be but i havent checked)

wateryrscaled[,4:ncol(wateryrscaled)] <- standardize.mat(as.matrix(wintP.wateryr[,4:ncol(wintP.wateryr)]))
tmaxAprMayJunscaled[,4:ncol(tmaxAprMayJunscaled)] <- standardize.mat(as.matrix(tmax.AprMayJun[,4:ncol(tmax.AprMayJun)]))
SDIscaled[,5:ncol(SDIscaled)] <- standardize.vector(as.matrix(SDI.mat.PLT.subp[,5:ncol(SDI.mat.PLT.subp)]))


#--------------------------------------------------------------------------------------------- 
# Read in the posterior parameter estimates
#--------------------------------------------------------------------------------------------- 
# this model models increment, not diameter...
# I didnt save the Xvals for this model, but just using it to get the code setup


#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/Regional_mu_testing_mvn-2022-05-19-20-07-51.6/IGFRegional_mvnmu_revCorr_xfixed.rds"))
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_lognormal_dist-2022-07-14-01-12-30.5/IGFRegional_inc_T0onlynoadapt.rds")) # plot random effect
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect
jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect


# READ IN STAN OUTPUT SUMMARY
#STAN.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect
#model.params<- read.csv("/Users/kellyheilman/Documents/SSM_small_test/model_simple_run/small_ssm_stan_sigma_dbh_normal_0.15_0.8_0_1constraint_parameter_summary.csv")
model.params<- read.csv("/Users/kellyheilman/Documents/SSM_small_test/full.ssm.working.model_parameter_summary.csv")

output.base.name <- "Regional_stan_model"
# out <- as.matrix(jags.comb)
# summary(out)
# betas <- out[,grep(pattern = "beta",colnames(out))]
# # just get the fixed effects:
# 
# betas.df <- data.frame(betas)
# betas.random <- betas.df[, grep(patter = "betaX_PLOT", colnames(betas))]
# names.fixed <- names(betas.df)[!(names(betas.df) %in% colnames(betas.random))] # get the names of fixed effects
# betas.fixed <- betas.df[,names.fixed]
# 
# betas.fixed.m <- reshape2::melt(betas.fixed)
# model.params <- betas.fixed.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5), 
#                                                                    ci.lo = quantile(value, 0.025), 
#                                                                    ci.hi = quantile(value, 0.975))
# colnames(model.params)[1]<- c("Parameter")
# 
# 
# dotplot.fixed <- ggplot(model.params, aes(x= Parameter, y = median ))+geom_point()+geom_hline(aes(yintercept = 0), color = "lightgrey", linetype = "dashed")+
#   geom_errorbar(aes(x = Parameter, ymin = ci.lo, ymax = ci.hi), width = 0.01)+theme_bw(base_size = 12)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Coefficient  Value")
# 
# alphas <- out[,grep(pattern = "alpha",colnames(out))]
# alpha.m <- reshape2::melt(alphas)
# 
# alpha.summary <- alpha.m %>% group_by(Var2) %>% summarise(mean = mean(value, na.rm = TRUE), 
#                                                           ci.lo = quantile(value, 0.025, na.rm =TRUE), 
#                                                           ci.hi = quantile(value, 0.975, na.rm =TRUE))
year.randoms <- read.csv("/Users/kellyheilman/Documents/SSM_small_test/full.ssm.working.model_year_RE_summary.csv")

mus <- model.params %>% filter(L1 %in% c("mu","sigma_TREE"))
betas <- model.params
# get year and tree random effects from STAN model


iterate_statespace.inc <- function( x = x.mat[,"x[1,36]"],  betas.all, beta_YEARid,  SDdbh = 0, SDinc = 0, covariates) {
  
  
  
  # pseudocode for now
  tree.growth <- betas.all$alpha + beta_YEARid +# sampled from tree level alpha randome effect
    # normal fixed effects
    betas.all$bMAP*covariates$MAP + 
    betas.all$bMAT*covariates$MAT +
    
    # size and SDI fixed
    betas.all$bSDI*covariates$SDI + 
    betas.all$bX*(x) + 
    
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
    betas.all$bSDI_ppt*covariates$SDI*covariates$ppt  
    
    # X interactions
    betas.all$bX_MAP*covariates$MAP + 
    betas.all$bX_MAT*covariates$MAT*x + 
    betas.all$bX_Precip*covariates$ppt*x + 
    betas.all$bX_Tmax*covariates$tmax*x + 
    betas.all$bX_SDI*covariates$SDI*x 
  
  tree.growth <-  ifelse(tree.growth < 0, 0, tree.growth) # we shouldn't need this but keeping in
  
  # Stochastic process model
  #incpred <- tree.growth
  
  xpred <- rnorm(length(tree.growth), (tree.growth + x), SDdbh) 
  
  
  xpred
  
}

cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)

simulate.xvals.from.model.oos <- function(m, nsamps = 100){
  # use the forecast function to forecast forward:
  
  treeids <- cov.data.regional %>% filter(plotid %in% x.mat[m,]$plotid) %>% dplyr::select(treeid)
  #if(length(treeids$treeid)>1){
  
  # alphatreeids <- vector()
  # for(i in 1:length(treeids$treeid)){
  #   alphatreeids[i]<- paste0("alpha_TREE[", treeids[i,], "]")
  #   
  # }
  # 
  # sample 
  #alphatreeid <- paste0("alpha_TREE[", x.mat[m,]$plotid, "]")
  
  #model.covs <- substring(colnames(betas), 5)
  
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  #alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  
  # write a function to get the MCMC samples

  get_mcmc_samples <- function(x, betas, nsamps){
    
    rnorm(nsamps, mean = as.numeric(betas %>% filter(L1 %in% x) %>% select(median)), sd =  as.numeric(betas %>% filter(L1 %in% x) %>% select(sd)))
  }
  
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  
  
  # sample from the population mean (mu) for the trees that dont have RE
  
  alpha <- get_mcmc_samples(x = "mu", betas = mus, nsamps = nsamps)
  
  # if(length(alphatreeids)>1){
  #   
  #   treealphas <- lapply(alphatreeids, get_mcmc_samples, betas = alphas, nsamps = nsamps)
  #   treealphas <- do.call(cbind, treealphas)
  #   colnames(treealphas)<- alphatreeids
  # }else{
  #   treealphas <- get_mcmc_samples(betas = alphas, nsamps = nsamps)
  # }
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
  
  # generate samples from the yearly random effects
  
  betayrVEC <- matrix(NA, ncol = time_steps, nrow = nMCMC)
  
  for(t in 1:time_steps){
    betayrVEC[,t] <- get_mcmc_samples(paste0("beta_YEAR[", t, "]"), betas = year.randoms, nsamps = nsamps)
  }
  
  for(t in 1:time_steps){
    if(t < which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if t is less than the measureyr assign NA (fo now)
      dbh.pred <- rep(NA, nMCMC)
      forecast[,t] <- dbh.pred
      inc[,t] <- dbh.pred
    }else{
      if(t == which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if the time step is the measuryr use the measureed DBH
        dbh.pred <- iterate_statespace.inc(x = x.mat[m,7+t],  betas.all = betas.all, beta_YEARid = betayrVEC[,t], SDinc = 0, covariates = data.frame(SDI = covs$SDI[t], 
                                                                                                                                                  ppt = covs$ppt[t], 
                                                                                                                                                  tmax = covs$tmax[t], 
                                                                                                                                                  MAP = covs$MAP,
                                                                                                                                                  MAT = covs$MAT))
        forecast[,t] <- dbh.pred
        inc[,t]<- forecast[,t]-x.mat[m,1]
        
      }else{
        dbh.pred <- iterate_statespace.inc(x = forecast[,t-1], betas.all = betas.all,beta_YEARid = betayrVEC[,t], SDinc = 0, covariates = data.frame(SDI = covs$SDI[t], 
                                                                                                                                                  ppt = covs$ppt[t], 
                                                                                                                                                  tmax = covs$tmax[t], 
                                                                                                                                                  MAP = covs$MAP,
                                                                                                                                                  MAT = covs$MAT))
        forecast[,t] <- dbh.pred
        inc[,t]<- forecast[,t]-forecast[,t-1]
        
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




# see how long this will take:
system.time(sims.x.forecast<- lapply(1:10, simulate.xvals.from.model.oos))
#3.8 user time multiplied by ~1500 =95 mintues 

sims.x.forecast <- lapply(1:length(unique(x.mat$CN)), simulate.xvals.from.model.oos)
x.mat2 <- do.call(cbind, sims.x.forecast)

saveRDS(x.mat2, paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))
x.mat2 <- readRDS(paste0("data/Xval_noncored_stan.",output.base.name,".RDS"))
#x.mat2 <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/mortality_future_sensitivity-2022-09-22-21-03-44.7/Xval_noncored.Regional_incifelse_T0.RDS"))
#--------------------------------------------------------------------------------------------- 
# forecast all trees on the plot from posterior estimates to get X values for 2001-2018, changing SDI values along the way
#--------------------------------------------------------------------------------------------- 

# get the estimated x values for each tree/plot (need to calculate SDI and make forecasts from 2001-2018)
#out.cored <- as.matrix(readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/increment_ifelse_constraint-2022-07-20-15-30-47.9/Xvals_Regional_incifelse_T0.RDS")))
out.cored <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/xcored_estimated_full.ssm.working.model.rds")
#head(out.cored)
#out.cored <- as.matrix(x.mat2)
x.ci      <- apply(out.cored  , 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
#    <- as.matrix(Xests) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out.cored), 1, 1) == "x") # grab the state variable columns
# 
# # generate 95% CI of the DBH
ci      <- apply(out.cored[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
mean.pred.cored       <- apply(out.cored[, x.cols], 2, mean) # get the var.pred for the last 800 samples
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

# #select the covvariate data for the cored and uncored trees:

#select the outdata for the cored and uncored trees:
sel.noncored <- which(ci.names.noncored $row %in% y)
out <- out.noncored[,sel.noncored]


cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)
plot<- cov.data.regional$PLT_CN[1]


all.noncored <- x.mat # x.mat from dbh.spread above
all.noncored$treeid <- 1:length(x.mat$PLT_CN)
plots <- unique(x.mat$PLT_CN)#[1]


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
source("R/Allom_Ave.R")
source("R/read.allom.data.R")
source("R/query.allom.data.R")
source("R/allom.BayesFit.R")
# Run AllomAve for each component in Kaye
kaye_pipo = AllomAve(pfts, components = c(4, 5, 8, 12, 18), ngibbs = 1000,
                     parm = "data/kaye_pipo.csv")

# had to read in the kaye_pipo csv...should just upload to the data
kaye.parm <- read.csv("data/kaye_pipo.csv")

# allom.stemwood = load.allom("Allom.PIPO.4.Rdata")
# allom.stembark = load.allom("Allom.PIPO.5.Rdata")
# allom.branchlive = load.allom("Allom.PIPO.8.Rdata")
# allom.branchdead = load.allom("Allom.PIPO.12.Rdata")
# allom.foliage = load.allom("Allom.PIPO.18.Rdata")

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

plot = '2482066010690'
density.dependent = FALSE
density.independent = FALSE
rcp <- "rcp26"

#-----------------------------------------------------------------------
# read in future climate, which has been mean corrected and downscaled:
#-----------------------------------------------------------------------

full.clim <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/future_climate_extraction-2022-08-12-23-22-14.1/full_time_mean_corrected_CMIP5_model_timeseriesIW.RDS"))

#-----------------------------------------------------------------------
# need to scale future climate data on the same scale as the past climate
#-----------------------------------------------------------------------
clim.data <- time_data

# note that each plot is locally scaled, so we will need to apply that to each future timeseries
x <- plot

library(data.table)
# full.clim.dt <- as.data.table(full.clim)     # data.table
# microbenchmark(DT[age > 5],times=10)
full.clim$ppt.scale <- NA
full.clim$tmax.scale <- NA
x <- plot

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






#system.time(scale.fut.clim.by.plt(1))

# if we looped over all plots this would take 7 hours!
# 40.75*length(unique(cov.data.regional$PLT_CN))
# [1] 27669.25
# > (40.75*length(unique(cov.data.regional$PLT_CN)))/60
# [1] 461.1542
# > 461.15/60
# [1] 7.685833

# Need to find a faster way of doing this

# 
# rm(tmax.fallspr)
# climate.ensemble.means <- clim.ts.df %>% group_by(lat, lon, year, rcp) %>% 
#   dplyr::summarise(mean.tmax.fs = mean(tmax.scaled, na.rm = TRUE), 
#                    SD.tmax = var(tmax.scaled, na.rm = TRUE),
#                    mean.ppt = mean(ppt.scale, na.rm = TRUE), 
#                    SD.ppt = sd(ppt.scale, na.rm = TRUE),
#                    n = n()) 

set.seed(22)
plot <- "2904285010690"
# implement mortality stochastically based on scaled SDI of the subplot:
unique(plots)
source("R/plot2AGB_kayeFVS.R")
source("R/biomass.sensitivity.periodic.R")
# run the function that makes all of the forecasts
system.time(biomass.sensitivity.periodic( plot = '2567520010690', density.dependent = TRUE, density.independent = TRUE, scenario = "rcp85", SDI.ratio.DD = 0.8, aggressiveCC = FALSE))
system.time(biomass.sensitivity.periodic( plot = '2873938010690', density.dependent = TRUE, density.independent = FALSE, scenario = "rcp26", SDI.ratio.DD = 0.8, aggressiveCC = FALSE))
system.time(biomass.sensitivity.periodic( plot ='2447900010690', density.dependent = FALSE, density.independent = TRUE, scenario = "rcp26", SDI.ratio.DD = 0.8, aggressiveCC = FALSE))
system.time(biomass.sensitivity.periodic( plot = '2447900010690', density.dependent = FALSE, density.independent = FALSE, scenario = "rcp26", SDI.ratio.DD = 0.8, aggressiveCC = FALSE))

# run all the plots for this scenario and 
# started at 12:12 pm....
lapply(unique(plots)[1:675],FUN = function(x){biomass.sensitivity.periodic(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.8, aggressiveCC = FALSE)})
lapply(unique(plots)[1:675],FUN = function(x){biomass.sensitivity.periodic(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp85", SDI.ratio.DD = 0.8, aggressiveCC = FALSE)})
lapply(unique(plots)[250:675],FUN = function(x){biomass.sensitivity.periodic(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp60", SDI.ratio.DD = 0.8, aggressiveCC = FALSE)})
#6.0 stopped at: plot "2483452010690"
# 4.5 stopped at plot: "2483452010690"
lapply(unique(plots)[250:675],FUN = function(x){biomass.sensitivity.periodic(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp45", SDI.ratio.DD = 0.8, aggressiveCC = FALSE)})
plot <- "2483452010690"
scenario = "rcp45"

#lapply(unique(plots)[273:274],FUN = function(x){biomass.sensitivity.periodic(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.8, aggressiveCC = FALSE)})
#stopped with plot "2587295010690" (plot 250)
unique(plots) %in% "2483452010690"
# plot 180
plot <- 2567520010690
# 2461254010690"ates no tmaxcalculating biomass: percent complete complete
# extracting future climate for the plot2461254010690Error in `filter()`:
# ! Problem while computing `..1 = PLT_CN == x`.
# Caused by error:
# ! vector memory exhausted (limit reached?)
# Run `rlang::last_error()` to see where th

plot <- "3215491010690"
unique(plots) %in% 3215491010690
# Error in `$<-.data.frame`(`*tmp*`, "TPA_UNADJ", value = c(6.01805, 6.01805 : 
#                                                             replacement has 2 rows, data has 3
lapply(unique(plots)[600:675],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26")})
# stopped wtih plot "plot2874190010690" # plot 383..384...with this error...Skip for now
unique(plots) %in% "3170850010690" # plot 578
plot <- "3320827010690"
# Error in if (mean(dbh.pred[i, , t]) == 0) { : 
#     missing value where TRUE/FALSE needed

unique(plots) %in% "2611722010690"

lapply(unique(plots)[1:675],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26")})
lapply(unique(plots)[664:675],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26")})
# plot 663 throwing an error
# Error in if (score > best$score && (!only.loose || (lmin <= dmin && lmax >=  : 
#                                                 missing value where TRUE/FALSE needed

# run all the plots
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp45")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp45")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp45")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp45")})

lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp60")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp60")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp60")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp60")})

lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp85")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp85")})

#----------------Run all the same plots but with the scenarios of no climate chage-----------------

#biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = unique(plots)[1], density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26")

plot <- "2482552010690"

lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26")})
lapply(unique(plots)[185:284],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26")})

lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp45")})

# redo the non-cc runs above
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp45")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp45")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp45")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp60")})

lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp60")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp60")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp60")})

lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp85")})
lapply(unique(plots)[11:50],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp85")})


plot <- "2449012010690"




#-----------------------------Plotting Forecasts--------------------------------
# need a function to read in the biomass and NPP data for all plots for the given rcp scenario & mortality scheme:
# note that only rcp85 were saved because I didnt add an rcp label to the data saving process..need to fix

plot <- "2447353010690"
mort.scheme <- "DIDD"
scenario <- "rcp26"

get_biomass_ests <- function(plot, mort.scheme, scenario){
  
  load(paste0("biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  
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
  hiNPP <- hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
  
  
  
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
                           
                           up.dead = up.dead[2:length(low.stemwood)], 
                           low.dead = low.dead[2:length(low.stemwood)],
                           
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
  
  load(paste0("biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  
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
  hiNPP <- hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- lowNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
  
  
  
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
                           year = yrvec[2:length(low.stemwood)], 
                           mort.scheme = mort.scheme,
                           rcp = "no climate change", 
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
                           
                           up.dead = up.dead[2:length(low.stemwood)], 
                           low.dead = low.dead[2:length(low.stemwood)],
                           
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
get_biomass_ests(plot = "2447353010690", mort.scheme = "DIDD",scenario = "rcp85")




# RCP8.5
normort.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp85")
DIonly.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp85")
DDonly.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp85")
DIDD.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp85")

# RCP 60:
normort.AGB.60 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp60")
DIonly.AGB.60 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp60")
DDonly.AGB.60 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp60")
DIDD.AGB.60 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp60")


# RCP 4.5:
normort.AGB.45 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp45")
DIonly.AGB.45 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp45")
DDonly.AGB.45 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp45")
DIDD.AGB.45 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp45")


# RCP 2.6:
normort.AGB.26 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp26")
DIonly.AGB.26 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp26")
DDonly.AGB.26 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp26")
DIDD.AGB.26 <- lapply(unique(plots)[1:100], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp26")



# no climate change scenarios:
nocc.nomort.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests_ncc, mort.scheme = "nomort", scenario = "rcp26")
nocc.DIonly.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests_ncc, mort.scheme = "DIonly", scenario = "rcp26")
nocc.DDonly.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests_ncc, mort.scheme = "DDonly", scenario = "rcp26")
nocc.DIDD.AGB <- lapply(unique(plots)[1:100], FUN = get_biomass_ests_ncc, mort.scheme = "DIDD", scenario = "rcp26")

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

AGB.line <- ggplot()+geom_line(data = ten.plot.summary, aes(year, mAGB, group = mort.scheme, color = mort.scheme))+theme_bw()+ylab("Total AGB for 10 plots \n (Mg/ha), RCP 8.5")+facet_wrap(~rcp)
NPP.line <- ggplot(ten.plot.summary, aes(year, mNPP, group = mort.scheme, color = mort.scheme))+geom_line()+theme_bw()+ylab("Total NPP for 10  plots \n (Mg/ha), RCP 8.5")+facet_wrap(~rcp)

png(height = 7, width = 10, units = "in", res = 150, "data/output/allrcps.example10plots.total.biomass.png")
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

png(height = 10, width = 12, units = "in", res = 150, "data/output/Total_biomass_kaye_ten_plots_allrcps_nocc.png")
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

png(height = 10, width = 12, units = "in", res = 150, "data/output/NPP_biomass_kaye_ten_plots_allrcps_nocc.png")
b.flux
dev.off()

# try a plot that is stacked barplots:

# big tree vs small tree carbon in forecasts
get_tree_levelC_ests <- function(plot, mort.scheme, scenario, nocc = FALSE){
  if(nocc == FALSE){
    load(paste0("biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  }else{
    load(paste0("biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
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
btst.AGB.DIDD.26 <- lapply(unique(plots)[1:83], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26")})
btst.AGB.DIDD.26.df <- do.call(rbind, btst.AGB.DIDD.26)
btst.AGB.DIDD.26.df

btst.AGB.nomort.26<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26")})
btst.AGB.nomort.26.df <- do.call(rbind, btst.AGB.nomort.26)

btst.AGB.DDonly.26<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26")})
btst.AGB.DDonly.26.df <- do.call(rbind, btst.AGB.DDonly.26)

btst.AGB.DIonly.26<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26")})
btst.AGB.DIonly.26.df <- do.call(rbind, btst.AGB.DIonly.26)

# RCP 4.5
btst.AGB.DIDD.45 <- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp45")})
btst.AGB.DIDD.45.df <- do.call(rbind, btst.AGB.DIDD.45)

btst.AGB.nomort.45<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp45")})
btst.AGB.nomort.45.df <- do.call(rbind, btst.AGB.nomort.45)

btst.AGB.DDonly.45<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp45")})
btst.AGB.DDonly.45.df <- do.call(rbind, btst.AGB.DDonly.45)

btst.AGB.DIonly.45<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp45")})
btst.AGB.DIonly.45.df <- do.call(rbind, btst.AGB.DIonly.45)

# RCP 6.0
btst.AGB.DIDD.60 <- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp60")})
btst.AGB.DIDD.60.df <- do.call(rbind, btst.AGB.DIDD.60)

btst.AGB.nomort.60<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp60")})
btst.AGB.nomort.60.df <- do.call(rbind, btst.AGB.nomort.60)

btst.AGB.DDonly.60<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp60")})
btst.AGB.DDonly.60.df <- do.call(rbind, btst.AGB.DDonly.60)

btst.AGB.DIonly.60<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp60")})
btst.AGB.DIonly.60.df <- do.call(rbind, btst.AGB.DIonly.60)

# RCP 8.5
btst.AGB.DIDD.85 <- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp85")})
btst.AGB.DIDD.85.df <- do.call(rbind, btst.AGB.DIDD.85)

btst.AGB.nomort.85<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp85")})
btst.AGB.nomort.85.df <- do.call(rbind, btst.AGB.nomort.85)

btst.AGB.DDonly.85<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp85")})
btst.AGB.DDonly.85.df <- do.call(rbind, btst.AGB.DDonly.85)

btst.AGB.DIonly.85<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp85")})
btst.AGB.DIonly.85.df <- do.call(rbind, btst.AGB.DIonly.85)

# no climate change scenario:
# read in and get the tree level estimates
# RCP 2.6
btst.AGB.DIDD.nocc <- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIDD", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DIDD.nocc.df <- do.call(rbind, btst.AGB.DIDD.nocc)
btst.AGB.DIDD.nocc.df

btst.AGB.nomort.nocc<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "nomort", scenario = "rcp26", nocc = TRUE)})
btst.AGB.nomort.nocc.df <- do.call(rbind, btst.AGB.nomort.nocc)

btst.AGB.DDonly.nocc<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DDonly", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DDonly.nocc.df <- do.call(rbind, btst.AGB.DDonly.nocc)

btst.AGB.DIonly.nocc<- lapply(unique(plots)[1:50], FUN = function(x){get_tree_levelC_ests(plot = x, mort.scheme = "DIonly", scenario = "rcp26", nocc = TRUE)})
btst.AGB.DIonly.nocc.df <- do.call(rbind, btst.AGB.DIonly.nocc)


# combine all the tree-level datasets together:
allplots.treeC <- rbind(btst.AGB.nomort.26.df, btst.AGB.DIonly.26.df, btst.AGB.DDonly.26.df, btst.AGB.DIDD.26.df, 
                        btst.AGB.nomort.45.df, btst.AGB.DIonly.45.df, btst.AGB.DDonly.45.df, btst.AGB.DIDD.45.df,
                        btst.AGB.nomort.60.df, btst.AGB.DIonly.60.df, btst.AGB.DDonly.60.df, btst.AGB.DIDD.60.df, 
                        btst.AGB.nomort.85.df, btst.AGB.DIonly.85.df, btst.AGB.DDonly.85.df, btst.AGB.DIDD.85.df, 
                        btst.AGB.nomort.nocc.df, btst.AGB.DIonly.nocc.df, btst.AGB.DDonly.nocc.df, btst.AGB.DIDD.nocc.df)


# ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% 
#   summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)


png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_all_50_plots.png")
ggplot(na.omit(allplots.treeC), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

# example for how single plots may vary quite alot!
png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_one_plot.png")
ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[1]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_two_plot.png")

ggplot(na.omit(allplots.treeC) %>% filter(plot %in% unique(allplots.treeC$plot)[2]), aes(x = time, y = AGB, fill = size_class))+geom_bar(stat = 'identity')+
  ylab("Median AGB (Mg/ha)")+theme_bw()+theme(panel.grid = element_blank())+
  ggtitle(paste("Plot ", unique(allplots.treeC$plot)[1]))+
  scale_fill_manual(name = 'Size class', 
                    values =c("big tree"="#e66101","small tree"="#5e3c99"))+facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_three_plot.png")

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

png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_dominated_nplots.png")

ggplot()+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.big), color = "#e66101")+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.small), color= "#5e3c99")+
  geom_line(data = whole.regionC , aes(x=time,y = nplots.mixed), color= "black")+
  scale_color_manual(name = 'Size class', 
                     values =c("big tree dominated"="#e66101","small tree dominated"="#5e3c99", "mixed dominated" = "black"))+
  facet_grid(cols =  vars(scenario), rows = vars(mort.scheme))+
  
  ylab(paste("number of plots dominated by big vs small trees"))+    xlab("Year")+
  
  theme_bw()+theme(panel.grid = element_blank())

dev.off()
# plot the number of plots dominated by small vs large trees:
png(height = 10, width = 10, units = "in", res = 150, "big_tree_vs_small_tree_dominated_prop_biomass.png")

ggplot()+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=prop.big.all, ymax=prop.big.all + prop.small.all), fill = "#e66101")+
  geom_ribbon(data = whole.regionC , aes(x=time,ymin=0, ymax=prop.big.all), fill = "#5e3c99")+
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






# calculate the fire flux breakpoint, for each year, as a percent of the total AGB:
firebreakpoints <- ten.plot.summary %>% group_by(rcp, mort.scheme, year) %>% summarise(ffbp = mNPP,
                                                                                       ffbp_pct = (mNPP/mAGB)*100, 
                                                                                       netsink = ifelse(mNPP > 0, "net sink", "net source"))

ggplot(firebreakpoints, aes(year, ffbp_pct, color = netsink))+geom_point()+facet_wrap(~mort.scheme)

ggplot(firebreakpoints, aes(year, ffbp, color = netsink))+geom_point()+facet_wrap(~mort.scheme)

lapply(unique(plots)[10:100],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[10:100],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[10:100],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26")})
lapply(unique(plots)[10:100],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaledv(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26")})

# should we just feed the means & 95% CI to the plt2AGB? to speed it up...not sure why it takes forever
# or only use 100 samples
### issues:

# SDI is off because the original calculation was for SDI at the subplot level, but calculation in above code is at the plot level
# this plot (2560687010690) has 4 subplots, each sampled in 1997, and a total of 31 trees > diameter 1, but in the additional trees there are 44?!
# These additional trees have status_cd == 0 or a mortality agent code listed....so these are dead trees at the time of sampling in 1997
# we went to omit these from our estimates
# rerunning the prediction to omit these trees that were already dead...
# then need to do the SDI calculation at the subplot level so will need to link to SUBP information 
# which is in the original x.mat in the first section of teh codee (used in sims.x.forecast)

# update 6/13/22:
# SDI is now changing with the SDI supplot level...
# reran the predictions omitting trees that were already dead so this isnt as off, but three are some sharp declines in diameter over time
# This might just be due to the dry climate 
# need to rerunt the additional tree forecasts for this model to account for the subplot SDI values, not just the plot values

# update 6/14/22:
# modifying code to prevent diameter from being consistently negative from year to year

# update 8/5/22:
# biomass estimates are taking really long...we need to speed this up, so right now
# just limiting the number of samples
# another issue is that we are running it with a model that has the plot-level random effects, so most trees are similar in increment
# need to rerun the script with tree-level random effects, which means changing PLOTSTATE to TREE everywhere
# 