# code to just apply the posterior estimates of the model to the additional trees on these plots (and walk forward)
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
# Read in the rest of the FIA tree data
#--------------------------------------------------------------------------------------------- 
fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))

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

png(height = 3, width = 10, units = "in", res = 150, "data/output/barplot_additional_trees.png")
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
tv.sdi <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v4.RDS"))

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

library(rjags)
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/Regional_mu_testing_mvn-2022-05-19-20-07-51.6/IGFRegional_mvnmu_revCorr_xfixed.rds"))
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_lognormal_dist-2022-07-14-01-12-30.5/IGFRegional_inc_T0onlynoadapt.rds")) # plot random effect
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect
jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect


out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
# just get the fixed effects:

betas.df <- data.frame(betas)
betas.random <- betas.df[, grep(patter = "betaX_PLOT", colnames(betas))]
names.fixed <- names(betas.df)[!(names(betas.df) %in% colnames(betas.random))] # get the names of fixed effects
betas.fixed <- betas.df[,names.fixed]

betas.fixed.m <- reshape2::melt(betas.fixed)
model.params <- betas.fixed.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5), 
                                                                   ci.lo = quantile(value, 0.025), 
                                                                   ci.hi = quantile(value, 0.975))
colnames(model.params)[1]<- c("Parameter")


dotplot.fixed <- ggplot(model.params, aes(x= Parameter, y = median ))+geom_point()+geom_hline(aes(yintercept = 0), color = "lightgrey", linetype = "dashed")+
  geom_errorbar(aes(x = Parameter, ymin = ci.lo, ymax = ci.hi), width = 0.01)+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Coefficient  Value")

alphas <- out[,grep(pattern = "alpha",colnames(out))]
alpha.m <- reshape2::melt(alphas)

alpha.summary <- alpha.m %>% group_by(Var2) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                          ci.lo = quantile(value, 0.025, na.rm =TRUE), 
                                                          ci.hi = quantile(value, 0.975, na.rm =TRUE))

mus <- out[,c("mu","tau_TREE")]

iterate_statespace.inc <- function( x = x.mat[,"x[1,36]"],  betas.all, alpha, SDdbh, SDinc = 0, covariates) {
  
  
  
  # pseudocode for now
  tree.growth <- alpha + # sampled from tree level alpha randome effect
    # normal fixed effects
    betas.all$bMAP*covariates$MAP + 
    betas.all$bMAT*covariates$MAT +
    
    # size and SDI fixed
    betas.all$bSDI*covariates$SDI + 
    betas.all$bX*(x-30) + 
    
    # climate fixed effects
    betas.all$bppt*covariates$ppt + 
    betas.all$btmax*covariates$tmax + 
    
    # MAP interactions
    betas.all$bMAP_MAT*covariates$MAP*covariates$MAT +
    betas.all$bMAP_SDI*covariates$MAP*covariates$SDI +
    
    betas.all$bMAP_tmax*covariates$MAP*covariates$tmax +
    betas.all$bMAP_ppt*covariates$MAP*covariates$ppt +
    
    # MAT interactions
    betas.all$bMAT_SDI*covariates$MAT*covariates$SDI+
    betas.all$bMAT_tmax*covariates$MAT*covariates$tmax +
    
    betas.all$bMAT_ppt*covariates$MAT*covariates$ppt +
    
    
    # tmax and precip interactions
    betas.all$btmax_ppt*covariates$tmax*covariates$ppt +
    
    # SDI interactions
    betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
    betas.all$bSDI_ppt*covariates$SDI*covariates$ppt  
  
  tree.growth <-  ifelse(tree.growth < 0, 0, tree.growth)
  
  # Stochastic process model
  xpred <- rnorm(length(tree.growth), (tree.growth + x), SDinc) 
  
  
  xpred
  
}



simulate.xvals.from.model.oos <- function(m, nsamps = 100){
  # use the forecast function to forecast forward:
  
  treeids <- cov.data.regional %>% filter(plotid %in% x.mat[m,]$plotid) %>% dplyr::select(treeid)
  #if(length(treeids$treeid)>1){
    
  alphatreeids <- vector()
  for(i in 1:length(treeids$treeid)){
  alphatreeids[i]<- paste0("alpha_TREE[", treeids[i,], "]")
  
  }
  
    # sample 
  #alphatreeid <- paste0("alpha_TREE[", x.mat[m,]$plotid, "]")
  
  model.covs <- substring(colnames(betas), 5)
  
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  #alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  
  # write a function to get the MCMC samples
  
  get_mcmc_samples <- function(x, betas, nsamps){
    
    rnorm(nsamps, mean = mean(betas[,x]), sd = sd(betas[,x]))
  }
  
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  
  
  # sample from the population mean (mu) for the trees that dont have RE
  
  alpha <- get_mcmc_samples("mu", betas = mus, nsamps = nsamps)
  
  if(length(alphatreeids)>1){
  
  treealphas <- lapply(alphatreeids, get_mcmc_samples, betas = alphas, nsamps = nsamps)
  treealphas <- do.call(cbind, treealphas)
  colnames(treealphas)<- alphatreeids
  }else{
    treealphas <- get_mcmc_samples(betas = alphas, nsamps = nsamps)
  }
  
  
  bMAP <- get_mcmc_samples("betaMAP", betas = betas, nsamps = nsamps)
  bMAT <- get_mcmc_samples("betaMAT", betas = betas, nsamps = nsamps)
  bMAP_MAT <- get_mcmc_samples("betaMAP_MAT", betas = betas, nsamps = nsamps)
  
  bSDI <- get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  bSDI_ppt <- get_mcmc_samples("betawateryrscaled_SDIscaled", betas = betas, nsamps = nsamps)
  bSDI_tmax <- get_mcmc_samples("betatmaxAprMayJunscaled_SDIscaled", betas = betas, nsamps = nsamps)
  
  
  
  #MAP interactions:
  bMAP_ppt <- get_mcmc_samples("betaMAP_wateryrscaled", betas = betas, nsamps = nsamps)
  bMAP_tmax <- get_mcmc_samples("betaMAP_tmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  bMAP_SDI <- get_mcmc_samples("betaMAP_SDIscaled", betas = betas, nsamps = nsamps)
  
  #MAT interactions:
  bMAT_ppt <- get_mcmc_samples("betaMAT_wateryrscaled", betas = betas, nsamps = nsamps)
  bMAT_tmax <- get_mcmc_samples("betaMAT_tmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  bMAT_SDI <- get_mcmc_samples("betaMAT_SDIscaled", betas = betas, nsamps = nsamps)
  
  
  bX <-  get_mcmc_samples("betaX", betas = betas, nsamps = nsamps)
  
  
  bppt <- get_mcmc_samples("betawateryrscaled", betas = betas, nsamps = nsamps)
  btmax <- get_mcmc_samples("betatmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  btmax_ppt <- get_mcmc_samples("betatmaxAprMayJunscaled_wateryrscaled", betas = betas, nsamps = nsamps)
  
  
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
                            bMAP_SDI,
                            #MAT interactions:
                            bMAT_ppt,
                            bMAT_tmax,
                            bMAT_SDI,
                            
                            bX,
                            bppt,
                            btmax,
                            btmax_ppt)
  
  
  
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
  
  # generate alphas from the tree alphas in the same plot as the tree
  # this will be slightly different for each tree adding uncertaint
  treealphas.samp <- sample(as.vector(treealphas), size = nmcmc )
  
  for(t in 1:time_steps){
    if(t < which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if t is less than the measureyr assign NA (fo now)
      dbh.pred <- rep(NA, nMCMC)
      forecast[,t] <- dbh.pred
      inc[,t] <- dbh.pred
    }else{
      if(t == which(!is.na(x.mat[m,8:ncol(x.mat)]))){ # if the time step is the measuryr use the measureed DBH
        dbh.pred <- iterate_statespace.inc(x = x.mat[m,7+t],  betas.all = betas.all, alpha = treealphas.samp,  SDinc = 0, covariates = data.frame(SDI = covs$SDI[t], 
                                                                                                                         ppt = covs$ppt[t], 
                                                                                                                         tmax = covs$tmax[t], 
                                                                                                                         MAP = covs$MAP,
                                                                                                                         MAT = covs$MAT))
        forecast[,t] <- dbh.pred
        inc[,t]<- forecast[,t]-x.mat[m,1]
        
      }else{
        dbh.pred <- iterate_statespace.inc(x = forecast[,t-1], betas.all = betas.all, alpha = treealphas.samp, SDinc = 0, covariates = data.frame(SDI = covs$SDI[t], 
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
  cat(m)
  
  saveRDS(forecast, paste0("data/output/xvals_additional_trees/Xvals_tree_",m,".RDS"))
  # forecast.med
  
  
}


# Need to rerun this!
simulate.xvals.from.model.oos(m = 9077)
# see how long this will take:
system.time(lapply(1:10, simulate.xvals.from.model.oos))


sims.x.forecast <- lapply(1:length(unique(x.mat$CN)), simulate.xvals.from.model.oos)
x.mat2 <- do.call(cbind, sims.x.forecast)

saveRDS(x.mat2,"data/output/Xvals_noncored_IGFRegional_mvnmu_revCorr_xfixed.RDS")


#--------------------------------------------------------------------------------------------- 
# forecast from posterior estimates to get X values for 2001-2018, changing SDI values along the way
#--------------------------------------------------------------------------------------------- 

# get the estimated x values for each tree/plot (need to calculate SDI and make forecasts from 2001-2018)
out.cored <- as.matrix(readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/increment_ifelse_constraint-2022-07-20-15-30-47.9/Xvals_Regional_incifelse_T0.RDS")))

x.ci      <- apply(out.cored  , 2, quantile, c(0.025, 0.5, 0.975))
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
# x.cols.noncored   <- which(substr(colnames(out.noncored), 1, 1) == "x") # grab the state variable columns
# 
# # generate 95% CI of the DBH
ci.noncored      <- apply(out.noncored[, x.cols.noncored], 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
# #mean.pred.noncored       <- apply(out.noncored[, x.cols.noncored], 2, var) # get the var.pred for the last 800 samples
# #use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
ci.names.noncored <- parse.MatrixNames(colnames(ci.noncored), numeric = TRUE)

# # read in cored data
# Xests <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/IGF_xvals_SDI_SI.norand.X.nadapt.5000.rds"))
# str(Xests) # structure is a list of 3 MCMC chains, 
# 

# join the estimated with the forecasts

#------------------------- match up with the plot information:

# the noncored trees:

#jags.new <- readRDS("diam_data/jags.new.SDI.SICOND.norandX.tau.inc.106.rds")

cov.data.regional$treeid <- 1:length(cov.data.regional$PLOTSTATE)


#jags.data.t2t$cov.data$PLOT %in% jags.new$cov.data$PLOT

# want a dataframe that has a column for t2t_treeid
# Tree2Tree_cov.data <- jags.data.t2t$cov.data
# cores_cov.data <- jags.new$cov.data
# 
# head(cores_cov.data)



# add up all the biomass in a single plot


plots <- unique(x.mat$plotid)
plot <- plots[1]


library(dplyr)
# select trees in the plot:
cored.in.plt <- cores_cov.data %>% filter (PLOT %in% plot)
cored.in.plt$treeid
# get id of trees with out cores:
trees.in.plt <- Tree2Tree_cov.data %>% filter (PLOT %in% plot)
trees.in.plt$treeid



# get the treeids for cored and noncored datasets
x <- cored.in.plt$treeid
y <- trees.in.plt$treeid

#select the covvariate data for the cored and uncored trees:

#select the outdata for the cored and uncored trees:
sel.noncored <- which(ci.names.noncored $row %in% y)
out <- out.noncored[,sel.noncored]

combined <- jags.data.t2t$data$z[y,]

cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)
plot<- cov.data.regional$PLT_CN[1]


all.noncored <- x.mat # x.mat from dbh.spread above
all.noncored$treeid <- 1:length(x.mat$PLT_CN)
plots <- unique(x.mat$PLT_CN)#[1]
#--------------------------------------------------------------------------------------------- 
# forecast from posterior estimates to get X values
#--------------------------------------------------------------------------------------------- 


# now make a function where you calculate plot level biomass
devtools::install_github("PecanProject/pecan",subdir="base/logger")
devtools::install_github("PecanProject/pecan",subdir="modules/allometry")


library(PEcAn.allometry)
library(devtools)
library(reshape2)
library(ggplot2)
library(tidyverse)


data("allom.components")
allom.components

pfts = list(PIPO = data.frame(spcd=122,acronym='PIPO')) # list our "Pfts--plant functional types" of interest--really list the species

# Run AllomAve for each component in Kaye
kaye_pipo = AllomAve(pfts, components = c(4, 5, 8, 12, 18), ngibbs = 1000,
                     parm = "data/output/kaye_pipo.csv")

# had to read in the kaye_pipo csv...should just upload to the data
kaye.parm <- read.csv("data/output/kaye_pipo.csv")

allom.stemwood = load.allom("Allom.PIPO.4.Rdata")
allom.stembark = load.allom("Allom.PIPO.5.Rdata")
allom.branchlive = load.allom("Allom.PIPO.8.Rdata")
allom.branchdead = load.allom("Allom.PIPO.12.Rdata")
allom.foliage = load.allom("Allom.PIPO.18.Rdata")
dbh = 1:50 # vector of DBH values to predict over

source("data/output/plot2AGB_kaye.R")
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
full.clim.dt <- as.data.table(full.clim)     # data.table
microbenchmark(DT[age > 5],times=10)
full.clim$ppt.scale <- NA
full.clim$tmax.scale <- NA
x <- plot

# we will use this function to set up the future climate
scale.fut.clim.by.plt <- function(x){
  cat(x)
  full.clim.plt <-  full.clim %>% filter(PLT_CN == plot)#full.clim.dt[PLT_CN %in% plot]
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
plot <- "2449012010690"
# implement mortality stochastically based on scaled SDI of the subplot:
biomass.changingsdi.zeroinc.SDIscaled.future <- function(plot, density.dependent = TRUE, density.independent = TRUE, scenario = "rcp26"){
  
  # -------- get the diameter estimates for all trees on the plot: ------------------------
  print(as.character(plot))
  # get id of trees with cores:
  cored.in.plt <- cov.data.regional %>% dplyr::filter (PLT_CN %in% plot)
  #cored.in.plt$treeid
  
  # get id of trees with out cores:
  trees.in.plt <- all.noncored %>% dplyr::filter (PLT_CN %in% plot)
  #trees.in.plt$treeid
  
  
  
  #if(!length(trees.in.plt$PLOT) == 0){
  #cored.treeid <- cored.in.plt$treeid
  
  x <- cored.in.plt$treeid
  y <- trees.in.plt$treeid
  
  # get the subplot information
  trees.in.plt.subp <- trees.in.plt[,c("treeid", "SUBP")]
  cored.in.plt.subp <- cored.in.plt[,c("treeid", "SUBP")]
  
  combined <- rbind( cored.in.plt.subp, trees.in.plt.subp)
  #y <- y # just to reconcile the updated data with current
  m <- x[1] # since all cored trees will have the same plot information this is okay
  
  
  
  read.xvals <- function(treid){if (file.exists(paste0("data/output/xvals_additional_trees/Xvals_tree_",treid,".RDS"))) {
    readRDS(paste0("data/output/xvals_additional_trees/Xvals_tree_",treid,".RDS"))}else{rep(NA, 1000)}}
  x.additionals <- lapply(y, FUN = read.xvals)
  
  
  # calculate CIS names
  x.plots <- do.call(cbind, x.additionals)
  ci.noncored      <- apply(x.plots, 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
  mean.pred.noncored       <- apply(x.plots, 2, mean) # get the var.pred for the last 800 samples
  # #use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
  ci.names.noncored <- parse.MatrixNames(colnames(ci.noncored), numeric = TRUE)
  
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
  mean.cored <- mean.pred.cored[sel]
  
  
  # plot the posterior predictions of DBH for a single tree:
  # dont really need this step any more because we are subsetting
  sel <- which(ci.names.noncored$row %in% y) # use sel to subset the data for the 415th tree
  mean.dia.noncored <- mean.pred.noncored[sel]
  
  
  
  # for noncored trees: just seelct the 36 year time point
  tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x & ci.names.noncored$col == 36)})
  i <- do.call(cbind, tree.ind.noncrored )
  out.noncored.plt <-  x.plots[, i] 
  
  # for cored trees:
  #yrs <- 31:135
  tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x & ci.names$col == 36)}) # select just the years 1994:2010 to match the plot level data:
  i.cored <- do.call(rbind, tree.ind.cored )
  
  nmcmc <- min(length(out.cored[,1]),length(out.noncored.plt[,1]))
  
  
  out.cored.plt <-  out.cored[(length(out.cored[,1])-nmcmc + 1):length(out.cored[,1]),i.cored] 
  
  all.dbh <- cbind(out.cored.plt, out.noncored.plt)
  
  # get plot subplot and cored status for each tree:
  trees.in.plt.subp$type <- "noncored"
  cored.in.plt.subp$type <- "cored"
  index.df <- rbind(cored.in.plt.subp, trees.in.plt.subp)
  
  # make a big array with all the DBH estimates:
  
  ni <- ncol(all.dbh) # number of individuals per plot
  
  
  nMCMC <- length(all.dbh[,1])
  nt <- length(2001:2098)
  
  # fill in the array with the diameter estimates for 2018:
  dbh.pred<- increment <- array(NA, dim = c(ni, nMCMC, nt + 1))
  for(i in 1:ni){
    dbh.pred[i,,1] <- all.dbh[,i]
  }
  
  
  # set up SDI and subplot information:
  # get the SDI information form 
  #SDI.matrix
  
  
  
  #---------------------------------------------------------------------------
  ##  get all the paramter estimates + uncertainty
  #---------------------------------------------------------------------------
  
  nMCMC1 <- nMCMC-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
  
  treeids <- cored.in.plt$treeid
  #if(length(treeids$treeid)>1){
  


  
  
  
  
  
  # use all of the parameter MCMCS:
  #alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  
  # write a function to get the MCMC samples
  
  get_mcmc_samples <- function(x, betas, nsamps){
    
    rnorm(nsamps, mean = mean(betas[,x]), sd = sd(betas[,x]))
  }
  
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = 1000)
  # for each tree generate a random sample for the tree-level intercept:
  
  if(length(treeids)>1){ # if we have more than one cored tree per plot
    alphatreeids <- vector()
    for(i in 1:length(treeids)){
      alphatreeids[i]<- paste0("alpha_TREE[", treeids[i], "]")
      
    }
    
  }else{ # if there is just one cored tree per plot
    alphatreeids <- paste0("alpha_TREE[", treeids, "]")
  }
  
  
  alpha <- get_mcmc_samples("mu", betas = mus, nsamps = nsamps)
  
  
  
  # for all of the trees with cores:
  alphas.tree.plot <- list()
  
  for(i in 1:length(index.df$treeid)){
    if(index.df[i,]$type %in% "cored"){ # if the tree was cored and has the alpha estimated in posteriors
      alphas.tree.plot[[i]] <- get_mcmc_samples(x = paste0("alpha_TREE[", index.df[i,]$treeid, "]"), betas = alphas, nsamps = nsamps)
      
    }else{ # if the tree was not cored and we randomly sample from the trees in the plot
      
      if(length(alphatreeids)>1)  {
        alphas2 <- as.matrix(as.vector(alphas[,alphatreeids]))
        treealphas <- rnorm(nsamps, mean = mean(alphas2), sd = sd(alphas2))
        # treealphas <- lapply(alphatreeids, get_mcmc_samples, betas = alphas2, nsamps = nsamps)
        # treealphas <- do.call(rbind, treealphas)
        # colnames(treealphas)<- alphatreeids
      }else{
        alphaplotid <- paste0("alpha_TREE[", treeids, "]")
        treealphas <- get_mcmc_samples(x = alphaplotid, betas = alphas, nsamps = nsamps)
      }
      
      alphas.tree.plot[[i]] <-treealphas
    }
    
  }
  
  
  # need to fix the alpha...now just taking the global intercept
  # if the tree is a cored tree, include plot random effect, but if it is not, then 
  alpha <- get_mcmc_samples("mu", betas = mus, nsamps = nmcmc)
  
  
  bMAP <- get_mcmc_samples("betaMAP", betas = betas, nsamps = nmcmc)
  bMAT <- get_mcmc_samples("betaMAT", betas = betas, nsamps = nmcmc)
  bMAP_MAT <- get_mcmc_samples("betaMAP_MAT", betas = betas, nsamps = nmcmc)
  
  bSDI <- get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nmcmc)
  bSDI_ppt <- get_mcmc_samples("betawateryrscaled_SDIscaled", betas = betas, nsamps = nmcmc)
  bSDI_tmax <- get_mcmc_samples("betatmaxAprMayJunscaled_SDIscaled", betas = betas, nsamps = nmcmc)
  
  
  
  #MAP interactions:
  bMAP_ppt <- get_mcmc_samples("betaMAP_wateryrscaled", betas = betas, nsamps = nmcmc)
  bMAP_tmax <- get_mcmc_samples("betaMAP_tmaxAprMayJunscaled", betas = betas, nsamps = nmcmc)
  bMAP_SDI <- get_mcmc_samples("betaMAP_SDIscaled", betas = betas, nsamps = nmcmc)
  
  #MAT interactions:
  bMAT_ppt <- get_mcmc_samples("betaMAT_wateryrscaled", betas = betas, nsamps = nmcmc)
  bMAT_tmax <- get_mcmc_samples("betaMAT_tmaxAprMayJunscaled", betas = betas, nsamps = nmcmc)
  bMAT_SDI <- get_mcmc_samples("betaMAT_SDIscaled", betas = betas, nsamps = nmcmc)
  
  
  bX <-  get_mcmc_samples("betaX", betas = betas, nsamps = nmcmc)
  
  
  bppt <- get_mcmc_samples("betawateryrscaled", betas = betas, nsamps = nmcmc)
  btmax <- get_mcmc_samples("betatmaxAprMayJunscaled", betas = betas, nsamps = nmcmc)
  btmax_ppt <- get_mcmc_samples("betatmaxAprMayJunscaled_wateryrscaled", betas = betas, nsamps = nmcmc)
  
  
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
                            bMAP_SDI,
                            #MAT interactions:
                            bMAT_ppt,
                            bMAT_tmax,
                            bMAT_SDI,
                            
                            bX,
                            bppt,
                            btmax,
                            btmax_ppt)
  
  
  
  # get PLT_CN
  PLT_CNint <- as.character(plot)
  
  # get the unique SUBPLOTS in the plot
  subplots <- unique(SDI.mat.PLT.subp %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(SUBP))
  
  SDI.PLT <- SDI.mat.PLT.subp %>% filter(PLT_CN %in% PLT_CNint)
  
  SDI.PLT.SCALED <- SDI.PLT # get the SDI values
  
  # get the scaled SDI for the PLT:
  SDI.PLT.SCALED[,5:ncol(SDI.PLT.SCALED)] <- standardize.vector(as.matrix(SDI.PLT[,5:ncol(SDI.PLT)]))
  
  
  cat("extracting future climate for the plot")
  
  fut.clim.plot <- scale.fut.clim.by.plt(PLT_CNint)
  
  #scenario <- "rcp26"
  
  fut.clim.scen <- fut.clim.plot %>% filter(rcp %in% scenario)
  
  models <- unique(fut.clim.scen$model) # 21 models
  sample.model <- sample(models, size = length(models), replace= FALSE)
  
  
  
  get.ens.df <- function(i){
    
    ens.proj.yr <- fut.clim.scen %>% filter(model %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scale, 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(models), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  ens.means <- ens.samps.df %>% group_by(year) %>% summarise(ppt.mean = mean(ppt, na.rm =TRUE), 
                                                             tmax.mean = mean(tmax, na.rm = TRUE))
  
  ppt.fut <- ens.means %>% dplyr::select(year, ppt.mean) %>% tidyr::spread(key = year, value = ppt.mean)%>% dplyr::select(`2019`:`2098`)
  tmax.fut <- ens.means %>% dplyr::select(year, tmax.mean) %>% tidyr::spread(key = year, value = tmax.mean)%>% dplyr::select(`2019`:`2098`)
  
  ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
  
  tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
  
  
  ppt <- cbind(ppt.hist, ppt.fut)
  tmax <- cbind(tmax.hist, tmax.fut)
  SDI <- SDI.PLT.SCALED %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint ) %>% dplyr::select(SUBP,`2001`)
  
  MAP <- x.mat[m,]$MAP
  MAT <- x.mat[m,]$MAT
  
  #if(length(SDI) <=36){
  # newsdivals <- rep(SDI[36], 17)
  #  names(newsdivals)  <- 2002:2018
  #  SDI.new <- cbind(SDI, newsdivals)
  #}
  #SICOND <- cov.data[m, ]$SICOND
  #}
  
  covariates <- list()
  covariates$SDI <- as.matrix(SDI)
  covariates$ppt <- as.matrix(ppt)
  covariates$tmax <- as.matrix(tmax)
  covariates$MAP <- MAP
  covariates$MAT <- MAT
  
  
  time_steps <- length(2001:2098)
  nMCMC <- max(length(betas.all$bSDI), length(x.mat[m,1]))
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  # set up sdi matrix
  sdi.subp <-  matrix(data = NA, nrow = nrow(subplots), ncol = nt + 2)
  
  sdi.subp[,1:2] <- covariates$SDI
  
  
  # function to calculate SDI from the diameters:
  calc.sdi.subp <- function(x, j = PLT_CNint, a = s){
    
    #SDI.mat.PLT <-  SDI.PLT %>% filter(PLT_CN %in% PLT_CNint)%>% ungroup() %>% select(`1966`:`2001`)
    SDI.mat.PLT.subp.sel <-  SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`1966`:`2001`)
    
    # if only one tree is on the plot
    if(is.null(dim(x))){
      avg.dbh <- mean(x,na.rm=TRUE)
    }else{
      avg.dbh <- apply(x, 1, mean, na.rm = TRUE) # get mean of the MCMCs for each tree
    }
    # note that any dead trees will have dbh == 0, which won't add to the sum of trees in the following line
    
    SDI.new <- sum(6.01*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI, convert to inches
    
    
    #SDI.mat.PLT.subp
    
    SDIscaled <-   (SDI.new - mean(as.matrix(SDI.mat.PLT.subp.sel),na.rm=TRUE))/sd(as.matrix(SDI.mat.PLT.subp.sel),na.rm=TRUE)
    SDIscaled
  }
  
  # calculation of raw SDI
  rescale.sdi.raw <- function(SDIscaled, j = SUBPPLT_CNint, a = PLT_CNint){
    
    SDI.mat.PLT.subp.sel <-  SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`1966`:`2001`)
    
    
    #SDI.new <- sum(6.01*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI
    SDI.raw <-   mean(as.numeric(SDI.mat.PLT.subp.sel), na.rm = TRUE)*sd(as.numeric(SDI.mat.PLT.subp.sel), na.rm = TRUE)+SDIscaled
    SDI.raw
  }
  # need to reindex the data to have a a timestep (t), subplot (s), and tree (i)
  
  
  nsubp <- unique(index.df$SUBP)
  
  # for each tree get the next statespace time step:
  for(t in 1:nt){ # for each year t in the # of trees
    #if(t == 1){
    
    # loop through all of the trees for year t
    for (i in 1:ni){ # for each tree i in the n of trees
      
      SUBPLOT.index <- index.df[ni,]$SUBP # select the subplot for the tree:
      
      if(mean(dbh.pred[i,,t]) == 0){ # if the tree was killed off dbh.pred will be zero, so assign as zero
        dbh.pred[i,,t+1] <- dbh.pred[i,,t]
        
      }else{
        dbh.pred[i,,t+1] <- iterate_statespace.inc(x = dbh.pred[i,,t],  betas.all = betas.all, alpha= alphas.tree.plot[[i]], SDdbh = 0, covariates =  data.frame(SDI = sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1], 
                                                                                                                                   MAP = MAP,
                                                                                                                                   MAT= MAT,
                                                                                                                                   ppt = covariates$ppt[,t], 
                                                                                                                                   tmax = covariates$tmax[,t]))
        
      }
      increment[i,,t+1] <- dbh.pred[i,,t+1]-dbh.pred[i,,t] # calculate increment
      
      # if increment is < 0, assign as 0 & keep dbh.pred at previous value
      
      zeros <- increment[i,,t+1] <= 0 #| is.na(increment[i,,t=1])
      
      
      if(TRUE %in% zeros){ # if there are zeros estimate in the increment,
        # set increment == 0, and set the dbh to the previous years diameter
        increment[i,zeros,t+1] <- 0   
        dbh.pred[i,zeros,t+1] <- dbh.pred[i,zeros,t]
      }
      
      # if after 5 years of forecast the median increment <0 for 3 years in a row, kill off the tree:
      if(t >5){
        
        # default is that mort.prob == 0 (i.e. no mortality)
        
        mort.prob <- 0
        
        
        if(density.independent == TRUE){
          zero.means <-  colMeans(increment[i,,(t-4):t], na.rm = TRUE) <= 0 
          zero.df <- ifelse(zero.means == FALSE, 0, 1)
          mort.prob <- mean(zero.df)
        }
        
        
        # use the current SDI to determine mortality prob
        sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1]
        SDI.raw <- rescale.sdi.raw(SDIscaled =  sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1], j = unique(as.character(cored.in.plt$PLT_CN)), a = index.df[ni,]$SUBP)
        
        if(density.dependent == TRUE){
          # if the stand is above 70% of SDI and mort probability is not already 1, increase mortality probability
          # this should increase mortality liklihood across all the trees, but mostly larger trees
          if(SDI.raw > (450/nrow(sdi.subp))*0.60 & mort.prob < 1 & mean(dbh.pred[i,,t+1]) <= 38){
            # mortality probability is how close SDI of the plot is to the self-thinning law +
            #the size of the tree relative tot the max tree size of the plot
            # there are some issues with this but we can edit later
            rel.size <- (max(dbh.pred[,,t+1])-mean(dbh.pred[i,,t+1]))/max(dbh.pred[,,t+1])
            
            # change the max dbh to 75, since the max dbh at start time is 76.9
            #rel.size <- ((75)-mean(dbh.pred[i,,t+1], na.rm =TRUE))/75
            
            mort.prob <- (SDI.raw/(450/nrow(sdi.subp)))*0.60 + rel.size# mortali#/(450/nrow(sdi.subp))#+mort.prob
          
            if(!is.na(rel.size)){
            mort.prob <- ifelse(mort.prob>1, 1, mort.prob)
            
            }else{ # if rel.size is na for some reason...kill off the tree
              
              mort.prob <- 1
              
            }
          }
        }
        mort.code <- rbinom(1,1, prob = mort.prob)
      }
      # else{
      #   mort.code <- 0
      # }
        if(mort.code == 1 ){
          dbh.pred[i,,(t+1):nt] <- 0
        }
      
    }
    
    
    # before moving onto the next year, calculate the SDI
    # calculate SDI by the subplot
    for(s in sdi.subp[,1]){ # for each subplot s in the # of subplots
      # need to index dbh.pred trees by by the subplot:
      trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
      sdi.subp[which(sdi.subp[,1]==s),t+2] <- calc.sdi.subp(dbh.pred[trees.subplot,,t], j = as.character(cored.in.plt$PLT_CN), a = s)
    }
  }
  
  
  if(density.dependent == TRUE & density.independent == TRUE){
    mort.scheme <- "DIDD"
  }else{
    if(density.dependent == TRUE & density.independent ==FALSE){
      mort.scheme <- "DDonly"
    }else{
      if(density.dependent == FALSE & density.independent ==TRUE){
        mort.scheme <- "DIonly"
      }else{
        mort.scheme <- "nomort"
      }
    }}
  # save the plot-level arrays:
  saveRDS(dbh.pred, paste0("data/output/diam_forecasts/PLT.dbh.",mort.scheme,".", plot,".", scenario,".2001.2018.RDS"))
  saveRDS(sdi.subp, paste0("data/output/diam_forecasts/PLT.sdi.",mort.scheme,".", plot,".", scenario,".SUBP.2001.2018.RDS"))
  saveRDS(index.df, paste0("data/output/diam_forecasts/PLT.dbh.combined",mort.scheme,".", plot,".", scenario,".2001.2018.RDS"))
  # make a plot of all the DBH forecasts:
  
  dbh.quants <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
  colnames(dbh.quants) <- c("quantile","treeno","time", "diameter")
  
  
  dbh.quants.spread <- dbh.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
  # dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  # colnames(dbh.means) <- c("treeno","time", "diameter")
  index.df$treeno <- 1:length(index.df$treeid)
  
  dbh.means.index <- left_join(dbh.quants.spread , index.df, by = "treeno")
  
  p <- ggplot()+geom_line(data = dbh.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) + 
    geom_ribbon(data = dbh.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
  
  #ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.dbhs.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p)
  
  
  # make a plot of all the increment forecasts:
  
  inc.quants <- reshape2::melt(apply(increment, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
  colnames(inc.quants) <- c("quantile","treeno","time", "increment")
  
  
  inc.quants.spread <- inc.quants %>% group_by(treeno, time) %>% spread(quantile, increment)
  # inc.means <- reshape2::melt(apply(inc.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  # colnames(inc.means) <- c("treeno","time", "diameter")
  index.df$treeno <- 1:length(index.df$treeid)
  
  inc.means.index <- left_join(inc.quants.spread , index.df, by = "treeno")
  
  p.inc <- ggplot() + 
    geom_ribbon(data = inc.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.25)+
    geom_line(data = inc.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno))+
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))+ labs(fill = "Subplot Number", color = "Subplot Number") 
  
  #ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.increment.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p.inc)
  
  
  
  
  combined <- index.df
  out.dbh <- as.data.frame(dbh.pred)
  
  #dbh.mat <- matrix(dbh.pred, nrow = dim(dbh.pred)[2], ncol = prod(dim(dbh.pred)[1],dim(dbh.pred)[3]), byrow = TRUE)
  # dim(dbh.mat)
  # summary((dbh.mat[,1:10 ]))
  # out <- dbh.mat
  # 
  # plot(out[1,])
  
  # var 1 is tree, var2 is mcmc sample, and var 3 is time
  test.m <- melt(dbh.pred, id.vars = dim(dbh.pred)[2])
  test.m$id <- paste0("x[",test.m$Var1, ",", test.m$Var3,"]")
  
  colid.ordered <- test.m$id
  
  test.m.time <- test.m %>% dplyr::select(-Var1, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
  out <- test.m.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(colid.ordered)
  out <- out[1:50,]
  out.mean <- colMeans(out)
  # biomass estimation
  
  cat("start biomass estimates")
  plot2AGB(combined = combined, out = out, mort.scheme = mort.scheme, allom.stats = kaye_pipo, unit.conv = 0.02, plot = plot, yrvec = 2001:2098, scenario = scenario, p = p, p.inc = p.inc)
  
}



system.time(biomass.changingsdi.zeroinc.SDIscaled.future( plot = '2447353010690', density.dependent = TRUE, density.independent = TRUE, scenario = "rcp26"))
system.time(biomass.changingsdi.zeroinc.SDIscaled.future( plot = '2447900010690', density.dependent = TRUE, density.independent = FALSE, scenario = "rcp26"))
system.time(biomass.changingsdi.zeroinc.SDIscaled.future( plot ='2447900010690', density.dependent = FALSE, density.independent = TRUE, scenario = "rcp26"))
system.time(biomass.changingsdi.zeroinc.SDIscaled.future( plot = '2447900010690', density.dependent = FALSE, density.independent = FALSE, scenario = "rcp26"))

# run all the plots
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26")})

# run all the plots
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp45")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp45")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp45")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp45")})

lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp60")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp60")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp60")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp60")})

lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp85")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp85")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp85")})

#----------------Run all the same plots but with the scenarios of no climate chage-----------------
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26")})
lapply(unique(plots)[1:10],FUN = function(x){biomass.changingsdi.zeroinc.SDIscaled.future.detrend (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26")})



# some basic summaries of the 10 plots I ran:
# need a function to read in the biomass and NPP data for all plots for the given rcp scenario & mortality scheme:
# note that only rcp85 were saved because I didnt add an rcp label to the data saving process..need to fix

plot <- "2447353010690"
mort.scheme <- "DDonly"
scenario <- "rcp85"

get_biomass_ests <- function(plot, mort.scheme, scenario){
      
    load(paste0("data/output/biomass_data/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
    
    # objects
    # out, AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
    # AGB.foliage, NPP.foliage, 
    # AGB.stembark, NPP.stembark,
    # AGB.stemwood, NPP.stemwood,
    # AGB.branchdead, NPP.branchdead,
    # AGB.branchlive, NPP.branchlive,
  i <- 1
  mNPP[i, ] <- apply( NPP[i, , ], 2, median, na.rm = TRUE)
  sNPP[i, ] <- apply(NPP[i, , ], 2, sd, na.rm = TRUE)
  mAGB[i, ] <- apply(AGB[i, , ], 2, median, na.rm = TRUE)
  sAGB[i, ] <- apply(AGB[i, , ], 2, sd, na.rm = TRUE)
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  
  # branch dead
  mNPP.branchdead[i, ] <- apply( NPP.branchdead[i, , ], 2, median, na.rm = TRUE)
  sNPP.branchdead[i, ] <- apply(NPP.branchdead[i, , ], 2, sd, na.rm = TRUE)
  mAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, median, na.rm = TRUE)
  sAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, sd, na.rm = TRUE)
  
  # foliage
  mNPP.foliage[i, ] <- apply( NPP.foliage[i, , ] , 2, median, na.rm = TRUE)
  sNPP.foliage[i, ] <- apply(NPP.foliage[i, , ] , 2, sd, na.rm = TRUE)
  mAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, mean, na.rm = TRUE)
  sAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, sd, na.rm = TRUE)
  
  # stembark
  mNPP.stembark[i, ] <- apply( NPP.stembark[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stembark[i, ] <- apply(NPP.stembark[i, , ] , 2, sd, na.rm = TRUE)
  mAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, median, na.rm = TRUE)
  sAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, sd, na.rm = TRUE)
  
  # branchlive
  mNPP.branchlive[i, ] <- apply( NPP.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sNPP.branchlive[i, ] <- apply(NPP.branchlive[i, , ], 2, sd, na.rm = TRUE)
  mAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  
  
  # stemwood
  mNPP.stemwood[i, ] <- apply( NPP.stemwood[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stemwood[i, ] <- apply(NPP.stemwood[i, , ], 2, sd, na.rm = TRUE)
  mAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ], 2, median, na.rm = TRUE)
  sAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  
  up  <- mNPP[i, ] + sNPP[i, ] * 1.96
  low <- mNPP[i, ] - sNPP[i, ] * 1.96
  
  up.stemwood  <- mNPP.stemwood[i, ] + sNPP.stemwood[i, ] * 1.96
  low.stemwood <- mNPP.stemwood[i, ] - sNPP.stemwood[i, ] * 1.96
  
  up.stembark  <- mNPP.stembark[i, ] + sNPP.stembark[i, ] * 1.96
  low.stembark <- mNPP.stembark[i, ] - sNPP.stembark[i, ] * 1.96
  
  up.branchlive  <- mNPP.branchlive [i, ] + sNPP.branchlive [i, ] * 1.96
  low.branchlive  <- mNPP.branchlive[i, ] - sNPP.branchlive[i, ] * 1.96
  
  up.branchdead  <- mNPP.branchdead [i, ] + sNPP.branchdead [i, ] * 1.96
  low.branchdead  <- mNPP.branchdead[i, ] - sNPP.branchdead[i, ] * 1.96
  
  up.foliage  <- mNPP.foliage  [i, ] + sNPP.foliage  [i, ] * 1.96
  low.foliage   <- mNPP.foliage [i, ] - sNPP.foliage [i, ] * 1.96
  
  # plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
  # lines(yrvec[-1], up)
  # lines(yrvec[-1], low)
  upA  <- mAGB[i, ] + sAGB[i, ] * 1.96
  lowA <- mAGB[i, ] - sAGB[i, ] * 1.96
  
  
  upA.stemwood  <- mAGB.stemwood[i, ] + sAGB.stemwood[i, ] * 1.96
  lowA.stemwood <- mAGB.stemwood[i, ] - sAGB.stemwood[i, ] * 1.96
  
  upA.stembark  <- mAGB.stembark[i, ] + sAGB.stembark[i, ] * 1.96
  lowA.stembark <- mAGB.stembark[i, ] - sAGB.stembark[i, ] * 1.96
  
  upA.branchlive  <- mAGB.branchlive [i, ] + sAGB.branchlive [i, ] * 1.96
  lowA.branchlive  <- mAGB.branchlive[i, ] - sAGB.branchlive[i, ] * 1.96
  
  upA.branchdead  <- mAGB.branchdead [i, ] + sAGB.branchdead [i, ] * 1.96
  lowA.branchdead  <- mAGB.branchdead[i, ] - sAGB.branchdead[i, ] * 1.96
  
  upA.foliage  <- mAGB.foliage  [i, ] + sAGB.foliage  [i, ] * 1.96
  lowA.foliage   <- mAGB.foliage [i, ] - sAGB.foliage [i, ] * 1.96
  
 
  i <- 1
  # calculate upper and lower bounds
  up  <- mNPP[i, ] + sNPP[i, ] * 1.96
  low <- mNPP[i, ] - sNPP[i, ] * 1.96
  
  upA  <- mAGB[i, ] + sAGB[i, ] * 1.96
  lowA <- mAGB[i, ] - sAGB[i, ] * 1.96
  
    
    total.plot <- data.frame(plot = plot, 
                             mort.scheme = mort.scheme,
                             rcp = scenario,
                             year = yrvec[2:length(low.stemwood)], 
                             mAGB = mAGB[,2:length(low.stemwood)], 
                             mAGB.stemwood = mAGB.stemwood[,2:length(low.stemwood)],
                             mAGB.stembark = mAGB.stembark[,2:length(low.stemwood)],
                             mAGB.branchlive = mAGB.branchlive[,2:length(low.stemwood)],
                             mAGB.branchdead = mAGB.branchdead[,2:length(low.stemwood)],
                             mAGB.foliage = mAGB.foliage[,2:length(low.stemwood)],
                             upA = upA[2:length(low.stemwood)], 
                             lowA = lowA[2:length(low.stemwood)], 
                             upA.stemwood = upA.stemwood[2:length(low.stemwood)],
                             upA.stembark = upA.stembark[2:length(low.stemwood)],
                             upA.branchlive = upA.branchlive[2:length(low.stemwood)],
                             upA.branchdead = upA.branchdead[2:length(low.stemwood)],
                             upA.foliage = upA.foliage[2:length(low.stemwood)],
                             
                             lowA.stemwood = lowA.stemwood[2:length(low.stemwood)],
                             lowA.stembark = lowA.stembark[2:length(low.stemwood)],
                             lowA.branchlive = lowA.branchlive[2:length(low.stemwood)],
                             lowA.branchdead = lowA.branchdead[2:length(low.stemwood)],
                             lowA.foliage = lowA.foliage[2:length(low.stemwood)],
                             
                             mNPP = mNPP[,2:length(low.stemwood)], 
                             mNPP.stemwood = mNPP.stemwood[2:length(low.stemwood)],
                             mNPP.stembark =mNPP.stembark[2:length(low.stemwood)],
                             mNPP.branchlive =mNPP.branchlive[2:length(low.stemwood)],
                             mNPP.branchdead = mNPP.branchdead[2:length(low.stemwood)],
                             mNPP.foliage = mNPP.foliage[2:length(low.stemwood)],                          
                             up = up[2:length(low.stemwood)], 
                             low = low[2:length(low.stemwood)], 
                             
                             
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

get_biomass_ests_ncc <- function(plot, mort.scheme, scenario){
  
  load(paste0("data/output/biomass_data_nocc/plot2AGB_",mort.scheme,".", plot,".",scenario, ".Rdata"))
  
  # objects
  # out, AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
  # AGB.foliage, NPP.foliage, 
  # AGB.stembark, NPP.stembark,
  # AGB.stemwood, NPP.stemwood,
  # AGB.branchdead, NPP.branchdead,
  # AGB.branchlive, NPP.branchlive,
  i <- 1
  mNPP[i, ] <- apply( NPP[i, , ], 2, median, na.rm = TRUE)
  sNPP[i, ] <- apply(NPP[i, , ], 2, sd, na.rm = TRUE)
  mAGB[i, ] <- apply(AGB[i, , ], 2, median, na.rm = TRUE)
  sAGB[i, ] <- apply(AGB[i, , ], 2, sd, na.rm = TRUE)
  
  # sequentially add up:
  # branchdead, then foliage, then stembark, then branchlive, then stemwood
  
  # branch dead
  mNPP.branchdead[i, ] <- apply( NPP.branchdead[i, , ], 2, median, na.rm = TRUE)
  sNPP.branchdead[i, ] <- apply(NPP.branchdead[i, , ], 2, sd, na.rm = TRUE)
  mAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, median, na.rm = TRUE)
  sAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, sd, na.rm = TRUE)
  
  # foliage
  mNPP.foliage[i, ] <- apply( NPP.foliage[i, , ] , 2, median, na.rm = TRUE)
  sNPP.foliage[i, ] <- apply(NPP.foliage[i, , ] , 2, sd, na.rm = TRUE)
  mAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, mean, na.rm = TRUE)
  sAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, sd, na.rm = TRUE)
  
  # stembark
  mNPP.stembark[i, ] <- apply( NPP.stembark[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stembark[i, ] <- apply(NPP.stembark[i, , ] , 2, sd, na.rm = TRUE)
  mAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, median, na.rm = TRUE)
  sAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, sd, na.rm = TRUE)
  
  # branchlive
  mNPP.branchlive[i, ] <- apply( NPP.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sNPP.branchlive[i, ] <- apply(NPP.branchlive[i, , ], 2, sd, na.rm = TRUE)
  mAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, median, na.rm = TRUE)
  sAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  
  
  # stemwood
  mNPP.stemwood[i, ] <- apply( NPP.stemwood[i, , ] , 2, median, na.rm = TRUE)
  sNPP.stemwood[i, ] <- apply(NPP.stemwood[i, , ], 2, sd, na.rm = TRUE)
  mAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ], 2, median, na.rm = TRUE)
  sAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
  
  
  up  <- mNPP[i, ] + sNPP[i, ] * 1.96
  low <- mNPP[i, ] - sNPP[i, ] * 1.96
  
  up.stemwood  <- mNPP.stemwood[i, ] + sNPP.stemwood[i, ] * 1.96
  low.stemwood <- mNPP.stemwood[i, ] - sNPP.stemwood[i, ] * 1.96
  
  up.stembark  <- mNPP.stembark[i, ] + sNPP.stembark[i, ] * 1.96
  low.stembark <- mNPP.stembark[i, ] - sNPP.stembark[i, ] * 1.96
  
  up.branchlive  <- mNPP.branchlive [i, ] + sNPP.branchlive [i, ] * 1.96
  low.branchlive  <- mNPP.branchlive[i, ] - sNPP.branchlive[i, ] * 1.96
  
  up.branchdead  <- mNPP.branchdead [i, ] + sNPP.branchdead [i, ] * 1.96
  low.branchdead  <- mNPP.branchdead[i, ] - sNPP.branchdead[i, ] * 1.96
  
  up.foliage  <- mNPP.foliage  [i, ] + sNPP.foliage  [i, ] * 1.96
  low.foliage   <- mNPP.foliage [i, ] - sNPP.foliage [i, ] * 1.96
  
  # plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
  # lines(yrvec[-1], up)
  # lines(yrvec[-1], low)
  upA  <- mAGB[i, ] + sAGB[i, ] * 1.96
  lowA <- mAGB[i, ] - sAGB[i, ] * 1.96
  
  
  upA.stemwood  <- mAGB.stemwood[i, ] + sAGB.stemwood[i, ] * 1.96
  lowA.stemwood <- mAGB.stemwood[i, ] - sAGB.stemwood[i, ] * 1.96
  
  upA.stembark  <- mAGB.stembark[i, ] + sAGB.stembark[i, ] * 1.96
  lowA.stembark <- mAGB.stembark[i, ] - sAGB.stembark[i, ] * 1.96
  
  upA.branchlive  <- mAGB.branchlive [i, ] + sAGB.branchlive [i, ] * 1.96
  lowA.branchlive  <- mAGB.branchlive[i, ] - sAGB.branchlive[i, ] * 1.96
  
  upA.branchdead  <- mAGB.branchdead [i, ] + sAGB.branchdead [i, ] * 1.96
  lowA.branchdead  <- mAGB.branchdead[i, ] - sAGB.branchdead[i, ] * 1.96
  
  upA.foliage  <- mAGB.foliage  [i, ] + sAGB.foliage  [i, ] * 1.96
  lowA.foliage   <- mAGB.foliage [i, ] - sAGB.foliage [i, ] * 1.96
  
  
  i <- 1
  # calculate upper and lower bounds
  up  <- mNPP[i, ] + sNPP[i, ] * 1.96
  low <- mNPP[i, ] - sNPP[i, ] * 1.96
  
  upA  <- mAGB[i, ] + sAGB[i, ] * 1.96
  lowA <- mAGB[i, ] - sAGB[i, ] * 1.96
  
  
  total.plot <- data.frame(plot = plot, 
                           mort.scheme = mort.scheme,
                           rcp = "no climate change",
                           year = yrvec[2:length(low.stemwood)], 
                           mAGB = mAGB[,2:length(low.stemwood)], 
                           mAGB.stemwood = mAGB.stemwood[,2:length(low.stemwood)],
                           mAGB.stembark = mAGB.stembark[,2:length(low.stemwood)],
                           mAGB.branchlive = mAGB.branchlive[,2:length(low.stemwood)],
                           mAGB.branchdead = mAGB.branchdead[,2:length(low.stemwood)],
                           mAGB.foliage = mAGB.foliage[,2:length(low.stemwood)],
                           upA = upA[2:length(low.stemwood)], 
                           lowA = lowA[2:length(low.stemwood)], 
                           upA.stemwood = upA.stemwood[2:length(low.stemwood)],
                           upA.stembark = upA.stembark[2:length(low.stemwood)],
                           upA.branchlive = upA.branchlive[2:length(low.stemwood)],
                           upA.branchdead = upA.branchdead[2:length(low.stemwood)],
                           upA.foliage = upA.foliage[2:length(low.stemwood)],
                           
                           lowA.stemwood = lowA.stemwood[2:length(low.stemwood)],
                           lowA.stembark = lowA.stembark[2:length(low.stemwood)],
                           lowA.branchlive = lowA.branchlive[2:length(low.stemwood)],
                           lowA.branchdead = lowA.branchdead[2:length(low.stemwood)],
                           lowA.foliage = lowA.foliage[2:length(low.stemwood)],
                           
                           mNPP = mNPP[,2:length(low.stemwood)], 
                           mNPP.stemwood = mNPP.stemwood[2:length(low.stemwood)],
                           mNPP.stembark =mNPP.stembark[2:length(low.stemwood)],
                           mNPP.branchlive =mNPP.branchlive[2:length(low.stemwood)],
                           mNPP.branchdead = mNPP.branchdead[2:length(low.stemwood)],
                           mNPP.foliage = mNPP.foliage[2:length(low.stemwood)],                          
                           up = up[2:length(low.stemwood)], 
                           low = low[2:length(low.stemwood)], 
                           
                           
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
get_biomass_ests(plot = "2447353010690", mort.scheme = "DDonly",scenario = "rcp85")


# RCP8.5
normort.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp85")
DIonly.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp85")
DDonly.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp85")
DIDD.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp85")

# RCP 60:
normort.AGB.60 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp60")
DIonly.AGB.60 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp60")
DDonly.AGB.60 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp60")
DIDD.AGB.60 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp60")


# RCP 4.5:
normort.AGB.45 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp45")
DIonly.AGB.45 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp45")
DDonly.AGB.45 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp45")
DIDD.AGB.45 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp45")


# RCP 2.6:
normort.AGB.26 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "nomort", scenario = "rcp26")
DIonly.AGB.26 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIonly", scenario = "rcp26")
DDonly.AGB.26 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DDonly", scenario = "rcp26")
DIDD.AGB.26 <- lapply(unique(plots)[1:10], FUN = get_biomass_ests, mort.scheme = "DIDD", scenario = "rcp26")



# no climate change scenarios:
nocc.nomort.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests_ncc, mort.scheme = "nomort", scenario = "rcp26")
nocc.DIonly.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests_ncc, mort.scheme = "DIonly", scenario = "rcp26")
nocc.DDonly.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests_ncc, mort.scheme = "DDonly", scenario = "rcp26")
nocc.DIDD.AGB <- lapply(unique(plots)[1:10], FUN = get_biomass_ests_ncc, mort.scheme = "DIDD", scenario = "rcp26")

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


ten.plot.summary <- all10plots %>% group_by(mort.scheme, rcp, year) %>% summarise_at(.vars = vars(mAGB:low.foliage), .funs = sum, na.rm = TRUE)


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
  theme_bw()+facet_grid(cols =  vars(rcp), rows = vars(mort.scheme))+
  ylab(paste("ten plot total biomass \n  (Mg/ha)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1" ))

b.plot

png(height = 10, width = 12, units = "in", res = 150, "data/output/Total_biomass_kaye_ten_plots_allrcps_nocc.png")
b.plot
dev.off()

tota.plot.trunk <- ten.plot.summary %>% filter(year <= 2099)
yhi <- max(  tota.plot.trunk$up.stemwood, na.rm = TRUE) + 1
ylow <- min(  tota.plot.trunk$low.branchdead, na.rm=TRUE) - 1

b.flux <- ggplot()+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = low.stemwood, ymax = up.stemwood, fill = "stem wood"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = low.branchlive, ymax = up.branchlive, fill = "live branch"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = low.stembark, ymax = up.stembark, fill = "stem bark"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = low.foliage, ymax = up.foliage, fill = "foliage"))+
  geom_ribbon(data = ten.plot.summary, aes(x = year, ymin = low.branchdead, ymax = up.branchdead, fill = "dead branch"))+
  theme_bw()+facet_grid(cols =  vars(rcp), rows = vars(mort.scheme), scales = "free_y")+#xlim(2001,2018)+ylim(ylow, yhi)+
  ylab(paste("Total biomass increment, \n RCP 8.5, ten plots (Mg/ha)"))+xlab("Year")+theme(panel.grid = element_blank())+
  scale_fill_manual(name = 'Biomass Component', 
                    values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1" ))

# save thes summaries:

png(height = 10, width = 12, units = "in", res = 150, "data/output/NPP_biomass_kaye_ten_plots_allrcps_nocc.png")
b.flux
dev.off()


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