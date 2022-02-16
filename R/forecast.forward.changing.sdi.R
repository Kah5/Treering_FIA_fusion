# script to make forecasts of tree level responses to climate change 
# and to parse climate response vs. changes in tree size.

# basic idea: 
# Use posteriors to forecast:
# 1. Initial Condition Uncertainty
# 2. Parameter Uncertainty
# 3. Process Uncertainty
# 4. Driver uncertainty (?)
library(tidyverse)
library(psych)
library(gridExtra)
library(pryr)
library(cowplot)
library(rjags)
library(coda)


stage2 = FALSE
output.base.name <- "Fixed_effects_interactions_site_quality_g"
setwd("/home/rstudio")

xvalurl <- "inserturlhere"
clim.naurl <- "inserturlhere"
cored.xvalests <- "inserturlhere"
noncored.xvalests <- "inserturlhere"
jags.t2tlink <- "inserturlhere"
jags.newlink <- "inserturlhere"


# assuming that we already ran the conditional_effects_mcmc_plots script, and that output.base.name is in our env
jags.comb.params <- readRDS(file=paste0("IGF",output.base.name,".rds"))
jags.comb.params <- readRDS(file=paste0("/home/rstudio/data/input/data/IGFFull.model.validation.nadapt5000.rds"))

out <- as.matrix(jags.comb.params)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]


# get the alpha_PLOT intercept random effects:
alphas <- out[,grep(pattern = "alpha_PLOT",colnames(out))]

# get the betaX_PLOT slope random effects:
# note that I forgot to save the betaX_PLOTS on this run, so we dont have these estimates, but lets build the framework for generating the postior ests:
betaXplots <- out[,grep(pattern = "betaX_PLOT",colnames(out))]

# get the estimated x values for each tree/plot:
xval.ests <- readRDS(xvalurl)
x.mat <- as.matrix(xval.ests)
x.ci      <- apply(x.mat , 2, quantile, c(0.025, 0.5, 0.975))
x.ci[, "x[1,24]"]
hist(x.mat[,"x[1,45]"])

# make sure we have the average climate conditions for each plot/tree:
# We have plot MAP and MAT standarized in cov.data:


# Add the plot index to cov.data
plot.table <- data.frame(PLOT = unique(cov.data$PLOT), 
                         plotid = 1:length(unique(cov.data$PLOT)))
cov.data <- jags.new$cov.data
cov.data$PLOT <- as.numeric(as.character(cov.data$PLOT))
cov.data <- left_join(plot.table, cov.data, by  = "PLOT")
cov.data$id <- 1:length(cov.data$PLOT)

# KH note: need to standardize SDIseq first sdece I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

# read in clim.data so we can properly scale:

clim.data <- readRDS("PRISM_non_scaled.rds")

# lets get the average climate for the 2020's:
clim.na <- readRDS(clim.naurl)

clim.na$cov.data$MAP.2020s.stand <-(clim.na$cov.data$MAP.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.2020s.stand <-(clim.na$cov.data$tmax.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))

# for rcp 8.5:
clim.na$cov.data$MAP.85.2020s.stand <-(clim.na$cov.data$MAP.85.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.85.2020s.stand <-(clim.na$cov.data$tmax85.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))


cov.data$MAP.85.2020s.stand <- clim.na$cov.data$MAP.85.2020s.stand 
cov.data$tmax.85.2020s.stand <-clim.na$cov.data$tmax.85.2020s.stand 



# get the time series of future climate:

clim.ts <- readRDS("data/input/data/pipo.cores.ds.mean.correct.climate_2018_2099.RDS")
colnames(clim.ts)[6:7] <-c("year.ppt", "tmax.fall.spr") 

clim.ts.df <- clim.ts #$future.climate.ts
clim.ts.df$tmax.fall.spr[is.nan(clim.ts.df$tmax.fall.spr)] <- NA
#tmax.fallspr.df <- tmax.fallspr


# need to scale future climate data on the same scale as the past climate

unscale_function <- function(zVar, myVar){(zVar * sd(myVar)) + mean(myVar)}


clim.ts.df$ppt.scale <-(clim.ts.df$year.ppt-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.ts.df$tmax.scaled <-(clim.ts.df$tmax.fall.spr-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))

# merge climate data with jags.cov data for stage 1 to get plot number, then crosswalk climate from there...why dont the LL's match? rounding

temp2.df.ll <- temp2 [, c("CountyNo", "PlotNo", "PLOT_LAT", "PLOT_LON")]
temp2.df.ll$PLOT <- as.numeric(paste0(temp2.df.ll$CountyNo, temp2.df.ll$PlotNo))
cov.data.test <- left_join(cov.data, temp2.df.ll, by = "PLOT")

unique(paste0(cov.data$PLOT)) %in% unique(paste0(temp2$CountyNo,temp2$PlotNo))  

# note that the plot lat and long are slightly different. Looks like one might be subplot and the other plot
plot(cov.data.test$LON, cov.data.test$PLOT_LON)
abline(a = 0, b = 1)

plot(cov.data.test$LAT, cov.data.test$PLOT_LAT)
abline(a = 0, b = 1)

# use the plot lat and plot lon to join with cliamte ensemble means

rm(tmax.fallspr)
climate.ensemble.means <- clim.ts.df %>% group_by(lat, lon, year, rcp) %>% 
  dplyr::summarise(mean.tmax.fs = quantile(tmax.scaled, na.rm = TRUE, 0.5), 
                   SD.tmax = var(tmax.scaled, na.rm = TRUE),
                   mean.ppt = quantile(ppt.scale, na.rm = TRUE, 0.5), 
                   SD.ppt = sd(ppt.scale, na.rm = TRUE),
                   n = n()) 
hist(climate.ensemble.means$mean.ppt )
hist(climate.ensemble.means$mean.tmax.fs )
#climate.ensemble.means$SD.tmax <- rnorm(length(climate.ensemble.means$SD.tmax), mean = 3, sd = 2)

new.table <- cov.data.test[,c("PLOT_LAT", "PLOT_LON",  "id", "plotid")]
colnames(new.table)[1:2] <- c("lat", "lon")

# merge with the climate.ensemble.means data base:
ens.means <- merge(new.table, climate.ensemble.means, by = c("lat", "lon"))
sample.ll <- ens.means[ens.means$id %in% 81,]
clim.ts.df.full <- left_join(new.table, clim.ts.df, by = c("lat", "lon"))
#ts.all  <- merge(new.table, clim.ts.df, by = c("lat", "lon"))
ts.all <- clim.ts.df.full
head(time_data)

head(ts.all)
ggplot(ts.all[ts.all$id %in% 81,], aes(x = year, y = year.ppt, color = modelrun))+geom_line()#+facet_wrap(~rcp)
ggplot(ts.all[ts.all$id %in% 92,], aes(x = as.numeric(year), y = tmax.fall.spr, color = modelrun))+geom_line() + stat_smooth()#+facet_wrap(~rcp)
ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = rcp))+geom_boxplot()#+facet_wrap(~rcp)

ts.all$period <- ifelse(ts.all$year >= 2018 & ts.all$year <2050, "2018 - 2050",
                        ifelse(ts.all$year >= 2050 & ts.all$year <2070, "2050 - 2070","2070-2099"))
ensemble.temp.plot <- ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, fill = period))+geom_boxplot()+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), legend.title= element_blank())+ylab("Fall - Spring Maximum Temperature")+xlab("Scenario")

ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = period))+geom_boxplot()+facet_wrap(~modelrun)
ensemble.ppt.plot <-ggplot(ts.all, aes(x = rcp, y = year.ppt, fill = period))+geom_boxplot()+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), legend.title= element_blank())+ylab("Total Precipitation")+xlab("Scenario")

png(height = 6, width = 10, units = "in", res = 300, "future_climate_ensemble_boxplots.png")
cowplot::plot_grid(ensemble.ppt.plot, ensemble.temp.plot, ncol = 2, align = "hv")
dev.off()

set.seed (11)



# ------------------------------------------------------------------------------------
# example single tree forecast for 1 step:
# ------------------------------------------------------------------------------------
# 1: select all trees in a single plot
# 2: forecast each tree in a plot forward one step;
# 3: calculate SDI from all the trees
# 4: forecast each tree forward

# get X estimates for noncored
Xests.noncored <- readRDS(noncored.xvalests)
# join the estimated with the forecasts
out.noncored      <- as.matrix(Xests.noncored)### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols.noncored   <- which(substr(colnames(out.noncored), 1, 1) == "x") # grab the state variable columns

# generate 95% CI of the DBH
ci.noncored      <- apply(out.noncored[, x.cols.noncored], 2, quantile, c(0.025, 0.5, 0.975))
mean.pred.noncored       <- apply(out.noncored[, x.cols.noncored], 2, var) # get the var.pred for the last 800 samples
#use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
ci.names.noncored <- parse.MatrixNames(colnames(ci.noncored), numeric = TRUE)

# read in cored data
Xests <- readRDS(cored.xvalests)
str(Xests) # structure is a list of 3 MCMC chains, 


# join the estimated with the forecasts
out.cored      <- as.matrix(Xests)[7001:7800,] ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out.cored), 1, 1) == "x") # grab the state variable columns

# generate 95% CI of the DBH
ci      <- apply(out.cored[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
mean.pred.cored       <- apply(out.cored[, x.cols], 2, mean) # get the var.pred for the last 800 samples


ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)

#------------------------- get the plot information:
#jags.data.t2t<- readRDS("data/jags.data.5794.stage2.rds")
jags.data.t2t<-readRDS(jags.t2tlink )
jags.data.t2t$cov.data$treeid <- 1:length(jags.data.t2t$cov.data$PLOT)


#jags.new <- readRDS("diam_data/jags.new.SDI.SICOND.norandX.tau.inc.106.rds")
jags.new <- readRDS(jags.newlink)
jags.new$cov.data$treeid <- 1:length(jags.new$cov.data$PLOT)
jags.data.t2t$cov.data$PLOT %in% jags.new$cov.data$PLOT

# want a dataframe that has a column for t2t_treeid
Tree2Tree_cov.data <- jags.data.t2t$cov.data
cores_cov.data <- jags.new$cov.data

head(cores_cov.data)



# add up all the biomass in a single plot


t2t.plot <- unique(Tree2Tree_cov.data$PLOT)
cores.plot <- unique(cores_cov.data$PLOT)


Tree2Tree_cov.data<- Tree2Tree_cov.data %>% filter(treeid <5680)
plots <- unique(Tree2Tree_cov.data$PLOT, cores_cov.data$PLOT)

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

# create a function that will generate the forecasts for all the trees in a given plot, walk forward, calculate SDI, then walk forward again

plot_plot2agb<- function(plot){
  
  print(as.character(plot))
  # get id of trees with cores:
  cored.in.plt <- cores_cov.data %>% dplyr::filter (PLOT %in% plot)
  cored.in.plt$treeid
  # get id of trees with out cores:
  trees.in.plt <- Tree2Tree_cov.data %>% dplyr::filter (PLOT %in% plot)
  trees.in.plt$treeid
  
  if(!length(trees.in.plt$PLOT) == 0){
    #cored.treeid <- cored.in.plt$treeid
    
    x <- cored.in.plt$treeid
    y <- trees.in.plt$treeid
    
    y <- y[y<5680] # just to reconcile the updated data with current
    
    # plot the posterior predictions of DBH for a single tree:
    sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
    mean.cored <- mean.pred.cored[sel]
    
    
    # plot the posterior predictions of DBH for a single tree:
    sel <- which(ci.names.noncored$row %in% y) # use sel to subset the data for the 415th tree
    mean.dia.noncored <- mean.pred.noncored[sel]
    
    
    # if y is in x, add 
    
    # filter the forecasts too and add onto these databases:
    cored.f <- dbh.all.unc %>% filter(treeid %in% x) %>% filter(year > 2018)
    uncored.f <-dbh.all.unc.noncored %>% filter(treeid %in% y) %>% filter(year > 2018)
    
    forecast.cored <- data.frame(diameter = cored.f$mean, 
                                 tree =cored.f$treeid, 
                                 time = rep(54:134, each = length(x)), 
                                 year = rep(2019:2099, each = length (x)))
    
    forecast.uncored <- data.frame(diameter = uncored.f$mean, 
                                   tree = paste0(uncored.f$treeid,"_nc"), 
                                   time = rep(18:98, each = length(y)), 
                                   year = rep(2019:2099, each = length (y)))
    
    # reorganise the cored and non-cored selected data into a combined value:
    # includes forecasts now
    noncored.mean.dia.df <- data.frame(diameter = mean.dia.noncored, 
                                       tree = rep(paste0(y, "_nc"), length(c(1:106))), 
                                       time = rep(c(1:106), each = length(y)), 
                                       year = rep(c(1994:2099), each = length(y)))
    
    #noncored.mean.dia.df <- rbind(noncored.mean.dia.df, forecast.uncored)
    
    # for the cored, now includes forecasts as well
    cored.mean.dia.df <- data.frame(diameter = mean.cored, 
                                    tree = rep(x, 134), 
                                    time = rep(1:134, each = length(x)), 
                                    year = rep(1966:2099, each = length(x)))
    
    #cored.mean.dia.df<- rbind(cored.mean.dia.df, forecast.cored)
    
    combined.long <- rbind(cored.mean.dia.df, noncored.mean.dia.df)
    #combined.long$id <- 1:length(combined.long$diameter)
    
    combined.mat <- combined.long %>% dplyr::select(-time) %>%  group_by(tree) %>% spread(year, value = diameter, drop = TRUE)
    combined <- combined.mat %>% ungroup()%>% dplyr::select(-tree) %>% dplyr::select(`1995`:`2099`)
    
    
    
    # --------join together the out dataframes of interest:
    
    # for noncored trees:
    tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x)})
    i <- do.call(cbind, tree.ind.noncrored )
    out.noncored.plt <-  out.noncored[, i] 
    
    # for cored trees:
    yrs <- 31:135
    tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x)}) # select just the years 1994:2010 to match the plot level data:
    i.cored <- do.call(rbind, tree.ind.cored )
    out.cored.plt <-  out.cored[,i.cored] 
    
    # select just the years 1994:2010 to match the plot level data:
    cored.names <- parse.MatrixNames(colnames(out.cored.plt), numeric = TRUE)
    tree.yr.cored <- lapply(X = yrs, FUN= function(yrs){which(cored.names$col == yrs)}) # select just the years 1994:2010 to match the plot level data:
    i.yr.cored <- do.call(rbind, tree.yr.cored )
    out.cored.plt.yrs <-  out.cored.plt[,i.yr.cored] 
    
    # combine them into an out value:
    out.mat <- cbind(out.cored.plt.yrs, out.noncored.plt)
    
    out <- out.mat
    
    # get the last X value for each tree in the plot:
    
    
    # run through this function
    summary.out <- plot2Forecats(combined = combined, out = out, outfolder = "forecasts_plot2AGB_outputs", allom.stats = kaye_pipo, unit.conv = unit.conversion, plot = plot)
    summary.out
  }else{cat("only 1 tree in plot")}
}


plot <- unique(plots)[5]
#system.time(plot_plot2agb(plot = plot))

plot <- 17990
plots <- unique(cores_cov.data$PLOT)
unique(plots)[12]
#biomass.plots <- lapply(unique(plots), plot_plot2agb)

#cov.data <- cores_cov.data
#pipo.plots.df <- do.call(rbind, biomass.plots)
#colnames(pipo.plots.df)[1] <- "PLOT"
m <- 1
scenario <- "rcp26"


# generic function to make forecasts from SSM
iterate_statespace.dbh <- function( x = x.mat[,"x[1,53]"], m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {
  
  #j <- 1
  
  
  # pseudocode for now
  tree.growth <- x +  alpha + betas.all$b0 + 
    betas.all$bSDI*covariates$SDI + 
    betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
    betas.all$bX*(x-30) + 
    betas.all$bX2*(x-30)*(x-30) + 
    betas.all$bX_SDI*(x-30)*covariates$SDI + 
    betas.all$bX_ppt*covariates$ppt*(x-30) + 
    betas.all$bSI*covariates$SICOND + 
    betas.all$bSI_X*(x-30)*covariates$SICOND + 
    betas.all$bSI_wintP.wateryr*covariates$ppt*covariates$SICOND + 
    betas.all$bSI_tmax.fallspr*covariates$ppt*covariates$SICOND + 
    betas.all$bppt*covariates$ppt + 
    betas.all$btmax*covariates$tmax + 
    betas.all$bX_tmax*(x-30)*covariates$tmax + 
    betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
    betas.all$btmax_ppt*covariates$tmax*covariates$ppt +
    betas.all$bSDI_SI*covariates$SDI*covariates$SICOND
  #tree.growth
  
  
  
  # Stochastic process model
  xpred <- rnorm(length(tree.growth), mean=tree.growth, SDdbh) 
  xpred[is.nan(xpred)]<- NA
  #inc = xpred - x
  #yinc <-  rnorm(length(inc), inc, SDinc) 
  
  #if(rw == "ringwidth"){
  #  yinc
  #}else{
  xpred
  #}
}



# function that gathers all the records from the plot:
# 1. walks SSM forward one step, makes individual tree level forecasts
# 2. estimates plot-level SDI
# 3. then repeat 1 and 2 until 2099:
m <- 1 # individual cored tree with plot
scenario <- "rcp26" # scenario for future forecasts:
plot <- unique(plots)[5] # unique plot id
xcored.data # estimated tree sizes for all cored trees
xnoncored.data # estimated tree sizes for all noncored trees on the plot



forecast.dbh.changingsdi <- function(plot, scenario){
  
  # -------- get the diameter estimates for all trees on the plot: ------------------------
  print(as.character(plot))
  # get id of trees with cores:
  cored.in.plt <- cores_cov.data %>% dplyr::filter (PLOT %in% plot)
  cored.in.plt$treeid
  
  # get id of trees with out cores:
  trees.in.plt <- Tree2Tree_cov.data %>% dplyr::filter (PLOT %in% plot)
  trees.in.plt$treeid
  
  #if(!length(trees.in.plt$PLOT) == 0){
  #cored.treeid <- cored.in.plt$treeid
  
  x <- cored.in.plt$treeid
  y <- trees.in.plt$treeid
  
  y <- y[y<5680] # just to reconcile the updated data with current
  m <- x[1] # since all cored trees will have the same plot information this is okay
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
  mean.cored <- mean.pred.cored[sel]
  
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names.noncored$row %in% y) # use sel to subset the data for the 415th tree
  mean.dia.noncored <- mean.pred.noncored[sel]
  
  
  
  # for noncored trees:
  tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x & ci.names.noncored$col == 25)})
  i <- do.call(cbind, tree.ind.noncrored )
  out.noncored.plt <-  out.noncored[, i] 
  
  # for cored trees:
  #yrs <- 31:135
  tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x & ci.names$col == 53)}) # select just the years 1994:2010 to match the plot level data:
  i.cored <- do.call(rbind, tree.ind.cored )
  out.cored.plt <-  out.cored[,i.cored] 
  all.dbh <- cbind(out.cored.plt, out.noncored.plt)
  
  # make a big array with all the DBH estimates:
  
  ni <- ncol(all.dbh) # number of individuals per plot
  
  
  nMCMC <- length(all.dbh[,1])
  nt <- 82
  
  # fill in the array with the diameter estimates for 2018:
  dbh.pred <- array(NA, dim = c(ni, nMCMC, nt + 1))
  for(i in 1:ni){
    dbh.pred[i,,1] <- all.dbh[,i]
  }
  
  
  #---------------------------------------------------------------------------
  ##  get all the paramter estimates + uncertainty
  #---------------------------------------------------------------------------
  
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  nMCMC1 <- nMCMC-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
  
  # use all of the parameter MCMCS:
  alpha = rep(quantile(alphas[, alphaplotid],0.5), nMCMC1)
  #alpha <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]))
  bSDI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI"]))
  bSDI_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]))
  if(!"betaX" %in% colnames(betas)){
    bX <-  rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]), sd = sd(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]))
    
  }else{
    bX <-  rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX"]))
    
  }
  bX2 <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX2"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX2"]))
  
  if("betaX_SDI" %in% colnames(betas)){
    bX_SDI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_SDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_SDI"]))
    
  }else{
    bX_SDI <- rep(0, nMCMC)
  }
  mcmc.its<- (length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_X <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_wintP.wateryr <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_tmax.fallspr <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSDI_SI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
  }else{
    bSI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND"]), sd = sd(betas[mcmc.its,"betaSICOND"]))
    bSI_X <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaX_SICOND"]), sd = sd(betas[mcmc.its,"betaX_SICOND"]))
    bSI_wintP.wateryr <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_wintP.wateryr"]), sd = sd(betas[mcmc.its,"betaSICOND_wintP.wateryr"]))
    
    bSI_tmax.fallspr <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_tmax.fallspr"]), sd = sd(betas[mcmc.its,"betaSICOND_tmax.fallspr"]))
    #bSDI_SI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI_SICOND"])), mean = mean(betas[mcmc.its,"betaSDI_SICOND"]), sd = sd(betas[mcmc.its,"betaSDI_SICOND"]))
    bSDI_SI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    
  }
  
  bX_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]))
  bppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betawintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betawintP.wateryr"]))
  btmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr"]))
  bX_tmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  btmax_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(B0[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])]), sd = sd(B0[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])]))
  
  
  alpha <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt,  bSDI_SI)
  
  
  future.proj <- unique(ens.means[ens.means$id == m & ens.means$rcp == scenario, ])
  proj.ordered <- future.proj[order(future.proj$year),]
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  ens.proj.ordered <-  future.ens[order( future.ens$year),]
  
  # use all of the parameter MCMCS:
  
  ppt <- tmax <- SDI <-SICOND <- matrix(NA, nrow =1000, ncol = 82 )
  yeardf<- 2018:2099
  
  # want to sample 1 of the 21-31 models (not all models have all RCP scenarios) for each new sample:
  models <- unique(ens.proj.ordered$modelrun)
  sample.model <- sample(models, size = length(models), replace= FALSE)
  
  
  # get climate ensembles from CMIP5 for the plot
  get.ens.df <- function(i){
    
    ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scaled, 
                     SDI =  rep(cov.data[m, ]$SDI, 82), 
                     SICOND =  rep(cov.data[m, ]$SICOND, 82), 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(models), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  
  time_steps <- 82
  
  # set up sdi matrix
  sdi <-  matrix(data = NA, nrow = 1, ncol = nt + 1)
  
  sdi[,1] <- cov.data[m, ]$SDI
  
  
  # function to calculate SDI from the diameters:
  calc.sdi <- function(x){
    
    avg.dbh <- apply(x, 1, mean) # get mean of the MCMCs for each tree
    # note that any dead trees will have dbh == 0, which won't add to the sum of trees in the following line
    
    SDI.new <- sum(6.01*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI
    SDI.scaled <-   (SDI.new - mean(temp2$SDI))/sd(temp2$SDI)
    SDI.scaled
  }
  
  
  # for each tree get the next statespace time step:
  for(t in 1:nt){
    #if(t == 1){
    for (i in 1:ni){
      
      dbh.pred[i,,t+1] <- iterate_statespace.dbh(x = dbh.pred[i,,t], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates =  data.frame(SDI = sdi[,t], 
                                                                                                                                                      ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                      tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                      SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      
    }
    sdi[,t+1]<- calc.sdi(dbh.pred[,,t])
  }
  
  # save the plot-level arrays:
  saveRDS(dbh.pred, paste0("plotDBHforecasts/dbh/PLT.dbh.", plot,".",scenario,".RDS"))
  saveRDS(sdi, paste0("plotDBHforecasts/sdi/PLT.sdi.", plot,".",scenario,".RDS"))
  
  # make a plot of all the DBH forecasts:
  dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  
  p <- ggplot(data = dbh.means, aes(x = Var2, y = value, color = Var1, group = Var1))+geom_line() + 
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + theme(legend.position = "none") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
  
  ggsave(paste0("plotDBHforecasts/plot/PLT.sdi.", plot,".",scenario,".png"),p)
  
}

forecast.dbh.changingsdi( plot = unique(plots)[6], scenario = "rcp26")
# run over all the plots
plotsids <- unique(plots)
lapply(plotsids, function(x){forecast.dbh.changingsdi(plot = x, scenario = "rcp26")})
lapply(plotsids, function(x){forecast.dbh.changingsdi(plot = x, scenario = "rcp45")})
lapply(plotsids, function(x){forecast.dbh.changingsdi(plot = x, scenario = "rcp60")})
lapply(plotsids, function(x){forecast.dbh.changingsdi(plot = x, scenario = "rcp85")})

# -----------------------------------------------------------------------------------
#    if increment falls below zero for greater than 3 years, kill that tree off
# -----------------------------------------------------------------------------------
forecast.dbh.changingsdi.zeroinc <- function(plot, scenario){
  
  # -------- get the diameter estimates for all trees on the plot: ------------------------
  print(as.character(plot))
  # get id of trees with cores:
  cored.in.plt <- cores_cov.data %>% dplyr::filter (PLOT %in% plot)
  cored.in.plt$treeid
  
  # get id of trees with out cores:
  trees.in.plt <- Tree2Tree_cov.data %>% dplyr::filter (PLOT %in% plot)
  trees.in.plt$treeid
  
  #if(!length(trees.in.plt$PLOT) == 0){
  #cored.treeid <- cored.in.plt$treeid
  
  x <- cored.in.plt$treeid
  y <- trees.in.plt$treeid
  
  y <- y[y<5680] # just to reconcile the updated data with current
  m <- x[1] # since all cored trees will have the same plot information this is okay
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
  mean.cored <- mean.pred.cored[sel]
  
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names.noncored$row %in% y) # use sel to subset the data for the 415th tree
  mean.dia.noncored <- mean.pred.noncored[sel]
  
  
  
  # for noncored trees:
  tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x & ci.names.noncored$col == 25)})
  i <- do.call(cbind, tree.ind.noncrored )
  out.noncored.plt <-  out.noncored[, i] 
  
  # for cored trees:
  #yrs <- 31:135
  tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x & ci.names$col == 53)}) # select just the years 1994:2010 to match the plot level data:
  i.cored <- do.call(rbind, tree.ind.cored )
  out.cored.plt <-  out.cored[,i.cored] 
  all.dbh <- cbind(out.cored.plt, out.noncored.plt)
  
  # make a big array with all the DBH estimates:
  
  ni <- ncol(all.dbh) # number of individuals per plot
  
  
  nMCMC <- length(all.dbh[,1])
  nt <- 82
  
  # fill in the array with the diameter estimates for 2018:
  increment <- dbh.pred <- array(NA, dim = c(ni, nMCMC, nt + 1))
  for(i in 1:ni){
    dbh.pred[i,,1] <- all.dbh[,i]
  }
  
  
  #---------------------------------------------------------------------------
  ##  get all the paramter estimates + uncertainty
  #---------------------------------------------------------------------------
  
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  nMCMC1 <- nMCMC-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
  
  # use all of the parameter MCMCS:
  alpha = rep(quantile(alphas[, alphaplotid],0.5), nMCMC1)
  #alpha <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]))
  bSDI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI"]))
  bSDI_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]))
  if(!"betaX" %in% colnames(betas)){
    bX <-  rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]), sd = sd(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]))
    
  }else{
    bX <-  rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX"]))
    
  }
  bX2 <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX2"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX2"]))
  
  if("betaX_SDI" %in% colnames(betas)){
    bX_SDI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_SDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_SDI"]))
    
  }else{
    bX_SDI <- rep(0, nMCMC)
  }
  mcmc.its<- (length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_X <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_wintP.wateryr <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSI_tmax.fallspr <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    bSDI_SI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
  }else{
    bSI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND"]), sd = sd(betas[mcmc.its,"betaSICOND"]))
    bSI_X <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaX_SICOND"]), sd = sd(betas[mcmc.its,"betaX_SICOND"]))
    bSI_wintP.wateryr <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_wintP.wateryr"]), sd = sd(betas[mcmc.its,"betaSICOND_wintP.wateryr"]))
    
    bSI_tmax.fallspr <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_tmax.fallspr"]), sd = sd(betas[mcmc.its,"betaSICOND_tmax.fallspr"]))
    #bSDI_SI <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI_SICOND"])), mean = mean(betas[mcmc.its,"betaSDI_SICOND"]), sd = sd(betas[mcmc.its,"betaSDI_SICOND"]))
    bSDI_SI <- rep(0,length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])) )
    
  }
  
  bX_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]))
  bppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betawintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betawintP.wateryr"]))
  btmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr"]))
  bX_tmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  btmax_ppt <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(B0[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])]), sd = sd(B0[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])]))
  
  
  alpha <- rnorm(length((length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-nMCMC1) : length(betas[,"betaSDI"]), alphaplotid]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt,  bSDI_SI)
  
  
  future.proj <- unique(ens.means[ens.means$id == m & ens.means$rcp == scenario, ])
  proj.ordered <- future.proj[order(future.proj$year),]
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  #future.ens.nona <- future.ens%>% group_by(modelrun, year) %>% fill(ppt.scale, tmax.scaled )
  #future.ens
  #future.ens.nona <- future.ens%>% group_by(modelrun, year) %>% fill(everything(), .direction = "down") #<- future.ens[!is.na(future.ens$ppt.scale),]
  models.withnas <- future.ens[is.na(future.ens$ppt.scale),]$modelrun
  future.ens.nona <- future.ens %>% filter(!modelrun %in% models.withnas)
  ens.proj.ordered <-  future.ens.nona[order( future.ens.nona$year),]
  
  # use all of the parameter MCMCS:
  
  ppt <- tmax <- SDI <-SICOND <- matrix(NA, nrow =1000, ncol = 82 )
  yeardf<- 2018:2099
  
  # want to sample 1 of the 21-31 models (not all models have all RCP scenarios) for each new sample:
  models <- unique(ens.proj.ordered$modelrun)
  sample.model <- sample(models, size = length(models), replace= FALSE)
  
  
  # get climate ensembles from CMIP5 for the plot
  get.ens.df <- function(i){
    
    ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scaled, 
                     SDI =  rep(cov.data[m, ]$SDI, 82), 
                     SICOND =  rep(cov.data[m, ]$SICOND, 82), 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(models), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  
  time_steps <- 82
  
  # set up sdi matrix
  sdi <-  matrix(data = NA, nrow = 1, ncol = nt + 1)
  
  sdi[,1] <- cov.data[m, ]$SDI
  
  
  # function to calculate SDI from the diameters:
  calc.sdi <- function(x){
    avg.dbh <- apply(x, 1, mean) # get mean of the MCMC
    
    SDI.new <- sum(6.01*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI
    SDI.scaled <-   (SDI.new - mean(temp2$SDI))/sd(temp2$SDI)
    SDI.scaled
  }
  
  
  # for each tree get the next statespace time step:
  for(t in 1:nt){
    #if(t == 1){
    for (i in 1:ni){
      
      if(dbh.pred[i,,t] == 0){ # if the tree was killed off dbh.pred will be zero, so assign as zero
        dbh.pred[i,,t+1] <- dbh.pred[i,,t]
      }else{
        dbh.pred[i,,t+1] <- iterate_statespace.dbh(x = dbh.pred[i,,t], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates =  data.frame(SDI = sdi[,t], 
                                                                                                                                                        ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                        tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                        SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
        
      }
      increment[i,,t+1] <- dbh.pred[i,,t+1]-dbh.pred[i,,t] # calculate increment
      
      # if increment is < 0, assign as 0 & keep dbh.pred at previous value
      
      zeros <- increment[i,,t+1] <= 0 #| is.na(increment[i,,t=1])
      if(TRUE %in% zeros){
        increment[i,zeros,t+1] <- 0   
        dbh.pred[i,zeros,t+1] <- dbh.pred[i,zeros,t]
      }
      # if after 5 years of forecast the median increment <0 for 3 years in a row, kill off the tree:
      if(t >5){
        if(median(increment[i,,(t-2):t], na.rm = TRUE) <= 0 ){
          dbh.pred[i,,(t+1):82] <- 0
        }
      }
    }
    sdi[,t+1]<- calc.sdi(dbh.pred[,,t])
  }
  
  # save the plot-level arrays:
  saveRDS(dbh.pred, paste0("plotDBHforecasts_zeros/dbh/PLT.dbh.", plot,".",scenario,".RDS"))
  saveRDS(sdi, paste0("plotDBHforecasts_zeros/sdi/PLT.sdi.", plot,".",scenario,".RDS"))
  
  # make a plot of all the DBH forecasts:
  dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  
  p <- ggplot(data = dbh.means, aes(x = Var2, y = value, color = Var1, group = Var1))+geom_line() + 
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + theme(legend.position = "none") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
  
  ggsave(paste0("plotDBHforecasts_zeros/plot/PLT.sdi.", plot,".",scenario,".png"),p)
  
  # make a plot of all the increment forecasts:
  inc.means <- reshape2::melt(apply(increment, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  
  p2 <- ggplot(data = inc.means, aes(x = Var2, y = value, color = Var1, group = Var1))+geom_line() + 
    theme_bw() + ylab("Increment (cm)")+xlab("years after 2018") + theme(legend.position = "none") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))
  
  ggsave(paste0("plotDBHforecasts_zeros/plot/PLT.sdi.increment", plot,".",scenario,".png"),p2)
  
}


plotsids <- unique(plots)

#plotsids[2]

forecast.dbh.changingsdi.zeroinc(plot = plotsids[3], scenario = "rcp26")


lapply(plotsids, function(x){forecast.dbh.changingsdi.zeroinc(plot = x, scenario = "rcp26")})
lapply(plotsids, function(x){forecast.dbh.changingsdi.zeroinc(plot = x, scenario = "rcp45")})
lapply(plotsids, function(x){forecast.dbh.changingsdi.zeroinc(plot = x, scenario = "rcp60")})
lapply(plotsids, function(x){forecast.dbh.changingsdi.zeroinc(plot = x, scenario = "rcp85")})
