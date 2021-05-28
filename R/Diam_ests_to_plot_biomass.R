#   ### Notes about setup--there are two options:
#   1. Clone the pecan repo directly, then open this rmd in the repository. (Maybe best option because of dependancies)
# 2. You can install the allometry module from github with the code below (from allometry vignette):
#   library(devtools)
# # if not run, need to install the pecan modules
install_github("PecanProject/pecan",subdir="base/logger")
install_github("PecanProject/pecan",subdir="modules/allometry")


library(PEcAn.allometry)
library(reshape2)
library(ggplot2)
library(tidyverse)





# ## Basic Pecan allometry tutorial
# Alot of this text and code is just annotated from the allometry vignette.
# To view the different components of the tree for which you could develop allomtrys + their abbreviations:
#   


data("allom.components")
allom.components

pfts = list(PIPO = data.frame(spcd=122, acronym='PIPO')) # list our "Pfts--plant functional types" of interest--really list the species



## AllomAve function in PEcAn allometry module
# - Fits the bayesian allomtric models for the pfts. ngibbs indicates the number of gibbs/mcmc samples
# - looks like there is a burn in of ~200
# - automatically outputs a pdf with the traceplots titled "allom.pipo.6.mcmc" and saves and .Rdata object.
# - not totally clear to be what the different betas are

allom.stats = AllomAve(pfts,components =6, ngibbs=1000) 
allom.stats$PIPO


## Predict for individual trees:
# - the allom.predict fuction will use the fit relationships from allomAve to predict the biomass component for the pft of interest (PIPO) over the DBH vector values:
#   - dbh = diameter to predict at
# - component is which component of biomass to predict see defintation in the allom.components object
# - unclear to me whether component = 6 is predicting Stem biomass, or is only suing the diameter of the stem in the allometry equation
# - use = Bg 
# 



##Predict for individual trees:
allom.fit = load.allom(getwd()) # get the allometry listed in our working directory
dbh = 1:50 # vector of DBH values to predict over

pred = allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") # predict allometries for trees with dbh 1:50, and get the prediction interval
conf = allom.predict(allom.fit,dbh = dbh,pft = "PIPO",component = 6,use = "Bg",interval = "confidence")# predict allometries for trees with dbh 1:50, and get the confidence interval (including uncertainties)
PI = apply(pred,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
CI = apply(conf,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
plot(dbh,CI[2,],type='l',lwd=3,ylim=range(PI),ylab="Biomass (kg)")
lines(dbh,CI[1,],lty=2,col="blue")
lines(dbh,CI[3,],lty=2,col="blue")
lines(dbh,PI[1,],lty=3,col="red")
lines(dbh,PI[3,],lty=3,col="red")



## read in DBH estimates with Uncertainty
#This code gives you an idea of what the posterior estimates of Tree DBH + uncertainty look like, using an example tree (tree 415).

#Overall, this dataset contains 7800 posterior estimates of tree diameter between 1965 - 2018 for 515 Pinus ponderosa trees in Arizona. 

# the MCMC samples for tree diameter are in a big rds file 
# note that these are just the last 7800 mcmc samples (from a mcmc of 750,000 samples total)
Xests <- readRDS("data/IGF_xvals_SDI_SI.norand.X.nadapt.5000.rds")
str(Xests) # structure is a list of 3 MCMC chains, 



out      <- as.matrix(Xests) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

# generate 95% CI of the DBH
ci      <- apply(out[7000:7800, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
var.pred       <- apply(out[7000:7800, x.cols], 2, var) # get the var.pred for the last 800 samples
# use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
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
} # parse.MatrixNames

ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)

# plot the posterior predictions of DBH for a single tree:
sel <- which(ci.names$row == 415) # use sel to subset the data for the 415th tree
ci.415 <-ci[, sel]
# do some hacky refomatting to get this into a formate that we can ggplot
ci.415.df <- data.frame(year = 1966:2018, 
                        median = as.vector(ci.415[2,]), 
                        ci.lo = as.vector(ci.415[1,]),
                        ci.hi = as.vector(ci.415[3,]))


head(ci.415.df)

# now plot DBH of tree 415 over time
ggplot()+geom_ribbon(data = ci.415.df, aes(x = year, ymin = ci.lo, ymax = ci.hi), fill = "mediumpurple")+geom_line(data = ci.415.df, aes(x = year, y = median), color = "midnightblue")+theme_bw()+theme(panel.grid = element_blank())+ylab("Estimated DBH")+xlab("Year")#+ylim(0, 50)

#Lets get an idea about how long the prediction will take with all the DBH uncertainty


# I think one of they ways to propagate the uncertainty around the DBH estimates is to just use all the MCMC samples for each time, and tree in the pecan allom.predict function. Although this has potential to take a while. So I am testing with a subset of the data here 

# use the MCMC samples, not the summarised CI data table 
# the out matrix is a 7800 x 27295 matrix 
# the rows are 7800 mcmc samples for that tree and time period
# columns are the tree and time period combination, so x[23, 45] contains the DBH mcmc estimates for tree 23 at time period 45. There are 515 trees and 53 time points

##Predict for individual trees:
allom.fit = load.allom(getwd()) # get the allometry listed in our working directory

PI <- list()

i <- 1


#dbh.df <- reshape2::melt(dbh)

# function to run allom.predict on one dbh

predict.mcmc.biomass <- function(i){
  
  dbh = out[,i] # get dbh predictions for all the mcmcs for tree 1 at time point 1
  
  pred <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  PI[[i]] <-  apply(pred,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  #print(i)
}

# compare time of apply and for loop
system.time(lapply(1:10, predict.mcmc.biomass))
# user  system elapsed 
# 32.154   2.699  39.266 

system.time(for(i in 1:10){
  predict.mcmc.biomass(i)
})

#  user  system elapsed 
# 32.743   3.208  39.182 

# both of these will take a long time to do the whole time series for a given tree...lets sample randomly from the mcmcs:
set.seed(22)
smp.mcmc <- sample(5000:7500, size = 100, replace = FALSE)

# to speed this up, lets sample randomly from the mcmcs
Pred.Interval<- Conf.Interval <- list()

predict.mcmc.biomass.subsample <- function(i){
  #tree.index <- which(ci.names$row == i)
  
  dbh = out[smp.mcmc,i] # get dbh predictions for all the mcmcs for tree 1 at time point 1
  
  #pred <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  conf <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "confidence") 
  
  
  #print(i)
  conf
}

predict.mcmc.biomass.subsample.pred <- function(i){
  #tree.index <- which(ci.names$row == i)
  
  dbh = out[smp.mcmc,i] # get dbh predictions for all the mcmcs for tree 1 at time point 1
  
  #pred <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  conf <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  
  
  #print(i)
  conf
}


# lets run this for the 1st tree
# need to set up a vector to do this:
tree.1.index <- which(ci.names$row == 1)

system.time(Conf.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample))
#  user  system elapsed 
# 0.912   0.124   1.106 



conf.int.df <- data.frame(do.call(rbind, Conf.Interval))
conf.int.df$time <- rep(1:length(Conf.Interval), sapply(Conf.Interval, nrow)) # add an id for each DBH fit


# in conf.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
# the MCMC samples from the allometry model are in the rows

# lets get the median + 95% confidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 

# basically, this means we need to calculate quantiles over all MCMC

conf.intervals.df.m <- reshape2::melt(conf.int.df, id.vars = c("time"))

# get the ci for each tree and time overall (This includes DBH + allometry uncertainty)
conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5), 
                                                                              ci.lo = quantile(value, 0.025), 
                                                                              ci.hi = quantile(value, 0.975))


# lets plot the timeseries of biomass!
ggplot()+geom_ribbon(data = conf.intervals.quants, aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "coral4")+geom_line(data = conf.intervals.quants, aes(x = time, y = median))+ylab("Biomass for tree 1 (kg)")+theme_bw()+theme(panel.grid = element_blank())


# this is just for one tree, so we should apply to all the trees, and also do a run where we don't include the DBH uncertainties, so we can parse the two

# apply this to get the confidence intervals for all the trees


tree.1.index <- which(ci.names$row == 1)

# function to get confidence intervals:
get.biomass.conf <- function(tree.1.index){
  print(tree.1.index[[1]])
  Conf.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample)
  #  user  system elapsed 
  # 0.912   0.124   1.106 
  
  conf.int.df <- data.frame(do.call(rbind, Conf.Interval))
  conf.int.df$time <- rep(1:length(Conf.Interval), sapply(Conf.Interval, nrow)) # add an id for each DBH fit
  tree <- tree.1.index[[1]]
  saveRDS(conf.int.df , paste0("outputs_biomass/biomass_conf_cored_mcmc/conf_", tree, ".RDS"))
  
  # in conf.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
  # the MCMC samples from the allometry model are in the rows
  
  # lets get the median + 95% confidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 
  
  # basically, this means we need to calculate quantiles over all MCMC
  
  conf.intervals.df.m <- reshape2::melt(conf.int.df, id.vars = c("time"))
  
  conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE))
  
  conf.intervals.quants
}

# function to get prediction intervals:
get.biomass.pred <- function(tree.1.index){
  print(tree.1.index[[1]])
  pred.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample.pred)
  #  user  system elapsed 
  # 0.912   0.124   1.106 
  
  pred.int.df <- data.frame(do.call(rbind, pred.Interval))
  pred.int.df$time <- rep(1:length(pred.Interval), sapply(pred.Interval, nrow)) # add an id for each DBH fit
  
  saveRDS( pred.int.df , paste0("outputs_biomass/biomass_pred_cored_mcmc/pred_", tree.1.index[[1]], ".RDS"))
  # in pred.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
  # the MCMC samples from the allometry model are in the rows
  
  # lets get the median + 95% predidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 
  
  # basically, this means we need to calculate quantiles over all MCMC
  
  pred.intervals.df.m <- reshape2::melt(pred.int.df, id.vars = c("time"))
  
  pred.intervals.quants <- pred.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE))
  
  pred.intervals.quants
}


tree.ind <- lapply(X = 1:515, FUN= function(x){which(ci.names$row == x)})

conf.intervals.one<- lapply(tree.ind[[1]], get.biomass.conf)
system.time(conf.intervals.all <- lapply(tree.ind, get.biomass.conf))
system.time(pred.intervals.all <- lapply(tree.ind, get.biomass.pred))
# # get the ci for each tree and time overall (This includes DBH + allometry uncertainty)
# conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5), 
#                                                                ci.lo = quantile(value, 0.025), 
#                                                                ci.hi = quantile(value, 0.975))
# 

# lets plot the timeseries of biomass!
ggplot()+geom_ribbon(data = pred.intervals.all[[2]], aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "blue")+geom_line(data = pred.intervals.all[[2]], aes(x = time, y = median))+geom_ribbon(data = conf.intervals.all[[2]], aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "coral4")+geom_line(data = conf.intervals.all[[2]], aes(x = time, y = median))+ylab("Biomass for tree 1 (kg)")+theme_bw()+theme(panel.grid = element_blank())

# save the conf and prediction intervals for these runs:
saveRDS(conf.intervals.all, "outputs_biomass/conf_intervals_all_jenkins_PIPO_comp_6_Bg.RDS")
saveRDS(pred.intervals.all, "outputs_biomass/pred_intervals_all_jenkins_PIPO_comp_6_Bg.RDS")
# for all of the trees in the list, get the proportion of variance 



# do the same for all the additional trees in the plot:


# the MCMC samples for tree diameter are in a big rds file 
# note that these are just the last 7800 mcmc samples (from a mcmc of 750,000 samples total)
#Xests <- readRDS("data/IGF_xvals_stage2_model3_5794.rds")
Xests <- readRDS(url("https://de.cyverse.org/dl/d/678DA0AE-A182-4C4C-9EC3-E73C8D58D9C3/IGF_xvals_stage2_model3_5794.rds"))

str(Xests) # structure is a list of 3 MCMC chains, 


library(rjags)
out      <- as.matrix(Xests) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

# generate 95% CI of the DBH
ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
var.pred       <- apply(out[, x.cols], 2, var) # get the var.pred for the last 800 samples
# use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
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
} # parse.MatrixNames

ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)

# plot the posterior predictions of DBH for a single tree:
sel <- which(ci.names$row == 1) # use sel to subset the data for the 415th tree
ci.415 <-ci[, sel]
# do some hacky refomatting to get this into a formate that we can ggplot
ci.415.df <- data.frame(year = 1:length(ci.415[1,]), 
                        median = as.vector(ci.415[2,]), 
                        ci.lo = as.vector(ci.415[1,]),
                        ci.hi = as.vector(ci.415[3,]))


head(ci.415.df)

# now plot DBH of tree 415 over time
ggplot()+geom_ribbon(data = ci.415.df, aes(x = year, ymin = ci.lo, ymax = ci.hi), fill = "mediumpurple")+geom_line(data = ci.415.df, aes(x = year, y = median), color = "midnightblue")+theme_bw()+theme(panel.grid = element_blank())+ylab("Estimated DBH")+xlab("Year")#+ylim(0, 50)



set.seed(22)
smp.mcmc <- sample(2000:3300, size = 100, replace = FALSE)

# to speed this up, lets sample randomly from the mcmcs
Pred.Interval<- Conf.Interval <- list()

predict.mcmc.biomass.subsample <- function(i){
  #tree.index <- which(ci.names$row == i)
  
  dbh = out[smp.mcmc,i] # get dbh predictions for all the mcmcs for tree 1 at time point 1
  
  #pred <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  conf <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "confidence") 
  
  
  #print(i)
  conf
}

predict.mcmc.biomass.subsample.pred <- function(i){
  #tree.index <- which(ci.names$row == i)
  
  dbh = out[smp.mcmc,i] # get dbh predictions for all the mcmcs for tree 1 at time point 1
  
  #pred <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  conf <-  allom.predict(allom.fit, dbh = dbh, pft = "PIPO", component =6, use = "Bg", interval = "prediction") 
  
  
  #print(i)
  conf
}


# lets run this for the 1st tree
# need to set up a vector to do this:
tree.1.index <- which(ci.names$row == 1)

predict.mcmc.biomass.subsample(tree.1.index[[1]])

system.time(Conf.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample))
#  user  system elapsed 
# 0.912   0.124   1.106 



conf.int.df <- data.frame(do.call(rbind, Conf.Interval))
conf.int.df$time <- rep(1:length(Conf.Interval), sapply(Conf.Interval, nrow)) # add an id for each DBH fit


# in conf.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
# the MCMC samples from the allometry model are in the rows

# lets get the median + 95% confidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 

# basically, this means we need to calculate quantiles over all MCMC

conf.intervals.df.m <- reshape2::melt(conf.int.df, id.vars = c("time"))

# get the ci for each tree and time overall (This includes DBH + allometry uncertainty)
conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5), 
                                                                              ci.lo = quantile(value, 0.025), 
                                                                              ci.hi = quantile(value, 0.975))


# lets plot the timeseries of biomass!
ggplot()+geom_ribbon(data = conf.intervals.quants, aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "coral4")+geom_line(data = conf.intervals.quants, aes(x = time, y = median))+ylab("Biomass for tree 1 (kg)")+theme_bw()+theme(panel.grid = element_blank())





tree.1.index <- which(ci.names$row == 13)

# function to get confidence intervals:
get.biomass.conf <- function(tree.1.index){
  print(tree.1.index[[1]])
  Conf.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample)
  #  user  system elapsed 
  # 0.912   0.124   1.106 
  
  conf.int.df <- data.frame(do.call(rbind, Conf.Interval))
  conf.int.df$time <- rep(1:length(Conf.Interval), sapply(Conf.Interval, nrow)) # add an id for each DBH fit
  saveRDS(conf.int.df, paste0("outputs_biomass/biomass_conf_noncored_mcmc/conf_", tree.1.index[[1]], ".RDS"))
  
  # in conf.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
  # the MCMC samples from the allometry model are in the rows
  
  # lets get the median + 95% confidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 
  
  # basically, this means we need to calculate quantiles over all MCMC
  
  conf.intervals.df.m <- reshape2::melt(conf.int.df, id.vars = c("time"))
  
  conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE))
  
  conf.intervals.quants
}


#tree.1.index <- tree.ind[[1]]
# function to get prediction intervals:
get.biomass.pred <- function(tree.1.index){
  print(tree.1.index[[1]])
  pred.Interval <- lapply(tree.1.index, predict.mcmc.biomass.subsample.pred)
  #  user  system elapsed 
  # 0.912   0.124   1.106 
  
  pred.int.df <- data.frame(do.call(rbind, pred.Interval))
  pred.int.df$time <- rep(1:length(pred.Interval), sapply(pred.Interval, nrow)) # add an id for each DBH fit
  saveRDS(pred.int.df, paste0("outputs_biomass/biomass_pred_noncored_mcmc/pred_", tree.1.index[[1]], ".RDS"))
  
  
  # in pred.intervals.df, the uncertinaty from DBH uncertainty (from state space model) should be reflected in the distribution of each row, 
  # the MCMC samples from the allometry model are in the rows
  
  # lets get the median + 95% predidence intervals across rows and colums for each tree/timepoint to estimate the total biomass uncertainty from DBH uncertainty + allometric equation uncertianty 
  
  # basically, this means we need to calculate quantiles over all MCMC
  
  pred.intervals.df.m <- reshape2::melt(pred.int.df, id.vars = c("time"))
  
  pred.intervals.quants <- pred.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE))
  
  pred.intervals.quants
}

y

tree.ind <- lapply(X = 1:5794, FUN= function(x){which(ci.names$row == x)})
#tree.indy <- lapply(X = y, FUN= function(x){which(ci.names$row == x)})
c <- get.biomass.conf(tree.ind[[15]])
p <- get.biomass.pred(tree.ind[[15]])
system.time(conf.intervals.all <- lapply(tree.ind, get.biomass.conf))
system.time(pred.intervals.all <- lapply(tree.ind, get.biomass.pred))

system.time(conf.intervals.ally <- lapply(tree.indy, get.biomass.conf))
system.time(pred.intervals.ally <- lapply(tree.indy, get.biomass.pred))

# # get the ci for each tree and time overall (This includes DBH + allometry uncertainty)
# conf.intervals.quants <- conf.intervals.df.m %>% group_by(time) %>% summarise(median = quantile(value, 0.5), 
#                                                                ci.lo = quantile(value, 0.025), 
#                                                                ci.hi = quantile(value, 0.975))
# 

# lets plot the timeseries of biomass!
ggplot()+geom_ribbon(data = p, aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "blue")+
  geom_line(data = p, aes(x = time, y = median))+
  geom_ribbon(data = c, aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "coral4")+
  geom_line(data = c, aes(x = time, y = median))+ylab("Biomass for tree 1 (kg)")+
  theme_bw()+theme(panel.grid = element_blank())

# save the conf and prediction intervals for these runs:
saveRDS(conf.intervals.all, "outputs_biomass/conf_intervals_all_jenkins_PIPO_comp_6_Bg_nocore5794.RDS")
saveRDS(pred.intervals.all, "outputs_biomass/pred_intervals_all_jenkins_PIPO_comp_6_Bg_nocore5794.RDS")
# for all of the trees in the list, get the proportion of variance 




# get datasets to match up the plots


#jags.data.t2t<- readRDS("data/jags.data.5794.stage2.rds")
jags.data.t2t<-readRDS(url("https://de.cyverse.org/dl/d/96202C21-8905-4F44-9BDD-88B07595AC1F/jags.data.5794.stage2.rds"))
jags.data.t2t$cov.data$treeid <- 1:length(jags.data.t2t$cov.data$PLOT)


#jags.new <- readRDS("diam_data/jags.new.SDI.SICOND.norandX.tau.inc.106.rds")
jags.new <- readRDS(url("https://de.cyverse.org/dl/d/2EA2123C-0ECC-4210-9804-1E6DB79F16CA/jags.new.SDI.SICOND.norandX.tau.inc.106.rds"))
jags.new$cov.data$treeid <- 1:length(jags.new$cov.data$PLOT)
jags.data.t2t$cov.data$PLOT %in% jags.new$cov.data$PLOT

# want a dataframe that has a column for t2t_treeid
Tree2Tree_cov.data <- jags.data.t2t$cov.data
cores_cov.data <- jags.new$cov.data

head(cores_cov.data)



# add up all the biomass in a single plot


t2t.plot <- unique(Tree2Tree_cov.data$PLOT)
cores.plot <- unique(cores_cov.data$PLOT)

plots <- unique(Tree2Tree_cov.data$PLOT, cores_cov.data$PLOT)


# add up all the ci (note that this is not exactly what we want to do)
conf.intervals.cored <- readRDS( "outputs_biomass/conf_intervals_all_jenkins_PIPO_comp_6_Bg.RDS")

pred.intervals.cored <- readRDS( "outputs_biomass/pred_intervals_all_jenkins_PIPO_comp_6_Bg.RDS")



organize.by.plot <- function(plot){
  # get id of trees with cores:
  cored.in.plt <- cores_cov.data %>% filter (PLOT %in% plot)
  cored.in.plt$treeid
  # get id of trees with out cores:
  trees.in.plt <- Tree2Tree_cov.data %>% filter (PLOT %in% plot)
  trees.in.plt$treeid
  
  
  #cored.treeid <- cored.in.plt$treeid
  
  x <- cored.in.plt$treeid
  y <- trees.in.plt$treeid
  
  
  # loop for trees cored
  time.df.d <- data.frame(time = 1:53, 
                          year = 1966:2018)
  biomass.c <- list()
  for(i in 1:length(x)){
    
    conf <- conf.intervals.cored[[x[i]]]
    conf$treeid <- x[i]
    #conf
    
    pred <- pred.intervals.cored[[x[i]]]
    pred$treeid <- x[i]
    colnames(pred) <-c("time", "median_pred", "ci.lo_pred", "ci.hi_pred", "treeid")
    biomass.cored <- left_join(pred, conf, by = c("time", "treeid"))
    biomass.cored.t<- left_join(time.df.d , biomass.cored, by = "time")
    biomass.c[[x[i]]] <- biomass.cored.t
  }
  # loop for trees not cored--time period is only 1994-2010
  time.df.d <- data.frame(time = 1:17, 
                          year = 1994:2010)
  biomass.d <- list()
  for(i in 1:length(y)){
    
    conf <- conf.intervals.all[[y[i]]]
    conf$treeid <- y[i]
    #conf
    
    pred <- pred.intervals.all[[y[i]]]
    pred$treeid <- y[i]
    colnames(pred) <-c("time", "median_pred", "ci.lo_pred", "ci.hi_pred", "treeid")
    biomass.cored <- left_join(pred, conf, by = c("time", "treeid"))
    biomass.cored.t<- left_join(time.df.d, biomass.cored, by = "time")
    biomass.d[[y[i]]] <- biomass.cored.t
  }
  dbh.biomass <- do.call(rbind, biomass.d)
  cored.biomass <- do.call(rbind, biomass.c)
  est.biomass <- rbind(dbh.biomass, cored.biomass)
  est.biomass$PLOT <- plot
  est.biomass
}

system.time(test<-  organize.by.plot(plots[2]))
# user  system elapsed 
#  0.156   0.010   0.214 

organized.plots <- lapply(plots, organize.by.plot)
saveRDS(organized.plots, "outputs_biomass/organized_plots_from_ci_list.RDS")

organized.plots[[1]]
x <- 1

summarise.plot.biomass.year <- function(x){
  
  total.plot <- data.frame(organized.plots[[x]]) %>% group_by(year)%>% dplyr::summarise(med.pred.tot = sum(median_pred), 
                                                                                        ci.lo.pred.tot = sum(ci.lo_pred), 
                                                                                        ci.hi.pred.tot = sum(ci.hi_pred), 
                                                                                        med.conf.tot = sum(median), 
                                                                                        ci.lo.conf.tot = sum(ci.lo), 
                                                                                        ci.hi.conf.tot=sum(ci.hi))
  
  
  total.plot$PLOT <- as.character(plots[x])
  total.plot
}


pipo.plot.totals <- lapply(1:length(plots), summarise.plot.biomass.year)
pipo.plots.df <- do.call(rbind, pipo.plot.totals)
write.csv(pipo.plots.df, "outputs_biomass/yearly_plot_pipo_AZ_biomass_by_ci.csv", row.names = FALSE)


ggplot(pipo.plots.df, aes(x = year, y= med.conf.tot, group = PLOT))+geom_line()+xlim(1994,2010)+theme_bw()+ylab("Median predicted plot biomass (kg)")

# plot interval plots with uncertainty
tot.biomass.2010 <- ggplot()+geom_errorbar(data = pipo.plots.df %>% filter(year == 2010 ),aes(x = PLOT, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), color = "red")+geom_point(data = pipo.plots.df %>% filter(year == 2010 ), aes(x = PLOT, y= med.conf.tot, group = PLOT))+geom_errorbar(data = pipo.plots.df %>% filter(year == 2010 ),aes(x = PLOT, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())


# map out total biomass 
cov.data.unique <- unique(cores_cov.data %>% dplyr::select(-TREE,-treeid))
colnames(cores_cov.data)

pipo.plt.cov.biomass <- left_join(cov.data.unique, pipo.plots.df, by = "PLOT")

tot.biomass.2010.byMAP <- ggplot()+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = MAP, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), color = "red")+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = MAP, y= med.conf.tot, group = PLOT))+geom_errorbar(data =pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = MAP, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())

tot.biomass.2010.byMAT <- ggplot()+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = MAT, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), color = "red")+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = MAT, y= med.conf.tot, group = PLOT))+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = MAT, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())


tot.biomass.2010.bySDI <- ggplot()+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = SDI, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), color = "red")+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = SDI, y= med.conf.tot, group = PLOT))+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = SDI, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())

tot.biomass.2010.bySICOND <- ggplot()+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = SICOND, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), color = "red")+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = SICOND, y= med.conf.tot, group = PLOT))+geom_errorbar(data = pipo.plt.cov.biomass %>% filter(year == 2010 ),aes(x = SICOND, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())


png(height = 6, width = 6, units = "in", res = 200, "diam_data/outputs_biomass/median_biomass_vs_covs_unc_2010.png")
cowplot::plot_grid(tot.biomass.2010.byMAP , tot.biomass.2010.byMAT, tot.biomass.2010.bySDI, tot.biomass.2010.bySICOND, ncol = 2)
dev.off()


tot.biomass.2010.byMAP.m <- ggplot()+
  geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = MAP, y= med.conf.tot, group = PLOT))+
  theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())

tot.biomass.2010.byMAT.m <- ggplot()+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = MAT, y= med.conf.tot, group = PLOT))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())


tot.biomass.2010.bySDI.m <- ggplot()+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = SDI, y= med.conf.tot, group = PLOT))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())

tot.biomass.2010.bySICOND.m <- ggplot()+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = SICOND, y= med.conf.tot, group = PLOT))+theme_bw()+ylab("Median predicted plot biomass (kg)")+theme(panel.grid = element_blank())


png(height = 6, width = 6, units = "in", res = 200, "diam_data/outputs_biomass/median_biomass_vs_covs_2010.png")
cowplot::plot_grid(tot.biomass.2010.byMAP.m , tot.biomass.2010.byMAT.m, tot.biomass.2010.bySDI.m, tot.biomass.2010.bySICOND.m, ncol = 2)
dev.off()


# map out in space:
all_states <- map_data("state")
az.states <- subset(all_states, region %in% c( "arizona") )
coordinates(az.states)<-~long+lat
class(az.states)
#proj4string(az.states) <-CRS("+proj=longlat +datum=NAD83")
mapdata.az <- az.states
mapdata.az <-data.frame(mapdata.az)



ll.median.map.biomass <- ggplot()+geom_point(data = pipo.plt.cov.biomass %>% filter(year == 2010 ), aes(x = LON, y=LAT,  color = med.conf.tot))+geom_path(data=data.frame(mapdata.az), aes(x = long, y = lat, group = group), color = 'black', fill = NA)+coord_equal()+theme_bw()+theme(panel.grid = element_blank(), legend.title = element_blank())+ scale_color_gradientn(colours = rev(terrain.colors(7)))+ggtitle("Median plot biomass in 2010")

png(height = 4, width = 5, units = "in", res = 200, "diam_data/outputs_biomass/map_of_median_biomass_2010.png")
ll.median.map.biomass
dev.off()

#########################################################################################
# add up plot biomass from full MCMC chains
#########################################################################################


# need a function to read in all the time series from each tree and aggregate
x <- 1

read.all.biomass <- function(x, dataset = "cored", type = "conf"){
  # print(x)
  
  biomass.conf <- biomass.pred <- list()
  #if(dataset == "noncored" & type == "pred"){
  # biomass.conf <- readRDS(paste0("outputs_biomass/biomass_", type,"_", dataset, "_mcmc/conf_",x, ".RDS"))# case of mislabeling
  
  #}else{
  biomass.conf <- readRDS(paste0("outputs_biomass/biomass_", type,"_", dataset, "_mcmc/", type,"_",x, ".RDS"))
  #}
  conf.intervals.df.m <- reshape2::melt(biomass.conf, id.vars = c("time"))
  
  conf.intervals.df.m$treeid <- x
  conf.intervals.df.m
}

test <- read.all.biomass(y[i], dataset = "noncored", type = "conf")
test2 <- read.all.biomass(y[i], dataset = "noncored", type = "pred")
# not run
#cored.conf.list <- lapply(1:515, read.all.biomass, dataset = "cored",  type = "conf")
#cored.pred.list <- lapply(1:515, read.all.biomass, dataset = "cored",  type = "pred")

#noncored.conf.list <- lapply(1:5794, read.all.biomass, dataset = "noncored",  type = "conf")
#noncored.pred.list <- lapply(1:5794, read.all.biomass, dataset = "noncored",  type = "pred")



plot<- plots[15]


organize.by.plot.full <- function(plot){
  
  print(as.character(plot))
  # get id of trees with cores:
  cored.in.plt <- cores_cov.data %>% dplyr::filter (PLOT %in% plot)
  cored.in.plt$treeid
  # get id of trees with out cores:
  trees.in.plt <- Tree2Tree_cov.data %>% dplyr::filter (PLOT %in% plot)
  trees.in.plt$treeid
  
  
  #cored.treeid <- cored.in.plt$treeid
  
  x <- cored.in.plt$treeid
  y <- trees.in.plt$treeid
  
  
  
  
  # loop for trees cored
  time.df.d <- data.frame(time = 1:53, 
                          year = 1966:2018)
  biomass.c <- list()
  
  print("gathering cored biomass")
  
  for(i in 1:length(x)){
    
    conf1 <- read.all.biomass(x[i], dataset = "cored",  type = "conf")#conf.intervals.cored[[x[i]]]
    
    #conf <- conf1 %>% group_by(time, variable)%>% top_n(10, row_number())
    conf <- conf1 %>% filter(variable %in% "X1")
    conf$rep <- rep(1:10, 53)
    conf$rep <- rep(1:2250, 53)
    #conf$treeid <- x[i]
    #conf$rep <-1
    
    pred1 <- read.all.biomass(x[i], dataset = "cored",  type = "pred") #pred.intervals.cored[[x[i]]]
    pred <- pred1 %>% group_by(time, variable)%>% top_n(10, row_number())
    #pred$rep <- 1
    pred <- pred1 %>% filter(variable %in% "X1")
    pred$rep <- rep(1:10, 53)
    pred$rep <- rep(1:2250, 53)
    #pred$treeid <- x[i]
    colnames(pred) <-c("time", "variable", "value_pred", "treeid", "rep")
    biomass.cored <- left_join(pred, conf, by = c("time", "treeid", "variable", "rep"))
    biomass.cored.t<- left_join(time.df.d , biomass.cored, by = "time")
    biomass.c[[x[i]]] <- biomass.cored.t
  }
  # loop for trees not cored--time period is only 1994-2010
  time.df.d <- data.frame(time = 1:17, 
                          year = 1994:2010)
  
  
  print("gathering noncored biomass")
  biomass.d <- list()
  for(i in 1:length(y)){
    
    conf1 <- read.all.biomass(y[i], dataset = "noncored",  type = "conf")#conf.intervals.cored[[x[i]]]
    
    #conf <- conf1 %>% group_by(time, variable)%>% top_n(10, row_number())
    conf <- conf1 %>% filter(variable %in% "X1")
    
    
    #conf$rep <- rep(1:10, 17)
    conf$rep <- rep(1:2250, 17)
    
    #conf$rep <- 1
    #conf$treeid <- x[i]
    #conf
    
    pred1 <- read.all.biomass(y[i], dataset = "noncored",  type = "pred")#pred.intervals.cored[[x[i]]]
    #pred <- pred1 %>% group_by(time, variable) %>% top_n(10, row_number())
    #pred$rep <- rep(1:10, 17)
    pred <- pred1 %>% filter(variable %in% "X1")
    
    
    #conf$rep <- rep(1:10, 17)
    pred$rep <- rep(1:2250, 17)
    #pred$rep<- 1
    colnames(pred) <-c("time", "variable", "value_pred", "treeid", "rep")
    biomass.noncored <- left_join(pred, conf, by = c("time", "treeid", "variable", "rep"))
    biomass.noncored.t<- left_join(time.df.d, biomass.noncored, by = "time")
    biomass.d[[y[i]]] <- biomass.noncored.t
  }
  dbh.biomass <- do.call(rbind, biomass.d)
  cored.biomass <- do.call(rbind, biomass.c)
  est.biomass <- rbind(dbh.biomass, cored.biomass)
  est.biomass$PLOT <- plot
  unique(est.biomass$treeid)
  
  # get sums for total biomass
  
  # first get the median & 95% ci of biomass for each tree: ie. quantiles across dbh reps and mcmc samples
  # total.tree <- est.biomass%>% ungroup() %>% group_by(rep, year)%>%
  # summarise(med.pred = mean(value_pred), 
  #                                            ci.lo.pred = quantile(value_pred, 0.025),
  #                                            ci.hi.pred = quantile(value_pred, 0.975),
  #                                           med.conf = mean(value),
  #                                           ci.lo.conf = quantile(value, 0.025),
  #                                           ci.hi.conf = quantile(value, 0.975))%>% ungroup()
  # 
  # 
  # #
  # total.plot <- total.tree %>% ungroup() %>% 
  #   group_by(year, rep) %>% 
  #   
  #    dplyr::summarise(tot.pred.med = sum(med.pred),
  #                                               tot.pred.ci.lo = sum(ci.lo.pred),
  #                                               tot.pred.ci.hi =sum(ci.hi.pred),
  #                                               tot.conf.med = sum(med.conf),
  #                                               tot.conf.ci.lo = sum(ci.lo.conf),
  #                                               tot.conf.ci.hi = sum(ci.hi.conf)) %>% ungroup ()
  
  # agbs <- total.plot  %>%
  #   mutate(flux.pred.med = tot.pred.med - lag(tot.pred.med, default = 0), 
  #          
  #          flux.pred.ci.lo.1 = tot.pred.ci.lo - lag(tot.pred.ci.lo, default = 0),
  #          flux.pred.ci.hi.1 = tot.pred.ci.hi - lag(tot.pred.ci.hi, default = 0),
  #          
  #          flux.conf.med = tot.conf.med - lag(tot.conf.med, default = 0), 
  #          flux.conf.ci.lo.1 = tot.conf.ci.lo - lag(tot.conf.ci.lo, default = 0),
  #          flux.conf.ci.hi.1 = tot.conf.ci.hi - lag(tot.conf.ci.hi, default = 0),
  #          )  %>% group_by(year)%>% mutate(flux.pred.ci.lo = min(flux.pred.ci.lo.1, flux.pred.ci.hi.1), 
  #                        flux.pred.ci.hi = max(flux.pred.ci.lo.1, flux.pred.ci.hi.1),
  #                        flux.conf.ci.lo = min(flux.conf.ci.lo.1, flux.conf.ci.hi.1), 
  #                        flux.conf.ci.hi = max(flux.conf.ci.lo.1, flux.conf.ci.hi.1))
  # 
  
  totals <- est.biomass %>% ungroup()%>% group_by(year, variable, rep ) %>% dplyr::summarise(tot.pred = sum(value_pred, na.rm = TRUE),
                                                                                             tot.conf = sum(value, na.rm = TRUE),
                                                                                             ntsamps = n()) %>% ungroup ()
  
  
  #totals %>% filter(year %in% 1995:2010)
  
  # get agb flux:
  agb.df  <- totals %>% ungroup()%>%
    group_by(variable) %>%
    mutate(agb_inc_pred = tot.pred - lag(tot.pred, default = 0),
           agb_inc_conf = tot.conf - lag(tot.conf, default = 0)) %>% ungroup()
  
  # summarise the sums
  total.plot <- agb.df  %>% group_by(year) %>% dplyr::summarise(tot.pred.med = mean(tot.pred),
                                                                tot.pred.ci.lo = quantile(tot.pred, 0.025),
                                                                tot.pred.ci.hi = quantile(tot.pred, 0.975),
                                                                
                                                                tot.conf.med = mean(tot.conf),
                                                                tot.conf.ci.lo = quantile(tot.conf, 0.025),
                                                                tot.conf.ci.hi = quantile(tot.conf, 0.975),
                                                                
                                                                flux.pred.med = median(agb_inc_pred),
                                                                flux.pred.ci.lo = quantile(agb_inc_pred, 0.025),                                                               flux.pred.ci.hi = quantile(agb_inc_pred, 0.99),
                                                                flux.pred.ci.hi = quantile(agb_inc_pred, 0.975),
                                                                
                                                                flux.conf.med = median(agb_inc_conf),
                                                                flux.conf.ci.lo = quantile(agb_inc_conf, 0.025),
                                                                flux.conf.ci.hi = quantile(agb_inc_conf, 0.975))
  
  
  total.plot$PLOT <- as.character(plot)
  
  b <- ggplot()+
    geom_ribbon(data = total.plot, aes(x = year, ymin = tot.pred.ci.lo, ymax = tot.pred.ci.hi), fill = "coral3")+
    geom_ribbon(data = total.plot, aes(x = year, ymin = tot.conf.ci.lo, ymax = tot.conf.ci.hi), fill = "skyblue4")+
    geom_line(data = total.plot, aes(x = year, y = tot.conf.med))+theme_bw(base_size = 12)+
    ylab(paste("Plot", plot, "stem  \n biomass (kg)"))+xlab("Year")+theme(panel.grid = element_blank())
  
  b.flux <- ggplot()+
    geom_ribbon(data = agbs , aes(x = year, ymin = flux.pred.ci.lo, ymax = flux.pred.ci.hi), fill = "coral3")+
    geom_ribbon(data = agbs, aes(x = year, ymin = flux.conf.ci.lo, ymax = flux.conf.ci.hi), fill = "skyblue4")+
    geom_line(data = agbs , aes(x = year, y = flux.pred.med), color = "black")+theme_bw(base_size = 12)+
    ylab(paste("Plot", plot, " stem  \n biomass increment (kg)"))+xlab("Year")+theme(panel.grid = element_blank())
  both.plot<- cowplot::plot_grid(b, b.flux, ncol = 1, align = "hv")
  cowplot::save_plot(paste0("outputs_biomass/timeseries_plots/Plot_biomass_inc_", plot, ".png"), both.plot, base_asp = 1.1)
  
  saveRDS(total.plot, paste0("outputs_biomass/total_plot_summaries/Plot_biomass_inc_", plot, ".RDS"))
  total.plot
}



system.time(test<-  organize.by.plot.full(plots[15]))
# user  system elapsed 
#  0.156   0.010   0.214 
plot <- plots[5]
biomass.total.inc.plots <- lapply(plots, organize.by.plot.full)
saveRDS(biomass.total.inc.plots, "outputs_biomass/organized_plots_list_agb_inc_tot.RDS")

organized.plots[[1]]
x <- 1

summarise.plot.biomass.year <- function(x){
  
  total.plot <- data.frame(organized.plots[[x]]) %>% group_by(year)%>% dplyr::summarise(med.pred.tot = sum(median_pred), 
                                                                                        ci.lo.pred.tot = sum(ci.lo_pred), 
                                                                                        ci.hi.pred.tot = sum(ci.hi_pred), 
                                                                                        med.conf.tot = sum(median), 
                                                                                        ci.lo.conf.tot = sum(ci.lo), 
                                                                                        ci.hi.conf.tot=sum(ci.hi))
  
  
  total.plot$PLOT <- as.character(plots[x])
  total.plot
}


pipo.plot.totals <- lapply(1:length(plots), summarise.plot.biomass.year)
pipo.plots.df <- do.call(rbind, pipo.plot.totals)
write.csv(pipo.plots.df, "outputs_biomass/yearly_plot_pipo_AZ_biomass_by_ci.csv", row.names = FALSE)




# calculate increment

# calculate increment

agb.df  <- pipo.plt.cov.biomass %>%
  group_by(PLOT) %>%
  mutate(agb_inc = med.pred.tot - lag(med.pred.tot, default = 0), 
         agb_inc_ci.lo = ci.lo.pred.tot - lag(ci.lo.pred.tot, default = 0) , 
         agb_inc_ci.hi = ci.hi.pred.tot - lag(ci.hi.pred.tot, default = 0) , 
         agb_inc_conf = med.conf.tot - lag(med.conf.tot, default = 0), 
         agb_inc_conf_ci.lo = ci.lo.conf.tot - lag(ci.lo.conf.tot, default = 0), 
         agb_inc_conf_ci.hi= ci.lo.conf.tot - lag(ci.lo.conf.tot, default = 0))


ggplot(agb.df, aes(x = year, y= agb_inc, group = PLOT, color = SICOND))+geom_line()+xlim(1995,2010)+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+ylim(-500,500)

ggplot(agb.df, aes(x = year, y= agb_inc, group = PLOT, color = SDI))+geom_line()+xlim(1995,2010)+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+ylim(-500,500)

# plot total plot biomass vs biomass increment

ggplot(agb.df %>% filter(year >= 1995 & year <=2010), aes(x = med.pred.tot, y= agb_inc, color = SDI))+geom_point()+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+xlab("Median predicted plot biomass (kg)")

ggplot(agb.df %>% filter(year >= 1995 & year <=2010), aes(x = med.pred.tot, y= agb_inc, color = SICOND))+geom_point()+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+xlab("Median predicted plot biomass (kg)")

ggplot(agb.df %>% filter(year >= 1995 & year <=2010), aes(x = med.pred.tot, y= agb_inc, color = MAP))+geom_point()+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+xlab("Median predicted plot biomass (kg)")


ggplot(agb.df %>% filter(year >= 1995 & year <=2010), aes(x = med.pred.tot, y= agb_inc, color = MAT))+geom_point()+theme_bw()+ylab("Median predicted plot biomass increment (kg)")+xlab("Median predicted plot biomass (kg)")

# for each plot, generate a timeseries from 1994-2010:
# plot biomass trajectory over time
# plot biomass increment + uncertainty over time
plot_plot_biomass<- function(x){
  print(x)
  agb.plt.focal <- agb.df %>% filter (PLOT %in% plots[x]) %>% filter(year >=1995 & year <=2010)
  
  ggbiomass<- ggplot() +geom_ribbon(data = agb.plt.focal, aes(x = year, ymin = ci.lo.pred.tot, ymax = ci.hi.pred.tot), fill = "coral4")+geom_ribbon(data = agb.plt.focal, aes(x = year, ymin = ci.lo.conf.tot, ymax = ci.hi.conf.tot), fill = "blue")+geom_line(data = agb.plt.focal, aes(x = year, y = med.pred.tot))+ylab(paste("Biomass for plot", plots[x],"(kg)"))+theme_bw()+theme(panel.grid = element_blank())
  
  ggsave(paste0("diam_data/outputs_biomass/biomass_plots/PLOT_biomass_", plots[x],"_1995_2010.png"),plot = ggbiomass,  device = "png", width = 6, height = 4, units = "in", dpi = 200)
  
  
}

plot_plot_biomass(x = 2)
lapply(1:length(unique(plots)), plot_plot_biomass)

ggplot()+ geom_ribbon(data = agb.plt.focal, aes(x = year, ymin = agb_inc_ci.lo, ymax = agb_inc_ci.hi), fill = "coral4")+geom_ribbon(data = agb.plt.focal, aes(x = year, ymin = agb_inc_conf_ci.lo, ymax = agb_inc_conf_ci.hi), fill = "blue")+geom_line(data = agb.plt.focal, aes(x = year, y = agb_inc))+ylab(paste("Biomass for plot", plots[x],"(kg)"))+theme_bw()+theme(panel.grid = element_blank())
```






# plot out proportion of biomass uncertainty over time

```{r}
pred.intervals.df <- do.call(rbind, pred.intervals.all)
pred.intervals.df$tree <- rep(1:length(pred.intervals.all), sapply(pred.intervals.all, nrow)) 

conf.intervals.df <- do.call(rbind, conf.intervals.all)
conf.intervals.df$tree <- rep(1:length(conf.intervals.all), sapply(conf.intervals.all, nrow)) 

year.time <- data.frame(time = 1:53, 
                        year = 1966:2018)

colnames(pred.intervals.df) <- c("time", "pred_med", "pred_ci.lo", "pred_ci.hi", "tree")
colnames(conf.intervals.df) <- c("time", "conf_med", "conf_ci.lo", "conf_ci.hi", "tree")
all.intervals <- left_join(pred.intervals.df, conf.intervals.df, by =c("time", "tree"))
all.intervals.yr <- left_join(all.intervals, year.time, by = c("time"))
head(all.intervals.yr)

all.intervals.yr$pred_t_ci.hi <- all.intervals.yr$pred_ci.hi - all.intervals.yr$conf_ci.hi
all.intervals.yr$pred_t_ci.lo <- all.intervals.yr$pred_ci.lo - all.intervals.yr$conf_ci.lo
all.intervals.yr$pred_t_med <- all.intervals.yr$pred_med - all.intervals.yr$conf_med

# get proportion of un certinat in the confidence intervals
all.intervals.yr$conf_prop_med <- all.intervals.yr$conf_med/(all.intervals.yr$pred_med + all.intervals.yr$conf_med)

all.intervals.yr$conf_prop_ci.hi <-all.intervals.yr$conf_ci.hi/(all.intervals.yr$conf_ci.hi + all.intervals.yr$pred_ci.hi)

all.intervals.yr$conf_prop_ci.lo<- all.intervals.yr$conf_ci.lo/(all.intervals.yr$conf_ci.lo + all.intervals.yr$pred_ci.lo)

# get proportion of un certinat in the prediction intervals
all.intervals.yr$pred_prop_med <- all.intervals.yr$pred_med/(all.intervals.yr$conf_med + all.intervals.yr$pred_med)

all.intervals.yr$pred_prop_ci.hi <-all.intervals.yr$pred_ci.hi/(all.intervals.yr$conf_ci.hi + all.intervals.yr$pred_ci.hi)

all.intervals.yr$pred_prop_ci.lo<- all.intervals.yr$pred_ci.lo/(all.intervals.yr$conf_ci.lo + all.intervals.yr$pred_ci.lo)

all.summary <- all.intervals.yr %>% group_by(year) %>% summarise(prop.p.ci.lo.all = mean(pred_prop_ci.lo),
                                                                 prop.p.ci.hi.all = mean(pred_prop_ci.hi),
                                                                 prop.p.ci.med.all = mean(pred_prop_med),
                                                                 prop.c.ci.lo.all = mean(conf_prop_ci.lo),
                                                                 prop.c.ci.hi.all = mean(conf_prop_ci.hi),
                                                                 prop.c.ci.med.all = mean(conf_prop_med))



med.summary <- all.summary %>% dplyr::select(year, prop.p.ci.med.all, prop.c.ci.med.all) %>% gather(simtype, variance, -year)


# var2 <- all.summary %>%
#    gather(simtype, variance, -x)
# 
#var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
prop.var <- ggplot(med.summary, aes(x=year, fill = simtype))+
  geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
  ylab("Percentage of total variance (%)")+
  xlab("Year")+
  #scale_fill_manual(values = my_cols, name = NULL,
  #                 labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()
prop.var


ggplot(data= all.summary, aes(x = year, y = prop.p.ci.lo.all))+geom_bar(stat = "identity")
hist(all.intervals.yr$conf_prop_ci.hi)
ggplot()+geom_ribbon(data = pred.intervals.all[[2]], aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "blue")+geom_line(data = pred.intervals.all[[2]], aes(x = time, y = median))+geom_ribbon(data = conf.intervals.all[[2]], aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "coral4")+geom_line(data = conf.intervals.all[[2]], aes(x = time, y = median))+ylab("Biomass for tree 1 (kg)")+theme_bw()+theme(panel.grid = element_blank())

```

