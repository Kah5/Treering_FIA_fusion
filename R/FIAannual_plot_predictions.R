library(tidyverse)
library(dplyr)
library(ggplot2)

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
unique(PLOT$PREV_PLT_CN)

# Match up the tree and plot data
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]
TREE$PREV_PLT_CN <- PLOT$PREV_PLT_CN[match(TREE$PLT_CN, PLOT$CN)]
TREE$PREV_MEASYR <- PLOT$MEASYEAR[match(TREE$PREV_PLT_CN, PLOT$CN)]
TREE$PREV_TPA_UNADJ <- TREE$TPA_UNADJ[match(TREE$PREV_TRE_CN, TREE$CN)]
#TREE$PREV_STATUS_CD <- PLOT$PREV_PLT_CN[match(TREE$PLT_CN, PLOT$CN)]
#--------------------------------------------------------------------------------------------- 
# Subset by trees in our study plots
#--------------------------------------------------------------------------------------------- 
all.pipo.trees <- TREE %>% filter(SPCD == "122" & STATECD %in% c("4", "8","16", "30", "35", "49", "56"))

length(unique(all.pipo.trees$PLT_CN))

PIPO.ll <- PLOT %>% filter(CN %in% unique(all.pipo.trees$PLT_CN)) %>% select(CN, LAT, LON, PREV_PLT_CN)
summary(PIPO.ll$PREV_PLT_CN)
PIPO.ll.REMEAS <- PIPO.ll %>% filter(!is.na(PREV_PLT_CN))

#PLOTS <- PLOT %>% filter(CN %in% unique(cov.data.regional$PLT_CN))
TREEinPLOTS <- TREE %>% filter(PLT_CN %in% unique(PIPO.ll.REMEAS$CN))
length(TREEinPLOTS$CN)
# 17968 trees! 

#TREEinPLOTS <- TREE %>% filter(AGENTCD >= 0 & STATUSCD ==1 & DIA > 1) %>% filter( PLT_CN %in% unique(PIPO.ll.REMEAS$CN))
#length(TREEinPLOTS$CN)

additional.trees <- TREEinPLOTS %>% group_by(SPCD) %>% summarise(number = n())

# png(height = 3, width = 10, units = "in", res = 150, "data/output/barplot_additional_trees.png")
# ggplot(additional.trees, aes(x = as.character(SPCD), y = number))+geom_bar(stat = "identity")
# dev.off()
# # most are PIPO, but second most are gambel oak QUGA, then PSME


additional.trees.by.plot <- TREEinPLOTS %>% group_by(PLT_CN, SPCD) %>% summarise(number = n()) %>%spread(SPCD, number)
#unique(TREEinPLOTS$MEASYR)

additional.trees.by.plot$ntrees <- rowSums(additional.trees.by.plot[,2:ncol(additional.trees.by.plot)], na.rm = TRUE)
additional.trees.by.plot$prop.pipo.trees <- additional.trees.by.plot$`122`/additional.trees.by.plot[,"ntrees"]


# select all the plots with >0.90 of the plot as pipo:
hist(as.numeric(additional.trees.by.plot$prop.pipo.trees$ntrees))

all.pipo.idx <- additional.trees.by.plot$prop.pipo.trees$ntrees >0.9


length(as.character(additional.trees.by.plot[all.pipo.idx,]$PLT_CN))
fiapltcns <- as.character(additional.trees.by.plot[all.pipo.idx,]$PLT_CN)
fiapltcns.all <- additional.trees.by.plot$PLT_CN
# 640 plots with >90% pipo trees & a prev_plt_cn number...much more manageable than 6000

# check these pipo plots to see if they have prev plt cns:
first.surveys <- PLOT %>% filter(CN %in% fiapltcns.all)

summary(first.surveys$PREV_PLT_CN)
# # none appear to have previous plot cns, but lets track what happens with these:
# second.surveys <- PLOT %>% filter(PREV_PLT_CN %in% fiapltcns.all ) %>%filter(!is.na(PREV_PLT_CN))
# PLOT_REMEAS <- PLOT %>% filter(!is.na(PREV_PLT_CN))# %>%filter(!is.na(PREV_PLT_CN))
# summary(PLOT_REMEAS$PREV_PLT_CN %in% unique(additional.trees.by.plot$PLT_CN))
# 
initial.survey <- TREE %>% filter(PLT_CN %in% first.surveys$PREV_PLT_CN)

# get SDI estimates for the new plots:
SDIs <- initial.survey  %>% ungroup() %>%  filter(DIA > 1) %>%
  group_by(PLT_CN, STATECD, PLOT, SUBP , COUNTYCD,  MEASYR) %>%
  summarise(ntrees_static = n(),
            TPA_static =sum(TPA_UNADJ), 
            Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/ntrees_static), 
            SDIdq_static = ((Dq_static/10)^1.6)*TPA_static, #calculate SDI (Summation Method) on the subplot:
            SDIs_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE))#, ## calculate SDI (Quadratic mean diameter) on the subplot:


#--------------------------------------------------------------------------------------------- 
# get X values from current trees
#--------------------------------------------------------------------------------------------- 
# for now lets just apply the pipo model to all (unrealistic)

dbh.measyr.newtrees <- initial.survey %>% select(CN, PLT_CN, SUBP,MEASYR, DIA) %>% mutate(DIA = DIA*2.54)

# get xdata:

year.df <- expand.grid(Year = 2001:2018, CN = unique(dbh.measyr.newtrees$CN))
cn.df <- unique(dbh.measyr.newtrees[,c("CN", "PLT_CN")])

year.cn.df <- left_join(year.df, cn.df)

dbh.measyr.repeats.full <- left_join(year.cn.df, dbh.measyr.newtrees)

spread.dbh.mat <- dbh.measyr.repeats.full %>% 
  mutate(DIA_measyr = ifelse(Year == MEASYR, DIA, NA)) %>% 
  select(-DIA, -MEASYR)  %>% 
  group_by(CN, PLT_CN) %>% tidyr::spread(Year, DIA_measyr)

#spread.dbh.mat <- spread.dbh.mat[!duplicated(spread.dbh.mat$CORE_CN),]

# combine with information on the MAP and MAT of these plots 

# read in the climate data:
all.clim.PLTCN <- read.csv( "data/pipo_FIA_all_tmax_ppt_v1.csv")
clim.data <- all.clim.PLTCN %>% filter(CN %in% unique(cn.df$PLT_CN) & year %in% 2001:2018) %>% select(lon,lat, CN, year, ppt.sum, Tmean_AprMayJun, MAT, MAP)

head(clim.data)

# can probably index cov.data.regional with the PLT_CN and plotid

# plotid.df <- data.frame(PLOTSTATE = unique(cov.data.regional$PLOTSTATE),
#                         plotid = 1:length(unique(cov.data.regional$PLOTSTATE)))
# 
# 
# cov.data.regional <- left_join(cov.data.regional, plotid.df, by = "PLOTSTATE")
unique.plts <- unique(clim.data [,c("CN", "MAP", "MAT")])
colnames(unique.plts)[1] <- "PLT_CN"
x.mat <- merge(unique.plts, spread.dbh.mat, by= c("PLT_CN"))
m <- 1

# get time series data:


# will need to match the core_CN from the plot
colnames(clim.data)[3] <- "PLT_CN"
pipo.climate.pltcn <- clim.data
#-------------------------------------------------------------------------------------
# Read in the climate data
#-------------------------------------------------------------------------------------

x <-"ppt.sum"
get_ordered_climate <- function(x){
  
  clim.subset <- unique(pipo.climate.pltcn[,c("PLT_CN", "lat","lon","year", x)])
  spread.tmax.fall.spr.mat <-  clim.subset  %>% filter(PLT_CN %in% spread.dbh.mat$PLT_CN)%>% 
    dplyr::select(lon, lat, PLT_CN, year, all_of(x)) %>% 
    group_by(lon, lat, PLT_CN) %>%  tidyr::spread( year, x, drop = T)
  
  
  # now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
  spread.tmax.fall.spr.mat.ordered <-  spread.tmax.fall.spr.mat[order(match(spread.tmax.fall.spr.mat$PLT_CN, spread.dbh.mat[,"PLT_CN"])),]
  climate.mat <- spread.tmax.fall.spr.mat.ordered#[,4:length(spread.tmax.fall.spr.mat.ordered)]
  climate.mat
}

#tmax.fallspr <- get_ordered_climate("tmax.fallspr")
wintP.wateryr <- get_ordered_climate("ppt.sum")
tmax.AprMayJun <- get_ordered_climate("Tmean_AprMayJun")
#tmax.monsoon <- get_ordered_climate("tmax.monsoon")
#TMAX <- get_ordered_climate("TMAX")

# -------------------------------------------------------------------------------
# read in the SDI time plot level data:
#--------------------------------------------------------------------------------
# tv.sdi <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v4.RDS"))
# 
# # get the tv.sdi estimates from just the x.mat plots and subplots?
# # reshape tv.sdi:
# tv.sdi.spread <- tv.sdi %>% select(STATECD, PLOT,SUBP,PLT_CN, Year,SDIs) %>%group_by(STATECD, PLOT,SUBP,PLT_CN) %>% spread(Year, SDIs) %>% select(`1966`:`2001`)


year.plt.df <- expand.grid(Year = 2001:2018, PLT_CN = unique(dbh.measyr.newtrees$PLT_CN))
SDIs.all <- left_join( SDIs, year.plt.df, by = "PLT_CN")
sdi.spread <- SDIs.all %>% ungroup() %>% select(PLT_CN, SUBP, SDIs_static, Year) %>% group_by(PLT_CN, SUBP)%>% spread(Year, SDIs_static)

SDI.mat.PLT.subp <- left_join(unique(x.mat[,c("PLT_CN", "SUBP")]), sdi.spread)


# Fill in the timeseires:
SDI.mat.PLT.subp
SDI.matrix.plt.subp <- SDI.mat.PLT.subp[,3:length(SDI.mat.PLT.subp)]

# # because jags won't take NA values as predictors, we need to give values..so replace NA with the max or min
for(i in 1:nrow(SDI.matrix.plt.subp)){
  for(t in 2:length(SDI.matrix.plt.subp)){
   # if(is.na(SDI.matrix.plt.subp[i,t])){
      SDI.matrix.plt.subp[i,t] <- SDI.matrix.plt.subp[i,t-1]
    #}
    # if(!is.na(SDI.matrix.plt.subp[i,t])){
    #   SDI.matrix.plt.subp[i,t] <- min(SDI.matrix.plt.subp[i,], na.rm = TRUE)
    # }
  }
  # if it is still NA, replace with the smallest SDI value in that row:

}


summary(SDI.matrix.plt.subp)


# relink to the rest of the dataset:

SDI.mat.PLT.subp[,3:length(SDI.mat.PLT.subp)] <- SDI.matrix.plt.subp



# combine the climate & SDI time data for each plot and standardize:
x <- as.matrix(SDI.mat.PLT.subp[,3:ncol(SDI.mat.PLT.subp)])

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
SDIscaled[,3:ncol(SDIscaled)] <- standardize.vector(as.matrix(SDI.mat.PLT.subp[,3:ncol(SDI.mat.PLT.subp)]))

# scale the MAP and MAT data:

x.mat$MAP.scaled <- standardize.vector(x.mat$MAP)
x.mat$MAT.scaled <-  standardize.vector(x.mat$MAT)

#--------------------------------------------------------------------------------------------- 
# Read in the posterior parameter estimates
#--------------------------------------------------------------------------------------------- 
# this model models increment, not diameter...
# I didnt save the Xvals for this model, but just using it to get the code setup

library(rjags)
#jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/Regional_mu_testing_mvn-2022-05-19-20-07-51.6/IGFRegional_mvnmu_revCorr_xfixed.rds"))
jags.comb <- readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/inc_treerand_model-2022-07-20-21-17-53.3/IGFRegional_incifelse_T0.rds")) # tree random effect

output.base.name <- "Regional_incifelse_T0"
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
tau_dbh <- out[,c("tau_dbh")]#out[,grep(pattern = "tau",colnames(out))]


#------------------------------------------------------------------------------------
# Start to make tree/plot forecasts with mortality in them
#--------------------------------------------------------------------------------------------- 
# forecast from posterior estimates to get X values
#--------------------------------------------------------------------------------------------- 

# allometric uncertainty part of pecan stopped working for me
# # now make a function where you calculate plot level biomass
# devtools::install_github("PecanProject/pecan",subdir="base/logger")
# devtools::install_github("PecanProject/pecan",subdir="modules/allometry")
# devtools::install_github("PecanProject/pecan",subdir="allometry")
# 
# 
# library(PEcAn.allometry)
# library(devtools)
# library(reshape2)
# library(ggplot2)
# library(tidyverse)


#data("allom.components")
#allom.components

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


#validation.time.dbh.changingsdi.zeroinc.SDIscaled( plot = unique(plots)[6])
#lapply(unique(plots)[1:2], validation.time.dbh.changingsdi.zeroinc.SDIscaled)



# we need the list of trees (combined), 
# and the diameter estimates for all the trees:
plot = unique(unique.plts$PLT_CN)[1]

plot = '4718375010690'
density.dependent = FALSE
density.independent = FALSE
rcp <- "rcp26"


set.seed(22)
source("R/plot2AGB_kayeFVS.R")
source("R/biomass.changing.SDI.FIAannual.R")
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

plot <- "12280908010690"
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.7)})

plot <- unique(unique.plts$PLT_CN)[2]
unique(unique.plts$PLT_CN) %in% "12289739010690"




lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", aggressiveCC = TRUE, SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26",  aggressiveCC = TRUE, SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26", aggressiveCC = TRUE,  SDI.ratio.DD = 0.7)})

lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.changingsdi.SDIscaled.FIA (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26",  aggressiveCC = TRUE, SDI.ratio.DD = 0.7)})
biomass.changingsdi.SDIscaled.FIA (plot = unique(unique.plts$PLT_CN)[1], density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26",  aggressiveCC = TRUE, SDI.ratio.DD = 0.7)



#---------------------------------------------------------------------------------
# Sensitivity analysis with no SDI and climate changes
#---------------------------------------------------------------------------------
# double cc mortality runs:
source("R/plot2AGB_kayeFVS.R")
source("R/biomass.sensitivity.FIA.R")

biomass.sensitivity.FIA (plot = unique(unique.plts$PLT_CN)[1], density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26",  aggressiveCC = FALSE, SDI.ratio.DD = 0.7)

lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.sensitivity.FIA (plot = x, density.dependent = TRUE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.sensitivity.FIA(plot = x, density.dependent = FALSE, density.independent = FALSE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.sensitivity.FIA (plot = x, density.dependent = TRUE, density.independent = FALSE , scenario = "rcp26", SDI.ratio.DD = 0.7)})
lapply(unique(unique.plts$PLT_CN)[1:417],FUN = function(x){biomass.sensitivity.FIA (plot = x, density.dependent = FALSE, density.independent = TRUE , scenario = "rcp26", SDI.ratio.DD = 0.7)})




#---------------------------------------------------------------------------------
# compare plot level mortality to the DIDD mortality in our forecasts
#---------------------------------------------------------------------------------
plot <- unique(unique.plts$PLT_CN)[1]


pred.obs.mort.plot <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC"){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    newTREE <- TREE %>% dplyr::filter (PREV_PLT_CN %in% plot)
    STATUSCD_change <- newTREE %>% group_by(STATUSCD,  PREV_STATUS_CD) %>% 
      mutate(dead.class = ifelse(STATUSCD == 1 & PREV_STATUS_CD == 1, "live", 
                                 ifelse(STATUSCD == 2 & PREV_STATUS_CD == 1, "died in inventory", 
                                        ifelse(STATUSCD == 2 & PREV_STATUS_CD == 2, "died before first survey",
                                               ifelse(STATUSCD == 1 & is.na(PREV_STATUS_CD) == "TRUE","ingrowth", 
                                                      ifelse(STATUSCD == 3 & PREV_STATUS_CD == 1,"cut/removed in inventory", 
                                                             ifelse(STATUSCD == 3 & PREV_STATUS_CD == 3, "cut/removed before first survey", "ingrowth")))))))
    newTREE$INV_Period <- newTREE$MEASYR - newTREE$PREV_MEASYR
    INVperiod <- mean(newTREE$INV_Period, na.rm =TRUE)
    
    mort.summary <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% summarise(mort_peryr_perha = sum(TPAMORT_UNADJ, na.rm =TRUE))
    rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
   # mort.scheme <- "DIDD"
    scenario <- "rcp26"
    #plot2AGB_nomort.31457625010690.rcp26.0.7.fixed.mort.rate.mort.prob.Rdata
    #load(file.path(paste0("biomass_dataFIAannual/plot2AGB_",mort.scheme,".",as.character(plot),".",scenario,".",SDI.ratio.DD,".fixed.mort.rate.mort.prob.Rdata")))
    #load(file.path(paste0("biomass_dataFIAannual/plot2AGB_DIDD.11790384010690.rcp26.0.7.fixed.mort.rate.mort.prob.Rdata")))
    #"biomass_dataFIAannual/plot2AGB_DIDD.11789975010690.rcp26.0.7.doubleCC.fixed.mort.rate.mort.prob.Rdata"
   #plot <- "11789975010690"
    if(cc.scenario == "doubleCC"){
    load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario, ".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
    }else{
      load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario,".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
      
    }
      #tpa.live[1,,1] # live TPA at 2001
    sim.nmort <- sum(tpa.dead[1,,19])

    pred.obs <- data.frame(forecasted.nmort = sim.nmort, 
               obs.mort = mort.summary$mort_peryr_perha*INVperiod, 
               obs.rem = rem.summary$rem_peryr_perha*INVperiod, 
               INV.period = INVperiod)
    pred.obs
  }
}
plot <- 31367645010690
p.o.mort.double.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){pred.obs.mort.plot (plot = x, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.mort.double.cc.df <- do.call(rbind, p.o.mort.double.cc )
p.o.mort.double.cc.df$mort.scheme <- "DIDD"
p.o.mort.double.cc.df$cc.scheme <- "doubleCC"


p.o.mort.single.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){pred.obs.mort.plot (plot = x, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.mort.single.cc.df <- do.call(rbind, p.o.mort.single.cc )
p.o.mort.single.cc.df$mort.scheme <- "DIDD"
p.o.mort.single.cc.df$cc.scheme <- "singleCC"

DIDD.mort.p.o <- rbind(p.o.mort.single.cc.df, p.o.mort.double.cc.df)

png(height = 4, width = 6, units = "in", res = 200, "outputs/forecasted_obs_annualPlot_mort_DDID_SDI0.7_prob.mort.reduced.mort.rate.png")
ggplot(DIDD.mort.p.o  , aes( forecasted.nmort,  obs.mort))+geom_point()+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+theme_bw()+facet_wrap(~cc.scheme)
dev.off()


# compare DIDD across singley and double CC

# now for DI only
p.o.mort.double.cc.DI <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){pred.obs.mort.plot (plot = x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.mort.double.cc.DI.df <- do.call(rbind, p.o.mort.double.cc.DI )
p.o.mort.double.cc.DI.df$mort.scheme <- "DIonly"
p.o.mort.double.cc.DI.df$cc.scheme <- "doubleCC"


p.o.mort.single.cc.DI <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){pred.obs.mort.plot (plot = x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.mort.single.cc.DI.df <- do.call(rbind, p.o.mort.single.cc.DI )
p.o.mort.single.cc.DI.df$mort.scheme <- "DIonly"
p.o.mort.single.cc.DI.df$cc.scheme <- "singleCC"


DIonly.mort.p.o <- rbind(p.o.mort.single.cc.DI.df, p.o.mort.double.cc.DI.df)


png(height = 4, width = 6, units = "in", res = 200, "outputs/forecasted_obs_annualPlot_mort_DIonly_SDI0.7_prob.mort.reduced.mort.rate.png")
ggplot(DIonly.mort.p.o , aes( forecasted.nmort,  obs.mort))+geom_point()+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+theme_bw()+facet_wrap(~cc.scheme)
dev.off()

summary(lm(data = p.o.mort.double.cc.df, obs.mort ~ forecasted.nmort))

ggplot(p.o.mort.df, aes( forecasted.nmort))+geom_histogram()+theme_bw()+xlim(0,1200)
ggplot(p.o.mort.df, aes( obs.mort))+geom_histogram()+theme_bw()+xlim(0,1200)


#------------------------------------------------------------------------


# compare diamter distributions of dead trees with diamete distributions of forecasted dead trees
plot <- "5378995010690"
get.diam.mort.plot <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC" ){
  
  
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    newTREE <- TREE %>% dplyr::filter (PREV_PLT_CN %in% plot)
    newTREE$PREVDIA
    STATUSCD_change <- newTREE %>% group_by(STATUSCD,  PREV_STATUS_CD) %>% 
      mutate(dead.class = ifelse(STATUSCD == 1 & PREV_STATUS_CD == 1, "live", 
                                 ifelse(STATUSCD == 2 & PREV_STATUS_CD == 1, "died in inventory", 
                                        ifelse(STATUSCD == 2 & PREV_STATUS_CD == 2, "died before first survey",
                                               ifelse(STATUSCD == 1 & is.na(PREV_STATUS_CD) == "TRUE","ingrowth", 
                                                      ifelse(STATUSCD == 3 & PREV_STATUS_CD == 1,"cut/removed in inventory", 
                                                             ifelse(STATUSCD == 3 & PREV_STATUS_CD == 3, "cut/removed before first survey", "ingrowth")))))))
    newTREE$INV_Period <- newTREE$MEASYR - newTREE$PREV_MEASYR
    INVperiod <- mean(newTREE$INV_Period, na.rm =TRUE)
    STATUSCD_change$PREVDIA*2.54
    
    
   
    
    dead.diams <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% 
      dplyr::select(CN, dead.class, PREVDIA, TPAMORT_UNADJ, TPA_UNADJ, PREV_TPA_UNADJ, AGENTCD) %>% mutate(PREVDBH = PREVDIA*2.54)
    #rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
   # mort.scheme <- "DIDD"
    scenario <- "rcp26"
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario, ".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
    }else{
      load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario, ".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
      
    }
    #  diam.live[2,,1]
    # tpa.dead[1,,19]
    # tpa.live[1,,19]
    
    forecast.dead <- data.frame(treeid = 1:length(diam.live[2,,1]), 
               dead.class = "died in forecast", 
               PREVDIA = diam.live[2,,1],
               TPAMORT_UNADJ = tpa.dead[1,,19], 
               PREV_TPA_UNADJ = tpa.live[1,,1], 
               PREVDBH = diam.live[2,,1], 
               type = "forecast", 
               plot = plot, 
               mort.scheme = mort.scheme, 
               cc.scenario = cc.scenario, 
               AGENTCD = "forecasted")
    
    forecast.dead$dead.class <- ifelse(forecast.dead$TPAMORT_UNADJ == 0, "all live in forecast", "died in forecast")
    
    if(nrow(dead.diams) == 0){
      "no dead inventory trees"
      observed.forecasted.dead <- forecast.dead
    }else{
    observed.dead <- data.frame(treeid = dead.diams$CN, 
                                dead.class = "died in inventory", 
                                PREVDIA = dead.diams$PREVDIA,
                                TPAMORT_UNADJ = dead.diams$TPAMORT_UNADJ, 
                                PREV_TPA_UNADJ = dead.diams$PREV_TPA_UNADJ, 
                                PREVDBH = dead.diams$PREVDBH, 
                                type = "observation", 
                                plot = plot, 
                                mort.scheme = mort.scheme, 
                                cc.scenario = cc.scenario, 
                                AGENTCD = dead.diams$AGENTCD)
    
    #tpa.live[1,,1] # live TPA at 2001
    observed.forecasted.dead <- rbind(observed.dead, forecast.dead)
    }
    observed.forecasted.dead
  }
}
p.o.mort.diams <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.mort.diams.df <- do.call(rbind, p.o.mort.diams)


p.o.mort.diams.single.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.mort.diams.single.cc.df <- do.call(rbind, p.o.mort.diams.single.cc )

DIonly.diams.mort.p.o <- rbind(p.o.mort.diams.df, p.o.mort.diams.single.cc.df)

png(height= 4, width = 4, res = 100, units = "in", "outputs/dead.tree.diams.forecasted.observed.DIonly_SDI0.7.reduced.mort.rate.mort.prob.png")
ggplot(DIonly.diams.mort.p.o, aes(x = PREVDBH, y = TPAMORT_UNADJ, color = dead.class))+geom_point()+facet_wrap(~cc.scenario)
dev.off()

png(height= 8, width = 8, res = 100, units = "in", "outputs/dead.tree.histogram.forecasted.observed.DIonly_SDI0.7.reduced.mort.rate.mort.prob.png")
ggplot(DIonly.diams.mort.p.o, aes( y = TPAMORT_UNADJ, fill = dead.class))+geom_histogram()+facet_grid(cc.scenario~dead.class)
dev.off()

png(height= 4, width = 8, res = 100, units = "in", "outputs/dead.tree.diam.histogram.forecasted.observed.DIonly_SDI0.7.reduced.mort.rate.mort.prob.png")
ggplot(DIonly.diams.mort.p.o, aes( y = PREVDBH, fill = dead.class))+geom_histogram()+facet_wrap(cc.scenario~dead.class)
dev.off()

# do the same for the DIDD scenario:


p.o.mort.diams.DIDD.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.mort.diams.DIDD.double.df <- do.call(rbind, p.o.mort.diams.DIDD.double)


p.o.mort.diams.single.DIDD.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.mort.diams.single.DIDD.cc.df <- do.call(rbind, p.o.mort.diams.single.DIDD.cc )

DIDD.diams.mort.p.o <- rbind(p.o.mort.diams.DIDD.double.df, p.o.mort.diams.single.DIDD.cc.df )

png(height= 4, width = 4, res = 100, units = "in", "outputs/dead.tree.diams.forecasted.observed.DIDD_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DIDD.diams.mort.p.o, aes(x = PREVDBH, y = TPAMORT_UNADJ, color = dead.class))+geom_point()+facet_wrap(~cc.scenario)
dev.off()

png(height= 8, width = 8, res = 100, units = "in", "outputs/dead.tree.histogram.forecasted.observed.DIDD_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DIDD.diams.mort.p.o, aes( y = TPAMORT_UNADJ, fill = dead.class))+geom_histogram()+facet_grid(cc.scenario~dead.class)
dev.off()

png(height= 4, width = 8, res = 100, units = "in", "outputs/dead.tree.diam.histogram.forecasted.observed.DIDD_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DIDD.diams.mort.p.o, aes( y = PREVDBH, fill = dead.class))+geom_histogram()+facet_wrap(cc.scenario~dead.class)
dev.off()


p.o.mort.diams.DDonly.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DDonly", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.mort.diams.DDonly.double.df <- do.call(rbind, p.o.mort.diams.DDonly.double)


p.o.mort.diams.single.DDonly.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get.diam.mort.plot(x, mort.scheme = "DDonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.mort.diams.single.DDonly.cc.df <- do.call(rbind, p.o.mort.diams.single.DDonly.cc )

DDonly.diams.mort.p.o <- rbind(p.o.mort.diams.DDonly.double.df, p.o.mort.diams.single.DDonly.cc.df )

png(height= 4, width = 4, res = 100, units = "in", "outputs/dead.tree.diams.forecasted.observed.DDonly_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DDonly.diams.mort.p.o, aes(x = PREVDBH, y = TPAMORT_UNADJ, color = dead.class))+geom_point()+facet_wrap(~cc.scenario)
dev.off()

png(height= 8, width = 8, res = 100, units = "in", "outputs/dead.tree.histogram.forecasted.observed.DDonly_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DDonly.diams.mort.p.o, aes( y = TPAMORT_UNADJ, fill = dead.class))+geom_histogram()+facet_grid(cc.scenario~dead.class)
dev.off()

png(height= 4, width = 8, res = 100, units = "in", "outputs/dead.tree.diam.histogram.forecasted.observed.DDonly_SDI0.7.fixed.mort.rate.mort.prob.png")
ggplot(DDonly.diams.mort.p.o, aes( y = PREVDBH, fill = dead.class))+geom_histogram()+facet_wrap(cc.scenario~dead.class)
dev.off()

##################################################################################3
# Look at what drives observed plot level mortality
##################################################################################3
plot.death <- DIonly.diams.mort.p.o %>% filter(dead.class %in% "died in inventory" & cc.scenario %in% "singleCC")

AGENTCD.df <- data.frame(AGENTCD = c("00", "10", "20","30", "40", "50", "60", "70", "80"), 
                         AGENT = c("no agent recorded", "insect", "disease", "fire", 
                                   "animal", "weather", "vegetation", "unknown", "silviculture"))
plot.death.agents <- left_join(plot.death, AGENTCD.df)


png(height= 8, width = 8, res = 100, units = "in", "outputs/dead.tree.diam.histogram_actual_AGENT.png")
ggplot(plot.death.agents, aes(PREVDBH))+geom_histogram()+facet_wrap(~AGENT)+xlab("Initial Survey Tree Diameter (cm)")
dev.off()


plot.dead.summary <- plot.death.agents %>% group_by(AGENT, AGENTCD) %>% summarise(DEAD.n = sum(TPAMORT_UNADJ, na.rm =TRUE))

png(height= 8, width = 8, res = 100, units = "in", "outputs/dead.tree.diam.histogram_actual_AGENT.png")
ggplot(plot.dead.summary, aes(x = AGENT, y = DEAD.n, fill = AGENT))+geom_point()
dev.off()


png(height= 3, width = 5, res = 100, units = "in", "outputs/PIPO_TPAMORT_UNADJ_actual_AGENT.png")
ggplot(plot.dead.summary, aes(fill=AGENT, y=DEAD.n, x=AGENT)) + 
  geom_bar(position="dodge", stat="identity") + ylab("Total TPAMORT_UNADJ observed")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

##################################################################################3
# Plot up stand level biomass and compare to the observed
##################################################################################3
get_biomass_ests <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC" ){
  cat(paste0("getting pred vs obs for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    newTREE <- TREE %>% dplyr::filter (PREV_PLT_CN %in% plot)
    newTREE$PREVDIA
    STATUSCD_change <- newTREE %>% group_by(STATUSCD,  PREV_STATUS_CD) %>% 
      mutate(dead.class = ifelse(STATUSCD == 1 & PREV_STATUS_CD == 1, "live", 
                                 ifelse(STATUSCD == 2 & PREV_STATUS_CD == 1, "died in inventory", 
                                        ifelse(STATUSCD == 2 & PREV_STATUS_CD == 2, "died before first survey",
                                               ifelse(STATUSCD == 1 & is.na(PREV_STATUS_CD) == "TRUE","ingrowth", 
                                                      ifelse(STATUSCD == 3 & PREV_STATUS_CD == 1,"cut/removed in inventory", 
                                                             ifelse(STATUSCD == 3 & PREV_STATUS_CD == 3, "cut/removed before first survey", "ingrowth")))))))
    newTREE$INV_Period <- newTREE$MEASYR - newTREE$PREV_MEASYR
    INVperiod <- mean(newTREE$INV_Period, na.rm =TRUE)
    STATUSCD_change$PREVDIA*2.54
    
    
    
    STATUSCD_change$DRYBIO_AG
    oldTREE$DRYBIO_AG
    
    # dead.diams <- STATUSCD_change %>% ungroup() %>% filter(dead.class == "died in inventory" ) %>% 
    #   dplyr::select(CN, dead.class, PREVDIA, TPAMORT_UNADJ, TPA_UNADJ, PREV_TPA_UNADJ, AGENTCD) %>% mutate(PREVDBH = PREVDIA*2.54)
    # #rem.summary <- STATUSCD_change  %>% ungroup() %>% filter(dead.class == "cut/removed in inventory" ) %>% summarise(rem_peryr_perha = sum(TPAREMV_UNADJ, na.rm =TRUE))
    
    
    
    # scale by TPAMORT_UNADJ to get trees per acre per year, 
    # may need to also cale by # inventory years
    # 
    # mort.scheme <- "DIDD"
    scenario <- "rcp26"
    if(cc.scenario == "doubleCC"){
      load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".", cc.scenario, ".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
    }else{
      load(paste0("biomass_dataFIAannual/plot2AGB_", mort.scheme, ".", plot, ".rcp26.", SDI.ratio.DD, ".",  cc.scenario, ".fixed.mort.rate.mort.prob.Rdata"))#,mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".fixed.mort.rate.mort.prob.Rdata")))
      
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
                           rcp = scenario,
                           cc.scenario = cc.scenario,
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
  
  obs.biomass <- data.frame(plot = c(plot,plot), 
                            DRYBIO_AG_lb_ha = c(sum(oldTREE$DRYBIO_AG*oldTREE$TPA_UNADJ), sum(STATUSCD_change$DRYBIO_AG*STATUSCD_change$TPA_UNADJ, na.rm =TRUE)),
                            year = c(unique(oldTREE$MEASYR), unique(STATUSCD_change$MEASYR)) )
  
  # convert lbs to kg
  
  obs.biomass$DRYBIO_AG_kg_ha <-  obs.biomass$DRYBIO_AG_lb_ha/2.205
  
  total.plot.obs <- left_join(total.plot, obs.biomass, by = c("plot", "year"))
  
  total.plot.obs
  }
  
  }
  
  
  

ggplot(data = total.plot.obs, aes(year, mAGB))+geom_line()+
  geom_point(data = total.plot.obs, aes(year, DRYBIO_AG_kg_ha))
# for nomort
p.o.biomass.nomort.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "nomort", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.biomass.nomort.double.df <- do.call(rbind, p.o.biomass.nomort.double)


p.o.biomass.single.nomort.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "nomort", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.biomass.single.nomort.cc.df <- do.call(rbind, p.o.biomass.single.nomort.cc )

# for DIDD
p.o.biomass.DIDD.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.biomass.DIDD.double.df <- do.call(rbind, p.o.biomass.DIDD.double)


p.o.biomass.single.DIDD.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DIDD", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.biomass.single.DIDD.cc.df <- do.call(rbind, p.o.biomass.single.DIDD.cc )



summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DIDD.cc.df))
summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DIDD.double.df))

ggplot(data = p.o.biomass.single.DIDD.cc.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data = p.o.biomass.single.DIDD.cc.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())

ggplot(data =p.o.biomass.DIDD.double.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data =p.o.biomass.DIDD.double.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())


p.o.biomass.DIonly.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.biomass.DIonly.double.df <- do.call(rbind, p.o.biomass.DIonly.double)


p.o.biomass.single.DIonly.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DIonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.biomass.single.DIonly.cc.df <- do.call(rbind, p.o.biomass.single.DIonly.cc )

summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DIonly.cc.df))
summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DIonly.double.df))

ggplot(data = p.o.biomass.single.DIonly.cc.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data = p.o.biomass.single.DIonly.cc.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme(panel.grid = element_blank())

ggplot(data =p.o.biomass.DIonly.double.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data =p.o.biomass.DIonly.double.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme(panel.grid = element_blank())


# for DDonly:

p.o.biomass.DDonly.double <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DDonly", SDI.ratio.DD = 0.7, cc.scenario = "doubleCC")})
p.o.biomass.DDonly.double.df <- do.call(rbind, p.o.biomass.DDonly.double)

p.o.biomass.single.DDonly.cc <- lapply(unique(unique.plts$PLT_CN)[1:10] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DDonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.biomass.single.DDonly.cc.df <- do.call(rbind, p.o.biomass.single.DDonly.cc )

p.o.biomass.single.DDonly.cc <- lapply(unique(unique.plts$PLT_CN)[1:417] , FUN = function(x){get_biomass_ests(x, mort.scheme = "DDonly", SDI.ratio.DD = 0.7, cc.scenario = "singleCC")})
p.o.biomass.single.DDonly.cc.df <- do.call(rbind, p.o.biomass.single.DDonly.cc )

summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DDonly.cc.df))
summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DDonly.double.df))


ggplot(data = p.o.biomass.single.DDonly.cc.df, aes( year, mAGB, group = plot))+geom_line()#+
  

ggplot(data = p.o.biomass.single.DDonly.cc.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data = p.o.biomass.single.DDonly.cc.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme(panel.grid = element_blank())

ggplot(data =p.o.biomass.DDonly.double.df, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data =p.o.biomass.DDonly.double.df, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())


p.o.biomass.all <- rbind(p.o.biomass.DDonly.double.df, p.o.biomass.single.DDonly.cc.df, 
                         p.o.biomass.DIonly.double.df, p.o.biomass.single.DIonly.cc.df, 
                         p.o.biomass.DIDD.double.df, p.o.biomass.single.DIDD.cc.df,
                         p.o.biomass.nomort.double.df, p.o.biomass.single.nomort.cc.df)


RMSE <- p.o.biomass.all %>% mutate(sq_error = (mAGB - DRYBIO_AG_kg_ha)^2) %>% group_by( mort.scheme, rcp, cc.scenario)%>%
  summarise(RMSE = sqrt(mean(sq_error, na.rm = TRUE)))
  
png(height = 8, width = 5, units = "in", res = 150, "outputs/FIAannual_pred_obs_biomass.mort.scheme.png")
ggplot(data =p.o.biomass.all, aes( DRYBIO_AG_kg_ha, mAGB))+geom_point()+
  geom_errorbar(data =p.o.biomass.all, aes( DRYBIO_AG_kg_ha, ymin = lowA, ymax = upA))+
  geom_abline(aes(intercept = 0, slope = 1), col = "red", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  facet_grid(mort.scheme~cc.scenario)+ylab("Forecasted AGB (kg/ha)")+xlab("FIA DRYBIO sum (kg/ha)")
dev.off()


# generate table of stats and residuals
#RMSE$rsq <- 
nomort.double.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.nomort.double.df))$r.squared
nomort.single.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.nomort.cc.df))$r.squared

DI.double.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DIonly.double.df))$r.squared
DI.single.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DIonly.cc.df))$r.squared

DD.double.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DDonly.double.df))$r.squared
DD.single.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DDonly.cc.df))$r.squared

DIDD.double.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.DIDD.double.df))$r.squared
DIDD.single.rsq <- summary(lm(mAGB ~ DRYBIO_AG_kg_ha, data = p.o.biomass.single.DIDD.cc.df))$r.squared

RMSE$rsq <- c(DD.double.rsq, 
              DD.single.rsq,
              DIDD.double.rsq , 
              DIDD.single.rsq, 
              DI.double.rsq, 
              DI.single.rsq, 
              nomort.double.rsq, 
              nomort.single.rsq)
write.csv(RMSE, "outputs/RMSE_FIAannual_cc.scenario_mort.scenario.csv")
