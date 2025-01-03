# new script for plotting up main effects in the stan models:


library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(here)

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
colnames(sigma.INC)[1] <- "L1"
sigma.DBH <- sigmas %>% filter(variable %in% "sigma_dbh")
sigma.ADD <- sigmas %>% filter(variable %in% "sigma_add")
colnames(sigma.ADD)[1] <- "L1"

cov.data.regional$treeid <- 1:length(cov.data.regional$CORE_CN)

get_mcmc_samples <- function(x, betas, nsamps){
  
  rnorm(nsamps, mean = as.numeric(betas %>% filter(L1 %in% x) %>% dplyr::select(median)), sd =  as.numeric(betas %>% filter(L1 %in% x) %>% dplyr::select(sd)))
}
nsamps <-  1000
#get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)


# sample from the population mean (mu) for the trees that dont have RE

alpha <- get_mcmc_samples(x = "mutree", betas = mus, nsamps = nsamps)


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

# get sigmas
sigma_INC <- get_mcmc_samples("sigma_inc", betas = sigma.INC, nsamps = nsamps)
sigma_ADD <- get_mcmc_samples("sigma_add", betas = sigma.ADD, nsamps = nsamps)

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


# lets make a grid where we hold values constant, using the range of covariate values:
ssm.data <- readRDS("data/regional_pipo_jags_formatted_data.RDS")
MAP.quant <- quantile(ssm.data$cov.data.regional$MAP, c(0.025, 0.5, 0.975))
MAT.quant <- quantile(ssm.data$cov.data.regional$MAT, c(0.025, 0.5, 0.975))
DIA.quant <- quantile(((ssm.data$cov.data.regional$DIA_cm-31.63)/10.61), c(0.025, 0.5, 0.975))# need to scale diameter
ppt.quant <- quantile(ssm.data$time_data$wateryrscaled, c(0.025, 0.5, 0.975))
tmax.quant <- quantile(ssm.data$time_data$tmaxAprMayJunscaled, c(0.025, 0.5, 0.975))
SDI.quant <- quantile(ssm.data$time_data$SDIscaled, na.rm =TRUE, c(0.025, 0.5, 0.975))

# get a grid with median values
median.matrix <- data.frame(MAP = rep(MAP.quant[2], 100), 
                            MAT= rep(MAT.quant[2], 100), 
                            DIA= rep(DIA.quant[2], 100), 
                            PPT= rep(ppt.quant[2], 100),
                            TMAX= rep(tmax.quant[2], 100),
                            SDI= rep(SDI.quant[2], 100))

# get a grid with 100 values across the range of data:
seq.matrix <- data.frame(MAP = seq(from = MAP.quant[1], to = MAP.quant[3], length.out = 100), 
           MAT = seq(from = MAT.quant[1], to = MAT.quant[3], length.out = 100), 
           DIA = seq(from = DIA.quant[1], to = DIA.quant[3], length.out = 100), 
           PPT = seq(from = ppt.quant[1], to = ppt.quant[3], length.out = 100), 
           TMAX = seq(from = tmax.quant[1], to = tmax.quant[3], length.out = 100), 
           SDI = seq(from = SDI.quant[1], to = SDI.quant[3], length.out = 100))


# now create a function where we say what values to change and hold constant and it predicts for an average tree/year:
vary.by <- "TMAX"

calculate_tree_growth<- function(covariates, betas.all){
  tree.growth <- #betas.all$alpha #+ beta_YEARid +# sampled from tree level alpha randome effect
  # normal fixed effects
  betas.all$bMAP*covariates$MAP + 
    betas.all$bMAT*covariates$MAT +
    
    # size and SDI fixed
    betas.all$bSDI*covariates$SDI + 
    betas.all$bX*covariates$DIA + 
    
    # climate fixed effects
    betas.all$bppt*covariates$PPT + 
    betas.all$btmax*covariates$TMAX + 
    
    # MAP interactions
    betas.all$bMAP_MAT*covariates$MAP*covariates$MAT +
    #betas.all$bMAP_SDI*covariates$MAP*covariates$SDI +
    
    betas.all$bMAP_tmax*covariates$MAP*covariates$TMAX +
    betas.all$bMAP_ppt*covariates$MAP*covariates$PPT +
    
    # MAT interactions
    #betas.all$bMAT_SDI*covariates$MAT*covariates$SDI+
    betas.all$bMAT_tmax*covariates$MAT*covariates$TMAX +
    
    betas.all$bMAT_ppt*covariates$MAT*covariates$PPT +
    
    
    # tmax and precip interactions
    betas.all$btmax_ppt*covariates$TMAX*covariates$PPT +
    
    # SDI interactions
    betas.all$bSDI_tmax*covariates$SDI*covariates$TMAX +
    betas.all$bSDI_ppt*covariates$SDI*covariates$PPT + 
    
    # X interactions
    betas.all$bX_MAP*covariates$MAP*covariates$DIA + 
    betas.all$bX_MAT*covariates$MAT*covariates$DIA + 
    betas.all$bX_ppt*covariates$PPT*covariates$DIA + 
    betas.all$bX_tmax*covariates$TMAX*covariates$DIA + 
    betas.all$bX_SDI*covariates$SDI*covariates$DIA 
  
  #tree.growth <-  ifelse(tree.growth < 0, 0, tree.growth) # we shouldn't need this but keeping in
  #tree.growth <-  exp(tree.growth) 
  treegrowth <- rlnorm(n = 10000, tree.growth, median(sigma_INC))
  
  treegrowth  <-  ifelse( treegrowth  < 0.001, 0,  treegrowth ) # Assign tree growth to 0 if its below measurable grwoth
  treegrowth  <-  ifelse( treegrowth  >= 2, 2,  treegrowth ) # we shouldn't need this but keeping in
  
  treegrowth
}

plot.simple.effect <- function(vary.by, seqs, meds){
    seqs <- seq.matrix %>% dplyr::select(vary.by)
    meds <- median.matrix %>% dplyr::select(!vary.by)
    covariates.5 <- cbind(seqs, meds)
    
    
    
    range.preds <- lapply(1:nrow(covariates.5), function(i){calculate_tree_growth(covariates = covariates.5[i,], betas.all = betas.all)})
    
    ranges.preds.df <- do.call(rbind, range.preds)
    cov.preds.long <- cbind(covariates.5, ranges.preds.df)
    cov.preds.long.m <- reshape2::melt(cov.preds.long, id.vars = colnames(covariates.5))
    
    pred.summary <- cov.preds.long.m %>% group_by(UQ(sym(vary.by))) %>% summarise(ci.lo = quantile(value, 0.025), 
                                                                       ci.hi = quantile(value, 0.975), 
                                                                       med = quantile(value, 0.5))
    
    
    effect.plot <- ggplot()+geom_ribbon(data = pred.summary, aes(x = .data[[vary.by]], ymin = ci.lo, ymax = ci.hi,fill = vary.by))+
      geom_line(data = pred.summary, aes(x = .data[[vary.by]], y = med))+
      scale_fill_manual(values =c("SDI"="#1b9e77","TMAX"= "#d95f02", "PPT"="#7570b3", "DIA" = "darkgrey", "MAP" = "darkblue", "MAT" = "darkred"))+theme(legend.position = "none")
      #+ylim(0,0.25)
    
    effect.plot
}

theme.gg <- theme_bw(base_size = 14) + theme(panel.grid = element_blank(), legend.position = "none")
tmax.eff <- plot.simple.effect(vary.by = "TMAX") + theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)
SDI.eff <- plot.simple.effect(vary.by = "SDI")+ theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)
DIA.eff <- plot.simple.effect(vary.by = "DIA")+ theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)
ppt.eff <- plot.simple.effect(vary.by = "PPT")+ theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)
MAP.eff <- plot.simple.effect(vary.by = "MAP")+ theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)
MAT.eff <- plot.simple.effect(vary.by = "MAT")+ theme.gg + ylab("Tree Growth (mm)")+ylim(0, 1.45)


png(height =5, width =6, units = "in", res = 300, "outputs/Main_effects_ssm_full.png")
cowplot::plot_grid(DIA.eff, MAT.eff, MAP.eff, SDI.eff, tmax.eff, ppt.eff, 
                   ncol = 3, align = "hv")
dev.off()

# combine with a map of observations and with the violin plots for figure 2:
# code for this is in regional.map.R
source("R/regional_map.R")
US.spp.map


# read in posterior estimates from the final model:
# Make a violin plot of the fixed effects. 
colnames(betas.all) <- c("alpha", "betaMAP", "betaMAT", "betaMAP_MAT", "betaSDI",
                         "betaSDI_Precip", "betaSDI_Tmax",  "betaMAP_Precip", "betaMAP_Tmax", 
                         "betaMAT_Precip", "betaMAT_Tmax", "betaX", "betaPrecip", "betaTmax", "betaTmax_Precip", 
                         "betaX_MAP","betaX_MAT", "betaX_Tmax",
                         "betaX_Precip", "betaX_SDI")

covariates.in.violin = c("betaX", "betaTmax", "betaPrecip", "betaSDI", "betaMAP", "betaMAT", 
               "betaMAP_Precip","betaMAT_Precip","betaTmax_Precip",  "betaSDI_Precip",
               "betaMAP_Tmax", "betaMAT_Tmax","betaSDI_Tmax",
               "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
               "betaX_MAP", "betaX_MAT")
cov.estimates <- betas.all %>% dplyr::select(all_of(covariates.in.violin))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
violin.plot.posteriors <- ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
violin.plot.posteriors
ggsave(paste0("outputs/covariate_violin_plots_model_6.png"))

# save as one big figure:
png(height = 8, width = 8, units = "in", res = 300, "outputs/map_parameter_ests_marginal_climate.png")
plot_grid(
plot_grid(US.spp.map, violin.plot.posteriors, align = "h", labels = c("A", "B")),
plot_grid(SDI.eff+xlab("Relative SDI"), tmax.eff+xlab("Relative max Temp."), ppt.eff + xlab("Relative Precip."), align = "hv", cols = 3, labels =c("C", "D", "E")),
cols = 1, rel_heights = c(1,0.5))
dev.off()

############################################################################
# extra code, not used
# 
# 
# # next steps would be to make these on the units of the variables
# 
# interaction.term <- "TMAX"
# 
# 
# 
# # make some interaction plots:
# plot.interaction.effect <- function(vary.by, interaction.term , seqs, meds){
#   seqs <- seq.matrix %>% dplyr::select(vary.by)
#   interaction.seqs <- seq.matrix %>% dplyr::select(interaction.term)
#   meds <- median.matrix %>% dplyr::select(!vary.by & !interaction.term)
#   meds.full<- rbind(meds, meds, meds) # need to make it 300 --100 for each interaction line
#   covariates.5 <- cbind(seqs, meds)
#   
#   interaction.lo.mid.hi <- as.matrix(t(data.frame(low = quantile(interaction.seqs[,1], 0.025), 
#                                       mid = quantile(interaction.seqs[,1], 0.5), 
#                                       high = quantile(interaction.seqs[,1], 0.975))))
#   #covariates.5
#   biggrid <- expand.grid(seqs[,1], interaction.lo.mid.hi)
#   colnames(biggrid) <- c(vary.by, interaction.term)
#   #nrow(biggrid)
#   covariates.all <- cbind(biggrid, meds)
#   
#   range.preds <- lapply(1:nrow(covariates.all), function(i){calculate_tree_growth(covariates = covariates.all[i,], betas.all = betas.all)})
#   
#   ranges.preds.df <- do.call(rbind, range.preds)
#   cov.preds.long <- cbind(covariates.all, ranges.preds.df)
#   cov.preds.long.m <- reshape2::melt(cov.preds.long, id.vars = colnames(covariates.all))
#   
#   pred.summary <- cov.preds.long.m %>% group_by(UQ(sym(vary.by)), UQ(sym(interaction.term))) %>% summarise(ci.lo = quantile(value, 0.025), 
#                                                                                 ci.hi = quantile(value, 0.975), 
#                                                                                 med = quantile(value, 0.5))
#   
#   
#   df.labels <- data.frame(TMAX = unique(pred.summary[,interaction.term]), 
#              quantile = c("low", "medium", "high"))
#   colnames(df.labels)[1]<- interaction.term
#   pred.summary.interaction <- left_join(pred.summary, df.labels)
#   
#   effect.plot <- ggplot()+geom_ribbon(data = pred.summary.interaction %>% filter(quantile %in% "low"), aes(x = .data[[vary.by]], ymin = ci.lo, ymax = ci.hi,fill = "low"), alpha = 0.75)+
#     geom_line(data = pred.summary.interaction %>% filter(quantile %in% "low"), aes(x = .data[[vary.by]], y = med))+
#     
#     geom_ribbon(data = pred.summary.interaction %>% filter(quantile %in% "medium"), aes(x = .data[[vary.by]], ymin = ci.lo, ymax = ci.hi,fill = "mid"), alpha = 0.75)+
#     geom_line(data = pred.summary.interaction %>% filter(quantile %in% "medium"), aes(x = .data[[vary.by]], y = med))+
#     
#     
#     geom_ribbon(data = pred.summary.interaction %>% filter(quantile %in% "high"), aes(x = .data[[vary.by]], ymin = ci.lo, ymax = ci.hi,fill = "high"), alpha = 0.75)+
#     geom_line(data = pred.summary.interaction %>% filter(quantile %in% "high"), aes(x = .data[[vary.by]], y = med))+
#     
#     scale_fill_manual(name = interaction.term, values =c("low"="#0571b0","mid"= "#fecc5c", "high"="#ca0020"), 
#                       breaks = c("low", "mid", "high")) + ylab("Tree Growth (mm)")+theme_bw(base_size = 14)#+theme(legend.position = "none")
#  
#   
#   effect.plot
# }
# 
# # new theme.gg.legend
# theme.gg.legend <- theme_bw(base_size = 14) + theme( panel.grid = element_blank())
# 
# # climate anomolies x SDI interactions
# l<- plot.interaction.effect(vary.by = "TMAX", interaction.term = "SDI")+ theme.gg.legend +ylim(0, 1.9)
# m <- plot.interaction.effect(vary.by = "PPT", interaction.term = "SDI")+ theme.gg.legend +ylim(0, 1.9)
# 
# # climate normals x SDI interactions
# # these two are not in the model
# a <- plot.interaction.effect(vary.by = "SDI", interaction.term = "MAP")+ theme.gg.legend +ylim(0, 1.9)
# b<- plot.interaction.effect(vary.by = "SDI", interaction.term = "MAT")+ theme.gg.legend +ylim(0, 1.9)
# 
# 
# # climate normals x climate anomalies
# plot.interaction.effect(vary.by = "MAP", interaction.term = "PPT")+ theme.gg.legend +ylim(0, 1.9)
# plot.interaction.effect(vary.by = "MAT", interaction.term = "PPT")+ theme.gg.legend +ylim(0, 1.9)
# plot.interaction.effect(vary.by = "MAT", interaction.term = "TMAX")+ theme.gg.legend +ylim(0, 1.9)
# plot.interaction.effect(vary.by = "MAP", interaction.term = "TMAX")+ theme.gg.legend +ylim(0, 1.9)
# 
# # same as the 4 above but changing x axis variable
# c <- plot.interaction.effect(vary.by = "TMAX", interaction.term = "MAP")+ theme.gg.legend +ylim(0, 1.9)
# d <- plot.interaction.effect(vary.by = "TMAX", interaction.term = "MAT")+ theme.gg.legend +ylim(0, 1.9)
# e <-plot.interaction.effect(vary.by = "PPT", interaction.term = "MAP")+ theme.gg.legend +ylim(0, 1.9)
# f <-plot.interaction.effect(vary.by = "PPT", interaction.term = "MAT")+ theme.gg.legend +ylim(0, 1.9)
# 
# # climate anomalies x diameter
# g <- plot.interaction.effect(vary.by = "PPT", interaction.term = "DIA")+ theme.gg.legend +ylim(0, 1.9)
# h <- plot.interaction.effect(vary.by = "TMAX", interaction.term = "DIA")+ theme.gg.legend +ylim(0, 1.9)
# 
# #SDI x diameter
# i<- plot.interaction.effect(vary.by = "SDI", interaction.term = "DIA")+ theme.gg.legend +ylim(0, 1.9)
# 
# # climate normals x diamter
# j <- plot.interaction.effect(vary.by = "MAP", interaction.term = "DIA")+ theme.gg.legend +ylim(0, 1.9)
# k <- plot.interaction.effect(vary.by = "MAT", interaction.term = "DIA")+ theme.gg.legend +ylim(0, 1.9)
# 
# 
# # plot up 12 plot interaction giant plot!
# 
# png(height =12, width =14, units = "in", res = 300, "outputs/Interaction_effects_ssm_full.png")
# cowplot::plot_grid(a, b, c, d, e, f, j, h, i, j, k, l, m,
#                    ncol = 3, align = "hv")
# dev.off()
