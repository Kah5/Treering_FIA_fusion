library(rstan)
library(MASS)
library(tidyverse)
options(mc.cores = parallel::detectCores())
# revised model
data <- readRDS("data/regional_pipo_jags_formatted_data.RDS")
data$tau_y_ic = 1/10

# just use 1966-2001
data$z <- data$z[,1:36]
data$y <- data$y[,1:36]
colnames(data$y) <- 1966:2001
colnames(data$z) <- 1966:2001


data$x0 <- data$z

#----------------------------------------------------------
# Randomly remove some of the tree ring data for validation
#----------------------------------------------------------
set.seed(22)

y.long <- reshape2::melt(data$y)
colnames(y.long) <- c("treeid", "year", "inc")
# does the conversion to diameter increment 
y.long <- y.long %>% mutate(diainc = (inc)*2) %>% filter(!is.na(diainc))

y.long$diainc
summary(y.long$inc)
summary(y.long$diainc)
# separate y into training and testing data:
N_train <- nrow(y.long)*0.80
N_test <- nrow(y.long)*0.20
train_ind <- sample(c(1:nrow(y.long)), size = N_train, replace = FALSE)

train_y <- y.long[train_ind,]
test_y <- y.long[-train_ind,]

summary(train_y)
summary(test_y)
summary(y.long)


# set up z diameter data frame in long format and leave out some diameter remeasuremetns for validation:
datz <-data$z[,1:36]
row.names(data$z) <- 1:1046 # 1 to # of trees
z.long <- reshape2::melt(data$z) 

colnames(z.long) <- c("tree.id", "year", "DIA")
summary(z.long)

time.df <- data.frame(year = 1966:2010, 
                      time = 1:length(1966:2010))
z.long <- z.long %>% filter(!is.na(z.long$DIA))

z.repeated <- z.long %>% filter(!is.na(DIA)) %>% group_by(tree.id) %>% summarise(n = n()) %>% filter(n > 1)

# delete
#z.train <- z.long %>% filter(!tree.id %in% test_z.tre$tree.id & !year > 2001)
#z.test <- z.long %>% filter(tree.id %in% test_z.tre$tree.id & year > 2001)


#z.train <- left_join(z.train, time.df)
#z.test <- left_join(z.test, time.df)
z.long <- left_join(z.long, time.df)
train_y <- left_join(train_y, time.df)
test_y <- left_join(test_y, time.df)


# if SDI is NA for the first year, assign the next year's SDI
data$SDIscaled[is.na(data$SDIscaled[,1]), ] <- data$SDIscaled[is.na(data$SDIscaled[,1]),2]



# Set up data for STAN model
mod.data <- list(Nrow = length(unique(y.long$treeid)),
                 Ncol = length(unique(y.long$year)),
                 #Ncolpred = length(37:45),
                 
                 # data and indices for increment
                 y = train_y$diainc, 
                 treeinc = train_y$treeid, 
                 yearinc = train_y$time,
                 Ninc = length(train_y$diainc),
                 
                 
                 # data and indices for the complete diameter data
                 z = z.long$DIA, # diameter data 
                 treedia = z.long$tree.id, # index for the tree
                 yeardia = z.long$time, # index for the year
                 Ndia = length(z.long$DIA), 
                 
                 
                 tmaxAprMayJunscaled = time_data$tmaxAprMayJunscaled[,1:36], 
                 wateryrscaled = time_data$wateryrscaled[,1:36], 
                 MAP = data$MAP, 
                 MAT = data$MAT, 
                 SDI = time_data$SDIscaled[,1:36])

saveRDS(mod.data, "data/mod.data.object.rds")
summary(mod.data$SDI)
mod.data$SDI

#---------------------------------------------------------------
# Run all versions of the model, sequentially adding in effects
#---------------------------------------------------------------
# Model 0
model.name <- "model_0"
# record the total time it takes to run
# fix the model folder in all of these models
start.time = Sys.time()
model.0 <- stan(file = "modelcode/model_0_no_missingdata_vectorized.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#list(adapt_delta = 0.99, stepsize = 0.5, max_treedepth = 15),#, stepsize = 0.01, max_treedepth = 15),
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "alpha_TREE", 
                         "sigma_TREE",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "x", "inc"))
stop.time <- Sys.time()
mod.0.time <- stop.time - start.time

# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.0),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.0),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 

par.names <- c("mutree",  "sigma_inc",
               "sigma_add",
               "sigma_dbh", "alpha_TREE[1]", "alpha_TREE[2]")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, "outputs/traceplot_model0.png")
traceplot (model.0, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.0, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.0) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")

#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
# covariates.m <- reshape2::melt(cov.estimates)
# head(covariates.m)

# # make violin plots of the samples to look at the medians & the uncertainty around each covariate:
# ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()
# ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))
# 
saveRDS(model.0, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.0) # remove from memory


# run model 1: TREE Size effect
model.name <- "model_1"
start.time <- Sys.time()
model.1 <- stan(file = "model_1_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX",
                         "alpha_TREE", "sigma_TREE",
                         
                         "x", "inc"))
stop.time <- Sys.time()
mod.1.scale.vec.time <- stop.time - start.time
mod.1.scale.vec.time

par.names <- c("mutree",  "sigma_inc",
               "sigma_add",
               "sigma_dbh","betaX","alpha_TREE[1]", "alpha_TREE[2]")
# make and save traceplots


# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.1, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.1, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.1) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")

#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))



# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.1),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.1),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.1, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.1) # remove from memory



# run model 2: TREE Size effect + Tmax
model.name <- "model_2"
start.time <- Sys.time()
model.2 <- stan(file = "model_2_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX", "betaTmax", 
                         "alpha_TREE", "sigma_TREE",
                         
                         
                         "x", "inc"))
stop.time <- Sys.time()
mod.2.time <- stop.time - start.time
mod.2.time

par.names <- c("mutree",  "sigma_inc",
               "sigma_add",
               "sigma_dbh","betaX","betaTmax", "alpha_TREE[1]", "alpha_TREE[2]")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.2, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.2, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.2) 
# create parameter plots to visualize the values of the effects:
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX","betaTmax")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))



# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.2),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.2),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.2, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, fit.ssm.df, model.2) # remove from memory




# run model 3: TREE Size effect + Tmax + Precip
model.name <- "model_3"
start.time <- Sys.time()
model.3 <- stan(file = "model_3_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX", "betaTmax", "betaPrecip",
                         "alpha_TREE", "sigma_TREE", 
                         
                         "x", "inc"))
stop.time <- Sys.time()
mod.3.time <- stop.time - start.time
mod.3.time

par.names <- c("mutree",  "sigma_inc",
               "sigma_add",
               "sigma_dbh","betaX","betaTmax", "betaPrecip","alpha_TREE[1]", "alpha_TREE[2]")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.3, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.3, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.3) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")

#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX","betaTmax", "betaPrecip")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))



# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.3),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.3),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.3, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.3) # remove from memory


# run model 4: TREE Size effect + Tmax + Precip + SDI
model.name <- "model_4"
start.time <- Sys.time()
model.4 <- stan(file = "model_4_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX", "betaTmax", "betaPrecip","betaSDI",
                         "alpha_TREE", "sigma_TREE",
                         "x", "inc"))
stop.time <- Sys.time()
mod.4.time <- stop.time - start.time
mod.4.time


par.names <- c("mutree",  "sigma_inc",
               "sigma_add",
               "sigma_dbh","betaX","betaTmax", "betaPrecip","betaSDI", "alpha_TREE[1]", "alpha_TREE[2]")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.4, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.4, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.4) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")

#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX","betaTmax", "betaPrecip", "betaSDI")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))


# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.4),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.4),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.4, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.4) # remove from memory



# run model 5: TREE Size effect + Tmax + Precip + SDI
model.name <- "model_5_short"
start.time <- Sys.time()
model.5 <- stan(file = "model_5_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX", "betaTmax", "betaPrecip","betaSDI",
                         "betaMAP", "betaMAT",
                         "alpha_TREE", "sigma_TREE",
                         
                         "x", "inc"))
stop.time <- Sys.time()
mod.5.time <- stop.time - start.time
mod.5.time




par.names <- c("mutree",  "sigma_inc", 
               "sigma_add", 
               "sigma_dbh", "betaX", "betaTmax", "betaPrecip", "betaSDI", "betaMAP", "betaMAT", "alpha_TREE[1]", "alpha_TREE[2]")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.5, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.5, pars = par.names)
dev.off()

# make beta parameter dotplots
fit_ssm_df <- as.data.frame(model.5) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")

#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX","betaTmax", "betaPrecip", "betaSDI", "betaMAT", "betaMAP")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))



# do predicted vs observed:
x.pred <- dplyr::select(as.data.frame(model.5),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(model.5),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.5, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.5) # remove from memory


# run model 6: TREE Size effect + Tmax + Precip + SDI
model.name <- "model_6"
start.time <- Sys.time()
model.6 <- stan(file = "model_6_nomissing_xscaled.stan", 
                data = mod.data,
                iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mutree",
                         "sigma_inc", 
                         "sigma_add", "sigma_dbh", 
                         "betaX", "betaTmax", "betaPrecip","betaSDI",
                         "betaMAP", "betaMAT",
                         "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
                         "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
                         "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
                         "betaX_MAP", "betaX_MAT",
                         "alpha_TREE", "sigma_TREE",
                         
                         "x", "inc"))
stop.time <- Sys.time()
mod.6.time <- stop.time - start.time
mod.6.time


#model.6 <-readRDS("outputs/SSM_posteriors/model_6STANfit.RDS")

par.names <- c("mutree",  "sigma_inc", 
               "sigma_add", 
               "sigma_dbh", "sigma_TREE","betaX", "betaTmax", "betaPrecip", "betaSDI", "betaMAP", "betaMAT", 
               "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
               "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
               "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
               "betaX_MAP", "betaX_MAT")

# make and save traceplots
png(height = 2*length(par.names), width = 12, units = "in", res = 200, paste0("outputs/traceplot_",model.name,".png"))
traceplot (model.6, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE)
dev.off()

png(height = 12, width = 12, units = "in", res = 200, paste0("outputs/pairsplots_", model.name, ".png"))
pairs(model.6, pars = par.names)
dev.off()

# get post-warmup draws (will make working with this a bit faster)
#model.6 %>% stan_names()
#model.6 %>% stan_select(mutree)
# 1500 post warmup draws
param.summary <- model.6 %>% stan_slice(1:1500,inc_warmup = FALSE) %>% 
  stan_select(!!!rlang::syms(par.names))
params.post.warmup <- as.data.frame(param.summary)
params.post.warmup.m <- reshape2::melt(params.post.warmup)
colnames(params.post.warmup.m) <- c("L1","value")
saveRDS(params.post.warmup.m, "params_post_warmup_model6.rds")
# make beta parameter dotplots
fit_ssm_df <- params.post.warmup#as.data.frame(model.6) 
# create parameter plots to visualize the values of the effects:
#covariates = c( "betaX")
alpha.summary <- as.data.frame(model.6 %>% stan_slice(1:1500,inc_warmup = FALSE) %>% 
                                 stan_select(!!!rlang::syms(sprintf('alpha_TREE[%s]',1:1046))))
#cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)
sigma <- fit_ssm_df[,c("sigma_add", "sigma_dbh", "sigma_inc")] # get additive process error
sigma.m <- reshape2::melt(sigma)
sigma.quant <- sigma.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                            ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                            ci.hi = quantile(value, 0.975, na.rm =TRUE),
                                                            sd = sd(value))
saveRDS(sigma.quant, "model6.1500.sigmas.rds")

# Tree-level random effects 
#alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))


# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha.summary)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE),
                                                                sd = sd(value))

saveRDS(tree.quant, "model6.1500.alpha_TREES.rds")
ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(paste0("outputs/tree_random_effects_", model.name, ".png"))


# Make a violin plot of the fixed effects. 
covariates = c("betaX", "betaTmax", "betaPrecip", "betaSDI", "betaMAP", "betaMAT", 
               "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
               "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
               "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
               "betaX_MAP", "betaX_MAT", "mutree", "sigma_TREE")
cov.estimates <- fit_ssm_df %>% dplyr::select(all_of(covariates))
covariates.m <- reshape2::melt(cov.estimates)
head(covariates.m)

# make violin plots of the samples to look at the medians & the uncertainty around each covariate:
ggplot(covariates.m, aes(x = variable, y = value))+geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), fill = "grey")+theme_bw()+
  ylab("Parameter Estimate") +xlab("Parameter")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("outputs/covariate_violin_plots_", model.name, ".png"))

covariate.summaries <- covariates.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5),
                                                                         ci.lo = quantile(value, 0.025), 
                                                                         ci.hi = quantile(value, 0.975), 
                                                                         sd = sd(value))
saveRDS(covariate.summaries, "model6.1500.betas.rds")
# do predicted vs observed:
x.pred <- as.data.frame(model.6 %>% stan_slice(1:1500,inc_warmup = FALSE) %>% 
                          stan_select( stan_contains('x')))
inc.pred <-  as.data.frame(model.6 %>% stan_slice(1:1500,inc_warmup = FALSE) %>% 
                             stan_select( stan_contains('inc')))
#x.pred <- dplyr::select(as.data.frame(model.6),"x[1,1]":"x[1046,36]")
#inc.pred <- dplyr::select(as.data.frame(model.6),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots

source("plot_held_out_regional_STANfit.R") 
saveRDS(model.6, paste0("outputs/", model.name, "STANfit.RDS"))
rm(fit_ssm_df, model.6) # remove from memory


#################
# read in all model output files an make a file with the predicted in and out of sample MSPE & adjusted r-sq
#################
oos.list <- list()
oos.inc.files <- paste0("outputs/model_",1:6, "pred.obs.out.of.sample.inc.rds" )
oos.list <- lapply(oos.inc.files, readRDS)

for(i in 1:length(oos.list)){
  oos.list[[i]]$model.name  <- paste0("model_", i)
}

oos.df <- do.call(rbind, oos.list)

oos.df

oos.summary <- oos.df %>% group_by(model.name)  %>% summarise(MSPE = mean((diainc-mean.ci)^2, na.rm =TRUE),
                                                              RMSPE = sqrt(mean((diainc-mean.ci)^2, na.rm =TRUE)),
                                                              MAPE = mean(abs(diainc-mean.ci), na.rm = TRUE), 
                                                              PPL = sum((diainc - mean.ci)^2) + sum(predvar)) #,
# V1 = mean(diainc-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
# V2 = (mean((diainc-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
# V3 = (mean((diainc-mean.ci)^2)^(1/2)))#, # goodness of fit estimate (small = better fit)
# #PPL = sum((diainc - mean.ci)^2) + sum(predvar)) # posterior predictive loss

oos.summary$validation <- "out-of-sample"

summary.stats <- summary(lm(diainc ~ mean.ci, data =oos.df))
all.regress <- oos.df %>% group_by(model.name)%>% do(lm(diainc ~ mean.ci, .)) #%>% ungroup()

summary.stats.list <- rsquared.list <- list()
for(i in 1:length(oos.list)){
  oos.mod <- oos.df %>% filter(model.name %in% paste0("model_", i))
  #lms <- lm(diainc ~ mean.ci, data = oos.mod)
  summary.stats.list[[i]] <- summary(lm(diainc ~ mean.ci, data = oos.mod))
  rsquared.list[[i]] <- data.frame(model.name = paste0("model_", i),
                                   rsquared = summary.stats.list[[i]]$r.squared, 
                                   adj.rsquared = summary.stats.list[[i]]$adj.r.squared)
}

oos.rsquared <- do.call(rbind, rsquared.list)
oos.table <- left_join(oos.summary, oos.rsquared)

write.csv(oos.table, "outputs/out_of_sample_increment_validation.csv", row.names = FALSE)


# do the same for in sample increments

ins.list <- list()
ins.inc.files <- paste0("outputs/model_",1:6, "pred.obs.within.sample.dbh.rds" )
ins.list <- lapply(ins.inc.files, readRDS)

for(i in 1:length(ins.list)){
  ins.list[[i]]$model.name  <- paste0("model_", i)
}

ins.df <- do.call(rbind, ins.list)

ins.df

ins.summary <- ins.df %>% group_by(model.name)  %>% summarise(MSPE = mean((diainc-mean.ci)^2, na.rm =TRUE),
                                                              RMSPE = sqrt(mean((diainc-mean.ci)^2, na.rm =TRUE)),
                                                              MAPE = mean(abs(diainc-mean.ci), na.rm = TRUE), 
                                                              PPL = sum((diainc - mean.ci)^2) + sum(predvar)) #,
# V1 = mean(diainc-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
# V2 = (mean((diainc-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
# V3 = (mean((diainc-mean.ci)^2)^(1/2)))#, # goodness of fit estimate (small = better fit)
# #PPL = sum((diainc - mean.ci)^2) + sum(predvar)) # posterior predictive loss

ins.summary$validation <- "out-of-sample"

summary.stats <- summary(lm(diainc ~ mean.ci, data =ins.df))
all.regress <- ins.df %>% group_by(model.name)%>% do(lm(diainc ~ mean.ci, .)) #%>% ungroup()

summary.stats.list <- rsquared.list <- list()
for(i in 1:length(ins.list)){
  ins.mod <- ins.df %>% filter(model.name %in% paste0("model_", i))
  #lms <- lm(diainc ~ mean.ci, data = ins.mod)
  summary.stats.list[[i]] <- summary(lm(diainc ~ mean.ci, data = ins.mod))
  rsquared.list[[i]] <- data.frame(model.name = paste0("model_", i),
                                   rsquared = summary.stats.list[[i]]$r.squared, 
                                   adj.rsquared = summary.stats.list[[i]]$adj.r.squared)
}

ins.rsquared <- do.call(rbind, rsquared.list)
ins.table <- left_join(ins.summary, ins.rsquared)

write.csv(ins.table, "outputs/out_of_sample_increment_validation.csv", row.names = FALSE)









