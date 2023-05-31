library(rstan)
library(MASS)
options(mc.cores = parallel::detectCores())
# revised model
data <- readRDS("jags.data.formatted.rds")
data$tau_y_ic = 1/10

# only one chain for testing
init <- readRDS("jags.init.formatted.rds")[[1]]
class(init)
data$z <- data$z[,1:36]
colnames(data$y) <- 1966:2001
colnames(data$z) <- 1966:2001
#colnames(time_data$wateryr.scaled) <- 1966:2001
#colnames(time_data$tmax.AprMayJun.scaled) <- 1966:2001

data$x0 <- data$z
dat <- data$y


# Randomly remove some data
#nmiss <- 115
#miss <- sample.int(prod(dim(dat)), size = nmiss)
#dat[miss] <- NA

# Extract the missing values into a VECTOR
dat_complete <- dat[!is.na(dat)]

# Extract the missing and present values as MATRICES
ind_pres <- which(!is.na(dat), arr.ind = TRUE)
ind_miss <- which(is.na(dat), arr.ind = TRUE)


# get missing z data:

datz <-data$z[,1:36]


# Randomly remove some data
#nmiss <- 115
#miss <- sample.int(prod(dim(dat)), size = nmiss)
#dat[miss] <- NA

# Extract the missing values into a VECTOR
dat_completez <- datz[!is.na(datz)]

# Extract the missing and present values as MATRICES
ind_presz <- which(!is.na(datz), arr.ind = TRUE)
ind_missz <- which(is.na(datz), arr.ind = TRUE)



data$SDInontv <- rowMeans(data$SDIscaled, na.rm = TRUE)

# there are two NA values in 1966 --find a better way to replace
data$SDIscaled[49,1] <- data$SDIscaled[49,2]
data$SDIscaled[86,1] <- data$SDIscaled[86,2]

mod.data <- list(Nrow = nrow(dat),
                 Ncol = ncol(dat),
                 Ncomp = length(dat_complete),
                 Nmiss = sum(is.na(dat)),
                 dat_complete = dat_complete,
                 ind_pres = ind_pres,
                 ind_miss = ind_miss , 
                 
                 Nrow_z = nrow(datz),
                 Ncol_z = ncol(datz),
                 Ncomp_z = length(dat_completez),
                 Nmiss_z = sum(is.na(datz)),
                 dat_completez = dat_completez,
                 ind_presz = ind_presz,
                 ind_missz = ind_missz, 
                 tmaxAprMayJunscaled = data$tmaxAprMayJunscaled, 
                 wateryrscaled = data$wateryrscaled, 
                 MAP = data$MAP, 
                 MAT = data$MAT, 
                 SDI = data$SDIscaled)

initfun <- function(...) {list(x=runif(1,5,35))}#, sigma2=runif(1,0.01,0.05))}
initfun(1)

model.name <- "RE.MAP.MAT.X.timevaryingclimate.SDI.all.interactions.REX"


# fit a bunch of models:
#setwd("model_simple_run/")
# null model:
fit.5 <- stan(file = 'model_5.stan' , 
              data = mod.data,
              iter = 6000, 
              chains = 3, 
              verbose=FALSE, 
              control =  list(max_treedepth = 15),#list(adapt_delta = 0.99, stepsize = 0.5, max_treedepth = 15),#, stepsize = 0.01, max_treedepth = 15),
              sample_file = model.name, 
              #adapt_delta = 0.99, 
              pars = c("mu", "sigma_inc", "sigma_add", "sigma_dbh","beta_YEAR", "alpha_TREE", 
                       "betaMAP", "betaMAT","betaX", "betaX_TREE","sigmaX_TREE","betaTmax", "betaPrecip","betaSDI",
                       "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
                       "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
                       "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
                       "betaX_MAP", "betaX_MAT",
                       "x", "inc")) # , init = initfun)

# note that chain 3 didnt converge/or even start sampling after 24 hours. Chains 1 & 2 were done
csvfiles <- here::here("model_simple_run", paste0(model.name, "_", 1:2, ".csv"))

#if (all(file.exists(csvfiles))) {
fit.5 <- read_stan_csv(csvfiles, col_major = TRUE) 

saveRDS(fit.5, here(paste0("small_model_fits/", model.name, "2chains.RDS")))
# Warning messages:
#   1: There were 7 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: There were 2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 3: Examine the pairs() plot to diagnose sampling problems
# 
# 4: The largest R-hat is 1.97, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 
posterior <- as.array(fit.5 )

par.names = c("mu", "sigma_inc", 
              "sigma_add", 
              "sigma_dbh", #)#, #)
              #"sigma_TREE", 
              "sigmaX_TREE", 
              "alpha_TREE[1]", "beta_YEAR[1]","betaX_TREE[1]",
              "betaX",
              #"betaTmax", "betaPrecip", 
              "betaMAP", "betaMAT",
              "betaTmax", "betaPrecip", 
              "betaSDI", 
              "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
              "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
              "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
              "betaX_MAP", "betaX_MAT") #,
#"betaPrecip_MAP","betaPrecip_MAT",
#"betaMAT", "betaSDI", "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
#"betaTmax_MAP", "betaTmax_MAT", 
#"betaX_Precip", "betaX_Tmax", "betaX_SDI")
#color_scheme_set("mix-blue-red")

png(height = 16, width = 7, units = "in", res = 100, paste0("output/traceplots_tau_", model.name, "default.png"))
#par(mfrow = c(5, 3))
traceplot (fit.5, pars = par.names, nrow = 8, ncol = 4, inc_warmup = FALSE) 
dev.off()

pairs(fit.5, pars = c("mu", "sigma_inc", 
                      "sigma_add", 
                      "sigma_TREE", 
                      "sigmaX_TREE",
                      "sigma_dbh", 
                      "betaMAP", "betaMAT", "betaTmax", "betaPrecip","betaSDI"))


x.pred <- dplyr::select(as.data.frame(fit.5),"x[1,1]":"x[100,36]")
inc.pred <- dplyr::select(as.data.frame(fit.5),"inc[1,1]":"inc[100,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots
#model.out <- fit.betaX.tmax.precip
source("plot_held_out_regional_STANfit.R") # run script to make predicted vs obs plots


# get convergence statistics & save
fit_ssm_df <- as.data.frame(fit.5) # takes awhile to convert to df
Rhats <- apply(fit_ssm_df, 2, Rhat)
hist(Rhats)
ESS_bulks <- apply(fit_ssm_df, 2, ess_bulk)
hist(ESS_bulks)
ESS_tails <- apply(fit_ssm_df, 2, ess_tail)
hist(ESS_tails)

convergence.stats <- as.data.frame(rbind(Rhats, ESS_bulks, ESS_tails))
convergence.stats$Statistic <- c("Rhat", "ESS_bulk", "ESS_tail")

write.csv(convergence.stats, here("model_simple_run/convergence_stats", paste0(model.name, "_convergence_stats.csv")))


covariates = c("betaX", "betaMAP","betaMAT",  "betaTmax", "betaPrecip","betaSDI", 
               "betaPrecip_MAP","betaPrecip_MAT","betaPrecip_Tmax",  "betaPrecip_SDI",
               "betaTmax_MAP", "betaTmax_MAT","betaTmax_SDI",
               "betaX_Precip", "betaX_Tmax", "betaX_SDI", "betaMAP_MAT",
               "betaX_MAP", "betaX_MAT")

# note for model 0 there are no covariates
cov.estimates <- fit_ssm_df %>% dplyr::select(covariates)

sigma <- fit_ssm_df[,"sigma_add"] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))
betaX_trees <- dplyr::select(fit_ssm_df, "betaX_TREE[1]":paste0("betaX_TREE[", mod.data$Nrow, "]"))

# Year-level random effects 
beta_years <- dplyr::select(fit_ssm_df, "beta_YEAR[1]":paste0("beta_YEAR[", mod.data$Ncol, "]"))



# plot year and tree random effects:
alpha_tree.m <- reshape2::melt(alpha_trees)
tree.quant <- alpha_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot()+geom_point(data = tree.quant, aes(x = variable, y = median))+
  geom_errorbar(data =tree.quant, aes(x = variable, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(here("model_simple_run/output", paste0("tree_random_", model.name, ".png")))


beta_year.m <- reshape2::melt(beta_years )
year.quant <- beta_year.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                               ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                               ci.hi = quantile(value, 0.975, na.rm =TRUE))

year.quant$year <- 1966:2001

ggplot()+geom_point(data = year.quant, aes(x = year, y = median))+
  geom_errorbar(data =year.quant, aes(x = year, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")

ggsave(height = 3, width = 4, units = "in", here("model_simple_run/output", paste0("year_random_", model.name, ".png")))



# for tree size random effects
betax_tree.m <- reshape2::melt(betaX_trees )
betaX.quant <- betax_tree.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5, na.rm =TRUE),
                                                                 ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                 ci.hi = quantile(value, 0.975, na.rm =TRUE))

betaX.quant$tree <- 1:100

ggplot()+geom_point(data = betaX.quant, aes(x = tree, y = median))+
  geom_errorbar(data =betaX.quant, aes(x = tree, ymin = ci.lo, ymax = ci.hi), linewidth = 0.1)+theme_bw()+
  theme(axis.text = element_text(angle= 45, hjust = 1), panel.grid = element_blank())+
  ylab("Estimated effect")+xlab("betaX_TREE")

ggsave(height = 3, width = 4, units = "in", here("model_simple_run/output", paste0("betaX_random_", model.name, ".png")))



# calculate loo:
colnames(x.pred)
# loop over trees to get an increment_mu, with random effects:
# set up an array to do this on
increment_mu <- array(NA, dim = c(mod.data$Nrow, length(alpha_trees[, 1]),ncol = mod.data$Ncol))
for(i in 1:mod.data$Nrow){
  
  for(t in 1:mod.data$Ncol){
    increment_mu[i,,t] <- as.matrix(alpha_trees[, i]) + as.matrix(beta_years[,t]) + 
      cov.estimates$betaMAP*data$MAP[i] + cov.estimates$betaMAT*data$MAT[i] + 
      as.matrix(betaX_trees[,i])*x.pred[,paste0("x[",i,",", t,"]")]+ cov.estimates$betaPrecip*data$wateryrscaled[i,t]+
      cov.estimates$betaTmax*data$tmaxAprMayJunscaled[i,t] + cov.estimates$betaSDI*data$SDIscaled[i,t]+
      cov.estimates$betaPrecip_MAP*data$wateryrscaled[i,t]*data$MAP[i] + 
      cov.estimates$betaPrecip_MAT*data$wateryrscaled[i,t]*data$MAT[i]+
      cov.estimates$betaPrecip_Tmax*data$wateryrscaled[i,t]*data$tmaxAprMayJunscaled[i,t] + 
      cov.estimates$betaPrecip_SDI*data$wateryrscaled[i,t]*data$SDIscaled[i,t]+
      cov.estimates$betaTmax_MAP*data$tmaxAprMayJunscaled[i,t]*data$MAP[i]+
      cov.estimates$betaTmax_MAT*data$tmaxAprMayJunscaled[i,t]*data$MAT[i]+
      cov.estimates$betaTmax_SDI*data$tmaxAprMayJunscaled[i,t]*data$SDIscaled[i,t]+
      cov.estimates$betaX_Precip*data$wateryrscaled[i,t]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaX_Tmax*data$tmaxAprMayJunscaled[i,t]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaX_SDI*data$SDIscaled[i,t]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaX_MAP*data$MAP[i]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaX_MAT*data$MAT[i]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaX_SDI*data$SDIscaled[i,t]*x.pred[,paste0("x[",i,",", t,"]")]+
      cov.estimates$betaMAP_MAT *data$MAP[i]*data$MAT[i]
    # 
    # note that we need to add to this as we add covariates
  }
}

# reorganize so we just have predicted increment_mus for the timepoints with data?
# 
inc.m <- reshape2::melt(increment_mu)
unique(inc.m$Var1) # # trees
increment_mu_spread <- inc.m %>% spread(Var2, value ) %>% select(-Var1, -Var3)

y.m <- reshape2::melt(data$y)
y.m.nona <- y.m[!is.na(y.m$value), ]$value
# now remove all the predictions for increment where y.m is ==NA
increment_mu_spread.nona <- increment_mu_spread[!is.na(y.m$value), ]

# calculate log liklihoood here:
ll <- matrix(0, length(sigma), length(y.m.nona))
for(i in 1:length(sigma)){
  ll[i,] <- dnorm(y.m.nona, increment_mu_spread.nona[,i], sd = sigma[i], log = TRUE)
}
newll <- as.matrix(ll)
r_eff <- relative_eff(exp(ll), chain_id = rep(1:3, each = 2500), cores = 1) # will have to change each for longer iterations
leaveoneout <- loo::loo(as.matrix(ll), r_eff = r_eff, save_psis = TRUE, cores = 1)

save(ll, r_eff, leaveoneout, file = here::here("looresults", "model_5_loo_small.RData"))

