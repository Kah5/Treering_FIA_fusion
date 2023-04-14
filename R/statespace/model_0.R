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

model.name <- "null.REonly"


# fit a bunch of models:

# null model:
fit.0 <- stan(model_code = mod.0.stan , 
              data = mod.data,
              iter = 2000, 
              chains = 3, 
              verbose=FALSE, 
              control =  list(max_treedepth = 15),#list(adapt_delta = 0.99, stepsize = 0.5, max_treedepth = 15),#, stepsize = 0.01, max_treedepth = 15),
              sample_file = model.name, 
              #adapt_delta = 0.99, 
              pars = c("mu", "sigma_inc", "sigma_add", "sigma_dbh","beta_YEAR", "alpha_TREE",
                       "x", "inc")) # , init = initfun)

saveRDS(fit.0, here(paste0("small_model_fits/", model.name, ".RDS")))
fit.0 <- readRDS(here(paste0("small_model_fits/", model.name, ".RDS")))

posterior <- as.array(fit.0 )

par.names = c("mu", "sigma_inc", 
              "sigma_add", 
              "sigma_dbh", #)#, #)
              #"alpha_TREE[1]", "beta_YEAR[1]",
              "betaX",
              "betaTmax", "betaPrecip", "betaMAP",
              "betaPrecip_MAP","betaPrecip_MAT",
              "betaMAT", "betaSDI", "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
              "betaTmax_MAP", "betaTmax_MAT", 
              "betaX_Precip", "betaX_Tmax", "betaX_SDI")
color_scheme_set("mix-blue-red")

png(height = 16, width = 7, units = "in", res = 100, paste0("output/traceplots_tau_", model.name, "default.png"))
#par(mfrow = c(5, 3))
traceplot (fit.betaX.tmax.precip1, pars = par.names, nrow = 8, ncol = 3, inc_warmup = FALSE) 
dev.off()

pairs(fit.betaX.tmax.precip, pars = c("mu", "sigma_inc", 
                                      "sigma_add", 
                                      "sigma_dbh"))


x.pred <- dplyr::select(as.data.frame(fit.0),"x[1,1]":"x[100,36]")
inc.pred <- dplyr::select(as.data.frame(fit.0),"inc[1,1]":"inc[100,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots
#model.out <- fit.betaX.tmax.precip

# calculate loo:
fit_ssm_df <- as.data.frame(fit.0) # takes awhile to convert to df
covariates = c(
  "betaX","betaTmax", "betaPrecip", "betaMAP","betaMAT", "betaSDI", 
  "betaPrecip_MAP","betaPrecip_MAT",
  "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
  "betaTmax_MAP", "betaTmax_MAT", 
  "betaX_Precip", "betaX_Tmax", "betaX_SDI")

# note for model 0 there are no covariates
#ssm_fixed_df %>% dplyr::select(covariates)

sigma <- fit_ssm_df[,"sigma_add"] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(fit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))

# Year-level random effects 
beta_years <- dplyr::select(fit_ssm_df, "beta_YEAR[1]":paste0("beta_YEAR[", mod.data$Ncol, "]"))


# loop over trees to get an increment_mu, with random effects:
# set up an array to do this on
increment_mu <- array(NA, dim = c(mod.data$Nrow, length(alpha_trees[, 1]),ncol = mod.data$Ncol))
for(i in 1:mod.data$Nrow){
  
  for(t in 1:mod.data$Ncol){
    increment_mu[i,,t] <- as.matrix(alpha_trees[, i]) + as.matrix(beta_years[,t]) # note that we need to add to this as we add covariates
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
r_eff <- relative_eff(exp(ll), chain_id = rep(1:3, each = 1000), cores = 1) # will have to change each for longer iterations
leaveoneout <- loo::loo(as.matrix(ll), r_eff = r_eff, save_psis = TRUE, cores = 1)

save(ll, r_eff, leaveoneout, file = here::here("looresults", "model_0_loo_small.RData"))

#-----------------------------------------------------------------------------------
# Leave Future out approximation: refit stan model
#-----------------------------------------------------------------------------------

L <- 20 # want 20 years of observations:1966-1985
dat.past <- dat
dat.past[21:36,] <- NA # change to NA
# Extract the missing values into a VECTOR
dat_complete <- dat.past[!is.na(dat.past)]

# Extract the missing and present values as MATRICES
ind_pres <- which(!is.na(dat.past), arr.ind = TRUE)
ind_miss <- which(is.na(dat.past), arr.ind = TRUE)


# get missing z data--dont remove this for LFO so the model is still constrained by DBH
datz <-data$z[,1:36]

# Extract the missing values into a VECTOR
dat_completez <- datz[!is.na(datz)]

# Extract the missing and present values as MATRICES
ind_presz <- which(!is.na(datz), arr.ind = TRUE)
ind_missz <- which(is.na(datz), arr.ind = TRUE)

# creaat a past data
past.data <- list(Nrow = nrow(dat.past),
                  Ncol = ncol(dat.past),
                  Ncomp = length(dat_complete),
                  Nmiss = sum(is.na(dat.past)),
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

# refit the SSM model with the 20 years of past data:

refit.0 <- stan(model_code = mod.0.stan , 
                data = past.data,
                iter = 2000, 
                chains = 3, 
                verbose=FALSE, 
                control =  list(max_treedepth = 15),#list(adapt_delta = 0.99, stepsize = 0.5, max_treedepth = 15),#, stepsize = 0.01, max_treedepth = 15),
                sample_file = model.name, 
                #adapt_delta = 0.99, 
                pars = c("mu", "sigma_inc", "sigma_add", "sigma_dbh","beta_YEAR", "alpha_TREE",
                         "x", "inc")) # , init = initfun)

# Note: this model is worse---
# Warning messages:
#   1: There were 258 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
# https://mc-stan.org/misc/warnings.html#bfmi-low 
# 3: Examine the pairs() plot to diagnose sampling problems
# 
# 4: The largest R-hat is 1.9, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 


# calculate log ratio of the predicted values from the past observations given the held out data
#loglik <- log_lik(fit_past, newdata = df_oos, oos = oos) # need to calculate log liklihood by hand:
# calculate loo:


# alot of this code below is base on the LFO tutorial from the loo package:
# https://mc-stan.org/loo/articles/loo2-lfo.html

refit_ssm_df <- as.data.frame(refit.0) # takes awhile to convert to df
covariates = c(
  "betaX","betaTmax", "betaPrecip", "betaMAP","betaMAT", "betaSDI", 
  "betaPrecip_MAP","betaPrecip_MAT",
  "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
  "betaTmax_MAP", "betaTmax_MAT", 
  "betaX_Precip", "betaX_Tmax", "betaX_SDI")

# note for model 0 there are no covariates
#ssm_fixed_df %>% dplyr::select(covariates)

sigma <- refit_ssm_df[,"sigma_add"] # get additive process error

# Tree-level random effects 
alpha_trees <- dplyr::select(refit_ssm_df, "alpha_TREE[1]":paste0("alpha_TREE[", mod.data$Nrow, "]"))

# Year-level random effects 
beta_years <- dplyr::select(refit_ssm_df, "beta_YEAR[1]":paste0("beta_YEAR[", mod.data$Ncol, "]"))


# loop over trees to get an increment_mu, with random effects:
increment_mu <- array(NA, dim = c(mod.data$Nrow, length(alpha_trees[, 1]),ncol = mod.data$Ncol))
for(i in 1:mod.data$Nrow){
  
  for(t in 1:mod.data$Ncol){
    increment_mu[i,,t] <- as.matrix(alpha_trees[, i]) + as.matrix(beta_years[,t]) # note that we need to add to this as we add covariates
  }
}

# reorganize so we just have predicted increment_mus for the timepoints with data?
inc.m <- reshape2::melt(increment_mu)
unique(inc.m$Var1) # # trees
increment_mu_spread <- inc.m %>% spread(Var2, value ) %>% select(-Var1, -Var3)

y.m <- reshape2::melt(data$y)
y.m.nona <- y.m[!is.na(y.m$value), ]

y.m.nona.yrs <- y.m[!is.na(y.m$value), ]
oos.index <- y.m.nona.yrs$Var2 > 1984
# now remove all the predictions for increment where y.m is ==NA
increment_mu_spread.nona <- increment_mu_spread[!is.na(y.m$value), ]

# calculate log liklihood of all the each time/individual predicted increment here:
ll <- matrix(0, length(sigma), length(y.m.nona))

for(i in 1:length(sigma)){
  ll[i,] <- dnorm(y.m.nona$value, increment_mu_spread.nona[,i], sd = sigma[i], log = TRUE)
}

refitll <- as.matrix(ll)


# helper function here 
sum_log_ratios <- function(loglik, ids = NULL) {
  if (!is.null(ids)) loglik <- loglik[, ids, drop = FALSE]
  rowSums(loglik)
}

# approx_elpds_1sap[L + 1] <- log_mean_exp(loglik[, oos])


# iterate over i > L to calculate the 1 step ahead 
#logratio & pareto k using the one-step ahead log liklihood
# note that we can't do this exactly for each tree because not all the tree records
# go up to 1997, so we will need to adjust by tree

N = ncol(data$y)
approx_elpds_1sap <- rep(NA, N)
i_refit <- L
i_refit <- L
refits <- L
ks <- NULL

for(i in 1:mod.data$Nrow){
  
  for (t in (L + 1):(N - 1)) {
    past <- 1:t
    oos <- t + 1
    # select the one step ahead observation/prediction to use as an index for refitll
    oos.index <- y.m.nona.yrs$Var2 == (1965+t) & y.m.nona.yrs$Var1 == i 
    oos.index2 <- y.m.nona.yrs$Var2 == (1965+oos) & y.m.nona.yrs$Var1 == i 
    #df_past <- df[past, , drop = FALSE]
    #df_oos <- df[c(past, oos), , drop = FALSE]
    refit.ll.mat <- data.frame(one = refitll[oos.index], two = refitll[oos.index2])
    refit.ll.mat <- cbind(refitll[oos.index], refitll[oos.index2])
    #loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    
    logratio <- sum_log_ratios(loglik = refit.ll.mat)  #, ids = (i_refit + 1):t)
    psis_obj <- suppressWarnings(psis(logratio)) # all of these are very bad--check calculations
    k <- psis_obj$diagnostics$pareto_k
    ks <- c(ks, k)
    
    # removing this here because refitting the model for each step ahead would be prohibitive with this model in stan
    # if (k > k_thres) {
    #   # refit the model based on the first i observations
    #   i_refit <- i
    #   refits <- c(refits, i)
    #   fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    #   loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    #   approx_elpds_1sap[i + 1] <- log_mean_exp(loglik[, oos])
    # } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    approx_elpds_1sap[t + 1] <- log_sum_exp(lw + refitll[, oos.index])
  }
} 

saveRDS(approx_elpds_1sap, here("looresults/", paste0("model_0_LFOelpds_1sap.Rdata")))
approx_elpds_1sap

approx_elpd_1sap <- sum(approx_elpds_1sap[1:32], na.rm = TRUE)

# plot_ks <- function(ks, ids, thres = 0.6) {
#   dat_ks <- data.frame(ks = ks, ids = ids)
#   ggplot(dat_ks, aes(x = ids, y = ks)) + 
#     geom_point(aes(color = ks > thres), shape = 3, show.legend = FALSE) + 
#     geom_hline(yintercept = thres, linetype = 2, color = "red2") + 
#     scale_color_manual(values = c("cornflowerblue", "darkblue")) + 
#     labs(x = "Data point", y = "Pareto k") + 
#     ylim(-0.5, 1.5)
# }
# approx_elpds_1sap
