library(rstan)
library(MASS)
options(mc.cores = parallel::detectCores())
# revised model
data <- readRDS("/Users/kellyheilman/Documents/Treering_FIA_fusion/data/regional_pipo_jags_formatted_data.RDS")
data$tau_y_ic = 1/10

# only one chain for testing
#init <- readRDS("jags.init.formatted.rds")[[1]]
#class(init)
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




# STAN model
mod.nomiss <- '
data {
    int<lower=0> Nrow;
    int<lower=0> Ncol;
    int<lower=0> Ncomp; // Number of non-missing values. 
    int<lower=0> Nmiss; // Number of missing values
    real dat_complete[Ncomp];   // Vector of non-missing values
    int ind_pres[Ncomp, 2];     // Matrix (row, col) of non-missing value indices
    int ind_miss[Nmiss, 2];     // Matrix (row, col) of missing value indices
}
parameters {
    // Multivariate normal distribution parameters
    real mu;
    real<lower=0> sigma_inc;
    real<lower=0> sigma_add;
    // Vector containing "stochastic" nodes (for filling missing values
    real ymiss[Nmiss];
    
    real<lower=0> inc[Nrow, Ncol];
}

transformed parameters {
    real y[Nrow, Ncol];   // The "data" with interpolated missing values
    #real z[Nrow_z];   // The z "data" with interpolated missing values
    
    // Fill y with non-missing values 
    for(n in 1:Ncomp) {
        y[ind_pres[n,1], ind_pres[n,2]] <- dat_complete[n];
    }
    // Fill the rest of y with missing value "parameters"
    for(n in 1:Nmiss){
        y[ind_miss[n, 1], ind_miss[n,2]] <- ymiss[n];
    }
    
    #  // Fill x with non-missing values 
    # for(n in 1:Ncomp_z) {
    #     z[ind_presz[n]] <- dat_completez[n];
    # }
    # // Fill the rest of y with missing value "parameters"
    # for(n in 1:Nmiss_z){
    #     z[ind_missz[n]] <- zmiss[n];
    # }
    # 
    
}
model {

//data & process model for increment
  for(i in 1:Nrow){
     for(t in 1:Ncol){
        y[i,t] ~ normal(inc[i,t], sigma_inc);
        inc[i,t] ~ lognormal(mu, sigma_add);
    }
  }
}
'


View(data$SDIscaled)
data$SDInontv <- rowMeans(data$SDIscaled, na.rm = TRUE)
data$SDIscaled[is.na(data$SDIscaled)] <- 0


# there are two NA values in 1966 --find a better way to replace
# data$SDIscaled[49,1] <- data$SDIscaled[49,2]
# data$SDIscaled[86,1] <- data$SDIscaled[86,2]


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


summary(mod.data$SDI)
initfun <- function(...) {list(x=runif(1,5,35))}#, sigma2=runif(1,0.01,0.05))}
initfun(1)

model.name <- "large_ssm_stan_sigma_dbh_gamma_3.04_20.9"
model.name <- "large_ssm_stan_sigma_dbh_normal_0_5"
model.name <- "small_ssm_stan_sigma_dbh_gamma_3.04_20.9_0_1constraint"
model.name <- "small_ssm_stan_sigma_dbh_normal_0.15_0.8_0_1constraint"
model.name <- "full_ssm_stan_sigma_dbh_normal_dbh_normal_full_X_interactions_sigma_constrained"

fit.betaX.tmax.precip <- stan(model_code = mod.ssm.stan , data = mod.data,
            iter = 2000, chains = 3, verbose=FALSE, control =  list(max_treedepth = 15),#list(adapt_delta = 0.99, stepsize = 0.5, max_treedepth = 15),#, stepsize = 0.01, max_treedepth = 15),
            sample_file = model.name, 
            #adapt_delta = 0.99, 
            pars = c("mu", "sigma_inc", "sigma_add", "sigma_dbh","beta_YEAR", "alpha_TREE", "betaX", "x", "inc", 
                     "betaTmax", "betaPrecip",  
                   
                     "betaTmax", "betaPrecip", "betaMAP",
                     "betaPrecip_MAP","betaPrecip_MAT",
                     "betaMAT", "betaSDI", "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
                     "betaTmax_MAP", "betaTmax_MAT",
                     "betaX_Precip", "betaX_Tmax", "betaX_SDI", "x", "inc")) # , init = initfun)
setwd("model_simple_run")
csvfiles <- paste0("full_ssm_stan_sigma_dbh_normal_dbh_normal_full_X_interactions_sigma_constrained_", 1:3,".csv")
csv1 <- rstan::read_stan_csv(csvfiles[1])
csv2 <- rstan::read_stan_csv(csvfiles[2])
csv3 <- rstan::read_stan_csv(csvfiles[3])

stancsvs <- list(csv1, csv2)

fit.betaX.tmax.precip <- sflist2stanfit(stancsvs)


par.names = c("mu", "sigma_inc", 
              "sigma_add", 
              "sigma_dbh", #)#, #)
             "alpha_TREE[1]", "beta_YEAR[1]",
               "betaX",
             "betaTmax", "betaPrecip", "betaMAP",
             "betaPrecip_MAP","betaPrecip_MAT",
             "betaMAT", "betaSDI", "betaPrecip_Tmax", "betaTmax_SDI", "betaPrecip_SDI",
             "betaTmax_MAP", "betaTmax_MAT", 
             "betaX_Precip", "betaX_Tmax", "betaX_SDI")
color_scheme_set("mix-blue-red")


png(height = 16, width = 7, units = "in", res = 100, paste0("output/traceplots_tau_", model.name, "default3.png"))
#par(mfrow = c(5, 3))
traceplot (csv3, pars = par.names, nrow = 8, ncol = 3, inc_warmup = FALSE) 
dev.off()


png(height = 16, width = 7, units = "in", res = 100, paste0("output/traceplots_tau_", model.name, "default.png"))
#par(mfrow = c(5, 3))
traceplot (fit.betaX.tmax.precip, pars = par.names, nrow = 8, ncol = 3, inc_warmup = FALSE) 
dev.off()

pairs(fit.betaX.tmax.precip, pars = c("mu", "sigma_inc", 
                                      "sigma_add", 
                                      "sigma_dbh"))

# other traces: traceplot(fit.betaX.tmax.precip, pars = c("mu", "betaX"), nrow = 5, ncol = 3,   inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaX", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "beta_YEAR[1]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "beta_YEAR[35]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "beta_YEAR[15]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "alpha_TREE[1]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "alpha_TREE[35]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "alpha_TREE[15]", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "sigma_inc", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "sigma_add", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaTmax", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaPrecip", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaMAP", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaMAT", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaPrecip_MAT", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaPrecip_MAP", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaSDI", inc_warmup = FALSE)
traceplot(fit.betaX.tmax.precip, pars = "betaPrecip_Tmax", inc_warmup = FALSE)

# get the x and inc outputs here:
rm(csv1, csv2)
x.pred <- dplyr::select(as.data.frame(fit.betaX.tmax.precip),"x[1,1]":"x[1046,36]")
inc.pred <- dplyr::select(as.data.frame(fit.betaX.tmax.precip),"inc[1,1]":"inc[1046,36]")

model.out <- cbind(x.pred, inc.pred) # get this to make plots
#model.out <- fit.betaX.tmax.precip


# mcmc_trace(posterior, pars = c("mu", "sigma_inc", "sigma_add")) #, 
#            #facet_args = list(ncol = 2, strip.position = "left"))
# mcmc_pairs(posterior, pars = c("mu", "sigma_inc", "sigma_add", "beta_YEAR[1]", "alpha_TREE[1]"),
#            off_diag_args = list(size = 1.5))
# # still getting errors:
# # Chain 1: Rejecting initial value:
# #   Chain 1:   Error evaluating the log probability at the initial value.
# # Chain 1: Exception: validate transformed params: x[i_0__][i_1__] is nan, but must be greater than or equal to 0  (in 'model2dfe15ccacbd_002e0aafcfec963b3f195ba7e2de2729' at line 38)
