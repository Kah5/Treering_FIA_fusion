# script to generate a summary of ssm models run so far:

library(here)
library(tidyverse)
library(rstan)


# list all increment predicted and observed files:
model.names.rds <- list.files(path = here("small_model_fits"), pattern = "\\.RDS$")
model.names <- substring(model.names.rds, 1, nchar(model.names.rds) - 4) # remove the .inc.rds

rds.files <- list.files(path = here("model_simple_run/output"), pattern = "\\.rds$")
inc.files <- list.files(path = here("model_simple_run/output"), pattern = "\\inc.rds$")
dbh.files <- list.files(path = here("model_simple_run/output"), pattern = "\\.dbh.rds$")# need the . for some reason

rds.inc <- list()
rds.inc <- lapply(inc.files, function(x){readRDS(here("model_simple_run/output/", x))})
names(rds.inc) <- inc.files
# need to change the names of these
rds.inc <- lapply(rds.inc, function(x){x %>% select(inc.data, year, min.ci, mean.ci, max.ci, id)})

# make a dataframe, add the file name as a column
inc.p.o.df <- do.call(rbind, rds.inc)
model.names.inc <- str_sub(names(rds.inc), 1, -31) # remove the .inc.rds
inc.p.o.df$filename <- rep(model.names.inc, sapply(rds.inc, nrow))#add file name as column

inc.p.o.df <- inc.p.o.df %>% filter (filename %in% model.names)

# add a model number shorthand
filename.num <- data.frame(filename = c("null.REonly", 
                                        "RE.MAP.MAT.X",
                                        "RE.MAP.MAT.X.timevaryingclimate", 
                                        "RE.MAP.MAT.X.timevaryingclimate.SDI", 
                                        "RE.MAP.MAT.X.timevaryingclimate.SDI.most.interactions", 
                                        "RE.MAP.MAT.X.timevaryingclimate.SDI.all.interactions.REX", 
                                        "RE.MAP.MAT.X.timevaryingclimate.SDI.all.interactions.Xdecay",
                                        "RE.MAP.MAT.X.timevaryingclimate.SDI.all.interactions.X2"), 
           number = 0:7)

inc.p.o.df <- left_join(inc.p.o.df, filename.num)

# compare increment to predicted means using MSPE
inc.MSE <- inc.p.o.df %>% group_by(year, number) %>% mutate(squared.error = (inc.data - mean.ci)^2) %>%
  ungroup() %>% group_by(number, filename) %>% summarise(INC.MSE = mean(squared.error, na.rm = TRUE))

ggplot(data = inc.MSE, aes(number, y = INC.MSE))+geom_point()


# do the same for diameter:

rds.dbh <- lapply(dbh.files, function(x){readRDS(here("model_simple_run/output/", x))})
names(rds.dbh) <- dbh.files
# need to change the names of these
rds.dbh <- lapply(rds.dbh, function(x){x %>% select(z.data, year, min.ci, mean.ci, max.ci)})

# make a dataframe, add the file name as a column
dbh.p.o.df <- do.call(rbind, rds.dbh)
model.names.dbh <- str_sub(names(rds.dbh), 1, -31) # remove the .inc.rds
dbh.p.o.df$filename <- rep(model.names.dbh, sapply(rds.dbh, nrow))#add file name as column
filename.num$filename

dbh.p.o.df <- dbh.p.o.df %>% filter (filename %in% model.names)

dbh.p.o.df <- left_join(dbh.p.o.df, filename.num)

# compare dbhrement to predicted means using MSPE
dbh.MSE <- dbh.p.o.df %>% group_by(year, number) %>% mutate(squared.error = (z.data - mean.ci)^2) %>%
  ungroup() %>% group_by(number, filename) %>% summarise(DBH.MSE = mean(squared.error, na.rm = TRUE))

ggplot(data = dbh.MSE, aes(number, y = DBH.MSE))+geom_point()

full.MSE <- left_join(inc.MSE, dbh.MSE)

unique(full.MSE$number)

# now for all of these read in the LOO statistics:
loo.model.list <- list()

for( i in 0:max(unique(full.MSE$number))){
    load(here("looresults", paste0("model_", i, "_loo_small.RData")))
    # load the ll, r_eff, leaveoneout stats for this model:
    looic <- leaveoneout$estimates[3,1] # get looic estimates
    looic.SD <- leaveoneout$estimates[3,2] # get looic estimates
    elpd <- leaveoneout$estimates[1,1] # get looic estimates
    elpd.SD <- leaveoneout$estimates[1,2] # get looic estimates
    
    loo.model.list[[i+1]] <- data.frame(model = i, 
                              looic = looic, 
                              looic.SD = looic.SD, 
                              elpd = elpd, 
                              elpd.SD = elpd.SD)
    
    rm(ll, r_eff, leaveoneout)
}
loo.model.df <- do.call(rbind, loo.model.list)

# plot the elpd stats
ggplot()+geom_point( data = loo.model.df , aes(x = as.character(model), elpd))+
  geom_linerange(data = loo.model.df, aes(x = as.character(model), ymin = elpd + qnorm(0.25)*elpd.SD, ymax = elpd + qnorm(0.75)*elpd.SD), linewidth = 1.5)+
  geom_linerange(data = loo.model.df, aes(x = as.character(model), ymin = elpd + qnorm(0.025)*elpd.SD, ymax = elpd + qnorm(0.975)*elpd.SD))+theme_bw(base_size = 16)+
  xlab("Model")+theme(panel.grid = element_blank())
ggsave(height = 4, width = 6, units = "in", here("model_simple_run/", "all-model-elpd.png"))

colnames(full.MSE)[1] <- "model"
all.summary <- left_join(full.MSE, loo.model.df)

# plot the MSE
ggplot()+geom_point( data = all.summary , aes(x = as.character(model), INC.MSE))+
  # geom_linerange(data = all.summary , aes(x = as.character(model), ymin = elpd + qnorm(0.25)*elpd.SD, ymax = elpd + qnorm(0.75)*elpd.SD), linewidth = 1.5)+
  # geom_linerange(data = all.summary , aes(x = as.character(model), ymin = elpd + qnorm(0.025)*elpd.SD, ymax = elpd + qnorm(0.975)*elpd.SD))+
  theme_bw(base_size = 16)+
  xlab("Model")+theme(panel.grid = element_blank()) + ylab("MSE for Increment")
ggsave(height = 4, width = 6, units = "in", here("model_simple_run/", "all-model-INC-MSE.png"))


ggplot()+geom_point( data = all.summary , aes(x = as.character(model), DBH.MSE))+
  # geom_linerange(data = all.summary , aes(x = as.character(model), ymin = elpd + qnorm(0.25)*elpd.SD, ymax = elpd + qnorm(0.75)*elpd.SD), linewidth = 1.5)+
  # geom_linerange(data = all.summary , aes(x = as.character(model), ymin = elpd + qnorm(0.025)*elpd.SD, ymax = elpd + qnorm(0.975)*elpd.SD))+
  theme_bw(base_size = 16)+
  xlab("Model")+theme(panel.grid = element_blank())+ ylab("MSE for Diameter")

ggsave(height = 4, width = 6, units = "in", here("model_simple_run/", "all-model-DBH-MSE.png"))


# SAVE THE OUTPUTS:
write.csv(all.summary, here("model_simple_run/", "all-model-validation-summary.csv"))

