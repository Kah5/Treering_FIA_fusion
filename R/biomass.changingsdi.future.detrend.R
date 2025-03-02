biomass.changingsdi.zeroinc.SDIscaled.future.detrend <- function(plot, density.dependent = TRUE, density.independent = TRUE, scenario = "rcp26"){
  
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
  
  
  
  # read.xvals <- function(treid){if (file.exists(paste0("data/output/xvals_additional_trees/Xvals_tree_",treid,".RDS"))) {
  #  readRDS(paste0("data/output/xvals_additional_trees/Xvals_tree_",treid,".RDS"))}else{rep(NA, 1000)}}
  #x.additionals <- lapply(y, FUN = read.xvals)
  selx <- which(ci.names.noncored$row %in%  y) 
  #x.plots <- x.mat2[,selx]
  # calculate CIS names
  #x.plots <- do.call(cbind, x.additionals)
  #ci.noncored      <- apply(x.plots, 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
  #mean.dia.noncored       <- apply(x.plots, 2, mean) # get the var.pred for the last 800 samples
  # #use mikes funciton to rename the x columns so that they indicate which tree and year it is referring to: x[tree, time]
  #ci.names.noncored <- parse.MatrixNames(colnames(ci.noncored), numeric = TRUE)
  
  
  # plot the posterior predictions of DBH for a single tree:
  sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
  mean.cored <- mean.pred.cored[sel]
  
  
  # plot the posterior predictions of DBH for a single tree:
  # dont really need this step any more because we are subsetting
  # sel <- which(ci.names.noncored$row %in% y) # use sel to subset the data for the 415th tree
  # mean.dia.noncored <- mean.pred.noncored[sel]
  # 
  
  
  # for noncored trees: just seelct the 36 year time point
  tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x & ci.names.noncored$col == 36)})
  i <- do.call(cbind, tree.ind.noncrored )
  out.noncored.plt <-  x.mat2[, i] 
  
  # for cored trees:
  #yrs <- 31:135
  tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x & ci.names$col == 36)}) # select just the years 1994:2010 to match the plot level data:
  i.cored <- do.call(rbind, tree.ind.cored )
  
  if(class(out.noncored.plt)== "numeric"){
    nmcmc <- min(length(out.cored[,1]),length(out.noncored.plt))
  }else{
    nmcmc <- min(length(out.cored[,1]),length(out.noncored.plt[,1]))
  }
  
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
  # dbh.dead:
  dbh.dead <- dbh.pred
  
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
  
  if(scenario %in% "rcp26"){
    clim.fut.scen <- future.clim.subset.26 
  }
  if(scenario %in% "rcp45"){
    clim.fut.scen <- future.clim.subset.26 
  }
  if(scenario %in% "rcp60"){
    clim.fut.scen <- future.clim.subset.26 
  }
  if(scenario %in% "rcp85"){
    clim.fut.scen <- future.clim.subset.26 
  }
  
  fut.clim.scen <- scale.fut.clim.by.plt(x = PLT_CNint, future.clim.subset = clim.fut.scen)
  
  #scenario <- "rcp26"
  
  
  models <- unique(fut.clim.scen$model) # 21 models
  sample.model <- sample(models, size = length(models), replace= FALSE)
  
  
  
  get.ens.df <- function(i){
    
    ens.proj.yr <- fut.clim.scen %>% filter(model %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scale, 
                     model = i, 
                     year = ens.proj.yr$year)
    df$diff.ppt <- NA
    df$diff.tmax <- NA
    
    for(t in 2:82){
      
      df[t,]$diff.ppt <- df[t,]$ppt-df[t-1,]$ppt
      df[t,]$diff.tmax <- df[t,]$tmax - df[t-1,]$tmax
    }
    
    df
  }
  
  ens.samps <- lapply(1:length(models), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  # detrend the future climate using differences
  #unique(duplicated(ens.samps.df)) # no duplicates
  #unique(duplicated(ens.samps.df[,c("model", "year")]))
  ens.samps.df  <- ens.samps.df %>% dplyr::select(diff.ppt, diff.tmax, model, year)
  colnames(ens.samps.df) <- c("ppt", "tmax", "model", "year")
  #   group_by(model, year)%>% 
  #  
  #   #spread(model, ppt)
  #   mutate(Diff = c(NA, diff(ppt)))%>%
  # arrange(model, year)
  #ens.means  
  #dplyr::mutate(diff.ppt = ifelse(year == 2018, ppt, ppt - lag(ppt)),
  #             diff.tmax =   ifelse(year == 2018, tmax, diff(tmax)))
  
  # ggplot(ens.means, aes(x = year, y = diff.tmax, group = model))+geom_line()
  
  ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
  
  tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
  hist.samps.df <- data.frame(ppt = rep(as.numeric(ppt.hist), length(unique(ens.samps.df$model))), 
                              tmax = rep(as.numeric(tmax.hist) , length(unique(ens.samps.df$model))), 
                              model = rep(1:length(unique(ens.samps.df$model)), each = length(as.numeric(ppt.hist))), 
                              year = rep(2001:2018, length(unique(ens.samps.df$model))))
  
  full.df <- rbind(hist.samps.df, ens.samps.df %>% filter(!year %in% 2018))
  full.df.nodups <- full.df[!duplicated(full.df),]
  #full.df$rowid <- 1:length(full.df$ppt)
  full.ens.ppt <- na.omit(full.df.nodups)  %>% dplyr::select( ppt, year, model)%>%  group_by(model)%>%
    spread(year, value = ppt, drop = FALSE) %>% ungroup ()%>% dplyr::select(-model)
  
  full.ens.tmax <- na.omit(full.df.nodups)  %>% dplyr::select(tmax, year, model)%>%  group_by(model)%>%
    spread(year, value = tmax, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
  
  
  
  
  ppt <- full.ens.ppt
  tmax <- full.ens.tmax
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
  # for each tree get the next statespace time step:
  for(t in 1:nt){ # for each year t in the # of trees
    #if(t == 1){
    mort.code <- 0
    # loop through all of the trees for year t
    for (i in 1:ni){ # for each tree i in the n of trees
      
      SUBPLOT.index <- index.df[ni,]$SUBP # select the subplot for the tree:
      
      if(mean(dbh.pred[i,,t]) == 0){ # if the tree was killed off dbh.pred will be zero, so assign as zero
        dbh.pred[i,,t+1] <- dbh.pred[i,,t]
        dbh.dead[i,,t+1] <- dbh.dead[i,,t]
        
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
        
        mort.code <- rbinom(1,1, prob = mort.prob)
      }
      # else{
      #   mort.code <- 0
      # }
      if(mort.code == 1 & is.na(dbh.dead[i,,t]) & dbh.pred[i,,t]>0){
        dbh.dead[i,,(t+1):nt] <- dbh.pred[i,,t] # set dead diameter to the last live estimated diameter for the tree
        dbh.pred[i,,(t+1):nt] <- 0 # set live diameter to zero
        
      }
      
    }
    
    # use the current SDI to determine density dependnt mortality at the stand scale, then distribute to trees:
    # my attempt to reflect what FVS is doing
    sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1]
    SDI.raw <- rescale.sdi.raw(SDIscaled =  sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1], j = unique(as.character(cored.in.plt$PLT_CN)), a = index.df[ni,]$SUBP)
    
    if(density.dependent == TRUE){
      # if the stand is above 70% of SDI and mort probability is not already 1, increase mortality probability
      # this should increase mortality liklihood across all the trees, but mostly larger trees
      if( SDI.raw > (450/nrow(sdi.subp))*0.60 ){
        
        # loosly basing on FVS mortality:
        # calculate an RI for each tree:
        p0 <- 5.5877
        p1 <- 0.005348
        DBH <- dbh.pred[,,t]/2.54
        live.trees <- rowSums(DBH) > 0
        rownames(DBH) <- 1:nrow(DBH)
        DBH.live  <- DBH[live.trees,]
        RI <- (1/(1+exp(p0 + p1*DBH.live)))*0.5
        Y <- 1
        RIP <- 1-(1-RI)^Y # when Y == 1, RI and RIP are equal
        Mort.rate <- mean(colSums(RI)) # total background mortality for the plot
        
        BA <- pi*(DBH.live/2)^2
        PCT <- apply(BA, 2, percent_rank)
        MR <- (0.84525-(0.01074*PCT)+(0.0000002*PCT^3))
        
        n.ded <- round(Mort.rate*nrow(DBH.live)) # number of trees to kill
        #n.ded <-1
        MR <- ifelse(MR <0, 0, MR)
        
        avg.mort.prob <- rowMeans(MR)
        
        dead.index <-  which(order(avg.mort.prob) %in% 1:n.ded) # gives the tree # (s) that should die
        
        dd.treenos <- as.numeric(rownames(DBH.live)[dead.index])
        
        
        if(n.ded > 0 ){
          dbh.dead[dd.treenos,,(t):nt] <- dbh.pred[dd.treenos,,t] # set dead diameter to the last live estimated diameter for the tree
          
          dbh.pred[dd.treenos,,(t+1):nt] <- 0 # set live diameter to zero
          #dbh.dead[dd.treenos,,t:nt] <- dbh.pred[dd.treenos,,t:nt] # set dead diameter to the last live estimated diameter for the tree
        } 
        #}
        # kill off trees using MR to estimate probability that they are killed? 
        # prob.list <- apply(MR, 1,FUN = function(x){rbinom(1,1, prob = x)})
        # 
        # # how to get prob.list to only haev the number of ded trees
        # 
        # prob.vec <- as.vector(do.call(cbind, prob.list))
        
        
        
        # mortality probability is how close SDI of the plot is to the self-thinning law +
        #the size of the tree relative tot the max tree size of the plot
        # there are some issues with this but we can edit later
        #rel.size <- (max(dbh.pred[,,t+1])-mean(dbh.pred[i,,t+1]))/max(dbh.pred[,,t+1])
        
        # change the max dbh to 75, since the max dbh at start time is 76.9
        #rel.size <- ((75)-mean(dbh.pred[i,,t+1], na.rm =TRUE))/75
        
        # mort.prob <- (SDI.raw/(450/nrow(sdi.subp)))*0.60 + rel.size# mortali#/(450/nrow(sdi.subp))#+mort.prob
        
        
      }
    }  
    
    # before moving onto the next year, calculate the SDI
    # calculate SDI by the subplot
    for(s in sdi.subp[,1]){ # for each subplot s in the # of subplots
      # need to index dbh.pred trees by by the subplot:
      trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
      sdi.subp[which(sdi.subp[,1]==s),t+2] <- calc.sdi.subp(dbh.pred[trees.subplot,,t+1], j = as.character(cored.in.plt$PLT_CN), a = s)
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
  saveRDS(dbh.pred, paste0("diam_forecasts_nocc/PLT.dbh.",mort.scheme,".", plot,".", scenario,".2001.2018.RDS"))
  saveRDS(dbh.dead, paste0("diam_forecasts_nocc/PLT.dbh.dead.",mort.scheme,".", plot,".", scenario,".2001.2018.RDS"))
  
  saveRDS(sdi.subp, paste0("diam_forecasts_nocc/PLT.sdi.",mort.scheme,".", plot,".", scenario,".SUBP.2001.2018.RDS"))
  saveRDS(index.df, paste0("diam_forecasts_nocc/PLT.dbh.combined",mort.scheme,".", plot,".", scenario,".2001.2018.RDS"))
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
  
  # plot dead trees to see how they are behaving:
  dead.quants <- reshape2::melt(apply(dbh.dead, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
  colnames(dead.quants) <- c("quantile","treeno","time", "diameter")
  
  
  dead.quants.spread <- dead.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
  # dead.means <- reshape2::melt(apply(dead.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
  # colnames(dead.means) <- c("treeno","time", "diameter")
  index.df$treeno <- 1:length(index.df$treeid)
  
  dead.means.index <- left_join(dead.quants.spread , index.df, by = "treeno")
  
  ded <- ggplot()+geom_line(data = dead.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) + 
    geom_ribbon(data = dead.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + ggtitle(paste0("Diameter of dead for plot ", plot))
  
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
    theme_bw() + ylab("Increments (cm)")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))+ labs(fill = "Subplot Number", color = "Subplot Number") 
  
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
  #out <- out[1:50,]
  out.mean <- apply(out, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})
  
  
  # calculate an out for the dead trees
  # var 1 is tree, var2 is mcmc sample, and var 3 is time
  test.m <- melt(dbh.dead, id.vars = dim(dbh.dead)[2])
  test.m$id <- paste0("x[",test.m$Var1, ",", test.m$Var3,"]")
  
  colid.ordered <- test.m$id
  
  test.m.time.dead <- test.m %>% dplyr::select(-Var1, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
  out.dead <- test.m.time.dead %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(colid.ordered)
  #out <- out[1:50,]
  out.mean.dead <- apply(out.dead, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})
  
  
  
  # biomass estimation
  
  cat("start biomass estimates")
  plot2AGB_nocc(combined = combined, out = out.mean, out.dead = out.mean.dead, mort.scheme = mort.scheme, allom.stats = kaye_pipo, unit.conv = 0.02, plot = plot, yrvec = 2001:2098, scenario = scenario, p = p, p.inc = p.inc)
  
}
