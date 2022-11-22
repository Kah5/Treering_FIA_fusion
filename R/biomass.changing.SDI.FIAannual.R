biomass.changingsdi.SDIscaled.FIA <- function(plot, density.dependent = TRUE, density.independent = TRUE, scenario = "rcp26", SDI.ratio.DD = 0.8){
  
  #TPA_lookup <- TREE %>% filter(PLT_CN %in% plot) %>% dplyr::select(TPA_UNADJ, TPAMORT_UNADJ, PLOT, SUBP, TREE, MEASYR, DESIGNCD)
  # -------- get the diameter estimates for all trees on the plot: ------------------------
  print(as.character(plot))
  # get id of trees with cores:
  # cored.in.plt <- cov.data.regional %>% dplyr::filter (PLT_CN %in% plot)
  # cored.in.plt <- cored.in.plt[!duplicated(cored.in.plt$TRE_CN),]
  # cored.in.plt$TPA_UNADJ <- TREE[which(TREE$CN %in% unique(cored.in.plt$TRE_CN)),]$TPA_UNADJ
  # 
  # get id of trees with out cores:
  trees.in.plt <- TREE %>% dplyr::filter (PLT_CN %in% plot & STATUSCD == 1)
  trees.in.plt$TPA_UNADJ <- TREE[which(TREE$CN %in% trees.in.plt$CN),]$TPA_UNADJ
  
  if(nrow(trees.in.plt)<=1){
    cat("less than 1 tree on the plot")
  }else{
  # get PLT_CN
  PLT_CNint <- as.character(plot)
  
  # get the unique SUBPLOTS in the plot
  
  SDI.PLT <- SDI.mat.PLT.subp %>% filter(PLT_CN %in% PLT_CNint & !is.na(`2001`))
  
  
  SDI.PLT.SCALED <- SDI.PLT # get the SDI values
  subplots <- unique(SDI.PLT.SCALED %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(SUBP))
  
  trees.in.plt <- left_join( trees.in.plt, subplots)
  unique(trees.in.plt$DIA)
  #if(!length(trees.in.plt$PLOT) == 0){
  #cored.treeid <- cored.in.plt$treeid
  trees.in.plt <- trees.in.plt[!is.na(trees.in.plt$DIA),]
  #trees.in.plt$DIA
  
  y <- trees.in.plt$CN
  
  #y
  # get the subplot information & TPA information
  trees.in.plt.subp <- trees.in.plt[,c("CN", "SUBP", "TPA_UNADJ")]
  #trees.in.plt.subp %>% filter()
  #cored.in.plt.subp <- cored.in.plt[,c("treeid", "SUBP","TPA_UNADJ")]
  
 # combined <- rbind( cored.in.plt.subp, trees.in.plt.subp)
  combined <- trees.in.plt.subp
  #y <- y # just to reconcile the updated data with current
  #m <- x[1] # since all cored trees will have the same plot information this is okay
  
  out.noncored <- spread.dbh.mat %>% filter(CN %in% combined$CN)
  out.noncored.yr <- names(which(!is.na(colSums(out.noncored[,4:length(out.noncored)]))))
  out.FIA<- list()  
  for(i in 1:nrow(out.noncored[,out.noncored.yr])){
   out.FIA[[i]] <- rnorm(100, mean = as.numeric(out.noncored[i,out.noncored.yr]), sd = mean(sqrt(1/tau_dbh)))
  }
  out.FIA.df <- do.call(cbind, out.FIA)
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  
  
  # sample from the population mean (mu) for the trees that dont have RE
  
  
  #read.xvals <- function(treid){if (file.exists(paste0("data/output/xvals_additional_trees/Xvals_tree_",treid,".RDS"))) {
  #   readRDS(paste0("xvals_additional_trees/Xvals_tree_",treid,".RDS"))}else{rep(NA, 1000)}}
  # x.additionals <- lapply(y, FUN = read.xvals)
  
  # instead of drawing from samples, just draw from the estimates of diameter for each tree.
  
  # # alternatively read from x.mat2
  # selx <- which(ci.names.noncored$row %in%  y) 
  # 
  # # plot the posterior predictions of DBH for a single tree:
  # sel <- which(ci.names$row %in%  x) # use sel to subset the data for the 415th tree
  # mean.cored <- mean.pred.cored[sel]
  # 
  # 
  # # plot the posterior predictions of DBH for a single tree:
  # # dont really need this step any more because we are subsetting
  # 
  # 
  # 
  # # for noncored trees: just seelct the 36 year time point
  # tree.ind.noncrored <- lapply(X = y, FUN= function(x){which(ci.names.noncored$row == x & ci.names.noncored$col == 33:36)})
  # i <- do.call(cbind, tree.ind.noncrored )
  # out.noncored.plt <-  x.mat2[, i] 
  # 
  # # for cored trees:
  # #yrs <- 31:135
  # tree.ind.cored <- lapply(X = x, FUN= function(x){which(ci.names$row == x & ci.names$col %in% 33:36)}) # select just the years 1994:2010 to match the plot level data:
  # i.cored <- do.call(rbind, tree.ind.cored )
  # 
  # if(class(out.noncored.plt)== "numeric"){
  #   nsamps <- min(length(out.cored[,1]),length(out.noncored.plt))
  # }else{
  #   nsamps <- min(length(out.cored[,1]),length(out.noncored.plt[,1]))
  # }
  # 
  
  
  # out.cored.plt <-  out.cored[(length(out.cored[,1])-nsamps + 1):length(out.cored[,1]),i.cored] 
  # 
  # all.dbh <- cbind(out.cored.plt, out.noncored.plt)
  # 
  # # get plot subplot and cored status for each tree:
  # trees.in.plt.subp$type <- "noncored"
  #cored.in.plt.subp$type <- "cored"
  index.df <-  trees.in.plt.subp
  all.dbh <-   out.FIA.df
  
  # make a big array with all the DBH estimates:
  
  ni <- nrow(index.df)# number of individuals per plot
  
  
  nsamps <- length(out.FIA.df [,1])
  nt <- length(2001:2018)
  ntfull <- length(2001:2018)
  # fill in the array with the diameter estimates for 2018:
  dbh.pred <- increment <- TPAmort<- array(NA, dim = c(ni, nsamps, ntfull + 1))
  # dbh.dead:
  dbh.dead <- dbh.pred
  
  
  
  id.ni <- rep(1:ni, each = 1)
  #id.time <- 1:17
  for(i in 1:length(id.ni)){
    
    dbh.pred[id.ni[i],,1] <- all.dbh[,i]
    
  }
  
  # get the increments
  for(i in 1:ni){
    for(t in 2:4){
      increment[i,,t] <- dbh.pred[i,,t] - dbh.pred[i,,t-1]
    }
  }
  
  for(i in 1:ni){
    
    TPAmort[i,,1] <- index.df[i, "TPA_UNADJ"]
    
  }
  
 # TPAmort <- TPAmort[,,4:(ntfull+1)]
  
  # set up SDI and subplot information:
  #SDI.matrix
  
  
  
  #---------------------------------------------------------------------------
  ##  get all the paramter estimates + uncertainty
  #---------------------------------------------------------------------------
  
  nsamps1 <- nsamps-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
  
  #treeids <- cored.in.plt$treeid
  #if(length(treeids$treeid)>1){
  
  
  # use all of the parameter MCMCS:
  #alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  
  # write a function to get the MCMC samples
  
  get_mcmc_samples <- function(x, betas, nsamps){
    
    rnorm(nsamps, mean = mean(betas[,x]), sd = sd(betas[,x]))
  }
  
  nsamps <- nsamps
  #get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = 1000)
  # for each tree generate a random sample for the tree-level intercept:
  
  # if(length(treeids)>1){ # if we have more than one cored tree per plot
  #   alphatreeids <- vector()
  #   for(i in 1:length(treeids)){
  #     alphatreeids[i]<- paste0("alpha_TREE[", treeids[i], "]")
  #     
  #   }
  #   
  # }else{ # if there is just one cored tree per plot
  #   alphatreeids <- paste0("alpha_TREE[", treeids, "]")
  # }
  # 
  
  alpha <- get_mcmc_samples("mu", betas = mus, nsamps = nsamps)
  
  
  
  # for all of the trees with cores:
  alphas.tree.plot <- list()
  # 
   for(i in 1:length(index.df$CN)){
  #   if(index.df[i,]$type %in% "cored"){ # if the tree was cored and has the alpha estimated in posteriors
  #     alphas.tree.plot[[i]] <- get_mcmc_samples(x = paste0("alpha_TREE[", index.df[i,]$treeid, "]"), betas = alphas, nsamps = nsamps)
  #     
  #   }else{ # if the tree was not cored and we randomly sample from the trees in the plot
  #     
  #     if(length(alphatreeids)>1)  {
        #alphas2 <- as.matrix(as.vector(alphas[,alphatreeids]))
        treealphas <- rnorm(nsamps, mean = mean(alpha), sd = sd(alpha))
        # treealphas <- lapply(alphatreeids, get_mcmc_samples, betas = alphas2, nsamps = nsamps)
        # treealphas <- do.call(rbind, treealphas)
      #   # colnames(treealphas)<- alphatreeids
      # }else{
      #   alphaplotid <- paste0("alpha_TREE[", treeids, "]")
      #   treealphas <- get_mcmc_samples(x = alphaplotid, betas = alphas, nsamps = nsamps)
      # }
      
      alphas.tree.plot[[i]] <-treealphas
   }
    
  #}
  
  
  # need to fix the alpha...now just taking the global intercept
  # if the tree is a cored tree, include plot random effect, but if it is not, then 
  alpha <- get_mcmc_samples("mu", betas = mus, nsamps = nsamps)
  
  
  bMAP <- get_mcmc_samples("betaMAP", betas = betas, nsamps = nsamps)
  bMAT <- get_mcmc_samples("betaMAT", betas = betas, nsamps = nsamps)
  bMAP_MAT <- get_mcmc_samples("betaMAP_MAT", betas = betas, nsamps = nsamps)
  
  bSDI <- get_mcmc_samples("betaSDIscaled", betas = betas, nsamps = nsamps)
  bSDI_ppt <- get_mcmc_samples("betawateryrscaled_SDIscaled", betas = betas, nsamps = nsamps)
  bSDI_tmax <- get_mcmc_samples("betatmaxAprMayJunscaled_SDIscaled", betas = betas, nsamps = nsamps)
  
  
  
  #MAP interactions:
  bMAP_ppt <- get_mcmc_samples("betaMAP_wateryrscaled", betas = betas, nsamps = nsamps)
  bMAP_tmax <- get_mcmc_samples("betaMAP_tmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  bMAP_SDI <- get_mcmc_samples("betaMAP_SDIscaled", betas = betas, nsamps = nsamps)
  
  #MAT interactions:
  bMAT_ppt <- get_mcmc_samples("betaMAT_wateryrscaled", betas = betas, nsamps = nsamps)
  bMAT_tmax <- get_mcmc_samples("betaMAT_tmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  bMAT_SDI <- get_mcmc_samples("betaMAT_SDIscaled", betas = betas, nsamps = nsamps)
  
  
  bX <-  get_mcmc_samples("betaX", betas = betas, nsamps = nsamps)
  
  
  bppt <- get_mcmc_samples("betawateryrscaled", betas = betas, nsamps = nsamps)
  btmax <- get_mcmc_samples("betatmaxAprMayJunscaled", betas = betas, nsamps = nsamps)
  btmax_ppt <- get_mcmc_samples("betatmaxAprMayJunscaled_wateryrscaled", betas = betas, nsamps = nsamps)
  
  
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
  
  
  
 
  # get the scaled SDI for the PLT:
  
 # not going beyond 2018 for now 
  # cat("extracting future climate for the plot")
  # 
  # if(scenario %in% "rcp26"){
  #   clim.fut.scen <- future.clim.subset.26 
  # }
  # if(scenario %in% "rcp45"){
  #   clim.fut.scen <- future.clim.subset.45 
  # }
  # if(scenario %in% "rcp60"){
  #   clim.fut.scen <- future.clim.subset.60 
  # }
  # if(scenario %in% "rcp85"){
  #   clim.fut.scen <- future.clim.subset.85 
  # }
  # 
  # scale.fut.clim.by.plt <- function(x, future.clim.subset){
  #   cat(x)
  #   full.clim.plt <-  future.clim.subset %>% filter(PLT_CN == x)#full.clim.dt[PLT_CN %in% plot]
  #   rowid <- which(cov.data.regional$PLT_CN %in%  x ) # get the row for the climate data
  #   full.clim.plt$ppt.scale <- ( full.clim.plt$ppt.corrected-mean(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE))/sd(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE)
  #   full.clim.plt$tmax.scale <- ( full.clim.plt$tmax.corrected-mean(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE))/sd(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE)
  #   full.clim.plt
  # }
  # 
  # fut.clim.scen <- scale.fut.clim.by.plt(x = PLT_CNint, future.clim.subset = clim.fut.scen)
  # 
  # if(length(unique(fut.clim.scen$year)) < 80){

  
    
  #if(nrow(fut.clim.scen)==0){
   # cat("no matching climate data")
  #}else{
   # if(length(unique(fut.clim.scen$year)) < 80){
    #  cat("less than 80 years of future data")
    #}else{
      #scenario <- "rcp26"
      
      #fut.clim.scen <- fut.clim.plot %>% filter(rcp %in% scenario)
      
      # models <- unique(fut.clim.scen$model) # 21 models
      # sample.model <- sample(models, size = length(models), replace= FALSE)
      # 
      # 
      # 
      # get.ens.df <- function(i){
      #   
      #   ens.proj.yr <- fut.clim.scen %>% filter(model %in% sample.model[i])
      #   ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
      #   
      #   
      #   df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
      #                    tmax = ens.proj.yr$tmax.scale, 
      #                    model = i, 
      #                    year = ens.proj.yr$year)
      #   df
      # }
      # 
      # 
      # ens.samps <- lapply(1:length(models), get.ens.df)
      # ens.samps.df <- do.call(rbind, ens.samps)
      # ens.means <- ens.samps.df #%>% group_by(year, i) #%>% summarise(ppt.mean = mean(ppt, na.rm =TRUE), 
      # #             tmax.mean = mean(tmax, na.rm = TRUE))
      # 
      #ppt.fut <- ens.means %>% group_by(year,i) %>% dplyr::select(year,i, ppt)  %>% tidyr::spread(key = year, value = ppt)%>% dplyr::select(`2019`:`2098`)
      #tmax.fut <- ens.means %>% dplyr::select(year, i, tmax) %>% tidyr::spread(key = year, value = tmax)%>% dplyr::select(`2019`:`2098`)
      
      ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
      
      tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`2001`:`2018`)
      full.df <- data.frame(ppt = as.numeric(ppt.hist), 
                                  tmax = as.numeric(tmax.hist), 
                                  #model = rep(1:length(unique(ens.means$model)), each = length(as.numeric(ppt.hist))), 
                                  year = 2001:2018)
      
      #rbind(hist.samps.df, ens.samps.df %>% filter(!year %in% 2018))
      full.df.nodups <- full.df[!duplicated(full.df),]
      #full.df.nodups$rowid <- 1:length(full.df.nodups$ppt)
      full.ens.ppt <- full.df.nodups  %>% dplyr::select( ppt, year)%>% group_by( year)%>% 
        summarise(ppt.m = mean(ppt, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(ppt.m, year) %>%group_by(year)%>%
        spread(year, value = ppt.m, drop = FALSE)%>% ungroup () 
      
      full.ens.tmax <- full.df.nodups  %>% dplyr::select(tmax, year)%>%  group_by(year)%>% 
        summarise(tmax.m = mean(tmax, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(tmax.m, year) %>%group_by(year)%>%
        spread(year, value = tmax.m, drop = FALSE)%>% ungroup () 
      
      #ppt <- cbind(ppt.hist, ppt.fut)
      #tmax <- cbind(tmax.hist, tmax.fut)
      SDI <- SDI.PLT.SCALED %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint ) %>% dplyr::select(SUBP,`2001`)
      
      cov.mat <- unique(x.mat %>% filter(PLT_CN %in% plot) %>% dplyr::select(PLT_CN, MAP, MAT, MAP.scaled, MAT.scaled))
      
      MAP <- cov.mat$MAP.scaled
      MAT <- cov.mat$MAT.scaled
      
      #if(length(SDI) <=36){
      # newsdivals <- rep(SDI[36], 17)
      #  names(newsdivals)  <- 2002:2018
      #  SDI.new <- cbind(SDI, newsdivals)
      #}
      #SICOND <- cov.data[m, ]$SICOND
      #}
      
      covariates <- list()
      covariates$SDI <- as.matrix(SDI)
      covariates$ppt <- as.matrix(full.ens.ppt)
      covariates$tmax <- as.matrix(full.ens.tmax) 
      covariates$MAP <- MAP
      covariates$MAT <- MAT
      
      
      time_steps <- length(2001:2018)
      nsamps <- max(length(betas.all$bSDI), length(x.mat[m,1]))
      forecast <- matrix(data = NA, nrow = nsamps, ncol = time_steps)
      inc <- matrix(data = NA, nrow = nsamps, ncol = time_steps)
      
      # set up sdi matrix
      sdi.subp <-  matrix(data = NA, nrow = nrow(subplots), ncol = nt + 2)
      
      sdi.subp[,1:2] <- covariates$SDI
      
      # need to fix the SDI scaling function to make it global scaling....
      
      # function to calculate SDI from the diameters:
      calc.sdi.subp <- function(x, j = PLT_CNint, a = s, TPAcalc){
        
        #SDI.mat.PLT <-  SDI.PLT %>% filter(PLT_CN %in% PLT_CNint)%>% ungroup() %>% select(`1966`:`2001`)
        subplot.sel<- SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`2001`:`2018`)
        
        if(nrow(subplot.sel)>0 ){ 
          if(!subplot.sel$`2001` == 0){# have one instance where we are missing subplot info
            SDI.mat.PLT.subp.sel <-  SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`2001`:`2018`)
          }}else{
            SDI.mat.PLT.subp.sel <-  colMeans(SDI.PLT %>% filter(PLT_CN %in% j )%>% ungroup() %>% dplyr::select(`2001`:`2018`))
            
          }
        # if only one tree is on the plot
        if(is.null(dim(x))){
          avg.dbh <- mean(x,na.rm=TRUE)
        }else{
          avg.dbh <- apply(x, 1, mean, na.rm = TRUE) # get mean of the MCMCs for each tree
        }
        # note that any dead trees will have dbh == 0, which won't add to the sum of trees in the following line
        
        SDI.new <- sum(TPAcalc*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI, convert to inches
        
        
        #SDI.mat.PLT.subp
        
        SDIscaled <-   (SDI.new - mean(as.matrix(SDI.mat.PLT.subp[,3:20]),na.rm=TRUE))/sd(as.matrix(SDI.mat.PLT.subp[,3:20]),na.rm=TRUE)
        SDIscaled
      }
      
      # calculation of raw SDI
      rescale.sdi.raw <- function(SDIscaled, j = PLT_CNint, a = SUBPPLT_CNint){
        
        subplot.sel <- SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`2001`:`2018`)
        
        if(nrow(subplot.sel)>0 ){
          if(!subplot.sel$`2001` == 0){ # have one instance where we are missing subplot info
            SDI.mat.PLT.subp.sel <-  SDI.PLT %>% filter(PLT_CN %in% j & SUBP %in% a)%>% ungroup() %>% dplyr::select(`2001`:`2018`)
          } }else{
            SDI.mat.PLT.subp.sel <-  colMeans(SDI.PLT %>% filter(PLT_CN %in% j )%>% ungroup() %>% dplyr::select(`2001`:`2018`))
            
          }
        
        
        #SDI.new <- sum(6.01*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI
        SDI.raw <-   mean(as.numeric(SDI.mat.PLT.subp.sel), na.rm = TRUE)*sd(as.numeric(SDI.mat.PLT.subp.sel), na.rm = TRUE)+SDIscaled
        SDI.raw
      }
      # need to reindex the data to have a a timestep (t), subplot (s), and tree (i)
      
      
      nsubp <- unique(index.df$SUBP)
  
      
      # left off here, but probably need to fix indexing since we are just starting with 1 dbh measurement
      
      # for each tree get the next statespace time step:
      for(t in 1:nt){ # for each year t in the # of trees
        
        #if(t == 1){
        mort.code <- 0
        # loop through all of the trees for year t
        for (i in 1:ni){ # for each tree i in the n of trees
          
          SUBPLOT.index <- index.df[ni,]$SUBP # select the subplot for the tree:
          
          if(mean(dbh.pred[i,,t]) == 0){ # if the tree was killed off dbh.pred will be zero, so assign as zero
            dbh.pred[i,,t+1] <- dbh.pred[i,,t]
            #dbh.dead[i,,3+t+1] <- dbh.dead[i,,3+t]
            
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
          
          # if after 1st years of forecast the median increment <0 for 3 years in a row, kill off the tree:
          if(t >= 2){
            
            # default is that mort.prob == 0 (i.e. no mortality)
            
            mort.prob <- 0
            
            
            if(density.independent == TRUE){
              
              tindex <- ifelse(t >=6, t-5, 
                               ifelse(t == 5, t-4, 
                                      ifelse(t ==4, t-3, 
                                             ifelse(t==3, t-2, 
                                                    ifelse(t == 2, t-1, t)))))
              
              zero.means <-  colMeans(increment[i,,(tindex):(t)], na.rm = TRUE) <= 0 
              zero.df <- ifelse(zero.means == FALSE, 0, 1)
              mort.prob <- mean(zero.df, na.rm =TRUE)
            }
            
            mort.code <- rbinom(1,1, prob = mort.prob)
          }
          
          if(mort.code == 1 & is.na(dbh.dead[i,,t]) & dbh.pred[i,,t]>0){
            #dbh.dead[i,,(3+t+1):(3+nt)] <- dbh.pred[i,,3+t] # set dead diameter to the last live estimated diameter for the tree
            #dbh.pred[i,,(3+t+1):(3+nt)] <- 0 # set live diameter to zero
            TPAmort[i,,(t+1)] <- TPAmort[i,,t]-1 # if the three has a high probability of dying, reduce TPA by 1
            
          }else{
            TPAmort[i,,(t+1)] <- TPAmort[i,,(t)]
          }
          
          if(TPAmort[i,,t+1] <= 0){
            TPAmort[i,,t+1] <- 0
            dbh.pred[i,,t+1] <- 0
          }
          if(TPAmort[i,,t+1] > TPAmort[i,,t]){
            cat(paste0("TPA at time ", t+1, "is greater than at time ",t, "for tree ", i))
            break
          }
        }
        
        # use the current SDI to determine density dependnt mortality at the stand scale, then distribute to trees:
        # my attempt to reflect what FVS is doing
        
        
        if(density.dependent == TRUE){
          
          for(s in sdi.subp[,1]){ 
            # for each subplot s in the # of subplots
            # need to index dbh.pred trees by by the subplot:
            
            trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
            if(length(trees.subplot)<=1){
              TPAcalc <- mean(TPAmort[trees.subplot,,(t+1)])
            }else{
              TPAcalc <- rowMeans(TPAmort[trees.subplot,,(t+1)])
            }
            sdi.temp <- calc.sdi.subp(x = dbh.pred[trees.subplot,,t], j = unique(as.character(trees.in.plt$PLT_CN)), a = s, TPAcalc = TPAcalc)
            SDI.raw <- rescale.sdi.raw(SDIscaled =  sdi.temp, j = unique(as.character(trees.in.plt$PLT_CN)), a = s)
            
            
            
            # if the stand is above 70% of SDI and mort probability is not already 1, increase mortality probability
            # this should increase mortality liklihood across all the trees, but mostly larger trees
            if( SDI.raw > (450/nrow(sdi.subp))*SDI.ratio.DD & length(trees.subplot) > 1){
              
              # loosly basing on FVS mortality:
              # calculate an RI for each tree:
              p0 <- 5.5877
              p1 <- 0.005348
              
              
              #rownames(dbh.pred[,,t]) <- 1:nrow(dbh.pred[,,t])
              
              DBH <- dbh.pred[trees.subplot,,t]/2.54
              #combined[trees.subplot,]
              rownames(DBH) <- 1:nrow(DBH)
              
              
              live.trees <- rowSums(DBH) > 0
              if(all(live.trees)==FALSE){
                cat("all trees are dead")
              }else{
                
                DBH.live  <- DBH[live.trees,]
                RI <- (1/(1+exp(p0 + p1*DBH.live)))*0.5
                Y <- 1
                RIP <- 1-(1-RI)^Y # when Y == 1, RI and RIP are equal
                Mort.rate <- mean(colSums(RIP)) # total background mortality for the plot
                
                BA <- pi*(DBH.live/2)^2
                PCT <- apply(BA, 2, percent_rank)
                MR <- (0.84525-(0.01074*PCT)+(0.0000002*PCT^3))
                
                MWT <- 0.85 # for #PP, in UTAH FVS variant documents
                MORT <- MR * MWT * 0.1
                
                #n.ded <- round(Mort.rate*nrow(DBH.live)) # number of trees to kill
                #n.ded <-1
                #MR <- ifelse(MR <0, 0, MR)
                
                avg.mort.prob <- rowMeans(MR)
                avg.MORT <- rowMeans(MORT) # note to include uncertainty in the mortality estimates, use the distributions
                
                # need the avg.MORT to sum to the mort.rate
                split.mort <- Mort.rate/length(live.trees)#avg.MORT
                
                mort.per.tree <- Mort.rate/avg.mort.prob
                # need to index by both the subplot == s & by the live trees
                # calculate a TPA that calucates the mort each year...probably needs to be the size of DBH and increment...
                # Also need to use this TPA to calculate SDI as we move forward...
                
                TPAmort[trees.subplot[live.trees],,t+1] <- TPAmort[trees.subplot[live.trees],,(t)]-(TPAmort[trees.subplot[live.trees],,(t)]*mort.per.tree)
                
                
                if(TPAmort[trees.subplot[live.trees],,t+1] > TPAmort[trees.subplot[live.trees],,t]){
                  cat(paste0("TPA at time ", t+1, "is greater than TPA at time ", t, "for subplot", s, "density dependant"))
                  break
                }
                #dead.index <-  which(order(avg.mort.prob) %in% 1:n.ded) # gives the tree # (s) that should die
                
                #dd.treenos <- as.numeric(rownames(DBH.live)[dead.index])
                
                #rm(dead.index)
                
                # if(n.ded > 0 ){
                #   
                #  
                #   dbh.dead[trees.subplot[dd.treenos],,(3+t):(3+nt)] <- dbh.pred[trees.subplot[dd.treenos],,3+t] # set dead diameter to the last live estimated diameter for the tree
                #   
                #   dbh.pred[trees.subplot[dd.treenos],,(3+t+1):(3+nt)] <- 0 # set live diameter to zero
                #   #dbh.dead[dd.treenos,,t:nt] <- dbh.pred[dd.treenos,,t:nt] # set dead diameter to the last live estimated diameter for the tree
                # } 
              }
              #}
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
          
          TPAmort[TPAmort[,1,t+1]<= 0, ,t+1] <- 0
        }
        # before moving onto the next year, calculate the SDI
        # calculate SDI by the subplot
        for(s in sdi.subp[,1]){ # for each subplot s in the # of subplots
          # need to index dbh.pred trees by by the subplot:
          trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
          if(length(trees.subplot)<=1){
            TPAcalc <- mean(TPAmort[trees.subplot,,(t+1)])
          }else{
            TPAcalc <- rowMeans(TPAmort[trees.subplot,,(t+1)])
          }
          sdi.subp[which(sdi.subp[,1]==s),t+2] <- calc.sdi.subp(x = dbh.pred[trees.subplot,,t+1], j = unique(as.character(trees.in.plt$PLT_CN)), a = s, TPAcalc = TPAcalc)
        }
      }
      
      
      # not sure what is happening with the TPA reducing and then going up again...]
      
      
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
      saveRDS(dbh.pred, paste0("diam_forecastsFIAannual/PLT.dbh.",mort.scheme,".", plot,".", scenario,".", SDI.ratio.DD,".2001.2018.RDS"))
      saveRDS(dbh.dead, paste0("diam_forecastsFIAannual/PLT.dbh.dead.",mort.scheme,".", plot,".", scenario,".", SDI.ratio.DD,".2001.2018.RDS"))
      saveRDS(TPAmort, paste0("diam_forecastsFIAannual/PLT.tpamort.",mort.scheme,".", plot,".", scenario,".", SDI.ratio.DD,".2001.2018.RDS"))
      
      saveRDS(sdi.subp, paste0("diam_forecastsFIAannual/PLT.sdi.",mort.scheme,".", plot,".", scenario,".", SDI.ratio.DD,".SUBP.2001.2018.RDS"))
      saveRDS(index.df, paste0("diam_forecastsFIAannual/PLT.dbh.combined",mort.scheme,".", plot,".", scenario,".", SDI.ratio.DD,".2001.2018.RDS"))
      # make a plot of all the DBH forecasts:
      
      dbh.quants <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
      colnames(dbh.quants) <- c("quantile","treeno","time", "diameter")
      
      
      dbh.quants.spread <- dbh.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
      # dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
      # colnames(dbh.means) <- c("treeno","time", "diameter")
      index.df$treeno <- 1:length(index.df$CN)
      
      dbh.means.index <- left_join(dbh.quants.spread , index.df, by = "treeno")
      
      p <- ggplot()+geom_line(data = dbh.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) + 
        geom_ribbon(data = dbh.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
        theme_bw() + ylab("Diameters (cm)")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
      
      # TPA mortality
      TPA.quants <- reshape2::melt(apply(TPAmort, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
      colnames(TPA.quants) <- c("treeno","time", "TPA")
      
      
      # tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
      # dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
      # colnames(dbh.means) <- c("treeno","time", "diameter")
      index.df$treeno <- 1:length(index.df$CN)
      
      tpa.means.index <- left_join(TPA.quants, index.df, by = "treeno")
      
      
      ggplot()+geom_line(data = tpa.means.index, aes(x = time, y = TPA, group = treeno, color = as.character(treeno))) 
      
      # join with the diameter 
      dbh.means.TPA.index<- left_join(tpa.means.index[,c("treeno", "time", "TPA")], dbh.means.index, by = c("treeno", "time"))
      dbh.means.TPA.index$med.scaled.DBH <- dbh.means.TPA.index$`50%`*dbh.means.TPA.index$TPA
      dbh.means.TPA.index$hi.scaled.DBH <- dbh.means.TPA.index$`97.5%`*dbh.means.TPA.index$TPA
      dbh.means.TPA.index$lo.scaled.DBH <- dbh.means.TPA.index$`2.5%`*dbh.means.TPA.index$TPA
      
      p <- ggplot()+geom_line(data = dbh.means.TPA.index, aes(x = time, y = med.scaled.DBH, color = as.character(SUBP), group = treeno)) + 
        geom_ribbon(data = dbh.means.TPA.index, aes(x = time, ymin = lo.scaled.DBH, ymax = hi.scaled.DBH,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
        theme_bw() + ylab("Diameters (cm)*TPA")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
      p
      #ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.dbhs.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p)
      
      # plot dead trees to see how they are behaving:
      
      # the dead trees with the TPA based mortality are the same diams as the dbh.pred
      # dead trees should be the # of tres in tpa.diff * out
      # dead.quants <- reshape2::melt(apply(dbh.dead, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
      # colnames(dead.quants) <- c("quantile","treeno","time", "diameter")
      # 
      # 
      # dead.quants.spread <- dead.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
      # # dead.means <- reshape2::melt(apply(dead.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
      # # colnames(dead.means) <- c("treeno","time", "diameter")
      # index.df$treeno <- 1:length(index.df$treeid)
      # 
      # dead.means.index <- left_join(dead.quants.spread , index.df, by = "treeno")
      # 
      # ded <- ggplot()+geom_line(data = dead.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) + 
      #   geom_ribbon(data = dead.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
      #   theme_bw() + ylab("Diameters (cm)")+xlab("years after 2018") + ggtitle(paste0("Diameter of dead for plot ", plot))
      # 
      # ded
      # 
      # make a plot of all the increment forecasts:
      
      inc.quants <- reshape2::melt(apply(increment, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
      colnames(inc.quants) <- c("quantile","treeno","time", "increment")
      
      
      inc.quants.spread <- inc.quants %>% group_by(treeno, time) %>% spread(quantile, increment)
      # inc.means <- reshape2::melt(apply(inc.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
      # colnames(inc.means) <- c("treeno","time", "diameter")
      index.df$treeno <- 1:length(index.df$CN)
      
      inc.means.index <- left_join(inc.quants.spread , index.df, by = "treeno")
      
      p.inc <- ggplot() + 
        geom_ribbon(data = inc.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.25)+
        geom_line(data = inc.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno))+
        theme_bw() + ylab("Increments (cm)")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))+ labs(fill = "Subplot Number", color = "Subplot Number") 
      
      #ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.increment.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p.inc)
      
      #p.inc
      
      
      combined <- index.df
      #out.dbh <- as.data.frame(dbh.pred)
      
      #dbh.mat <- matrix(dbh.pred, nrow = dim(dbh.pred)[2], ncol = prod(dim(dbh.pred)[1],dim(dbh.pred)[3]), byrow = TRUE)
      # dim(dbh.mat)
      # summary((dbh.mat[,1:10 ]))
      # out <- dbh.mat
      # 
      # plot(out[1,])
      
      # var 1 is tree, var2 is mcmc sample, and var 3 is time
      test.m <- melt(dbh.pred, id.vars = dim(dbh.pred)[2])
      test.m$id <- paste0("x[",test.m$Var1, ",", test.m$Var3,"]")
      #test.m <- test.m%>% filter(Var3 %in% 4:102)
      colid.ordered <- test.m$id
      
      test.m.time <- test.m %>% dplyr::select(-Var1, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
      out <- test.m.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(all_of(colid.ordered))
      #out <- out[1:50,]
      out.mean <- apply(out, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})
      
      
      
      # get a TPA out:
      tpa.m <- melt(TPAmort, id.vars = dim(TPAmort)[2])
      #tpa.m$newVar3 <- rep(4:102, each = 100*ni)
      tpa.m$id <- paste0("x[",tpa.m$Var1, ",", tpa.m$Var3,"]")
      
      TPA.ordered <- tpa.m$id
      
      tpa.m.time <- tpa.m %>% dplyr::select(-Var1, -Var3, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
      out.tpa <- tpa.m.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(all_of(TPA.ordered))
      #out <- out[1:50,]
      tpa.mean <- apply(out.tpa, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})
      
      
      # calculate an out for the dead trees
      
      # need to redo how we are determingin dead vs living with FVS mortality...
      # the TPA adjustment also needs to be applied at the tree scale....
      # The dead trees will are the original TPA-current TPA multiplied by the out
      # tpa.diff = (original tpa - current year tpa)*out for that year
      tpa.diff.df <- tpa.m %>% group_by( Var1, Var2) %>% mutate(firstTPA = value[1]) %>%
        ungroup()%>% group_by(Var1, Var2) %>% mutate(TPAdiff = firstTPA-value)
      
      summary(tpa.diff.df$TPAdiff)
      
      #ggplot(data = tpa.diff.df,aes(x = newVar3, y = TPAdiff, group =as.character(Var1) , color = as.character(Var1)))+geom_line()
      
      #tpa.diff.df
      tpa.diff.time <-  tpa.diff.df %>% ungroup()%>% dplyr::select(-Var1, -Var3, -value) %>% group_by(Var2) %>% spread(key = id, value = TPAdiff)
      out.tpa.diff <- tpa.diff.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(TPA.ordered)
      #out <- out[1:50,]
      tpa.diff.mean <- apply(out.tpa.diff, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})

      # 
      # out.dead <- out.mean*tpa.diff.mean
      
      
      # dead.m <- melt(dbh.dead, id.vars = dim(dbh.dead)[2])
      # dead.m$newVar3 <- rep(4:102, each = 100*ni)
      # tpa.m$id <- paste0("x[",tpa.m$Var1, ",", tpa.m$newVar3,"]")
      # 
      # TPA.ordered <- tpa.m$id
      # test.m.time.dead <- test.m %>% dplyr::select(-Var1, -Var3) %>% group_by(Var2) %>%  %>% spread(key = id, value = value)
      # out.dead <- test.m.time.dead %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(colid.ordered)
      # # #out <- out[1:50,]
      # out.mean.dead <- apply(out.dead, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})
      # 
      
      
      # biomass estimation
      
      cat("start biomass estimates")
      plot2AGB(combined = combined, out = out.mean, tpa = tpa.mean, tpa.diff = tpa.diff.mean, mort.scheme = mort.scheme, allom.stats = kaye_pipo, unit.conv = 0, plot = plot, yrvec = 2001:2018, scenario = scenario, p = p, p.inc = p.inc, SDI.ratio.DD = SDI.ratio.DD)
  }   
    }  
  

