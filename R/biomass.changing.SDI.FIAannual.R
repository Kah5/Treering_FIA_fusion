biomass.sensitivity.annual <- function(plt.num, # = plot, 
                                         density.dependent = TRUE, 
                                         density.independent = TRUE, 
                                         scenario = "rcp26", 
                                         SDI.ratio.DD = 0.7, 
                                         aggressiveCC = FALSE, 
                                         scale.mort.prob = 1, 
                                         cov.data.regional.df, # = cov.data.regional, 
                                         TREE.FIA, # = TREE, 
                                         ci.names.df, # = ci.names, 
                                         ci.names.noncored.df, # = ci.names.noncored, 
                                         mean.pred.cored.df, # = mean.pred.cored,
                                         #xmat2 = xmat2, 
                                         SDIscaled.matrix, # = SDIscaled,
                                         time_data_list # = time_data
){
  
  
  source("R/plot2AGB_kayeFVS.R")
  source("R/get_objects.R")
  source("R/generate_forecast.R")
  #TPA_lookup <- TREE.FIA %>% filter(PLT_CN %in% plot) %>% dplyr::select(TPA_UNADJ, TPAMORT_UNADJ, PLOT, SUBP, TREE, MEASYR, DESIGNCD)
  # -------- get the diameter estimates for all trees on the plot: ------------------------
  #print(as.character(plt.num))
  
  print(as.character(plot))
  # get id of trees with cores:
  # cored.in.plt <- cov.data.regional %>% dplyr::filter (PLT_CN %in% plot)
  # cored.in.plt <- cored.in.plt[!duplicated(cored.in.plt$TRE_CN),]
  # cored.in.plt$TPA_UNADJ <- TREE[which(TREE$CN %in% unique(cored.in.plt$TRE_CN)),]$TPA_UNADJ
  # 
  # get id of trees with out cores:
  trees.in.plt <- TREE %>% dplyr::filter (PLT_CN %in% plot & STATUSCD == 1)
  trees.in.plt$TPA_UNADJ <- TREE[which(TREE$CN %in% trees.in.plt$CN),]$TPA_UNADJ
  
  trees.in.plt$DIA_cm_T2 <- TREE[which(TREE$PREV_TRE_CN %in% trees.in.plt$CN),]$DIA*2.54
  trees.in.plt$MEASYEAR_T2 <- TREE[which(TREE$PREV_TRE_CN %in% trees.in.plt$CN),]$PREV_MEASYR
  # if there are no other trees on the plot just don't run the forecats
  if(length(trees.in.plt$PLOT) == 0){
    ##print("no other trees on plot")
  }else{
    
    
    PLT_CNint <- as.character(plot)
    
    # get the unique SUBPLOTS in the plot
    
    SDI.PLT <- SDI.mat.PLT.subp %>% filter(PLT_CN %in% PLT_CNint & !is.na(`2001`))
    
    
    SDI.PLT.SCALED <- SDI.PLT # get the SDI values
    subplots <- unique(SDI.PLT.SCALED %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(SUBP))
    
    
    # get the subplot information & TPA information
    trees.in.plt <- left_join( trees.in.plt, subplots)
    unique(trees.in.plt$DIA)
    #if(!length(trees.in.plt$PLOT) == 0){
    #cored.treeid <- cored.in.plt$treeid
    trees.in.plt <- trees.in.plt[!is.na(trees.in.plt$DIA),]
    #trees.in.plt$DIA
    
    y <- trees.in.plt$CN
    
    #y
    # get the subplot information & TPA information
    combined <- trees.in.plt %>% select(CN, SUBP, TPA_UNADJ, TREE, DIA)
    #y <- y # just to reconcile the updated data with current
    #m <- x[1] # since all cored trees will have the same plot information this is okay
    
    # generate some uncertainty around the measurement of the first diameter for all trees
    
    #out.noncored <- spread.dbh.mat %>% filter(CN %in% combined$CN)
    #out.noncored.yr <- names(which(!is.na(colSums(out.noncored[,4:length(out.noncored)]))))
    #sigma.DBH
    out.FIA<- list()  
    for(i in 1:length(trees.in.plt$CN)){
      out.FIA[[i]] <- rnorm(100, mean = trees.in.plt[i,"DIA"]*2.54, sd = sigma.DBH$median)
    }
    out.FIA.df <- do.call(cbind, out.FIA)
    
    trees.in.plt$treeid <- 1:length(trees.in.plt$CN)
    index.df <-  trees.in.plt
    all.dbh <-   out.FIA.df
    colnames(all.dbh) <- paste0("x[",trees.in.plt$CN, ",", trees.in.plt$MEASYR,"]")
    
    # make a big array with all the DBH estimates:
    
    ni <- nrow(index.df)# number of individuals per plot
    
    
    nsamps <- length(out.FIA.df [,1])
    nt <- length(2001:2018)
    ntfull <- length(2001:2018)
    
  
    # also need to reorder the columns
    # combined all diameter estimates
    #all.dbh <- cbind(out.cored.plt, out.noncored.plt)
    
    all.dbh.ids <- separate(reshape2::melt(colnames(all.dbh)), col =value, into = c("x[", "tree", "year", "]"))
    all.dbh.ids$col.id <- 1:length(all.dbh.ids$tree)
    #yr.2001.ids <- as.vector(unlist(all.dbh.ids %>% filter(year == 36) %>% dplyr::select(col.id)))
    
    #all.dbh <- all.dbh[,yr.2001.ids]
    # get plot subplot and cored status for each tree:
    trees.in.plt$type <- "noncored"
    
    index.df <- trees.in.plt.subp
    #index.df$CN <- index.df$treeid
    
    all.dbh.means <- colMeans(all.dbh) %>% reshape2::melt(.)
    
    all.dbh.means$CN <- separate(reshape2::melt(rownames(all.dbh.means)), col =value, into = c("x[", "tree", "year", "]"))[,"tree"]
    all.dbh.means$DBH_cm <- all.dbh.means$value
    all.dbh.means$DBH_in <- all.dbh.means$value/2.54
    # get the design codes
    # get the updated TPA factors for trees in variable radius plots
    DESIGNCD.plt <- unique(TREE.FIA[which(TREE.FIA$CN %in% trees.in.plt$CN),]$DESIGNCD)
   
    
    TPA.designcd.table <- data.frame(DESIGNCD = c(1, 410, 411, 413, 424, 425, 423, 412), #unique(TREEinPLOTS$DESIGNCD),
                                     RADIUS = c("24 ft", 
                                                "Variable", 
                                                "Variable", 
                                                "Variable", 
                                                "Fixed", 
                                                "Fixed", 
                                                "Fixed",
                                                "Variable" ), 
                                     BAF = c(NA, 
                                             40, 
                                             40, 
                                             20,
                                             NA, 
                                             NA, 
                                             NA,
                                             40), 
                                     N.subplots.points = c(4, 
                                                           7,#  maybe 5?
                                                           10, 
                                                           5, 
                                                           4, 
                                                           4, 
                                                           4, 
                                                           5), 
                                     N.microplots = c(4, 
                                                      7, 
                                                      3, 
                                                      5, 
                                                      4,
                                                      4,
                                                      4, 
                                                      3),
                                     MicropplotRadius = c("6.8 ft", 
                                                          "1/300th acre",
                                                          "1/300th acre", 
                                                          "1/300th acre", 
                                                          "1/300th acre", 
                                                          "1/300th acre", 
                                                          "1/300th acre",
                                                          "1/300th acre"), 
                                     TPA.eq = c("1/(N*A)", 
                                                "(BAF/0.005454*DIA^2)/N", 
                                                "(BAF/0.005454*DIA^2)/N",
                                                "(BAF/0.005454*DIA^2)/N",
                                                "1/(N*A)",
                                                "1/(N*A)",
                                                "1/(N*A)",
                                                "(BAF/0.005454*DIA^2)/N"), 
                                     AREA.acr = c(0.0415172, 
                                                  NA, 
                                                  NA, 
                                                  NA, 
                                                  0.05,
                                                  0.2,
                                                  0.1,
                                                  NA), 
                                     microplot.AREA.acr = c(0.003334877, 
                                                            NA, 
                                                            NA, 
                                                            NA, 
                                                            0.003333333, 
                                                            0.003333333, 
                                                            0.003333333, 
                                                            NA) )
    
    
    DESIGNCD.table <- TPA.designcd.table %>% filter(DESIGNCD == unique(DESIGNCD.plt))
    
    calcTPA_unadj <- function(DBH_in, DESIGN.tb = DESIGNCD.table){
      if(DESIGN.tb$DESIGNCD %in% c(1, 423, 424, 425)){
        
        # all fixed area plots, but with different # of microplots
        if(DBH_in > 5){ # for macroplot
          PROB = 1/(DESIGN.tb$N.subplots.points*DESIGN.tb$AREA.acr)
        }else{ # for microplot
          PROB = 1/(DESIGN.tb$N.microplots*DESIGN.tb$microplot.AREA.acr)
        }
      }else{
        
        
        if(DBH_in > 5){
          PROB = DESIGN.tb$BAF/(0.005454*DBH_in^2)/(DESIGN.tb$N.subplots.points)
        }else{ # assume the tree is on the microplot
          PROB = 1/(DESIGN.tb$N.microplots*(1/300))
        }
      }
      PROB
    }
    
    
    PROB <- do.call(rbind, lapply(all.dbh.means$DBH_in, function(x){calcTPA_unadj(DBH_in = x, DESIGN.tb = DESIGNCD.table)}))
    
    index.df$TPA_UNADJ <- round(PROB, digits = 3)
    index.df$DBH_in <- all.dbh.means$DBH_in
    
    # make a big array with all the DBH estimates:
    
    ni <- nrow(index.df)# number of individuals per plot
    
    
    nsamps <- length(out.FIA.df [,1])
    nt <- length(2001:2018)
    ntfull <- length(2001:2018)
    # fill in the array with the diameter estimates for 2018:
    dbh.pred <- increment <- SDI.tracking <-TPAmort <- TPAlive <- TPAsize <- TPADD <- TPADI <- mort.prob.reduced<- array(NA, dim = c(ni, nsamps, ntfull + 1))
    # dbh.dead:
    dbh.dead <- dbh.pred
    
    
    # set up empty matrices for dbh, increment, TPA projections
    id.ni <- rep(1:ni, each = 1)
    
    
    
    #---------------------------------------------------------------------------
    ##  get all the paramter estimates + uncertainty
    #---------------------------------------------------------------------------
    ##print("set up the beta values")
    nsamps1 <- nsamps-1 # need to subtract for the DBH mcmcs and parameter mcmcs used to make the forecast matches
    
    
    
    # write a function to get the MCMC samples
    
    get_mcmc_samples <- function(x, betas, nsamps){
      
      rnorm(nsamps, mean = as.numeric(betas %>% filter(L1 %in% x) %>%dplyr::select(median)), sd =  as.numeric(betas %>% filter(L1 %in% x) %>%dplyr::select(sd)))
    }
    
   
    
      alpha <- get_mcmc_samples("mutree", betas = mus, nsamps = nsamps)
      
    
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
    
    
    betas.all <- data.frame(  alpha ,
                             # alphas.trees,
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
    
    
    ##print ("set up SDI")
    # get PLT_CN
    PLT_CNint <- as.character(plt.num)
    
    
    # leaving off here July 2nd
    # get the scaled SDI for the PLT:
    
    # get the unique SUBPLOTS in the plot
    subplots <- unique(SDIscaled.matrix %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(SUBP))
    
    SDI.PLT <- SDIscaled.matrix %>% filter(PLT_CN %in% PLT_CNint)
    
    SDI.PLT.SCALED <- SDI.PLT # get the SDI values
    
    
    ##print("extracting future climate for the plot")
    
    if(scenario %in% "rcp26"){
      clim.fut.scen <- future.clim.subset.26 
    }
    if(scenario %in% "rcp45"){
      clim.fut.scen <- future.clim.subset.45 
    }
    if(scenario %in% "rcp60"){
      clim.fut.scen <- future.clim.subset.60 
    }
    if(scenario %in% "rcp85"){
      clim.fut.scen <- future.clim.subset.85 
    }
    
    
    scale.fut.clim.by.plt <- function(x, future.clim.subset){
      #print(x)
      full.clim.plt <-  future.clim.subset %>% filter(PLT_CN == x)#full.clim.dt[PLT_CN %in% plot]
      rowid <- which(cov.data.regional.df$PLT_CN %in%  x ) # get the row for the climate data
      full.clim.plt$ppt.scale <- ( full.clim.plt$ppt.corrected-mean(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE))/sd(as.matrix(clim.data$wintP.wateryr[rowid,]), na.rm = TRUE)
      full.clim.plt$tmax.scale <- ( full.clim.plt$tmax.corrected-mean(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE))/sd(as.matrix(clim.data$tmaxAprMayJun[rowid,]), na.rm =TRUE)
      full.clim.plt
    }
    
    fut.clim.scen <- scale.fut.clim.by.plt(x = PLT_CNint, future.clim.subset = clim.fut.scen)
    
    if(nrow(fut.clim.scen)==0){
      #print("no matching climate data")
    }else{
      if(length(unique(fut.clim.scen$year)) < 80){
        #print("less than 80 years of future data")
      }else{
        #scenario <- "rcp26"
        
        #fut.clim.scen <- fut.clim.plot %>% filter(rcp %in% scenario)
        #print("sampling by model")
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
          
          return(df)
        }
        
        ens.samps <- lapply(1:length(models), get.ens.df)
        ens.samps.df <- do.call(rbind, ens.samps)
        # ggplot(ens.samps.df, aes(x = year, y = ppt, group = model))+geom_line()
        # ggplot(ens.samps.df, aes(x = year, y = tmax, group = model))+geom_line()
        # ggplot(ens.samps.df, aes(x = year, y = diff.ppt, group = model))+geom_line()
        # ggplot(ens.samps.df, aes(x = year, y = diff.tmax, group = model))+geom_line()
        # # detrend the future climate using differences
        #unique(duplicated(ens.samps.df)) # no duplicates
        #unique(duplicated(ens.samps.df[,c("model", "year")]))
        detrend.samps.df  <- ens.samps.df %>% dplyr::select(diff.ppt, diff.tmax, model, year)# %>%
        colnames(detrend.samps.df) <- c("ppt", "tmax", "model", "year")
        
        
        samps.df  <- ens.samps.df %>% dplyr::select(ppt, tmax, model, year)
        detrend.samps.df  <- detrend.samps.df %>% dplyr::select(ppt, tmax, model, year)
        
        
        ens.samps.df <- samps.df #%>% group_by(year, i) #%>% summarise(ppt.mean = mean(ppt, na.rm =TRUE), 
        #             tmax.mean = mean(tmax, na.rm = TRUE))
        
        #ppt.fut <- ens.samps.df %>% group_by(year,i) %>% dplyr::select(year,i, ppt)  %>% tidyr::spread(key = year, value = ppt)%>% dplyr::select(`2019`:`2098`)
        #tmax.fut <- ens.samps.df %>% dplyr::select(year, i, tmax) %>% tidyr::spread(key = year, value = tmax)%>% dplyr::select(`2019`:`2098`)
        
        ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        
        tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        hist.samps.df <- data.frame(ppt = rep(as.numeric(ppt.hist), length(unique(ens.samps.df$model))), 
                                    tmax = rep(as.numeric(tmax.hist) , length(unique(ens.samps.df$model))), 
                                    model = rep(1:length(unique(ens.samps.df$model)), each = length(as.numeric(ppt.hist))), 
                                    year = rep(2001:2018, length(unique(ens.samps.df$model))))
        
        full.df <- rbind(hist.samps.df, ens.samps.df %>% filter(!year %in% 2018))
        full.df.nodups <- full.df[!duplicated(full.df),]
        #full.df.nodups$rowid <- 1:length(full.df.nodups$ppt)
        full.ens.ppt <- full.df.nodups  %>% dplyr::select( ppt, year, model)%>% group_by(model, year)%>% 
          summarise(ppt.m = mean(ppt, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(ppt.m, year, model) %>%group_by(year)%>%
          spread(year, value = ppt.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
        full.ens.tmax <- full.df.nodups  %>% dplyr::select(tmax, year, model)%>%  group_by(model, year)%>% 
          summarise(tmax.m = mean(tmax, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(tmax.m, year, model) %>%group_by(year)%>%
          spread(year, value = tmax.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
        #ppt <- cbind(ppt.hist, ppt.fut)
        #tmax <- cbind(tmax.hist, tmax.fut)
        SDI <- SDI.PLT.SCALED %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint ) %>% dplyr::select(SUBP,`2001`)
        
        cov.mat <- unique(x.mat %>% filter(PLT_CN %in% plt.num) %>% dplyr::select(PLT_CN, MAP, MAT))#, MAP.scaled, MAT.scaled))
        
        # check the MAP and MAT--I think we can just get from cored.in.plt df
        # this indexing may be off now
        MAP <- cov.mat$MAP
        MAT <- cov.mat$MAT
        
        #print("assembling covariate data ")
        
        covariates <- list()
        covariates$SDI <- as.matrix(SDI)
        covariates$ppt <- as.matrix(full.ens.ppt)
        covariates$tmax <- as.matrix(full.ens.tmax) 
        covariates$MAP <- MAP
        covariates$MAT <- MAT
        
        cov.data.plot <- cov.data.regional.df %>% filter(PLT_CN %in% plt.num)  %>% select(MAT, MAP)
        
        assertthat::assert_that(covariates$MAP == unique(cov.data.plot$MAP))
        assertthat::assert_that(covariates$MAT == unique(cov.data.plot$MAT))
        
        time_steps <-  length(2001:2098)
        nsamps <- max(length(betas.all$bSDI), length(x.mat[m,1]))
        forecast <- matrix(data = NA, nrow = nsamps, ncol = time_steps)
        inc <- matrix(data = NA, nrow = nsamps, ncol = time_steps)
        
        # set up sdi matrix
        sdi.subp <-  matrix(data = NA, nrow = nrow(subplots), ncol = nt + 2)
        
        sdi.subp[,1:2] <- covariates$SDI
        
        sdi.subp.raw <-  matrix(data = NA, nrow = nrow(subplots), ncol = nt + 2)
        sdi.subp.raw[,1] <- covariates$SDI[,1]
        
        # note that these are subplot SDI values that we are scaling by!
        SDI.mean.all <- mean(time_data_list$SDI, na.rm =TRUE)
        SDI.sd.all <- sd(time_data_list$SDI, na.rm =TRUE)
        
        rescale.sdi <- function(SDIscaled.val, scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all){
          SDI.raw <-   scale.by.mean + (scale.by.sd*SDIscaled.val)
          return(SDI.raw)
        }
        
        #rescale.sdi(SDIscaled.val = SDI$`2001`) 
        
        # get the first years SDI (raw) based on the PIPO trees
        sdi.subp.raw[,2] <- rescale.sdi(covariates$SDI[,2])
        
        # simpler SDI calculation
        calc.sdi.subp.simple <- function(x, TPAcalc, scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all){
          
          # get the average diameters
          if(is.null(dim(x))){
            avg.dbh <- mean(x, na.rm=TRUE)
          }else{
            avg.dbh <- apply(x, 1, mean, na.rm = TRUE) # get mean of the MCMCs for each tree
          }
          # note that any dead trees will have dbh == 0, which won't add to the sum of trees in the following line
          
          SDI.new <- sum(TPAcalc*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI, convert to inches
          
          SDIscaled.val <- (SDI.new - scale.by.mean)/scale.by.sd
          return( SDIscaled.val )
        }
        
        
        
        # need to reindex the data to have a a timestep (t), subplot (s), and tree (i)
        
        
        nsubp <- unique(index.df$SUBP)
        
        # Pseudocode:
        # 1. run full model
        # 2. run ssm model but remove the effect of SDI
        # 3. run ssm model but remove the effect of time varying climate
        # 4. run ssm model but remove the effect of 
        #------------------------------------------------------------------
        ### Run for the full model     
        #------------------------------------------------------------------    
        source("R/generate_forecast.R")
        #print("running full scenario")
        full <- generate.plot.forecast(index.trees.df = index.df, 
                                       covariates.list = covariates, 
                                       all.dbh.df = all.dbh , 
                                       betas.all.df = betas.all, 
                                       sdi.subplot.df = sdi.subp, 
                                       sdi.subplot.df.raw = sdi.subp.raw,
                                       
                                       DESIGNCD.table.plot = DESIGNCD.table,
                                       ramp.density = TRUE, # if true, will decrease the SDImax threshold 
                                       scale.DImort = 2, # scaler to multiply DI mortality by
                                       # keep these set to true
                                       density.dependent = TRUE, 
                                       density.independent = TRUE, 
                                       plt.number = plt.num, 
                                       # other information
                                       scenario = "rcp26", 
                                       SDI.ratio.DD = 0.6, 
                                       scale.mort.prob = 1, 
                                       # set to true for the no climate change scenario
                                       detrended.clim = FALSE, 
                                       SDI.mean.scale = SDI.mean.all, 
                                       SDI.sd.scale = SDI.sd.all, 
                                       MSB = TRUE, 
                                       parse = "full")
        
        ####################################################
        #run SSM by scaling growth dependent climate changes by 2
        ####################################################
        source("R/generate_forecast.R")
        GD.20 <- generate.plot.forecast(index.trees.df = index.df, 
                                        covariates.list = covariates, 
                                        all.dbh.df = all.dbh , 
                                        betas.all.df = betas.all, 
                                        sdi.subplot.df = sdi.subp, 
                                        sdi.subplot.df.raw = sdi.subp.raw,
                                        plt.number = plt.num, 
                                        DESIGNCD.table.plot = DESIGNCD.table,
                                        ramp.density = FALSE, # if true, will decrease the SDImax threshold 
                                        scale.DImort = 2, # scaler to multiply DI mortality by
                                        # keep these set to true
                                        density.dependent = TRUE, 
                                        density.independent = TRUE, 
                                        
                                        # other information
                                        scenario = "rcp26", 
                                        SDI.ratio.DD = 0.6, 
                                        scale.mort.prob = 1, 
                                        # set to true for the no climate change scenario
                                        detrended.clim = FALSE, 
                                        SDI.mean.scale = SDI.mean.all, 
                                        SDI.sd.scale = SDI.sd.all, 
                                        MSB = TRUE, 
                                        parse = "GD.20")
        
        
        ####################################################
        #run SSM by only ramping up growth dependent climate changes (GD1-)
        ####################################################
        source("R/generate_forecast.R")
        GD.10 <- generate.plot.forecast(index.trees.df = index.df, 
                                        covariates.list = covariates, 
                                        all.dbh.df = all.dbh , 
                                        betas.all.df = betas.all, 
                                        sdi.subplot.df = sdi.subp, 
                                        sdi.subplot.df.raw = sdi.subp.raw,
                                        plt.number = plt.num, 
                                        DESIGNCD.table.plot = DESIGNCD.table,
                                        ramp.density = FALSE, # if true, will decrease the SDImax threshold 
                                        scale.DImort = 1, # scaler to multiply DI mortality by
                                        # keep these set to true
                                        density.dependent = TRUE, 
                                        density.independent = TRUE, 
                                        
                                        # other information
                                        scenario = "rcp26", 
                                        SDI.ratio.DD = 0.6, 
                                        scale.mort.prob = 1, 
                                        # set to true for the no climate change scenario
                                        detrended.clim = FALSE, 
                                        SDI.mean.scale = SDI.mean.all, 
                                        SDI.sd.scale = SDI.sd.all,
                                        MSB = TRUE, 
                                        parse = "GD.10")
        ####################################################
        #run SSM by only ramping up density dependent climate changes (DD.ramp)
        ####################################################
        source("R/generate_forecast.R")
        DD.ramp <- generate.plot.forecast(index.trees.df = index.df, 
                                          covariates.list = covariates, 
                                          all.dbh.df = all.dbh , 
                                          betas.all.df = betas.all, 
                                          sdi.subplot.df = sdi.subp, 
                                          sdi.subplot.df.raw = sdi.subp.raw,
                                          plt.number = plt.num, 
                                          DESIGNCD.table.plot = DESIGNCD.table,
                                          ramp.density = TRUE, # if true, will decrease the SDImax threshold 
                                          scale.DImort = 1, # scaler to multiply DI mortality by
                                          # keep these set to true
                                          density.dependent = TRUE, 
                                          density.independent = TRUE, 
                                          
                                          # other information
                                          scenario = "rcp26", 
                                          SDI.ratio.DD = 0.6, 
                                          scale.mort.prob = 1, 
                                          # set to true for the no climate change scenario
                                          detrended.clim = FALSE, 
                                          SDI.mean.scale = SDI.mean.all, 
                                          SDI.sd.scale = SDI.sd.all,
                                          MSB = TRUE, 
                                          parse = "DD.ramp")
        ####################################################
        #run SSM with detrended climate impacts
        ####################################################
        
        # get the detrended version of the climate:
        ppt.hist <- wateryrscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        
        tmax.hist <- tmaxAprMayJunscaled %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint) %>% dplyr::select(`1966`:`2001`)
        hist.samps.df <- data.frame(ppt = rep(as.numeric(ppt.hist), length(unique(detrend.samps.df$model))), 
                                    tmax = rep(as.numeric(tmax.hist) , length(unique(detrend.samps.df$model))), 
                                    model = rep(1:length(unique(detrend.samps.df$model)), each = length(as.numeric(ppt.hist))), 
                                    year = rep(2001:2018, length(unique(detrend.samps.df$model))))
        
        full.df <- rbind(hist.samps.df, detrend.samps.df %>% filter(!year %in% 2018))
        full.df.nodups <- full.df[!duplicated(full.df),]
        
        full.ens.ppt.detrend <- full.df.nodups  %>% dplyr::select( ppt, year, model)%>% group_by(model, year)%>% 
          summarise(ppt.m = mean(ppt, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(ppt.m, year, model) %>%group_by(year)%>%
          spread(year, value = ppt.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
        full.ens.tmax.detrend <- full.df.nodups  %>% dplyr::select(tmax, year, model)%>%  group_by(model, year)%>% 
          summarise(tmax.m = mean(tmax, na.rm = TRUE)) %>% ungroup()%>%dplyr::select(tmax.m, year, model) %>%group_by(year)%>%
          spread(year, value = tmax.m, drop = FALSE)%>% ungroup () %>% dplyr::select(-model)
        
        
        SDI <- SDI.PLT.SCALED %>% ungroup() %>% filter(PLT_CN %in% PLT_CNint ) %>% dplyr::select(SUBP,`2001`)
        
        cov.mat <- unique(x.mat %>% filter(PLT_CN %in% plt.num) %>% dplyr::select(PLT_CN, MAP, MAT))#, MAP.scaled, MAT.scaled))
        
        # MAP <- x.mat[m,]$MAP
        # MAT <- x.mat[m,]$MAT
        
        
        covariates <- list()
        covariates$SDI <- as.matrix(SDI)
        covariates$ppt <- as.matrix(full.ens.ppt)
        covariates$tmax <-  as.matrix(full.ens.tmax.detrend) 
        covariates$MAP <- cov.mat$MAP
        covariates$MAT <- cov.mat$MAT
        
        source("R/generate_forecast.R")
        noClim <- generate.plot.forecast(index.trees.df = index.df, 
                                         covariates.list = covariates, 
                                         all.dbh.df = all.dbh , 
                                         betas.all.df = betas.all, 
                                         sdi.subplot.df = sdi.subp, 
                                         sdi.subplot.df.raw = sdi.subp.raw,
                                         plt.number = plt.num, 
                                         
                                         DESIGNCD.table.plot = DESIGNCD.table,
                                         ramp.density = FALSE, # if true, will decrease the SDImax threshold 
                                         scale.DImort = 1, # scaler to multiply DI mortality by
                                         # keep these set to true
                                         density.dependent = TRUE, 
                                         density.independent = TRUE, 
                                         
                                         # other information
                                         scenario = "rcp26", 
                                         SDI.ratio.DD = 0.6, 
                                         scale.mort.prob = 1, 
                                         # set to true for the no climate change scenario
                                         detrended.clim = TRUE, 
                                         SDI.mean.scale = SDI.mean.all, 
                                         SDI.sd.scale = SDI.sd.all,
                                         MSB = TRUE, 
                                         parse = "noCC")
        
        ################################################################################
        # SAVE THE PLOT level figures
        inc.plts <- cowplot::plot_grid(full$p.inc, 
                                       GD.10$p.inc + ggtitle("scaling GD by 10 only"), 
                                       GD.20$p.inc + ggtitle("scaling GD by 20 only"), 
                                       DD.ramp$p.inc + ggtitle("ramping DD only"), 
                                       noClim$p.inc+ggtitle("no climate change"), align="hv", ncol = 2)
        
        cowplot::save_plot(inc.plts, base_height = 10, device = "png",filename=  paste0("plot_level_images_MSB/increments_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        
        
        # SDI tracking plots
        SDI.plts <- cowplot::plot_grid(full$p.SDI, 
                                       GD.10$p.SDI + ggtitle("scaling GD by 10 only"), 
                                       GD.20$p.SDI+ ggtitle("scaling GD by 20 only"), 
                                       DD.ramp$p.SDI + ggtitle("ramping DD only"), 
                                       noClim$p.SDI +ggtitle("no climate change"), align="hv", ncol = 2)
        
        cowplot::save_plot(SDI.plts, base_height = 10, device = "png",filename=  paste0("plot_level_images_MSB/SUBPLOT_SDI_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        
        # TPA tracking plots
        TPA.plts <- cowplot::plot_grid(full$p.TPA.DD, 
                                       full$p.TPA.DI,
                                       full$p.TPA.MSB,
                                       full$p.TPA.all,
                                       full$p.TPA.allmort, 
                                       # GD.10$p.TPA.DD + ggtitle("scaling GD by 10 only"), 
                                       # GD.20$p.SDI+ ggtitle("scaling GD by 20 only"), 
                                       # DD.ramp$p.SDI + ggtitle("ramping DD only"), 
                                       # noClim$p.SDI +ggtitle("no climate change")
                                       align="hv", ncol = 2)
        # 
        cowplot::save_plot(TPA.plts, base_height = 10, device = "png",filename=  paste0("plot_level_images_MSB/TPA_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        
        
        # DIAMETER plots
        dia.plts <- cowplot::plot_grid(full$p.dbh, 
                                       GD.10$p.dbh + ggtitle("scaling GD by 10 only"), 
                                       GD.20$p.dbh + ggtitle("scaling GD by 20 only"), 
                                       DD.ramp$p.dbh + ggtitle("ramping DD only"), 
                                       noClim$p.dbh + ggtitle("no climate change"), 
                                       
                                       full$p.diam.tpa,
                                       GD.10$p.diam.tpa + ggtitle("scaling GD by 10 only"), 
                                       GD.20$p.diam.tpa + ggtitle("scaling GD by 20 only"), 
                                       DD.ramp$p.diam.tpa + ggtitle("ramping DD only"), 
                                       noClim$p.diam.tpa + ggtitle("no climate change"), align="hv", ncol = 5)
        
        
        cowplot::save_plot(dia.plts,base_width = 15, base_height = 5, device = "png", filename= paste0("plot_level_images_MSB/diameter_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        
        ## plot the cored.remeas for all together
        if(length(full$cored.remeas)==1){
          cat("no remeasured cores")
        }else{
          full$cored.remeas$forecast.type <- "full"
          GD.10$cored.remeas$forecast.type <- "GD.10"
          GD.20$cored.remeas$forecast.type <- "GD.20"
          DD.ramp$cored.remeas$forecast.type <- "DD.ramp"
          noClim$cored.remeas$forecast.type <- "no climate change"
          all.scen.cored <- rbind(full$cored.remeas, GD.10$cored.remeas, GD.20$cored.remeas, DD.ramp$cored.remeas, noClim$cored.remeas)
          
          pred.obs.cored <- ggplot()+geom_point(data = all.scen.cored, aes(x = DIA_cm_T2, y = predDBH_T2, color = forecast.type))+
            geom_errorbar(data = all.scen.cored, aes(x = DIA_cm_T2, ymin = predDBH_T2.lo, ymax = predDBH_T2.hi, color = forecast.type))
          cowplot::save_plot( pred.obs.cored, base_height = 5, device = "png", filename= paste0("plot_level_images_MSB/cored_tree_validation_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        }
        
        full$forecast$forecast.type <- "full"
        GD.10$forecast$forecast.type <- "GD.10"
        GD.20$forecast$forecast.type <- "GD.20"
        DD.ramp$forecast$forecast.type <- "DD.ramp"
        noClim$forecast$forecast.type <- "no climate change"
        all.scen <- rbind(full$forecast, GD.10$forecast, GD.20$forecast, DD.ramp$forecast, noClim$forecast)
        
        #ggplot(full, aes(x = year, y = mAGB.dead))+geom_line()
        AGB.plt <- ggplot()+geom_line(data = all.scen, aes(x = year, y = mAGB*0.001, group = forecast.type, color = forecast.type))+
          geom_ribbon(data = all.scen, aes(x = year, ymin = lowA*0.001, ymax = upA*0.001, fill = forecast.type),  alpha = 0.5)+
          ylab("AGB (Mg)")+theme_bw()
        AGB.ded.plot <- ggplot()+geom_line(data = all.scen, aes(x = year, y = mAGB.dead*0.001, group = forecast.type, color = forecast.type))+
          geom_ribbon(data = all.scen, aes(x = year, ymin = lowA.dead*0.001, ymax = upA.dead*0.001, fill = forecast.type),  alpha = 0.5)+  ylab("Dead AGB (Mg)")+
          theme_bw()
        
        abg.plt <- cowplot::plot_grid(AGB.plt, AGB.ded.plot, align="hv")
        cowplot::save_plot(abg.plt, base_height = 5, device = "png", filename= paste0("plot_level_images_MSB/agb_",plt.num, "_", scenario, "_", scale.mort.prob, ".png"))
        
        
        
        rm(full, GD.10, GD.20, DD.ramp, noClim, all.scen, sdi.subp, sdi.subp.raw, MAT, MAP, cored.in.plt, trees.in.plt )
      }   
      
      
    }  
  }
  #rm(list = get.objects( message = FALSE))
}



