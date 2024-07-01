# code for the forecast:
# inputs:
calcTPA_unadj <- function(DBH_in, DESIGN.tb ){
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
  return(PROB)
}

rescale.sdi <- function(SDIscaled.val, scale.by.mean , scale.by.sd ){
  SDI.raw <-   scale.by.mean + (scale.by.sd*SDIscaled.val)
  return(SDI.raw)
}

# simpler SDI calculation
calc.sdi.subp.simple <- function(x, TPAcalc, scale.by.mean , scale.by.sd ){
  
  # get the average diameters
  if(is.null(dim(x))){
    avg.dbh <- mean(x, na.rm=TRUE)
  }else{
    avg.dbh <- apply(x, 1, mean, na.rm = TRUE) # get mean of the MCMCs for each tree
  }
  # note that any dead trees will have dbh == 0, which won't add to the sum of trees in the following line
  
  SDI.new <- sum(TPAcalc*(((avg.dbh/2.54)/10)^1.6)) # calculate SDI, convert to inches
  
  SDIscaled.val <- (SDI.new - scale.by.mean)/scale.by.sd
  return(SDIscaled.val)
}

generate.plot.forecast <- function(index.trees.df, 
                                   covariates.list, 
                                   all.dbh.df, 
                                   betas.all.df,
                                   sdi.subplot.df, 
                                   sdi.subplot.df.raw,
                                   # read in designCD.table
                                   DESIGNCD.table.plot,  #= DESIGNCD.table,
                                   ramp.density, # = TRUE, # if true, will decrease the SDImax threshold 
                                   scale.DImort = 10, # scaler to multiply DI mortality by
                                   # keep these set to true
                                   density.dependent = TRUE, 
                                   density.independent = TRUE, 
                                   plt.number, 
                                   # other information
                                   scenario = "rcp26", 
                                   SDI.ratio.DD = 0.6, 
                                   scale.mort.prob = 1, 
                                   # set to true for the no climate change scenario
                                   detrended.clim = FALSE, 
                                   SDI.mean.scale, 
                                   SDI.sd.scale, 
                                   MSB = TRUE, 
                                   parse = "full"){

rm(forecast.combined, p.inc, p.dbh, p.diam.tpa, p.SDI, p.dbh.validate, p.TPA.all,
   p.TPA.allmort, p.TPA.MSB, p.TPA.DI, p.TPA.DD, cored.remeas)
ni <- ncol(all.dbh.df)#/4 # number of individuals per plot (divide by 4 because its the #years in the data)


nsamps <- length(all.dbh.df[,1])

nt <- length(2001:2098)
ntfull <- length(1998:2098)
# fill in the array with the diameter estimates for 2018:
dbh.pred <- increment <- SDI.tracking <-TPAmort <- TPAlive <- TPAsize <- TPADD <- TPADI <- TPAMSB <- mort.prob.reduced<- array(NA, dim = c(ni, nsamps, ntfull + 1))



# set up empty matrices for dbh, increment, TPA projections
id.ni <- rep(1:ni, each = 1)

for(i in 1:length(id.ni)){
  
  dbh.pred[id.ni[i],,1] <- all.dbh.df[,i]
  
}

# get the increments
for(i in 1:ni){
  for(t in 2:4){
    increment[i,,t] <- dbh.pred[i,,t] - dbh.pred[i,,t-1]
  }
}


# get the tree diameter means
dbh.pred.means <- apply(dbh.pred, 1, FUN = mean, na.rm=TRUE)

for(i in 1:ni){
  # TPA expansion factors for live trees
  TPAsize[i,,1:(ntfull + 1)] <- index.trees.df[i, "TPA_UNADJ"] # TPAlive adjusted for SIZE
  TPAlive[i,,1:(ntfull + 1)] <- index.trees.df[i, "TPA_UNADJ"] # live TPA trees (will be reduced by TPAmort)
  # TPA for tracking mortality
  TPADI[i,,1:(ntfull + 1)] <- 0 # total dead due to DI mortality
  TPADD[i,,1:(ntfull + 1)] <- 0 # total dead due to DD mortality 
  TPAMSB[i,,1:(ntfull + 1)] <- 0 # total dead due to MSB mortality 
  TPAmort[i,,1:(ntfull + 1)] <- 0 # dead TPA total
  
}



for(t in 1:nt){ # for each year t in the # of trees
  
  
  mort.code <- 0
  # loop through all of the trees for year t
  for (i in 1:ni){ # for each tree i in the n of trees
    
    SUBPLOT.index <- index.trees.df[ni,]$SUBP # select the subplot for the tree:
    
    # just changed this....
    if(min(TPAlive[i,,t], na.rm =TRUE) <= 0){ # if the tree was killed off assign dbh.pred to be the previous years prediction will be zero
      dbh.pred[i,,(t+1):nt] <- dbh.pred[i,,t]
      mort.code <- 0
      TPAlive[i,,(t+1):nt] <- 0
      TPADI[i,,(t+1):nt] <- TPADI[i,,t]
      TPADD[i,,(t+1):nt] <- TPADD[i,,t]
      TPAMSB[i,,(t+1):nt] <- TPAMSB[i,,t] # total dead due to DD mortality
      TPAmort[i,,(t+1):nt] <- TPAmort[i,,t]
      increment[i,,(t+1):nt ] <- 0
      
    }else{
      # get the right alpha for the tree
      if(index.trees.df[i,]$type %in% "cored"){
        tree.id.name <- paste0("alpha_TREE.",index.trees.df[i,]$treeid,".")
      tree.alpha <- betas.all.df[,tree.id.name]
      }else{
        tree.alpha <- betas.all.df$alpha
      }
      increment[i,,t+1] <- iterate_statespace.incpred(x = dbh.pred[i,,t],  
                                                      betas.all = betas.all.df, 
                                                      alpha = tree.alpha, 
                                                      SDdbh = 0, 
                                                      covariates =  data.frame(SDI = sdi.subplot.df[which(sdi.subplot.df[,1]==SUBPLOT.index),t+1],
                                                                               MAP = covariates.list$MAP,
                                                                               MAT= covariates.list$MAT,
                                                                               ppt = covariates.list$ppt[,t],
                                                                               tmax = covariates.list$tmax[,t]))
      
      dbh.pred[i,,t+1] <-  increment[i,,t+1] + dbh.pred[i,,t] # calculate new dbh
      # calculate a new TPA value, incase the tree grew out of the subplot:
      TPAsize[i,,t+1] <- rep(calcTPA_unadj (DBH_in = mean(dbh.pred[i,,t+1])/2.54, DESIGN.tb =  DESIGNCD.table.plot ), length(TPAsize[i,,t+1]))
      
      # adjust TPA mort by size if there is a difference
      TPAlive[i,,t] <- TPAlive[i,,t] - (TPAsize[i,,t] - TPAsize[i,,t+1])
      
      # TPAsize for a given tree and year is the tree-level mortality maximum in that year et up mortality maximums:
      # if increment is less than 0, assign inc as 0 & keep dbh.pred at previous value
      
      # this may be redundant
      zeros <- increment[i,,t+1] <= 0 #| is.na(increment[i,,t=1])
      if(TRUE %in% zeros){ # if there are zeros estimate in the increment,
        # set increment == 0, and set the dbh to the previous years diameter
        increment[i,zeros,t+1] <- 0   
        dbh.pred[i,zeros,t+1] <- dbh.pred[i,zeros,t]
        }
      
      # if after 1st years of forecast the median increment <0 for 3 years in a row, kill off the tree:
      
    }
  } # closes loop for growth model around all individual trees i
 
  
  
  # implement either SDI or MSB based mortality, given maxes are met:
  if(density.dependent == TRUE){
    
    if(MSB == TRUE){
      
      # get the total TPA, stand BASAL area of live trees:
      DBH.plt <- dbh.pred[,,t]/2.54 # gets DBH in inches
      
      if(is.null(nrow(DBH.plt))){
        TPA.plt <- TPAlive[,,t]
        TPA.plt[TPA.plt < 0] <- 0 
        # get all the trees where there are still TPAlive
        live.trees.plt <- sum(TPA.plt) > 0
      }else{
        rownames(DBH.plt) <- 1:nrow(DBH.plt)
        TPA.plt <- TPAlive[,,t]
        TPA.plt[TPA.plt < 0] <- 0 
        # get all the trees where there are still TPAlive
        live.trees.plt <- rowSums(TPA.plt) > 0
      }
      
      # calculate QMD (using DBH in inches)
      #sqrt(sum(DIA^2, na.rm =TRUE)/n())
      # should be dividing by subplot TPA and then dont need to multipy by the number of design code subplots
      #QMD.plot
     # TREE.BA.subp <- (pi*(rowMeans(DBH.plt)/2)^2)/144 # tree BA in Feet
      TREE.BA <- 0.005454154* rowMeans(DBH.plt)^2 # in sq feet
      TREE.BA.per.acre <- TREE.BA*rowMeans(TPA.plt) # in sq feet per acre
      BA.stand <- (sum(TREE.BA.per.acre)) # in sq Ft per acre
      
      TPA.stand <- sum(rowMeans(TPA.plt))
      
      AVG.BA.per.TREE = (BA.stand / (TPA.stand))
      # should be sqrt of everything??
      #QMD <- sqrt(AVG.BA.per.TREE)/(TPA.stand*0.005454154)
      QMD <- sqrt(AVG.BA.per.TREE/(TPA.stand*0.005454154))
      if(is.na(QMD)){
        QMD <- 0
      }
      #QMD.plt <- (sqrt(sum(rowMeans(DBH)^2, na.rm =TRUE)/length(trees.subplot)))*DESIGNCD.table.plot$N.subplots.points
      #TPA.plt <- sum(rowMeans(TPA.all))*DESIGNCD.table.plot$N.subplots.points
      
     
      
      # if QMD is > the MSB equation, sample mortality
      # if mean diameter is > 10 use this, but if not, just use SDI-mortality
    if( QMD > (18.68-20.63*exp(-13.25*(TPA.stand)^(-0.503)))+2 & TPA.stand > 0 | QMD > 0 &  max(DBH.plt) > 30){# & 
        
        #  SDI.raw > (450/DESIGNCD.table.plot$N.subplots.points)*SDI.ratio.DD.progression){
        
          rowMeans(DBH.plt) > 30
          
         log.fun.msb <-  data.frame(MSB.mort.log = 1/ (1+ exp(-0.1*(2:70 - 35))), 
                                    Diameter = 2:70)
        
         ggplot(log.fun.msb, aes(x = Diameter, y = MSB.mort.log))+geom_point()
         
         # if the QMD is high, use logistic curve to estimate mortality 
        RIP.mort.MSB <-  1/ (1+ exp(-0.1*(rowMeans(DBH.plt) - 35)))
        #RIP.mort.MSB <- 0.5
        
          RIP.mort.MSB[rowMeans(DBH.plt) < 30] <- 0
          #RIP.mort.MSB[rowMeans(DBH.plt) > 30] <- 0.11
      
        MSB.mort.per.tree <- matrix(rbind(lapply(X = RIP.mort.MSB, function(x){mean(rbinom(1000, 1, prob = x))})))
        MSB.mort.per.tree.MAT <- matrix(nrow = dim(TPAMSB)[1], ncol = dim(TPAMSB)[2], rep(as.numeric(MSB.mort.per.tree), dim(TPAMSB)[2]))

        # if there is no mortality just walk the trees forward with any MSB mortality ( for now, we want to preferentially add MSB mortality to larger trees)
        TPAMSB[,,t+1] <- TPAMSB[,,t] + MSB.mort.per.tree.MAT
        TPADD[,,t+1] <- TPADD[,,t] 
        TPAlive[,,t+1] <- TPAlive[,,t] - MSB.mort.per.tree.MAT
        
      
        
      }else{
        TPAMSB[,,t+1] <- TPAMSB[,,t] 
        TPADD[,,t+1] <- TPADD[,,t] 
        TPAlive[,,t+1] <- TPAlive[,,t]
    
     
      }# closes if/else statementfor QMD
      
    if(QMD < (18.68-20.63*exp(-13.25*(TPA.stand)^(-0.503)))+2){ # if the qmd is not greater than the MSB line, enact sdi mort
  
    for(s in sdi.subplot.df[,1]){ 
      # for each subplot s in the # of subplots
      # need to index dbh.pred trees by by the subplot:
      
      trees.subplot <- as.numeric(rownames(index.trees.df[index.trees.df$SUBP == s,]))
      if(length(trees.subplot)<=1){
        TPAcalc <- mean(TPAlive[trees.subplot,,(t)])
      }else{
        TPAcalc <- rowMeans(TPAlive[trees.subplot,,(t)])
      }
      sdi.temp <-  calc.sdi.subp.simple(x = dbh.pred[trees.subplot,,t], scale.by.mean = SDI.mean.scale, scale.by.sd = SDI.sd.scale, TPAcalc = TPAcalc)
      SDI.raw <- rescale.sdi(SDIscaled =  sdi.temp, scale.by.mean = SDI.mean.scale, scale.by.sd = SDI.sd.scale)
      
      if(ramp.density == TRUE){
        # when set to true, this ramps up the density dependent mortality by reducing the SDImax that is needed to turn DD mort on
      SDI.ratio.DD.progression <- ifelse(t < 30, SDI.ratio.DD, 
                                         ifelse(t >= 30 & t < 40, 0.55, 
                                                ifelse(t >= 40 & t < 40, 0.5, 
                                                       ifelse(t >= 50 & t <60, 0.45, 
                                                              ifelse(t >=60 & t < 70, 0.4, 0.38)))))
      }else{
        SDI.ratio.DD.progression <- SDI.ratio.DD
      }
      
      
      # loosly basing on FVS mortality:
      # calculate an RI for each tree:
      p0 <- 5.5877
      p1 <- -0.005348
      
      
      # get the diameter in inches
      DBH <- dbh.pred[trees.subplot,,t]/2.54
      #combined[trees.subplot,]
      if(is.null(nrow(DBH))){
        TPA.all <- TPAlive[trees.subplot,,t]
        TPA.all[TPA.all < 0] <- 0 
        # get all the trees where there are still TPAlive
        live.trees <- sum(TPA.all) > 0
      }else{
      rownames(DBH) <- 1:nrow(DBH)
      TPA.all <- TPAlive[trees.subplot,,t]
      TPA.all[TPA.all < 0] <- 0 
      # get all the trees where there are still TPAlive
      live.trees <- rowSums(TPA.all) > 0
      }
     
      
      
      ## get subplot level MSB
 
    # use MSB instead
      
      # if the stand is above 70% of SDI and mort probability is not already 1, increase mortality probability
      # this should increase mortality liklihood across all the trees, but mostly larger trees
      if( SDI.raw > (450/DESIGNCD.table.plot$N.subplots.points)*SDI.ratio.DD.progression){ # if SDI is greater than SDI max, implement mortality
        
       
        
        if(all(live.trees==FALSE)==TRUE | length(live.trees[live.trees==TRUE]) ==1){# if there is one or less live stem on the plot, just ignore DD mortality
          #print("only <=1 trees on the subplot are alive ")
          TPADD[trees.subplot,,t+1] <- TPADD[trees.subplot,,t]
          TPAlive[trees.subplot,,t+1] <- TPAlive[trees.subplot,,t]
          TPAmort[trees.subplot,,t+1] <- TPAmort[trees.subplot,,t]
          TPAMSB[trees.subplot,,t+1] <- TPAMSB[trees.subplot,,t] #+ TPAlive[trees.subplot,,t]*MSB.mort.per.tree
          
        }else{ 
          # calculate the mortality to distribute among trees
          
          DBH.live  <- DBH[live.trees,]
          RI <- (1/(1+exp(p0 + p1*DBH.live)))*0.5
          Y <- 1
          # changed from what FVS manual says to margarets eq when Y == 1, RI and RIP are equal
          
          # RIP <- 1-(1-RI)^(1/Y) # changed from what FVS manual says to margarets eq when Y == 1, RI and RIP are equal
          # 
          # Mort.rate <- mean(colSums(RIP)) # total background mortality for the plot
          # 
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
          #split.mort <- Mort.rate/length(live.trees)#avg.MORT
          #sum(avg.MORT)
          #mort.per.tree <- Mort.rate/avg.mort.prob
          mort.per.tree <- avg.MORT# avg.mort.prob
          mort.per.tree[rowMeans(DBH.live) > 30] <- 0# avg.mort.prob
          
         
          # indexing for the live trees on the subplot, add mortality to existing DD mortality and DI mortality to get the total
          TPADD[trees.subplot[live.trees],,t+1] <- TPADD[trees.subplot[live.trees],,t] + (TPAlive[trees.subplot[live.trees],,(t)]*mort.per.tree) #+ MSB.mort.per.tree)
          TPAlive[trees.subplot[live.trees],,(t+1)] <- TPAlive[trees.subplot[live.trees],,(t)] - (TPAlive[trees.subplot[live.trees],,(t)]*mort.per.tree) #+ MSB.mort.per.tree)
          #TPAMSB[trees.subplot[live.trees],,t+1] <- TPAMSB[trees.subplot[live.trees],,t]
        } # closes else statement
      
    }# closes if/else statement with SDI mortality
    } # closes for loop that loops SDI mortality around each plot
      }# closes if/else statement that either implements MSB mortality, or if SDI max is hit, does SDI mortality
    }# if MSB == TRUE
    
  } # closes if(density.dependent == TRUE) statement   
 
 # if t > 2 & density.independent == TRUE, allow Growth-dependent mortality to happen 
  if(t >= 2 ){
    
    # default is that mort.prob == 0 (i.e. no mortality)
    
    mort.prob <- 0
    
    
    
    if(density.independent == TRUE){
      
      tindex <- ifelse(t >=6, t-5,
                       ifelse(t == 5, t-4,
                              ifelse(t ==4, t-3,
                                     ifelse(t==3, t-2,
                                            ifelse(t == 2, t-1, t)))))
      
      for(i in 1:ni){
        # zero.means <-  colMeans(increment[i,,(tindex):(t)], na.rm = TRUE) <= 0.01 
        # zero.df <- ifelse(zero.means == FALSE, 0, 1)
        # mort.prob <- mean(zero.df, na.rm =TRUE)
        # #psurv <-  as.vector(inv.logit(alpha + (b.growth * increment[i,,tindex+1:t])))
        # updating psurv to reflect the mean of the last ~5 years
        psurv <-  as.vector(inv.logit(alpha.mort + (b.growth * increment[i,,t]) + (b.dbh*dbh.pred[i,,t]) ))
        
        mort.prob <-  1 - psurv
        
        mort.prob.reduced[i,,t] <- mort.prob*scale.DImort/(scale.mort.prob) # check what the distribution is of this
        mort.code <- rbinom(1,1, prob = min(1, mort.prob*scale.DImort))#(mort.prob.reduced[i,,t])) # to tone down, reduce mort.prob 
        
        
        
        # if the mort code is one, and the tree is not already dead-dead, and it was not already killed in density dependent mortality
        if(mort.code == 1  &  min(TPAlive[i,,t], na.rm = TRUE)>0){
          
          
  
            # if the cc is not mulitplied by 2
            TPADI[i,,(t+1)] <- TPADI[i,,(t)] + (mort.prob.reduced[i,,t])  #(0.2) # would also have to reduce mnort prob here
            
          
          
        }else{
          # just carry over the values from the previous year
          TPADI[i,,(t+1)] <- TPADI[i,,(t)]
          TPAlive[i,,(t+1)] <- TPAlive[i,,(t)]
          # issue--if there is no TPADD, then we need to just carry over the values
          # but if there is TPADD, there will be an updated TPAmort
          TPAmort[i,,(t+1)] <- TPAmort[i,,t]
        }
        
        
        # finally for each tree calculate the total mortality:
        TPAmort[i,,t+1] <- TPADI[i,,t+1] + TPADD[i,,t+1] + TPAMSB[i,,t+1]
        TPAlive[i,,(t+1)] <- TPAlive[i,,t] - (TPAmort[i,,t+1]-TPAmort[i,,t])  #(0.2) # would also have to reduce mnort prob here
        
        # if the total dead is greater than the total possible dead, 
        # reconcile the values reduce DD and DI
        # the combined DI and DD mortality can't be more than TPAsize
        # this might need to be TPAlive instead of TPAsize??
        
        if(mean(TPAmort[i,,t+1], na.rm =TRUE) > mean(TPAsize[i,,t])){
          
          over.by <- ( TPAmort[i,,t+1] - TPAsize[i,,t] )
          
          DI.diff <- TPADI[i,,t+1]-TPADI[i,,t]
          SDI.diff <- TPADD[i,,t+1]-TPADD[i,,t]
          MSB.diff <- TPAMSB[i,,t+1]-TPAMSB[i,,t]
          DD.diff <- SDI.diff + MSB.diff
          # reduce each of the types of mortality by a weighted average of how much we are over by
          if(mean(DI.diff)==0 & !mean(SDI.diff)==0 & mean(MSB.diff) == 0){
            TPADI[i,,(t+1)] <- TPADI[i,,(t+1)]
            TPADD[i,,(t+1)] <- TPADD[i,,(t+1)] - over.by
          }
          if(mean(DI.diff)==0 & mean(SDI.diff)==0 & !mean(MSB.diff) == 0){
            TPADI[i,,(t+1)] <- TPADI[i,,(t+1)]
            TPADD[i,,(t+1)] <- TPADD[i,,(t+1)]
            TPAMSB[i,,(t+1)] <- TPAMSB[i,,(t+1)] - over.by
            
          }
          
            if(mean(DD.diff)==0 & ! mean(DI.diff)==0 ){
              TPADD[i,,(t+1)] <- TPADD[i,,(t+1)]
              TPADI[i,,(t+1)] <- TPADI[i,,(t+1)] - over.by
              }
          # otherwise, split mortality up by ratio of differences
         # if(!mean(DD.diff)==0 & !mean(DI.diff)==0 & ! mean(MSB.diff)==0){
          TPADI[i,,(t+1)] <- TPADI[i,,(t+1)] - (DI.diff/(DI.diff + SDI.diff + MSB.diff))*over.by
          TPADD[i,,(t+1)] <- TPADD[i,,(t+1)] - (SDI.diff/(DI.diff + SDI.diff + MSB.diff))*over.by
          TPAMSB[i,,(t+1)] <- TPAMSB[i,,(t+1)] - (MSB.diff/(DI.diff + SDI.diff + MSB.diff))*over.by
          #}
          
          # now update the totals for live and mort
          TPAlive[i,,(t+1)] <- 0
          TPAmort[i,,(t+1)] <- TPADI[i,,(t+1)] + TPADD[i,,(t+1)] + TPAMSB[i,,(t+1)] # should be equal to TPAsize
        }
      }
      
     for(i in 1:ni){
      if(min(TPAlive[i,,t+1], na.rm =TRUE) <= 0){ # if all the representative trees are dead, 
        TPAlive[i,,(t+1):nt] <- 0
        # set TPADI as equal to the the max value :
        
        TPADI[i,,(t+1):nt] <- TPADI[i,,t]
        TPADD[i,,(t):nt] <- TPADD[i,,t]
        TPAmort[i,,(t):nt] <- TPAmort[i,,t]
        TPAMSB[i,,(t+1):nt] <- TPAMSB[i,,t]
        # set DBH == 0? Or keep DBH == the last living value?
        
        dbh.pred[i,,(t+1):nt] <- dbh.pred[i,,t]
      }
     }
    }# closes density independent if statement
  } # closes the t > 2 statement
  
  
  ### After adding in mortality, recalculate SDI
  for(s in sdi.subplot.df[,1]){ # for each subplot s in the # of subplots
    # need to index dbh.pred trees by by the subplot:
    trees.subplot <- as.numeric(rownames(index.trees.df[index.trees.df$SUBP == s,]))
    if(length(trees.subplot)<=1){
      TPAcalc <- mean(TPAlive[trees.subplot,,(t+1)])
    }else{
      TPAcalc <- rowMeans(TPAlive[trees.subplot,,(t+1)])
    }
    # recalculated scaled SDI
    sdi.subplot.df[which(sdi.subplot.df[,1]==s),t+2] <-  calc.sdi.subp.simple(x = dbh.pred[trees.subplot,,t], scale.by.mean = SDI.mean.scale, scale.by.sd = SDI.sd.scale, TPAcalc = TPAcalc)
    
    # put sdi back on the original scale
    sdi.subplot.df.raw[which(sdi.subplot.df.raw[,1]==s),t+2] <- rescale.sdi(sdi.subplot.df[which(sdi.subplot.df[,1]==s),t+2], scale.by.mean = SDI.mean.scale, scale.by.sd = SDI.sd.scale)
  }

} # closes the time loop

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



cc.scenario <- "singleCC"


dbh.quants <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
colnames(dbh.quants) <- c("quantile","treeno","time", "diameter")


dbh.quants.spread <- dbh.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.trees.df$treeno <- 1:length(index.trees.df$CN)

dbh.means.index <- left_join(dbh.quants.spread , index.trees.df, by = "treeno")

p.dbh <- ggplot()+geom_line(data = dbh.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) +
  geom_ribbon(data = dbh.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
  theme_bw() + ylab("Diameters (cm)")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plt.number))

cored.remeas <- index.trees.df %>% filter(type %in% "cored" & !is.na(DIA_cm_T2) &  MEASYEAR_T2 > 2001)

if(length(cored.remeas$treeid) >0 ){
  p.dbh.validate <- ggplot()+geom_line(data = dbh.means.index %>% filter(treeno %in% cored.remeas$treeno), aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) +
    geom_ribbon(data = dbh.means.index %>% filter(treeno %in% cored.remeas$treeno), aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
    geom_point(data = cored.remeas, aes(x = MEASYEAR_T2-2001, y = DIA_cm_T2))+
    theme_bw() + ylab("Diameters (cm)")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for  ", plt.number))
  cored.remeas$time <- cored.remeas$MEASYEAR_T2-2001
  
  
  
  pred.dbh.validate <- dbh.means.index %>% filter(treeno %in% cored.remeas$treeno & time %in% cored.remeas$time)
  cored.remeas$predDBH_T2 <-  pred.dbh.validate$`50%`
  cored.remeas$predDBH_T2.lo <-  pred.dbh.validate$`2.5%`
  cored.remeas$predDBH_T2.hi <-  pred.dbh.validate$`97.5%`
}else{
  p.dbh.validate = NA
  cored.remeas = NA
}

ggplot()+geom_line(data = dbh.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) +
  geom_ribbon(data = dbh.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
  theme_bw() + ylab("Diameters (cm)")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plt.number))


# TPA of live trees
TPA.quants <- reshape2::melt(apply(TPAlive, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.quants) <- c("treeno","time", "TPA")

p.TPA.all <- ggplot(TPA.quants, aes(x = time, y = TPA, group = treeno, color = treeno))+geom_line()+ggtitle("Live TPA")
# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.trees.df$treeno <- 1:length(index.trees.df$CN)

tpa.means.index <- left_join(TPA.quants, index.trees.df, by = "treeno")
#tpa.means.index %>% group_by(treeno) %>% summarise(min(TPA))
# do the same for the TPAdd and TPAdi:
TPA.DD.quants <- reshape2::melt(apply(TPADD, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.DD.quants) <- c("treeno","time", "TPADD")

p.TPA.DD <- ggplot(TPA.DD.quants, aes(x = time, y = TPADD, group = treeno, color = treeno))+geom_line()+ggtitle("TPA DD")


TPA.MSB.quants <- reshape2::melt(apply(TPAMSB, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.MSB.quants) <- c("treeno","time", "TPAMSB")


p.TPA.MSB <- ggplot(TPA.MSB.quants, aes(x = time, y = TPAMSB, group = treeno, color = treeno))+geom_line()+ggtitle("TPA MSB")


# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.trees.df$treeno <- 1:length(index.trees.df$CN)

tpa.DD.means.index <- left_join(TPA.DD.quants, index.trees.df, by = "treeno")


TPA.DI.quants <- reshape2::melt(apply(TPADI, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.DI.quants) <- c("treeno","time", "TPADI")


p.TPA.DI <- ggplot(TPA.DI.quants, aes(x = time, y = TPADI, group = treeno, color = treeno))+geom_line()+ggtitle("TPA DI")


## add all together:
TPA.alldead.quants <- left_join(TPA.DI.quants, TPA.DD.quants) %>% left_join(., TPA.MSB.quants) %>%
                      reshape2::melt(., id.vars = c("treeno", "time")) 

colnames(TPA.alldead.quants) <- c("treeno", "time", "Mortality.type", "value")
TPA.alldead.quants <- TPA.alldead.quants %>% group_by(time, Mortality.type) %>%
  summarise(TPA = sum(value))

p.TPA.allmort <- ggplot(TPA.alldead.quants, aes(x = time, y = TPA, fill = Mortality.type))+geom_col()

#TPA.DI.quants %>% filter(time %in% 99)
# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.trees.df$treeno <- 1:length(index.trees.df$CN)

tpa.DI.means.index <- left_join(TPA.DI.quants, index.trees.df, by = "treeno")


#ggplot()+geom_line(data = tpa.means.index %>% filter(treeno == 20), aes(x = time, y = TPA, group = treeno, color = as.character(treeno))) 

# join with the diameter 
dbh.means.TPA.index<- left_join(tpa.means.index[,c("treeno", "time", "TPA")], dbh.means.index, by = c("treeno", "time"))
dbh.means.TPA.index$med.scaled.DBH <- dbh.means.TPA.index$`50%`*dbh.means.TPA.index$TPA
dbh.means.TPA.index$hi.scaled.DBH <- dbh.means.TPA.index$`97.5%`*dbh.means.TPA.index$TPA
dbh.means.TPA.index$lo.scaled.DBH <- dbh.means.TPA.index$`2.5%`*dbh.means.TPA.index$TPA


p.diam.tpa <- ggplot()+geom_line(data = dbh.means.TPA.index, aes(x = time, y = med.scaled.DBH, color = as.character(SUBP), group = treeno)) +
  geom_ribbon(data = dbh.means.TPA.index, aes(x = time, ymin = lo.scaled.DBH, ymax = hi.scaled.DBH,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
  theme_bw() + ylab("Diameters (cm)*TPA")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plt.number))
#p.diam.tpa


inc.quants <- reshape2::melt(apply(increment, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
colnames(inc.quants) <- c("quantile","treeno","time", "increment")


inc.quants.spread <- inc.quants %>% group_by(treeno, time) %>% spread(quantile, increment)
# inc.means <- reshape2::melt(apply(inc.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(inc.means) <- c("treeno","time", "diameter")
index.trees.df$treeno <- 1:length(index.trees.df$CN)

inc.means.index <- left_join(inc.quants.spread , index.trees.df, by = "treeno")

p.inc <- ggplot() +
  geom_ribbon(data = inc.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`, fill = as.character(SUBP), group = treeno), alpha = 0.25)+
  geom_line(data = inc.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno))+
  theme_bw() + ylab("Increments (cm)")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plt.number))+ labs(fill = "Subplot Number", color = "Subplot Number")

#ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.increment.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p.inc)

#p.inc

# plot out the SDI values
#sdi.subplot.df.raw.df <- data.frame(sdi.subplot.df.raw)
SDI.values  <- reshape2::melt(sdi.subplot.df.raw[,2:100]) # ,  function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
if(nrow(sdi.subplot.df.raw) == 1){
  SDI.values <- data.frame(Var2 = 2001:2099, 
                           value = SDI.values$value, 
                           Var1 = 1)
}
if(ramp.density == TRUE){
  
  SDI.ratio.df <- data.frame(SDI.ratio = c(rep(SDI.ratio.DD, 29), 
                         rep( 0.55, 10), 
                         rep( 0.5, 10), 
                         rep(0.45, 10), 
                         rep( 0.4, 10), 
                         rep(0.38, 30)), 
                         year = 1:99, 
                         SDI.max = 450/DESIGNCD.table.plot$N.subplots.points) %>% 
    group_by(year)%>%
    mutate(ramped.SDI = (SDI.max)*SDI.ratio)
  

p.SDI <- ggplot() +
  geom_line(data = SDI.values, aes(x = Var2, y = value, color = as.character(Var1)))+
  theme_bw() + ylab("Subplot SDI")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plt.number))+ labs(fill = "Subplot Number", color = "Subplot Number")+
  geom_line(data = SDI.ratio.df, aes(x = year, y = ramped.SDI), color = "darkgrey", linetype = "dashed")

}else{
  
  SDI.ratio.DD.progression <- SDI.ratio.DD
  p.SDI <- ggplot() +
    geom_line(data = SDI.values, aes(x = Var2, y = value, color = as.character(Var1)))+
    theme_bw() + ylab("Subplot SDI")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plt.number))+ labs(fill = "Subplot Number", color = "Subplot Number")+
    geom_hline(yintercept = ((450/DESIGNCD.table.plot$N.subplots.points)*SDI.ratio.DD.progression), color = "darkgrey", linetype = "dashed")
  
}


combined <- index.trees.df

# var 1 is tree, var2 is mcmc sample, and var 3 is time
test.m <- reshape2::melt(dbh.pred, id.vars = dim(dbh.pred)[2])
test.m$id <- paste0("x[",test.m$Var1, ",", test.m$Var3,"]")
test.m <- test.m%>% filter(Var3 %in% 1:99)
colid.ordered <- test.m$id

test.m.time <- test.m %>% dplyr::select(-Var1, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
out <- test.m.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(all_of(colid.ordered))
#out <- out[1:50,]
out.mean <- apply(out, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})

# get a TPA out:
tpa.m <- reshape2::melt(TPAlive, id.vars = dim(TPAlive)[2])
#tpa.m$newVar3 <- rep(4:102, each = 100*ni)

# var1 os the tree Var 2 is the samples and Var3 is the year

tpa.m <- tpa.m %>% filter(Var3 %in% 1:99)
tpa.m$id <- paste0("x[",tpa.m$Var1, ",", tpa.m$Var3,"]")

TPA.ordered <- tpa.m$id

tpa.m.time <- tpa.m %>% dplyr::select(-Var1, -Var3, -Var3) %>% group_by(Var2) %>% spread(key = id, value = value)
out.tpa <- tpa.m.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(all_of(TPA.ordered))
#out <- out[1:50,]
tpa.mean <- apply(out.tpa, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})


# calculate an out for the dead trees
# should we do this difference, or just add up the total dead trees? 
tpa.diff.df <- tpa.m %>% group_by( Var1, Var2) %>% mutate(firstTPA = value[1]) %>%
  ungroup()%>% group_by(Var1, Var3) %>% mutate(TPAdiff = firstTPA-value)

#summary(tpa.diff.df$TPAdiff)

#ggplot(data = tpa.diff.df,aes(x = Var3, y = TPAdiff, group =as.character(Var1) , color = as.character(Var1)))+geom_line()

#tpa.diff.df
tpa.diff.time <-  tpa.diff.df %>% ungroup()%>% dplyr::select(-Var1, -Var3, -value) %>% group_by(Var2) %>% spread(key = id, value = TPAdiff)
out.tpa.diff <- tpa.diff.time %>% ungroup()%>% dplyr::select(-Var2) %>% dplyr::select(TPA.ordered)
#out <- out[1:50,]
tpa.diff.mean <- apply(out.tpa.diff, MARGIN = 2, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm =TRUE)})


if(ramp.density == TRUE & scale.DImort == 20 & detrended.clim == FALSE){
  parse = "full"
}
if(ramp.density == TRUE & scale.DImort == 10 & detrended.clim == FALSE){
  parse = "DD.ramp"
}
if(ramp.density == FALSE & scale.DImort == 20 & detrended.clim == FALSE){
  parse = "GD.20"
}
if(ramp.density == FALSE & scale.DImort == 10 & detrended.clim == FALSE){
  parse = "GD.10"
}
if(ramp.density == FALSE & scale.DImort == 10 & detrended.clim == TRUE){
  parse = "detrendedCC"
}
source("R/plot2AGB_kayeFVS.R")
#print("start biomass estimates full model")
forecast.combined <- plot2AGB(combined = combined, 
                 out = out.mean, 
                 tpa = tpa.mean, 
                 tpa.diff = tpa.diff.mean, 
                 tpa.dd = tpa.DD.means.index, 
                 tpa.di = tpa.DI.means.index, 
                 mort.scheme = mort.scheme, 
                 allom.stats = kaye_pipo, 
                 unit.conv = 0, 
                 plot.id = plt.number, 
                 yrvec = 2001:2098, 
                 scenario = scenario,
                 cc.scenario = cc.scenario, 
                 #p = NULL, 
                 #p.inc = NULL, 
                 SDI.ratio.DD = SDI.ratio.DD, 
                 plt.design = "periodic", 
                 folder.name = paste0("biomass_dataFIAperiodic_", scale.mort.prob), 
                 parse.type = parse, 
                 mort.prob.reduced  = mort.prob.reduced, 
                 index.trees.data = index.trees.df, 
                 cored.remeas = cored.remeas)

#List the objects created in this script
#get.objects()
# rm(list = get.objects(exception = c(forecast.combined,kaye_pipo, p.inc, p.dbh, p.diam.tpa,
#                                     p.SDI, p.dbh.validate, p.TPA.all, p.TPA.allmort,
#                                     p.TPA.MSB, p.TPA.DI, p.TPA.DD, cored.remeas), message = TRUE))

return(list(forecast = forecast.combined,
            p.inc = p.inc, 
            p.dbh = p.dbh,
            p.diam.tpa = p.diam.tpa, 
            p.SDI = p.SDI, 
            p.TPA.DI = p.TPA.DI, 
            p.TPA.DD = p.TPA.DD, 
            p.TPA.MSB = p.TPA.MSB, 
            p.TPA.all = p.TPA.all, 
            p.TPA.allmort = p.TPA.allmort, 
            p.dbh.validate = p.dbh.validate, 
            cored.remeas = cored.remeas))

rm(list = get.objects(exception = c(forecast.combined,kaye_pipo, p.inc, p.dbh, p.diam.tpa, 
                                    p.SDI, p.dbh.validate, p.TPA.all, p.TPA.allmort, 
                                    p.TPA.MSB, p.TPA.DI, p.TPA.DD, cored.remeas), message = TRUE))

}
