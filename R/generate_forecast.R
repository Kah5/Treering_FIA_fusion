# code for the forecast:
# inputs:

generate.plot.forecast <- function(index.df, covariates, all.dbh, betas.all,
                                   sdi.subp, 
                                   sdi.subp.raw,
                                   ramp.density = TRUE, # if true, will decrease the SDImax threshold 
                                   scale.DImort = 10, # scaler to multiply DI mortality by
                                   # keep these set to true
                                   density.dependent = TRUE, 
                                   density.independent = TRUE, 
                                   
                                   # other information
                                   scenario = "rcp26", 
                                   SDI.ratio.DD = 0.6, 
                                   scale.mort.prob = 1, 
                                   # set to true for the no climate change scenario
                                   detrended.clim = FALSE){

ni <- ncol(all.dbh)#/4 # number of individuals per plot (divide by 4 because its the #years in the data)


nsamps <- length(all.dbh[,1])

nt <- length(2001:2098)
ntfull <- length(1998:2098)
# fill in the array with the diameter estimates for 2018:
dbh.pred <- increment <- SDI.tracking <-TPAmort <- TPAlive <- TPAsize <- TPADD <- TPADI <- mort.prob.reduced<- array(NA, dim = c(ni, nsamps, ntfull + 1))



# set up empty matrices for dbh, increment, TPA projections
id.ni <- rep(1:ni, each = 1)

for(i in 1:length(id.ni)){
  
  dbh.pred[id.ni[i],,1] <- all.dbh[,i]
  
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
  TPAsize[i,,1:(ntfull + 1)] <- index.df[i, "TPA_UNADJ"] # TPAlive adjusted for SIZE
  TPAlive[i,,1:(ntfull + 1)] <- index.df[i, "TPA_UNADJ"] # live TPA trees (will be reduced by TPAmort)
  # TPA for tracking mortality
  TPADI[i,,1:(ntfull + 1)] <- 0 # total dead due to DI mortality
  TPADD[i,,1:(ntfull + 1)] <- 0 # total dead due to DD mortality 
  TPAmort[i,,1:(ntfull + 1)] <- 0 # dead TPA total
  
}



for(t in 1:nt){ # for each year t in the # of trees
  
  
  mort.code <- 0
  # loop through all of the trees for year t
  for (i in 1:ni){ # for each tree i in the n of trees
    
    SUBPLOT.index <- index.df[ni,]$SUBP # select the subplot for the tree:
    
    # just changed this....
    if(mean(TPAlive[i,,t], na.rm =TRUE) <= 0){ # if the tree was killed off assign dbh.pred to be the previous years prediction will be zero
      dbh.pred[i,,(t+1):nt] <- dbh.pred[i,,t]
      mort.code <- 0
      TPAlive[i,,(t+1):nt] <- 0
      TPADI[i,,(t+1):nt] <- TPADI[i,,t]
      TPADD[i,,(t+1):nt] <- TPADD[i,,t]
      TPAmort[i,,(t+1):nt] <- TPAmort[i,,t]
      increment[i,,(t+1):nt ] <- 0
      
    }else{
      increment[i,,t+1] <- iterate_statespace.incpred(x = dbh.pred[i,,t],  betas.all = betas.all, beta_YEARid = rep(0, nsamps), SDdbh = 0, covariates =  data.frame(SDI = sdi.subp[which(sdi.subp[,1]==SUBPLOT.index),t+1],
                                                                                                                                                                    MAP = MAP,
                                                                                                                                                                    MAT= MAT,
                                                                                                                                                                    ppt = covariates$ppt[,t],
                                                                                                                                                                    tmax = covariates$tmax[,t]))
      
      dbh.pred[i,,t+1] <-  increment[i,,t+1]+dbh.pred[i,,t] # calculate new dbh
      # calculate a new TPA value, incase the tree grew out of the subplot:
      TPAsize[i,,t+1] <- rep(calcTPA_unadj (DBH_in = mean(dbh.pred[i,,t+1])/2.54, DESIGN.tb =  DESIGNCD.table ), length(TPAsize[i,,t+1]))
      
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
  }
  # use the current SDI to determine density dependnt mortality at the stand scale, then distribute to trees:
  # my attempt to reflect what FVS is doing
  
  
  if(density.dependent == TRUE){
    
    for(s in sdi.subp[,1]){ 
      # for each subplot s in the # of subplots
      # need to index dbh.pred trees by by the subplot:
      
      trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
      if(length(trees.subplot)<=1){
        TPAcalc <- mean(TPAlive[trees.subplot,,(t)])
      }else{
        TPAcalc <- rowMeans(TPAlive[trees.subplot,,(t)])
      }
      sdi.temp <-  calc.sdi.subp.simple(x = dbh.pred[trees.subplot,,t], scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all, TPAcalc = TPAcalc)
      SDI.raw <- rescale.sdi(SDIscaled =  sdi.temp, scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all)
      
      if(ramp.density ==TRUE){
        # when set to true, this ramps up the density dependent mortality by reducing the SDImax that is needed to turn DD mort on
      SDI.ratio.DD.progression <- ifelse(t < 30, SDI.ratio.DD, 
                                         ifelse(t >= 30, 0.55, 
                                                ifelse(t >= 40, 0.5, 
                                                       ifelse(t >= 50, 0.45, 
                                                              ifelse(t >=60, 0.4, 
                                                                     ifelse(t>=70, 0.38))))))
      }else{
        SDI.ratio.DD.progression <- SDI.ratio.DD
      }
      # if the stand is above 70% of SDI and mort probability is not already 1, increase mortality probability
      # this should increase mortality liklihood across all the trees, but mostly larger trees
      if( SDI.raw > (450/4)*SDI.ratio.DD.progression & length(trees.subplot) > 1){
        
        # loosly basing on FVS mortality:
        # calculate an RI for each tree:
        p0 <- 5.5877
        p1 <- -0.005348
        
        
        # get the diameter in inches
        DBH <- dbh.pred[trees.subplot,,t]/2.54
        #combined[trees.subplot,]
        rownames(DBH) <- 1:nrow(DBH)
        
        TPA.all <- TPAlive[trees.subplot,,t]
        # get all the trees where there are still TPAlive
        live.trees <- rowSums(TPA.all) > 0
        
        if(all(live.trees==FALSE)==TRUE | length(live.trees[live.trees==TRUE]) ==1){
          cat("only <=1 trees on the subplot are alive ")
          TPADD[trees.subplot,,t+1] <- TPADD[trees.subplot,,t]
          TPAlive[trees.subplot,,t+1] <- TPAlive[trees.subplot,,t]
          TPAmort[trees.subplot,,t+1] <- TPAmort[trees.subplot,,t]
        }else{
          
          DBH.live  <- DBH[live.trees,]
          RI <- (1/(1+exp(p0 + p1*DBH.live)))*0.5
          Y <- 1
          # changed from what FVS manual says to margarets eq when Y == 1, RI and RIP are equal
          
          RIP <- 1-(1-RI)^(1/Y) # changed from what FVS manual says to margarets eq when Y == 1, RI and RIP are equal
          
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
          
          # indexing for the live trees on the subplot, add mortality to existing DD mortality and DI mortality to get the total
          TPADD[trees.subplot[live.trees],,t+1] <- TPADD[trees.subplot[live.trees],,t] + TPAlive[trees.subplot[live.trees],,(t)]*mort.per.tree
          # TPAmort[trees.subplot[live.trees],,t+1] <- TPADI[trees.subplot[live.trees],,t] + TPAlive[trees.subplot[live.trees],,(t)]*mort.per.tree
          # # subtract fraction of DD killed trees the values to get TPA live
          # TPAlive[trees.subplot[live.trees],,t+1] <- TPAlive[trees.subplot[live.trees],,(t)] - TPAlive[trees.subplot[live.trees],,(t)]*mort.per.tree
          # 
          # need to put in a max mortality so assign TPADD for all the live trees on the plot to the starting TPA
          # for trees where all the TPA trees are also dead set TPADD to the previous years TPAlive
          
          # # TPADD if the total of TPADD and TPADI for each tree is greater than TPAsize
          # TPADD[trees.subplot[live.trees],,t+1] [ TPAmort[trees.subplot[live.trees],,t+1]  > TPAsize[trees.subplot[live.trees],,t]] <-  TPAsize[trees.subplot[live.trees],,t]-(TPADI[trees.subplot[live.trees],,(t)])
          #   
          #  
          #  # if the sum of TPADD + TPADI is maxed out, set TPAlive to zero
          # TPAlive[trees.subplot[live.trees],,(t+1)][TPAmort[trees.subplot[live.trees],,(t+1)] > TPAsize[trees.subplot[live.trees],,t]] <- 0
          # 
          # # if the sum of TPADD + TPADI is maxed out, set TPAmort to the max 
          # TPAmort[trees.subplot[live.trees],,(t+1)][TPAmort[trees.subplot[live.trees],,(t+1)] > TPAsize[trees.subplot[live.trees],,t]] <- TPAsize[trees.subplot[live.trees],,t]
          # 
          #  # if the sum of TPADD + TPADI is maxed out, set TPADD to the difference between D to the max 
          # TPADD[trees.subplot[live.trees],,(t+1)][TPAmort[trees.subplot[live.trees],,(t+1)] > TPAsize[trees.subplot[live.trees],,t]] <-  TPADD[trees.subplot[live.trees],,(t)] + (TPAsize[trees.subplot[live.trees],,t] - TPAmort[trees.subplot[live.trees],,(t)])
          # 
          
          # for trees that are already dead:
          TPADD[trees.subplot[!live.trees],,t+1] <- TPADD[trees.subplot[!live.trees],,t]#TPAsize[trees.subplot[!live.trees],,t] #TPADD[trees.subplot[!live.trees],,t] # keep tracker for dead trees
          
          
        }
      }else{
        # if there is no mortality just walk the trees forward
        TPADD[trees.subplot,,t+1] <- TPADD[trees.subplot,,t]
        #   TPAlive[trees.subplot,,t+1] <- TPAlive[trees.subplot,,t]
        #   TPAmort[trees.subplot,,t+1] <- TPAmort[trees.subplot,,t]
      }
    }
  }
  # before moving onto the next year, calculate the SDI
  # calculate SDI by the subplot
  
  
  
  
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
        # #pmort <-  as.vector(inv.logit(alpha + (b.growth * increment[i,,tindex+1:t])))
        # updating pmort to reflect the mean of the last ~5 years
        pmort <-  as.vector(inv.logit(alpha.mort + (b.growth * increment[i,,t]) + (b.dbh*dbh.pred[i,,t]) ))
        
        mort.prob <-  pmort
        
        mort.prob.reduced[i,,t] <- mort.prob*scale.DImort/(scale.mort.prob) # check what the distribution is of this
        mort.code <- rbinom(1,1, prob = min(1, pmort*scale.DImort))#(mort.prob.reduced[i,,t])) # to tone down, reduce mort.prob 
        
        
        # if the mort code is one, and the tree is not already dead-dead, and it was not already killed in density dependent mortality
        if(mort.code == 1  &  mean(TPAlive[i,,t], na.rm = TRUE)>0){
          
          
          if(aggressiveCC == TRUE){
            
            TPADI[i,,(t+1)] <- TPADI[i,,(t)] + (mort.prob.reduced[i,,t]*2)  #(0.2) # would also have to reduce mnort prob here
        
          }else{
            
            # if the cc is not mulitplied by 2
            TPADI[i,,(t+1)] <- TPADI[i,,(t)] + (mort.prob.reduced[i,,t])  #(0.2) # would also have to reduce mnort prob here
            
          }
          
        }else{
          # just carry over the values from the previous year
          TPADI[i,,(t+1)] <- TPADI[i,,(t)]
          TPAlive[i,,(t+1)] <- TPAlive[i,,(t)]
          # issue--if there is no TPADD, then we need to just carry over the values
          # but if there is TPADD, there will be an updated TPAmort
          TPAmort[i,,(t+1)] <- TPAmort[i,,t]
        }
        
        
        # finally for each tree cacluate the total mortality:
        TPAmort[i,,t+1] <- TPADI[i,,t+1] + TPADD[i,,t+1]
        TPAlive[i,,(t+1)] <- TPAlive[i,,t] - (TPAmort[i,,t+1]-TPAmort[i,,t])  #(0.2) # would also have to reduce mnort prob here
        
        # if the total dead is greater than the total possible dead, 
        # reconcile the values reduce DD and DI
        # the combined DI and DD mortality can't be more than TPAsize
        if(mean(TPAmort[i,,t+1], na.rm =TRUE) > mean(TPAsize[i,,t])){
          
          over.by <- ( TPAmort[i,,t+1] - TPAsize[i,,t] )
          
          DI.diff <- TPADI[i,,t+1]-TPADI[i,,t]
          DD.diff <- TPADD[i,,t+1]-TPADD[i,,t]
          
          # reduce each of the types of mortality by a weighted average of how much we are over by
          if(mean(DI.diff)==0 & !mean(DD.diff)==0){
            TPADI[i,,(t+1)] <- TPADI[i,,(t+1)]
            TPADD[i,,(t+1)] <- TPADD[i,,(t+1)] - over.by
          }
            if(mean(DD.diff)==0 & ! mean(DI.diff)==0){
              TPADD[i,,(t+1)] <- TPADD[i,,(t+1)]
              TPADI[i,,(t+1)] <- TPADI[i,,(t+1)] - over.by
              }
          
          if(!mean(DD.diff)==0 & !mean(DI.diff)==0){
          TPADI[i,,(t+1)] <- TPADI[i,,(t+1)] - (DI.diff/(DI.diff + DD.diff))*over.by
          
          TPADD[i,,(t+1)] <- TPADD[i,,(t+1)] - (DD.diff/(DI.diff + DD.diff))*over.by
          }
          # now update the totals for live and mort
          TPAlive[i,,(t+1)] <- 0
          TPAmort[i,,(t+1)] <- TPADI[i,,(t+1)] + TPADD[i,,(t+1)] # should be equal to TPAsize
        }
      }
      
      if(mean(TPAlive[i,,t+1], na.rm =TRUE) <= 0){ # if all the representative trees are dead, 
        TPAlive[i,,(t+1):nt] <- 0
        # set TPADI as equal to the the max value :
        
        TPADI[i,,(t+1):nt] <- TPADI[i,,t]
        TPADD[i,,(t):nt] <- TPADI[i,,t]
        TPAmort[i,,(t):nt] <- TPAmort[i,,t]
        # set DBH == 0? Or keep DBH == the last living value?
        
        dbh.pred[i,,(t+1):nt] <- dbh.pred[i,,t]
      }
    }
  } 
  
  
  # recaluclate SDI now
  for(s in sdi.subp[,1]){ # for each subplot s in the # of subplots
    # need to index dbh.pred trees by by the subplot:
    trees.subplot <- as.numeric(rownames(index.df[index.df$SUBP == s,]))
    if(length(trees.subplot)<=1){
      TPAcalc <- mean(TPAlive[trees.subplot,,(t+1)])
    }else{
      TPAcalc <- rowMeans(TPAlive[trees.subplot,,(t+1)])
    }
    # recalculated scaled SDI
    sdi.subp[which(sdi.subp[,1]==s),t+2] <-  calc.sdi.subp.simple(x = dbh.pred[trees.subplot,,t], scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all, TPAcalc = TPAcalc)
    
    # put sdi back on the original scale
    sdi.subp.raw[which(sdi.subp.raw[,1]==s),t+2] <- rescale.sdi(sdi.subp[which(sdi.subp[,1]==s),t+2], scale.by.mean = SDI.mean.all, scale.by.sd = SDI.sd.all)
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



  cc.scenario <- "singleCC"


dbh.quants <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
colnames(dbh.quants) <- c("quantile","treeno","time", "diameter")


dbh.quants.spread <- dbh.quants %>% group_by(treeno, time) %>% spread(quantile, diameter)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.df$treeno <- 1:length(index.df$CN)

dbh.means.index <- left_join(dbh.quants.spread , index.df, by = "treeno")

p.dbh <- ggplot()+geom_line(data = dbh.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno)) +
  geom_ribbon(data = dbh.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
  theme_bw() + ylab("Diameters (cm)")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
#p.dbh
# TPA of live trees
TPA.quants <- reshape2::melt(apply(TPAlive, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.quants) <- c("treeno","time", "TPA")

#ggplot(TPA.quants, aes(x = time, y = TPA, group = treeno, color = treeno))+geom_line()
# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.df$treeno <- 1:length(index.df$CN)

tpa.means.index <- left_join(TPA.quants, index.df, by = "treeno")

# do the same for the TPAdd and TPAdi:
TPA.DD.quants <- reshape2::melt(apply(TPADD, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.DD.quants) <- c("treeno","time", "TPA")

#ggplot(TPA.DD.quants, aes(x = time, y = TPA, group = treeno, color = treeno))+geom_line()

# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.df$treeno <- 1:length(index.df$CN)

tpa.DD.means.index <- left_join(TPA.DD.quants, index.df, by = "treeno")


TPA.DI.quants <- reshape2::melt(apply(TPADI, c(1,3), function(x){quantile(x,c(0.5), na.rm = TRUE)}))
colnames(TPA.DI.quants) <- c("treeno","time", "TPA")


#ggplot(TPA.DI.quants, aes(x = time, y = TPA, group = treeno, color = treeno))+geom_line()


TPA.DI.quants %>% filter(time %in% 99)
# tpa.quants.spread <- TPA.quants %>% group_by(treeno, time) %>% spread(quantile, TPA)
# dbh.means <- reshape2::melt(apply(dbh.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(dbh.means) <- c("treeno","time", "diameter")
index.df$treeno <- 1:length(index.df$CN)

tpa.DI.means.index <- left_join(TPA.DI.quants, index.df, by = "treeno")


#ggplot()+geom_line(data = tpa.means.index, aes(x = time, y = TPA, group = treeno, color = as.character(treeno))) 

# join with the diameter 
dbh.means.TPA.index<- left_join(tpa.means.index[,c("treeno", "time", "TPA")], dbh.means.index, by = c("treeno", "time"))
dbh.means.TPA.index$med.scaled.DBH <- dbh.means.TPA.index$`50%`*dbh.means.TPA.index$TPA
dbh.means.TPA.index$hi.scaled.DBH <- dbh.means.TPA.index$`97.5%`*dbh.means.TPA.index$TPA
dbh.means.TPA.index$lo.scaled.DBH <- dbh.means.TPA.index$`2.5%`*dbh.means.TPA.index$TPA


p.diam.tpa <- ggplot()+geom_line(data = dbh.means.TPA.index, aes(x = time, y = med.scaled.DBH, color = as.character(SUBP), group = treeno)) +
  geom_ribbon(data = dbh.means.TPA.index, aes(x = time, ymin = lo.scaled.DBH, ymax = hi.scaled.DBH,fill = as.character(SUBP), group = treeno), alpha = 0.5)+
  theme_bw() + ylab("Diameters (cm)*TPA")+xlab("years after 2001") + ggtitle(paste0("Diameter forecasts (means) for plot ", plot))
#p.diam.tpa


inc.quants <- reshape2::melt(apply(increment, c(1,3), function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
colnames(inc.quants) <- c("quantile","treeno","time", "increment")


inc.quants.spread <- inc.quants %>% group_by(treeno, time) %>% spread(quantile, increment)
# inc.means <- reshape2::melt(apply(inc.pred, c(1,3), function(x){mean(x, na.rm = TRUE)}))
# colnames(inc.means) <- c("treeno","time", "diameter")
index.df$treeno <- 1:length(index.df$CN)

inc.means.index <- left_join(inc.quants.spread , index.df, by = "treeno")

p.inc <- ggplot() +
  geom_ribbon(data = inc.means.index, aes(x = time, ymin = `2.5%`, ymax = `97.5%`, fill = as.character(SUBP), group = treeno), alpha = 0.25)+
  geom_line(data = inc.means.index, aes(x = time, y = `50%`, color = as.character(SUBP), group = treeno))+
  theme_bw() + ylab("Increments (cm)")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))+ labs(fill = "Subplot Number", color = "Subplot Number")

#ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.increment.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p.inc)

#p.inc

# plot out the SDI values

SDI.values  <- reshape2::melt(sdi.subp.raw[,2:100]) # ,  function(x){quantile(x,c(0.025,0.5,0.975), na.rm = TRUE)}))
if(nrow(sdi.subp.raw) == 1){
  SDI.values <- data.frame(Var2 = 2001:2099, 
                           value = SDI.values$value, 
                           Var1 = 1)
}
p.SDI <- ggplot() +
  geom_line(data = SDI.values, aes(x = Var2, y = value, color = as.character(Var1)))+
  theme_bw() + ylab("Subplot SDI")+xlab("years after 2018") + ggtitle(paste0("Increment forecasts (means) for plot ", plot))+ labs(fill = "Subplot Number", color = "Subplot Number")+
  geom_hline(yintercept = (450/4)*0.6, color = "darkgrey", linetype = "dashed")

#ggsave(paste0("data/output/plotDBHforecasts_zeroinc_stochastic_sdimort/plot/PLT.increment.",mort.scheme,".", plot,".", scenario,".2001.2018.png"), p.inc)

#p.SDI

combined <- index.df

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
cat("start biomass estimates full model")
full <- plot2AGB(combined = combined, 
                 out = out.mean, 
                 tpa = tpa.mean, 
                 tpa.diff = tpa.diff.mean, 
                 tpa.dd = tpa.DD.means.index, 
                 tpa.di = tpa.DI.means.index, 
                 mort.scheme = mort.scheme, 
                 allom.stats = kaye_pipo, 
                 unit.conv = 0, 
                 plot = plot, 
                 yrvec = 2001:2098, 
                 scenario = scenario,
                 cc.scenario = cc.scenario, 
                 p = NULL, 
                 p.inc = NULL, 
                 SDI.ratio.DD = SDI.ratio.DD, 
                 plt.design = "periodic", 
                 folder.name = paste0("biomass_dataFIAperiodic_",scale.mort.prob), 
                 parse.type = "full", 
                 mort.prob.reduced  = mort.prob.reduced)



return(list(forecast = full,
            p.inc = p.inc, 
            p.dbh = p.dbh,
            p.diam.tpa = p.diam.tpa, 
            p.SDI = p.SDI))
}
