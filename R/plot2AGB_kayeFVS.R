plot2AGB <- function(combined, out, tpa , tpa.diff , tpa.di, tpa.dd, mort.scheme, allom.stats, unit.conv = 0.02, plot = plot, yrvec = 2001:2098, scenario = "rcp26",cc.scenario = "singleCC", p = p, p.inc = p.inc, SDI.ratio.DD, plt.design = "periodic", folder.name, parse.type, mort.prob.reduced) {
  
  ## Jenkins: hemlock (kg) b0 <- -2.5384 b1 <- 2.4814
  
  ## Allometric statistics
  # for stem wood (4)
  b0.stemwood   <- kaye_pipo[[1]][[4]]$statistics["Bg0", "Mean"]
  b1.stemwood   <- kaye_pipo[[1]][[4]]$statistics["Bg1", "Mean"]
  B.stemwood    <- kaye_pipo[[1]][[4]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.stemwood <- kaye_pipo[[1]][[4]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.stemwood  <- sqrt(kaye_pipo[[1]][[4]]$statistics["Sg", "Mean"])
  
  # for stem bark (5)
  b0.stembark   <- kaye_pipo[[1]][[5]]$statistics["Bg0", "Mean"]
  b1.stembark   <- kaye_pipo[[1]][[5]]$statistics["Bg1", "Mean"]
  B.stembark    <- kaye_pipo[[1]][[5]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.stembark <- kaye_pipo[[1]][[5]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.stembark  <- sqrt(kaye_pipo[[1]][[5]]$statistics["Sg", "Mean"])
  
  # for branch live (8)
  b0.branchlive   <- kaye_pipo[[1]][[8]]$statistics["Bg0", "Mean"]
  b1.branchlive   <- kaye_pipo[[1]][[8]]$statistics["Bg1", "Mean"]
  B.branchlive    <- kaye_pipo[[1]][[8]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.branchlive <- kaye_pipo[[1]][[8]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.branchlive  <- sqrt(kaye_pipo[[1]][[8]]$statistics["Sg", "Mean"])
  
  # for branch dead (12)
  b0.branchdead   <- kaye_pipo[[1]][[12]]$statistics["Bg0", "Mean"]
  b1.branchdead   <- kaye_pipo[[1]][[12]]$statistics["Bg1", "Mean"]
  B.branchdead    <- kaye_pipo[[1]][[12]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.branchdead <- kaye_pipo[[1]][[12]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.branchdead  <- sqrt(kaye_pipo[[1]][[12]]$statistics["Sg", "Mean"])
  
  # for foliage (15)
  b0.foliage   <- kaye_pipo[[1]][[18]]$statistics["Bg0", "Mean"]
  b1.foliage   <- kaye_pipo[[1]][[18]]$statistics["Bg1", "Mean"]
  B.foliage    <- kaye_pipo[[1]][[18]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.foliage <- kaye_pipo[[1]][[18]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.foliage  <- sqrt(kaye_pipo[[1]][[18]]$statistics["Sg", "Mean"])
  
  # for dead stemwood: # just use stem wood to calculate dead stemwood
  b0.dead.stemwood   <- kaye_pipo[[1]][[4]]$statistics["Bg0", "Mean"]
  b1.dead.stemwood   <- kaye_pipo[[1]][[4]]$statistics["Bg1", "Mean"]
  B.dead.stemwood    <- kaye_pipo[[1]][[4]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov.dead.stemwood <- kaye_pipo[[1]][[4]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd.dead.stemwood  <- sqrt(kaye_pipo[[1]][[4]]$statistics["Sg", "Mean"])
  
  
  
  ## prep data: 
  out[out < 0.1] <- 0.1
  #out.dead[is.na(out.dead)] <- 0
  nrep    <- nrow(out)
  ntree   <- nrow(combined)
  nt      <- ncol(out) / ntree
  mplot   <- 1  ## later need to generalize to splitting up plots
  ijindex <- matrix(1, ntree, 1)
  
  if(length(combined) >6){
    
    yrvec   <- as.numeric(colnames(combined))
  }else{
    yrvec <- yrvec
  }
  
  
  yrvec <- yrvec[!is.na(yrvec)]
  
  ## set up storage
  NPP<- NPP.stemwood <- NPP.stembark <- NPP.branchlive <- NPP.branchdead <- NPP.foliage <- NPP.dead <- array(NA, c(mplot, nrep, nt))
  AGB<- AGB.stemwood <- AGB.stembark <- AGB.branchlive <- AGB.branchdead <- AGB.foliage <- AGB.dead <-  array(NA, c(mplot, nrep, nt))
  biomass.stemwood <- array(NA, c(nrep,  ntree, nt))
  biomass.stembark <- array(NA, c(nrep,  ntree, nt))
  biomass.branchlive <- array(NA, c(nrep,  ntree, nt))
  biomass.branchdead <- array(NA, c(nrep,  ntree, nt))
  biomass.foliage <- array(NA, c(nrep,  ntree, nt))
  biomass.dead <-array(NA, c(nrep,  ntree, nt))
  
  diam.dead <- array(NA, c(nrep,  ntree, nt))
  diam.live <- array(NA, c(nrep,  ntree, nt))
  tpa.dead <- array(NA, c(nrep,  ntree, nt))
  tpa.live <- array(NA, c(nrep,  ntree, nt))
  #biomass_acsa3 <- array(NA, c(mplot, nrep, nt))
  #biomass_beal2 <- array(NA, c(mplot, nrep, nt))
  #biomass_thoc2 <- array(NA, c(mplot, nrep, nt))
  
  ## sample over tree chronologies
  cat ("calculating biomass: percent complete")
  pb <- txtProgressBar(min = 0, max = nrep, style = 3)
  for (g in seq_len(nrep)) {
    # g <- 1
    j <- 1
    ## Draw allometries
    b.stemwood <- mvtnorm::rmvnorm(1, B.stemwood, Bcov.stemwood)
    b.stembark <- mvtnorm::rmvnorm(1, B.stembark, Bcov.stembark)
    b.branchlive <- mvtnorm::rmvnorm(1, B.branchlive, Bcov.branchlive)
    b.branchdead <- mvtnorm::rmvnorm(1, B.branchdead, Bcov.branchdead)
    b.foliage <- mvtnorm::rmvnorm(1, B.foliage, Bcov.foliage)
    b.dead <- mvtnorm::rmvnorm(1, B.dead.stemwood, Bcov.dead.stemwood)
    
    ## convert tree diameter to biomass
    biomass.stemwood[g,,] <- matrix(exp(b.stemwood[1] + b.stemwood[2] * log(out[g, ])), ntree,nt,  byrow = FALSE)#nt)
    biomass.stembark[g,,] <- matrix(exp(b.stembark[1] + b.stembark[2] * log(out[g, ])), ntree, nt, byrow = FALSE)#nt)
    biomass.branchlive[g,,] <- matrix(exp(b.branchlive[1] + b.branchlive[2] * log(out[g, ])), ntree, nt, byrow = FALSE)#nt)
    biomass.branchdead[g,,] <- matrix(exp(b.branchdead[1] + b.branchdead[2] * log(out[g, ])), ntree, nt, byrow = FALSE)#nt)
    biomass.foliage[g,,] <- matrix(exp(b.foliage[1] + b.foliage[2] * log(out[g, ])), ntree, nt, byrow = FALSE)#nt)
    biomass.dead[g,,] <- matrix(exp(b.dead[1] + b.dead[2] * log(out[g, ])), ntree, nt, byrow = FALSE)#nt)
    
    diam.dead[g,,] <- matrix(out[g, ], ntree, nt, byrow = FALSE)#nt)
    diam.live[g,,] <- matrix(out[g, ], ntree, nt, byrow = FALSE)#nt)
    tpa.live[g,,] <- matrix(tpa[g, ], ntree, nt, byrow = FALSE)#nt)
    tpa.dead[g,,] <- matrix(tpa.diff[g, ], ntree, nt, byrow = FALSE)#nt)
    
    
    #for (j in seq_len(mplot)) {
    
    # system.time(apply(apply(biomass.stemwood,2, as.numeric), 2, sum, na.rm = TRUE)*unit.conv)
    #system.time(colSums(apply(biomass.stemwood,2, as.numeric), na.rm = TRUE) * unit.conv)
    
    ## aggregate to stand AGB
    
    AGB.stemwood[j, g, ] <- apply(biomass.stemwood[g,,]*tpa.live[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    AGB.stembark[j, g, ] <- apply(biomass.stembark[g,,]*tpa.live[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    AGB.branchlive[j, g, ] <- apply(biomass.branchlive[g,,]*tpa.live[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    AGB.branchdead[j, g, ] <- apply(biomass.branchdead[g,,]*tpa.live[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    AGB.foliage[j, g, ] <- apply(biomass.foliage[g,,]*tpa.live[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    AGB [j, g, ] <- AGB.stemwood[j, g, ] + AGB.stembark[j, g, ] + AGB.branchlive[j, g, ] + AGB.branchdead[j, g, ] +  AGB.foliage[j, g, ]
    
    AGB.dead[j, g, ] <- apply(biomass.dead[g,,]*tpa.dead[g,,], 2, FUN = function(x){sum(as.numeric(x), na.rm = TRUE)})
    
    
    # AGB[j,g,] <- apply(biomass[ijindex[,1]==j,],2,sum,na.rm=TRUE)*unit.conv
    
    #biomass_tsca[j, g, ] <- apply(biomass[combined$SPP == "TSCA", ], 2, sum, na.rm = TRUE) * unit.conv
    #biomass_acsa3[j, g, ] <- apply(biomass[combined$SPP == "ACSA3", ], 2, sum, na.rm = TRUE) * unit.conv
    #biomass_beal2[j, g, ] <- apply(biomass[combined$SPP == "BEAL2", ], 2, sum, na.rm = TRUE) * unit.conv
    #biomass_thoc2[j, g, ] <- apply(biomass[combined$SPP == "THOC2", ], 2, sum, na.rm = TRUE) * unit.conv
    
    ## diff to get NPP
    NPP.stemwood[j, g, ] <- c(0,diff(AGB.stemwood[j, g, ]))
    NPP.stembark[j, g, ] <- c(0,diff(AGB.stembark[j, g, ]))
    NPP.branchlive[j, g, ] <- c(0,diff(AGB.branchlive[j, g, ]))
    NPP.branchdead[j, g, ] <- c(0,diff(AGB.branchdead[j, g, ]))
    NPP.foliage[j, g, ] <- c(0,diff(AGB.foliage[j, g, ]))
    NPP[j, g,] <- c(0,diff(AGB[j,g,]))
    NPP.dead[j, g,] <- c(0,diff(AGB.dead[j,g,]))
    
    #}
    setTxtProgressBar(pb, g)
  }
  
  mAGB.dead <- sAGB.dead <- mAGB <- sAGB <- mAGB.stemwood <- mAGB.stembark <- mAGB.branchlive <- mAGB.branchdead <- mAGB.foliage<- sAGB.stemwood <- sAGB.stembark <- sAGB.branchlive <- sAGB.branchdead <- sAGB.foliage<- matrix(NA, mplot, nt)
  mNPP.dead<- sNPP.dead <- mNPP <- sNPP <- mNPP.stemwood <-  mNPP.stembark <-  mNPP.branchlive <-  mNPP.branchdead<- mNPP.foliage <-  sNPP.stemwood <-  sNPP.stembark <-  sNPP.branchlive <-  sNPP.branchdead <-  sNPP.foliage<- matrix(NA, mplot,nt)
  lowAGB <- lowAGB.stemwood <- lowAGB.stembark <- lowAGB.branchlive <- lowAGB.branchdead <- lowAGB.foliage<- hiAGB <- hiAGB.stemwood <- hiAGB.stembark <- hiAGB.branchlive <- hiAGB.branchdead <- hiAGB.foliage <- lowAGB.dead<- hiAGB.dead  <- matrix(NA, mplot, nt)
  hiNPP <- hiNPP.stemwood <-  hiNPP.stembark <-  hiNPP.branchlive <-  hiNPP.branchdead<- hiNPP.foliage <-   lowNPP <- lowNPP.stemwood <-  lowNPP.stembark <-  lowNPP.branchlive <-  lowNPP.branchdead<- lowNPP.foliage <- lowNPP.dead<- hiNPP.dead  <-matrix(NA, mplot,nt)
  
  #mbiomass_tsca  <- sbiomass_tsca <- matrix(NA, mplot, nt)
  #mbiomass_acsa3 <- sbiomass_acsa3 <- matrix(NA, mplot, nt)
  #mbiomass_beal2 <- sbiomass_beal2 <- matrix(NA, mplot, nt)
  #mbiomass_thoc2 <- sbiomass_thoc2 <- matrix(NA, mplot, nt)
  
  for (i in seq_len(mplot)) {
    mNPP[i, ] <- apply( NPP[i, , ], 2, median, na.rm = TRUE)
    sNPP[i, ] <- apply(NPP[i, , ], 2, sd, na.rm = TRUE)
    lowNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP[i,]<- apply(NPP[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB[i, ] <- apply(AGB[i, , ], 2, median, na.rm = TRUE)
    sAGB[i, ] <- apply(AGB[i, , ], 2, sd, na.rm = TRUE)
    lowAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB[i,]<- apply(AGB[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    # sequentially add up:
    # branchdead, then foliage, then stembark, then branchlive, then stemwood
    
    # branch dead
    mNPP.branchdead[i, ] <- apply( NPP.branchdead[i, , ], 2, median, na.rm = TRUE)
    sNPP.branchdead[i, ] <- apply(NPP.branchdead[i, , ], 2, sd, na.rm = TRUE)
    lowNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.branchdead[i,]<- apply(NPP.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, median, na.rm = TRUE)
    sAGB.branchdead[i, ] <- apply(AGB.branchdead[i, , ], 2, sd, na.rm = TRUE)
    lowAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.branchdead[i,]<- apply(AGB.branchdead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    # foliage
    mNPP.foliage[i, ] <- apply( NPP.foliage[i, , ] , 2, median, na.rm = TRUE)
    sNPP.foliage[i, ] <- apply(NPP.foliage[i, , ] , 2, sd, na.rm = TRUE)
    lowNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.foliage[i,]<- apply(NPP.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, mean, na.rm = TRUE)
    sAGB.foliage[i, ] <- apply(AGB.foliage[i, , ] , 2, sd, na.rm = TRUE)
    lowAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.foliage[i,]<- apply(AGB.foliage[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    # stembark
    mNPP.stembark[i, ] <- apply( NPP.stembark[i, , ] , 2, median, na.rm = TRUE)
    sNPP.stembark[i, ] <- apply(NPP.stembark[i, , ] , 2, sd, na.rm = TRUE)
    lowNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.stembark[i,]<- apply(NPP.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, median, na.rm = TRUE)
    sAGB.stembark[i, ] <- apply(AGB.stembark[i, , ] , 2, sd, na.rm = TRUE)
    lowAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.stembark[i,]<- apply(AGB.stembark[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    # branchlive
    mNPP.branchlive[i, ] <- apply( NPP.branchlive[i, , ] , 2, median, na.rm = TRUE)
    sNPP.branchlive[i, ] <- apply(NPP.branchlive[i, , ], 2, sd, na.rm = TRUE)
    lowNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.branchlive[i,]<- apply(NPP.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, median, na.rm = TRUE)
    sAGB.branchlive[i, ] <- apply(AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
    lowAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.branchlive[i,]<- apply(AGB.branchlive[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    
    # stemwood
    mNPP.stemwood[i, ] <- apply( NPP.stemwood[i, , ] , 2, median, na.rm = TRUE)
    sNPP.stemwood[i, ] <- apply(NPP.stemwood[i, , ], 2, sd, na.rm = TRUE)
    lowNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.stemwood[i,]<- apply(NPP.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ], 2, median, na.rm = TRUE)
    sAGB.stemwood[i, ] <- apply(AGB.stemwood[i, , ] + AGB.branchlive[i, , ] , 2, sd, na.rm = TRUE)
    
    lowAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.stemwood[i,]<- apply(AGB.stemwood[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    
    # dead trees:
    # stemwood
    mNPP.dead[i, ] <- apply( NPP.dead[i, , ] , 2, median, na.rm = TRUE)
    sNPP.dead[i, ] <- apply(NPP.dead[i, , ], 2, sd, na.rm = TRUE)
    lowNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiNPP.dead[i,]<- apply(NPP.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    mAGB.dead[i, ] <- apply(AGB.dead[i, , ], 2, median, na.rm = TRUE)
    sAGB.dead[i, ] <- apply(AGB.dead[i, , ] , 2, sd, na.rm = TRUE)
    
    lowAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.025)
    hiAGB.dead[i,]<- apply(AGB.dead[i, , ], 2, quantile, na.rm = TRUE, 0.975)
    
    
  }
  
  #pdf(file.path(outfolder, "plot2AGB_test.pdf"))
  #par(mfrow = c(2, 1))
  
  # for (i in seq_len(mplot)) {
  
  up  <- hiNPP
  low <- lowNPP
  
  up.deadstem  <- hiNPP.dead
  low.deadstem <- lowNPP.dead
  
  up.stemwood  <- hiNPP.stemwood
  low.stemwood <- lowNPP.stemwood
  
  up.stembark  <- hiNPP.stembark
  low.stembark <- lowNPP.stembark
  
  up.branchlive  <- hiNPP.branchlive
  low.branchlive  <- hiNPP.branchlive
  
  up.branchdead  <- hiNPP.branchdead
  low.branchdead  <- hiNPP.branchdead
  
  up.foliage  <- hiNPP.foliage
  low.foliage   <- lowNPP.foliage
  
  # plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "kg/acre/yr", xlab = "year")
  # lines(yrvec[-1], up)
  # lines(yrvec[-1], low)
  upA  <- hiAGB
  lowA <- lowAGB
  
  upA.deadstem  <- hiAGB.dead
  lowA.deadstem <- lowAGB.dead
  
  upA.stemwood  <- hiAGB.stemwood
  lowA.stemwood <- lowAGB.stemwood
  
  upA.stembark  <- hiAGB.stembark
  lowA.stembark <- lowAGB.stembark
  
  upA.branchlive  <- hiAGB.branchlive
  lowA.branchlive  <- lowAGB.branchlive
  
  upA.branchdead  <- hiAGB.branchdead
  lowA.branchdead  <- lowAGB.branchdead
  
  upA.foliage  <- hiAGB.foliage
  lowA.foliage   <- lowAGB.foliage
  
  # plot(yrvec, mAGB[i, ], ylim = range(c(upA, lowA)), ylab = "kg/acre", xlab = "year")
  # lines(yrvec, upA)
  # lines(yrvec, lowA)
  # }
  #grDevices::dev.off()
  
  # make nicer plots for each plot:
  i <- 1
  # calculate upper and lower bounds
  up  <- hiNPP
  low <- lowNPP
  
  upA  <- hiAGB
  lowA <- lowAGB
  
  
  total.plot <- data.frame(plot = plot, 
                           year = yrvec[2:length(low.stemwood)], 
                           mAGB = mAGB[i,2:length(low.stemwood)], 
                           mAGB.stemwood = mAGB.stemwood[i,2:length(low.stemwood)],
                           mAGB.stembark = mAGB.stembark[i,2:length(low.stemwood)],
                           mAGB.branchlive = mAGB.branchlive[i,2:length(low.stemwood)],
                           mAGB.branchdead = mAGB.branchdead[i,2:length(low.stemwood)],
                           mAGB.foliage = mAGB.foliage[i,2:length(low.stemwood)],
                           mAGB.dead  = mAGB.dead[i,2:length(low.stemwood)],
                           upA = upA[2:length(low.stemwood)], 
                           lowA = lowA[2:length(low.stemwood)], 
                           upA.stemwood = upA.stemwood[2:length(low.stemwood)],
                           upA.stembark = upA.stembark[2:length(low.stemwood)],
                           upA.branchlive = upA.branchlive[2:length(low.stemwood)],
                           upA.branchdead = upA.branchdead[2:length(low.stemwood)],
                           upA.foliage = upA.foliage[2:length(low.stemwood)],
                           upA.dead = upA.deadstem[2:length(low.stemwood)], 
                           
                           lowA.stemwood = lowA.stemwood[2:length(low.stemwood)],
                           lowA.stembark = lowA.stembark[2:length(low.stemwood)],
                           lowA.branchlive = lowA.branchlive[2:length(low.stemwood)],
                           lowA.branchdead = lowA.branchdead[2:length(low.stemwood)],
                           lowA.foliage = lowA.foliage[2:length(low.stemwood)],
                           lowA.dead = lowA.deadstem[2:length(low.stemwood)], 
                           
                           mNPP = mNPP[i,2:length(low.stemwood)], 
                           mNPP.stemwood = mNPP.stemwood[2:length(low.stemwood)],
                           mNPP.stembark =mNPP.stembark[2:length(low.stemwood)],
                           mNPP.branchlive =mNPP.branchlive[2:length(low.stemwood)],
                           mNPP.branchdead = mNPP.branchdead[2:length(low.stemwood)],
                           mNPP.foliage = mNPP.foliage[2:length(low.stemwood)],  
                           mNPP.dead = mNPP.dead[2:length(low.stemwood)], 
                           
                           up = up[2:length(low.stemwood)], 
                           low = low[2:length(low.stemwood)], 
                           
                           up.dead = up.deadstem[2:length(low.stemwood)], 
                           low.dead = low.deadstem[2:length(low.stemwood)],
                           
                           up.stemwood = up.stemwood[2:length(low.stemwood)],
                           up.stembark = up.stembark[2:length(low.stemwood)],
                           up.branchlive = up.branchlive[2:length(low.stemwood)],
                           up.branchdead =  up.branchdead[2:length(low.stemwood)],
                           up.foliage =up.foliage[2:length(low.stemwood)],
                           
                           low.stemwood = low.stemwood[2:length(low.stemwood)],
                           low.stembark = low.stembark[2:length(low.stemwood)],
                           low.branchlive = low.branchlive[2:length(low.stemwood)],
                           low.branchdead = low.branchdead[2:length(low.stemwood)],
                           low.foliage = low.foliage[2:length(low.stemwood)])
  
  
  # calculate totals for plotting purposes: AGB
  upA.stem <- upA.stemwood[2:length(low.stemwood)]+(upA.stembark[2:length(low.stemwood)]+upA.branchlive[2:length(low.stemwood)] + upA.branchdead[2:length(low.stemwood)]+ upA.foliage[2:length(low.stemwood)])
  lowA.stem <- lowA.stemwood[2:length(low.stemwood)]+(lowA.stembark[2:length(low.stemwood)]+lowA.branchlive[2:length(low.stemwood)] + lowA.branchdead[2:length(low.stemwood)]+ lowA.foliage[2:length(low.stemwood)])
  
  upA.bark <- upA.stem-(upA.stembark[2:length(low.stemwood)]+upA.branchlive[2:length(low.stemwood)] + upA.branchdead[2:length(low.stemwood)]+ upA.foliage[2:length(low.stemwood)])
  lowA.bark <- lowA.stem - (lowA.stembark[2:length(low.stemwood)]+lowA.branchlive[2:length(low.stemwood)] + lowA.branchdead[2:length(low.stemwood)]+ lowA.foliage[2:length(low.stemwood)])
  
  upA.branch <- upA.bark-(upA.branchlive[2:length(low.stemwood)] + upA.branchdead[2:length(low.stemwood)]+ upA.foliage[2:length(low.stemwood)])
  lowA.branch <- lowA.bark-(lowA.branchlive[2:length(low.stemwood)] + lowA.branchdead[2:length(low.stemwood)]+ lowA.foliage[2:length(low.stemwood)])
  
  upA.dead <- upA.branch-( upA.branchdead[2:length(low.stemwood)]+ upA.foliage[2:length(low.stemwood)])
  lowA.dead <- lowA.branch-( lowA.branchdead[2:length(low.stemwood)]+ lowA.foliage[2:length(low.stemwood)])
  
  upA.needles <- upA.dead-(upA.foliage[2:length(low.stemwood)])
  lowA.needles <- lowA.dead-(lowA.foliage[2:length(low.stemwood)])
  
  
  
  # calculate totals for plotting purposes: NPP
  up.stem <- up.stemwood[2:length(low.stemwood)]+(up.stembark[2:length(low.stemwood)]+up.branchlive[2:length(low.stemwood)] + up.branchdead[2:length(low.stemwood)]+ up.foliage[2:length(low.stemwood)])
  low.stem <- low.stemwood[2:length(low.stemwood)]+(low.stembark[2:length(low.stemwood)]+low.branchlive[2:length(low.stemwood)] + low.branchdead[2:length(low.stemwood)]+ low.foliage[2:length(low.stemwood)])
  
  up.bark <- up.stem-(up.stembark[2:length(low.stemwood)]+up.branchlive[2:length(low.stemwood)] + up.branchdead[2:length(low.stemwood)]+ up.foliage[2:length(low.stemwood)])
  low.bark <- low.stem - (low.stembark[2:length(low.stemwood)]+low.branchlive[2:length(low.stemwood)] + low.branchdead[2:length(low.stemwood)]+ low.foliage[2:length(low.stemwood)])
  
  up.branch <- up.bark-(up.branchlive[2:length(low.stemwood)] + up.branchdead[2:length(low.stemwood)]+ up.foliage[2:length(low.stemwood)])
  low.branch <- low.bark-(low.branchlive[2:length(low.stemwood)] + low.branchdead[2:length(low.stemwood)]+ low.foliage[2:length(low.stemwood)])
  
  up.dead <- up.branch-( up.branchdead[2:length(low.stemwood)]+ up.foliage[2:length(low.stemwood)])
  low.dead <- low.branch-( low.branchdead[2:length(low.stemwood)]+ low.foliage[2:length(low.stemwood)])
  
  up.needles <- up.dead-(up.foliage[2:length(low.stemwood)])
  low.needles <- low.dead-(low.foliage[2:length(low.stemwood)])
  
  
  
  total.plot.all <- data.frame(plot = plot, 
                               year = yrvec, #[1:length(low.stemwood)], 
                               
                               
                               # upA = upA[2:length(low.stemwood)], 
                               #lowA = lowA[2:length(low.stemwood)], 
                               upA.stemwood = upA.stem,
                               upA.stembark = upA.bark,
                               upA.branchlive = upA.branch,
                               upA.branchdead = upA.dead,
                               upA.foliage = upA.needles,
                               upA.deadstem = upA.deadstem[,2:length(upA.deadstem)],
                               
                               lowA.stemwood = as.numeric(rbind(lapply(lowA.stem, FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               lowA.stembark = as.numeric(rbind(lapply(lowA.bark, FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               lowA.branchlive = as.numeric(rbind(lapply(lowA.branch, FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               lowA.branchdead = as.numeric(rbind(lapply(lowA.dead, FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               lowA.foliage = as.numeric(rbind(lapply(lowA.needles, FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               lowA.deadstem = as.numeric(rbind(lapply(lowA.deadstem[,2:length(upA.deadstem)], FUN = function(x){max(c(0,x), na.rm = TRUE)}))),
                               
                               # up.dead = up.deadstem[2:length(low.stemwood)], 
                               # low.dead = low.deadstem[2:length(low.stemwood)],
                               up.stemwood = up.stem,
                               up.stembark = up.bark,
                               up.branchlive = up.branch,
                               up.branchdead = up.dead,
                               up.foliage = up.needles,
                               up.deadstem = up.deadstem[,2:length(upA.deadstem)],
                               
                               low.stemwood = low.stem,
                               low.stembark = low.bark,
                               low.branchlive = low.branch,
                               low.branchdead = low.dead,
                               low.foliage = low.needles, 
                               low.deadstem = low.deadstem[,2:length(upA.deadstem)])#,
  
  
  b.plot.all <- ggplot()+
    #geom_ribbon(data = total.plot, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.stemwood, ymax = upA.stemwood, fill = "stem wood"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.stembark, ymax = upA.stembark, fill = "stem bark"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.branchlive, ymax = upA.branchlive, fill = "live branch"))+
    
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.branchdead, ymax = upA.branchdead, fill = "dead branch"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.foliage, ymax = upA.foliage, fill = "foliage"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = lowA.deadstem, ymax = upA.deadstem, fill = "dead stem"))+
    
    theme_bw()+
    ylab(paste("Plot", plot, "stem  \n biomass (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+
    scale_fill_manual(name = 'Biomass Component', 
                      values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black"))
  
  
  b.flux.all <- ggplot()+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = low.stemwood, ymax = up.stemwood, fill = "stem wood"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = low.branchlive, ymax = up.branchlive, fill = "live branch"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = low.stembark, ymax = up.stembark, fill = "stem bark"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = low.branchdead, ymax = up.branchdead, fill = "dead branch"))+
    geom_ribbon(data = total.plot.all, aes(x = year, ymin = low.foliage, ymax = up.foliage, fill = "foliage"))+
    
    theme_bw()+#ylim(ylow, yhi)+
    ylab(paste("Plot", plot, " stem  \n biomass increment (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+
    scale_fill_manual(name = 'Biomass Component', 
                      values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black" ))
  
  
  
  b.plot <- ggplot()+
    #geom_ribbon(data = total.plot, aes(x = year, ymin = lowA, ymax = upA), fill = "darkseagreen4")+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.stemwood, ymax = upA.stemwood, fill = "stem wood"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.branchlive, ymax = upA.branchlive, fill = "live branch"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.stembark, ymax = upA.stembark, fill = "stem bark"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.foliage, ymax = upA.foliage, fill = "foliage"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.branchdead, ymax = upA.branchdead, fill = "dead branch"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = lowA.dead, ymax = upA.dead, fill = "dead stem"))+
    theme_bw()+
    ylab(paste("Plot", plot, "stem  \n biomass (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+
    scale_fill_manual(name = 'Biomass Component', 
                      values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1", "dead stem" = "black" ))
  
  
  
  
  # b.flux <- ggplot()+
  #   geom_ribbon(data = total.plot , aes(x = year, ymin = low, ymax = up), fill = "coral4")+
  #   geom_line(data = total.plot , aes(x = year, y = mNPP), color = "black")+theme_bw(base_size = 12)+
  #   ylab(paste("Plot", plot, " stem  \n biomass increment (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+xlim(1995, 2010)
  # 
  # 
  tota.plot.trunk <- total.plot %>% filter(year <= 2099)
  yhi <- max(  tota.plot.trunk$up.stemwood, na.rm = TRUE) + 1
  ylow <- min(  tota.plot.trunk$low.branchdead, na.rm=TRUE) - 1
  
  b.flux <- ggplot()+
    geom_ribbon(data = total.plot, aes(x = year, ymin = low.stemwood, ymax = up.stemwood, fill = "stem wood"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = low.branchlive, ymax = up.branchlive, fill = "live branch"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = low.stembark, ymax = up.stembark, fill = "stem bark"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = low.foliage, ymax = up.foliage, fill = "foliage"))+
    geom_ribbon(data = total.plot, aes(x = year, ymin = low.branchdead, ymax = up.branchdead, fill = "dead branch"))+
    theme_bw()+#xlim(2001,2018)+ylim(ylow, yhi)+
    ylab(paste("Plot", plot, " stem  \n biomass increment (kg/acre)"))+xlab("Year")+theme(panel.grid = element_blank())+
    scale_fill_manual(name = 'Biomass Component', 
                      values =c("dead branch"="grey","foliage"="#018571", "stem bark"="#a6611a","live branch"="#dfc27d","stem wood"="#80cdc1" ))
  
  
  
  #both.plot<- cowplot::plot_grid(p, b.plot, p.inc, b.flux, ncol = 2, align = "hv")
  
  cat("saving outputs")
  #cowplot::save_plot(paste0("biomass_plotsFIAannual/Plot_biomass_inc_",mort.scheme,".", plot, ".",scenario,".",cc.scenario,".png"), both.plot, base_height = 10, base_width = 12, units = "in")
  
  
  #both.plot.all<- cowplot::plot_grid(b.plot.all, b.flux, ncol = 1, align = "hv")
  
  #cat("saving outputs")
  #cowplot::save_plot(paste0("/home/rstudio/data/output/biomass_plots/Plot_biomass_total_inc_", plot, ".png"), both.plot.all, base_asp = 1.1)
  
  #saveRDS(total.plot, paste0(output, "Plot_biomass_inc_", plot, ".RDS"))
  
  save(out, #out.dead, 
       AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, plot, 
       AGB.foliage, NPP.foliage, 
       AGB.stembark, NPP.stembark,
       AGB.stemwood, NPP.stemwood,
       AGB.branchdead, NPP.branchdead,
       AGB.branchlive, NPP.branchlive,
       AGB.dead, NPP.dead,
       biomass.stemwood , 
       biomass.stembark , 
       biomass.branchlive, 
       biomass.branchdead, 
       biomass.foliage, 
       biomass.dead, 
       
       diam.dead, 
       diam.live, 
       tpa.dead, 
       tpa.live,
       tpa.dd, 
       tpa.di,
       mort.prob.reduced,
       # mbiomass_tsca, sbiomass_tsca, mbiomass_acsa3, sbiomass_acsa3, 
       # mbiomass_beal2, sbiomass_beal2, mbiomass_thoc2, sbiomass_thoc2, 
       file = file.path(paste0(folder.name ,"/plot2AGB_",mort.scheme,".",plot,".",scenario,".", SDI.ratio.DD,".",cc.scenario,".", parse.type, ".Rdata")))
  
  
  return(total.plot )#, 
  # biomass_tsca = biomass_tsca, biomass_acsa3 = biomass_acsa3, 
  # biomass_beal2 = biomass_beal2, biomass_thoc2 = biomass_thoc2))
}


