# checking density independent mortality distributions
library(tidyverse)
# read in the mort.prob.reduced values for all predictions (just the full for now)


get.mort.prob.reduced <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" ){
  
  cat(paste0("getting mort.prob for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))){
      cat("no existing future climate data") 
    }else{
     
   load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
   #dim(mort.prob.reduced)     # dimensions = 16x100x102 = # trees X nsamps X # years
   mort.prob.m <- reshape2::melt(mort.prob.reduced)
    mort.prob.m$plot <- plot
    mort.prob.m$rcp <- rcp
    mort.prob.m$mort.scheme <- mort.scheme
    mort.prob.m$parse <- "full"
    mort.prob.m$value <- ifelse(is.na(mort.prob.m$value), 0, mort.prob.m$value)
    mort.prob.tree.time <- mort.prob.m %>% group_by(Var1, Var3, plot, rcp, parse) %>% summarise(mortP = mean(value, na.rm = TRUE))
    
    summary(tpa.dd)
    summary(tpa.di)
    mort.prob.m <- reshape2::melt(mort.prob.reduced)
    
        mort.prob.tree.time
    }
  }
}
x.mat <- readRDS("outputs/x.mat.RDS")
plots <- unique(x.mat$PLT_CN)#[1]

unique(plots) %in% 2500345010690
 plot <- 2500345010690
DIDD.AGB <- lapply(unique(plots)[1:675], FUN = get.mort.prob.reduced, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" )
mort.prob <- do.call(rbind, DIDD.AGB)


head(mort.prob)
length(unique(mort.prob$Var1)) # ntree
#length(unique(mort.prob$Var2)) # nsamp
length(unique(mort.prob$Var3)) # time

# now get the plot level summaries by year
# not sure summing is right here because each prob is a fraction of the TPA to kill off, 
#so if all trees have 20% fraction to kill off then that is a problem.
# this is the total plot mortality fraction in each year
mort.prob.plt.time <- mort.prob %>% ungroup() %>% 
                                    group_by(Var3, plot, rcp, parse, mortP) %>% 
                                    summarise(plot.mort = sum(mortP))
hist(mort.prob.plt.time$plot.mort, breaks = 100, main = "Annual Mortality Predicted Distribution ", xlab = "Yearly probability of mortality")


# total plot mortality probabilities over all the years
mort.prob.plt <- mort.prob.plt.time %>% ungroup() %>% 
                                        group_by( plot, rcp, parse, mortP) %>% 
                                        summarise(plot.mort = sum(mortP))

hist(mort.prob.plt$plot.mort, breaks = 100, main = "Sum of plot and yearly Mortality Predicted Distribution", xlab = "Yearly probability of mortality")

# lets do it by propoportion of DI dead:

get.di.dead <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" ){
  
  cat(paste0("getting mort.prob for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))){
      cat("no existing future climate data") 
    }else{
      
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      #dim(mort.prob.reduced)     # dimensions = 16x100x102 = # trees X nsamps X # years
      tpa.di %>% group_by( CN, time ) %>% mutate(TPA_LIVE = TPA_UNADJ-TPA)
      prop.dead.di <-  tpa.di %>% group_by( time) %>% filter(time == 99)%>%
        summarise(tot_dead = sum(TPA),
                  tot_live = sum(TPA_UNADJ) - tot_dead,
                  prop_dead = sum(TPA)/sum(TPA_UNADJ), 
                  mort_rate_a = prop_dead/99) %>% select(prop_dead, mort_rate_a, tot_dead, tot_live)
      
      prop.dead.di$plot <- plot
      prop.dead.di$mort.scheme <- mort.scheme
      prop.dead.di$rcp <- rcp
      
      # prop.dead.dd <-  tpa.dd %>% group_by( time) %>% filter(time == 99)%>%
      #   summarise(tot_dead = sum(TPA),
      #             tot_live = sum(TPA_UNADJ) - tot_dead,
      #             prop_dead = sum(TPA)/sum(TPA_UNADJ), 
      #             mort_rate_a = prop_dead/99) %>% select(prop_dead, mort_rate_a, tot_dead, tot_live)
      # 
      # prop.dead.dd$plot <- plot
      # prop.dead.dd$mort.scheme <- mort.scheme
      # prop.dead.dd$rcp <- rcp
      
      prop.dead.di
      
      # sum of DI and DD should be compared to FIA number
      
    }
  }
}

DIDD.di.ded <- lapply(unique(plots)[1:675], FUN = get.di.dead, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" )
mort.prob.a <- do.call(rbind, DIDD.di.ded)
hist(mort.prob.a$mort_rate_a)
hist(mort.prob.a$prop_dead)


get.didd.dead <- function(plot, mort.scheme = "DIDD", SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" ){
  
  cat(paste0("getting mort.prob for ",as.character(plot)))
  
  oldTREE <- TREE %>% dplyr::filter(PLT_CN %in% plot & STATUSCD ==1 )
  if(nrow(oldTREE) <=1){
    cat("less than 2 trees on the first plot")
  }else{
    if(!file.exists(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))){
      cat("no existing future climate data") 
    }else{
      
      load(paste0("biomass_dataFIAperiodic/plot2AGB_", mort.scheme, ".", plot, ".",rcp,".", SDI.ratio.DD, ".", cc.scenario, ".full.Rdata"))#,mort.scheme,".",plot,".",rcp".", SDI.ratio.DD,".",cc.scenario,".full.Rdata")))
      #dim(mort.prob.reduced)     # dimensions = 16x100x102 = # trees X nsamps X # years
      
      
      DI <- tpa.di %>% group_by( CN, time ) %>% mutate(TPADI = TPA) %>% select(treeno, time, CN, TPADI, TPA_UNADJ)
      DD <- tpa.di %>% group_by( CN, time ) %>% mutate(TPADD = TPA) %>% select(treeno, time, CN, TPADD, TPA_UNADJ)
      all.mort <- left_join(DI, DD)
      all.mort <- all.mort %>% mutate(TPAdead = TPADI + TPADD)
      
      prop.dead.di <-  all.mort %>% group_by( time) %>% filter(time == 99)%>%
        summarise(tot_dead = sum(TPAdead),
                  tot_live = sum(TPA_UNADJ) - tot_dead,
                  prop_dead = sum(TPAdead)/sum(TPA_UNADJ), 
                  mort_rate_a = prop_dead/99) %>% select(prop_dead, mort_rate_a, tot_dead, tot_live)
      
      prop.dead.di$plot <- plot
      prop.dead.di$mort.scheme <- mort.scheme
      prop.dead.di$rcp <- rcp
      
      # prop.dead.dd <-  tpa.dd %>% group_by( time) %>% filter(time == 99)%>%
      #   summarise(tot_dead = sum(TPA),
      #             tot_live = sum(TPA_UNADJ) - tot_dead,
      #             prop_dead = sum(TPA)/sum(TPA_UNADJ), 
      #             mort_rate_a = prop_dead/99) %>% select(prop_dead, mort_rate_a, tot_dead, tot_live)
      # 
      # prop.dead.dd$plot <- plot
      # prop.dead.dd$mort.scheme <- mort.scheme
      # prop.dead.dd$rcp <- rcp
      
      prop.dead.di
      
      # sum of DI and DD should be compared to FIA number
      
    }
  }
}

DIDD.ded <- lapply(unique(plots)[1:675], FUN = get.didd.dead, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" )
mort.prob.a <- do.call(rbind, DIDD.ded)
hist(mort.prob.a$mort_rate_a, breaks = 100, main = "average mortality porportion over 99 year forecast")
hist(mort.prob.a$prop_dead, breaks = 100, main = "Total mortality porportion over 99 year forecast")
