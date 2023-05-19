# checking density independent mortality distributions

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
    
    mort.prob.tree.time
    }
  }
}
x.mat <- readRDS("outputs/x.mat.RDS")
plots <- unique(x.mat$PLT_CN)#[1]

unique(plots) %in% 2500345010690

DIDD.AGB <- lapply(unique(plots)[1:175], FUN = get.mort.prob.reduced, mort.scheme = "DIDD",  SDI.ratio.DD = 0.6, rcp = "rcp26", cc.scenario = "singleCC" )
mort.prob <- do.call(rbind, DIDD.AGB)


head(mort.prob)
length(unique(mort.prob$Var1)) # ntree
#length(unique(mort.prob$Var2)) # nsamp
length(unique(mort.prob$Var3)) # time

# now get the plot level summaries by year
# not sure summing is right here because each prob is a fraction of the TPA to kill off, so if all trees have 20% fraction to kill off then that is a problem.
mort.prob.plt.time <- mort.prob.tree.time %>% ungroup() %>% group_by(Var3, plot, rcp, parse, mortP) %>% summarise(plot.mort = sum(mortP))
hist(mort.prob.plt.time$plot.mort)

mort.prob.plt <- mort.prob.tree.time %>% ungroup() %>% group_by( plot, rcp, parse, mortP) %>% summarise(plot.mort = sum(mortP))

hist(mort.prob.plt$plot.mort/100)
# this actually looks okay, in terms of the shape of the distribution, but is off by a magnitude of ~ 100. Should we just divide by 100?
# generate histograms
# check what is going on 