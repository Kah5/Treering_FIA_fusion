# script to compare held-out samples of DBH measurements with the forecasts from 2001-2018:
### first, some helper functions...
## from Mike Dietze
##' @name parse.MatrixNames
##' @title parse.MatrixNames
##' @author Michael Dietze
##' @param w mcmc object containing matrix outputs
##' @param pre prefix (variable name) for the matrix variable to be extracted
##' @param numeric boolean, whether to coerce class to numeric
##' @return matrix
##' @export
parse.MatrixNames <- function(w, pre = "x", numeric = FALSE) {
  w <- sub(pre, "", w)
  w <- sub("[", "", w, fixed = TRUE)
  w <- sub("]", "", w, fixed = TRUE)
  w <- matrix(unlist(strsplit(w, ",")), nrow = length(w), byrow = TRUE)
  if (numeric) {
    class(w) <- "numeric"
  }
  colnames(w) <- c("row", "col")
  return(as.data.frame(w))
} # parse.MatrixNames

#' plots a confidence interval around an x-y plot (e.g. a timeseries)
#' 
#' @param x Vector defining CI center
#' @param ylo Vector defining bottom of CI envelope
#' @param yhi Vector defining top of CI envelope
#' @export 
#' @author Michael Dietze, David LeBauer
ciEnvelope <- function(x, ylo, yhi, ...) {
  m   <- rbind(x, ylo, yhi)
  nas <- which(apply(is.na(m), 2, sum) > 0)
  if (length(nas) > 0) {
    ## break overall dataset into complete blocks
    sub.m <- list()
    for (i in seq_along(nas)) {
      if (i == 1) {
        if (nas[i] > 1) {
          sub.m[[i]] <- m[, 1:(nas[i] - 1)]
        }
      } else {
        if (nas[i] > (nas[i - 1] + 1)) {
          ## if NAs are not consecutive
          sub.m[[i]] <- m[, (nas[i - 1] + 1):(nas[i] - 1)]
        }
      }
    }
  } else {
    sub.m <- list(m = m)
  }
  for (i in seq_along(sub.m)) {
    x <- sub.m[[i]]["x", ]
    ylo <- sub.m[[i]]["ylo", ]
    yhi <- sub.m[[i]]["yhi", ]
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi), ylo[1])), border = NA, ...)
  }
} # ciEnvelope


# read in the posterior estimates that contain forecasts from 2001-2001 that we will validate:
# make sure we have the appropriate column names
data$time <- 1966:2001
colnames(data$y)<- 1966:2001
data$z <- data$z[,1:36]
colnames(data$z)<- 1966:2001
yvals <- data$y
ndex.dups <- duplicated(yvals)
yvals.new <- yvals[!ndex.dups,]
colnames(data$z) <- colnames(data$y)

# make the predicted and observed plots for diameter
pdf(paste0("outputs/plot_held_out_dbh",model.name,".pdf"))
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC outputs INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
var.pred       <- apply(out[, x.cols], 2, var)
ci.names <- parse.MatrixNames(colnames(ci), pre = "x", numeric = TRUE)
smp <- sample.int(data$ni, max(100, data$ni)) # select a random sample of 8 trees to plot

in.sample.obs <- out.sample.obs<- list()

for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  plot(data$time, ci[2, sel], type = "n", 
       ylim = range(rng), ylab = "DBH (cm)", xlab="Year", main = i)
  ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
  # plot all the insample and out of sample data
  points(as.vector(z.long %>% filter(tree.id %in% i) %>% select(year))$year, as.vector(z.long %>% filter(tree.id %in% i) %>% select(DIA))$DIA, pch = "+", cex = 1.5, type = "b", lty = 2)
  points(as.vector(z.train %>% filter(tree.id %in% i) %>% select(year))$year, as.vector(z.train %>% filter(tree.id %in% i) %>% select(DIA))$DIA, pch = "+", cex = 1.5,  lty = 2, col = "black")
  points(as.vector(z.test %>% filter(tree.id %in% i) %>% select(year))$year, as.vector(z.test %>% filter(tree.id %in% i) %>% select(DIA))$DIA, pch = "+", cex = 1.5,  lty = 2, col = "red")
  
  in.sample.obs[[i]] <- data.frame(z.data = data$z[i, 1:36], 
                                   year = data$time, 
                                   predvar = var.pred[sel], # calculate varince of the predictions
                                   min.ci = ci[1,sel],
                                   mean.ci = ci[2,sel],
                                   max.ci = ci[3,sel])#, 
  # id = cov.data.ordered[i,]$CORE_CN)
  ci.year <- ci[,sel]
  colnames(ci.year)<- 1966:2001
  
  
  # var.pred
  var.year <- var.pred[sel]
  names(var.year) <- 1966:2001
  
  out.sample.obs[[i]] <- data.frame(z.data = max(data$z[i,1:36],na.rm =TRUE),
                                    
                                    year = names(which( data$z[i,1:36]== max(data$z[i,1:36],na.rm =TRUE))),
                                    #predvar = var.year[names(which( data$z[i,1:36]== max(data$z[i,1:36],na.rm =TRUE)))],
                                    min.ci = ci.year[1,names(which( data$z[i,1:36]== max(data$z[i,1:36],na.rm =TRUE)))],
                                    mean.ci = ci.year[2,names(which( data$z[i,1:36]== max(data$z[i,1:36],na.rm =TRUE)))],
                                    max.ci = ci.year[3,names(which( data$z[i,1:36]== max(data$z[i,1:36],na.rm =TRUE)))])#,
  # id = cov.data.ordered[i,]$CORE_CN)
  
  
}

dev.off()

# plot up the out of sample predictions for DBH:
out.sample.dbh.df <- do.call(rbind,   out.sample.obs)
summary.stats <- summary(lm(z.data ~ mean.ci, data = out.sample.dbh.df))

summary.stats
#saveRDS(out.sample.dbh.df, paste0("data/outputs/",output.base.name,"pred.obs.out.of.sample.dbh.rds"))


p.o.out.of.sample <- ggplot()+
  geom_errorbar(data = out.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = out.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (held-out samples)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)#+
#geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)



# plot up the within-sample predictions for DBH:
in.sample.dbh.df <- do.call(rbind,   in.sample.obs)
in.sample.summary.stats <- summary(lm(z.data ~ mean.ci, data = in.sample.dbh.df))


saveRDS(in.sample.dbh.df, paste0("outputs/",model.name,"pred.obs.within.sample.dbh.rds"))


p.o.within.sample <-ggplot()+
  geom_errorbar(data = in.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = in.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (within-sample)")+
  theme_bw(base_size = 14)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)
p.o.within.sample
ggsave( device = "png", height = 5, width = 5, units = "in", paste0("outputs/",model.name, "_DBH_held_out_p.o.plots.png"))

#dev.off()


#--------------------------------------------------------------------------
# Validation for Increment (within sample only)
#--------------------------------------------------------------------------
pdf(paste0("outputs/rw_insample_increment",model.name,".pdf"))
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
inc.cols   <- which(substr(colnames(out), 1, 3) == "inc") # grab the state variable columns

ci      <- apply(out[, inc.cols], 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
var.pred       <- apply(out[, inc.cols], 2, var, na.rm = TRUE)
ci.names <- parse.MatrixNames(colnames(ci),pre = "inc", numeric = TRUE) # issue with parsing matrix names here
smp <- sample.int(data$ni, max(50, data$ni)) # select a random sample of 8 trees to plot


in.sample.inc <- out.of.sample.inc <-  list()

for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  
  # increment growth
  sel      <- which(ci.names$row == i)
  inc.mcmc <- out[,inc.cols[sel]] #apply(out[, x.cols[sel]], 1, diff)
  inc.ci   <- apply(inc.mcmc, 2, quantile, c(0.025, 0.5, 0.975))
  inc.names = parse.MatrixNames(colnames(ci),pre = "inc",numeric=TRUE)
  var.pred.inc <- apply( inc.mcmc, 1, var)
  
  
  plot(data$time, inc.ci[2, ] , type = "n",
       ylim = c(min(c(range(inc.ci, na.rm = TRUE), range(data$y[i,], na.rm = TRUE))),max(c(range(inc.ci, na.rm = TRUE)), range(data$y[i,], na.rm = TRUE))), ylab = "Increment (mm)", xlab="Year")
  ciEnvelope(data$time, inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  #ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  points(as.vector(y.long %>% filter(treeid %in% i) %>% select(year))$year, as.vector(y.long %>% filter(treeid %in% i) %>% select(diainc))$diainc, pch = "+", cex = 1.5, type = "b", lty = 2)
  points(as.vector(train_y %>% filter(treeid %in% i) %>% select(year))$year, as.vector(train_y %>% filter(treeid %in% i) %>% select(diainc))$diainc, pch = "+", cex = 1.5,  lty = 2, col = "black")
  points(as.vector(test_y %>% filter(treeid %in% i) %>% select(year))$year, as.vector(test_y %>% filter(treeid %in% i) %>% select(diainc))$diainc, pch = "+", cex = 1.5,  lty = 2, col = "red")
  
  # get predicted increments, then match up with training and testing datasets
  pred.inc <- data.frame(
    year = data$time,
    #predvar = var.pred.inc, # calculate varince of the predictions
    min.ci = inc.ci[1,],
    mean.ci = inc.ci[2,],
    max.ci = inc.ci[3,], 
    id = i)
  
  
  in.sample.inc[[i]] <- left_join(pred.inc, train_y %>% filter(treeid %in% i))
  out.of.sample.inc[[i]] <- left_join(pred.inc, test_y %>% filter(treeid %in% i))
  
}

dev.off()

#------------------------Do validation & diagnostics for increment----------------------------

# plot up the out of sample predictions for increment:
in.sample.inc.df <- do.call(rbind,  in.sample.inc)
summary.stats <- summary(lm(diainc ~ mean.ci, data =in.sample.inc.df))

saveRDS(in.sample.inc.df, paste0("outputs/",model.name,"pred.obs.in.sample.inc.rds"))


p.o.inc.in.sample <- ggplot()+
  geom_errorbar(data = in.sample.inc.df, aes(diainc, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = in.sample.inc.df, aes(diainc, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted increment (cm)")+xlab("Measured Increment (in-sample)")+
  theme_bw(base_size = 15)+theme(panel.grid = element_blank())#)+#+ylim(0, 2.5)+xlim(0, 2.5)#+
#geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=0.25, y=0.75)


p.o.inc.in.sample

ggsave(device = "png", height = 5, width = 5, units = "in",  paste0("outputs/", model.name, "_increment_in_sample_p.o.plots.png"))



# plot up the out of sample predictions for increment:
out.of.sample.inc.df <- do.call(rbind,  out.of.sample.inc)
summary.stats <- summary(lm(diainc ~ mean.ci, data =out.of.sample.inc.df))

saveRDS(out.of.sample.inc.df, paste0("outputs/",model.name,"pred.obs.out.of.sample.inc.rds"))


p.o.inc.out.of.sample <- ggplot()+
  geom_errorbar(data = out.of.sample.inc.df, aes(diainc, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = out.of.sample.inc.df, aes(diainc, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted increment (cm)")+xlab("Measured Increment (out-of-sample)")+
  theme_bw(base_size = 15)+theme(panel.grid = element_blank())#+#+ylim(0, 2.5)+xlim(0, 2.5)#+
#geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=0.25, y=0.75)


p.o.inc.out.of.sample

ggsave(device = "png", height = 5, width = 5, units = "in",  paste0("outputs/", model.name, "_increment_out_of_sample_p.o.plots.png"))


