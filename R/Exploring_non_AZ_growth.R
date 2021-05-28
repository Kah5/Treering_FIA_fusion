# looking at pipo data for the larger region...what do we need to model it?
library(tidyr)
library(ggplot2)
library(dplyr)
library(here)
library(dplR)

# Here's one concrete idea in terms of what to send to Lachlan for this purpose: 
#could you (Kelly) sort through a set of 100 TR time series (either PSME or PIPO or PIED or 
#all three?) in groups of 10 with respect to their correlation with a regionally defined chronology? 
#Lachlan, this would then give you a range of time series with respect to the degree that 
#they "march to the same drum" in response to climate variation...with greater synchrony 
#(correlation with the regional pattern) one way to measure/quantify a tree's resistance 
#to climate stress (being unmoved by stress that affects other trees). 
#Lachlan, the three codes (PSME, PIPO, and PIED) are species codes. Does it matter to you what species the tree-ring time series come from?

full.clim.data <- read.csv(paste0(here(), "/data/pipo_all_tmean_ppt_v3.csv"))
region.rwl <- read.csv("data/trees-rwl-1-31-17.csv") # note that this data is has all RWLS in columsn and a year column
region.ll <- read.csv("data/locs-env-1-31-17.csv")
region.ll$SPCD <- as.character(region.ll$SPCD)


rownames(region.rwl) <- years <- region.rwl$Year

# make RWL
region.rwl <- as.rwl(region.rwl[,3:length(region.rwl)])


# get the time series names for each species
PIED.outside.AZ <- region.ll %>% filter(SPCD %in% "106")
PIPO.outside.AZ <- region.ll %>% filter(SPCD %in% "122")
PSME.outside.AZ <- region.ll %>% filter(SPCD %in% "202")


# make species specific RWL files
PIED.rwl <- as.rwl(region.rwl %>% select(PIED.outside.AZ$series))
PIPO.rwl <- as.rwl(region.rwl %>% select(PIPO.outside.AZ$series))
PSME.rwl <- as.rwl(region.rwl %>% select(PSME.outside.AZ$series))

# make giant spaghetti plots of the data
png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_all_ts_PIED.png")
plot(PIED.rwl, plot.type="spag")
dev.off()

png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_all_ts_PIPO.png")
plot(PIPO.rwl, plot.type="spag")
dev.off()

png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_all_ts_PSME.png")
plot(PSME.rwl, plot.type="spag")
dev.off()

# make reduced plots with 100 cores for each species
png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_100_ts_PIED.png")
plot(PIED.rwl[,1:100], plot.type="spag")
dev.off()

png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_100_ts_PIPO.png")
plot(PIPO.rwl[,1:100], plot.type="spag")
dev.off()

png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_100_ts_PSME.png")
plot(PSME.rwl[,1:100], plot.type="spag")
dev.off()


rownames(PIED.rwl) <- region.rwl$Year
rownames(PIPO.rwl) <- region.rwl$Year
rownames(PSME.rwl) <- region.rwl$Year

# get stats on each species:
rwl.stats.PIED <- rwl.stats(PIED.rwl) 
rwl.stats.PIPO <- rwl.stats(PIPO.rwl) 
rwl.stats.PSME <- rwl.stats(PSME.rwl) 

# detrend all using splines
PIPO.rwi <- detrend(PIPO.rwl, method="Spline")
PSME.rwi <- detrend(PSME.rwl, method="Spline")
PIED.rwi <- detrend(PIED.rwl, method="Spline")
# got warnings: Warning message:
# In (function (y, y.name = "", make.plot = TRUE, method = c("Spline",  :
#                                                              Spline fit is not all positive

PIPO.rwi.modnegexp <- detrend(PIPO.rwl, method="ModNegExp", constrain.nls = "when.fail")
PSME.rwi.modnegexp <- detrend(PSME.rwl, method="ModNegExp", constrain.nls = "when.fail")
PIED.rwi.modnegexp <- detrend(PIED.rwl, method="ModNegExp", constrain.nls = "when.fail")

PIPO.rwi.friedman <- detrend(PIPO.rwl, method="Friedman")
PSME.rwi.friedman <- detrend(PSME.rwl, method="Friedman")
PIED.rwi.friedman <- detrend(PIED.rwl, method="Friedman")

# get stats for the overall set of rwi:

PSME.rwi.stats <- rwi.stats(PSME.rwi, prewhiten=TRUE)
PIED.rwi.stats <- rwi.stats(PIED.rwi, prewhiten=TRUE)
PIPO.rwi.stats <- rwi.stats(PIPO.rwi, prewhiten=TRUE)

PSME.rwi.stats.modnegexp <- rwi.stats(PSME.rwi.modnegexp , prewhiten=TRUE)
PIED.rwi.stats.modnegexp  <- rwi.stats(PIED.rwi.modnegexp , prewhiten=TRUE)
PIPO.rwi.stats.modnegexp  <- rwi.stats(PIPO.rwi.modnegexp , prewhiten=TRUE)

PSME.rwi.stats.friedman <- rwi.stats(PSME.rwi.friedman , prewhiten=TRUE)
PIED.rwi.stats.friedman  <- rwi.stats(PIED.rwi.friedman , prewhiten=TRUE)
PIPO.rwi.stats.friedman  <- rwi.stats(PIPO.rwi.friedman , prewhiten=TRUE)
# rbar is generally low (expected with fia data, but splines or modnegexp probably the choice)

# develop chronologies from the 3 different detrended timeseries
PSME.rwi.chron <- chron(PSME.rwi, prewhiten=TRUE)
PIED.rwi.chron <- chron(PIED.rwi, prewhiten=TRUE)
PIPO.rwi.chron <- chron(PIPO.rwi, prewhiten=TRUE)

plot(PSME.rwi.chron)
plot(PIED.rwi.chron)
plot(PIPO.rwi.chron)

PSME.rwi.chron.modnegexp <- chron(PSME.rwi.modnegexp , prewhiten=TRUE)
PIED.rwi.chron.modnegexp  <- chron(PIED.rwi.modnegexp , prewhiten=TRUE)
PIPO.rwi.chron.modnegexp  <- chron(PIPO.rwi.modnegexp , prewhiten=TRUE)

PSME.rwi.chron.friedman <- chron(PSME.rwi.friedman , prewhiten=TRUE)
PIED.rwi.chron.friedman  <- chron(PIED.rwi.friedman , prewhiten=TRUE)
PIPO.rwi.chron.friedman  <- chron(PIPO.rwi.friedman , prewhiten=TRUE)


# get interseries correlations:

PIED.rho <- interseries.cor(PIED.rwi, prewhiten=TRUE, method="spearman")
PIPO.rho <- interseries.cor(PIPO.rwi, prewhiten=TRUE, method="spearman")
PSME.rho <- interseries.cor(PSME.rwi, prewhiten=TRUE, method="spearman")


# visualize range of residual correlations
summary(PIED.rho$res.cor)
summary(PIPO.rho$res.cor)
summary(PSME.rho$res.cor)

hist(PIED.rho$res.cor)
hist(PSME.rho$res.cor)
hist(PIPO.rho$res.cor)

# sort by correlation with each other 
# get 10 trees from 10 groups-- each quantiles (range of interseries correlations)
PIED.qs <- quantile(PIED.rho$res.cor, c(0, 0.1,0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
PIED.rho <- PIED.rho %>% mutate(corr.quant = cut(res.cor, breaks=PIED.qs, labels=c("0.1","0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")))
boxplot(PIED.rho$res.cor~PIED.rho$corr.quant)

PIPO.qs <- quantile(PIPO.rho$res.cor, c(0, 0.1,0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
PIPO.rho <- PIPO.rho %>% mutate(corr.quant = cut(res.cor, breaks=PIPO.qs, labels=c("0.1","0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")))
boxplot(PIPO.rho$res.cor~PIPO.rho$corr.quant)

PSME.qs <- quantile(PSME.rho$res.cor, c(0, 0.1,0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
PSME.rho <- PSME.rho %>% mutate(corr.quant = cut(res.cor, breaks=PSME.qs, labels=c("0.1","0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")))
boxplot(PSME.rho$res.cor~PSME.rho$corr.quant)

PSME.rho$series <- rownames(PSME.rho)
PIPO.rho$series <- rownames(PIPO.rho)
PIED.rho$series <- rownames(PIED.rho)

# randomly select 10 cores from each quantile group with varying correlations from regional chronology:
PSME.q.samps <- PSME.rho %>% filter(! is.na(corr.quant)) %>% dplyr::group_by(corr.quant) %>% sample_n(10)
PIPO.q.samps <- PIPO.rho %>% filter(! is.na(corr.quant)) %>% dplyr::group_by(corr.quant) %>% sample_n(10)
PIED.q.samps <- PIED.rho %>% filter(! is.na(corr.quant)) %>% dplyr::group_by(corr.quant) %>% sample_n(10)



PSME.q.rwi <- as.rwl(PSME.rwi %>% select( PSME.q.samps$series ))
PIED.q.rwi <- as.rwl(PIED.rwi %>% select( PIED.q.samps$series ))
PIPO.q.rwi <- as.rwl(PIPO.rwi %>% select( PIPO.q.samps$series )) 



PSME.q.rwi$year <- years
PIED.q.rwi$year <- years
PIPO.q.rwi$year <- years



# visualize these timeseries:
PSME.q.rwi.m <- melt(PSME.q.rwi, id.vars = "year")
PIED.q.rwi.m <- melt(PIED.q.rwi, id.vars = "year")
PIPO.q.rwi.m <- melt(PIPO.q.rwi, id.vars = "year")
colnames(PSME.q.rwi.m) <- c("year", "series", "rwi")
colnames(PIPO.q.rwi.m) <- c("year", "series", "rwi")
colnames(PIED.q.rwi.m) <- c("year", "series", "rwi")

# merge series to get series intercorrelation:
PSME.q.rwi.m <- left_join(PSME.q.rwi.m, PSME.q.samps, by = "series")
PIPO.q.rwi.m <- left_join(PIPO.q.rwi.m, PIPO.q.samps, by = "series")
PIED.q.rwi.m <- left_join(PIED.q.rwi.m, PIED.q.samps, by = "series")

# add the chronologies:
PIED.rwi.chron$year <- years
PIPO.rwi.chron$year <- years
PSME.rwi.chron$year <- years

# make plots of rwi grouped by residual correlations:
red.scale <- c("#ece2f0",
  "#fff7ec",
  "#fee8c8",
  "#fdd49e",
  "#fdbb84",
  "#fc8d59",
  "#ef6548",
  "#d7301f",
  "#b30000",
  "#7f0000")

PSME.rwi.plt.cat.corr <- ggplot()+
  geom_line(data = PSME.q.rwi.m, aes(x = year, y = rwi, group = series, color = corr.quant))+
  scale_color_manual(values = red.scale)+xlim(1850, 2020)+
  geom_line(data = PSME.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())




PIED.rwi.plt.cat.corr <- ggplot()+geom_line(data = PIED.q.rwi.m, aes(x = year, y = rwi, group = series, color = corr.quant))+
  scale_color_manual(values = red.scale)+xlim(1600, 2000)+
  geom_line(data = PIED.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())

PIPO.rwi.plt.cat.corr <- ggplot()+geom_line(data = PIPO.q.rwi.m, aes(x = year, y = rwi, group = series, color = corr.quant))+
  scale_color_manual(values = red.scale)+xlim(1825, 2020)+
  geom_line(data = PIPO.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())



PSME.rwi.plt.cont.corr <- ggplot()+
  geom_line(data = PSME.q.rwi.m, aes(x = year, y = rwi, group = series, color = res.cor))+
  scale_colour_gradient(low = "white", high = "black")+xlim(1850, 2020)+
  geom_line(data = PSME.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())


PSME.rwi.plt.cont.corr <- ggplot()+geom_line(data = PIED.q.rwi.m, aes(x = year, y = rwi, group = series, color = res.cor))+
  scale_colour_gradient(low = "white", high = "black")+xlim(1600, 2000)+
  geom_line(data = PIED.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())

PSME.rwi.plt.cont.corr <- ggplot()+geom_line(data = PIPO.q.rwi.m, aes(x = year, y = rwi, group = series, color = res.cor))+
  scale_colour_gradient(low = "white", high = "black")+xlim(1825, 2020)+
  geom_line(data = PIPO.rwi.chron, aes(x = year, y = xxxstd), color = "black", size = 1.5)+
  ylab("Ring Width Index")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())


# plot the raw ring widths based on correlation with master:
PSME.q.rwl <- as.rwl(PSME.rwl %>% select( PSME.q.samps$series ))
PIED.q.rwl <- as.rwl(PIED.rwl %>% select( PIED.q.samps$series ))
PIPO.q.rwl <- as.rwl(PIPO.rwl %>% select( PIPO.q.samps$series )) 

PSME.q.rwl$year <- years
PIED.q.rwl$year <- years
PIPO.q.rwl$year <- years



# visualize these timeseries:
PSME.q.rwl.m <- melt(PSME.q.rwl, id.vars = "year")
PIED.q.rwl.m <- melt(PIED.q.rwl, id.vars = "year")
PIPO.q.rwl.m <- melt(PIPO.q.rwl, id.vars = "year")
colnames(PSME.q.rwl.m) <- c("year", "series", "rwl")
colnames(PIPO.q.rwl.m) <- c("year", "series", "rwl")
colnames(PIED.q.rwl.m) <- c("year", "series", "rwl")

# merge series to get series intercorrelation:
PSME.q.rwl.m <- left_join(PSME.q.rwl.m, PSME.q.samps, by = "series")
PIPO.q.rwl.m <- left_join(PIPO.q.rwl.m, PIPO.q.samps, by = "series")
PIED.q.rwl.m <- left_join(PIED.q.rwl.m, PIED.q.samps, by = "series")


# make plots of rwl grouped by residual correlations:
PSME.rwl.plt <- ggplot(data = PSME.q.rwl.m, aes(x = year, y = rwl, group = series, color = corr.quant))+geom_line()+xlim(1850, 2020)+
  scale_color_manual(values = red.scale)+xlim(1600, 2000)+
  ylab("Tree Ring Width (mm)")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())


PIED.rwl.plt <-ggplot(PIED.q.rwl.m, aes(x = year, y = rwl, group = series, color = corr.quant))+geom_line()+scale_color_manual(values = red.scale)+xlim(1600, 2000)+
  ylab("Tree Ring Width (mm)")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())

PIPO.rwl.plt <-ggplot(PIPO.q.rwl.m, aes(x = year, y = rwl, group = series, color = corr.quant))+geom_line()+xlim(1845, 2020)+scale_color_manual(values = red.scale)+
  ylab("Tree Ring Width (mm)")+xlab("Year")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())

cowplot::plot_grid(PIPO.rwi.plt.cat.corr, PIPO.rwl.plt, nrow = 2, align = "hv", labels = "AUTO")

#ggplot(PSME.q.rwl.m, aes(x = year, y = rwl, group = series, color = res.cor))+geom_line()+xlim(1850, 2020)
#ggplot(PIED.q.rwl.m, aes(x = year, y = rwl, group = series, color = res.cor))+geom_line()
#ggplot(PIPO.q.rwl.m, aes(x = year, y = rwl, group = series, color = res.cor))+geom_line()+xlim(1845, 2020)


library(dplR)
data(wa082)
wa082.sum <- summary(wa082) 
mean(wa082.sum$year)
region.sum <- dplR::summary(region.rwl)
mean(region.sum$stdev)

rwl.stats.all.region <- rwl.stats(region.rwl) #


region.rwi <- detrend(region.rwl, method="Spline")

png(height = 8, width = 8, units = "in", res = 300, "outputs/spag_plot_100_ts_rwi.png")
plot(region.rwi[,1:100], plot.type="spag")
dev.off()
# need to split into species, then develop chronology 



# make a map of all of these:
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona", "utah", "new mexico", "colorado", 
                                            "idaho", "wyoming", "montana", "nevada", 
                                            "california", "oregon", "washington", "texas", "kansas", 
                                            "nebraska", "north dakota", "south dakota") )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata <- states
mapdata<-data.frame(mapdata)

region.ll$SPCD <- as.character(region.ll$SPCD)

#png(height = 6, width = 7, units = "in", res = 200, "new_regional_cores_by_SPCD.png")
ggplot(data = region.ll, aes(x = LON, y = LAT, color = SPCD))+geom_point()+
  geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
               colour = "darkgrey", fill = NA)+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), legend.position = "bottom")+ylab("Latitude")+
  xlab("Longitude")+coord_cartesian(xlim = c(-118, -103), ylim = c(32, 49))+facet_wrap(~SPCD)
#dev.off()


rwl.m <- melt(region.rwl %>% dplyr::select(-X), id.vars = "Year")
colnames(rwl.m) <- c("year", "series", "growth")
region.ll$series <- as.character(region.ll$series)


all.outside.AZ <- left_join(region.ll, rwl.m, by = c( "series"))

# get only the PIED
PIED.outside.AZ <- all.outside.AZ %>% filter(SPCD %in% "106")
PIPO.outside.AZ <- all.outside.AZ %>% filter(SPCD %in% "122")
#PIED.outside.AZ <- all.outside.AZ %>% filter(SPCD %in% "106")
doug.fir.outside.AZ <- all.outside.AZ %>% filter(SPCD %in% "202")


head(PIPO.outside.AZ)

# merge with climate data:

head(full.clim.data)
colnames(full.clim.data)[3] <- "series"


growth.clim <- left_join(PIPO.outside.AZ , full.clim.data)

ggplot(growth.clim, aes(Precip_DecJanFeb , growth, color = as.character(STATECD)))+geom_point()

colnames(growth.clim)[63:74] <- paste0("TMEAN_",  head(statewide.clim.summary$month, 12))
# plot out climate space of the seasons by state
statewide.clim.summary <- growth.clim %>% select(STATECD, series, year, PPT_1:PPT_9, TMEAN_1:TMEAN_9) %>%  
  gather( "climate", "value",PPT_1:PPT_9, TMEAN_1:TMEAN_9) %>% group_by(STATECD, climate)%>%
  summarise(mean.clim = mean(value, na.rm=TRUE), 
            sd.clim = sd(value, na.rm = TRUE), 
            ci.lo = quantile(value, 0.025, na.rm = TRUE), 
            ci.hi = quantile(value, 0.975, na.rm = TRUE)) %>% separate(climate, into = c("clim", "month"))


ppt.clim.summary <- statewide.clim.summary %>% filter(clim %in% "PPT")
tmean.clim.summary <- statewide.clim.summary %>% filter(clim %in% "TMEAN")

colnames(ppt.clim.summary)[4:7] <- c("ppt.mean", "ppt.sd", "ppt.ci.lo", "ppt.ci.hi")
colnames(tmean.clim.summary)[4:7] <- c("tmean.mean", "tmean.sd", "tmean.ci.lo", "tmean.ci.hi")

climate.state.sum <- merge(ppt.clim.summary, tmean.clim.summary, by = c("STATECD", "month"))

statecodes <- data.frame(STATECD = c(8, 16, 30, 49, 56),
                         state = c("Colorado", "Idaho", "Montana", "Utah", "Wyoming"))

climate.state.summary<- merge(climate.state.sum, statecodes, by = c("STATECD"))

months <- data.frame(month = 1:12,
                     Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                               "Sep", "Oct", "Nov", "Dec"))

climate.state.summary<- merge(climate.state.summary, months, by = c("month"))
climate.state.summary$Month <- factor(climate.state.summary$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"))
                                                                              
                                                                              
 png(height = 4, width = 7, units = "in", res = 300, "full_region_state_climate_diagrams.png")
 ggplot()+geom_point(data = climate.state.summary, aes(x = tmean.mean, y = ppt.mean, color = Month))+
 geom_errorbar(data = climate.state.summary, aes(x = tmean.mean, ymin = ppt.ci.lo, ymax = ppt.ci.hi, color = Month))+
geom_errorbarh(data = climate.state.summary, aes(y = ppt.mean, xmin = tmean.ci.lo, xmax = tmean.ci.hi, color = Month))+
facet_wrap(~state)+ylab("Precipitation (mm)")+xlab("Mean Temperature (DegC)")
dev.off()
                                                                              
                                                                              
#----------------------------------------------------------------------------------
# make individaul tree correlations
#----------------------------------------------------------------------------------
                                                                              # 
trw.test <- growth.clim %>% group_by(series) #%>% dplyr::select(-YearID) #%>% spread(TreeID, Width)

trw.nona <- trw.test[!is.na(trw.test$growth),]
colnames(trw.nona )[75:86] <-paste0("PREV_TMEAN_", c(1,10,11, 12, 2, 3, 4, 5, 6, 7, 8, 9)) 
# creat a function to do the correlation on a given TREEID (x) then apply:
library(Hmisc)
correlate.rwi <- function(treeid){
  
  test.nona <- trw.nona[trw.nona$series %in% treeid, ] # select 1 the tree of choice
  
  
  # if for some reason growth is not numeric, make it numeric
  if(!is.numeric(test.nona$growth)){
    test.nona$growth <- as.numeric(test.nona$growth)
  }
  
  # here we do the correlations
  # corM <- cor(test.nona$growth, test.nona[, colnames(test.nona)[2:32]], use = "pairwise.complete")
  
  
  cor.mat <- rcorr( as.matrix(test.nona[, c("growth", colnames(test.nona)[25:85])]), type="pearson") 
  cor.mat.df <- data.frame(climate = colnames(test.nona)[25:85],
                           coef = cor.mat$r[2:62,1], 
                           p = cor.mat$P[2:62,1])
  #cat("*")
  cor.mat.df
}


names <- as.list(as.character(unique(trw.nona$series))) # get names of unique trees to apply function over

ppt.cors <- lapply(names, correlate.rwi)

names(ppt.cors) <- unique(trw.nona$series)

ppt.cors.df <- do.call(rbind, ppt.cors) # takes a minute

ppt.cors.df$series <- rep(names(ppt.cors), sapply(ppt.cors, nrow)) # add the site names

# lets visualize the individual correlations:
#ggplot(ppt.cors.df, aes(climate, coef, color = p < 0.05))+geom_jitter(size = 0.5)+scale_color_manual(values = c("grey", "black"))+geom_violin(color = "black", fill = "yellow",  alpha = 0.25)+theme_bw()


#clim.trans <- data.frame(climate = unique(ppt.cors.df$climate),
clim = c(rep("PPT", 12), rep("TMAX", 18), rep("PPT", 4)), 
month = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep",
          "AprMayJun","fall","fall_spr", "Jan_Aug", "Mar_Jul", "JulAugSep",
          "Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep",
          "Jan_Jul", "Nov_Mar", "Nov_Aug", "wateryr"))


#ppt.cors.df<- left_join(ppt.cors.df, clim.trans, by = "climate")

#ppt.cors.df$month_ordered <- factor(ppt.cors.df$month, levels= c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "AprMayJun","fall","fall_spr", "Jan_Aug", "Mar_Jul", "JulAugSep","Jan_Jul", "Nov_Mar", "Nov_Aug", "wateryr"))

png(height = 4, width = 8, units = "in", res = 200, "nonAZ_individual_clim_correlations_boxplot.png")                                                                              
ggplot(data = ppt.cors.df, aes(x = climate, y = coef ))+geom_boxplot()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))            
dev.off()

png(height = 4, width = 8, units = "in", res = 200, "nonAZ_individual_clim_correlations_dotplot.png")                                                                              
ggplot(data = ppt.cors.df, aes(x = climate, y = coef ))+geom_jitter(size = 0.05)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))    
dev.off()   

unique.trw.series <- unique(trw.test[,c("STATECD", "COUNTYCD", "PLOT", "SUBP", "TREE", "series", "DIA", "LAT", "LON", "ELEV", "ASPECT", "SLOPE", "SICOND")])

ppt.cors.trw <- left_join(unique.trw.series, ppt.cors.df, by = "series")
ppt.cors.trw <- merge(ppt.cors.trw, statecodes, by = c("STATECD"))

ggplot(data = ppt.cors.trw, aes(x = climate, y = coef ))+geom_jitter(size = 0.05)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~state)    

png(height = 7, width = 8, units = "in", res = 200, "nonAZ_individual_clim_correlations_boxplot_bystate.png")                                                                              

ggplot(data = ppt.cors.trw, aes(x = climate, y = coef ))+geom_boxplot()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~state, ncol = 2)            
dev.off()


ggplot(data = ppt.cors.trw[ppt.cors.trw$climate %in% "Precip_AprMayJun",], aes(x = SICOND, y = coef ))+geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~state, ncol = 2)            

ggplot(data = ppt.cors.trw[ppt.cors.trw$climate %in% "Precip_AprMayJun",], aes(x = DIA, y = coef ))+geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~state, ncol = 2)            

ggplot(data = ppt.cors.trw[ppt.cors.trw$climate %in% "Precip_AprMayJun",], aes(x = ELEV, y = coef ))+geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+stat_smooth()    

ggplot(data = ppt.cors.trw[ppt.cors.trw$climate %in% "Precip_AprMayJun",], aes(x = SLOPE, y = coef ))+geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+stat_smooth() 

png(height = 6, width = 7, units = "in", res = 300, "Map_nonAZ_pipo_ppt_APRMAYJUN_correlations.png")
ggplot()+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                                                         colour = "darkgrey", fill = NA)+theme_bw(base_size = 12)+
  geom_point(data = ppt.cors.trw[ppt.cors.trw$climate %in% "Precip_AprMayJun",], aes(x = LON, y = LAT, color = coef ))+
  scale_color_viridis_c(option = "plasma")+
  theme(panel.grid = element_blank(), legend.position = "bottom")+ylab("Latitude")+
  xlab("Longitude")+coord_cartesian(xlim = c(-118, -103), ylim = c(32, 49))+ggtitle("Correlation with Apr - Jun Precipitation")
dev.off()
