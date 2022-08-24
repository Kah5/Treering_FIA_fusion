# script to readin in the downscaled climate model projections from https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Complete%20Archives
# Under Subset request, I selected downscaled projections for Jan 2018 - Dec 2099 and highlighted the region of AZ for the domain
# I used the projection set: BCSD-CMIP5-Hydrology-monthly and selected maximum temperature and precipiation
# then for all the rcps, I selected "all"
# then I selected "no analysis" and "netcdf" on the last page...it took less than an hour for them to email me with a link to download the zipped data
# #######################more information from the product:
# Product:               Bias-corrected statistically downscaled GCM data
# Variables:             tasmax    
# NetCDF type:          float     
# NetCDF missing value:  1e+20     
# Period:                2018Jan through 2099Dec
# Resolution:            1/8 degree
# Latitude bounds:       (29.875, 38.125)
# Longitude bounds:      (-115.125, -108.0)
# Area within bounds:    602058 km^2 (approx)
# Dimensions:         
#   Times:                984
# Latitudes:            66
# Longitudes:           57
# Projections:          97
# 
# 
# Global attributes
# -----------------
#   Conventions:           GDT 1.2
# authors:               Bridget Thrasher, Ed Maurer
# description:           Bias-corrected and downscaled GCM data
# creation_date:         2012
# institution:           Climate Analytics Group, Santa Clara U.
# SurfSgnConvention:     Traditional


# Selected 
# overview:
# 1. Read in the lat long data we need to extract climate data over
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
# 3. output & repeat for the next climate variable

library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4) # a must have package for opening netcdfs
library(lubridate)
#library(tidync) couldnt download
library(dplyr)
library(tidyverse)
library(terra)
#------------------------------------------------------------------
# 1. Read in the lat long data we need to extract climate data over
#------------------------------------------------------------------
# read in the data set that has the lat long of the plots/cores we want to extract projections from

cov.data.ll <- readRDS("FIA_inc_data/cov.data.regional.ll.rds")

coordinates(cov.data.ll) <- ~  PLOT_LON + PLOT_LAT
proj4string(cov.data.ll) <- CRS("+init=epsg:4326")

#cov.data.ll <- vect(cov.data.ll, crs="+init=epsg:4326")

# dont need this, but this would be the way to transform to a new projection (needed for climat NA)
# cov.data.en <- spTransform(cov.data.ll, CRSobj = CRS("+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"))
# 
# cov.data.en.df <- data.frame(cov.data.en)
#plot(cov.data.en)
plot(cov.data.ll)

cov.data.xy <- data.frame(cov.data.ll)[,c("PLOT_LON", "PLOT_LAT")]
#------------------------------------------------------------------
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
#------------------------------------------------------------------

# list all the netcdfs for precip
hydro.files <- list.files("hydro5/", pattern = ".nc")

hydro.ncs <- paste0(getwd(),"/hydro5/", hydro.files)

# for the precipitation:
x <- hydro.ncs[1]


# open the netcdf
#nc <- nc_open(x)

ppt = terra::rast(x, "ppt") # get ppt
dim(ppt)  #145   145 94381 # lat, lon, time*projection*senario
names(ppt) 

# the naming structure is really confusing here, but I think that the 
# pr_projection_num_num2 identifies the modelprojection from the projection list first, then the month number from 2018 - Jan to 2099-jan
ppt[names(ppt)[2]]

# try plotting to see what will happen
terra::plot(ppt[[1]])
points(cov.data.ll)


#variableofinterest <- names(nc$var) # get the variable of interest
#ppt <- ncvar_get(nc,variableofinterest) # this extracts a 4 dimensional array of data
# 4 Dimensional array:
# dim 1: long
# dim 2: lat
# dim 3: time in months (jan 2018 - Dec 2099)
# dim 4: each climate model/ensemble member
# lat <- ncvar_get(nc,"latitude") # get lat
# lon <- ncvar_get(nc, "longitude") # get long
# nc.time <- ncvar_get(nc, "time") # get long
# projection <- ncvar_get(nc, "projection") # cant get the dimvar, but metadata has info on projections
# 

projection <- read.delim("hydro5/Projections5.txt")
nmodels <- length(projection[,1])
dim(ppt)# look at the dimensions
nmonths <- 973#dim(ppt
proj.df <- data.frame(projection = projection[,1], 
                      id = 1:length(projection[,1]))


mo.year.df <- data.frame(nmonth = 1:nmonths, 
                         month = c(rep(1:12, 81),1),
                         year = c(rep(2018:2098, each = 12), 2099))

# 3rd dimension is the number of months in the downscaled projections
#nTmax <- dim(ppt)
#nc_close(nc) # close the netcdf file when you are done extracting

system.time(test.points <- terra::extract(ppt, cov.data.xy[1:10,]))
#user  system elapsed 
#7.172   4.433  26.616 

rlist <- list()
nyears <- 82#nmonths/12
startyr <- 2018
endyr <- 2018+(nyears-1)

system.time(extracted.pts <- terra::extract(ppt, cov.data.xy)) # could take awhile to get through
#extracted.pts2 <- extracted.pts
extracted.pts$PLOT_LAT <- cov.data.xy$PLOT_LAT
extracted.pts$PLOT_LON <- cov.data.xy$PLOT_LON
saveRDS(extracted.pts, "ppt_future_regional.RDS") 



# get tmax:
# list all the netcdfs for precip
hydro.files <- list.files("hydro5tmax/", pattern = ".nc")

hydro.ncs <- paste0(getwd(),"/hydro5tmax/", hydro.files)

# for the precipitation:
x <- hydro.ncs[1]


ppt = terra::rast(x, "tmax") # get ppt
dim(ppt)  #145   145 94381 # lat, lon, time*projection*senario
names(ppt) 

# the naming structure is really confusing here, but I think that the 
# pr_projection_num_num2 identifies the modelprojection from the projection list first, then the month number from 2018 - Jan to 2099-jan
ppt[names(ppt)[2]]

# try plotting to see what will happen
terra::plot(ppt[[8]])
points(cov.data.ll)



projection <- read.delim("hydro5tmax/Projections5.txt")
nmodels <- length(projection[,1])
# look at the dimensions
nmonths <- 973#dim(ppt
proj.df <- data.frame(projection = projection[,1], 
                      id = 1:length(projection[,1]))


mo.year.df <- data.frame(nmonth = 1:nmonths, 
                         month = c(rep(1:12, 81),1),
                         year = c(rep(2018:2098, each = 12), 2099))

# 3rd dimension is the number of months in the downscaled projections


nyears <- 82#nmonths/12
startyr <- 2018
endyr <- 2018+(nyears-1)


system.time(extracted.pts <- terra::extract(ppt, cov.data.xy)) # could take awhile to get through
#extracted.pts2 <- extracted.pts
extracted.pts$PLOT_LAT <- cov.data.xy$PLOT_LAT
extracted.pts$PLOT_LON <- cov.data.xy$PLOT_LON
saveRDS(extracted.pts, "tmax_future_regional.RDS") 


# get timin:
# get tmax:
# list all the netcdfs for precip
hydro.files <- list.files("hydro5tmin/", pattern = ".nc")

hydro.ncs <- paste0(getwd(),"/hydro5tmin/", hydro.files)

# for the precipitation:
x <- hydro.ncs[1]




# get the tmax extracted data and summaries by projection
extracted.pts <- readRDS( "tmax_future_regional.RDS") 
#extracted.pts <- readRDS( "ppt_future_regional.RDS")
#colnames(extracted.pts) <- c("ID",paste0("ppt_",rep(1:97, each = 973),"_", rep(startyr:endyr, each = 12), "_", rep(1:12, nyears) ),"PLOT_LAT", "PLOT_LON") # note may need to change this to make more customizable
extracted.pts.m <- reshape2::melt(extracted.pts, id.vars = c("PLOT_LAT", "PLOT_LON", "ID"))
#extracted.pts.m$value <- ifelse(extracted.pts.m$value >= 1e+20, NA, extracted.pts.m$value) # set NA values
extracted.pts.m$variable <- as.character(extracted.pts.m$variable)
#ext.sep <- extracted.pts.m %>% tidyr::separate(variable, sep = "_", into = c("clim","proj", "year", "month"))
# if the above line crashes R even with the variable as a character, try left_joining with a df tha thas all the names

proj.proj.names = data.frame(proj = rep(1:97, each = 973),
                             year = rep(c(rep(startyr:2098, each = 12),2099),  97),
                             month = rep(c(rep(1:12, 81), 1), 97),
                             variable = as.character(unique(extracted.pts.m$variable)))

ext.sep <- dplyr::left_join(proj.proj.names, extracted.pts.m, by = "variable")


saveRDS(ext.sep, "future.projections.monthly.tmax.RDS")
#I use yearly ppt, but we could make a different summary of interest here


#------------------------------------------------
#
# read in and summarise all the climate variables:
tmax.sep  <- readRDS( "future.projections.monthly.tmax.RDS")
yearly.tmax <- tmax.sep %>% dplyr::group_by(PLOT_LON, PLOT_LAT,ID, year, proj) %>% dplyr::summarise(year.tmax.ave = mean(value, na.rm=TRUE)) 
saveRDS(yearly.tmax, "future.proj.yearly.tmax.RDS")
rm(yearly.tmax)

AprMayJun.tmax <- tmax.sep %>% dplyr::group_by(PLOT_LON, PLOT_LAT,ID, year, proj) %>% dplyr::filter(month %in% c(4,5,6)) #%>% 
rm(tmax.sep)
AprMayJun.tmax.m <- AprMayJun.tmax %>% dplyr::summarise(tmaxAprMayJun = mean(value, na.rm=TRUE)) 
saveRDS(AprMayJun.tmax.m, "future.proj.AprMayJun.tmax.RDS")


rm(AprMayJun.tmax)

ppt.sep  <- readRDS( "future.projections.monthly.ppt.RDS")
yearly.ppt <- ppt.sep %>% dplyr::group_by(PLOT_LON, PLOT_LAT,ID, year, proj) %>% dplyr::summarise(year.ppt = sum(value, na.rm=TRUE)) 
#AprMayJun.tmax <- tmax.sep %>% dplyr::group_by(PLOT_LON, PLOT_LAT,ID, year) %>% dplyr::filter(month %in% 4:6) %>% dplyr::summarise(tmaxAprMayJun= mean(value, na.rm=TRUE)) 
saveRDS(yearly.ppt, "future.proj.yearly.ppt.RDS")
rm(ppt.sep)
rm(yearly.ppt)










# get the projection names
# because the projection labels were not working for this, I need to read in a text file with all the projection names:
proj <- read.delim("hydro5_1950_2099/Projections5.txt", header = FALSE)
#proj.tas <- read.delim("bcsd5/Projections5.txt", header = FALSE)

#add the projection names to the tmax and ppt data frames

for( i in seq_along(all.future.ppt)){
  
  all.future.tmax[[i]]$proj <- rep(proj[i,], nrow(all.future.tmax[[i]]))
  
}

for( i in seq_along(all.future.ppt)){
  
  all.future.ppt[[i]]$proj <- rep(proj[i,], nrow(all.future.ppt[[i]]))
  
}
# convert to df
all.tmax.df <- do.call(rbind, all.future.tmax)
all.ppt.df <- do.call(rbind, all.future.ppt)


#all.tmax.df$proj <- rep(proj$V1, sapply(all.future.tmax , nrow))
#all.ppt.df$proj <- rep(proj$V1, sapply(all.future.ppt , nrow))

ppt.models <- all.ppt.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) #%>% 
#tidyr::separate(modelrun, sep = "-", into = c("model", "run"))
tmax.models <- all.tmax.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) 
# summary.tas <- tmax.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(tmax.fall.spr, na.rm = TRUE), 
#                                                             mean = mean(tmax.fall.spr, na.rm = TRUE))
# 
# summary.ppt <- ppt.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(ppt, na.rm = TRUE), 
#                                                                                          mean = mean(ppt, na.rm = TRUE))
# 
# okay lets merge these together:
future.climate.ts <- dplyr::left_join(ppt.models, tmax.models, by = c("lat", "lon", "year", "modelrun","rcp"))
hist(future.climate.ts$year.ppt)
hist(future.climate.ts$tmax.fall.spr)
future.climate.ts[future.climate.ts$tmax.fall.spr >= 50,]

future.climate.ts <- future.climate.ts %>% dplyr::select(-climate.x, -climate.y)
pipo.cores.ll$cov.data
pipo.cores.ll$future.climate.ts <- future.climate.ts

# make plots of individual future climate models:




saveRDS(pipo.cores.ll, "pipo.cores.with.downscaled.hydro.ppt.climate_1950_2099_v4.rds")


head(pipo.cores.ll)


pipo.cores.ll <- readRDS( "pipo.cores.with.downscaled.hydro.ppt.climate_1950_2099_v4.rds")
head(pipo.cores.ll)
head(pipo.cores.ll$future.climate.ts)
future.climate.ts  <- pipo.cores.ll$future.climate.ts
clim.ts.df <- pipo.cores.ll$future.climate.ts
clim.ts.df$tmax.fall.spr[is.nan(clim.ts.df$tmax.fall.spr)] <- NA

head(clim.ts.df)

head(future.climate.ts)

ll.df <- unique(future.climate.ts[,c("lat", "lon")])
test.df<- future.climate.ts %>% filter(lat %in% ll.df[2,]$lat & lon %in% ll.df[2,]$lon)
test.df$year <- as.numeric(test.df$year)
ggplot(test.df, aes(year, year.ppt, color = modelrun))+geom_line()+facet_wrap(~rcp)
ggplot(test.df, aes(year, tmax.fall.spr, color = modelrun))+geom_line()+facet_wrap(~rcp)


prev.df<- clim.ts.df  %>% filter(lat %in% ll.df[1,]$lat & lon %in% ll.df[1,]$lon)
prev.df$year <- as.numeric(prev.df$year)
ggplot(prev.df, aes(year, tmax.fall.spr, color = modelrun))+geom_line()+facet_wrap(~rcp)


# ------------------------------------------------------------------------------------------
# Compare the time series of climate from CMIP5 models at each site to the observed values
# ------------------------------------------------------------------------------------------

head(cov.data.ll)
head(time_data)

tmax.fall.spr <- as.data.frame(time_data$tmax.fallspr)
cov.data.ll.df <- as.data.frame(cov.data.ll)

tmax.fall.spr$lat <- cov.data.ll.df$LAT
tmax.fall.spr$lon <- cov.data.ll.df$LON

colnames(tmax.fall.spr)[1:53] <- 1966:2018
tmax.fall.spr.obs <- melt(tmax.fall.spr, id.vars = c("lat", "lon"))
colnames(tmax.fall.spr.obs) <- c("lat", "lon", "year", "PRISM_fall_spr_tmax")


# get water year precip:
wateryr_ppt <- time_data$wintP.wateryr
cov.data.ll.df <- as.data.frame(cov.data.ll)

wateryr_ppt$lat <- cov.data.ll.df$LAT
wateryr_ppt$lon <- cov.data.ll.df$LON

colnames(wateryr_ppt)[1:53] <- 1966:2018
wateryr_ppt.obs <- melt(wateryr_ppt, id.vars = c("lat", "lon"))
colnames(wateryr_ppt.obs) <- c("lat", "lon", "year", "PRISM_ppt")

# join the prism  historical climates
obs.climate <- left_join(wateryr_ppt.obs, tmax.fall.spr.obs, by = c("lat", "lon", "year"))


# join the observed climate to the cmip5 dataframe by lat, lon, year & plot the difference:

fut.past.compare <- left_join(clim.ts.df, obs.climate, by = c("lat", "lon", "year"))

ggplot(fut.past.compare, aes(PRISM_ppt, year.ppt))+geom_point()+facet_wrap(~rcp)

fut.past.compare$time_period <- ifelse(fut.past.compare$year < 1965, "1950-1964", 
                                       ifelse(fut.past.compare$year >= 1965 & fut.past.compare$year < 2018, "1965-2018", 
                                              ifelse(fut.past.compare$year >= 2018 & fut.past.compare$year < 2049, "2019-2049", 
                                                     ifelse(fut.past.compare$year >= 2050 & fut.past.compare$year < 2075, "2050-2074", "2075-2099" ))))


fut.past.compare$ppt.diff <- fut.past.compare$PRISM_ppt - fut.past.compare$year.ppt
fut.past.compare$tmax.diff <- fut.past.compare$PRISM_fall_spr_tmax - fut.past.compare$tmax.fall.spr

ts.site.means <- fut.past.compare %>% group_by(lat, lon, rcp, modelrun, time_period ) %>% summarise(ppt.obs = mean(PRISM_ppt, na.rm = TRUE), 
                                                                                                    tmax.obs = mean(PRISM_fall_spr_tmax, na.rm = TRUE), 
                                                                                                    ppt.fut = mean(year.ppt, na.rm = TRUE), 
                                                                                                    tmax.fut = mean(tmax.fall.spr, na.rm = TRUE), 
                                                                                                    avg.tmax.diff = mean(tmax.diff, na.rm =TRUE), 
                                                                                                    avg.ppt.diff = mean(ppt.diff, na.rm =TRUE))



ggplot(ts.site.means %>% filter(time_period %in% c("1965-2018")), aes(avg.tmax.diff, fill= rcp))+geom_histogram()+facet_wrap(~rcp)+xlab("tmax obs - tmax fut")+geom_vline(xintercept = 0)
ggplot(ts.site.means %>% filter(time_period %in% c("1965-2018")), aes(avg.ppt.diff, fill= rcp))+geom_histogram()+facet_wrap(~rcp)+xlab("ppt obs - ppt fut")+geom_vline(xintercept = 0)


# Adjust the values of future climate so that all future model run means for historic period match the means for each site, 

# get the mean difference just for 1965-2018
ts.site.means.1965.2018 <- ts.site.means %>% filter(time_period %in% c("1965-2018")) %>% dplyr::select(lat, lon, rcp, modelrun, avg.tmax.diff, avg.ppt.diff)

fut.past.means.compare <- left_join(fut.past.compare, ts.site.means.1965.2018, by =c("lat", "lon", "rcp", "modelrun"))
fut.corr <- fut.past.means.compare
fut.corr$ppt.corrected <- fut.corr$year.ppt + fut.corr$avg.ppt.diff
fut.corr$ppt.corrected <- ifelse(fut.corr$ppt.corrected < 0, 0, fut.corr$ppt.corrected)
fut.corr$tmax.corrected <- fut.corr$tmax.fall.spr + fut.corr$avg.tmax.diff

ggplot()+geom_line(data = test.df, aes(year, tmax.corrected, color = modelrun))+
  geom_line(data = test.df, aes(year, PRISM_fall_spr_tmax), color = "black")+facet_wrap(~rcp)



fut.corr.sub <- fut.corr 
fut.corr.sub$rcp2 <- ifelse(fut.corr.sub$time_period %in% c("1950-1964", "1965-2018"), "historical", fut.corr.sub$rcp) 


color.scheme <- c("#d7191c",
                  "#fdae61",
                  "#ffffbf",
                  "#abd9e9",
                  "#2c7bb6")

temp.boxes <- ggplot()+geom_boxplot(data = fut.corr.sub, aes(time_period, tmax.corrected, fill = rcp2), outlier.alpha = 0.1)+
  scale_fill_manual(values = rev(color.scheme))+theme_bw(base_size = 12)+
  ylab("Spring - Fall Max. Temperature")+xlab("Time Period")+theme(panel.grid = element_blank(), legend.title = element_blank())

ppt.boxes <- ggplot()+geom_boxplot(data = fut.corr.sub, aes(time_period, ppt.corrected, fill = rcp2),  outlier.alpha = 0.1)+
  scale_fill_manual(values = rev(color.scheme))+theme_bw(base_size = 12)+
  ylab("Total Precipitation")+xlab("Time Period")+theme(panel.grid = element_blank(), legend.title = element_blank())


rcp.legend <- cowplot::get_legend(ppt.boxes)

png(height = 4, width = 10, units = "in", res = 300, "paper_figures/future_mean_corrected_climate_summary.png")
cowplot::plot_grid(
  ppt.boxes+theme(legend.position = "none"), 
  temp.boxes+theme(legend.position = "none"),
  rcp.legend, 
  ncol = 3, rel_widths = c(1,1,0.2),labels = c("A", "B", ""), label_fontface = "plain"
)
dev.off()

test.df <- fut.corr %>% filter(lat %in% ll.df[120,]$lat & lon %in% ll.df[120,]$lon)
test.df$year <- as.numeric(test.df$year)

ggplot()+geom_line(data = test.df, aes(year, tmax.corrected, color = modelrun))+
  geom_line(data = test.df, aes(year, PRISM_fall_spr_tmax), color = "black")+facet_wrap(~rcp)


ggplot()+geom_line(data = test.df, aes(year, ppt.corrected, color = modelrun))+
  geom_line(data = test.df, aes(year, PRISM_ppt), color = "black")+facet_wrap(~rcp)


# okay lets save the future climate with the matched means:

saveRDS(fut.corr, "full_time_mean_corrected_CMIP5_model_timeseries.RDS")
fut.corr <- readRDS( "full_time_mean_corrected_CMIP5_model_timeseries.RDS")
fut.corr.sub <- fut.corr %>% filter(year >=2018) %>% select(lat, lon, year, rcp, modelrun, ppt.corrected, tmax.corrected)

saveRDS(fut.corr.sub, "pipo.cores.ds.mean.correct.climate_2018_2099.RDS")



