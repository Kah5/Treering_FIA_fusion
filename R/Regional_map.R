# map of PIPO cored locations with species range distribution on it
library(dplyr)
library(ggplot2)
#library(sp)
library(sf)
library(tidyr)
#library(rFIA)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(mapdata)
library(maptools)

# make sure that Rdriver_local_regional.R has been run

unique(cov.data.regional$PLT_CN)
cov.data.regional$LAT <- PLOT$LAT[match(cov.data.regional$PLT_CN, PLOT$CN)]
cov.data.regional$LON <- PLOT$LON[match(cov.data.regional$PLT_CN, PLOT$CN)]

# get the states:
# make a map of all of these:
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona", "utah", "new mexico", "colorado","idaho", "wyoming", "montana", "nevada", 
                                                "california", "oregon", "washington", "texas", "kansas", 
                                                "nebraska", "north dakota", "south dakota", "oklahoma") )
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

dir.path = "/Users/kellyheilman/USTreeAtlas/shp/" # where the distribution shape files are

spp <- "pinupond"
spp.distribution <- st_read(paste0(dir.path, spp, "/"))


if(file.exists(paste0(dir.path, spp, "/"))){
  spp.distribution <- st_read(paste0(dir.path, spp, "/"))
  
  plt.spp <- plt.shp %>% filter(shp.code %in% spp)
  
  # plot distribution 
  spp.distribution %>% 
    ggplot() +
    geom_polygon(data = states, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_polygon(data = mexico, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_polygon(data = canada, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_sf(alpha = 0.5, fill = "forestgreen")+
    geom_point(data = cov.data.regional, aes(x = LON, y = LAT), size = 0.75)+theme_bw()+
    coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.text = element_blank())
  
  ggsave(height = 6, width = 8, units = "in", here("outputs/cored_pipo_distn_", paste0(spp, "_distribution_map_zoom.png")))
  
  spp.distribution %>% 
    ggplot() +
    geom_polygon(data = states, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_polygon(data = mexico, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_polygon(data = canada, 
                 aes(x=long, y=lat, group = group), 
                 color = "black", fill = "white") +
    geom_sf(alpha = 0.5, fill = "forestgreen")+
    geom_point(data = cov.data.regional, aes(x = LON, y = LAT), size = 0.75)+theme_bw()+
    coord_sf(xlim = c(-125, -100), ylim = c(20, 52))+theme(axis.text = element_blank())
  
  ggsave(height = 6, width = 8, units = "in", here("outputs/cored_pipo_distn_", paste0(spp, "_distribution_map_full.png")))
  
  
  }  

