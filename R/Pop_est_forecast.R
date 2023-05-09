# script for scaling up estimates:

library(viridis)
library(here)
library(tidyverse)
library(rFIA)
library(sf)
AGB <- readRDS("outputs/parse.DIDD.mort.RDS")
colnames(AGB)[1] <- "PLT_CN"

db <- readRDS("data/InWeUS_FIAdb.rds")
db$PLOT <- db$PLOT %>% filter(STATECD %in% c(4, 8, 35, 49, 56, 16, 30))#& CN %in% unique(AGB$CN))
db$POP_EVAL <- rbind(read.csv("data/AZ_POP_EVAL.csv"), read.csv("data/NM_POP_EVAL.csv"), read.csv("data/UT_POP_EVAL.csv"), read.csv("data/CO_POP_EVAL.csv"),  read.csv("data/MT_POP_EVAL.csv"), read.csv("data/ID_POP_EVAL.csv"), read.csv("data/WY_POP_EVAL.csv"))
db$POP_ESTN_UNIT <- rbind(read.csv("data/AZ_POP_ESTN_UNIT.csv"), read.csv("data/NM_POP_ESTN_UNIT.csv"),  read.csv("data/UT_POP_ESTN_UNIT.csv"),  read.csv("data/CO_POP_ESTN_UNIT.csv"), read.csv("data/MT_POP_ESTN_UNIT.csv"),  read.csv("data/ID_POP_ESTN_UNIT.csv"),  read.csv("data/WY_POP_ESTN_UNIT.csv"))
db$POP_PLOT_STRATUM_ASSGN <- rbind(read.csv("data/AZ_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/NM_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/UT_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/CO_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/MT_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/ID_POP_PLOT_STRATUM_ASSGN.csv"), read.csv("data/WY_POP_PLOT_STRATUM_ASSGN.csv"))
db$POP_EVAL_TYP <- rbind(read.csv("data/AZ_POP_EVAL_TYP.csv"), read.csv("data/NM_POP_EVAL_TYP.csv"), read.csv("data/UT_POP_EVAL_TYP.csv"), read.csv("data/CO_POP_EVAL_TYP.csv"), read.csv("data/MT_POP_EVAL_TYP.csv"), read.csv("data/ID_POP_EVAL_TYP.csv"), read.csv("data/WY_POP_EVAL_TYP.csv"))
db$POP_STRATUM <- rbind(read.csv("data/AZ_POP_STRATUM.csv"), read.csv("data/NM_POP_STRATUM.csv"), read.csv("data/UT_POP_STRATUM.csv"), read.csv("data/CO_POP_STRATUM.csv"), read.csv("data/MT_POP_STRATUM.csv"), read.csv("data/ID_POP_STRATUM.csv"), read.csv("data/WY_POP_STRATUM.csv"))

## Select only the columns we need from each table, to keep things slim
PLOT <- select(db$PLOT, CN, MACRO_BREAKPOINT_DIA, ECOSUBCD)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)


PLOT$ECOSUBCD <- str_trim(PLOT$ECOSUBCD)
PLOT$ECODIV <- str_sub(PLOT$ECOSUBCD, 1, -3)
## One is doesnt work
# PLOT <- PLOT %>%
#   mutate(ECODIV = case_when(ECODIV == 'M313' ~ '313',
#                             TRUE ~ ECODIV))

POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)



ids <- db$POP_EVAL %>%
  select('CN', 'END_INVYR', 'EVALID') %>%
  inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>%
  ## Now we filter out everything except current area and 
  ## current volume ids
  filter(EVAL_TYP %in% c('EXPCURR', 'EXPVOL'))


db <- clipFIA(db, evalid = ids$EVALID)
## Select only the columns we need from each table, to keep things slim
PLOT <- select(PLOT, CN, MACRO_BREAKPOINT_DIA, ECODIV, ECOSUBCD)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD, FORTYPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)
POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)




# join with forecasts:
colnames(AGB)[1] <- "CN"
data <- PLOT %>% #filter 
  ## Add a PLT_CN column for easy joining
  mutate(PLT_CN = CN) %>%
  # ## Join COND & TREE
  left_join(COND, by = 'PLT_CN') %>%
  left_join(TREE, by = c('PLT_CN', 'CONDID')) %>%
  ## Population tables
  left_join(POP_PLOT_STRATUM_ASSGN, by = 'PLT_CN') %>%
  left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
  left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
  left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
  left_join(POP_EVAL_TYP, by = 'EVAL_CN')


# ## Join the tables
# data <- PLOT %>%
#   ## Add a PLT_CN column for easy joining
#   mutate(PLT_CN = CN) %>%
#   ## Join COND & TREE
#   left_join(COND, by = 'PLT_CN') %>%
#   left_join(TREE, by = c('PLT_CN', 'CONDID')) %>%
#   ## Population tables
#   left_join(POP_PLOT_STRATUM_ASSGN, by = 'PLT_CN') %>%
#   left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
#   left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
#   left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
#   left_join(POP_EVAL_TYP, by = 'EVAL_CN')
#Now let’s make a column that will adjust for non-response in our sample 
#(See Bechtold and Patterson (2005), 3.4.3 ‘Nonsampled Plots and Plot Replacement’). 
#Since we know there are no macroplots in RI, we don’t really need to worry about that here,
# but we will show you anyways.

## Make some adjustment factors
data <- data %>%
  mutate(
    ## AREA
    aAdj = case_when(
      ## When NA, stay NA
      is.na(PROP_BASIS) ~ NA_real_,
      ## If the proportion was measured for a macroplot,
      ## use the macroplot value
      PROP_BASIS == 'MACR' ~ as.numeric(ADJ_FACTOR_MACR),
      ## Otherwise, use the subpplot value
      PROP_BASIS == 'SUBP' ~ ADJ_FACTOR_SUBP),
    ## TREE
    tAdj = case_when(
      ## When DIA is na, adjustment is NA
      is.na(DIA) ~ ADJ_FACTOR_SUBP,
      ## When DIA is less than 5", use microplot value
      DIA < 5 ~ ADJ_FACTOR_MICR,
      ## When DIA is greater than 5", use subplot value
      DIA >= 5 ~ ADJ_FACTOR_SUBP
    ))
#Next, we need to construct what Bechtold and Patterson (2005) called a ‘domain indicator function’. 
#(see Eq. 4.1, pg. 47 of the publication). This is essentially just a vector which indicates whether 
#a tree (or plot, condition, etc.) is within our domain of interest (live trees on forest land).
## Build a domain indicator for land type and live trees
## Land type (all forested area)
data$aDI <- if_else(data$COND_STATUS_CD == 1, 1, 0)
## Live trees only (on forested area)
data$tDI <- if_else(data$STATUSCD == 1, 1, 0) * data$aDI

## Now, le
data <- data %>%
  mutate(YEAR = END_INVYR) %>%
  ## remove any NAs
  filter(!is.na(YEAR))

unique(data$YEAR)
## Estimate Tree totals
colnames(AGB)[1] <- "PLT_CN"
AGB <- AGB %>% filter(parse == "full") # just get the full scernario
periodic.data <- data %>% filter(PLT_CN %in% unique(AGB$PLT_CN))
periodic.data$EXPNS
periodic.data$CONDPROP_UNADJ
unique(periodic.data$aAdj)
unique(periodic.data$FORTYPCD)
unique(periodic.data)

# notes on how to adjust this:
# We already have plot totals for each plot, need to multiply by the EXPNS
# make sure that EXPNS is a plot value--It looks like it from the manual and dataset


AGB.periodic <- left_join(periodic.data, AGB)



# tre_bio <- AGB.periodic  %>%
#   filter(EVAL_TYP == 'EXPVOL') %>%
#   ## Make sure we only have unique observations of plots, trees, etc.
#   distinct( ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE,  mort.scheme, rcp, parse, year, .keep_all = TRUE) %>%
#   ## Plot-level estimates first (multiplying by EXPNS here)
#   group_by(YEAR, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN,  mort.scheme, rcp, parse, year) %>%
#   summarize(bioPlot = mAGB* EXPNS / 2000,
#             carbPlot = bioPlot*0.501) %>%
#   ## Now we simply sum the values of each plot (expanded w/ EXPNS)
#   ## to obtain population totals
#   group_by(year) %>%
#   summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
#             CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))
# 
# ## Estimate Area totals
# area_bio <- AGB.periodic %>%
#   filter(EVAL_TYP == 'EXPCURR') %>%
#   ## Make sure we only have unique observations of plots, trees, etc.
#   distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
#   ## Plot-level estimates first (multiplying by EXPNS here)
#   group_by(year, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
#   summarize(forArea = sum(CONDPROP_UNADJ *  EXPNS, na.rm = TRUE)) %>% # I changed this not sure if its right
#   ## Now we simply sum the values of each plot (expanded w/ EXPNS)
#   ## to obtain population totals
#   group_by(year) %>%
#   summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))
# 
# # assume the area is the same for all values going forward?
# # proably want to adjust this but for now lets assume
# area_bio_full <- data.frame(year = 2002:2099, 
#                             AREA_TOTAL = rep(area_bio$AREA_TOTAL, length(2002:2099)))
# 
# #Then we can join these tables up, and produce ratio estimates:
# 
# bio <- left_join(tre_bio, area_bio_full) %>%
#   mutate(BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
#          CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL) %>%
#   ## Reordering the columns
#   select(year, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL)
# 
# #biomass(clipFIA(fiaRI), totals = TRUE)
# 
# ggplot(bio, aes(year, BIO_AG_TOTAL))+geom_line()

# lets add some grouping variables by ECOTYPE CODES

#Without Sampling Errors
#Now we are ready to start computing estimates. If we don’t care aboute sampling errors, 
#we can use the EXPNS column in the POP_STRATUM table to make our lives easier. 
#EXPNS is an expansion factor which descibes the area, in acres, that a stratum represents 
#divided by the number of sampled plots in that stratum (see Bechtold and Patterson (2005), 
#section 4.2 for more information on FIA stratification procedures). When summed across summed 
#across all plots in the population of interest, EXPNS allows us to easily obtain estimates of 
#population totals, without worrying about fancy stratifaction procedures and variance estimators.

#Adding grouping variables by ecotype code
#To add grouping variables to the above procedures, we can simply add the names 
#of the variables we wish to group by to the group_by call:
## Grouping by Ownership group (OWNGRPCD)
## Estimate Tree totals
tre_bioGrp <- AGB.periodic %>%
  filter(EVAL_TYP == 'EXPVOL') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ECODIV, ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE,year, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(year, ECODIV, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(bioPlot = mAGB* EXPNS / 2000,
            carbPlot = bioPlot*0.501) %>%  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(year, ECODIV) %>%
  summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
            CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))
tre_bioGrp 

## Estimate Area totals by ECOTYPCD
area_bioGrp <- AGB.periodic %>%
  filter(EVAL_TYP == 'EXPCURR') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ECODIV, ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(year, ECODIV, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(forArea = sum(CONDPROP_UNADJ *  EXPNS, na.rm = TRUE)) %>% # I changed this not sure if its right
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(year, ECODIV) %>%
  summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))
area_bioGrp 


## Now we can simply join these two up, and produce ratio estimates
bioGrp <- left_join(tre_bioGrp, area_bioGrp[,c("ECODIV", "AREA_TOTAL")], by = "ECODIV") %>%
  mutate(BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
         CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL) %>%
  ## Reordering the columns
  select(year, ECODIV, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL)

# 313 is the only ecodivs in AZ with PIPO
ggplot(bioGrp, aes(year, BIO_AG_TOTAL, color = as.character(ECODIV)))+geom_line()
bio.C.diff <- bioGrp %>% filter(year %in% 2002 | year == 2098) %>% select(ECODIV, year, CARB_AG_TOTAL)%>% group_by(ECODIV) %>% spread(year, CARB_AG_TOTAL) %>%
  summarise(deltaC = `2098`-`2002`,
            pct.deltaC = (`2098`-`2002`)/`2002`) %>% mutate(MAP_UNIT_S = ECODIV)

# link up with ecoregion map in AZ:

eco.regions <- read_sf( "data/S_USA/S_USA.EcoMapProvinces.shp")

eco.regions %>%
  ggplot() +
  geom_sf() +
  theme_bw()

eco.regions.Cdiff <- left_join(eco.regions, bio.C.diff)

eco.regions.Cdiff %>%
  ggplot() +
  geom_sf(aes(fill = deltaC)) +
  theme_bw() 

eco.regions.Cdiff %>%
  ggplot() +
  geom_sf(aes(fill = pct.deltaC)) +
  theme_bw() 
ggsave(height = 4, width = 8, units = "in", "outputs/full_changepctC_ecoregion.png")
  
eco.regions.Cdiff %>%
  ggplot() +
  geom_sf(aes(fill = MAP_UNIT_S)) +
  theme_bw() 

st_crs(eco.regions)
#-124.79,49.38, 24.41, -101
bbox <- st_as_sf(as(raster::extent(-124.79, -101, 24.41, 49.38), "SpatialPolygons"))
st_crs(bbox) <- 4326
bbox <- st_transform(bbox, st_crs(eco.regions))

eco_crop <- st_crop(eco.regions, bbox)
unique(eco.regions$MAP_UNIT_S)
eco.regions %>% filter(MAP_UNIT_S %in% unique(bioGrp$ECODIV))
