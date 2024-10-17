# code to get most recent FIA surveys (after 2010) and matach up with our trees to see if we can have "out" of sample validation:
#library(DBI)
library(dplyr)
library(ggplot2)
#library(sp)
#library(sf)
library(tidyr)
library(rFIA)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(reshape2)

# note that you will need to change path names throughout this script

#if(!exists("/Users/kellyheilman/Documents/Treering_FIA_fusion/data/InWeUS_FIAdb.rds")){
 # fiadb <- getFIA(states = c("AZ", "NM","UT", "CO", "ID", "WY", "MT"), dir = "InWeUS_FIA", common = FALSE, tables = c("PLOT", "TREE", "COND", "SUBPLOT"), nCores = 1)
#}else{
 #fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))
#saveRDS(fiadb, "/Users/kellyheilman/Documents/Treering_FIA_fusion_PIPO/data/InWeUS_FIAdb_cyverse.rds")
  fiadb <- readRDS("/Users/kellyheilman/Documents/Treering_FIA_fusion_PIPO/data/InWeUS_FIAdb_cyverse.rds")
#}
# save as RDS:
setwd("/Users/kellyheilman/Documents/Treering_FIA_fusion_PIPO/")

PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE
#TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE$INVYR)
unique(fiadb$PLOT$STATECD)


# read in the non-AZ points:
region.rwl <- read.csv("data/trees-rwl-1-31-17.csv") # note that this data is has all RWLS in columsn and a year column
region.ll <- read.csv("data/locs-env-1-31-17.csv")


#unique(region.ll $CN %in% TREE$CN)
# also new mexico data:
nm.meta <- read.delim("data/new-mexico-meta.txt", sep = ",", as.is = TRUE)
nm.rwl <- read.delim("data/new-mexico-ring-width.txt", sep = ",", as.is = TRUE)


colnames(region.ll)
colnames(nm.meta)

# get the appropriate variables to join the data with the TREE table
nm.meta.s <- nm.meta[,c("TRE_CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD","CN", "DIA")]

# change tree_cn to the CN to match the tree table
colnames(nm.meta.s) <- c("CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD","CORE_CN", "DIA")

nm.meta.s$CN %in% TREE$CN

colnames(region.ll)[20] <- "CORE_CN"

FIA.nm <- left_join(nm.meta.s, 
                    TREE, by = c("CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA"))

# join the new mexico and the rest of the nonAZ data
region.ll.nm <- rbind(nm.meta.s[,c("CORE_CN","CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA")], region.ll[,c("CORE_CN","CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA")])

# Match up the tree and plot data
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]

# example plot of all the data (missing NM)
ggplot(TREE %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()

# we should be able to use STATECD, COUNTYCD, PLOT, SUBP, TREE, and SPCD & DIA from region.ll to match with trees from FIADB
colnames(region.ll.nm)[2] <- "TRE_CN"
FIA.outside.AZ <- left_join(region.ll.nm[,c("CORE_CN","TRE_CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA")], 
                            TREE, by = c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA"))


View(FIA.outside.AZ)
FIA.not.orphaned.plots.nona <- FIA.outside.AZ[!is.na(FIA.outside.AZ$INVYR),]
short.FIA <- FIA.not.orphaned.plots.nona[,c("PLT_CN","STATECD","COUNTYCD","PLOT", "SUBP", "TREE")]

#FIA.outside.AZ
# now get the repeated survey data for those FIA plots:
unique(FIA.not.orphaned.plots.nona$PLT_CN %in% TREE$PLT_CN)



TREE_REMEAS <- subset(TREE, !is.na(PREVDIA))# keep trees that were remeasured (n = 382530)

#ggplot(TREE_REMEAS %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()

TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1 | STATUSCD == 2)# trees that either lived or died (n= 357303)

TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1)# Only keep trees that didn't die (n= 244816)

# get the previous DIA and INVYR for the previous survey
TREE_REMEAS$PREV_TRE_CN %in% TREE$CN

TREE_REMEAS$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$PREV_CONDID <- TREE$CONDID[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T1_DIA <- TREE$DIA[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]

TREE_REMEAS$T1_INVYR <- TREE$INVYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T1_MEASYR <- TREE$MEASYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T2_MEASYR <- TREE_REMEAS$MEASYR
TREE_REMEAS$T2_DIA <- TREE_REMEAS$DIA
TREE_REMEAS$T2_TRE_CN <- TREE_REMEAS$CN
TREE_REMEAS$T1_TRE_CN <- TREE_REMEAS$PREV_TRE_CN

TREE_REMEAS$PREV_PREV_TREE_CN <- TREE$PREV_TRE_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]


# calculate static SDI for each subplot and measure year
static_SDI <- TREE %>% ungroup() %>%  filter(AGENTCD >= 0 & STATUSCD ==1) %>% group_by(PLT_CN, STATECD, PLOT, SUBP, MEASYR) %>% filter(DIA > 1) %>%
  summarise(ntrees_static = n(),
            TPA_static = sum(TPA_UNADJ, na.rm = TRUE), 
            Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/length(DIA)), 
            SDIs_static = ((Dq_static/10)^1.6)*TPA_UNADJ, #calculate SDI (Summation Method) on the subplot:
            SDIdq_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE))#, ## calculate SDI (Quadratic mean diameter) on the subplot:
#SDIrat_static = SDIs/SDIdq) 

# summarise, note that these are for all species
hist(static_SDI$SDIdq_static)
summary(static_SDI)


# calculate static SDI for each PLOT and measure year
static_SDI_pltcn <- TREE %>% ungroup() %>%  filter(AGENTCD >= 0 & STATUSCD ==1) %>%  filter(DIA > 1) %>%
  group_by(PLT_CN, STATECD, PLOT, COUNTYCD,  MEASYR) %>%
  summarise(ntrees_static = n(),
            TPA_static =sum(TPA_UNADJ), 
            Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/ntrees_static), 
            SDIdq_static = ((Dq_static/10)^1.6)*TPA_static, #calculate SDI (Summation Method) on the subplot:
            SDIs_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE))#, ## calculate SDI (Quadratic mean diameter) on the subplot:
#SDIrat_static = SDIs/SDIdq) 

hist(static_SDI_pltcn$SDIdq_static)

static_SDI_pltcn.unique <- unique(static_SDI_pltcn[!duplicated(static_SDI_pltcn),])

saveRDS(static_SDI_pltcn.unique, "data/static_SDI_by_plot_pltcn.unique.rds")
#test.join.sdi <- merge(FIA.outside.AZ, static_SDI_pltcn.unique, by = c("PLT_CN", "STATECD","COUNTYCD", "PLOT", "MEASYR"))
# get the increment data, trees stacked with rwls:
#saveRDS(test.join.sdi, "data/FIA_outside_AZ_ll_SDI_plot.rds")

# full.clim.data <- read.csv("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/pipo_all_tmean_ppt_v3.csv")
# region.rwl <- read.csv("data/trees-rwl-1-31-17.csv") # note that this data is has all RWLS in columsn and a year column
# region.ll <- read.csv("data/locs-env-1-31-17.csv", stringsAsFactors = TRUE)
# region.ll$DIA_cm <- region.ll$DIA*2.54 # convert these DBH to cm (needed to back cacluate tree diameters for cored trees)
# 
# # here we should probably be joining by CN's...but not sure if it is the core cn or tree cn.
# FIA.outside.AZ.cn <- left_join(region.ll[,c("CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD","series", "DIA", "ELEV", "LAT", "LON", "ELEV", "ASPECT", "SLOPE", "SICOND", "DIA_cm")], 
#                             TREE, by = c("CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA"))
# 
FIA.outside.AZ$MEASYEAR <- ifelse(!is.na(FIA.outside.AZ$MEASYR), FIA.outside.AZ$MEASYR, FIA.outside.AZ$INVYR)

# read in AZ data:

PIPO.az.cov <- read.delim("data/PIPOCores518Meta.txt")

PIPO.az.sub <- PIPO.az.cov[,c("CORE_CN","TRE_CN","PLT_CN","STATECD", "COUNTYCD", "PLOT", "SUBPLOT",  "TREE", "SPCD", "T1_DIA", "MEASYEAR")]
#PIPO.az.sub$STATECD <- 4

#PIPO.az.sub$SPCD <- 122

#PIPO.az.sub$series <- 1:length(PIPO.az.sub$CountyNo)
# plot design codes are all listed as either 1 or NA in the AZ data...so I am using the default TPA_UNADJ
PIPO.az.sub$TPA_UNADJ <- 6.01805 # This TPA_UNADJ needs to be corrected to the right TPA_UNAJ not sure that AZ plots are here

colnames(PIPO.az.sub) <- c("CORE_CN", "CN","PLT_CN","STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA",  "MEASYR","TPA_UNADJ")

PIPO.az.sub$DIA_cm <- PIPO.az.sub$DIA*2.54 # convert to cm



# join up with the ring width data (AZ):
AZ.growth <- read.delim("data/PIPOCores518RingWidths.txt")

colnames(AZ.growth) <- c("RW", "Year", "CORE_CN")
AZ.cov.growth.data <- left_join(AZ.growth[,c("CORE_CN", "Year", "RW")], PIPO.az.sub, by="CORE_CN") 



# merge non az data cored data and the regional data
summary(FIA.outside.AZ)
region.rwl.m <- reshape2::melt(region.rwl[,2:length(region.rwl)], id.vars = "Year")
colnames(region.rwl.m)<- c("Year", "CORE_CN", "RW")
colnames(nm.rwl) <- c("RW", "Year", "CORE_CN")
# join with NM rwl data:
region.rwl.m$CORE_CN <- as.character(region.rwl.m$CORE_CN)

regional.nonaz.rwl<- rbind(region.rwl.m[,c("CORE_CN", "Year", "RW")], nm.rwl[,c("CORE_CN", "Year", "RW")])


# all the PLT_CN's turn to NAs when we merge these
full.inc.nonaz <- left_join(regional.nonaz.rwl, FIA.outside.AZ , by = "CORE_CN")
full.inc.nonaz.nona <- full.inc.nonaz [!is.na(full.inc.nonaz $RW),]
full.inc.nonaz$DIA_cm <- full.inc.nonaz$DIA*2.54 # convert to centimeters

unique(full.inc.nonaz$STATECD)

# adapting from Courtney Giebink's code: https://github.com/clgiebink/UT_FVS/blob/master/scripts/AnnualizeDBH.R
#dplyr::select the covariates
nonaz_2cov <- full.inc.nonaz %>%
  dplyr::select(CORE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT, TREE, SPCD,SUBP,MEASYR,TPA_UNADJ,
                CN,
                DIA_cm, DIA)
nonaz_2cov$RW <- nonaz_2cov$RW/10 # convert mm to cm--I am assuming all the measurements are in mm

#dplyr::select the covariates for the AZ data
naz_2cov <- AZ.cov.growth.data %>%
  dplyr::select(CORE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT, TREE, SPCD,SUBP,MEASYR,TPA_UNADJ,
                CN,
                DIA_cm, DIA)
naz_2cov$RW <- naz_2cov$RW/10 # convert mm to cm--I am assuming all the measurements are in mm

all.region.data <- rbind(nonaz_2cov, naz_2cov) # combine all regional data togther 
all.pipo <- all.region.data  %>% filter(SPCD == 122) # just get PIPO

#ggplot(all.pipo, aes(x = STATECD,y= RW, group = STATECD))+geom_boxplot()

head(all.region.data)
#dplyr::select pipos that have tree CNs, and dont have NAs
PIPO.filtered <- all.region.data  %>% filter(SPCD == 122 & !is.na(RW) & !is.na(CN)) %>% rename(TRE_CN = CN, MEASYEAR = MEASYR)
plots.in.region<- all.region.data[, c("PLT_CN", "CN", "STATECD","PLOT", "SUBP", "TREE")]
ok.plots <- unique(plots.in.region[, c("PLT_CN", "STATECD", "PLOT","SUBP")])
regional.tree <- TREE %>% filter(PLT_CN %in% ok.plots$PLT_CN) # the tree table to draw from

unique(PIPO.filtered$STATECD)


# get SICOND for all the data:
PIPO.cov <- left_join(PIPO.filtered, COND[,c("PLT_CN", "STATECD", "COUNTYCD", "PLOT", "CONDID", "SICOND", "STDAGE")])
PIPO.cov <- PIPO.cov %>% filter(CONDID == 1) # just get the plot center condition class # check this
#PIPO.filtered %>% group_by(CORE_CN, PLT_CN) %>% 

# join with time varying variables including climate and time varying SDI:
tv.sdi <- readRDS("data/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v4.RDS")

PIPO.cov.sdi <- left_join( PIPO.cov,tv.sdi, by = c("STATECD", "PLOT", "SUBP", "PLT_CN", "Year"))

# only keep those with time varying SDI
PIPO.cov.sdi <- PIPO.cov.sdi %>% filter(!is.na(SDIs))


unique(PIPO.cov.sdi$STATECD) # double check that all states are present: 8 16 30 49 56 35  4

#cov.data.full.p <- cov.data.full[!is.na(cov.data.full$LAT),]
saveRDS(PIPO.cov.sdi, "PIPO.united.covariates.sdi.rds")

# reduce to get cov.data
cov.data <- unique(PIPO.cov.sdi[,c("CORE_CN", "PLT_CN", "STATECD", "COUNTYCD", "PLOT", "SUBP", "SPCD", "TRE_CN", "DIA_cm", "DIA", "SICOND", "STDAGE")])


ggplot(PIPO.cov.sdi, aes(y = SDIs, x = RW, color =  DIA_cm))+geom_point()+
  facet_wrap(~STATECD)

ggplot(PIPO.cov.sdi, aes(y = RW, x = SDIs, color =  DIA_cm))+geom_point()+
  facet_wrap(~STATECD)

ggplot(PIPO.cov.sdi, aes(y = SDIs, x = RW, color = STATECD))+geom_point()

#-----------------------------------------------------------------------------------------
# structure data for jags
#-----------------------------------------------------------------------------------------


# get the pipo only ring widths:
pipo.rwl.m <- PIPO.cov.sdi %>%dplyr::select(CORE_CN, PLT_CN, Year, RW)
# get the years that match with our years of interest:
estimate.yrs <- 1966:2013
pipo.subset <- pipo.rwl.m %>% filter(Year %in% estimate.yrs)

# now rearrange so that each row is a tree and each column is a year from 1966-2010
spread.pipo.mat <- pipo.subset %>% group_by(CORE_CN) %>% tidyr::spread(Year, RW)
spread.pipo.mat <- spread.pipo.mat[!duplicated(spread.pipo.mat$CORE_CN),]
spread.pipo.mat <- data.frame(spread.pipo.mat)


# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
#ordered.pipo.mat<- spread.pipo.mat[order(match(spread.pipo.mat[,"variable"], pipo.ll[,"series"])),]
#colnames(ordered.pipo.mat)[2:length(ordered.pipo.mat)] <- colnames(y)
y <- as.matrix(spread.pipo.mat[,3:length(spread.pipo.mat)])


# likewise get DBH values
pipo.dbh.m <- PIPO.cov.sdi %>%dplyr::select(CORE_CN, PLT_CN, Year, DIA_cm, MEASYEAR) %>% filter(Year %in% estimate.yrs)

# if !year == MEASYEAR, make DIA NA
dbh.measyr <- pipo.dbh.m %>%   complete(Year, CORE_CN) %>% group_by(CORE_CN) %>% # get the complete years (some cores have the DBH measured the year after growth starts)
  mutate(
    DIA_cm = max(DIA_cm, na.rm = TRUE), 
    PLT_CN = mean(PLT_CN, na.rm = TRUE), 
    MEASYEAR = mean(MEASYEAR, na.rm = TRUE),
    DIA_measyr = ifelse(Year == MEASYEAR, DIA_cm, NA))

# get the additional DBH measurements for Arizona and their years---the archived data does not have hte years for some reason:
AZ.data <- read.delim("data/AZ_FIA_RWL_PRISM_allinone_04192017.txt")
AZ.data.sub <- AZ.data[,c("CountyNo", "PlotNo", "SubplotNo", "TreeNo", "DBH", "T1_MEASYR", "T2_DIA", "T2_MEASYR" )]

colnames(AZ.data.sub) <- c("COUNTYCD", "PLOT", "SUBP", "TREE", "DIA", "MEASYR", "T2_DIA", "T2_MEASYR")


# merge with AZ dataset:
test <- left_join(PIPO.az.sub, AZ.data.sub, by = c("COUNTYCD", "PLOT", "SUBP", "TREE", "MEASYR", "DIA"))
repeat.AZ.dbh <- test %>% filter(!is.na(T2_DIA )) %>% mutate(T2_DIA_cm = T2_DIA*2.54) %>% 
  
 dplyr::select(CORE_CN, PLT_CN, T2_MEASYR, T2_DIA_cm , T2_MEASYR, T2_DIA_cm)
repeat.AZ.dbh$CORE_CN <- as.character(repeat.AZ.dbh$CORE_CN)
repeat.AZ.dbh$PLT_CN <- as.character(repeat.AZ.dbh$PLT_CN)

colnames(repeat.AZ.dbh)[3:4] <- c("Year", "DIA_cm")
repeat.AZ.dbh$MEASYEAR <- repeat.AZ.dbh$Year
repeat.AZ.dbh$DIA_measyr <- repeat.AZ.dbh$DIA_cm

repeat.AZ.dbh.nona <- repeat.AZ.dbh %>% distinct() %>% group_by(CORE_CN, PLT_CN) #%>% tidyr::spread(T2_MEASYR, T2_DIA_cm)
dbh.measyr$PLT_CN <- as.character(dbh.measyr$PLT_CN)

dbh.measyr.repeats <- rbind(dbh.measyr, repeat.AZ.dbh.nona)
dbh.measyr.repeats <- dbh.measyr.repeats[!duplicated(dbh.measyr.repeats),]

dbh.measyr.repeats.nona <- dbh.measyr.repeats %>%dplyr::select(CORE_CN, PLT_CN, DIA_cm, MEASYEAR, DIA_measyr) %>% 
  distinct() %>% filter(!is.na(DIA_measyr)) %>%
  group_by(CORE_CN, PLT_CN) %>% mutate(Year = MEASYEAR)

year.df <- expand.grid(Year = 1966:2013, CORE_CN = unique(dbh.measyr.repeats.nona$CORE_CN))
cn.df <- unique(dbh.measyr.repeats.nona[,c("CORE_CN", "PLT_CN")])

year.cn.df <- left_join(year.df, cn.df)

dbh.measyr.repeats.full <- left_join(year.cn.df, dbh.measyr.repeats.nona)

spread.dbh.mat <- dbh.measyr.repeats.full %>%dplyr::select(-DIA_cm, -MEASYEAR)  %>% group_by(CORE_CN, PLT_CN) %>% tidyr::spread(Year, DIA_measyr)
spread.dbh.mat <- spread.dbh.mat[!duplicated(spread.dbh.mat$CORE_CN),]

# Check that there are records with multiple DBH values:
#View(tail(spread.dbh.mat %>% filter(CORE_CN %in% 295713486489998)))

# rearrange to match cores:
spread.dbh.mat <- data.frame(spread.dbh.mat)
spread.dbh.mat.ordered <- spread.dbh.mat[order(match(as.vector(spread.dbh.mat[,"CORE_CN"]), as.character(spread.pipo.mat[,"CORE_CN"]))),]



z <- as.matrix(data.frame(spread.dbh.mat.ordered[,3:length(spread.dbh.mat)]))
length(z[,1])
hist(z)


#-----------------------------------------------------------------------------------------
# add time varying climate data to our dataset
#-----------------------------------------------------------------------------------------

# read in the larger region climate data:
pipo.clim <- read.csv("data/pipo_all_tmean_ppt_v5.csv")

colnames(pipo.clim)

pipo.clim.one <- pipo.clim %>% filter(year %in% 1950)

#ggplot(pipo.clim.one, aes(lon, lat))+geom_point()

pipo.clim$wintP.NovAug <- rowSums(pipo.clim[,c("PPT_8", "PPT_9", "PPT_10", "PPT_11")])
pipo.clim$wintP.wateryr <- rowSums(pipo.clim[,c("PPT_1", "PPT_2", "PPT_3", "PPT_4", "PPT_5", "PPT_6", "PPT_7", "PPT_8", "PPT_9", "PPT_10", "PPT_11", "PPT_12")])
pipo.clim$wintP.NM <- rowSums(pipo.clim[,c("PPT_11", "PPT_12", "PPT_1", "PPT_2", "PPT_3")])
pipo.clim$wintP.JJ <- rowSums(pipo.clim[,c("PPT_6", "PPT_7")])

pipo.clim$tmax.fallspr <- rowMeans(pipo.clim[,c( "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12", "TMAX_1", "TMAX_2", "TMAX_3",  "TMAX_4")])
pipo.clim$tmax.JanA <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8")])
pipo.clim$tmax.MJul <- rowMeans(pipo.clim[,c("TMAX_5", "TMAX_6", "TMAX_7")])
pipo.clim$tmax.AprMayJun <- rowMeans(pipo.clim[,c("TMAX_4","TMAX_5", "TMAX_6")])
pipo.clim$tmax.fall <- rowMeans(pipo.clim[,c("TMAX_9","TMAX_10","TMAX_11")])
pipo.clim$tmax.monsoon <- rowMeans(pipo.clim[,c("TMAX_7","TMAX_8", "TMAX_9")])
pipo.clim$TMAX <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8", "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12")])
pipo.clim$TMEAN <- rowMeans(pipo.clim[,c("TMEAN_1", "TMEAN_2", "TMEAN_3", "TMEAN_4", "TMEAN_5", "TMEAN_6", "TMEAN_7", "TMEAN_8", "TMEAN_9", "TMEAN_10", "TMEAN_11", "TMEAN_12")])

require(dplyr)
pipo.clim %>% dplyr::select(lon, lat, name, year, PPT_1, PPT_2, PPT_3, PPT_4, PPT_5, PPT_6, PPT_7, PPT_8, PPT_9, PPT_10, PPT_11, PPT_12,
                            TMAX_1, TMAX_2, TMAX_3, TMAX_4, TMAX_5, TMAX_6, TMAX_7, TMAX_8, TMAX_9, TMAX_10, TMAX_11, TMAX_12,
                            wintP.NovAug , 
                            wintP.wateryr , wintP.NM ,    wintP.JJ,       tmax.fallspr  , tmax.JanA,     
                            tmax.MJul    ,  tmax.AprMayJun ,tmax.fall,      tmax.monsoon,   TMAX, TMEAN   )

#spread.pipo.mat[] 
#test.melt <- reshape2::melt(time_data)
#colnames(test.melt) <- c("TreeID", "YearID", "Value", "Climate")


#clim.spread <- test.melt %>% group_by(TreeID, YearID) %>% spread(Climate, Value)
# each climate variable has 45 years (columns) & 544 cores (rows)

# we want to add the pipo.clim values onto those 45 rows
years <- 1966:2013

pipo.clim.crop <- pipo.clim %>% filter(year %in% years)
pipo.clim.crop$X <- 1:length(pipo.clim.crop$lon)


get_ordered_climate <- function(x){
  
  
  spread.tmax.fall.spr.mat <- pipo.clim.crop %>% filter(name %in% unique(spread.pipo.mat$CORE_CN))%>% dplyr::select(lon, lat, name, year, x) %>% group_by(lon, lat, name) %>%  tidyr::spread( year, x, drop = T)
  
  
  # now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
  spread.tmax.fall.spr.mat.ordered <-  spread.tmax.fall.spr.mat[order(match(spread.tmax.fall.spr.mat$name,  unique(spread.pipo.mat[,"CORE_CN"]))),]
  climate.mat <- spread.tmax.fall.spr.mat.ordered#[,4:length(spread.tmax.fall.spr.mat.ordered)]
  climate.mat
}

tmax.fallspr <- get_ordered_climate("tmax.fallspr")
wintP.wateryr <- get_ordered_climate("wintP.wateryr")
tmax.AprMayJun <- get_ordered_climate("tmax.AprMayJun")
tmax.monsoon <- get_ordered_climate("tmax.monsoon")
TMAX <- get_ordered_climate("TMAX")
TMEAN <- get_ordered_climate("TMEAN")
#---------------------------------------------------------------
# get ordered SDI
#---------------------------------------------------------------
head(PIPO.cov.sdi)

SDI.mat <- PIPO.cov.sdi %>% filter(CORE_CN %in% unique(tmax.fallspr$name))%>% filter(Year %in% years) %>%
  dplyr::select(CORE_CN, PLT_CN,PLOT, SUBP, Year, SDIs) %>% group_by(CORE_CN, PLT_CN,PLOT, SUBP) %>%  tidyr::spread( Year, SDIs, drop = T)


# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
SDI.mat.ordered <- SDI.mat[order(match(SDI.mat$CORE_CN,  spread.dbh.mat.ordered[,"CORE_CN"])),]
SDI.mat.ordered <- SDI.mat.ordered[!duplicated(SDI.mat.ordered$CORE_CN),]
SDI.matrix <- SDI.mat.ordered[,5:length(SDI.mat.ordered)]


# because jags won't take NA values as predictors, we need to give values..so replace NA with the max or min
for(i in 1:nrow(SDI.matrix[,1])){
  for(t in 2:length(SDI.matrix[i,])){
    if(is.na(SDI.matrix[i,t])){
      SDI.matrix[i,t] <- SDI.matrix[i,t-1]
    }
    if(is.na(SDI.matrix[i,t])){
      SDI.matrix[i,t] <- min(SDI.matrix[i,], na.rm = TRUE)
    }
  }
  # if it is still NA, replace with the smallest SDI value in that row:
  
}


summary(SDI.matrix)

# make sure column names are the same
# names(time_data$tmax.AprMayJun) <- names(tmax.monsoon)
# names(time_data$tmax.monsoon) <- names(tmax.monsoon)
# names(time_data$tmax.fallspr) <- names(tmax.monsoon)
# names(time_data$wintP.wateryr) <- names(tmax.monsoon)
# names(time_data$TMAX) <- names(TMAX)
time_data <- list()
time_data$tmax.monsoon <-  as.matrix(tmax.monsoon[,4:length(tmax.monsoon)])
time_data$tmax.fallspr <- as.matrix(tmax.fallspr[,4:length(tmax.monsoon)])
time_data$wintP.wateryr <- as.matrix(wintP.wateryr[,4:length(tmax.monsoon)])
time_data$tmax.AprMayJun <- as.matrix(tmax.AprMayJun[,4:length(tmax.monsoon)])
time_data$TMAX <- as.matrix(TMAX[,4:length(tmax.monsoon)])
time_data$TMEAN <- as.matrix(TMEAN[,4:length(tmax.monsoon)])
time_data$SDI <- as.matrix (SDI.matrix)



# scale climate variables


standardize.mat <- function(x){
  scaled.data <- matrix(NA,nrow = nrow(x), ncol = ncol(x) )
  
  for(i in 1:length(x[,1])){
    x.bar <- mean(as.matrix(x[i,]), na.rm = TRUE)
    s.d. <- sd(as.matrix(x[i,]), na.rm = TRUE)
    scaled.data[i,]<- (x[i,]-x.bar)/s.d.
  }
  
  scaled.data
}

standardize.vector <- function(x){
  x.bar <- mean(as.vector(x), na.rm = TRUE)
  s.d. <- sd(x, na.rm = TRUE)
  return((x-x.bar)/s.d.)
}

time_data$TMAX.scaled <- standardize.mat(time_data$TMAX)
time_data$TMEAN.scaled <- standardize.mat(time_data$TMEAN)
time_data$tmax.fallspr.scaled <- standardize.mat(time_data$tmax.fallspr)
time_data$wateryr.scaled <- standardize.mat(time_data$wintP.wateryr)
time_data$tmax.AprMayJun.scaled <- standardize.mat(time_data$tmax.AprMayJun)
time_data$SDI.scaled <- standardize.vector(time_data$SDI)


# get index with climate variables:
keeps <- !is.na(rowMeans(time_data$TMAX.scaled, na.rm = TRUE))

# use time_data TMAX and wintP.Wayteryr columns to get mean annual temperature and mean annual max T

MAT <- rowMeans(time_data$TMEAN)
MATmax <- rowMeans(time_data$TMAX)
MAP <- rowMeans(time_data$wintP.wateryr)

cov.data.ordered <- cov.data[order(match(cov.data$CORE_CN,  unique(spread.dbh.mat.ordered[,"CORE_CN"]))),]
cov.data.ordered  <- cov.data.ordered [!duplicated(cov.data.ordered$CORE_CN),]
cov.data.ordered <- cov.data.ordered %>% filter(CORE_CN %in% TMAX$name)

cov.data.ordered$MAT <- MAT#[keeps]
cov.data.ordered$MATmax <- MATmax#[keeps]
cov.data.ordered$MAP <- MAP#[keeps]

# scale the plot level variables (just SICOND, MAT, and MAP)
cov.data.full.scaled <- cov.data.ordered %>% mutate_at(standardize.vector, .vars = c("SICOND", "MAT", "MAP"))



data <- list()



data$y <- y[keeps,]
data$z <- z[keeps,]



cov.data.regional <- cov.data.full.scaled[keeps,]


time_data$tmaxAprMayJun2002.2013 <- time_data$tmax.AprMayJun[keeps,37:48]
time_data$wintP.wateryr2002.2013 <- time_data$wintP.wateryr[keeps,37:48]
time_data$tmaxAprMayJunscaled2002.2013 <- time_data$tmax.AprMayJun.scaled[keeps,37:48]
time_data$wateryrscaled2002.2013 <- time_data$wateryr.scaled[keeps,37:48]

time_data$tmaxAprMayJunscaled <- time_data$tmax.AprMayJun.scaled[keeps,1:36]
time_data$wateryrscaled <- time_data$wateryr.scaled[keeps,1:36]
time_data$SDIscaled <- time_data$SDI.scaled[keeps,1:36]
time_data$tmaxAprMayJun <- time_data$tmax.AprMayJun[keeps,1:36]
time_data$wintP.wateryr <- time_data$wintP.wateryr[keeps,1:36]




#time_data$SDIscaled <- time_data$SDI.scaled[keeps,1:36]


# randomly sample 100 cores instead of 1000

# subsample.keep <- sample(1:length(cov.data.regional$CORE_CN), 100, replace = FALSE)
# 
# data$y <- data$y[subsample.keep,]
# data$z <- data$z[subsample.keep,]
# 
# 
# 
# cov.data.regional <- cov.data.regional[subsample.keep,]
# 
# time_data$tmaxAprMayJunscaled <- time_data$tmaxAprMayJunscaled[subsample.keep,1:36]
# time_data$wateryrscaled <- time_data$wateryrscaled[subsample.keep,1:36]
# time_data$SDIscaled <- time_data$SDIscaled[subsample.keep,1:36]

data$time <- 1966:2001
data$ni <- nrow(data$z)
data$nt <- length(1966:2001)

data$startyr <- rep(1, nrow(data$z))
data$startyr2 <- rep(2, nrow(data$z))
data$nt2 <- rep(47, nrow(data$z))
data$endyr <- rep(47, nrow(data$z))

states <- unique(cov.data$STATECD)
data$states <- states
data$STATECD <- cov.data.full.scaled$STATECD
#time_data
data$a_dbh <- 512
data$r_dbh <- 256

sd <- 0.337
shape <- (90^2) / (0.337^2)
ra <-     90    / (0.337^2)
data$a_inc <- shape
data$r_inc <- ra


data$a_add <- 1
data$r_add <-1

data$x_ic <- 1
data$tau_ic <-1e-04

data$y_ic <- mean(data$y, na.rm=TRUE)
data$tau_y_ic <-1e-04

# create a z0:
#if(is.null(z0)){
z0 <- t(apply(data$y, 1, function(y) {
  -rev(cumsum(rev(y)))
})) + data$z[, ncol(data$z)]
#} 

data$time_data <- time_data
data$ni
length(data$y[,1])
length(cov.data.regional$CORE_CN)
length(time_data$SDI.scaled[,1])
length(time_data$tmax.AprMayJun.scaled[,1])
length(time_data$wateryr.scaled[,1])
length(time_data$SDI.scaled[,1])

length(time_data$SDIscaled[1,])
length(time_data$tmax.AprMayJun.scaled[1,])
length(time_data$wateryr.scaled[1,])
length(time_data$SDI.scaled[1,])

plot( time_data$SDIscaled, data$y) # check that these make sense
plot(time_data$tmaxAprMayJunscaled, data$y) # check that these make sense
plot(time_data$wateryrscaled, data$y) # check that these make sense

plot(cov.data.regional$MAP, rowMeans(data$y, na.rm = TRUE)) # check that these make sense
plot( cov.data.regional$MAT, rowMeans(data$y, na.rm = TRUE)) # check that these make sense
plot(cov.data.regional$SICOND, rowMeans(data$y, na.rm = TRUE)) # check that these make sense
plot( cov.data.regional$MAT, rowMeans(data$y, na.rm = TRUE)) # check that these make sense


cov.data.regional$PLOTSTATE <- as.numeric(as.character(paste0(cov.data.regional$PLOT, cov.data.regional$STATECD)))
cov.data.regional$TREE <- 1:length(cov.data.regional$CORE_CN)

# save the full covariate and time-varying data 
data$cov.data.regional <- cov.data.regional
saveRDS(data, "data/regional_pipo_jags_formatted_data.RDS")


cov.data.regional<- data$cov.data.regional

cov.data.regional

cov.data.regional$CORE_CN <-  gsub('X','',cov.data.regional$CORE_CN)

#z.years <- data$z[which(!is.na(data$z))]
z.long <- melt(data$z) %>% filter(!is.na(value )) %>% mutate(year = as.numeric(gsub("X", '', Var2)))%>%
                            group_by(Var1) %>% slice_min(year, n = 1)

test <- melt(data$z) %>% filter(!is.na(value )) %>% mutate(year = as.numeric(gsub("X", '', Var2)))%>%
  group_by(Var1) %>% summarise(n())
View(z.long)

cov.data.regional$DBH_year <- z.long$year
cov.data.regional$n_DBH_meas <- test$`n()`
cov.data.regional.export <- cov.data.regional %>%dplyr::select(CORE_CN, PLT_CN, TRE_CN, STATECD, COUNTYCD, PLOT, SUBP, SPCD, DIA_cm, DIA, DBH_year, n_DBH_meas)
cov.data.regional.export

write_delim(cov.data.regional.export, "data/CORED_TREES_FOR_DIA_MATCH.txt")

rm(all.pipo)
rm(fiadb)
rm(all.region.data)
rm(COND)

rm(nm.rwl)
rm(nm.meta)

rm(plots.in.region)

# get the datasets 
# #-----------------------------------------------------------------------------------------
# # make some maps of growth, diameter to check expected growth patterns
# #-----------------------------------------------------------------------------------------
# plt.averages <- cov.data.regional
# plt.averages$FIA_LAT <- PLOT[match(plt.averages$PLT_CN, PLOT$CN),]$LAT
# plt.averages$FIA_LON <- PLOT[match(plt.averages$PLT_CN, PLOT$CN),]$LON
# plt.averages$DESIGNCD <- PLOT[match(plt.averages$PLT_CN, PLOT$CN),]$DESIGNCD
# colnames(plt.averages)
# 
# plt.averages$avg_RW <- rowMeans(data$y, na.rm =TRUE)
# plt.averages$avg_DIA_inc <- rowMeans(data$y*2, na.rm =TRUE)
# 
# # get averages for trees on the plot:
# TREE.summaries <- TREE %>% filter(PLT_CN %in% unique(plt.averages$PLT_CN) & STATUSCD == 1)%>%
#                           group_by(PLT_CN, SPCD) %>% summarise(n.tree = n(), 
#                                                          avg_DIA = mean(DIA*2.54, na.rm =TRUE), 
#                                                          max_DIA = max(DIA*2.54, na.rm =TRUE), 
#                                                          QMD = sqrt(sum(DIA^2, na.rm =TRUE)/n()),
#                                                          sd_DIA = sd(DIA*2.54, na.rm =TRUE), 
#                                                          BA_sq_ft = sum((DIA^2)*0.005454), 
#                                                          TPA.tot = sum(TPA_UNADJ, na.rm =TRUE)) %>% filter(SPCD %in% 122)
# TREE.summaries$PLT_CN
# plt.averages <- left_join(plt.averages, TREE.summaries)
# TREE.summaries$BA_sq_ft
# 
# MSB.ll <- data.frame(TPA.tot = 1:1500, 
#                      MSB.longleaf = (18.68-20.63*exp(-13.25*(1:1500)^(-0.503)))+2, 
#                      MSB.shifted = (18.68-20.63*exp(-13.25*(1:1500)^(-0.503)))+9)
# # # they shifted the curve so that most of the obs would fall inside it
# # MSB.pp <- data.frame(TPA.tot = 1:1500, 
# #                      MSB = (18.68-20.63*exp(-13.25*(1:1500)^(-0.503)))+9)
# 
# #plt.averages$sd_RW <- rowMeans(data$y, na.rm =TRUE)
# ggplot(data = TREE.summaries, aes(TPA.tot, QMD))+geom_point()+
#   geom_line(data = MSB.ll, aes(TPA.tot, MSB.longleaf), color = "red")+
#   geom_line(data = MSB.ll, aes(TPA.tot, MSB.shifted), color = "forestgreen")+xlab("TPA")
# ggsave("outputs/Mature_stand_boundary_FIA_data.png")
# 
# ggplot(TREE.summaries, aes(TPA.tot, avg_DIA), color = "red")+geom_point()+
#   geom_line(data = MSB.ll, aes(TPA.tot, MSB))
# # get quantile regression of TPA.total & QMD
# 
# 
# plt.averages$avg_cored_DIA <- rowMeans(data$z, na.rm =TRUE)
# 
# 
# library(mapdata)
# all_states <- map_data("state")
# states <- subset(all_states, region %in% c( "arizona", "utah", "new mexico", "colorado","idaho", "wyoming", "montana", "nevada", 
#                                             "california", "oregon", "washington", "texas", "kansas", 
#                                             "nebraska", "north dakota", "south dakota", "oklahoma") )
# canada <- map_data("worldHires", "Canada")
# mexico <- map_data("worldHires", "Mexico")
# 
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = MAT))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/MAT_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = MAP))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/MAP_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = n.tree))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/ntrees_plot_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = avg_DIA))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/avg_DIA_plot_map_cored_plots.png")
# 
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = BA_sq_ft))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/BA_sq_ft_plot_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = avg_RW))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/avg_RW_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = avg_cored_DIA))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())+scale_color_viridis_c()
# ggsave(height = 15, width = 10, "outputs/avg_RW_map_cored_plots.png")
# 
# ggplot()+
#   geom_polygon(data = states, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = mexico, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white") +
#   geom_polygon(data = canada, 
#                aes(x=long, y=lat, group = group), 
#                color = "black", fill = "white")+
#   geom_point(data = plt.averages, aes(x = FIA_LON, y = FIA_LAT, color = as.character(DESIGNCD)))+theme_bw()+
#   coord_sf(xlim = c(-118, -103), ylim = c(32, 49))+theme(axis.title = element_blank())
# ggsave(height = 15, width = 10, "outputs/DESIGNCD_map_cored_plots.png")
# 
# saveRDS(plt.averages, "outputs/plt.avgs.for.plotting.AGB.against.rds")
# plt.averages <- readRDS( "outputs/plt.avgs.for.plotting.AGB.against.rds")
# # read in the predicted AGB 
# DIDD.parse.df.60 <- readRDS( "outputs/temporary.DIDD.parse.df.60.rds")
# DIDD.parse.df.60$PLT_CN <- as.numeric(DIDD.parse.df.60$plot)
# AGB.proj <- left_join(DIDD.parse.df.60,plt.averages)
# a <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = n.tree, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# b <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = avg_DIA_inc, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# c <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = avg_DIA, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# d <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = BA_sq_ft, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# 
# e <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = avg_RW, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# f <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = MAT, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# g <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = MAP, y = mAGB/1000))+geom_point()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# h <- ggplot(AGB.proj %>% filter(year == 2098 & parse %in% "full"), aes(x = as.factor(DESIGNCD), y = mAGB/1000))+geom_jitter()+facet_wrap(~parse)+ylab("AGB in 2099 (Mg/ha)")
# 
# png(height = 6, width = 10, units = "in", res = 150, "outputs/mAGB_2098_vs_plot_attributes.png")
# cowplot::plot_grid(a, b,c,
#                    d, e, f,
#                    g,h, ncol = 4, align = "hv")
# dev.off()
# 
# 
# cov.data.regional$
# 
# alphas <- readRDS("/Users/kellyheilman/Documents/SSM_small_test/model6.1500.alpha_TREES.rds")
# colnames(alphas)[1] <- "L1"
# alphas$treeid <- 1:length(alphas$L1)
# 
# alphas.df <- left_join(alphas, cov.data.regional[,c("PLT_CN", "treeid")])
# 
#  ggplot(data = alphas.df, aes(x = treeid, y = median, color = PLT_CN %in% unique(high.plts$plot)))+geom_point()+
#   geom_errorbar(data = alphas.df, aes(x = treeid, ymin = ci.lo, ymax = ci.hi, color = PLT_CN %in% unique(high.plts$plot)))+ylab("TREE Random Effect")+
#    theme(legend.position = "bottom")
# 
# ggsave(height = 4, width =6, units = "in", "outputs/alphas_colored_by_highAGB.png")
# 
# 
# future.climate.high <- future.clim.subset.26 %>% filter(PLT_CN %in% unique(high.plts$plot) & year <= 2098)
# 
# ggplot(future.climate.high, aes(x = year, y = tmax.corrected, group = PLT_CN))+geom_line()+facet_wrap(~model)
# ggplot(future.climate.high, aes(x = year, y = ppt.corrected, group = PLT_CN))+geom_line()+facet_wrap(~model)
# 
# ggplot(future.clim.subset.26, aes(x = year, y = tmax.corrected, group = PLT_CN, color = PLT_CN %in% unique(high.plts$plot) ))+geom_line()+facet_wrap(~model)
# ggplot(future.clim.subset.26, aes(x = year, y = ppt.corrected, group = PLT_CN, color = PLT_CN %in% unique(high.plts$plot) ))+geom_line()+facet_wrap(~model)
# 
# 
# ggplot(future.clim.subset.26, aes(y = tmax.corrected, fill = PLT_CN %in% unique(high.plts$plot) ))+geom_histogram()+facet_wrap(~model, ncol = 7)+theme(legend.position = "bottom")
# ggsave(height = 5, width = 12, units = "in", "outputs/Future_tmax_high_agb_plots.png")
# ggplot(future.clim.subset.26, aes(y = ppt.corrected, fill = PLT_CN %in% unique(high.plts$plot) ))+geom_histogram()+facet_wrap(~model, ncol = 7)+theme(legend.position = "bottom")
# ggsave(height = 5, width = 12, units = "in", "outputs/Future_ppt_high_agb_plots.png")
# 
# max.AGB.df <- AGB.proj %>% group_by(plot, n.tree, avg_DIA_inc, avg_DIA, BA_sq_ft, avg_RW, MAT, MAP, DESIGNCD, parse) %>% summarise(maxAGB = max(mAGB)/1000)
# ggplot(max.AGB.df, aes(x = n.tree, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = avg_DIA_inc, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = avg_DIA, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = BA_sq_ft, y = maxAGB))+geom_point()+facet_wrap(~parse)
# 
# ggplot(max.AGB.df, aes(x = avg_RW, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = MAT, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = MAP, y = maxAGB))+geom_point()+facet_wrap(~parse)
# ggplot(max.AGB.df, aes(x = as.factor(DESIGNCD), y = maxAGB))+geom_jitter()+facet_wrap(~parse)
# 
# high.plts <- as.character(unique(AGB.proj %>% filter(mAGB/1000 > 350 ) %>%dplyr::select(PLT_CN) %>% distinct())$PLT_CN)
# View(TREE %>% filter(PLT_CN %in% high.plts) %>% group_by(PLT_CN, MEASYR) %>% summarise(n(), 
#                                                                                        avg.TPA = avg(TPA_UNADJ)))
# 
# View(TREE %>% filter(PLT_CN %in% high.plts) %>% group_by(PLT_CN, MEASYR, TPA_UNADJ) %>% summarise(n()))
# 
# 
# View(TREE %>% filter(PLT_CN %in% high.plts) %>% group_by(PLT_CN, MEASYR) %>% summarise(avg))
# View(PLOT %>% filter(CN %in% high.plts) %>% group_by(CN, MEASYEAR) %>% summarise(n()))
# #-----------------------------------------------------------------------------------------
# # set up held out diameters (AZ only)
# #-----------------------------------------------------------------------------------------
# # cov.data.regional<- cov.data.regional  %>% mutate(ST_CT_PLT_SUBP = paste0(STATECD,"_", COUNTYCD, "_", PLOT, "_", SUBP))
# # df.validation <- df.validation %>% mutate(ST_CT_PLT_SUBP = paste0("4_", COUNTYCD, "_", PlotNo, "_", SUBP))
# # df.validation$ST_CT_PLT_SUBP
# # colnames(df.validation)[5] <- "DIA"
# # colnames(df.validation)[3] <- "TreeNo"
# # #View(cov.data.regional %>% filter(ST_CT_PLT_SUBP %in% unique(df.validation$ST_CT_PLT_SUBP)))
# # 
# # # join up by the first diameter and the STATE-COUNTY_PLOT_SUBPLOT code
# # join.held.out.AZ.dia <- left_join(cov.data.regional%>%dplyr::select(ST_CT_PLT_SUBP, DIA, DIA_cm, TREE), df.validation)
# # 
# # join.held.out.AZ.dia %>% filter(!is.na(T2_DIA))
# # 
# # saveRDS(join.held.out.AZ.dia, "data/held_out_diameters.RDS")
