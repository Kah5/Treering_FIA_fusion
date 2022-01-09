# code to backcalculate diameters to estimate SDI back through time.
# we need a list of trees in each plot, diameter, measure year
# then for the trees with increment cores, we backcalculate DBH based on increment + DBH measured
# for trees without increment cores, use Basal area ratio?
#library(DBI)
library(rFIA)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# note that you will need to change path names throughout this script

#if(!exists("/Users/kah/Treering_FIA_fusion/InWeUS_FIA/NM_COND.csv")){
 # fiadb <- getFIA(states = c("AZ", "NM","UT", "CO", "ID", "WY", "MT"), dir = "InWeUS_FIA", common = FALSE, tables = c("PLOT", "TREE", "COND", "SUBPLOT"), nCores = 1)
#}else{
  fiadb <- readFIA(dir = "/Users/kah/Treering_FIA_fusion/InWeUS_FIA/")
#}
# save as RDS:
  saveRDS(fiadb, "/Users/kah/Treering_FIA_fusion/InWeUS_FIAdb.rds")
  
PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE
#TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE$INVYR)
unique(fiadb$PLOT$STATECD)


# read in the non-AZ points:
full.clim.data <- read.csv("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/pipo_all_tmean_ppt_v3.csv")
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
saveRDS(FIA.not.orphaned.plots.nona, "data/FIA.not.orphaned.plots.nona.RDS")
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
# these are the old datasets, need to replace with 518
#jags.data <- readRDS("/Users/kah/Documents/docker_pecan/pecan/jags.data.515.trees.rds")
#PIPO.az.cov <- read.csv("/Users/kah/Documents/docker_pecan/pecan/Full.AZ.PIPO.515.trees.csv")
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
regional.nonaz.rwl<- rbind(region.rwl.m[,c("CORE_CN", "Year", "RW")], nm.rwl[,c("CORE_CN", "Year", "RW")])
#region.rwl.m$CORE_CN <- as.character(region.rwl.m$CORE_CN)


# all the PLT_CN's turn to NAs when we merge these
full.inc.nonaz <- left_join(regional.nonaz.rwl, FIA.outside.AZ , by = "CORE_CN")
full.inc.nonaz.nona <- full.inc.nonaz [!is.na(full.inc.nonaz $RW),]
full.inc.nonaz$DIA_cm <- full.inc.nonaz$DIA*2.54 # convert to centimeters


# adapting from Courtney Giebink's code: https://github.com/clgiebink/UT_FVS/blob/master/scripts/AnnualizeDBH.R
# select the covariates
nonaz_2cov <- full.inc.nonaz %>%
  dplyr::select(CORE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT, TREE, SPCD,SUBP,MEASYR,TPA_UNADJ,
                CN,
                DIA_cm, DIA)
nonaz_2cov$RW <- nonaz_2cov$RW/10 # convert mm to cm--I am assuming all the measurements are in mm

# select the covariates for the AZ data
naz_2cov <- AZ.cov.growth.data %>%
  dplyr::select(CORE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT, TREE, SPCD,SUBP,MEASYR,TPA_UNADJ,
                CN,
                DIA_cm, DIA)
naz_2cov$RW <- naz_2cov$RW/10 # convert mm to cm--I am assuming all the measurements are in mm

all.region.data <- rbind(nonaz_2cov, naz_2cov) # combine all regional data togther 
all.pipo <- all.region.data  %>% filter(SPCD == 122) # just get PIPO

#ggplot(all.pipo, aes(x = STATECD,y= RW, group = STATECD))+geom_boxplot()

head(all.region.data)
# select pipos that have tree CNs, and dont have NAs
PIPO.filtered <- all.region.data  %>% filter(SPCD == 122 & !is.na(RW) & !is.na(CN)) %>% rename(TRE_CN = CN, MEASYEAR = MEASYR)
plots.in.region<- all.region.data[, c("PLT_CN", "CN", "STATECD","PLOT", "SUBP", "TREE")]
ok.plots <- unique(plots.in.region[, c("PLT_CN", "STATECD", "PLOT","SUBP")])
regional.tree <- TREE %>% filter(PLT_CN %in% ok.plots$PLT_CN) # the tree table to draw from

unique(regional.tree$STATECD ) # check that all states are here
unique(plots.in.region$STATECD)
#regional.tree.SDI.static <- 

#dataframe of coefficients for bark ratio calculation
#from Utah variant guide (from Courtney's code)
bratio_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                        #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                        b1 = c(0.9502,0.867,0.8967,0.890,0.890,0.9002,0.9502,0.9002,0.9625,0.9002,0.93789),
                        b2 = c(-0.2528, 0, -0.4448,0,0,-0.3089,-0.2528,-0.3089,-0.1141,-0.3089,-0.24096),
                        exp = c(1,0,1,0,0,1,1,1,1,1,1)) #can add more species later 
# test.tree<- PIPO.filtered$TRE_CN[1] 
# 
# PIPO.test <- PIPO.filtered %>% filter(TRE_CN ==  test.tree & !is.na(RW))
# 
# DIA_cm<- PIPO.test$DIA_cm
# MEASYEAR <- PIPO.test$MEASYEAR
# Year<- PIPO.test$Year
# RW<- PIPO.test$RW
# SPCD<- PIPO.test$SPCD
#

#annualized DBH
#DBH0 = DBH - k * DG , where k = 1/BRATIO and DG = 2 * RW  
#DG = periodic increment in inside bark diameter 

#function to annualize, or back calculate dbh using diameter increment data (2*RW)

calculateDIA <- function(TRE_CN,DIA,MEASYEAR,Year,RW,SPCD){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA,MEASYEAR,Year,RW,SPCD,DIA_C = NA)
  #N is the row where measure year and ring width year are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR[1]) #next step is to allow N to be ring width year -1
  if(length(N) == 0){
    N <- which(tree_df$Year + 1 == tree_df$MEASYEAR[1])
  }
  Species <- tree_df$SPCD[1]
  if(length(N) > 0 & Species %in% bratio_df$species){
    Curr_row <- N-1 #each time through subtract 1 and move down one row (or back one year)
    tree_df$DIA_C[N] <- tree_df$DIA[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0 & !is.na(tree_df$DIA_C[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA[N] for the first round
      RW1 <- tree_df$RW[Curr_row+1] 
      #convert ring width from mm to inches
      RW1 = RW1* 0.0393701
      b1 <- bratio_df$b1[bratio_df$species == Species]
      b2 <- bratio_df$b2[bratio_df$species == Species]
      exp <- bratio_df$exp[bratio_df$species == Species]
      tree_df$DIA_C[Curr_row] <- DIA_1 - ((2*RW1)/(b1+b2/(DIA_1^exp)))
      #stop back calculating for small trees (<1 inch)
      if(tree_df$DIA_C[Curr_row] < 0){
        tree_df$DIA_C[Curr_row] <- NA
      }
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_C)
}

incr_imputed <- PIPO.filtered  %>% 
  group_by(TRE_CN) %>% #for each tree calculate dbh
  arrange(Year) %>%
  mutate(DIA_C = calculateDIA(TRE_CN = TRE_CN,DIA,MEASYEAR,Year,RW,SPCD)) %>% 
  mutate(DIA_C = ifelse(DIA_C < 1, NA, DIA_C))

unique(incr_imputed$TRE_CN)[1]

# test<- PIPO.filtered  %>% 
#  filter(TRE_CN %in% unique(incr_imputed$TRE_CN)[1]) %>% #for each tree calculate dbh
#  arrange(Year) #%>%
#  mutate(DIA_C = calculateDIA(TRE_CN = test$TRE_CN,DIA_cm= test$DIA_cm,MEASYEAR= test$MEASYEAR,Year = test$Year,RW = test$RW,SPCD = test$SPCD)) %>% 
#  mutate(DIA_C = ifelse(DIA_C < 1, NA, DIA_C))

unique(incr_imputed$PLT_CN)
unique(PIPO.filtered $CORE_CN)
#check
#stop when DIA is less than 1
min(incr_imputed$DIA_C,na.rm = T) #minimum diameter = 1.001022 cm
max(incr_imputed$DIA_C,na.rm = T) #minimum diameter = 1.001022 cm

#did DIA_C = DIA_t when last RW year was 1 year less than MEASYEAR
check_data <- incr_imputed[which(incr_imputed$Year + 1 == incr_imputed$MEASYEAR),]
check_data <- check_data[check_data$SPCD == 202,]

#filter for trees with back calculated DBH
length(unique(incr_imputed$TRE_CN)) #only 609 trees with back calculated dbh
#incr_imputed <- incr_imputed %>%
# filter(!is.na(DIA_C)) #%>%
#filter for >3" - check variant for large tree growth threshold - species specific
#  filter(DIA_C >= 3)
length(unique(incr_imputed$TRE_CN)) #698

length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 202])) #0
length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 122])) #698
length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 93]))  #0

#save dataframe
#save dataframe
save(incr_imputed,file = "./data/Back_calculated_DBH_all_region_incr_imputed.Rdata")
load("./data/Back_calculated_DBH_all_region_incr_imputed.Rdata")
#Example plot--suggests that most cored trees are young? I think this is reasonable, but I want to check some of the floating cores
ggplot(incr_imputed, aes(x = Year, y = DIA_C, group = TRE_CN))+geom_line()

#--------------------------------------------------------------------------------------
# Now do the back calculation for the other trees on the plot
#--------------------------------------------------------------------------------------

# again using Courtney Giebinks code to back estimate diameter for the trees without increment cores:
#https://github.com/clgiebink/UT_FVS/blob/master/scripts/MissingDBH.R


#From User's Guide to the Stand Prognosis Model
#Wykoff, Crookston, Stage
#pg 48
#Backdating imput diameters
##with trees with increment data calculate BAR (basal area ratio)
###BAR = DBH_0^2/DBH^2 -- average over species
##with missing trees
###DBH_0 = sqrt(BAR * DBH^2)

#library(tidyverse)

#First calculate BAR for every year for each tree
#create column on glmm.data.imputed

incr_imputed <- incr_imputed %>%
  group_by(TRE_CN) %>%
  mutate(BAR = (lag(DIA_C)^2)/DIA_C^2) %>% ungroup() %>% group_by(PLT_CN) %>%
  mutate(BAR_av = mean(BAR, na.rm = TRUE))

#species
unique(incr_imputed$SPCD) #Values of BAR can be used for other species
#[1] 106 202 122  93  15  65 108  19  96 133 321

# explore BAR by different groups:
#BAR by size?
bar_df <- incr_imputed %>%
  ungroup() %>%
  dplyr:: select(SPCD,DIA_C,BAR) %>%
  group_by(SPCD,DBHRange = cut(DIA_C, breaks = c(0, 12, 40), 
                               labels = c("0-12", "13-40"))) %>%
  summarise(BAR_Avg = mean(BAR, na.rm = TRUE))

#or just by species?
bar_df <- incr_imputed %>%
  ungroup() %>%
  dplyr:: select(SPCD,BAR) %>%
  group_by(SPCD) %>%
  summarise(BAR_Avg = mean(BAR, na.rm = TRUE))
#use in projection script

#create dataframe of trees without increment cores in plots with trees that have increment cores
# get tree to tree data for AZ:
incr_imputed <- incr_imputed %>%
  group_by(TRE_CN) %>%
  mutate(BAR = (lag(DIA_C)^2)/DIA_C^2)


#create dataframe of trees without increment cores in plots with trees that have increment cores
plot_rw <- unique(incr_imputed$PLT_CN) #475
tree_rw <- unique(incr_imputed$TRE_CN) #568
#(tree$PLT_CN %in% FIA.outside.AZ.CNs$PLT_CN) & !(tree$CN %in% tree_rw)

# get subset of the tree data for all states
tree <- TREE[,c("CN","STATECD", "PLOT", "SUBP","INVYR", "MEASYR","PLT_CN","SUBP","SPCD","STATUSCD","MORTYR","DIA","TPA_UNADJ","DIST","AGENTCD", "DESIGNCD")]

#get the miss_data, or the trees missing cores!
miss_data <- tree[(tree$PLT_CN %in% regional.tree$PLT_CN) &  !(tree$CN %in% tree_rw),c("CN","STATECD", "PLOT", "SUBP","INVYR", "MEASYR","PLT_CN","SUBP","SPCD","STATUSCD","MORTYR","DIA","TPA_UNADJ","DIST","AGENTCD", "DESIGNCD")]

#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including trees with increment data
colnames(miss_data)[colnames(miss_data)=="CN"] <- "TRE_CN"
colnames(miss_data)[colnames(miss_data)=="DIA"] <- "DIA_t"
colnames(miss_data)[colnames(miss_data)=="SUBP.1"] <- "SUBP_t"
miss_data$MEASYR <- PLOT$MEASYEAR[match(miss_data$PLT_CN, PLOT$CN)]
#miss_data$DESIGNCD <- plot$DESIGNCD[match(miss_data$PLT_CN, plot$PLT_CN)]
#miss_data$CONDID <- cond$CONDID[match(miss_data$PLT_CN, cond$PLT_CN)]
#miss_data$SLOPE <- cond$SLOPE[match(miss_data$PLT_CN, cond$PLT_CN)]

#miss_data$Year <- NA #important for mutate(Year) to work
miss_data$BAR_av <- NA
miss_data$DIA_C <- NA

#check
length(plot_rw) #542
length(unique(miss_data$PLT_CN)) #1588
unique(miss_data$STATUSCD) # either 1 or 2
unique(miss_data$STATECD)
unique(miss_data$AGENTCD)

# get the mortalities data (status code = 2)
miss_mort <- miss_data %>%
  filter(STATUSCD == 2) %>%
  dplyr::select(PLT_CN,SUBP,TRE_CN,SPCD,MEASYR,STATUSCD,MORTYR)
#no mortality year; no way to reconstruct death
write.csv(miss_mort,file = "./data/miss_mortality_all.csv")

#filter for alive and recently dead--is this making the sdi strange?
miss_data <- miss_data %>%
  filter(AGENTCD >= 0)
#how many mortality trees on plots?
plt_cal <- unique(miss_data$PLT_CN)
miss_mort <- miss_data %>%
  filter(STATUSCD ==2) %>%
  filter(PLT_CN %in% plt_cal) %>%
  group_by(PLT_CN) %>%
  summarise(n_tre = n())
hist(miss_mort$n_tre, breaks = 50, xlab = "number of mort trees per plot", main = "calibration")

#filter for live trees
# miss_data <- miss_data %>%
#   filter(STATUSCD == 1)

#empty (year&DIA_C) dataframe?
miss_data <- miss_data %>% 
  group_by(TRE_CN) %>%
  mutate(start = ifelse(STATUSCD == 1, 0,
                        base::sample(0:9,1)))

# this takes along time =---But we need it....we also need a way to make it go back in time by a standard, and to go forward in time
miss_data <- miss_data %>%
  slice(rep(1:n(), each = 40)) %>% #repeat each row 40 times
  group_by(TRE_CN) %>%
  mutate(Year = c((MEASYR[1]-39-start):(MEASYR[1]-start))) %>% #40 yrs is arbitrary; 
  ungroup() %>%
  filter(Year >= 1958)

n.in.plts <- miss_data %>% group_by(STATECD, PLOT, INVYR) %>% summarise(n())

# instead make a df with year expanded (KAH attempt to have the calculation of diamters go back and forward in time):
# tre.yrs <- expand.grid(TRE_CN = unique(miss_data$TRE_CN), 
#            Year = 1966:2010)
# miss_data <- left_join( miss_data, tre.yrs, by = "TRE_CN")

# throws an error because miss_mort_imputed is not an object
miss_mort_check <- miss_mort_imputed %>%
  dplyr::select(TRE_CN,STATUSCD,start) %>%
  distinct() %>%
  filter(STATUSCD == 2)
hist(miss_mort_check$start, breaks = 50, xlab = 'Difference in mortality year and inventory', main = "Estimated Calibration Mortality")



#match BAR_av from incr_imputed to miss_data using plot, species and year information

#KAH converted the for loop (which worked, but took  a long time) to a function + lapply

# create progress bar because there are like a million data points
pb <- txtProgressBar(min = 0, max = length(miss_data$TRE_CN), style = 3)

get_bar_imputed <- function(i){
  species <- miss_data$SPCD[i]
  if(species %in% c(106,202,122,93)){ #focal species
    BAR <- incr_imputed$BAR_av[incr_imputed$PLT_CN == miss_data$PLT_CN[i] &
                                 incr_imputed$SPCD == miss_data$SPCD[i]]
    #first average within a plot for a specific species (over years)
    if(length(BAR) == 0){
      BAR <- incr_imputed$BAR[incr_imputed$SPCD == miss_data$SPCD[i]]
      #if no species on the plot, then just average species across plots
    }
  }
  if(!(species %in% c(106,122,202,93))){ #all other, non-focal species 
    #includes 15, 19, 65, 66, 96, 108, 102, 113, 322, 475, 746, 749, & 814
    BAR <- incr_imputed$BAR[!(species %in% c(106,122,202,93))]
  }
  BAR_av <- mean(BAR, na.rm = TRUE)
  #cat(i)
  
  # update GUI console
  setTxtProgressBar(pb, i)    
  BAR_av
}
#a<- lapply(1:10, get_bar_imputed)
# should take labout 15 minutes 
bar.vals <- lapply(1:length(miss_data$TRE_CN), get_bar_imputed)
bar.df <- do.call(rbind, bar.vals)
head(bar.df)
tail(bar.df)
#unique(bar.df)
miss_data$BAR <- bar.df[,1]

# old for loop here:
# for(i in 1:nrow(miss_data)){ #miss_data includes all trees without increment data
#   species <- miss_data$SPCD[i]
#   if(species %in% c(106,202,122,93)){ #focal species
#     BAR <- incr_imputed$BAR_av[incr_imputed$PLT_CN == miss_data$PLT_CN[i] &
#                               incr_imputed$SPCD == miss_data$SPCD[i]]
#     #first average within a plot for a specific species (over years)
#     if(length(BAR) == 0){
#       BAR <- incr_imputed$BAR[incr_imputed$SPCD == miss_data$SPCD[i]]
#       #if no species on the plot, then just average species across plots
#     }
#   }
#   if(!(species %in% c(106,122,202,93))){ #all other, non-focal species 
#     #includes 15, 19, 65, 66, 96, 108, 102, 113, 322, 475, 746, 749, & 814
#     BAR <- incr_imputed$BAR[!(species %in% c(106,122,202,93))]
#   }
#   miss_data$BAR_av[i] <- mean(BAR, na.rm = TRUE)
#   cat(i)
# }

#check how many trees there is no BAR for
length(unique(miss_data$TRE_CN[is.na(miss_data$BAR)]))
#0
#BAR = 1: 2970 for just plot, species, year
length(unique(miss_data$TRE_CN))
#8025

#unique(miss_data$SPCD[miss_data$BAR_av == 1])
#[1]  66 321 202 746  65 475  15 814 113  19 108  93  96 122 106 102

miss_data$BAR <- ifelse(is.na(miss_data$BAR), mean(miss_data$BAR, na.rm = TRUE), miss_data$BAR)
summary(miss_data$BAR)

# with the bark ratio, I think we need to have diameter in IN

calculateDIA <- function(TRE_CN,DIA,MEASYEAR,Year,RW,SPCD){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA,MEASYEAR,Year,RW,SPCD,DIA_C = NA)
  #N is the row where measure year and ring width year are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR[1]) #next step is to allow N to be ring width year -1
  if(length(N) == 0){
    N <- which(tree_df$Year + 1 == tree_df$MEASYEAR[1])
  }
  Species <- tree_df$SPCD[1]
  if(length(N) > 0 & Species %in% bratio_df$species){
    Curr_row <- N-1 #each time through subtract 1 and move down one row (or back one year)
    tree_df$DIA_C[N] <- tree_df$DIA[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0 & !is.na(tree_df$DIA_C[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA[N] for the first round
      RW1 <- tree_df$RW[Curr_row+1] 
      #convert ring width from mm to inches
      RW1 = RW1* 0.0393701
      b1 <- bratio_df$b1[bratio_df$species == Species]
      b2 <- bratio_df$b2[bratio_df$species == Species]
      exp <- bratio_df$exp[bratio_df$species == Species]
      tree_df$DIA_C[Curr_row] <- DIA_1 - ((2*RW1)/(b1+b2/(DIA_1^exp)))
      #stop back calculating for small trees (<1 inch)
      if(tree_df$DIA_C[Curr_row] < 1){
        tree_df$DIA_C[Curr_row] <- NA
      }
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_C)
}

# create progress bar because there are like a million data points
pb <- txtProgressBar(min = 0, max = length(unique(miss_data$TRE_CN)), style = 3)

#calculate DIA from BAR
DIA_BAR_tcn <- function(tcn){
  #create data frame with empty column for annualized dbh
  tree_df <- miss_data %>% filter(TRE_CN == tcn)
  tree_df$DIA_C <- NA
  #tree_df data.frame(TRE_CN,DIA_t,MEASYR,start,Year,BAR_av,DIA_C = NA)
  #N is the row where measure year and year of growth are the same
  N <- which(tree_df$Year == (tree_df$MEASYR[1] - tree_df$start[1]))
  tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of growth and measure year are equal
  Curr_row <- N-1 #each time through subtract 1 and move up one row to the previous year
  while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
    #!is.na(tree_df$DIA_C[Curr_row + 1])
    DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
    BAR_av <- tree_df$BAR[Curr_row+1] 
    tree_df$DIA_C[Curr_row] <- sqrt(BAR_av * (DIA_1^2))
    #allow dbh to be <1 inch
    #continue loop for next row until curr_row>0
    Curr_row = Curr_row - 1 
  }
  Curr_row <- N+ 1#each time through add 1 and move up one row to the previous year
  while (Curr_row <= length(tree_df$TRE_CN)) { #loop will stop when it gets to the end of data for that tree
    #!is.na(tree_df$DIA_C[Curr_row + 1])
    DIA_1 <- tree_df$DIA_C[Curr_row-1] #or DIA_t[N] for the first round
    BAR_av <- tree_df$BAR[Curr_row-1] 
    tree_df$DIA_C[Curr_row] <- DIA_1 + (DIA_1- sqrt(BAR_av*DIA_1^2) )#sqrt(BAR_av* (DIA_1^2))
    #allow dbh to be <1 inch
    #continue loop for next row until curr_row>0
    Curr_row = Curr_row + 1 
  }
  # update GUI console
  cat(tcn)   
  return(tree_df$DIA_C)
}

#pb <- txtProgressBar(min = 0, max = length(unique(miss_data$TRE_CN)), style = 3)
#tcn <- unique(miss_data$TRE_CN)[1]
#DIA_BAR_tcn(tcn)


system.time(tree.df.dia.c <- lapply(unique(miss_data$TRE_CN), FUN = DIA_BAR_tcn))
saveRDS(tree.df.dia.c , "data/tree.df.dia.c_v3.RDS")

tree.df.dia.c <- readRDS("data/tree.df.dia.c_v3.RDS")
make.diam.df <- function(x){
  cat(x)
  df<- data.frame(tree.df.dia.c[[x]])
  df$TRE_CN <- unique(miss_data$TRE_CN)[x]
  tree.data <- miss_data %>% filter(TRE_CN %in% unique(miss_data$TRE_CN)[x]) 
  df$Year <- tree.data$Year #1966:2010
  df
}

list.of.dias <- lapply(1:length(unique(miss_data$TRE_CN)), make.diam.df)

#system.time(list.of.dias <- lapply(1:100, make.diam.df))
tree.df.all <- do.call(rbind, list.of.dias)
colnames(tree.df.all)<- c("DIA_c", "TRE_CN", "Year")


# merge with the larger dataseet:
miss_data.new <- left_join(miss_data, tree.df.all, by = c("TRE_CN", "Year"))

saveRDS(miss_data.new, "data/noncored_trees_DIA_c_v3.rds")
miss_data.new<- readRDS("data/noncored_trees_DIA_c_v3.rds")

nrecords <- miss_data.new %>% ungroup() %>% group_by(PLOT, Year) %>% summarise(n())
summary(nrecords) # check to see that there is more than 1 tree in each year
# combine the miss_data.new with the cored dataset for nonAZ species:
colnames(incr_imputed)
colnames(miss_data.new)

miss_data.new$TRE_CN <- as.character(miss_data.new$TRE_CN) # just need to do this to join the two together to calculate plot variables
incr_imputed$STATUSCD <- 1 # all trees with rw are alive
inc.density.data <- as.data.frame(incr_imputed[,c("TRE_CN" , "Year",  "STATECD" ,"PLOT","PLT_CN","SPCD", "SUBP","MEASYEAR","DIA_C", "TPA_UNADJ","STATUSCD" )])
noninc.density.data <- as.data.frame(miss_data.new[,c("TRE_CN" , "Year",  "STATECD" ,"PLOT","PLT_CN","SPCD", "SUBP","MEASYR","DIA_c" , "TPA_UNADJ","STATUSCD" )])
colnames(inc.density.data)
colnames(noninc.density.data) <- colnames(inc.density.data)

density.data <- rbind(inc.density.data , noninc.density.data )
density.data <- density.data %>% filter(Year > 1950 )
nrecords.all <- density.data  %>% ungroup() %>% group_by(TRE_CN, Year) %>% summarise(n())
nrecords.plt <- density.data  %>% ungroup() %>% group_by(STATECD, PLOT, Year) %>% summarise(n())
summary(nrecords.plt)
summary(nrecords.all)
length(density.data$PLOT)/length(TPA.df$STATECD)


# get TPA_UNADJ
#TPA.df <- unique(miss_data.new %>% ungroup() %>% select(STATECD,PLOT, SUBP, TPA_UNADJ, MEASYR))
#nuniqueTPA <- TPA.df %>% group_by(MEASYR, STATECD,PLOT, SUBP) %>% summarise(n())


#density.data.TPA <- left_join(density.data, TPA.df)

#test.dens <- unique(density.data) 
density.data.TPA <- density.data
#density.data.nona <- density.data.TPA[!is.na(density.data.TPA$DIA_C),]
#<- duplicated(density.data.nona)
density.data.TPA$DIA_Cin <- density.data.TPA$DIA_C#*2.54
density.data.TPA$count <- 1


# at the plot level calculate the time varying SDI
Time_varying_SDI <- density.data.TPA %>% ungroup() %>% group_by(STATECD, PLOT, PLT_CN,Year) %>% filter(DIA_Cin > 1 & STATUSCD == 1) %>%
  summarise(ntrees = n(),
         TPA = sum(TPA_UNADJ), 
         Dq = sqrt(sum(DIA_Cin^2, na.rm =TRUE)/ntrees), 
         SDIdq = ((Dq/10)^1.6)*TPA, ## calculate SDI (Quadratic mean diameter) on the subplot:
         SDIs = sum(TPA_UNADJ*((DIA_Cin/10)^1.6)),#calculate SDI (Summation Method) on the subplot: 
         SDIrat = SDIs/SDIdq) 

#Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/length(DIA)), 
#SDIs_static = ((Dq_static/10)^1.6)*TPA_UNADJ, #calculate SDI (Summation Method) on the subplot:
#SDIdq_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE), 


saveRDS(Time_varying_SDI, "data/Time_varying_SDI_TPA_UNADJ_PLT_CN_v2.RDS")


hist(Time_varying_SDI$SDIdq)# SDIdq seems to skew low, but in the range
hist(Time_varying_SDI$SDIs) # SDIs seems too high?
fia.nums <- short.FIA %>% group_by(STATECD, PLOT) %>% summarise(n())


ggplot(Time_varying_SDI, aes(x = Year, y = SDIs, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)

png(height = 4, width = 7, units = "in", res = 200, "SDIdq_time_Varying.png")
ggplot(Time_varying_SDI, aes(x = Year, y = SDIdq, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)
dev.off()

png(height = 4, width = 7, units = "in", res = 200, "SDIs_time_Varying.png")
ggplot(Time_varying_SDI, aes(x = Year, y = SDIs, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)
dev.off()


png(height = 4, width = 7, units = "in", res = 200, "SDIdq_time_Varying_histogram.png")
ggplot(Time_varying_SDI, aes(SDIdq))+geom_histogram()+geom_vline(aes(xintercept = 450), color = "red", linetype = "dashed")+facet_wrap(~STATECD)
dev.off()

png(height = 4, width = 7, units = "in", res = 200, "SDIs_time_Varying_histogram.png")
ggplot(Time_varying_SDI, aes(SDIs))+geom_histogram()+geom_vline(aes(xintercept = 450), color = "red", linetype = "dashed")+facet_wrap(~STATECD)
dev.off()


Time_varying_SDI.subset <- Time_varying_SDI %>% filter(Year < 2002)

# now lets link it back up to the data:
#test.static <- left_join(Time_varying_SDI.subset, static_SDI, by = c("PLT_CN", "STATECD","PLOT", "SUBP"))

test.static.pltcn <- left_join(Time_varying_SDI.subset, static_SDI_pltcn.unique, by = c("PLT_CN", "STATECD","PLOT"))

ggplot(test.static, aes(SDIs, SDIs_static))+geom_point()
ggplot(test.static.pltcn , aes(SDIdq, SDIdq_static))+geom_point()

#png(height = 4, width = 4, res = 150, unit = "in", "SDItv_SDIstatic.png")
ggplot(test.static.pltcn , aes(SDIdq, SDIdq_static))+geom_point()
#dev.off()

test.static.invyr <- test.static.pltcn %>% group_by(PLT_CN) %>% filter(Year == MEASYR)


png(height = 4, width = 4, res = 150, unit = "in", "SDItv_SDIstatic_Dq.png")
ggplot(test.static.invyr , aes(SDIdq, SDIdq_static))+geom_point()+geom_abline(aes(intercept = 0, slope = 1))+
  ylab("Static SDI calculated for MEASYR")+xlab("Time Varying SDI calulation for MEASYR")
dev.off()

png(height = 4, width = 4, res = 150, unit = "in", "SDItv_SDIstatic_summation.png")
ggplot(test.static.invyr , aes(SDIs, SDIs_static))+geom_point()+geom_abline(aes(intercept = 0, slope = 1))+
  ylab("Static SDI calculated for MEASYR")+xlab("Time Varying SDI calulation for MEASYR")
dev.off()

varying.static.pltcn <- left_join(Time_varying_SDI, static_SDI_pltcn.unique, by = c("PLT_CN", "STATECD","PLOT"))
ggplot(test.static.invyr , aes(SDIdq, SDIdq_static))+geom_point()+geom_abline(aes(intercept = 0, slope = 1))

saveRDS( varying.static.pltcn, "data/Time_varying_SDI_static_SDI_PLT_CN.RDS")
saveRDS(static_SDI_pltcn, "data/static_SDI_PLT_CN.RDS")

# make the same caluculations on the subplot scale:
Time_varying_SDI_subp <- density.data.TPA %>% ungroup() %>% group_by(STATECD, PLOT,SUBP, PLT_CN,Year) %>% filter(DIA_Cin > 1 & STATUSCD == 1) %>%
  summarise(ntrees = n(),
            TPA = sum(TPA_UNADJ), 
            Dq = sqrt(sum(DIA_Cin^2, na.rm =TRUE)/ntrees), 
            SDIdq = ((Dq/10)^1.6)*TPA, ## calculate SDI (Quadratic mean diameter) on the subplot:
            SDIs = sum(TPA_UNADJ*((DIA_Cin/10)^1.6)), #calculate SDI (Summation Method) on the subplot: 
            SDIrat = SDIs/SDIdq) 


saveRDS(Time_varying_SDI_subp, "/home/rstudio/datasaved/Time_varying_SDI_TPA_UNADJ_PLT_CN_SUBP_v4.RDS")

hist(Time_varying_SDI_subp$SDIdq) # SDI dq now falls in the expected range, but skews low
hist(Time_varying_SDI_subp$SDIs) # SDI s is now very low?? TPA unadj should scale to the acre, but summing means that SDIs will be lower for subplot vs plot



ggplot(Time_varying_SDI_subp, aes(x = Year, y = SDIdq, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)

png(height = 4, width = 7, units = "in", res = 200, "SDIdq_time_Varying_SUBP.png")
ggplot(Time_varying_SDI_subp, aes(x = Year, y = SDIdq, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)
dev.off()

png(height = 4, width = 7, units = "in", res = 200, "SDIs_time_Varying_SUBP.png")
ggplot(Time_varying_SDI_subp, aes(x = Year, y = SDIs, group = PLT_CN))+geom_point()+facet_wrap(~STATECD)
dev.off()


png(height = 4, width = 7, units = "in", res = 200, "SDIdq_time_Varying_histogram_SUBP.png")
ggplot(Time_varying_SDI_subp, aes(SDIdq))+geom_histogram()+geom_vline(aes(xintercept = 450), color = "red", linetype = "dashed")+facet_wrap(~STATECD)
dev.off()

png(height = 4, width = 7, units = "in", res = 200, "SDIs_time_Varying_histogram_SUBP.png")
ggplot(Time_varying_SDI_subp, aes(SDIs))+geom_histogram()+geom_vline(aes(xintercept = 450), color = "red", linetype = "dashed")+facet_wrap(~STATECD)
dev.off()






#######################################################################################
# Below is deprecated code
#######################################################################################
# 
# 
# 
# 
# TREE.in.region.ll <- TREE %>% filter(PLT_CN %in% region.ll$CN)
# 
# 
# # now do the same thing, but with TREE2TREE dataset in AZ:
# Tree2Tree <- read.csv("data/Tree2Tree.csv", stringsAsFactors = F)
# #head(Tree2Tree$T1_TRE_CN )
# ### limit analysis to those trees with second DBH measurement in =< year 2015
# ### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
# 
# #Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]
# 
# ### eliminate those cases without SI (SDI seems to always be there)
# Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
# Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]
# 
# # need to get TPA for the cored trees in AZ...
# 
# # system.time( %>% arrange(Year) %>%
# #   mutate(DIA_C = DIA_BAR(TRE_CN,DIA_t,MEASYR,Year,BAR_av,start)))
# # 
# # miss_data_imputed <- miss_data %>%
# #   group_by(TRE_CN) %>%
# #   arrange(Year) %>%
# #   mutate(DIA_C = DIA_BAR(TRE_CN,DIA_t,MEASYR,Year,BAR_av,start))
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #######################################################################################
# #
# 
# 
# # how many cored trees of each species?
# FIA.outside.AZ %>% group_by(SPCD) %>% summarise(n())
# # # A tibble: 4 x 2
# # SPCD `n()`
# # <int> <int>
# #   1   106   415
# # 2   113    56
# # 3   122   981
# # 4   202  1526
# 
# # over 981 PIPO trees cored 
# 
# 
# 
# # of the coredtrees with two meausrements,  how many for each species?
# FIA.not.orphaned.plots.nona %>% group_by(SPCD) %>% summarise(n())
# # SPCD `n()`
# #    106   289
# #    113     8
# #    122   371
# #    202   872
# 
# # 371 pipo trees with at least 2 DBH measurements (outside AZ)
# 
# 
# 
# # there are 16131 remeasured PIPO trees in the FIA database
# TREE_REMEAS %>% filter(SPCD == 122) %>% summarise(n())
# 
# TREE_REMEAS.PIPO <- TREE_REMEAS %>% filter(SPCD == 122)
# 
# unique(TREE_REMEAS.PIPO$PLOT_LON)
# 
# head(TREE_REMEAS.PIPO)
# 
# ggplot()+
#   geom_point(data = TREE_REMEAS %>% filter(SPCD == 122), aes(x = PLOT_LON, y = PLOT_LAT), color = "black")+
#   geom_point(data = FIA.outside.AZ %>% filter(SPCD == 122), aes(x= LON, y = LAT, color = as.character(INVYR)))
# 
# summary(FIA.outside.AZ$HT)
# length(FIA.outside.AZ$HT)
# 
# FIA.outside.AZ.CNs <- FIA.outside.AZ[!is.na(FIA.outside.AZ$CN),]
# 
# 
# FIA.outside.AZ.plots<- PLOT %>% filter(CN %in% FIA.outside.AZ.CNs$PLT_CN)
# FIA.outside.AZ.TREES <- TREE %>% filter(PLT_CN %in% FIA.outside.AZ.CNs$PLT_CN)
# 
# 
# # test.plot <- FIA.outside.AZ.TREES %>% filter(PLT_CN %in% unique(FIA.outside.AZ.plots$CN)[1])
# # ggplot(test.plot, aes(x = DIA, y = HT, color = as.character(SPCD)))+geom_point()
# 
# 
# # note: not sure if I should be calculating SDI on the PLOT or the SUBP scale. I think we want SDI at the subplot scacle (reflects closer to what the tree feels)
# 
# FIA.outside.AZ.TREES.w.SDI <- FIA.outside.AZ.TREES %>% group_by(PLT_CN, STATECD, COUNTYCD,PLOT, SUBP) %>% filter(DIA > 1) %>%
#   mutate(TPA = sum(TPA_UNADJ), 
#          Dq = sqrt(sum(DIA^2)/length(DIA)), 
#          SDIs = ((Dq/10)^1.6)*TPA, #calculate SDI (Summation Method) on the subplot:
#          SDIdq = sum(TPA_UNADJ*((DIA/10)^1.6)), ## calculate SDI (Quadratic mean diameter) on the subplot:
#          SDIrat = SDIs/SDIdq) # ratio of SDIsum to SDIdq; should be close to 1 for even aged stands
# 
# 
# # Calculate importance values
# FIA.outside.AZ.TREES.w.SDI$BASAL_AREA <- pi*((FIA.outside.AZ.TREES.w.SDI$DIA/2)^2)
# 
# 
# # Importance value = Relative density (%) + Relative Basal Area (%)
# 
# # note: not sure if I should be calculating Importance values on the PLOT or the SUBP scale
# plot.IV <- FIA.outside.AZ.TREES.w.SDI %>% group_by(PLT_CN, PLOT, INVYR, SPCD) %>%
#   summarise(density = n(), sumBA = sum(BASAL_AREA, na.rm = TRUE)) %>%
#   group_by(PLT_CN,PLOT, INVYR) %>% mutate(total_density = sum(density), 
#                                           total_BA = sum(sumBA)) %>% ungroup() %>%
#   mutate(rel_density = (density/total_density)*100, 
#          rel_BA= (sumBA/total_BA)*100) %>%
#   mutate(ImportanceValue = rel_density + rel_BA)
# 
# 
# FIA.outside.AZ.TREES.w.SDI.IV <-left_join(FIA.outside.AZ.TREES.w.SDI, plot.IV, by = c("PLT_CN", "PLOT", "INVYR", "SPCD"))
# 
# ggplot(plot.IV, aes(x=as.character(SPCD), y = ImportanceValue))+geom_point()
# 
# FIA.outside.AZ.TREES.w.SDI.IV
# ggplot(FIA.outside.AZ.TREES.w.SDI.IV %>% filter(SPCD == 122), aes(x=SDIs, y = ImportanceValue))+geom_point()
# 
# ggplot(FIA.outside.AZ.TREES.w.SDI.IV %>% filter(SPCD == 122), aes(x=SDIdq, y = ImportanceValue))+geom_point()
# 
# sdis.sdidq.subplt<- ggplot(FIA.outside.AZ.TREES.w.SDI.IV %>% filter(SPCD == 122), aes(x=SDIdq, y = SDIs, color = ImportanceValue))+
#   geom_point()+geom_abline(aes(intercept = 0, slope = 1))+theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylab("SDIdq calculated at SUBP")+ylab("SDIs calculated at SUBP")
# 
# # plot SDI relative to SDI max
# 
# SDIs.hist.supb <- ggplot(FIA.outside.AZ.TREES.w.SDI.IV %>% filter(SPCD == 122), aes(SDIs))+geom_histogram()+
#   geom_vline(aes(xintercept = 450), linetype = "dashed")+xlab("SDI calculated on SUBPLOT level \n (summation method)")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())
# 
# SDId.hist.supb <- ggplot(FIA.outside.AZ.TREES.w.SDI.IV %>% filter(SPCD == 122), aes(SDIdq))+geom_histogram()+
#   geom_vline(aes(xintercept = 450), linetype = "dashed")+xlab("SDI calculated on SUBPLOT level \n (Quadratic mean diameter method)")+theme_bw(base_size = 12)+theme(panel.grid = element_blank())
# 
# 
# # do the same thing but at the PLOT level:
# 
# 
# FIA.outside.AZ.TREES.w.SDI.PLT <- FIA.outside.AZ.TREES %>% group_by(PLT_CN, STATECD, COUNTYCD,PLOT, MEASYR) %>% filter(DIA > 1) %>%
#   mutate(TPA = sum(TPA_UNADJ), 
#          Dq = sqrt(sum(DIA^2)/length(DIA)), 
#          SDIs = ((Dq/10)^1.6)*TPA, #calculate SDI (Summation Method) on the subplot:
#          SDIdq = sum(TPA_UNADJ*((DIA/10)^1.6)), ## calculate SDI (Quadratic mean diameter) on the subplot:
#          SDIrat = SDIs/SDIdq) # ratio of SDIsum to SDIdq; should be close to 1 for even aged stands
# 
# 
# # gets estimates for all plots in the TREE database
# FIA.outside.AZ.all.PLTS.w.SDI.PLT <- TREE %>% group_by(PLT_CN, STATECD, COUNTYCD,PLOT, INVYR) %>% filter(DIA > 1) %>%
#   summarise(TPA = sum(TPA_UNADJ, na.rm = TRUE), 
#             Dq = sqrt(sum(DIA^2, na.rm = TRUE)/length(DIA)), 
#             SDIs = ((Dq/10)^1.6)*TPA, #calculate SDI (Summation Method) on the subplot:
#             SDIdq = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE), ## calculate SDI (Quadratic mean diameter) on the subplot:
#             SDIrat = SDIs/SDIdq) # ratio of SDIsum to SDIdq; should be close to 1 for even aged stands
# 
# 
# CN_PREV <- unique(PLOT[,c("CN", "PREV_PLT_CN")]) # get the CNS and prev plot cns
# colnames(CN_PREV) <- c("PLT_CN", "PREV_PLT_CN")
# FIA.plot.SDIS <- left_join(FIA.outside.AZ.all.PLTS.w.SDI.PLT, CN_PREV,  by =  "PLT_CN")
# 
# 
# unique(FIA.plot.SDIS$INVYR)
# 
# # for each plot, if there is a previous plot CN, get the previous measure year, and pervious SDIs
# 
# FIA.plot.SDIS$INVYR_PREV <- 0
# FIA.plot.SDIS$SDIs_PREV <- 0
# FIA.plot.SDIs$SDIdq_PREV <- 0
# 
# # FIA.plot.SDIS$INVYR_NEXT <- 0
# # FIA.plot.SDIS$SDIs_NEXT <- 0
# # FIA.plot.SDIs$SDIdq_NEXT <- 0
# 
# for (i in 1:length(FIA.plot.SDIS$PLT_CN)){
#   if(!is.na(FIA.plot.SDIS[i,]$PREV_PLT_CN)){
#     FIA.plot.SDIS[i,]$SDIs_PREV <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PREV_PLT_CN, FIA.plot.SDIS$PLT_CN),]$SDIs
#     FIA.plot.SDIS[i,]$INVYR_PREV <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PREV_PLT_CN, FIA.plot.SDIS$PLT_CN),]$INVYR
#     FIA.plot.SDIS[i,]$SDIdq_PREV <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PREV_PLT_CN, FIA.plot.SDIS$PLT_CN),]$SDIdq
#     
#     # # get the next inventory:
#     # FIA.plot.SDIS[i,]$SDIs_NEXT <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PLT_CN, FIA.plot.SDIS$PREV_PLT_CN),]$SDIs
#     # FIA.plot.SDIS[i,]$INVYR_NEXT <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PLT_CN, FIA.plot.SDIS$PREV_PLT_CN),]$INVYR
#     # FIA.plot.SDIS[i,]$SDIdq_NEXT <- FIA.plot.SDIS[match(FIA.plot.SDIS[i,]$PLT_CN, FIA.plot.SDIS$PREV_PLT_CN),]$SDIdq
#     
#     
#   }
#   cat(i)
# }
# 
# FIA.plots.sdi.short <- FIA.plot.SDIS %>% select(PLT_CN, PREV_PLT_CN, COUNTYCD, STATECD, PLOT, INVYR, INVYR_PREV, SDIs, SDIs_PREV,  SDIdq, SDIdq_PREV)
# 
# CNS_indataset<- unique(FIA.outside.AZ[!is.na(FIA.outside.AZ$PLT_CN),]$PLT_CN )
# 
# FIA.plots.prev.sdi.indataset <- FIA.plots.sdi.prevs %>% filter(PREV_PLT_CN %in% CNS_indataset)
# FIA.plots.sdi.indataset <- FIA.plots.sdi.currents %>% filter(PLT_CN %in% CNS_indataset  )
# 
# FIA.plots.sdi.prevs <- FIA.plot.SDIS %>% select(PLT_CN, PREV_PLT_CN, COUNTYCD, STATECD, PLOT, INVYR_PREV, SDIs_PREV,  SDIdq_PREV)
# FIA.plots.sdi.currents <- FIA.plot.SDIS %>% select(PLT_CN,PREV_PLT_CN, COUNTYCD, STATECD, PLOT, INVYR, SDIs,  SDIdq)
# 
# colnames(FIA.plots.sdi.prevs) <- colnames(FIA.plots.sdi.currents) 
# FIA.plots.sdis <- rbind(FIA.plots.sdi.prevs, FIA.plots.sdi.currents)
# #FIA.plots.sdis$id <- 1:length(FIA.plots.sdis$PLT_CN)
# 
# 
# FIA.SDIs.spread <- FIA.plots.sdis %>% select( -SDIdq) %>% filter(!SDIs == 0) %>% group_by(PLT_CN, COUNTYCD, STATECD, PLOT) %>% spread(INVYR, SDIs)
# 
