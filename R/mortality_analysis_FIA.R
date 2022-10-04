fiadb <-readRDS(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/InWeUS_FIAdb.rds"))

PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE
#TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE$INVYR)
unique(fiadb$PLOT$STATECD)


# read in the non-AZ points:
#full.clim.data <- read.csv("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/pipo_all_tmean_ppt_v3.csv")
region.rwl <- read.csv(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/trees-rwl-1-31-17.csv")) # note that this data is has all RWLS in columsn and a year column
region.ll <- read.csv(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/locs-env-1-31-17.csv"))
#unique(region.ll $CN %in% TREE$CN)
# also new mexico data:
nm.meta <- read.delim(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/new-mexico-meta.txt"), sep = ",", as.is = TRUE)
nm.rwl <- read.delim(url("https://data.cyverse.org/dav-anon/iplant/home/kah5/analyses/INV_FIA_DATA/data/new-mexico-ring-width.txt"), sep = ",", as.is = TRUE)

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

# get the previous survey for each tree: 
TREE_remeas <- subset(TREE, !is.na(PREVDIA))
TREE_remeas <- subset(TREE_remeas, STATUSCD == 1 | STATUSCD == 2) 

# Look up previous AGB
### look up previous AGB, PLT_CN, and CONDID
TREE_remeas$PREV_DRYBIO_AG <- TREE$DRYBIO_AG[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
TREE_remeas$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]
TREE_remeas$PREV_CONDID <- TREE$CONDID[match(TREE_remeas$PREV_TRE_CN, TREE$CN)]

# Subset records with previous AGB found
TREE_remeas <- subset(TREE_remeas, !is.na(PREV_DRYBIO_AG))

# for growth analysis
TREE_remeas$DRYBIO_AG_DIFF <- TREE_remeas$DRYBIO_AG - TREE_remeas$PREV_DRYBIO_AG
TREE_remeas$DIA_DIFF <- TREE_remeas$DIA - TREE_remeas$PREVDIA

# basal area increment
TREE_remeas$BAt1 <- ((TREE_remeas$PREVDIA/2)^2)*3.14159
TREE_remeas$BAt2 <- ((TREE_remeas$DIA/2)^2)*3.14159
TREE_remeas$BA_DIFF <- TREE_remeas$BAt2 - TREE_remeas$BAt1

# Read in plot data and get coordinates and previous measurement year
#plots <- read.csv(paste(data.path,"PLOT_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)

TREE_remeas$LAT <- PLOT$LAT[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$LON <- PLOT$LON[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$ELEV <- PLOT$ELEV[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PLT_CN, PLOT$CN)]
TREE_remeas$PREV_MEASYEAR <- PLOT$MEASYEAR[match(TREE_remeas$PREV_PLT_CN, PLOT$CN)]

# Calculate census interval
TREE_remeas$CENSUS_INTERVAL <- TREE_remeas$MEASYEAR - TREE_remeas$PREV_MEASYEAR

colnames(TREE_remeas)

# zero if no change, 
# 2 if the tree died in the census interval
TREE_remeas$STATUSCD_CHANGE <- ifelse(TREE_remeas$PREV_STATUS_CD == TREE_remeas$STATUSCD, 0, 
                                      ifelse(TREE_remeas$PREV_STATUS_CD == 1 & TREE_remeas$STATUSCD == 2, 2, NA))

# match up PLOT and COND data
PLOT$DSTRBYR1 <- COND$DSTRBYR1[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBYR2 <- COND$DSTRBYR2[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBYR3 <- COND$DSTRBYR3[match(PLOT$CN, COND$PLT_CN)]


PLOT$DSTRBCD1 <- COND$DSTRBCD1[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBCD2 <- COND$DSTRBCD2[match(PLOT$CN, COND$PLT_CN)]
PLOT$DSTRBCD3 <- COND$DSTRBCD3[match(PLOT$CN, COND$PLT_CN)]

# Match up the tree and plot data
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
TREE$DESIGNCD <- PLOT$DESIGNCD[match(TREE$PLT_CN, PLOT$CN)]


TREE$DSTRBCD1 <- PLOT$DSTRBCD1[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBCD2 <- PLOT$DSTRBCD2[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBCD3 <- PLOT$DSTRBCD3[match(TREE$PLT_CN, PLOT$CN)]

TREE$DSTRBYR1 <- PLOT$DSTRBYR1[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBYR2 <- PLOT$DSTRBYR2[match(TREE$PLT_CN, PLOT$CN)]
TREE$DSTRBYR3 <- PLOT$DSTRBYR3[match(TREE$PLT_CN, PLOT$CN)]


unique(TREE$MORTCD)
unique(TREE$STATECD)

TREE_remeas %>% dplyr::filter(SPCD %in% "122" & STATUSCD %in% c(1, 2)) %>% group_by(STATUSCD_CHANGE) %>% summarise(median.dbh = median(DIA, na.rm = TRUE), 
                                                                              median.ht = median(HT, na.rm = TRUE))

ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(DIA))+facet_wrap(~STATUSCD_CHANGE)

ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(HT))+facet_wrap(~STATUSCD_CHANGE)

# goal: make plots of the distribution of tree diameters that died with groups of diameter, heights, subplot SDIs, and SI
# compare these to biomass estimates
# get a static estimate of SDI, which includes the dead trees:

static_SDI_pltcn <- TREE %>% ungroup() %>%  filter(DIA > 1) %>%
  group_by(PLT_CN, STATECD, PLOT, COUNTYCD,  MEASYR) %>%
  summarise(ntrees_static = n(),
            TPA_static =sum(TPA_UNADJ), 
            Dq_static = sqrt(sum(DIA^2, na.rm = TRUE)/ntrees_static), 
            SDIdq_static = ((Dq_static/10)^1.6)*TPA_static, #calculate SDI (Summation Method) on the subplot:
            SDIs_static = sum(TPA_UNADJ*((DIA/10)^1.6), na.rm = TRUE))#, ## calculate SDI (Quadratic mean diameter) on the subplot:


hist(static_SDI_pltcn$SDIs_static) 

TREE$SDIs_static <- static_SDI_pltcn$SDIs_static[match(TREE$PLT_CN, static_SDI_pltcn$PLT_CN)]
TREE_remeas$SDIs_static <- static_SDI_pltcn$SDIs_static[match(TREE_remeas$PLT_CN, static_SDI_pltcn$PLT_CN)]

ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(HT))+facet_wrap(~STATUSCD_CHANGE)

ggplot()+geom_point(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" ), aes(x = SDIs_static, y = DIA, color = as.character(STATUSCD_CHANGE)))#+facet_wrap(~STATUSCD)


TREE_remeas <- TREE_remeas %>% mutate(SDIbin=cut(SDIs_static, breaks=c(0,50,100, 150,200,250,300,350,400, 450,Inf), labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", 
                                                                                                               "300-350", "350-400", "400-450", ">450")))
TREE_remeas <- TREE_remeas %>% mutate(DIAbin=cut(DIA, breaks=c(0,5,10, 15,20,25,30,35,40, 45,Inf), labels=c("0-5","5-10","10-15", "15-20", "20-25", "25-30", 
                                                                                                               "30-35", "35-40", "40-45", ">45")))

ggplot()+geom_density(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" & !is.na(SDIbin)), aes(x = DIA, fill = as.character(STATUSCD_CHANGE)), alpha = 0.5)+
  facet_wrap(~SDIbin)

png(height = 10, width = 10, units = "in", res = 150, "/home/rstudio/data/output/barplot_totals_mort_by_dia_sdi.png")
ggplot(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" & !is.na(SDIbin)), aes(x = STATUSCD_CHANGE, fill = as.character(STATUSCD_CHANGE)))+geom_bar( alpha = 0.5)+
  facet_grid(DIAbin~SDIbin)
dev.off()

png(height = 10, width = 10, units = "in", res = 150, "/home/rstudio/data/output/hist_statuscd_by_dia_sdi.png")
ggplot()+geom_histogram(data = TREE_remeas %>% dplyr::filter(SPCD %in% "122" & !is.na(SDIbin)), aes(x = DIA, fill = as.character(STATUSCD_CHANGE)), alpha = 0.5, position = "stack")+
  facet_wrap(~SDIbin)
dev.off()
# calculate the proportion of dead trees in each sdi and dbh class?
# this is binned across all the data so not exactly the mortality rate
prop.dead <- TREE_remeas %>% group_by(SDIbin, DIAbin, STATUSCD_CHANGE) %>% summarise(n()) %>% 
  ungroup() %>% group_by (SDIbin, DIAbin) %>% spread(`n()`, key = STATUSCD_CHANGE) %>% mutate(prop.dead = `2`/`0`) %>% mutate(prop.dead = ifelse(is.na(prop.dead), 0, prop.dead))


hist(prop.dead$prop.dead)


png(height = 4, width = 6, units = "in", res = 150, "/home/rstudio/data/output/scatter_prop_mort_by_dia_sdi_lines_remeas.png")
ggplot(prop.dead %>% filter(!is.na(DIAbin)), aes(x = DIAbin, y = prop.dead, color = SDIbin, group = SDIbin))+
  geom_point()+geom_line()+theme_bw()+ylab("proportion of trees dead")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "/home/rstudio/data/output/boxplot_prop_mort_by_dia_remeas.png")
ggplot(prop.dead %>% filter(!is.na(DIAbin)), aes( y = prop.dead, x = DIAbin))+
  geom_boxplot()+theme_bw()+ylab("proportion of trees dead")+xlab("Diameter Class (in)")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "/home/rstudio/data/output/boxplot_prop_mort_by_sdi_remeas.png")
ggplot(prop.dead %>% filter(!is.na(DIAbin)), aes( y = prop.dead, x = SDIbin))+
  geom_boxplot()+theme_bw()+ylab("proportion of trees dead")+xlab("SDI Class")+theme(panel.grid = element_blank())
dev.off()

png(height = 4, width = 6, units = "in", res = 150, "/home/rstudio/data/output/tile_prop_mort_by_dia_sdi_remeas.png")
ggplot(prop.dead %>% filter(!is.na(DIAbin) & !is.na(SDIbin)), aes( x = DIAbin, y = SDIbin, fill = prop.dead))+
  geom_raster()+scale_fill_gradientn(colors = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))

dev.off()


# calculate plot-level mortality rates for size classes, then summarise across SDI:
# this is strange...because some diabins only have dead trees while some only have live trees..
plot.mort <- TREE_remeas %>% group_by(PLT_CN, DIAbin, STATUSCD_CHANGE) %>% summarise(n()) %>% 
  ungroup() %>% group_by (PLT_CN,DIAbin,) %>% spread(`n()`, key = STATUSCD_CHANGE) %>% mutate(prop.dead = `2`/`0`)

plot.mort$SDIs_static <- static_SDI_pltcn$SDIs_static[match(plot.mort$PLT_CN, static_SDI_pltcn$PLT_CN)]

plot.mort <- plot.mort %>% mutate(SDIbin=cut(SDIs_static, breaks=c(0,50,100, 150,200,250,300,350,400, 450,Inf), labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", 
                                                                                                                          "300-350", "350-400", "400-450", ">450")))

head(plot.mort)
plot.mort.by.SDI <- plot.mort %>% group_by(SDIbin) %>% summarise(med.prop = median(prop.dead, na.rm = TRUE))

ggplot(plot.mort, aes(x = prop.dead, fill = SDIbin))+geom_density(alpha=0.5)+xlim(0,1)+facet_grid(SDIbin~DIAbin)

