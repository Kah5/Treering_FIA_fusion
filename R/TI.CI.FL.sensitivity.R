# Sensitivity analysis of CI and TI and flamelength to canopy characterists

library(ggplot2)
library(reshape2)
library(tidyverse)
library(firebehavioR)
data(fuelModels, fuelMoisture)
exampSurfFuel = fuelModels['TU1',]

fuelMoisture['D1L1',]

exampFuelMoisture = fuelMoisture['D1L1',]

# naw.az <- terra::rast("nawfd_arizona.tif")
# plot(naw.az)
# rm(naw.az)
# fuelModels
# read in the summary stats from the MTRI fuels product:
# ideally down the road I will draw samples and propagate the uncertainty through

NAM.means <- read.csv("data/NAmWildlandFuelsDatabase_evt_groups_2022-12-30.csv")
pipo.fuels <- NAM.means %>% filter(evt_group_name %in% "Ponderosa Pine Forest| Woodland and Savanna" )

colnames(pipo.fuels)

# run the rothermal equations for 

exampFuelMoisture = fuelMoisture['D1L1',]

# exampCrownFuel = data.frame(
#   CBD = coForest$cbd_kgm3,
#   FMC = 100,
#   CBH = coForest$cbh_m,
#   CFL = coForest$cfl_kgm2
# )

exampEnviro = data.frame(
  slope = 10,
  windspeed = 40,
  direction = 0,
  waf = 0.2
)

basal.areas <- seq(1, 60, 2)
heights <- seq(2, 62, 4) # height in meters
trees.per.hecatare  <- seq(1, 4000, 500)

plt.characteristics <- expand.grid(ba = basal.areas, ht = heights, tph = trees.per.hecatare)
plt.characteristics$plot <- 1:length(plt.characteristics$ba)

# change range of crown fuel:
plt.CrownFuel <- matrix(NA, nrow = length(plt.characteristics$plot), ncol = 3)
for(i in 1:length(plt.characteristics$plot)){
  cat(paste("calculating crown fuels for ", i))
  #cat(paste("year", i))
  plt.CrownFuel[i,] = as.matrix(canFuel(ba = plt.characteristics[i,]$ba, ht = plt.characteristics[i,]$ht, tph = plt.characteristics[i,]$tph, type = "pp")) # pp is ponderosa pine:
}
colnames(plt.CrownFuel) <- c("cfl", "cbd", "cbh")

plt.CrownFuel<- data.frame(plt.CrownFuel)
plt.CrownFuel$FMC <- 95

plt.CrownFuel <- plt.CrownFuel[, c("cfl","FMC",  "cbd", "cbh")]

#plt.CrownFuel

exampFuelMoisture = sapply(exampFuelMoisture, rep, length(plt.characteristics$ba))
exampEnviro = sapply(exampEnviro, rep, length(plt.characteristics$ba))

# change surface fuel model:

exampSurfFuel = sapply(exampSurfFuel, rep, length(plt.characteristics$ba))

TU1.sensitivity = rothermel(exampSurfFuel, exampFuelMoisture, plt.CrownFuel, exampEnviro)

TU1.sensitivity$fireBehavior$`Flame Length [m]`

TU1.output <- data.frame(basal.area = plt.characteristics$ba, 
                         height = plt.characteristics$ht, 
                         trees.per.hecatare = plt.characteristics$tph, 
                         plot = plt.characteristics$plot, 
                         fuelmodel = "TU1", 
                         CFL = plt.CrownFuel$cfl, 
                         CBD = plt.CrownFuel$cbd, 
                         FMC = plt.CrownFuel$FMC, 
                         CBH = plt.CrownFuel$cbh, 
                         flame.length = TU1.sensitivity$fireBehavior$`Flame Length [m]`, 
                         TI = TU1.sensitivity$fireBehavior$`Torching Index [m/min]`, 
                         CI = TU1.sensitivity$fireBehavior$`Crowning Index [km/hr]`)

ggplot(data = TU1.output, aes(basal.area, CFL, color = trees.per.hecatare))+geom_point()#+geom_line()
ggplot(data = TU1.output, aes(basal.area, CBH, color = height))+geom_point()#+geom_line()
ggplot(data = TU1.output, aes(basal.area, CBD, color = trees.per.hecatare))+geom_point()
ggplot(data = TU1.output, aes(trees.per.hecatare, CFL, color = CBD))+geom_point()

ggplot(data = TU1.output, aes(trees.per.hecatare, TI, color = CBD))+geom_point()
ggplot(data = TU1.output, aes(trees.per.hecatare, CI, color = CBD))+geom_point()

# using TL9 surface fuel mode
exampSurfFuel = fuelModels['TL9',]
exampSurfFuel = sapply(exampSurfFuel, rep, length(plt.characteristics$ba))

TL9.sensitivity = rothermel(exampSurfFuel, exampFuelMoisture, plt.CrownFuel, exampEnviro)

TL9.sensitivity$fireBehavior$`Flame Length [m]`

TL9.output <- data.frame(basal.area = plt.characteristics$ba, 
                         height = plt.characteristics$ht, 
                         trees.per.hecatare = plt.characteristics$tph, 
                         plot = plt.characteristics$plot, 
                         fuelmodel = "TL9", 
                         CFL = plt.CrownFuel$cfl, 
                         CBD = plt.CrownFuel$cbd, 
                         FMC = plt.CrownFuel$FMC, 
                         CBH = plt.CrownFuel$cbh, 
                         flame.length = TL9.sensitivity$fireBehavior$`Flame Length [m]`, 
                         TI = TL9.sensitivity$fireBehavior$`Torching Index [m/min]`, 
                         CI = TL9.sensitivity$fireBehavior$`Crowning Index [km/hr]`)

# TL5 high load conifer litter
exampSurfFuel = fuelModels['TL5',]
exampSurfFuel = sapply(exampSurfFuel, rep, length(plt.characteristics$ba))

TL5.sensitivity = rothermel(exampSurfFuel, exampFuelMoisture, plt.CrownFuel, exampEnviro)

TL5.sensitivity$fireBehavior$`Flame Length [m]`

TL5.output <- data.frame(basal.area = plt.characteristics$ba, 
                         height = plt.characteristics$ht, 
                         trees.per.hecatare = plt.characteristics$tph, 
                         plot = plt.characteristics$plot, 
                         fuelmodel = "TL5", 
                         CFL = plt.CrownFuel$cfl, 
                         CBD = plt.CrownFuel$cbd, 
                         FMC = plt.CrownFuel$FMC, 
                         CBH = plt.CrownFuel$cbh, 
                         flame.length = TL5.sensitivity$fireBehavior$`Flame Length [m]`, 
                         TI = TL5.sensitivity$fireBehavior$`Torching Index [m/min]`, 
                         CI = TL5.sensitivity$fireBehavior$`Crowning Index [km/hr]`)



Fuel.models.sensitivity <- rbind(TL5.output, TL9.output, TU1.output)

head(Fuel.models.sensitivity)

Fuel.models.sensitivity$TI_km_hr <- (Fuel.models.sensitivity$TI*0.001)*60
# plot of CFL vs CI

ggplot(Fuel.models.sensitivity, aes(CFL, CI, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(CBD, CI, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(CBH, CI, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)


ggplot(Fuel.models.sensitivity, aes(trees.per.hecatare, CI, color = basal.area))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(basal.area, CI, color = trees.per.hecatare))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(height, CI, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)

ggplot(Fuel.models.sensitivity, aes(trees.per.hecatare, TI_km_hr, color = basal.area))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(basal.area, TI_km_hr, color = trees.per.hecatare))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(height, TI_km_hr, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)


ggplot(Fuel.models.sensitivity, aes(trees.per.hecatare, flame.length, color = basal.area))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(basal.area, flame.length, color = trees.per.hecatare))+geom_point()+facet_wrap(~fuelmodel)
ggplot(Fuel.models.sensitivity, aes(height, flame.length, color = fuelmodel))+geom_point()+facet_wrap(~fuelmodel)

# what is the sensitivity of CBD, CBH, and CFL to BA, 

cbd.tph <- ggplot(Fuel.models.sensitivity, aes(trees.per.hecatare, CBD, color = basal.area, group = basal.area))+geom_point()+geom_line()+facet_wrap(~fuelmodel)+xlab("Trees per hectare")
cfl.tph <- ggplot(Fuel.models.sensitivity, aes(trees.per.hecatare, CFL, color = basal.area, group = basal.area))+geom_point()+geom_line()+facet_wrap(~fuelmodel)+xlab("Trees per hectare")
cbh.ht <- ggplot(Fuel.models.sensitivity, aes(height, CBH, color = basal.area, group = basal.area))+geom_point()+geom_line()+facet_wrap(~fuelmodel)+xlab("Height (m)")

png(height = 8, width = 5, units = "in", res = 150, "outputs/canopy_characteristic_sensitivity.png")
cowplot::plot_grid(cbd.tph, cfl.tph, cbh.ht, nrow = 3, align = "hv")
dev.off()


Fuel.models.sensitivity %>% filter(fuelmodel %in% "TL5")

# holding all other values at the median, calculate the sensitivity of TI, CI, flamelength, CBH, CBD, and CFL to each input
# find the mid values for each 
mean(basal.areas) #29
mean(heights) #30
mean(trees.per.hecatare) # 2001

tph.sensitivity.df <- Fuel.models.sensitivity %>% filter(basal.area == 29 & height == 30 ) 
                            #group_by(fuelmodel) %>% summarise(CFL.sens = stats::splinefun(trees.per.hecatare, CFL, method = "monoH.FC"))
ba.sensitivity.df <- Fuel.models.sensitivity %>% filter(trees.per.hecatare == 2001 & height == 30 ) #%>%

ht.sensitivity.df <- Fuel.models.sensitivity %>% filter(trees.per.hecatare == 2001 & basal.area == 29 ) #%>%


tph.sensitivity.m <- melt(tph.sensitivity.df, id.vars = c("basal.area", "height", "trees.per.hecatare", "plot", "fuelmodel"))
head(tph.sensitivity.df )
ba.sensitivity.m <- melt(ba.sensitivity.df, id.vars = c("basal.area", "height", "trees.per.hecatare", "plot", "fuelmodel"))
ht.sensitivity.m <- melt(ht.sensitivity.df, id.vars = c("basal.area", "height", "trees.per.hecatare", "plot", "fuelmodel"))


png(height = 6, width = 4, units = "in", res = 100, "outputs/tph.sensitivity.all.png")
ggplot(tph.sensitivity.m, aes(trees.per.hecatare, value))+geom_point()+geom_line()+facet_grid(variable~fuelmodel, scales = "free")+theme_bw()+theme(panel.grid = element_blank())
dev.off()

png(height = 6, width = 4, units = "in", res = 100, "outputs/BA.sensitivity.all.png")
ggplot(ba.sensitivity.m, aes(basal.area, value))+geom_point()+geom_line()+facet_grid(variable~fuelmodel, scales = "free")+theme_bw()+theme(panel.grid = element_blank())
dev.off()

png(height = 6, width = 4, units = "in", res = 100, "outputs/ht.sensitivity.all.png")
ggplot(ht.sensitivity.m, aes(height, value))+geom_point()+geom_line()+facet_grid(variable~fuelmodel, scales = "free")+theme_bw()+theme(panel.grid = element_blank())
dev.off()


ggplot(tph.sensitivity.df, aes(trees.per.hecatare, CBD))+geom_point()
ggplot(tph.sensitivity.df, aes(trees.per.hecatare, ))

stats::splinefun(ba.sensitivity.df$trees.per.hecatare, ba.sensitivity.df$CFL, method = "monoH.FC")


sa.splinefun <- function(quantiles.input, quantiles.output) {
  return(stats::splinefun(quantiles.input, quantiles.output, method = "monoH.FC"))
} # sa.splinefun

sa.splinefun(Fuel.models.sensitivity$basal.area, Fuel.models.sensitivity$CFL)
sa.splines <- sapply(traits,
                     function(trait) sa.splinefun(sa.samples[[trait]], sa.output[[trait]]))

get.sensitivity <- function(trait.samples, sa.splinefun) {
  sensitivity <- sa.splinefun(stats::median(trait.samples), 1)
  return(sensitivity)
} # get.sensitivity


sensitivity.analysis <- function(trait.samples, sa.samples, sa.output, outdir) {
  traits <- names(trait.samples)
  sa.splines <- sapply(traits,
                       function(trait) sa.splinefun(sa.samples[[trait]], sa.output[[trait]]))
  
  spline.estimates <- lapply(traits, 
                             function(trait) spline.truncate(sa.splines[[trait]](trait.samples[[trait]])))
  names(spline.estimates) <- traits
  sensitivities <- sapply(traits, 
                          function(trait) get.sensitivity(trait.samples[[trait]], sa.splines[[trait]]))
  elasticities <- sapply(traits, 
                         function(trait) get.elasticity(sensitivities[[trait]], 
                                                        trait.samples[[trait]], 
                                                        spline.estimates[[trait]]))
  variances <- sapply(traits, function(trait) stats::var(spline.estimates[[trait]]))
  partial.variances <- variances / sum(variances)
  
  coef.vars <- sapply(trait.samples, get.coef.var)
  outlist <- list(sensitivity.output = list(sa.samples = sa.samples, 
                                            sa.splines = sa.splines), 
                  variance.decomposition.output = list(coef.vars = coef.vars,
                                                       elasticities = elasticities, 
                                                       sensitivities = sensitivities,
                                                       variances = variances,
                                                       partial.variances = partial.variances))
  return(outlist)
} # sensitivity.analysis

