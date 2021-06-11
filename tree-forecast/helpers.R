# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties

# forecast <- INC.tot
# treeid <- 1

my_cols <- c("#1b9e77",
             "#d95f02",
             "black",
             "#7570b3", 
             "grey")

total_dbh_unc <- function( var ){
  forecast <- INC.tot
  forecast$Unc <- factor(forecast$Unc, levels = rev(c("Initial Conditions", "Parameter", "Random effects", "Driver", "Process")))
  
  forecast.subset <- forecast[forecast$treeid == var, ]
  
  
  
  ggplot(forecast.subset, aes(x = year, ymin = Low, ymax = High, fill = Unc))+geom_ribbon()+
    theme_bw(base_size = 12)+theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = "bottom")+
    facet_wrap(~rcp, ncol = 4)+ylab("Projected diameter growth increment (mm)")+xlab("Year")+scale_fill_manual(values = my_cols)+
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
  
  
  
}




# percent_map <- function(var, color, legend.title, min = 0, max = 100) {
#   
#   # generate vector of fill colors for map
#   shades <- colorRampPalette(c("white", color))(100)
#   
#   # constrain gradient to percents that occur between min and max
#   var <- pmax(var, min)
#   var <- pmin(var, max)
#   percents <- as.integer(cut(var, 100, 
#                              include.lowest = TRUE, ordered = TRUE))
#   fills <- shades[percents]
#   
#   # plot choropleth map
#   map("county", fill = TRUE, col = fills, 
#       resolution = 0, lty = 0, projection = "polyconic", 
#       myborder = 0, mar = c(0,0,0,0))
#   
#   # overlay state borders
#   map("state", col = "white", fill = FALSE, add = TRUE,
#       lty = 1, lwd = 1, projection = "polyconic", 
#       myborder = 0, mar = c(0,0,0,0))
#   
#   # add a legend
#   inc <- (max - min) / 4
#   legend.text <- c(paste0(min, " % or less"),
#                    paste0(min + inc, " %"),
#                    paste0(min + 2 * inc, " %"),
#                    paste0(min + 3 * inc, " %"),
#                    paste0(max, " % or more"))
#   
#   legend("bottomleft", 
#          legend = legend.text, 
#          fill = shades[c(1, 25, 50, 75, 100)], 
#          title = legend.title)
# }
