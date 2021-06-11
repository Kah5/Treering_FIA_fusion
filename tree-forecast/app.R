library(shiny)
library(ggplot2)
library(tidyverse)
source("helpers.R")
INC <- readRDS("data/INC.TOT.ALLSDI_SI.norand.X.resampled.RDS")
#DBH <- readRDS("data/INC.TOT.ALLSDI_SI.norand.X.resampled.RDS")

inc.unc.df <- data.frame(uncertainty = c("I", "IP", "IPA", "IPD", "IPP"), 
                         Unc = c("Initial Conditions", "Parameter","Random effects", "Driver", "Process"))

INC.tot <- left_join(INC, inc.unc.df, by = "uncertainty")


unique(INC.tot$treeid)

# User interface ----
ui <- fluidPage(
  titlePanel("Ponderosa pine tree-level forecasts in Arizona"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a tree to make a diameter increment forecast"),
      
     
      sliderInput("var", h3("Tree ID"),
                  min = 1, max = 515, value = 10)),
    mainPanel(plotOutput("forecastplt"))
   )
    
   
  
)

# Server logic ----
server <- function(input, output) {
  
  output$forecastplt <- renderPlot({

    
    total_dbh_unc(input$var)
  })
  
}

# Run app ----
shinyApp(ui, server)