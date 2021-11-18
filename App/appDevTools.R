# Dev -----
{
  {
    library(cowplot)
    library(GGally)
    ## min packages
    library(shiny)
    library(shinyWidgets)
    library(waiter)
    library(dplyr)
    library(DT)
    library(reactable)
    library(sf)
    library(leaflet)
    library(plotly)
    library(rintrojs)
    library(shinyjs)
    library(highcharter)
    library(htmlwidgets)
    ### Load UI Components
    source("appUI.R", local = T)
    load("cleaned_le_income_cities_UIelements.rdata")
  }
  
  {
    load("cleaned_le_income_cities_appBundle.rdata")
  }
}

input = list()
input$fig1_ineq = "Total"
input$fig1_type = "Abs. Disparity"


## Total
outcomeTmp = "Total"
typeTmp = "Rel. Disparity"
typeTmp = "Abs. Disparity"
sizeTmp ="<120,000"
ineqTmp ="Total"
typeTmp = "Abs. Disparity"

## Income
outcomeTmp = "Income"
typeTmp = "Top/Bottom Difference"

## Figure 3
metricTmp = "Mean"
