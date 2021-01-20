library(shiny)
library(tidyverse)
library(cowplot)
library(GGally)
library(sf)
library(shinyWidgets)
library(DT)



## Home ----
ui_home = (
  column(12, align = 'center',
         tagList(
           h1("INTRO TO PAPER")
         ))
)

## Figure 1 (Plot) ----
ui_figure1 = sidebarLayout(
  sidebarPanel(width = 3,
               pickerInput(
                 inputId = "fig1_ineq",
                 label = "Inequalities" ,
                 choices = c("Total Inequalities" ="total",
                             "Income Inequalities"="income")),
               uiOutput("fig1_ui_input")
  ),
  mainPanel(plotOutput("plot_fig1"))
)



## Table 1 ----
ui_table1 =  sidebarLayout(
  sidebarPanel(width = 3
               
  ),
  mainPanel(DTOutput("table1"))
)
