# Setup -----
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
  ### Load UI Components
  source("appUI.R", local = T)
  load("cleaned_le_income_cities_UIelements.rdata")
}





server <- function(input, output) {
  # Loading Screen ----
  load("cleaned_le_income_cities_appBundle.rdata")
  source("appPlotterTable1.R", local = T)
  source("appPlotterFigure1.R", local = T)
  source("appPlotterFigure2.R", local = T)
  source("appPlotterFigure3.R", local = T)
  w <- Waiter$new(id = c("plot_fig1",
                    "plot_fig2_ui",
                    "plot_fig3"),
                  html = figure_loading_screen,
                  color = 'rgb(201, 201, 201,0.5)')
  Sys.sleep(1)
  waiter_hide()
  
  # Table 1 ----
  output$table1 = renderReactable({ 
    table1_plotter(df_table1)
  })
  
  # Figure 1 ----
  output$fig1_ui_input = renderUI({
    choices_tmp = df_fig1_choices_type %>% filter(outcome == input$fig1_ineq) %>% pull(type)
    pickerInput(inputId = "fig1_type", label = "Type" , choices = choices_tmp)
  })
  output$plot_fig1 = renderPlotly({
    w$show()
    req(input$fig1_type)
    outcomeTmp = input$fig1_ineq
    typeTmp = input$fig1_type
    figure1_plotter(outcomeTmp,typeTmp)
  })
  output$plot_fig1_ui = renderUI({
    plotlyOutput("plot_fig1")
  })
  
  # Figure 2 ----
  output$fig2_ui_input = renderUI({
    choices_tmp = df_fig1_choices_type %>% filter(outcome == input$fig2_ineq) %>% pull(type)
    pickerInput( inputId = "fig2_type",label = "Type" , choices = choices_tmp)
  })
  
  output$header_fig2 = renderUI({
    req(input$fig2_type)
    ineq_tmp = ifelse(input$fig2_ineq=="total","Total","Income")
    type_tmp = input$fig2_type
    h3(paste0(ineq_tmp,": ",type_tmp), align = 'center')
  })
  
  output$plot_fig2 = renderLeaflet({
    req(input$fig2_type)
    ineqTmp = input$fig2_ineq
    typeTmp = input$fig2_type
    figure2_plotter(ineqTmp,typeTmp)
  })
  
  output$plot_fig2_ui = renderUI({
    w$show()
    ineqTmp = input$fig2_ineq
    typeTmp = input$fig2_type
    
    div(
      fluidRow(
        uiOutput("header_fig2")
      ),
      fluidRow(
        leafletOutput("plot_fig2")
      ),
      div(class = "Footnote",
          HTML("<u>Footnote: </u> Rank indicates the widest (1) to narrowest (499) disparities. "))
      
    )
    
  })
  
  # Figure 3 ----
  output$plot_fig3 = renderPlotly({
    w$show()
    sizeTmp = input$fig3_MSAsize
    figure3_plotter(sizeTmp)
  })
}


shinyApp(ui = ui, server = server)
