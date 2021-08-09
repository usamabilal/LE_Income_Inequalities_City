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
  ### Load UI Components
  source("appUI.R", local = T)
}





server <- function(input, output) {
  # Loading Screen ----
  load("cleaned_le_income_cities_appBundle.rdata")
  source("appHelperFunctions.R", local = T)
  waiter_hide()
  
  # Table 1 ----
  output$table1 = renderReactable({ 
    table1_plotter(df_table1)
  })
  
  # Figure 1 ----
  output$fig1_ui_input = renderUI({
    choices_tmp = df_fig1 %>% 
      filter(outcome == input$fig1_ineq) %>% 
      count(type) %>% 
      pull(type) %>% 
      as.character()
    pickerInput(
      inputId = "fig1_type",
      label = "Type" ,
      choices = choices_tmp)
  })
  output$plot_fig1 = renderPlot({
    req(input$fig1_type)
    df_tmp = df_fig1 %>% 
      filter(outcome == input$fig1_ineq) %>% 
      filter(type==input$fig1_type)
    title_tmp = ifelse(input$fig1_ineq =="total",
                       "Total Inequalities in Life Expectancy by MSA",
                       "Income Inequalities in Life Expectancy by MSA")
    yaxis_tmp = input$fig1_type
    
    
    ggplot(df_tmp,aes(x=Region_Name, y=value))+
      geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
      geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
                  width=0.1, height=0, alpha=1,
                  color="black", pch=21) +
      # facet_wrap(~type, scales="free_y")+
      guides(color=F, fill=F, size=F)+
      labs(x="",
           y=yaxis_tmp,
           title=title_tmp)+
      #scale_y_continuous(sec.axis=dup_axis(name = ylab2), limits=ylim)+
      theme_bw() +
      theme(legend.position = "bottom",
            legend.key.width = unit(50, "points"),
            panel.grid.major.x = element_blank(),
            axis.text.x=element_text(size=14, color="black"),
            axis.text.y=element_text(size=12, color="black"),
            axis.title.y=element_text(face="bold", size=20),
            strip.text =element_text(face="bold", size=20),
            strip.background = element_blank(),
            plot.title=element_text(face="bold", size=25))
  })
  
  
  # Figure 2 ----
  output$fig2_ui_input = renderUI({
    choices_tmp = df_fig1 %>%
      filter(outcome == input$fig2_ineq) %>%
      count(type) %>%
      pull(type) %>%
      as.character()
    pickerInput(
      inputId = "fig2_type",
      label = "Type" ,
      choices = choices_tmp)
  })
  
  output$header_fig2 = renderUI({
    req(input$fig2_type)
    ineq_tmp = ifelse(input$fig2_ineq=="total","Total","Income")
    type_tmp = input$fig2_type
    h3(paste0(ineq_tmp,": ",type_tmp), align = 'center')
  })
  
  output$plot_fig2 = renderLeaflet({
    # input = list()
    # input$fig2_ineq ="total"
    # input$fig2_type = "Abs. Difference"
    req(input$fig2_type)
    sf_tmp = sf_cbsa %>% 
      left_join(df_fig2 %>%
                  filter(ineq == ifelse(input$fig2_ineq=="total","Total","Income")) %>%
                  filter(type == input$fig2_type))
    
    pal <- colorBin(
      reverse = T,
      palette = "Reds",
      bins = 4,
      domain = sf_tmp$rank)
    bbox = c(-126, 24, -67,50)
    zoom_tmp  = 3.5
    leaflet(options = leafletOptions(minZoom = 3.5)) %>% 
      # addTiles(options = providerTileOptions(minZoom = 3, maxZoom = 10) )%>%
      setView(-100,41, zoom = zoom_tmp) %>% 
      addPolygons(data =sf_tmp,
                  color = ~pal(rank),
                  fillOpacity = 1,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "black", weight = 1,bringToFront = TRUE),
                  opacity = 1,
                  label = ~label)  %>% 
      addPolylines (data =sf_regions,
                    weight = 3,
                    fillOpacity = 0,
                    opacity = 1,
                    color = "black")  %>%
      setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addLegend("bottomright", pal = pal, values = sf_tmp$rank,
                title = "Rank",
                
                opacity = 1
      )
  })
  
  # Figure 4 ----
  
}


shinyApp(ui = ui, server = server)
