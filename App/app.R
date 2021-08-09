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
  source("appHelperFunctions.R", local = T)
  waiter_hide()
  
  # Table 1 ----
  output$table1 = renderReactable({ 
    table1_plotter(df_table1)
  })
  
  # Figure 1 ----
  output$fig1_ui_input = renderUI({
    choices_tmp = df_fig1_choices_type %>% filter(outcome == input$fig1_ineq) %>% pull(type)
    pickerInput(
      inputId = "fig1_type",
      label = "Type" ,
      choices = choices_tmp)
  })
  output$plot_fig1 = renderPlotly({
    req(input$fig1_type)
    outcomeTmp = input$fig1_ineq
    typeTmp = input$fig1_type
    df_tmp = df_fig1 %>% 
      filter(outcome == outcomeTmp) %>% 
      filter(type==typeTmp) %>% 
      select(Region_Name, value, cbsa, cbsa_name,total_pop ) %>% 
      mutate(value = round(value,2)) %>% 
      group_by(Region_Name) %>% 
      mutate(median = median(value)) %>% 
      ungroup() %>% 
      arrange(desc(median))
    levelsTmp = unique(df_tmp$Region_Name)
    df_tmp = df_tmp %>% mutate(Region_Name=factor(Region_Name, levels = levelsTmp))
    title_tmp = ifelse(outcomeTmp =="total","Total Life Expectancy Disparities by MSA","Income-based Life Expectancy Disparities by MSA")
    yaxis_tmp = typeTmp
    
    
    plot1 = ggplot(df_tmp,aes(x=Region_Name, y=value))+
      geom_boxplot(aes(group=(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
      geom_jitter(aes(fill=(Region_Name), size=total_pop), 
                  width=0.1, height=0, alpha=1,
                  color="black", pch=21) +
      # facet_wrap(~type, scales="free_y")+
      guides(color=F, fill=F, size=F)+
      labs(x="",
           y=yaxis_tmp,
           title=title_tmp)+
      #scale_y_continuous(sec.axis=dup_axis(name = ylab2), limits=ylim)+
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x=element_text(size=10, color="black"),
            axis.text.y=element_text(size=10, color="black"),
            axis.title.y=element_text(face="bold", size=12),
            plot.title=element_text(face="bold", size=13))
    ggplotly(plot1) %>% 
      config(displayModeBar = F)
  })
  
  
  # Figure 2 ----
  output$fig2_ui_input = renderUI({
    choices_tmp = df_fig1_choices_type %>% filter(outcome == input$fig2_ineq) %>% pull(type)
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
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 3.5,maxZoom = 6,
                                     attributionControl=FALSE)) %>% 
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
  
  # Figure 3 ----
  output$header_fig3 = renderUI({
    sizeTmp = input$fig3_MSAsize
    titleBase = "Life expectancy by median household income decile for each US MSA"
    h3(paste0(titleBase," (",sizeTmp, " people) by region"), align = 'center')
  })
  output$plot_fig3 = renderPlotly({
    sizeTmp = input$fig3_MSAsize
    le_by_decile = df_fig3 %>% 
      filter(popGrp == sizeTmp)
    
    
    figure3<-ggplot(le_by_decile, 
                    aes(x=decile_income, y=le, group=cbsa)) +
      geom_line(data=le_by_decile %>% mutate(Region_Name="Midwest"), 
                color="gray", alpha=1)+
      geom_line(data=le_by_decile %>% mutate(Region_Name="Northeast"), 
                color="gray", alpha=1)+
      geom_line(data=le_by_decile %>% mutate(Region_Name="South"), 
                color="gray", alpha=1)+
      geom_line(data=le_by_decile %>% mutate(Region_Name="West"), 
                color="gray", alpha=1)+
      geom_line(aes(color=Region_Name))+
      geom_point(aes(fill=Region_Name), size=2, color="black", pch=21)+
      # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
      # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
      # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
      # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
      labs(x="Decile of Median Household Income",
           y="Life Expectancy (years)")+
      scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
      scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
      facet_wrap(~Region_Name)+
      guides(color=F, fill=F)+
      theme_bw() +
      theme(axis.text=element_text(color="black", size=10),
            axis.title.x =element_text(color="black", face="bold", size=12
                                      ),
            axis.title.y =element_text(color="black", face="bold", size=12),
            strip.text=element_text(color="black", face="bold", size=10),
            strip.background = element_blank(),
            legend.position = 'none')
    ggplotly(figure3)%>% 
      layout(margin = list(l=25, r=50, b=50, t=50, pad=0)) %>% 
      config(displayModeBar = F)
  })
}


shinyApp(ui = ui, server = server)
