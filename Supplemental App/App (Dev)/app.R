library(shiny)
library(tidyverse)
library(cowplot)
library(GGally)
library(sf)
library(leaflet)

library(shinyWidgets)
library(DT)

load("cleaned_bundle.rdata")
source("global.R", local = T)

## Figure 3 (Map) ----
ui_figure3 = sidebarLayout(
    sidebarPanel(width = 3,
                 pickerInput(
                     inputId = "fig3_ineq",
                     label = "Inequalities" ,
                     choices = c("Total Inequalities" ="total",
                                 "Income Inequalities"="income")),
                 uiOutput("fig3_ui_input")
    ),
    mainPanel(
        fluidRow(
            uiOutput("header_fig3")
        ),
        fluidRow(
            leafletOutput("plot_fig3", 
                          height = '500px', 
                          width = "800px")
        )
    )
)

ui <- function(){
    fluidPage(
        tags$head(
            tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
        ),
        navbarPage(
            title = "LE Paper",
            tabPanel("Home",ui_home),
            tabPanel("Figure 1",ui_figure1),
            tabPanel("Figure 3",ui_figure3),
            tabPanel("Table 1",ui_table1)
        )
    )
}




server <- function(input, output) {
    ## Figure 1 ----
    ## __UI ----
    output$fig1_ui_input = renderUI({
        choices_tmp = tidy_ineq %>% 
            filter(outcome == input$fig1_ineq) %>% 
            count(type) %>% 
            pull(type) %>% 
            as.character()
        pickerInput(
            inputId = "fig1_type",
            label = "Type" ,
            choices = choices_tmp)
    })
    ## __Outputs ----
    output$plot_fig1 = renderPlot({
        # input = list()
        # input$fig1_ineq ="total"
        # input$fig1_type = 
        req(input$fig1_type)
        df_tmp = tidy_ineq %>% 
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
    
    
    ## Figure 3 ----
    ## __UI ----
    output$fig3_ui_input = renderUI({
        choices_tmp = tidy_ineq %>%
            filter(outcome == input$fig3_ineq) %>%
            count(type) %>%
            pull(type) %>%
            as.character()
        pickerInput(
            inputId = "fig3_type",
            label = "Type" ,
            choices = choices_tmp)
    })
    
    output$header_fig3 = renderUI({
       ineq_tmp = ifelse(input$fig3_ineq=="total","Total","Income")
       type_tmp = input$fig3_type
       h3(paste0(ineq_tmp,": ",type_tmp), align = 'center')
    
            
        
    })
    
    
    ## __Outputs ----
    output$plot_fig3 = renderLeaflet({
        # input = list()
        # input$fig3_ineq ="total"
        # input$fig3_type = "Abs. Difference"
        req(input$fig3_type)
        sf_tmp = shp_with_data %>%
            filter(ineq == ifelse(input$fig3_ineq=="total","Total","Income")) %>%
            filter(type == input$fig3_type) %>% 
            mutate(label = str_c("<b>",NAME,"</b><br>",
                                 "<b>",ineq," ",type," :</b>", round(value,2),"<br>",
                                 "<b>Rank :</b>",rank
                                 ) %>% 
                       map(~HTML(.x)))
        
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
            addPolylines (data =regions,
                        weight = 3,
                        fillOpacity = 0,
                        opacity = 1,
                        color = "black")  %>%
            setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            addLegend("bottomright", pal = pal, values = sf_tmp$rank,
                      title = "Rank",
                    
                      opacity = 1
            )

        # ggplot()+
        # geom_sf(data=sf_tmp, size=0,
        #         aes(geometry=geometry, color=rank, fill=rank))+
        # geom_sf(data=sf_states, size=0.5, color="black",
        #         fill=NA,
        #         aes(geometry=geometry))+
        # geom_sf(data=regions, size=0.5, color="black",
        #         fill=NA,
        #         aes(geometry=geometry))+
        # scale_fill_binned(name="Rank", type="gradient",
        #                   show.limits=T,n.breaks=5,labels=round,
        #                   low="red", high="white")+
        # scale_color_binned(name="Rank", type="gradient",
        #                    show.limits=T,n.breaks=5,labels=round,
        #                    low="red", high="white")+
        # coord_sf(xlim = c(bbox_temp["xmin"], bbox_temp["xmax"]),
        #          ylim = c(bbox_temp["ymin"], bbox_temp["ymax"]), expand = T)# +
        # guides(alpha=F, size=F, color=F)+
        # #labs(title="Renta media por hogar") +
        # facet_wrap(~type2, nrow=2)+
        # theme_void()+
        # theme(plot.title = element_text(size=20, face="bold", hjust=.5),
        #       strip.text = element_text(size=10, face="bold", hjust=.5),
        #       panel.background = element_rect(fill = "white", color=NA),
        #       legend.position="bottom")
        # 
    })
    
    
    ## Table 1 ----
    ## __Outputs ----
    
    output$table1 = renderDT({ 
        table1
    })
    
}


shinyApp(ui = ui, server = server)
