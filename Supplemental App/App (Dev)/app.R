load("cleaned_bundle.rdata")
source("global.R", local = T)
library(shiny)
library(tidyverse)
library(cowplot)
library(GGally)
library(sf)
library(shinyWidgets)

ui_tab1 = sidebarLayout(
    sidebarPanel(width = 3,
        pickerInput(
            inputId = "fig1_outcome",
            label = "Inequalities" ,
            choices = c("Total Inequalities" ="total",
                        "Income Inequalities"="income")),
        uiOutput("fig1_ui_input")
    ),
    mainPanel(plotOutput("plot_fig1"))
)


ui <- function(){
    fluidPage(
        navbarPage(
            title = "LE Paper",
            tabPanel("Home",ui_home),
            tabPanel("Figure 1",ui_tab1)
        )
    )
}



server <- function(input, output) {
    ## Fig1 ----
    ## __UI ----
    output$fig1_ui_input = renderUI({
        choices_tmp = tidy_ineq %>% 
            filter(outcome == input$fig1_outcome) %>% 
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
        # input$fig1_outcome ="total"
        # input$fig1_type = 
        req(input$fig1_type)
        df_tmp = tidy_ineq %>% 
            filter(outcome == input$fig1_outcome) %>% 
            filter(type==input$fig1_type)
        title_tmp = ifelse(input$fig1_outcome =="total",
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
}


shinyApp(ui = ui, server = server)
