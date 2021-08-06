ui <- function(){
  fluidPage(
    use_waiter(include_js = F),
    tags$head(includeCSS("css/styles.css")),
    tags$head(includeCSS("css/footer.css")),
    tags$head(includeHTML("html/headScripts.html")),
    includeHTML("html/customUHCHeader.html"),
    navbarPage(
      title = "Heterogeneity in Disparities in Life Expectancy across US Metropolitan Areas",
      tabPanel("Home",ui_home),
      tabPanel("Table 1",ui_table1),
      tabPanel("Fig 1",ui_figure1),
      tabPanel("Fig 2",ui_figure2),
      tabPanel("Fig 3",ui_figure3)
    ),
    hr(),
    includeHTML("html/footerUHC.html"),
    waiter_show_on_load(
      html = loading_screen,
      color = "#07294d"
    )
  )
}


# ---- Loading
loading_screen = tagList(
  tags$img(
    src="urban-health-collaborative-logo.svg",
    height=100,
    id = "loadingScreen" # set id
  ),br(),br(),
  spin_ring(),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
)


## Home ----
ui_home = (
  column(12, align = 'center',
         tagList(
           h1("INTRO TO PAPER")
         ))
)

## Table 1 ----
ui_table1 =  sidebarLayout(
  sidebarPanel(width = 3
               
  ),
  mainPanel(DTOutput("table1"))
)

## Figure 1  ----
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





## Figure 2 ----
ui_figure2 = sidebarLayout(
  sidebarPanel(width = 3,
               pickerInput(
                 inputId = "fig2_ineq",
                 label = "Inequalities" ,
                 choices = c("Total Inequalities" ="total",
                             "Income Inequalities"="income")),
               uiOutput("fig2_ui_input")
  ),
  mainPanel(
    fluidRow(
      uiOutput("header_fig2")
    ),
    fluidRow(
      leafletOutput("plot_fig2", 
                    height = '500px', 
                    width = "800px")
    )
  )
)

## Figure 3  ----
ui_figure3 = "FIGURE 4 Content"