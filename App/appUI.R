ui <- function(){
  fluidPage(
    use_waiter(include_js = F),
    tags$head(includeCSS("css/styles.css")),
    tags$head(includeCSS("css/footer.css")),
    tags$head(includeCSS("css/home.css")),
    tags$head(includeCSS("css/figures.css")),
    tags$head(includeHTML("html/headScripts.html")),
    includeHTML("html/footerUHC2.html"),
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
  tagList(
    includeHTML("html/homePaperIntro.html")
  )
)

## Table 1 ----
ui_table1 = div(class = "figureTabContainer",
                div(class = "figureTitle", "Table 1: Absolute and Relative in life expectancy in US MSAs"),
                HTML('<button class="tourButton"><i class="fas fa-route"> Table 1 Guide</i></button>'),
                div(class = "figureContainer",
                    reactableOutput("table1")))


## Figure 1  ----
ui_figure1 =  div(class = "figureTabContainer",
                  div(class = "figureTitle", "Figure 1: Total and Income-Based Life Expectancy Inequality Indicators in US MSAs"),
                  HTML('<button class="tourButton"><i class="fas fa-route"> Figure 2 Guide</i></button>'),
                  div(
                    class = "figureContainer",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   pickerInput(
                                     inputId = "fig1_ineq",
                                     label = "Inequalities" ,
                                     choices = c("Total Inequalities" ="total",
                                                 "Income Inequalities"="income")),
                                   uiOutput("fig1_ui_input")),
                      mainPanel(plotOutput("plot_fig1")) ) ) )







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