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
                    reactableOutput("table1"),
                    div(class = "Footnote",
                        HTML("<u>Footnote: </u> Absolute disparity was calculated as the difference between the 90th and 10th population-weighted percentiles of life expectancy for each city. Relative disparity was calculated as the ratio between the 90th and 10th population-weighted percentiles of life expectancy for each city. "))
                ))


## Figure 1  ----
ui_figure1 =  div(class = "figureTabContainer",
                  div(class = "figureTitle", "Figure 1: Total and Income-Based Life Expectancy Disparities Indicators in US MSAs"),
                  HTML('<button class="tourButton"><i class="fas fa-route"> Figure 1 Guide</i></button>'),
                  div(
                    class = "figureContainer",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   pickerInput(
                                     inputId = "fig1_ineq",
                                     label = "Inequalities" ,
                                     choices = c("Total" ="total",
                                                 "Income-based"="income")),
                                   uiOutput("fig1_ui_input")),
                      mainPanel(plotlyOutput("plot_fig1")) ) ) )







## Figure 2 ----
ui_figure2 = div(class = "figureTabContainer",
                 div(class = "figureTitle", "Figure 2: Spatial distribution of Disparities by MSA in the US."),
                 HTML('<button class="tourButton"><i class="fas fa-route"> Figure 2 Guide</i></button>'),
                 div(
                   class = "figureContainer",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  pickerInput(
                                    inputId = "fig2_ineq",
                                    label = "Inequalities" ,
                                    choices = c("Total" ="total",
                                                "Income-based"="income")),
                                  uiOutput("fig2_ui_input")
                     ),
                     mainPanel(
                       fluidRow(
                         uiOutput("header_fig2")
                       ),
                       fluidRow(
                         leafletOutput("plot_fig2")
                       ),
                       div(class = "Footnote",
                           HTML("<u>Footnote: </u> Rank indicates the widest (1) to narrowest (499) disparities. "))
        
                     )
                   )) )




## Figure 3  ----
ui_figure3  =  div(class = "figureTabContainer",
                   div(class = "figureTitle", "Figure 3: Life expectancy by median household income decile for each US MSA by region"),
                   HTML('<button class="tourButton"><i class="fas fa-route"> Figure 3 Guide</i></button>'),
                   div(
                     class = "figureContainer",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    pickerInput(
                                      inputId = "fig3_MSAsize",
                                      label = "MSA Size" ,
                                      choices = c("<120,000",
                                                  "120,000-150,000",
                                                  "150,000-220,000",
                                                  "220,000-400,000",
                                                  "400,000-800,000",
                                                  ">800,000"))),
                       mainPanel(
                         fluidRow(
                           uiOutput("header_fig3")
                         ),
                         plotlyOutput("plot_fig3")
                         
                         ) ) ) )


