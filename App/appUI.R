ui <- function(){
  fluidPage(
    useWaiter(),
    introjsUI(),
    useShinyjs(),
    # autoWaiter(c("plot_fig1", "plot_fig3")),
    tags$head(includeCSS("css/styles.css")),
    tags$head(includeCSS("css/footer.css")),
    tags$head(includeCSS("css/home.css")),
    tags$head(includeCSS("css/figures.css")),
    tags$head(includeHTML("html/headScripts.html")),
    tags$head(tags$script(src="fig3Unhover.js")),
    # tags$script(src = "introModal.js"),
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
loading_screen = div(
  class = "loadingScreenUHCDark",
  tags$img(
    src="urban-health-collaborative-logo.svg",
    height=100,
    id = "loadingScreen" 
  ),
  spin_ring()
  
)

figure_loading_screen = div(
  class = "figureLoadingScreen",
  spin_ring()
)

## Home ----
ui_home = (
  tagList(
    includeHTML("html/homePaperIntro.html")
    # actionButton("btn","Press me")
  )
)

## Table 1 ----
ui_table1 = div(class = "figureTabContainer",
                div(class = "figureTitle", "Table 1: Absolute and Relative in life expectancy in US MSAs"),
                HTML(' <button id="tourButtonTable1" type="button" class="btn btn-default action-button tourButton"> <i class="fas fa-route"> Table 1 Tour</i> </button>'),
                div(class = "figureContainer",
                    reactableOutput("table1"),
                    div(class = "Footnote",
                        HTML("<u>Footnote: </u> Absolute disparity was calculated as the difference between the 90th and 10th population-weighted percentiles of life expectancy for each city. Relative disparity was calculated as the ratio between the 90th and 10th population-weighted percentiles of life expectancy for each city. "))
                ))


## Figure 1  ----
ui_figure1 =  div(class = "figureTabContainer",
                  div(class = "figureTitle", "Figure 1: Total and Income-Based Life Expectancy Disparities Indicators in US MSAs"),
                  HTML(' <button id="tourButtonFigure1" type="button" class="btn btn-default action-button tourButton"> <i class="fas fa-route"> Figure 1 Tour</i> </button>'),
                  div(
                    class = "figureContainer",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   pickerInput(
                                     inputId = "fig1_age",
                                     label = "Age Group" ,
                                     choices = df_fig1_choices_age),
                                   pickerInput(
                                     inputId = "fig1_ineq",
                                     label = "Inequalities" ,
                                     choices = c("Total" ="Total",
                                                 "Income-based"="Income")),
                                   pickerInput(inputId = "fig1_type", 
                                               label = "Type" , 
                                               choices = fig1_type_default)),
                      mainPanel(uiOutput("plot_fig1_ui")) ) ) )







## Figure 2 ----
ui_figure2 = div(class = "figureTabContainer",
                 div(class = "figureTitle", "Figure 2: Spatial distribution of Disparities by MSA in the US."),
                 HTML(' <button id="tourButtonFigure2" type="button" class="btn btn-default action-button tourButton"> <i class="fas fa-route"> Figure 2 Tour</i> </button>'),
                 div(
                   class = "figureContainer",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  class = "wellFigure2",
                                  pickerInput(
                                    inputId = "fig2_ineq",
                                    label = "Inequalities" ,
                                    choices = c("Total" ="Total",
                                                "Income-based"="Income")),
                                  uiOutput("fig2_ui_input")
                     ),
                     mainPanel(
                       uiOutput("plot_fig2_ui")
                     )
                   )) )




## Figure 3  ----
ui_figure3  =  div(class = "figureTabContainer",
                   div(class = "figureTitle", "Figure 3: Mean, Standard Deviation, and Coefficient of Variation for life expectancy by tract median household income decile for each US MSA by region"),
                   HTML(' <button id="tourButtonFigure3" type="button" class="btn btn-default action-button tourButton"> <i class="fas fa-route"> Figure 3 Tour</i> </button>'),
                   div(
                     class = "figureContainer",
                     sidebarLayout(
                       sidebarPanel(width = 3,
                                    class = "wellFigure3",
                                    pickerInput(
                                      inputId = "fig3_age",
                                      label = "Age Group" ,
                                      choices = df_fig1_choices_age),
                                    pickerInput(
                                      inputId = "fig3_metric",
                                      label = "Select Statistic" ,
                                      choices = c("Mean",
                                                  "Standard Deviation",
                                                  "Coefficient of Variation"))),
                       mainPanel(
                         uiOutput("plot_fig3_ui")
                       ) ) ) )


