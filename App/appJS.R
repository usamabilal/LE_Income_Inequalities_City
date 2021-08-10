
home_intro = function(session, df_intro_home){
  runjs(js_makeItemsActive)
  introjs(session,
          options = list(steps = df_intro_home),
          events = list("oncomplete"=I(js_restoreItemClass),
                        "onexit"=I(js_restoreItemClass) ))
}



figure_intro  = function(session, df_steps){ introjs(session, options = list(steps = df_steps))}

# Home ----
introModal =  modalDialog( 
  easyClose = TRUE,
  div(id = "intro_modal",
      includeHTML("html/modalIntro.html")
  ),
  footer = fluidRow(
    column(4,actionButton(inputId = "intro", 
                          label = "Introduction Tour", 
                          icon = icon("info-circle fa-blue"))),
    column(6),
    column(2,modalButton("Dismiss")),
    align = "center" ) )

js_makeItemsActive  = " 
navItems = document.
    querySelector('.navbar-nav').
    querySelectorAll('li');

  navItems.forEach((element) => {
    element.classList.add('active');
  });"
js_restoreItemClass  = " 
  navItems.forEach((element) => {
    element.classList.remove('active');
  });

  navItems[0].classList.add('active');
"
df_intro_home = read.csv( "introJS/df_intro_home.csv") %>% select(-X)

# Table 1 ----
df_intro_table1 = read.csv( "introJS/df_intro_table1.csv") %>% select(-X)

# Figure 1 ----
df_intro_figure1 = read.csv( "introJS/df_intro_figure1.csv") %>% select(-X)

# Figure 2 ----
df_intro_figure2 = read.csv( "introJS/df_intro_figure2.csv") %>% select(-X)

# Figure 3 ----
df_intro_figure3 = read.csv( "introJS/df_intro_figure3.csv") %>% select(-X)
