figure3_plotter = function(sizeTmp){
  le_by_decile = df_fig3 %>% filter(popGrp == sizeTmp)
  dataShare = highlight_key(le_by_decile, ~cbsa )
  titleBase = "Life expectancy by median household income decile for each US MSA"
  # title_tmp =   paste0(titleBase," (",sizeTmp, " people) by region")
  title_tmp =   paste0("MSA with ",sizeTmp, " people")
  
  gray_alpha = 0.4
  figure3 = ggplot(dataShare, 
                   aes(x=decile_income, y=le, group=cbsa)) +
    geom_line(data=le_by_decile %>% mutate(Region_Name="Midwest"),
              color="gray", alpha=gray_alpha)+
    geom_line(data=le_by_decile %>% mutate(Region_Name="Northeast"),
              color="gray", alpha=gray_alpha)+
    geom_line(data=le_by_decile %>% mutate(Region_Name="South"),
              color="gray", alpha=gray_alpha)+
    geom_line(data=le_by_decile %>% mutate(Region_Name="West"),
              color="gray", alpha=gray_alpha)+
    geom_line(aes(col=Region_Name))+
    geom_point(aes(fill=Region_Name, text = tooltip), size=2, color="black", pch=21)+
    labs(x="Decile of Median Household Income",
         y="Life Expectancy (years)",
         title = title_tmp)+
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
  gg = ggplotly(figure3,
                tooltip = c("text"),
                textposition = 'right')%>% 
    config(displayModeBar = F)
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- -0.03
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1
  highlight( gg, on = "plotly_hover", off = "plotly_doubleclick", color = NULL ,
               opacityDim=0.15)  %>%
    onRender("
    function(el) {
      el.on('plotly_unhover', function(d) {
        Plotly.restyle('plot_fig3', {opacity: 1}, [4, 5, 6, 7, 8, 9, 10, 11]);
      });
    }
  ")
  
}
