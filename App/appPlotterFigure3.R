figure3_plotter = function(sizeTmp){
  le_by_decile = df_fig3 %>% 
    filter(popGrp == sizeTmp)
  titleBase = "Life expectancy by median household income decile for each US MSA"
  # title_tmp =   paste0(titleBase," (",sizeTmp, " people) by region")
  title_tmp =   paste0("MSA with ",sizeTmp, " people")
  
  
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
    geom_point(aes(fill=Region_Name, text = tooltip), size=2, color="black", pch=21)+
    # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
    # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
    # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
    # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
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
  gp = ggplotly(figure3,
           tooltip = c("text"))%>% 
    config(displayModeBar = F)
  gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.03
  gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1
  gp 
  
}
