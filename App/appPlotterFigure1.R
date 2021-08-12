figure1_plotter = function(outcomeTmp,typeTmp){
  df_tmp = df_fig1 %>% 
    filter(outcome == outcomeTmp) %>% 
    filter(type==typeTmp) 
  levelsTmp = unique(df_tmp$Region_Name)
  df_tmp = df_tmp %>% mutate(Region_Name=factor(Region_Name, levels = levelsTmp))
  title_tmp = ifelse(outcomeTmp =="Total","Total Life Expectancy Disparities by MSA","Income-based Life Expectancy Disparities by MSA")
  yaxis_tmp = typeTmp
  plot1 = ggplot(df_tmp,aes(x=Region_Name, y=value))+
    geom_boxplot(aes(group=(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
    geom_jitter(aes(fill=(Region_Name), size=total_pop, text = tooltip), 
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
  ggplotly(p = plot1,
           tooltip = c("text")) %>% 
    config(displayModeBar = F)
}
