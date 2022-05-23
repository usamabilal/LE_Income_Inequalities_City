figure1_plotter = function(outcomeTmp,typeTmp, ageTmp){
  df_tmp = df_fig1 %>% 
    filter(outcome == outcomeTmp) %>% 
    filter(type==typeTmp) %>% 
    filter(age_grp == ageTmp)
  print(df_tmp)
  levelsTmp = unique(df_tmp$Region_Name)
  df_tmp = df_tmp %>% mutate(Region_Name=factor(Region_Name, levels = levelsTmp))
  title_tmp = ifelse(outcomeTmp =="Total","Total Life Expectancy Disparities by MSA","Income-based Life Expectancy Disparities by MSA")
  title_tmp = paste0(title_tmp," (",ageTmp,")")
  yaxis_tmp = typeTmp
  minTmp = ifelse(min(df_tmp$value)<0,min(df_tmp$value)*1.05,min(df_tmp$value)*0.95)
  yMinTmp = ifelse(typeTmp%in%c('Top/Bottom Ratio','Rel. Disparity'), 1,0)
  if (minTmp<yMinTmp){yMinTmp=minTmp}
  yMaxTmp = max(df_tmp$value)*1.01
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
          plot.title=element_text(face="bold", size=13))+
    ylim(c(yMinTmp,yMaxTmp))
  ## Add lines
  if (typeTmp %in% c('Top/Bottom Ratio','Rel. Disparity')){plot1=plot1+ geom_hline(yintercept = 1, lty = 2)}
  if (typeTmp %in% c('Top/Bottom Difference','Abs. Disparity')){plot1=plot1+ geom_hline(yintercept = 0, lty = 2)}
  ggp = ggplotly(p = plot1,
           tooltip = c("text")) %>% 
    config(displayModeBar = F)
  ggp$x$data[[1]]$marker$outliercolor ="rgba(0,0,0,0)"
  ggp$x$data[[1]]$marker$color ="rgba(0,0,0,0)"# When creating plot p with ggplot if you specify fill = cut use x$fill$color instead $line$color
  ggp$x$data[[1]]$marker$line = "rgba(0,0,0,0)" # When creating plot p with ggplot if you specify fill = cut use x$fill$color instead $line$color
  ggp$x$data[[1]]$marker$size = 0.00001 # When creating plot p with ggplot if you specify fill = cut use x$fill$color instead $line$color
  ggp
  
}
