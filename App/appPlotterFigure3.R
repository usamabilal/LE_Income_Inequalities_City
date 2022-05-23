figure3_plotter = function(metricTmp){
  dfTmp = df_fig3 %>% filter(type==metricTmp)  
  dataShare =  highlight_key(dfTmp, ~Region )
  yAxisTitleTmp =metricTmp %>%  recode("mean"="",'sd'='SD of ','cv'='CV of ') %>%  paste0("Life Expectancy (years)")
  
  titleTmp =paste( metricTmp, ifelse(metricTmp=="Mean",'','for'),'life expectancy by tract median household income decile for each US MSA by Region')
  
  hchart(
    dfTmp,
    type = "line",
    hcaes(x = decile_income , y = value, group = Region),
    marker = list(symbol = 'circle'),
    color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","black"),
    showInLegend = TRUE,
    tooltip = list(pointFormat = "{point.tooltip_HTML}" )
  ) %>% 
    hc_yAxis(title = list(text =yAxisTitleTmp)) %>% 
    hc_xAxis(title = list(text = "Decile of Median Household Income")) %>% 
    hc_title(text = titleTmp) %>% 
    hc_tooltip(headerFormat = '')
  
  
  # 
  # if (metricTmp == 'cv' ){
  #   figure3<-figure3+
  #     scale_y_continuous(limits=c(0, 4.2), breaks=seq(0, 4, by=1))+
  #     scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))
  # } else if (metricTmp == 'sd') {
  #   figure3<- figure3+
  #     scale_y_continuous(limits=c(0, 3.2), breaks=seq(0, 3, by=1))+
  #     scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))
  # } else if (metricTmp == 'mean'){
  #   figure3<- figure3+
  #     scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  #     scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=2))
  # }
  # 
  
  
}