outcomeTmp = "total"
typeTmp = "Abs. Disparity"
sizeTmp ="<120,000"

# Figure 1 Scr -----
outcomeTmp = input$fig1_ineq
typeTmp = input$fig1_type
df_tmp = df_fig1 %>% 
  filter(outcome == outcomeTmp) %>% 
  filter(type==typeTmp) %>% 
  select(Region_Name, value, cbsa, cbsa_name,total_pop ) %>% 
  mutate(value = round(value,2))

formatDots = paste0(
  "<span style='font-size: 14px;'>",
  "MSA: {point.cbsa_name} <br>",
  typeTmp,": {point.value} <br>",
  "</span>"
)

formatBox = paste0(
  "<span style='font-size: 14px;'>",
  "MSA: {point.key} <br>",
  typeTmp,": {point.value} <br>",
  "</span>"
)

#Create the chart
hcboxplot(
  x = df_tmp$value,
  var = df_tmp$Region_Name,
  name = typeTmp,
  outliers =F
) %>%
  hc_chart(type = "column") %>%
  hc_title(text = title_tmp) %>% 
  hc_yAxis(title = list(text = typeTmp)) %>%
  hc_add_series(
    data = df_tmp,
    type = "scatter",
    hcaes(x = "Region_Name", 
          y = df_tmp$value, 
          group = "Region_Name",
          size = df_tmp$total_pop)
  ) %>%
  hc_plotOptions(
    scatter = list(
      # tooltip = list(
      #   useHTML = T,
      #   pointFormat = formatDots,
      #   headerFormat = ""),
      # color = "red",
      marker = list(
        radius = 2,
        symbol = "circle",
        lineWidth = 1
      ),
      jitter = list(x = 0.1, y = 0)
    ),
    boxplot = list(
      color = "black"
      # tooltip = list(
      #   useHTML = T,
      #   pointFormat = "Maximum: {point.high}<br/>Upper quartile: {point.q3}<br/>Median: {point.median}<br/>Lower quartile: {point.q1}<br/>Minimum: {point.low}<br/>",
      #   headerFormat = "<span style= 'font-weight: bold; font-size: 77px;'>{point.key}</span><br>"
      # )
    ))
