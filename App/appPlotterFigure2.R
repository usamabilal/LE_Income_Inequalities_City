figure2_plotter = function(ineqTmp,typeTmp){
  
  sf_tmp = sf_cbsa %>% 
    left_join(df_fig2 %>%
                filter(ineq == ineqTmp) %>%
                filter(type2 == typeTmp))
  
  pal <- colorBin(
    reverse = T,
    palette = "Reds",
    bins = 4,
    domain = sf_tmp$rank)
  bbox = c(-126, 24, -67,50)
  zoom_tmp  = 3.5
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 3.5,maxZoom = 6,
                                   attributionControl=FALSE)) %>% 
    setView(-100,41, zoom = zoom_tmp) %>% 
    addPolygons (data =sf_regions,
                 color = "#c7c7c7",
                 weight = 0,
                 fillOpacity = 0.5)  %>%
    addPolygons(data =sf_tmp,
                color = ~pal(rank),
                fillOpacity = 1,
                weight = 1,
                highlightOptions = highlightOptions(color = "black", weight = 1,bringToFront = TRUE),
                opacity = 1,
                label = ~label)  %>%
    addPolylines (data =sf_regions,
                  weight = 3,
                  fillOpacity = 0,
                  opacity = 1,
                  color = "black")  %>%
    # addPolylines(data = sf_fig2_missing_hatched,
    #              color = c("black"),
    #              weight = 1.0,
    #              label="Data was not available for ME and WI") %>% 
    # addPolygons(data = sf_fig2_states_missing,
    #             fillOpacity = 0,
    #             weight = 1.0,
    #             color = 'black',
    #             label="Data was not available for ME and WI") %>% 
    setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
    addLegend("bottomright", pal = pal, values = sf_tmp$rank,
              title = "Rank",
              
              opacity = 1
    )
}
