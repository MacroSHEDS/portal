output$MAP <- renderLeaflet({

    leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
        group='Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
        addProviderTiles('Esri.WorldImagery', group='Aerial Imagery') %>%
        addCircleMarkers(lng=~longitude, lat=~latitude,
            popup=glue(map_buttons, domain=site_data$domain,
                stream=site_data$stream, site_name=site_data$site_name,
                latitude=site_data$latitude, longitude=site_data$longitude),
            popupOptions=c(
                className=paste0(site_data$domain, "__",
                    site_data$site_name, '_popup'),
                minWidth=200, maxWidth=500
            ),
            data=site_data) %>%
        # addPolygons(
        #     data = isco.sheds,
        #     weight = 3,
        #     smooth = 0,
        #     stroke = T,
        #     fillOpacity = 0.2,
        #     color = shed.col(isco.sheds$BigName),
        #     popup = paste('Site = ', isco.sheds$Site, sep = ''),
        #     layerId = isco.sheds$Site,
        #     group = 'Catchments'
        # ) %>%
    # addLegend(
    #   position = 'topright',
    #   values = isco.sheds$BigName,
    #   labels = isco.sheds$BigName,
    #   pal = shed.col,
    #   title = 'Study Catchment'
    # ) %>%
    addLayersControl(position='topright',
        baseGroups=c('Topo Map', 'Aerial Imagery'),
        # overlayGroups = c('Catchments'),
        options=layersControlOptions(collapsed=FALSE, autoZIndex=TRUE)) %>%
    setView(lng=mean(site_data$longitude, na.rm=TRUE),
        lat=mean(site_data$latitude, na.rm=TRUE), zoom=8)
})


# #Get id from map click
# id <- reactive({
#     validate(
#         need(
#             input$MapMine_shape_click != "",
#             "Please select a catchment from the map to the left to view plots and data.
#         App may take a few seconds to load data after selecting data (depending on internet connection speed)."
#         )
#     )
#     (input$MapMine_shape_click)
# })
