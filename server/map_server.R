output$MAP = renderLeaflet({

    rain_gages = filter(site_data, site_type == 'rain_gage')
    stream_gages = filter(site_data, site_type == 'gaging_station')

    leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
        group='Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
        addProviderTiles('Esri.WorldImagery', group='Aerial Imagery') %>%
        addCircleMarkers(lng=~longitude, lat=~latitude, color='#228B22',
            popup=glue(map_buttons, domain=site_data$domain,
                stream=site_data$stream, site_name=site_data$site_name,
                latitude=site_data$latitude, longitude=site_data$longitude),
            popupOptions=c(
                className=paste0(site_data$domain, "__",
                    site_data$site_name, '_popup'),
                minWidth=200, maxWidth=500
            ),
            data=stream_gages) %>%
        addCircleMarkers(lng=~longitude, lat=~latitude, color='#0000FF',
            fillColor='#c6dbef', radius=8, opacity=0.6, weight=4,
            popup=glue(map_buttons, domain=site_data$domain,
                stream=site_data$stream, site_name=site_data$site_name,
                latitude=site_data$latitude, longitude=site_data$longitude),
            popupOptions=c(
                className=paste0(site_data$domain, "__",
                    site_data$site_name, '_popup'),
                minWidth=200, maxWidth=500
            ),
            data=rain_gages) %>%
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
    # setView(lng=mean(site_data$longitude, na.rm=TRUE),
    #     lat=mean(site_data$latitude, na.rm=TRUE), zoom=3)
    setView(lng=-97.380979, lat=42.877742, zoom=3) #center of lower 48
    #flyTo() #temporary: improve ux with this and marker groupings
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
