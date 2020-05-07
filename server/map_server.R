output$MAP = renderLeaflet({

    rg = filter(site_data, site_type == 'rain_gauge')
    sg = filter(site_data, site_type == 'stream_gauge')

    leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
        group='Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
        addProviderTiles('Esri.WorldImagery', group='Aerial Imagery') %>%
        addCircleMarkers(lng=~longitude, lat=~latitude, color='#228B22',
        # addCircleMarkers(lng=~longitude, lat=~latitude, color='#228B22',
            popup=glue(stream_gauge_buttons, domain=sg$domain,
                pretty_domain=sg$pretty_domain, stream=sg$stream,
                site_code=sg$site_name, site_name=sg$full_name,
                site_type=sg$site_type, latitude=sg$latitude,
                longitude=sg$longitude),
            popupOptions=c(
                className=paste0(sg$domain, "__", sg$site_name, '_popup'),
                minWidth=200, maxWidth=500
            ),
            data=sg) %>%
        addCircleMarkers(lng=~longitude, lat=~latitude, color='#0000FF',
            fillColor='#c6dbef', radius=8, opacity=0.6, weight=4,
            popup=glue(rain_gauge_buttons, domain=rg$domain,
                pretty_domain=rg$pretty_domain, site_type=rg$site_type,
                site_code=rg$site_name, site_name=rg$full_name,
                latitude=rg$latitude, longitude=rg$longitude),
            popupOptions=c(
                className=paste0(rg$domain, "__", rg$site_name, '_popup'),
                minWidth=200, maxWidth=500
            ),
            data=rg) %>%
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
