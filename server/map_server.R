

sheds <- sf::st_read('data/general/shed_boundary') %>%
    sf::st_transform(4326)

sg = filter(site_data, site_type == 'stream_gauge')

sheds <- sheds %>%
    filter(site_name %in% sg$site_name)

output$MAP = renderLeaflet({

    rg = filter(site_data, site_type == 'rain_gauge')


    leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
        group='Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
        addProviderTiles('Esri.WorldImagery', group='Aerial Imagery') %>%
        addPolygons(
            data = sheds,
            weight = 3,
            smooth = 0,
            stroke = T,
            fillOpacity = 0.2,
            color = '#000000',
            layerId = sheds$site_name,
            highlightOptions = highlightOptions(color = '#228B22', fill = '#228B22',
                                                opacity=1),
            group = 'Catchments'
        ) %>%
        addCircleMarkers(lng=rg$longitude, lat=rg$latitude, color='#0000FF',
                         fillColor='#0000FF', stroke = TRUE, opacity=0.5, radius=4,
                         weight=10, fillOpacity=1,
                         popup=glue(rain_gauge_buttons, domain=rg$domain,
                                    pretty_domain=rg$pretty_domain, site_type=rg$site_type,
                                    site_code=rg$site_name, site_name=rg$full_name,
                                    latitude=rg$latitude, longitude=rg$longitude),
                         popupOptions=c(
                             className=paste0(rg$domain, "__", rg$site_name, '_popup'),
                             minWidth=200, maxWidth=500
                         ),
                         data=rg) %>%
        addCircleMarkers(lng=sg$longitude, lat=sg$latitude, color='#228B22',
                         layerId=sg$site_name, stroke = TRUE, opacity=0.5, radius=4,
                         weight=10, fillOpacity=1, fillColor='#228B22',
                    popup=glue(stream_gauge_buttons, domain=sg$domain,
                         pretty_domain=sg$pretty_domain, stream=sg$stream,
                         site_code=sg$site_name, site_name=sg$full_name,
                         site_type=sg$site_type, latitude=sg$latitude,
                         longitude=sg$longitude,
                         attribution=paste0(sg$domain, "__", sg$site_name)),
                    popupOptions=c(
                         className=paste0(sg$domain, "__", sg$site_name, '_popup'),
                         minWidth=200, maxWidth=500), label = ~site_name, clusterId=sg$domain,
                    clusterOptions = markerClusterOptions(zoomToBoundsOnClick=T, maxClusterRadius=4.5),
                         data=sg) %>%
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
    setView(lng=-97.380979, lat=42.877742, zoom=2)  #center of lower 48
    #flyTo() #temporary: improve ux with this and marker groupings
})

#Highlight on click
proxy <- leafletProxy("MAP", session)

#Highlight stream gauges when watersheds are clicked
prev_select <- reactiveVal()

observeEvent({
        input$MAP_shape_click
        input$MAP_click
    }, {

    if(is.null(prev_select()) && is.null(input$MAP_shape_click)) { } else {

        code <- str_split_fixed(input$MAP_shape_click$id, '-', n = Inf)[1]

        sg <- filter(site_data, site_type == 'stream_gauge') %>%
            filter(site_name == code)

        selected <- list(id = code, lat = sg$latitude, lng = sg$longitude)

        proxy %>%
            addCircleMarkers(
                layerId = paste0(code, '-temp'),
                lng = sg$longitude,
                lat = sg$latitude,
                color = '#228B22', stroke = TRUE, opacity = 1, radius = 4,
                weight = 10, fillOpacity = 1, fillColor = '#228B22',
                popup = glue(stream_gauge_buttons, domain = sg$domain,
                             pretty_domain = sg$pretty_domain, stream = sg$stream,
                             site_code = sg$site_name, site_name = sg$full_name,
                             site_type = sg$site_type, latitude = sg$latitude,
                             longitude = sg$longitude,
                             attribution = paste0(sg$domain, "__", sg$site_name)),
                popupOptions = c(
                    className = paste0(sg$domain, "__", sg$site_name, '_popup'),
                    minWidth = 200, maxWidth = 500), label = sg$site_name,
                data = sg)  %>%
            removeMarker(layerId = paste0(prev_select()$id, '-temp'))

        prev_select(selected)
    }
})

# Highlight watersheds when stream guages are clicked
prev_select_m <- reactiveVal()
observeEvent({
        input$MAP_marker_click
        input$MAP_click
    }, {

    if(is.null(prev_select_m()) && is.null(input$MAP_marker_click)) { } else {

        code_ <- str_split_fixed(input$MAP_marker_click$id, '-', n=Inf)[1]
        shed <- sheds %>%
            filter(site_name == code_)

        selected <- list(id = code_)

        site_remove <- prev_select_m()$id

            proxy %>%
                addPolygons(data = shed, weight = 3, smooth = 0, stroke = T,
                            fillOpacity = 0.2, color = '#228B22',
                            layerId = paste0(shed$site_name, '-temp'), group = 'Catchments')

        proxy %>%
            addPolygons(data = shed, weight = 3, smooth = 0, stroke = T,
                fillOpacity = 0.2, color = '#228B22',
                layerId = paste0(shed$site_name, '-temp'), group = 'Catchments') %>%
            removeShape(layerId = paste0(site_remove, '-temp'))

        prev_select_m(selected)
    }
})


## Add site as a class
# htmlwidgets::onRender("
#         for (var i = 0; i < data.longitude.length; i++) {
#         var site = data.site_name[i];
#         var domain = data.domain[i];
#         var sep = '-'
#         var name = domain.concat(sep, site);
#         var myIcon = L.divIcon({className: name});
#         L.marker([data.latitude[i], data.longitude[i]], {icon: myIcon}).addTo(this);",
#                       data = sg)



#Get id from map click
# test <- reactive({
#     validate(
#         need(
#             input$MapMine_shape_click != "",
#             "Please select a catchment from the map to the left to view plots and data.
#         App may take a few seconds to load data after selecting data (depending on internet connection speed)."
#         )
#     )
#     (input$MapMine_shape_click)
# })
