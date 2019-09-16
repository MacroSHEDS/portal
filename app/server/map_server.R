output$MAP <- renderLeaflet({
    # #Setup color values
    # shed.col <-
    #     colorFactor(c.col,
    #         domain = isco.sheds$BigName)
    leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
        group = 'Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
        addProviderTiles('Esri.WorldImagery', group = 'Aerial Imagery') %>%
        addCircleMarkers(lng=~long, lat=~lat,

            popup=paste0(
                # "<input id='",
                # site_data$sitegroup, "__", site_data$site,
                # "_info' type='button' class='btn btn-link ",
                # "btn-sm btn-block' value='Info'>",
                "<button data-toggle='collapse' class='btn btn-sm btn-link ",
                "btn-block' data-target='#", site_data$sitegroup, "__", site_data$site,
                "_collapse'>Info</button> <div id='",
                site_data$sitegroup, "__", site_data$site,
                "_collapse' class='collapse'> ",
                '<strong>Site name: </strong>',

                site_data$name, '<br>',
                '<strong>Data source: </strong>', site_data$source, '<br>',
                '<strong>Site group: </strong>', site_data$sitegroup, '<br>',
                '<strong>Site code: </strong>', site_data$site, '<br>',
                '<strong>Latitude: </strong>', 'add this<br>',
                '<strong>Longitude: </strong>', 'and this',

                " </div> ",
                "<button class='btn btn-sm btn-link btn-block' ",
                "id='",
                # " </div> <input id='",
                site_data$sitegroup, "__", site_data$site,
                "_goto'> Go to </button>"),
            # "_goto' type='button' class='btn btn-link ",
            # "btn-sm btn-block' value='Go to'>"),
            popupOptions=c(
                className=paste0(site_data$sitegroup, "__",
                    site_data$site, '_popup'),
                minWidth=200,
                maxWidth=500
            ),
            # popup=paste0('<strong>Site name: </strong>',
            #     site_data$name, '<br>',
            #     '<strong>Data source: </strong>', site_data$source, '<br>',
            #     '<strong>Site group: </strong>', site_data$sitegroup, '<br>',
            #     '<strong>Site code: </strong>', site_data$site),
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
    addLayersControl(
        position = 'topright',
        baseGroups = c('Topo Map', 'Aerial Imagery'),
        overlayGroups = c('Catchments'),
        options = layersControlOptions(collapsed = F, autoZIndex =
                T)
    ) %>%
        setView(lng = -81.93603515625,
            lat = 38.1046650598288,
            zoom = 10)
})


#Get id from map click
id <- reactive({
    validate(
        need(
            input$MapMine_shape_click != "",
            "Please select a catchment from the map to the left to view plots and data.
        App may take a few seconds to load data after selecting data (depending on internet connection speed)."
        )
    )
    (input$MapMine_shape_click)
})
