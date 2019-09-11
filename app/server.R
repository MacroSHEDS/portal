library(shiny)
library(Cairo)
# library(shinyjs)

options(shiny.usecairo=TRUE)

shinyServer(function(input, output, session){

    # #hacky way to specify div height by % with js
    # height50 = reactive({
    #     ifelse(is.null(input$height50), 0, input$height50)
    # })
    # js$getHeight50()

    source('server/site_comparison_server.R', local=TRUE)
    source('server/timeseries_server.R', local=TRUE)
    source('server/summary_biplot_server.R', local=TRUE)

    output$MAP <- renderLeaflet({
        # #Setup color values
        # shed.col <-
        #     colorFactor(c.col,
        #         domain = isco.sheds$BigName)
        leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
            group = 'Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
            addProviderTiles('Esri.WorldImagery', group = 'Aerial Imagery') %>%
            addCircleMarkers(lng=~long, lat=~lat,
                popup=paste0('<strong>Site name: </strong>',
                    site_data$name, '<br>',
                    '<strong>Data source: </strong>', site_data$source, '<br>',
                    '<strong>Site group: </strong>', site_data$sitegroup, '<br>',
                    '<strong>Site code: </strong>', site_data$site),
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

})
