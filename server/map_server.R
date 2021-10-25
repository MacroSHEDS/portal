

sheds <- sf::st_read('data/general/shed_boundary') %>%
    sf::st_transform(4326)

sg <- filter(site_data,
             site_type == 'stream_gauge')

watershed_summaries <- sm(read_csv('data/general/spatial_downloadables/watershed_summaries.csv'))

watershed_quar <- watershed_summaries %>%
    summarise('Annual Mean Precip (mm)_bb' = quantile(cc_mean_annual_precip, .25, na.rm = T),
              'Annual Mean Precip (mm)_tt' = quantile(cc_mean_annual_precip, .75, na.rm = T),
              'Annual Mean Temp (C)_bb' = quantile(cc_mean_annual_temp, .25, na.rm = T),
              'Annual Mean Temp (C)_tt' = quantile(cc_mean_annual_temp, .75, na.rm = T),
              'Mean Slope (%)_bb' = quantile(te_slope_mean, .25, na.rm = T),
              'Mean Slope (%)_tt' = quantile(te_slope_mean, .75, na.rm = T),
              'Area (ha)_bb' = quantile(ws_area_ha, .25, na.rm = T),
              'Area (ha)_tt' = quantile(ws_area_ha, .75, na.rm = T)) %>%
    pivot_longer(cols = everything()) %>%
    mutate(var = str_split_fixed(name, '_', n = Inf)[,1],
           quan = str_split_fixed(name, '_', n = Inf)[,2]) %>%
    select(-name) %>%
    pivot_wider(names_from = quan, values_from = value)

sheds <- sheds %>%
    filter(site_code %in% sg$site_code)

output$MAP <- renderLeaflet({

    rg <- filter(site_data,
                 site_type == 'rain_gauge')

    leaflet() %>%
        addProviderTiles("Esri.WorldTopoMap",
                         group = paste0('Topo Mahttps://www.dropbox.com/s/kjkhw',
                                        'ip0t8erh3a/MTM_PQ_data.csv?dl=0p')) %>%
        addProviderTiles('Esri.WorldImagery',
                         group = 'Aerial Imagery') %>%
        addPolygons(
            data = sheds,
            weight = 3,
            smooth = 0,
            stroke = TRUE,
            fillOpacity = 0.2,
            color = '#000000',
            layerId = sheds$site_code,
            highlightOptions = highlightOptions(color = '#b66397',
                                                fill = '#b66397',
                                                opacity=.9),
            group = 'Catchments') %>%
        # rain gauge
        # purple #8856a7  lightgrey #A2A7A9 darkgrey #222d32 grey #82898B
        addCircleMarkers(lng = rg$longitude,
                         lat = rg$latitude,
                         color = '#69D9FE80',
                         fillColor = '#4a565cc50',
                         layerId = paste0(rg$site_code, '_*_rain'),
                         stroke = TRUE,
                         opacity = 0.5,
                         radius = 3,
                         weight = 10,
                         fillOpacity = 1,
                         popup = glue(rain_gauge_buttons,
                                      domain = rg$domain,
                                      # pretty_domain = rg$pretty_domain,
                                      # site_type = rg$site_type,
                                      site_code = rg$site_code,
                                      # full_name = rg$full_name,
                                      # latitude = rg$latitude,
                                      # longitude = rg$longitude
                                      ),
                         popupOptions = c(className = paste0(rg$domain,
                                                             '__',
                                                             rg$site_code,
                                                             '_popup'),
                                          minWidth = 200,
                                          maxWidth = 500),
                         data = rg) %>%
        # chemistry gauge
        addCircleMarkers(lng = sg$longitude,
                         lat = sg$latitude,
                         color = '#69D9FE80',
                         layerId = sg$site_code,
                         stroke = TRUE,
                         opacity = 0.5,
                         radius = 3,
                         weight = 4.5,
                         fillOpacity = 1,
                         fillColor = '#FF75D5',
                    popup = glue(stream_gauge_buttons,
                                 domain = sg$domain,
                                 # pretty_domain = sg$pretty_domain,
                                 # stream = sg$stream,
                                 site_code = sg$site_code,
                                 # full_name = sg$full_name,
                                 # site_type = sg$site_type,
                                 # latitude = sg$latitude,
                                 # longitude = sg$longitude,
                                 attribution = paste0(sg$domain,
                                                      '__',
                                                      sg$site_code)),
                    popupOptions = c(className = paste0(sg$domain,
                                                        '__',
                                                        sg$site_code,
                                                        '_popup'),
                                     minWidth = 200,
                                     maxWidth = 500),
                    label = ~site_code,
                    clusterId = sg$domain,
                    clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                          maxClusterRadius = 4.5,
                                                          iconCreateFunction=JS("function (cluster) {
                                                                                    var childCount = cluster.getChildCount();
                                                                                    if (childCount < 3) {
                                                                                      c = '#69D9FE60;'
                                                                                    } else if (childCount < 5) {
                                                                                      c = '#59bce460;'
                                                                                    } else if (childCount < 7) {
                                                                                      c = '#4aa0ca60;'
                                                                                    } else if (childCount < 9) {
                                                                                      c = '#3a84b160;'
                                                                                    } else {
                                                                                      c = '#2a6a9960;'
                                                                                    }
                                                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

                                                                                  }")),
                    data = sg) %>%
    addLayersControl(position = 'topright',
                     baseGroups = c('Topo Map', 'Aerial Imagery'),
                     options = layersControlOptions(collapsed = FALSE,
                                                    autoZIndex = TRUE)) %>%
    setView(lng = -97.380979,
            lat = 42.877742,
            zoom = 2)  #center of lower 48
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

        site_id <- input$MAP_shape_click$id
        code_temp_check <- str_split_fixed(site_id, '-', n = Inf)[1,]

        if(code_temp_check[length(code_temp_check)] == 'temp'){
            code <- substr(site_id, 1, nchar(site_id)-5)
        } else{
            code <- site_id
        }

        sg <- filter(site_data, site_type == 'stream_gauge') %>%
            filter(site_code == code)

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
                             site_code = sg$site_code, full_name = sg$full_name,
                             site_type = sg$site_type, latitude = sg$latitude,
                             longitude = sg$longitude,
                             attribution = paste0(sg$domain, "__", sg$site_code)),
                popupOptions = c(
                    className = paste0(sg$domain, "__", sg$site_code, '_popup'),
                    minWidth = 200, maxWidth = 500), label = sg$site_code,
                data = sg)  %>%
            removeMarker(layerId = paste0(prev_select()$id, '-temp'))

        prev_select(selected)
    }
})

# Highlight watersheds when stream guages are clicked
prev_select_m <- reactiveVal()
observeEvent(ignoreNULL = FALSE,{
        input$MAP_marker_click
        input$MAP_click
    }, {

    if(is.null(prev_select_m()) && is.null(input$MAP_marker_click)) { } else {

        site_id <- input$MAP_marker_click$id
        code_temp_check <- str_split_fixed(site_id, '-', n = Inf)[1,]

        if(code_temp_check[length(code_temp_check)] == 'temp'){
            code_ <- substr(site_id, 1, nchar(site_id)-5)
        } else{
            code_ <- site_id
        }

        shed <- sheds %>%
            filter(site_code == code_)

        selected <- list(id = code_)

        site_remove <- prev_select_m()$id

            proxy %>%
                addPolygons(data = shed, weight = 3, smooth = 0, stroke = T,
                            fillOpacity = 0.2, color = '#228B22',
                            layerId = paste0(shed$site_code, '-temp'), group = 'Catchments')


                proxy %>%
                    addPolygons(data = shed, weight = 3, smooth = 0, stroke = T,
                                fillOpacity = 0.2, color = '#228B22',
                                layerId = paste0(shed$site_code, '-temp'), group = 'Catchments') %>%
                    removeShape(layerId = paste0(site_remove, '-temp'))

        prev_select_m(selected)
    }
})

# Display table below map when a gauge is clicked
site_info_tib <- reactive({

        site_id <- input$MAP_marker_click$id

        if(is.na(site_id) || is.null(site_id)){
            return()
        }

        code_temp_check <- str_split_fixed(site_id, '-', n = Inf)[1,]

        if(code_temp_check[length(code_temp_check)] == 'temp'){
            code <- substr(site_id, 1, nchar(site_id)-5)
        } else{
            code <- site_id
        }

        rain <- str_split_fixed(code, '_[*]_', n = Inf)[2]

        if(rain == 'rain' & !is.na(rain)) {

            site_code <- str_split_fixed(code, '_[*]_', n = Inf)[1]

            shed <- site_data %>%
                filter(site_code == !!site_code) %>%
                filter(site_type == 'rain_gauge')

            fin_tib <- tibble(var = c('Site Code', 'Full Name', 'Domain', 'Site Type'),
                              val = c(site_code, shed$full_name, shed$pretty_domain, 'Rain Gauge'))
        } else {


            shed <- site_data %>%
                filter(site_code == !!code) %>%
                filter(site_type == 'stream_gauge')

            shed_summary <- watershed_summaries %>%
                filter(site_code == !!code)

            dom_cover <- shed_summary %>%
                select(starts_with('lg')) %>%
                pivot_longer(cols = starts_with('lg')) %>%
                filter(value == max(value))

            dom_cover_name <- variables[variables$variable_code == dom_cover$name, ]$variable_name

            if(nrow(dom_cover) == 0){
                dom_cover <- NA
            } else {
                dom_cover <- dom_cover_name
            }

            fin_tib <- sw(tibble(var = c('Site Code', 'Full Name', 'Domain', 'Site Type', 'Stream',
                                      'Area (ha)', 'Mean Slope (%)', 'Annual Mean Precip (mm)', 'Annual Mean Temp (C)',
                                      'Dominant  Land Cover'),
                              val = c(code, shed$full_name, shed$pretty_domain, 'Stream Gauge',
                                      shed$stream, round(shed$ws_area_ha, 1), round(shed_summary$te_slope_mean, 1),
                                      round(shed_summary$cc_mean_annual_precip, 1),
                                      round(shed_summary$cc_mean_annual_temp, 1), dom_cover)) %>%
                left_join(watershed_quar, by = 'var') %>%
                mutate(qua = ifelse(as.numeric(val) < bb, 'Bottom 25%', NA),
                       qua = ifelse(as.numeric(val) >= tt, 'Top 25%', qua)) %>%
                select(-bb, -tt))

        }

        fin_tib[is.na(fin_tib)] <- ''

        return(fin_tib)
    })

output$MAP_SITE_INFO <- renderTable(colnames = FALSE,
                                   # bordered = TRUE,
                                    {
    expr = {
        tib <- site_info_tib()

        return(tib)

    }

})

output$MAP_SITE_INFO_TITLE <- renderText({
    site_tib <- site_info_tib()
    if(nrow(site_tib) == 0 || is.null(site_tib) ) {
        return(' ')
    } else {
        return('Site Information')
    }


})

## Add site as a class
# htmlwidgets::onRender("
#         for (var i = 0; i < data.longitude.length; i++) {
#         var site = data.site_code[i];
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
