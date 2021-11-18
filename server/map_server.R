sheds <- sf::st_read("data/general/shed_boundary") %>%
    sf::st_transform(4326)

sg <- filter(
    site_data,
    site_type == "stream_gauge"
)

watershed_summaries <- sm(read_csv("data/general/spatial_downloadables/watershed_summaries.csv"))

watershed_quar <- watershed_summaries %>%
    summarise(
        "Annual Mean Precip (mm)_bb" = quantile(cc_mean_annual_precip, .25, na.rm = T),
        "Annual Mean Precip (mm)_tt" = quantile(cc_mean_annual_precip, .75, na.rm = T),
        "Annual Mean Temp (C)_bb" = quantile(cc_mean_annual_temp, .25, na.rm = T),
        "Annual Mean Temp (C)_tt" = quantile(cc_mean_annual_temp, .75, na.rm = T),
        "Mean Slope (%)_bb" = quantile(te_slope_mean, .25, na.rm = T),
        "Mean Slope (%)_tt" = quantile(te_slope_mean, .75, na.rm = T),
        "Area (ha)_bb" = quantile(ws_area_ha, .25, na.rm = T),
        "Area (ha)_tt" = quantile(ws_area_ha, .75, na.rm = T)
    ) %>%
    pivot_longer(cols = everything()) %>%
    mutate(
        var = str_split_fixed(name, "_", n = Inf)[, 1],
        quan = str_split_fixed(name, "_", n = Inf)[, 2]
    ) %>%
    select(-name) %>%
    pivot_wider(names_from = quan, values_from = value)

sheds <- sheds %>%
    filter(site_code %in% sg$site_code)

rg <- filter(
    site_data,
    site_type == "rain_gauge"
)

# add_polygon(
# data = sheds,
# layer = "polygon_layer",
# stroke_colour = "#b6639750",
# stroke_width = 180,
# stroke_opacity = .2,
# fill_colour = "#d3d3d320",
# fill_opacity = 0.9,
# auto_highlight = TRUE,
# highlight_colour = "#b6639750",
# focus_layer = TRUE,
# id = sheds$site_code
# ) %>%
#     add_scatterplot(
#       data = rg_geo,
#       layer_id = "scatter_layer",
#       lon = "longitude",
#       lat = "latitude",
#       radius_min_pixels = 5,
#       radius_max_pixels = 15,
#       radius = 10,
#       fill_colour = "b19cd9"
#     )
# }
#
output$MAP <- renderLeaflet({
    leaflet() %>%
        # MapBox (blue styled basemap)
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvctd9at07qk14qrirm7m6nz",
        #   username = "wslaughter",
        #   group = "Plain"
        # ) %>%
        # NEXRAD Weather
        # addWMSTiles(
        #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        #   layers = "nexrad-n0r-900913",
        #   options = WMSTileOptions(format = "image/png", transparent = TRUE),
        #   attribution = "Weather data © 2012 IEM Nexrad",
        #   group = "Weather"
        # ) %>%
        # USGS Wetland and Lotic Polygons
        # addWMSTiles(
        #   "https://geowebservices.stanford.edu/geoserver/wms",
        #   layers = "druid:sv709xw7113",
        #   options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
        #   # attribution = "Weather data © 2012 IEM Nexrad",
        #   group = "Wetlands and Water Bodies"
        # ) %>%
        # Stanford/UT Tree Canopy WMS
        # addWMSTiles(
        #     "https://geowebservices.stanford.edu/geoserver/wms",
        #     layers = "druid:nh022gd9639",
        #     options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
        #     group = "Tree Canopy"
        # ) %>%
        # Tree Canopy Change (2011-2016)
        addWMSTiles(
            "https://www.mrlc.gov/geoserver/mrlc_display/NLCD_Tree_Canopy_Change_Index_L48/ows?SERVICE=WMS&",
            layers = "NLCD_Tree_Canopy_Change_Index_L48",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            group = "Tree Canopy Change (2011-2016)",
            attribution = "Multi-Resolution Land Characteristics (MRLC) consortium",
        ) %>%
        # addWMSLegend(uri = paste0(
        #     # "https://www.mrlc.gov/geoserver/",
        #     "mrlc_display/NLCD_Tree_Canopy_Change_Index_L48/",
        #     "ows?service=WMS&request=GetLegendGraphic&format",
        #     "=image%2Fpng&width=20&height=20&layer=",
        #     "NLCD_Tree_Canopy_Change_Index_L48"
        # )) %>%
        # Tree Canopy 2016
        addWMSTiles(
            "https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Tree_Canopy_L48/ows?SERVICE=WMS&",
            layers = "NLCD_2016_Tree_Canopy_L48",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "Multi-Resolution Land Characteristics (MRLC) consortium",
            group = "Tree Canopy (2016)"
        ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://www.mrlc.gov/geoserver/",
        #     "mrlc_display/NLCD_2016_Tree_Canopy_L48/",
        #     "ows?service=WMS&request=GetLegendGraphic&format=",
        #     "image%2Fpng&width=20&height=20&layer=",
        #     "NLCD_2016_Tree_Canopy_L48"
        # )) %>%
        # Geology
        addWMSTiles(
            "https://www.sciencebase.gov/arcgis/services/Catalog/5888bf4fe4b05ccb964bab9d/MapServer/WmsServer?",
            layers = "0",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "Horton, J.D., 2017, The State Geologic Map Compilation (SGMC) geodatabase of the conterminous United States (ver. 1.1, August 2017): U.S. Geological Survey data release, https://doi.org/10.5066/F7WH2N65.",
            group = "Geology"
        ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://www.sciencebase.gov/",
        #     "arcgis/services/Catalog/",
        #     "5888bf4fe4b05ccb964bab9d/MapServer/",
        #     "WmsServer?request=GetLegendGraphic%",
        #     "26version=1.3.0%26format=image/png%26layer=0"
        # )) %>%
        # Stanford/UT Impervious Surface (dark theme)
        # addWMSTiles(
        #     "https://geowebservices.stanford.edu/geoserver/wms",
        #     layers = "druid:sw186vh2473",
        #     options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
        #     attribution = "Geological Survey (U.S.). 100-Meter Resolution Impervious Surface of the Conterminous United States, 2001. 100-Meter Resolution Impervious Surface of the Conterminous United States, 2001 - TexasGeoDataPortal.",
        #     group = "Impervious Surfaces"
        # ) %>%
        # USGS Impervious Surface
        addWMSTiles(
            "https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Impervious_L48/ows?SERVICE=WMS&",
            layers = "NLCD_2019_Impervious_L48",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "Multi-Resolution Land Characteristics (MRLC) consortium",
            group = "Impervious Surfaces"
        ) %>% # ttps://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Impervious_L48/ows?service=WMS&request=GetLegendGraphic&format=image%2Fpng&width=20&height=20&layer=NLCD_2019_Impervious_L48
        # addWMSLegend(uri = paste0(
        #     "https://www.mrlc.gov/geoserver/",
        #     "mrlc_display/NLCD_2019_Impervious_L48/",
        #     "ows?service=WMS&request=GetLegendGraphic&format",
        #     "=image%2Fpng&width=20&height=20&layer=NLCD_2019_Impervious_L48"
        # )) %>%
        # code for HomeBrew NLCD serving (retired due to free option with full zoom)
        addTiles(
            urlTemplate = "http://macrosheds.org/map_tiles/nlcd_30m/nlcd/{z}/{x}/{y}.png",
            options = tileOptions(minZoom = 0, maxZoom = 12, tms = TRUE),
            attribution = "USGS Multi-Resolution Land Characteristics Consortium",
            group = "Land Cover (2019)" # NLCD (30m)
        ) %>%
        # USGS NLCD (2019)
        # addWMSTiles(
        #     "https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/ows?SERVICE=WMS&",
        #     layers = "NLCD_2019_Land_Cover_L48",
        #     options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
        #     attribution = "Multi-Resolution Land Characteristics (MRLC) consortium",
        #     group = "Land Cover USGS"
        # ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://www.mrlc.gov/geoserver/",
        #     "mrlc_display/NLCD_2019_Land_Cover_L48/",
        #     "ows?service=WMS&request=GetLegendGraphic&format",
        #     "=image%2Fpng&width=20&height=20&layer=",
        #     "NLCD_2019_Land_Cover_L48"
        # )) %>%
        # USGS NLCD (2001-2019)
        addWMSTiles(
            "https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2001_2019_change_index_L48/ows?SERVICE=WMS&",
            layers = "NLCD_2001_2019_change_index_L48",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "Multi-Resolution Land Characteristics (MRLC) consortium",
            group = "Land Cover Change (2001-2019)"
        ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://www.mrlc.gov/geoserver",
        #     "/mrlc_display/NLCD_2001_2019_change_index_L48/",
        #     "ows?service=WMS&request=GetLegendGraphic&format",
        #     "=image%2Fpng&width=20&height=20&layer=",
        #     "NLCD_2001_2019_change_index_L48"
        # )) %>%
        # USGS Streams (NHD)
        addWMSTiles(
            "https://geowebservices.stanford.edu/geoserver/wms",
            layers = "druid:fc050zj3595",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "USGS- National Hydrography Dataset",
            group = "Streams"
        ) %>%
        # NHD Flow (poor single-wms coverage)
        # addWMSTiles(
        #     "https://hydro.nationalmap.gov:443/arcgis/services/nhd/MapServer/WmsServer?&",
        #     layers = "6",
        #     options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
        #     attribution = "USGS",
        #     group = "NHDPlus"
        # ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://hydro.nationalmap.gov:443/",
        #     "arcgis/services/nhd/MapServer",
        #     "/WmsServer?request=GetLegendGraphic%26version=",
        #     "1.3.0%26format=image/png%26layer=6"
        # )) %>%
        # 3DEP Elevation
        addWMSTiles(
            "https://elevation.nationalmap.gov:443/arcgis/services/3DEPElevation/ImageServer/WMSServer",
            layers = "3DEPElevation:Hillshade Elevation Tinted",
            options = WMSTileOptions(format = "image/png", info_format = "text/html", transparent = TRUE),
            attribution = "USGS National Map 3D Elevation Program (3DEP)",
            group = "3DEP Elevation"
        ) %>%
        # addWMSLegend(uri = paste0(
        #     "https://elevation.nationalmap.gov:443/",
        #     "arcgis/services/3DEPElevation/ImageServer/",
        #     "WMSServer?request=GetLegendGraphic%26version=",
        #     "1.3.0%26format=image/png%26layer=3DEPElevation:Hillshade Elevation Tinted"
        # )) %>%
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvij6fet2e4k15pey4yj3qlj",
        #   username = "wslaughter",
        #   group = "Landcover" # NLCD landcover (500m)
        # ) %>%
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvgzj12521jj15mzp96ens8f",
        #   username = "wslaughter",
        #   group = "Pop. Density"
        # ) %>%
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvh2vbn22i4k14p6vri1pvfa",
        #   username = "wslaughter",
        #   group = "Simple"
        # ) %>%
        addProviderTiles("Esri.WorldTopoMap",
            group = paste0(
                "Topo Mahttps://www.dropbox.com/s/kjkhw",
                "ip0t8erh3a/MTM_PQ_data.csv?dl=0p"
            )
        ) %>%
        addProviderTiles("Esri.WorldTopoMap",
            group = "Base Map"
        ) %>%
        # addProviderTiles("Esri.WorldImagery",
        #   group = "Aerial Imagery"
        # ) %>%
        mapboxapi::addMapboxTiles(
            style_id = "ckvh270o93lon14ohwd7im4xl",
            username = "wslaughter",
            group = "EPA Ecoregions",
            access_token = conf$mapboxapi_sk,
            # options = WMSTileOptions(legend = TRUE)
        ) %>%
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvgtv89o68vw14pbg3ycuo61",
        #   username = "wslaughter",
        #   group = "Soils"
        # ) %>%
        # mapboxapi::addMapboxTiles(
        #   style_id = "ckvcs4e6o0wi214o0uogacuvg",
        #   username = "wslaughter",
        #   group = "Hazardous Sites"
        # ) %>%
        addPolygons(
            data = sheds,
            weight = 3,
            smooth = 0,
            stroke = TRUE,
            fillOpacity = 0.2,
            color = "#000000",
            layerId = sheds$site_code,
            highlightOptions = highlightOptions(
                color = "#01bffe90",
                fill = "#b66397",
                opacity = .9
            ),
            group = "Catchments"
        ) %>%
        # rain gauge
        # purple #8856a7  lightgrey #A2A7A9 darkgrey #222d32 grey #82898B
        addCircleMarkers(
            lng = rg$longitude,
            lat = rg$latitude,
            color = "#69D9FE80",
            fillColor = "#FF75D5",
            layerId = paste0(rg$site_code, "_*_rain"),
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
            popupOptions = c(
                className = paste0(
                    rg$domain,
                    "__",
                    rg$site_code,
                    "_popup"
                ),
                minWidth = 200,
                maxWidth = 500
            ),
            data = rg
        ) %>%
        # chemistry gauge
        addCircleMarkers(
            lng = sg$longitude,
            lat = sg$latitude,
            color = "#69D9FE80",
            layerId = sg$site_code,
            stroke = TRUE,
            opacity = 0.5,
            radius = 3,
            weight = 4.5,
            fillOpacity = 1,
            fillColor = "#4a565cc50",
            popup = glue(stream_gauge_buttons,
                domain = sg$domain,
                # pretty_domain = sg$pretty_domain,
                # stream = sg$stream,
                site_code = sg$site_code,
                # full_name = sg$full_name,
                # site_type = sg$site_type,
                # latitude = sg$latitude,
                # longitude = sg$longitude,
                attribution = paste0(
                    sg$domain,
                    "__",
                    sg$site_code
                )
            ),
            popupOptions = c(
                className = paste0(
                    sg$domain,
                    "__",
                    sg$site_code,
                    "_popup"
                ),
                minWidth = 200,
                maxWidth = 500
            ),
            label = ~site_code,
            clusterId = sg$domain,
            clusterOptions = markerClusterOptions(
                zoomToBoundsOnClick = TRUE,
                maxClusterRadius = 4.5,
                iconCreateFunction = JS("function (cluster) {
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

                                                                                }")
            ),
            data = sg
        ) %>%
        addLayersControl(
            position = "topright",
            baseGroups = c("Base Map", "Land Cover (2019)", "Land Cover Change (2001-2019)", "Impervious Surfaces", "Geology", "EPA Ecoregions", "3DEP Elevation", "Streams", "Tree Canopy (2016)", "Tree Canopy Change (2011-2016)"),
            # all maps
            # baseGroups = c("Plain", "Simple", "Geochemistry", "Wetlands and Water Bodies", "Shaded Relief", "Impervious Surfaces", "Tree Canopy", "Streams", "Landcover", "Sulfur", "SO3 and NH3/NH4", "Pop. Density", "Topo Map", "Aerial Imagery", "EPA Ecoregions", "Soils", "Hazardous Sites"),
            options = layersControlOptions(
                collapsed = FALSE,
                autoZIndex = TRUE
            )
        ) %>%
        setView(
            lng = -97.380979,
            lat = 42.877742,
            zoom = 2
        ) # center of lower 48
})

# Highlight on click
proxy <- leafletProxy("MAP", session)

# Highlight stream gauges when watersheds are clicked
prev_select <- reactiveVal()

observeEvent(
    {
        input$MAP_shape_click
        input$MAP_click
    },
    {
        if (is.null(prev_select()) && is.null(input$MAP_shape_click)) { } else {
            site_id <- input$MAP_shape_click$id
            code_temp_check <- str_split_fixed(site_id, "-", n = Inf)[1, ]

            if (code_temp_check[length(code_temp_check)] == "temp") {
                code <- substr(site_id, 1, nchar(site_id) - 5)
            } else {
                code <- site_id
            }

            sg <- filter(site_data, site_type == "stream_gauge") %>%
                filter(site_code == code)

            selected <- list(id = code, lat = sg$latitude, lng = sg$longitude)

            proxy %>%
                addCircleMarkers(
                    layerId = paste0(code, "-temp"),
                    lng = sg$longitude,
                    lat = sg$latitude,
                    color = "#d8854680", stroke = TRUE, opacity = 1, radius = 6,
                    weight = 2.5, fillOpacity = .1, fillColor = "#018dbc20",
                    popup = glue(stream_gauge_buttons,
                        domain = sg$domain,
                        pretty_domain = sg$pretty_domain, stream = sg$stream,
                        site_code = sg$site_code, full_name = sg$full_name,
                        site_type = sg$site_type, latitude = sg$latitude,
                        longitude = sg$longitude,
                        attribution = paste0(sg$domain, "__", sg$site_code)
                    ),
                    popupOptions = c(
                        className = paste0(sg$domain, "__", sg$site_code, "_popup"),
                        minWidth = 200, maxWidth = 500
                    ), label = sg$site_code,
                    data = sg
                ) %>%
                removeMarker(layerId = paste0(prev_select()$id, "-temp"))

            prev_select(selected)
        }
    }
)

# Highlight watersheds when stream guages are clicked
prev_select_m <- reactiveVal()
observeEvent(
    ignoreNULL = FALSE,
    {
        input$MAP_marker_click
        input$MAP_click
    },
    {
        if (is.null(prev_select_m()) && is.null(input$MAP_marker_click)) { } else {
            site_id <- input$MAP_marker_click$id
            code_temp_check <- str_split_fixed(site_id, "-", n = Inf)[1, ]

            if (code_temp_check[length(code_temp_check)] == "temp") {
                code_ <- substr(site_id, 1, nchar(site_id) - 5)
            } else {
                code_ <- site_id
            }

            shed <- sheds %>%
                filter(site_code == code_)

            selected <- list(id = code_)

            site_remove <- prev_select_m()$id

            proxy %>%
                addPolygons(
                    data = shed, weight = 3, smooth = 0, stroke = T,
                    fillOpacity = 0.2, color = "#228B22",
                    layerId = paste0(shed$site_code, "-temp"), group = "Catchments"
                )


            proxy %>%
                addPolygons(
                    data = shed, weight = 3, smooth = 0, stroke = T,
                    fillOpacity = 0.2, color = "#228B22",
                    layerId = paste0(shed$site_code, "-temp"), group = "Catchments"
                ) %>%
                removeShape(layerId = paste0(site_remove, "-temp"))

            prev_select_m(selected)
        }
    }
)

# Display table below map when a gauge is clicked
site_info_tib <- reactive({
    site_id <- input$MAP_marker_click$id

    if (is.na(site_id) || is.null(site_id)) {
        return()
    }

    code_temp_check <- str_split_fixed(site_id, "-", n = Inf)[1, ]

    if (code_temp_check[length(code_temp_check)] == "temp") {
        code <- substr(site_id, 1, nchar(site_id) - 5)
    } else {
        code <- site_id
    }

    rain <- str_split_fixed(code, "_[*]_", n = Inf)[2]

    if (rain == "rain" & !is.na(rain)) {
        site_code <- str_split_fixed(code, "_[*]_", n = Inf)[1]

        shed <- site_data %>%
            filter(site_code == !!site_code) %>%
            filter(site_type == "rain_gauge")

        fin_tib <- tibble(
            var = c("Site Code", "Full Name", "Domain", "Site Type"),
            val = c(site_code, shed$full_name, shed$pretty_domain, "Rain Gauge")
        )
    } else {
        shed <- site_data %>%
            filter(site_code == !!code) %>%
            filter(site_type == "stream_gauge")

        shed_summary <- watershed_summaries %>%
            filter(site_code == !!code)

        dom_cover <- shed_summary %>%
            select(starts_with("lg")) %>%
            pivot_longer(cols = starts_with("lg")) %>%
            filter(value == max(value))

        dom_cover_name <- variables[variables$variable_code == dom_cover$name, ]$variable_name

        if (nrow(dom_cover) == 0) {
            dom_cover <- NA
        } else {
            dom_cover <- dom_cover_name
        }

        fin_tib <- sw(tibble(
            var = c(
                "Site Code", "Full Name", "Domain", "Site Type", "Stream",
                "Area (ha)", "Mean Slope (%)", "Annual Mean Precip (mm)", "Annual Mean Temp (C)",
                "Dominant  Land Cover"
            ),
            val = c(
                code, shed$full_name, shed$pretty_domain, "Stream Gauge",
                shed$stream, round(shed$ws_area_ha, 1), round(shed_summary$te_slope_mean, 1),
                round(shed_summary$cc_mean_annual_precip, 1),
                round(shed_summary$cc_mean_annual_temp, 1), dom_cover
            )
        ) %>%
            left_join(watershed_quar, by = "var") %>%
            mutate(
                qua = ifelse(as.numeric(val) < bb, "Bottom 25%", NA),
                qua = ifelse(as.numeric(val) >= tt, "Top 25%", qua)
            ) %>%
            select(-bb, -tt))
    }

    fin_tib[is.na(fin_tib)] <- ""

    return(fin_tib)
})

output$MAP_SITE_INFO <- renderTable(
    colnames = FALSE,
    # bordered = TRUE,
    {
        expr <- {
            tib <- site_info_tib()

            return(tib)
        }
    }
)

output$MAP_SITE_INFO_TITLE <- renderText({
    site_tib <- site_info_tib()
    if (nrow(site_tib) == 0 || is.null(site_tib)) {
        return(" ")
    } else {
        return("Site Information")
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



# Get id from map click
# test <- reactive({
#     validate(
#         need(
#             input$MapMine_shape_click != "",
#             "Please select a catchment from the map to the left to view plots and data.
#         App may take a few seconds to load data after selecting data (depending on internet connection speed)."
#         )
#     )
#     (input$MapMine_shape_click) %>%#
#     (input$MapMine_shape_click) %>%""
