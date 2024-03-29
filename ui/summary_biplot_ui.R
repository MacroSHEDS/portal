biplot_data_types_x <- append(biplot_data_types, "Year", after = 0)
biplot_data_types_size <- append(biplot_data_types, "Proportion of Record Missing", after = 0)
# biplot_data_types_size <- biplot_data_types

summary_biplot_tab <- tabPanel("Site Comparison",
    value = "biplot",
    sidebarLayout(
        sidebarPanel(
            ## div(
            ##     class = "text-center",
            ##     actionButton("GEN_PLOTS2",
            ##         "Plot",
            ##         class = "text-center, btn btn-block btn-primary",
            ##     )
            ## ),
            ## br(),
            div("Site Selection", class = "widget-title text-center"),
            radioButtons("SITE_SELECTION2",
                label = NULL,
                choices = c(
                    "Show all sites" = "ALL_SITES2",
                    "Select by domain" = "DOMINE_NETWORK2",
                    "Select individual sites" = "BY_SITE2",
                    "Map Selections " = "BY_BUCKET2"
                ),
                selected = "ALL_SITES2"
            ),
            # br(),
            conditionalPanel(
                'input.SITE_SELECTION2 == "DOMINE_NETWORK2"',
                div("Domains", class = "widget-title text-center"),
                selectizeInput("DOMAINS2",
                    label = NULL, selected = default_domain,
                    choices = domains_pretty, multiple = TRUE
                )
            ),
            conditionalPanel(
                'input.SITE_SELECTION2 == "BY_SITE2"',
                div("Domains", class = "widget-title text-center"),
                selectizeInput("DOMAINS2_S",
                    label = NULL, selected = default_domain,
                    choices = domains_pretty, multiple = TRUE
                ),
                div("Sites", class = "widget-title text-center"),
                selectizeInput("SITES2",
                    label = NULL, selected = default_site,
                    choices = default_sitelist, multiple = TRUE
                )
            ),
            # br(),
            conditionalPanel(
                'input.SITE_SELECTION2 == "BY_BUCKET2"',
                div("Domains", class = "widget-title text-center"),
                selectizeInput("DOMAINS2_B",
                    label = NULL, selected = c(),
                    choices = domains_pretty, multiple = TRUE
                ),
                div("Sites", class = "widget-title text-center"),
                selectizeInput("SITES2_B",
                    label = NULL, selected = c(),
                    choices = default_sitelist, multiple = TRUE
                )
            ),
            div(
                id = "agg_div",
                div("Aggregation", class = "widget-title text-center"),
                radioButtons("AGG2",
                    label = NULL,
                    choices = c(
                        "Full record" = "WHOLE2",
                        "Yearly" = "YEARLY2"
                    ),
                    #  'Monthly'='MONTHLY2'),
                    selected = "WHOLE2"
                )
            ),
            div(
                id = "axes_div",
                div("X-axis", class = "widget-title text-center"),
                selectizeInput("X_TYPE2",
                    label = NULL,
                    multiple = FALSE, choices = biplot_data_types_x, selected = "Watershed Characteristics"
                ),
                selectizeInput("X_VAR2",
                    label = NULL,
                    selected = "PRISM Precipitation",
                    multiple = FALSE, choices = ws_trait_types
                ),
                selectizeInput("X_UNIT2",
                    label = NULL,
                    multiple = FALSE, choices = "mm", selected = "mm"
                ),
                radioButtons("LOG_X2",
                    label = NULL,
                    choices = c(
                        "Standard" = "XAXIS_sta2",
                        "Log Scale" = "XAXIS_log2"
                    ),
                    selected = "XAXIS_sta2"
                ),
                br(),
                actionButton("REFRESH", "", style = "display: none"),
                div("Y-axis", class = "widget-title text-center"),
                selectizeInput("Y_TYPE2",
                    label = NULL,
                    multiple = FALSE, choices = biplot_data_types, selected = "Discharge"
                ),
                selectizeInput("Y_VAR2",
                    label = NULL,
                    # selected = biplot_options[[1]][[5]],
                    multiple = FALSE, choices = biplot_options
                ),
                selectizeInput("Y_UNIT2",
                    label = NULL,
                    multiple = FALSE, choices = conc_units_bi, selected = conc_units_bi[3]
                ),
                radioButtons("LOG_Y2",
                    label = NULL,
                    choices = c(
                        "Standard" = "YAXIS_sta2",
                        "Log scale" = "YAXIS_log2"
                    ),
                    selected = "YAXIS_sta2"
                )
            ),

            # div('Size Variable'),
            # radioButtons('ADD_SIZE2', label=NULL,
            #              choices=c('Yes'='SIZE_YES2',
            #                        'No'='SIZE_NO2'),
            #              selected='SIZE_NO2'),
            div("Point Size", class = "widget-title text-center"),
            div(
                id = "size_div",
                checkboxInput("ADD_SIZE2",
                    value = FALSE,
                    label = paste("Vary point size by...")
                )
            ),
            conditionalPanel("input.ADD_SIZE2 == true",
                selectizeInput("SIZE_TYPE2",
                    label = NULL,
                    multiple = FALSE, choices = biplot_data_types_size, selected = "Stream Concentration"
                ),
                selectizeInput("SIZE_VAR2",
                    label = NULL,
                    selected = biplot_options[[1]][[3]],
                    multiple = FALSE, choices = biplot_options
                ),
                selectizeInput("SIZE_UNIT2",
                    label = NULL,
                    multiple = FALSE, choices = conc_units
                ),
                width = 3
            )
        ),
        mainPanel(
            div("Single-click legend to toggle plot groups by color. Double-click to isolate a color or restore the full plot",
                class = "widget-caption"
            ),
            plotlyOutput("SUMMARY_BIPLOT", height = "600px"),
            sliderInput("DATES2_INTER",
                label = NULL, min = dtrng[1], max = dtrng[2],
                value = c(dtrng[1], dtrng[2]),
                width = "100%", timeFormat = "%Y", step = 365,
                dragRange = TRUE
            )
        )
    )
)
