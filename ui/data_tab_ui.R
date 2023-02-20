data_tab <- tabPanel(
    "Data",
    br(),
    fluidRow(
        class = "text-center",
        column(6,
            offset = 3,
            h2("Explore",
                style = "color: #7b96b6"
            ),
            actionButton(
                inputId = "SITE_CATALOG_BUTTON",
                label = "Site Catalog",
                width = "100%",
                style = "color: #333"
            ),
            # icon = icon("th"))
            actionButton(
                inputId = "VARIABLE_CATALOG_BUTTON",
                label = "Variable Catalog",
                width = "100%",
                style = "color: #333"
            ),
            br(),
            h2("Download",
                style = "color: #7b96b6"
            ),
            downloadButton(
                outputId = "DL_SUBMIT_SITE",
                label = "Site Catalog",
                style = "color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%"
            ),
            downloadButton(
                outputId = "DL_SUBMIT_VAR",
                label = "Variable Catalog",
                style = "color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%"
            ),
            # actionButton(inputId = 'TIMESERIES_DL_BUTTON',
            #              label = 'Time-series data',
            #              width = '100%',
            #              style = 'color: #333'),
            # actionButton(inputId = 'SPATIAL_DL_BUTTON',
            #              label = 'Spatial data and GIS files',
            #              width = '100%',
            #              style = 'color: #333')
            br(),
            h2("Main Dataset",
                style = "color: #7b96b6"
            ),
            actionButton(
                inputId = "EDI_LINK",
                label = "MacroSheds EDI",
                icon = icon("external-link",
                    lib = "font-awesome"
                ),
                width = "100%",
                style = "color: #333",
                ## class = "active",
                onclick = "window.open('https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262', '_blank')"
            ),
            # p('("Download all" option coming soon.)', style = 'color: white')
            br(), br(),
            div(
                class = "well-sm",
                tags$footer("for R users,",
                          tags$a(href="https://github.com/MacroSHEDS/macrosheds", "the MacroSheds R package"),
                          "is the most convenient way to download, explore, and manipulate",
                          "macrosheds data", style = "color: white")
            )
        )
    )
)
