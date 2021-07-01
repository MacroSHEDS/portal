data_tab <- tabPanel('Data',

    br(),
    fluidRow(class = 'text-center',

        column(6, offset = 3,

            h2('Explore'),
            actionButton(inputId = 'SITE_CATALOG_BUTTON',
                         label = 'Site Catalog',
                         width = '100%',
                         style = 'color: #333'),
                # icon = icon("th"))
            actionButton(inputId = 'VARIABLE_CATALOG_BUTTON',
                         label = 'Variable Catalog',
                         width = '100%',
                         style = 'color: #333'),

            h2('Download'),
            downloadButton(outputId = 'DL_SUBMIT_SITE',
                           label = 'Site summary table',
                           style = 'color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%'),
            downloadButton(outputId = 'DL_SUBMIT_VAR',
                           label = 'Variable table',
                           style = 'color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%'),
            actionButton(inputId = 'TIMESERIES_DL_BUTTON',
                         label = 'Time-series data',
                         width = '100%',
                         style = 'color: #333'),
            actionButton(inputId = 'SPATIAL_DL_BUTTON',
                         label = 'Spatial data and GIS files',
                         width = '100%',
                         style = 'color: #333')
        )
    )
)
